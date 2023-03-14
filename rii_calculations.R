
# Load libraries ----------------------------------------------------------
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(purrr)
library(broom)
library(glue)

# Load data ---------------------------------------------------------------
# This data has a row for each IMD decile. The first column is the IMD decile
# number, the second column is the total GP list size for that decile.
# Each of the other columns is the total activity for that metric and decile.
# In this example there are 32 metrics.
#
# This test dataset has been created using a random number generator with data
# items in a pattern, range and with an average similar to the original dataset.
#

activity_type_decile <-
  readRDS("data/activity_by_type_decile_stg1_testdata.rds") |>
  clean_names()

# Metrics 2 - 9 listed as use GP list size 16+ as denominator
# else uses CHD prevalence
by_list_total <- c(
  "02", "05",
  "06", "07",
  "08", "09"
)

# Produces a string to be used in an if(else) in later code to match against
# the metric numbers listed in by_list_total
by_list_regex <- glue("^metric({paste(by_list_total, collapse = '|')})_total$")

# Data manipulation code --------------------------------------------------
activity_long <- activity_type_decile |>
  pivot_longer(
    cols = c(starts_with("metric"), -metric01_total),
    names_to = "metric_name",
    values_to = "metric_total"
  ) |>
  mutate(
    total_column = ifelse(
      str_detect(metric_name, by_list_regex),
      list_size_total,
      metric01_total
    ),
    total_ratio = metric_total / total_column * 1000
  ) |>
  select(
    quantile = gp_im_dquantile,
    metric01_total,
    metric_name,
    metric_total,
    total_ratio
  ) |>
  mutate(activity = (total_ratio * metric01_total / 1000)) |>
  group_by(metric_name) |>
  # arrange() required for first() and last() functions to calculate
  # most_depr_value and least_depr_value
  arrange(
    metric_name,
    quantile
  ) |>
  mutate(
    total_activity = sum(activity),
    prevalence = sum(metric01_total),
    overall_value = total_activity / prevalence * 1000,
    population = metric01_total,
    most_depr_value = first(total_ratio),
    least_depr_value = last(total_ratio),
    total_pop = sum(population),
    proportion_pop = population / total_pop,
    abs_range = most_depr_value - least_depr_value, # not used in this particular code
    rel_range = most_depr_value / least_depr_value
  ) |>
  ungroup() |>
  ############################################### .
  ## Population attributable risk (PAR) ----
############################################### .
# Calculation PAR
# Formula here: https://pdfs.semanticscholar.org/14e0/c5ba25a4fdc87953771a91ec2f7214b2f00d.pdf
# https://fhop.ucsf.edu/sites/fhop.ucsf.edu/files/wysiwyg/pg_apxIIIB.pdf
mutate(
  par_rr = (total_ratio / least_depr_value - 1) * proportion_pop,
  par = sum(par_rr) / (sum(par_rr) + 1) * 100
) |>
  select(-par_rr)


# set up data frame for model
model_df_all_variables <- activity_long |>
  group_by(metric_name) |>
  dplyr::mutate(cumulative_proportion_pop = cumsum(proportion_pop)) |>
  dplyr::mutate(adj_IMD_decile = case_when(quantile == 1 ~ 0.5*proportion_pop,
                                           quantile != 1 ~ lag(cumulative_proportion_pop) + 0.5*proportion_pop)) |>
  select(metric_name, adj_IMD_decile, total_ratio, population)


# create variable to loop over
variables <- levels(as.factor(activity_long$metric_name))

# create table to hold intermediate results of sii calculations
sii_staging <- data.frame(metric_name = character(),
                              pred0 = numeric(),
                              pred0se = numeric(),
                              pred1 = numeric(),
                              pred1se = numeric())


# calculate intermediate sii results for each variable in activity_long
for (i in variables)
  
{
  
  # create subset of med data just for selected variable
  mod_df <- model_df_all_variables |>
    dplyr::filter(metric_name == i) |>
    ungroup() |>
    select(-metric_name)
  
  # run linear regression of total_ratio(value) against IMD weighting for population
  mod <- lm(total_ratio ~ adj_IMD_decile,
            weights = population,
            data = mod_df)
  
  # create dataframe to run predictions
  pred_df <- data.frame(adj_IMD_decile = c(0, 1),
                        total_ratio = c(NA_real_, NA_real_),
                        population = c(NA_real_, NA_real_))
  
  # predict value at either end of IMD spectrum using model
  mod_results = data.frame(metric_name = i,
                           pred0 = predict(mod, newdata = pred_df)[1],
                           pred0se = predict(mod, se.fit = TRUE, newdata = pred_df)$se.fit[1],
                           pred1 = predict(mod, newdata = pred_df)[2],
                           pred1se = predict(mod, se.fit = TRUE, newdata = pred_df)$se.fit[2])
  
  # store results into sii staging df
  sii_staging <- sii_staging |>
    bind_rows(mod_results)
  
}



#set the confidence interval for sii and rii
#change this as required
confLevel <- 0.95


# calculate sii and CIs
# note se of difference of two variables is the square root of the summed 
#squares of the se's of these variables
sii_results <- sii_staging |> 
  dplyr::mutate(sii = pred0 - pred1,
                siiSe = (pred0se ^ 2 + pred1se ^ 2) ^ 0.5) |> 
  dplyr::mutate(siiLcl95 = sii + qnorm(p = (1-confLevel)/2, lower.tail = TRUE)*siiSe,
                siiUcl95 = sii + qnorm(p = (1+confLevel)/2, lower.tail = TRUE)*siiSe) |> 
  select(-siiSe, -pred0, - pred1, -pred0se, -pred1se) 

# join to activity data
test_data <- activity_long |> 
  left_join(sii_results, by = 'metric_name') |> 
  dplyr::mutate(rii = sii / overall_value,
                riiLcl95 = siiLcl95 / overall_value,
                riiUcl95 = siiUcl95 / overall_value)

# Put the data into a table for use or export
sii_table<-test_data |> 
  group_by (`metric_name`)|>
  summarise(
    SII = mean(-`sii`),
    UCI = mean(-`siiUcl95`),
    LCI = mean(-`siiLcl95`),
    RII = mean(-`rii`),
    UCI_RII = mean(-`riiUcl95`),
    LCI_RII = mean(-`riiLcl95`),
    RR=mean(-`rel_range` )) |>
  arrange()

# Save out the results to a csv
#write.csv(sii_table,"data/sii_table.csv")

