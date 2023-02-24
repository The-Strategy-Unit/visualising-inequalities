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
# In this example there are 33 metrics.
#
# This test dataset has been created using a random number generator with data
# items in a range similar to the original dataset.
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


############################################### .
## Slope of index on inequality (SII) ----
############################################### .
# The calculations below are those of the linear SII, you will have to amend the
# model if you wanted to calculate the Poisson SII
# This code will produce the results of the model, including confidence intervals
sii_model <- activity_long |>
  group_by(metric_name) |>
  mutate(
    cumulative_pro = cumsum(proportion_pop), # cumulative proportion population for each area
    relative_rank = case_when(
      quantile == 1 ~ 0.5 * proportion_pop,
      quantile != 1 ~ lag(cumulative_pro) + 0.5 * proportion_pop
    ),
    sqr_proportion_pop = sqrt(proportion_pop), # square root of the proportion of the population in each SIMD
    relrank_sqr_proppop = relative_rank * sqr_proportion_pop,
    value_sqr_proppop = sqr_proportion_pop * total_ratio
  ) |> # value based on population weights
  nest() |> # creating one column called data with all the variables not in the grouping
  # Calculating linear regression for all the groups, then formatting the results
  # and calculating the confidence intervals
  mutate(
    model = map(data, ~ lm(value_sqr_proppop ~ sqr_proportion_pop + relrank_sqr_proppop + 0, data = .)),
    # extracting sii from model, a bit fiddly but it works
    sii = -1 * as.numeric(map(map(model, "coefficients"), "relrank_sqr_proppop")),
    cis = map(model, confint_tidy)
  ) |> # calculating confidence intervals
  ungroup() |>
  unnest(cis) |>
  # selecting only even row numbers which are the ones that have the sii cis
  filter(row_number() %% 2 == 0) |>
  mutate(
    lowci_sii = -1 * conf.high, # fixing interpretation
    upci_sii = -1 * conf.low
  ) |>
  select(
    -conf.low,
    -conf.high
  ) # non-needed variables


# Merging sii results with main data set
activity <- activity_long |>
  left_join(sii_model, by = "metric_name")


############################################### .
## Relative index of inequality (RII) ----
############################################### .
# This is the calculation of the linear RII which is based on the SII values,
# so that section needs to be run before this one.
rii_index <- activity |>
  mutate(
    rii = sii / overall_value,
    lowci_rii = lowci_sii / overall_value,
    upci_rii = upci_sii / overall_value,
    # Transforming RII into %. This way is interpreted as "most deprived areas are
    # xx% above the average" For example: Cancer mortality rate is around 55% higher
    # in deprived areas relative to the mean rate in the population
    rii_int = rii * 0.5 * 100,
    lowci_rii_int = lowci_rii * 0.5 * 100,
    upci_rii_int = upci_rii * 0.5 * 100
  )



sii_table <- rii_index |>
  group_by(metric_name) |>
  summarise(
    SII = mean(-sii),
    UCI = mean(-upci_sii),
    LCI = mean(-lowci_sii),
    RII = mean(-rii),
    UCI_RII = mean(-upci_rii),
    LCI_RII = mean(-lowci_rii),
    RR = mean(-rel_range)
  ) |>
  arrange()

sii_table
