
# set up data frame for model
model_df_all_variables <- test_data_ICB %>%
  group_by(variable) %>%
  dplyr::mutate(cumulative_proprtion_pop = cumsum(proportion_pop)) %>%
  dplyr::mutate(adj_IMD_decile = case_when(quantile == 1 ~ 0.5*proportion_pop,
                                           quantile != 1 ~ lag(cumulative_proprtion_pop) + 0.5*proportion_pop)) %>%
  select(variable, adj_IMD_decile, value, population)

# create variable to loop over
variables <- levels(test_data_ICB$variable)

# create table to hold intermediate results of sii calculations
sii_staging_ICB <- data.frame(variable = character(),
                              pred0 = numeric(),
                              pred0se = numeric(),
                              pred1 = numeric(),
                              pred1se = numeric())


# calculate intermediate sii results for each variable in test_data
for (i in variables)
  
{
  
  # create subset of med data just for selected variable
  mod_df <- model_df_all_variables %>%
    dplyr::filter(variable == i) %>%
    ungroup() %>%
    select(-variable)
  
  # run linear regression of value against IMD weighting for population
  mod <- lm(value ~ adj_IMD_decile,
            weights = population,
            data = mod_df)
  
  # create dataframe to run predictions'
  pred_df <- data.frame(adj_IMD_decile = c(0, 1),
                        value = c(NA_real_, NA_real_),
                        population = c(NA_real_, NA_real_))
  
  # predict value at either end of IMD spectrum using model
  mod_results = data.frame(variable = i,
                           pred0 = predict(mod, newdata = pred_df)[1],
                           pred0se = predict(mod, se.fit = TRUE, newdata = pred_df)$se.fit[1],
                           pred1 = predict(mod, newdata = pred_df)[2],
                           pred1se = predict(mod, se.fit = TRUE, newdata = pred_df)$se.fit[2])
  
  # store results into sii staging df
  sii_staging_ICB <- sii_staging_ICB %>%
    bind_rows(mod_results)
  
}




#sii_staging <- sii_staging_new2
#test_data_initial <- readRDS(file = "test_data_new.RDS")


# BELOW IS getSII_results

# set the confidence interval for sii and rii
# change this as required
# confLevel <- 0.95


# calculate sii and CIs
# note se of difference of two variables is the square root of the summed squares of the se's of these variables
sii_results <- sii_staging_ICB %>% 
  dplyr::mutate(sii = pred0 - pred1,
                siiSe = (pred0se ^ 2 + pred1se ^ 2) ^ 0.5) %>% 
  dplyr::mutate(siiLcl95 = sii + qnorm(p = (1-confLevel)/2, lower.tail = TRUE)*siiSe,
                siiUcl95 = sii + qnorm(p = (1+confLevel)/2, lower.tail = TRUE)*siiSe) %>% 
  select(-siiSe, -pred0, - pred1, -pred0se, -pred1se) 

# BELOW IS update_test_data

test_data <- test_data_ICB %>% 
  left_join(sii_results, by = 'variable') %>% 
  dplyr::mutate(rii = sii / overall_value,
                riiLcl95 = siiLcl95 / overall_value,
                riiUcl95 = siiUcl95 / overall_value)


# BELOW IS get_SII_data
SII_table<-test_data %>% 
  group_by (`variable`)%>% 
  summarise(SII = mean(-`sii`),UCI = mean(-`siiUcl95`),LCI = mean(-`siiLcl95`),
            RII = mean(-`rii`),UCI_RII = mean(-`riiUcl95`),LCI_RII = mean(-`riiLcl95`),
            RR=mean(-`rel_range` )) %>%
  #summarise(Patients = sum(`Number of Patients`)) %>%
  #summarise(totalIMD = sum(`scoretimespopLSOA`)) %>%
  arrange()


