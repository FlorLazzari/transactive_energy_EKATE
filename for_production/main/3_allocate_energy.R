# Process description ##########################################################

############ 1) REQUESTED INPUTS ############

### 1.1) for plots ####
print_plots = T
version = 1

### 1.2) load workspace1 & workspace2 ####
load(file = paste0("workspace/workspace1_(version",version,").RData"))
load(file = paste0("workspace/workspace2_(version",version,").RData"))

### 1.3) import libraries ####
library(ggpubr)
library(reshape2)
source("functions.R")


############ 2)  ############

df_cons_characteristic_selected = df_cons_characteristic[, optimal_combination==1]
df_cons_characteristic_sunny_selected = df_cons_characteristic_sunny[, optimal_combination==1]
matrix_coefficients = calculate_allocation_coefficients(community_objective = "environmental", df_gen_characteristic_sunny, df_cons_characteristic_sunny_selected, n_community = sum(optimal_combination))

df_gen_characteristic_allocated = calculate_gen_assigned_betas(df_gen_day = df_gen_characteristic_sunny, matrix_coefficients = matrix_coefficients)


plot_disaggregated_community_betas_year_area_mean(name = "optimal_1",
                                             df_gen_assigned = df_gen_characteristic_allocated,
                                             df_cons_selected_users = df_cons_characteristic_selected,
                                             df_local_time = df_local_time_characteristic)

plot_disaggregated_community_betas_year_area(name = "optimal_1",
                                             df_gen_assigned = df_gen_characteristic_allocated,
                                             df_cons_selected_users = df_cons_characteristic_selected,
                                             df_local_time = df_local_time_characteristic)

plot_disaggregated_community_betas_year_area_facets(name = "optimal_1",
                                                    df_gen_assigned = df_gen_characteristic_allocated,
                                                    df_cons_selected_users = df_cons_characteristic_selected,
                                                    df_local_time = df_local_time_characteristic)

plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_characteristic_allocated, 
                                             df_cons_selected_users = df_cons_characteristic_selected, 
                                             df_local_time = df_local_time_characteristic)

plot_solar_consumption_daily_summer_betas_price_curve(name = "hola", 
                                                      df_gen = df_gen_characteristic, 
                                                      df_gen_assigned = df_gen_characteristic_assigned, 
                                                      df_cons_selected_sunny = df_cons_characteristic_sunny_selected, 
                                                      df_local_time = df_local_time, 
                                                      individual_investment_selected = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8), 
                                                      df_purchase_price_one_day = df_purchase_price[1:24, ])

