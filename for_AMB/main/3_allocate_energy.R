# Process description ##########################################################

############ 1) REQUESTED INPUTS ############

### 1.1) for plots ####
print_plots = T
version = 2

### 1.2) load workspace1 & workspace2 ####
load(file = paste0("workspace/workspace1_(version",version,").RData"))
load(file = paste0("workspace/workspace2_(version",version,").RData"))

### 1.3) import libraries ####
library(ggpubr)
library(reshape2)
source("functions.R")


############ 2)  ############

# community_objective = "environmental"
# community_objective = "none"
community_objective = "constant_environmental"

gen_sunny = df_characteristic_selected[(df_characteristic_selected$sunny == T), "energy"]
cons = df_characteristic_selected[, grep("cons", colnames(df_characteristic_selected))]
cons_sunny = df_characteristic_selected[df_characteristic_selected$sunny == T, grep("cons", colnames(df_characteristic_selected))]
local_time = df_characteristic_selected[, c("month", "week", "hour", "sunny")]

matrix_coefficients = calculate_allocation_coefficients(community_objective, gen_sunny, cons_sunny, n_community = sum(grepl("cons", colnames(df_characteristic_selected))))
# matrix_coefficients = calculate_allocation_coefficients(community_objective = "constant_environmental", gen_sunny, cons_sunny, n_community = sum(optimal_combination))
# matrix_coefficients = calculate_allocation_coefficients(community_objective = "none", gen_sunny, cons_sunny, n_community = sum(optimal_combination))

gen_sunny_allocated = calculate_gen_assigned_betas(df_gen_day = gen_sunny, matrix_coefficients = matrix_coefficients)
colnames(gen_sunny_allocated) = paste0("alloc_",1:5)

df_characteristic_aux = cbind(gen_sunny_allocated, df_characteristic_selected[(df_characteristic_selected$sunny == T), c("month", "week", "hour", "sunny")])

df_characteristic_selected_allocated = df_characteristic_selected 
df_characteristic_selected_allocated = merge(df_characteristic_selected_allocated, df_characteristic_aux)

# working here, the idea is to have:
# CLEAN_plot_disaggregated_community_betas_year_area_mean(name = "1", df_characteristic_selected_allocated)
plot_disaggregated_community_betas_year_area_mean_2(name = paste0(version, community_objective),
                                             df_gen_assigned = gen_sunny_allocated,
                                             df_cons_selected_users = cons,
                                             df_cons_selected_users_sunny = cons_sunny,
                                             df_local_time = local_time)


df_characteristic_selected_sunny = df_characteristic_selected[(df_characteristic_selected$sunny == T), ] 
df_characteristic_selected_sunny_week = df_characteristic_selected_sunny[df_characteristic_selected_sunny$week == T, ]

gen_sunny_allocated_week = gen_sunny_allocated[df_characteristic_selected_sunny$week == T, ]
cons_sunny_week = df_characteristic_selected_sunny_week[, grep("cons", colnames(df_characteristic_selected_sunny_week))]
df_characteristic_selected_week = df_characteristic_selected[(df_characteristic_selected$week == T), ] 
cons_week = df_characteristic_selected_week[, grep("cons", colnames(df_characteristic_selected_week))]
local_time_week = df_characteristic_selected[(df_characteristic_selected$week == T), c("month", "week", "hour", "sunny")]

plot_disaggregated_community_betas_year_area_facets_3(name = paste0(version,community_objective,"_week"),
                                                    df_gen_assigned = gen_sunny_allocated_week,
                                                    df_cons_selected_users = cons_week,
                                                    df_cons_selected_users_sunny = cons_sunny_week,
                                                    df_local_time = local_time_week)

df_characteristic_selected_sunny = df_characteristic_selected[(df_characteristic_selected$sunny == T), ] 
df_characteristic_selected_sunny_end_week = df_characteristic_selected_sunny[df_characteristic_selected_sunny$week == F, ]

gen_sunny_allocated_end_week = gen_sunny_allocated[df_characteristic_selected_sunny$week == F, ]
cons_sunny_end_week = df_characteristic_selected_sunny_end_week[, grep("cons", colnames(df_characteristic_selected_sunny_end_week))]
df_characteristic_selected_end_week = df_characteristic_selected[(df_characteristic_selected$week == F), ] 
cons_end_week = df_characteristic_selected_end_week[, grep("cons", colnames(df_characteristic_selected_end_week))]
local_time_end_week = df_characteristic_selected[(df_characteristic_selected$week == F), c("month", "week", "hour", "sunny")]

plot_disaggregated_community_betas_year_area_facets_3(name = paste0(version,community_objective,"_end_week"),
                                                      df_gen_assigned = gen_sunny_allocated_end_week,
                                                      df_cons_selected_users = cons_end_week,
                                                      df_cons_selected_users_sunny = cons_sunny_end_week,
                                                      df_local_time = local_time_end_week)




# df_gen_characteristic_cut = df_gen_characteristic[(df_gen_characteristic$sunny == T), ] 
# df_gen_characteristic_allocated_end_week = df_gen_characteristic_allocated[df_gen_characteristic_cut$week == F, ]
# 
# plot_disaggregated_community_betas_year_area_facets_2(name = "3",
#                                                       df_gen_assigned = df_gen_characteristic_allocated_end_week,
#                                                       df_cons_selected_users = cons,
#                                                       df_local_time = df_local_time_characteristic)
# 
# 
# plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_characteristic_allocated, 
#                                              df_cons_selected_users = df_cons_characteristic_selected, 
#                                              df_local_time = df_local_time_characteristic)
# 
# plot_solar_consumption_daily_summer_betas_price_curve(name = "hola", 
#                                                       df_gen = df_gen_characteristic, 
#                                                       df_gen_assigned = df_gen_characteristic_allocated, 
#                                                       df_cons_selected_sunny = df_cons_characteristic_sunny_selected, 
#                                                       df_local_time = df_local_time, 
#                                                       individual_investment_selected = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8), 
#                                                       df_purchase_price_one_day = df_purchase_price[1:24, ])
# 
