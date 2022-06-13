############################# MAIN 4 #############################

############################# generate plot combination: with characteristic df #############################

############################# inputs #############################

list_matrix_coefficients_2 
list_matrix_coefficients_3 
list_matrix_coefficients_4 

list_individual_investment_selected 

############################# outputs #############################

############################# plot 0 #############################

# this plot is not very understandable.. so I will take statistics to better reproduce the results 
# 
# df_payback_2 = data.frame("i_matrix" = factor(), 
#                        "combination" = factor(), 
#                        "user" = factor(), 
#                        "value" = as.numeric())
# 
# df_payback_3 = data.frame("i_matrix" = factor(), 
#                        "combination" = factor(), 
#                        "user" = factor(), 
#                        "value" = as.numeric())
# 
# df_payback_4 = data.frame("i_matrix" = factor(), 
#                        "combination" = factor(), 
#                        "user" = factor(), 
#                        "value" = as.numeric())
# 
# for (i in 1:length(list_combination)) {
#   combination_i = list_combination[[i]]
#   n_community = sum(combination_i)
#   df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
#   df_cons_selected_users = df_cons[, combination_i==1]  
# 
#   payback_2 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
#                                                  individual_investment = list_individual_investment_selected[[i]], 
#                                                  matrix_coefficients = list_matrix_coefficients_2[[i]]))
#   df_payback_2 = rbind(df_payback_2, data.frame("i_matrix" = factor(2), "combination" = i, "user" = factor(1:ncol(df_cons_selected_users)), "value" = payback_2))
# 
#   payback_3 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
#                                                  individual_investment = list_individual_investment_selected[[i]], 
#                                                  matrix_coefficients = list_matrix_coefficients_3[[i]]))
#   df_payback_3 = rbind(df_payback_3, data.frame("i_matrix" = factor(3), "combination" = i, "user" = factor(1:ncol(df_cons_selected_users)), "value" = payback_3))
# 
#   payback_4 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
#                                                  individual_investment = list_individual_investment_selected[[i]], 
#                                                  matrix_coefficients = list_matrix_coefficients_4[[i]]))
#   df_payback_4 = rbind(df_payback_4, data.frame("i_matrix" = factor(4), "combination" = i, "user" = factor(1:ncol(df_cons_selected_users)), "value" = payback_4))
# }
# 
# plot_comparison_payback(name = "2", comparison = df_payback_2)
# plot_comparison_payback(name = "3", comparison = df_payback_3)
# plot_comparison_payback(name = "4", comparison = df_payback_4)

############################# plot 1: payback and surplus stats #############################

list_surplus = vector("list", 3)
names(list_surplus) = c("optimal", "random_n_comm", "random_big") 

for (i in 1:length(list_combination)) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  
  
  surplus_2 = sum(calculate_surplus_hourly_individual_betas(list_matrix_coefficients_2[[i]], df_gen_sunny, df_cons_selected_sunny))
  surplus_3 = sum(calculate_surplus_hourly_individual_betas(list_matrix_coefficients_3[[i]], df_gen_sunny, df_cons_selected_sunny))
  surplus_4 = sum(calculate_surplus_hourly_individual_betas(list_matrix_coefficients_4[[i]], df_gen_sunny, df_cons_selected_sunny))
  
  list_surplus[[i]] = data.frame("investment" = surplus_2, "solar_excess" = surplus_3, "solar_excess_and_payback" = surplus_4)
}


list_sunny_emissions = vector("list", 3)
names(list_sunny_emissions) = c("optimal", "random_n_comm", "random_big") 

for (i in 1:length(list_combination)) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  

  sunny_emissions_2 = sum(calculate_sunny_emissions(list_matrix_coefficients_2[[i]], df_gen_sunny, df_cons_selected_sunny))
  sunny_emissions_3 = sum(calculate_sunny_emissions(list_matrix_coefficients_3[[i]], df_gen_sunny, df_cons_selected_sunny))
  sunny_emissions_4 = sum(calculate_sunny_emissions(list_matrix_coefficients_4[[i]], df_gen_sunny, df_cons_selected_sunny))
  
  list_sunny_emissions[[i]] = data.frame("investment" = sunny_emissions_2, "solar_excess" = sunny_emissions_3, "solar_excess_and_payback" = sunny_emissions_4)
}


list_avoided_emissions = vector("list", 3)
names(list_avoided_emissions) = c("optimal", "random_n_comm", "random_big") 

for (i in 1:length(list_combination)) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  
  
  avoided_emissions_2 = calculate_daily_avoided_emissions(list_matrix_coefficients_2[[i]], df_gen_sunny, df_cons_selected_sunny, df_local_time)
  avoided_emissions_3 = calculate_daily_avoided_emissions(list_matrix_coefficients_3[[i]], df_gen_sunny, df_cons_selected_sunny, df_local_time)
  avoided_emissions_4 = calculate_daily_avoided_emissions(list_matrix_coefficients_4[[i]], df_gen_sunny, df_cons_selected_sunny, df_local_time)

  list_avoided_emissions[[i]] = data.frame("investment" = avoided_emissions_2, "solar_excess" = avoided_emissions_3, "solar_excess_and_payback" = avoided_emissions_4)
}


list_self_sufficiency = vector("list", 3)
names(list_self_sufficiency) = c("optimal", "random_n_comm", "random_big") 

for (i in 1:length(list_combination)) {
  combination_i = list_combination[[i]]
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  
  
  self_sufficiency_2 = calculate_self_sufficiency(list_matrix_coefficients_2[[i]], df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time)
  self_sufficiency_3 = calculate_self_sufficiency(list_matrix_coefficients_3[[i]], df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time)
  self_sufficiency_4 = calculate_self_sufficiency(list_matrix_coefficients_4[[i]], df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time)
  
  list_self_sufficiency[[i]] = data.frame("investment" = self_sufficiency_2, "solar_excess" = self_sufficiency_3, "solar_excess_and_payback" = self_sufficiency_4)
}


list_self_consumption = vector("list", 3)
names(list_self_consumption) = c("optimal", "random_n_comm", "random_big") 

for (i in 1:length(list_combination)) {
  combination_i = list_combination[[i]]
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  
  
  self_consumption_2 = calculate_self_consumption(list_matrix_coefficients_2[[i]], df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time)
  self_consumption_3 = calculate_self_consumption(list_matrix_coefficients_3[[i]], df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time)
  self_consumption_4 = calculate_self_consumption(list_matrix_coefficients_4[[i]], df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time)
  
  list_self_consumption[[i]] = data.frame("investment" = self_consumption_2, "solar_excess" = self_consumption_3, "solar_excess_and_payback" = self_consumption_4)
}


  



list_max_payback = vector("list", 3)
list_mean_payback = vector("list", 3)
list_min_payback_aux = vector("list", 3)
list_diff_max_min_payback = vector("list", 3)

names(list_max_payback) = c("optimal", "random_n_comm", "random_big") 
names(list_mean_payback) = c("optimal", "random_n_comm", "random_big")
names(list_min_payback_aux) = c("optimal", "random_n_comm", "random_big")
names(list_diff_max_min_payback) = c("optimal", "random_n_comm", "random_big")  

for (i in 1:length(list_combination)) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  
  
  payback_2 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                                 individual_investment = list_individual_investment_selected[[i]], 
                                                 matrix_coefficients = list_matrix_coefficients_2[[i]]))
  
  payback_3 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                                 individual_investment = list_individual_investment_selected[[i]], 
                                                 matrix_coefficients = list_matrix_coefficients_3[[i]]))

  payback_4 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                                 individual_investment = list_individual_investment_selected[[i]], 
                                                 matrix_coefficients = list_matrix_coefficients_4[[i]]))

  list_max_payback[[i]] = data.frame("investment" = max(payback_2), "solar_excess" = max(payback_3), "solar_excess_and_payback" = max(payback_4))
  list_mean_payback[[i]] = data.frame("investment" = mean(payback_2), "solar_excess" = mean(payback_3), "solar_excess_and_payback" = mean(payback_4))
  list_min_payback_aux[[i]] = data.frame("investment" = min(payback_2), "solar_excess" = min(payback_3), "solar_excess_and_payback" = min(payback_4))
  list_diff_max_min_payback[[i]] = list_max_payback[[i]] - list_min_payback_aux[[i]]
  
}

plot_comparison_stats(name = "", list_surplus, list_max_payback, list_mean_payback, list_diff_max_min_payback)
plot_comparison_stats(name = "_emissions", list_sunny_emissions, list_max_payback, list_mean_payback, list_diff_max_min_payback)
plot_comparison_stats(name = "_avoided_emissions", list_avoided_emissions, list_max_payback, list_mean_payback, list_diff_max_min_payback)

plot_comparison_stats_complete(name = "_complete3", list_surplus, list_avoided_emissions, list_self_sufficiency, list_self_consumption, list_max_payback, list_mean_payback, list_diff_max_min_payback)

plot_comparison_stats_cut(name = "final", list_surplus, list_avoided_emissions, list_self_sufficiency, list_self_consumption, list_max_payback, list_mean_payback, list_diff_max_min_payback)

############################# plot 2: payback and surplus stats #############################

# do 3 graphs choosing the combination for:
# - best surplus
# - best economic
# - the novel

vector_names = c("_optimal", "_random", "_random_large")

list_matrix_to_use = list("1" = list_matrix_coefficients_4[[1]],
                          "2" = list_matrix_coefficients_2[[2]],
                          "3" = list_matrix_coefficients_3[[3]])

# to compare SURPLUS:
for (i in 1:3) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  

  df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = list_matrix_to_use[[i]])

  plot_disaggregated_community_betas_year_area_facets(name = vector_names[i], df_gen_assigned, df_cons_selected_users, df_local_time)  
  plot_disaggregated_community_betas_year_area_mean(name = vector_names[i], df_gen_assigned, df_cons_selected_users, df_local_time)
}
# explain that the 3rd doesnt change much... it is only interesnting to see the order of magnitud of the aggregated consumption, thats all


# to compare delta PAYBACK:
for (i in 1:3) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = list_matrix_to_use[[i]])
  individual_investment_selected = list_individual_investment_selected[[i]]

  payback = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                               individual_investment = individual_investment_selected, 
                                               matrix_coefficients = list_matrix_to_use[[i]]))

  # the problem of this graph is that for the large communities it is very difficult to interpret.. 
  # so, I will select 3 users, the one with the lowest payback and the one with the highest:
  user_min = which.min(payback)
  user_near_mean = which.min(abs(mean(payback) - payback))
  user_max = which.max(payback)

  plot_simple_users_2(name = vector_names[i], df_local_time, df_cons_selected_sunny[, c(user_min, user_near_mean, user_max)])

  # plot_simple_users_2(name = "cons", df_local_time, df_cons_selected_sunny[, c(user_min, user_near_mean, user_max)])
  # plot_simple_users_2(name = "assigned", df_local_time, df_gen_assigned[, c(user_min, user_near_mean, user_max)])

  payback_selected = payback[c(user_min, user_near_mean, user_max)]
    
  plot_solar_consumption_daily_mean_betas_price_curve_3(name = vector_names[i], df_gen,
                                                      df_gen_assigned = df_gen_assigned[, c(user_min, user_near_mean, user_max)],
                                                      df_cons_selected_sunny = df_cons_selected_sunny[, c(user_min, user_near_mean, user_max)],
                                                      df_local_time,
                                                      individual_investment_selected = individual_investment_selected[c(user_min, user_near_mean, user_max)], 
                                                      payback_selected = payback_selected)
  
}

# TODO: calculate percentages to emphasise in the final conclusions of the paper!!! :)



############################# plot 2: payback and surplus stats #############################

# do 3 graphs choosing the combination for:
# - best surplus
# - best economic
# - the novel

vector_names = c("_optimal", "_random", "_random_large")

list_matrix_to_use = list("1" = list_matrix_coefficients_4[[1]],
                          "2" = list_matrix_coefficients_2[[2]],
                          "3" = list_matrix_coefficients_3[[3]])

for (i in 1:3) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = list_matrix_to_use[[i]])
  
  # to compare SURPLUS:
  # plot_disaggregated_community_betas_year_area_mean(name = vector_names[i], df_gen_assigned, df_cons_selected_users, df_local_time)

  
  # to compare delta PAYBACK:  
  individual_investment_selected = list_individual_investment_selected[[i]]
  
  payback = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                               individual_investment = individual_investment_selected, 
                                               matrix_coefficients = list_matrix_to_use[[i]]))

  user_min = which.min(payback)
  user_near_mean = which.min(abs(mean(payback) - payback))
  user_max = which.max(payback)
  
  # plot_simple_users_2(name = vector_names[i], df_local_time, df_cons_selected_sunny[, c(user_min, user_near_mean, user_max)])
  
  selected_3users = c(user_min, user_near_mean, user_max)
  
  plot_disaggregated_community_betas_year_area_mean_final(name = vector_names[i], df_gen, df_gen_assigned, df_cons_selected_users, df_local_time, individual_investment_selected, payback, selected_3users)
  
}
# explain that the 3rd doesnt change much... it is only interesnting to see the order of magnitud of the aggregated consumption, thats all


# TODO: calculate percentages to emphasise in the final conclusions of the paper!!! :)

