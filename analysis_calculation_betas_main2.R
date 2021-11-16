############################# MAIN 2 #############################

############################# ordering #############################

# self_consumption
self_consumption_per_user = c()
for (i in 1:ncol(df_cons)) {
  df_cons_sunny_selected = df_cons_sunny[, i]
  self_consumption_no_limits = df_gen_sunny/df_cons_sunny_selected
  self_consumption = ifelse(self_consumption_no_limits > 1, 1, self_consumption_no_limits)
  self_consumption = mean(self_consumption)
  self_consumption_per_user[i] = self_consumption
}

df_cons_sunny = df_cons_sunny[, order(self_consumption_per_user)]
df_cons = df_cons[, order(self_consumption_per_user)]

n_binary_rep = log(ncol(df_cons), base=2)

############################# run model #############################
optimal_combination = optimization_1_to_analize(n_community = n_community_min, 
                                                 n_binary_rep = n_binary_rep, 
                                                 df_gen_to_optimize = df_gen_sunny, 
                                                 df_cons_to_optimize = df_cons_sunny, 
                                                 max_iter = 120)

# surplus = sum(calculate_surplus_hourly_community(combination = combination, df_gen = df_gen_sunny, df_cons = df_cons_sunny))
# surplus = colSums(apply(X = as.matrix(combination), MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny))

#############################  #############################

# plot the aggregated consumption of the whole community without the PV but with the lines of each consumer
optimal_combination = optimal_combinations
# combination = optimal_combinations[1, ]

# not sure if this graph is useful:
df_cons_selected_sunny = df_cons_sunny[, optimal_combination==1]
matrix_coefficients_3 = calculate_matrix_coefficients(optimizer_number = 3, df_gen_sunny, df_cons_selected_sunny, n_community = sum(optimal_combination))
df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_3)
df_cons_selected_users = df_cons[, optimal_combination==1]

plot_disaggregated_community_betas_year_area(name = "optimal_1",
                                             df_gen_assigned = df_gen_assigned,
                                             df_cons_selected_users = df_cons_selected_users,
                                             df_local_time = df_local_time)

############################# compare with random search #############################

vector_surplus_random = c()
combination_random = matrix(0, nrow = 40, ncol = ncol(df_cons))
for (i in 1:40) {
  combination_random_aux = order(runif(ncol(df_cons)))[1:n_community_min]
  combination_random[i, combination_random_aux] = 1
  surplus = sum(calculate_surplus_hourly_community(combination = combination_random[i,], df_gen = df_gen_sunny, df_cons = df_cons_sunny))
  vector_surplus_random = c(vector_surplus_random, surplus)
}

surplus_random = mean(vector_surplus_random)

############################# select a representative random combination #############################

# plot the aggregated consumption of the whole community without the PV but with the lines of each consumer

# select a representative combination:
criteria = min(vector_surplus_random[surplus_random < vector_surplus_random])
selection = which(vector_surplus_random == criteria)
combination_random_n_comm = combination_random[selection,]

# not sure if this graph is useful:
df_cons_random_n_comm_sunny = df_cons_sunny[, combination_random_n_comm==1]
matrix_coefficients_3_random_n_comm = calculate_matrix_coefficients(optimizer_number = 3, df_gen_sunny = df_gen_sunny, df_cons_selected_sunny = df_cons_random_n_comm_sunny, n_community = sum(combination_random_n_comm))
# calulate_payback_surplus_for_matrix(matrix_coefficients_3, df_gen_sunny, df_cons_random_sunny, n_community, purchase_price_sunny, individual_investment_random)
df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_3_random_n_comm)
df_cons_random_n_comm_users = df_cons[, combination_random_n_comm==1]

plot_disaggregated_community_betas_year_area(name = "random",
                                             df_gen_assigned = df_gen_assigned,
                                             df_cons_selected_users = df_cons_random_n_comm_users,
                                             df_local_time = df_local_time)

############################# compare with random bigger search #############################

# have to add ~n_community_min*0.5 participants to obtain a simmilar mean surplus 
vector_surplus_random = c()
combination_random = matrix(0, nrow = 40, ncol = ncol(df_cons))
for (i in 1:40) {
  combination_random_aux = order(runif(ncol(df_cons)))[1:(n_community_min+round(n_community_min*2, digits = 0))]
  # combination_random = rep(x = 0, times = ncol(df_cons))
  combination_random[i, combination_random_aux] = 1
  
  surplus = sum(calculate_surplus_hourly_community(combination = combination_random[i,], df_gen = df_gen_sunny, df_cons = df_cons_sunny))
  vector_surplus_random = c(vector_surplus_random, surplus)
}

surplus_random = mean(vector_surplus_random)

#############################  #############################

# plot the aggregated consumption of the whole community without the PV but with the lines of each consumer

# select a representative combination:
criteria = min(vector_surplus_random[surplus_random < vector_surplus_random])
selection = which(vector_surplus_random == criteria)
combination_random_big = combination_random[selection,]

df_cons_random_big_sunny = df_cons_sunny[, combination_random_big==1]
matrix_coefficients_3_random_big = calculate_matrix_coefficients(optimizer_number = 3, df_gen_sunny, df_cons_random_big_sunny, n_community = sum(combination_random_big))

# from here on I will compare the results of the second optimization with "combination_random_n_comm" & "combination_random_big" 
