############################# MAIN 3 #############################

# objective: 
# first calculate the optimum coefficients for each combination
# then select the best combination

############################# inputs #############################

list_combination = list("optimal" = optimal_combination, 
                        "random_n_comm" = combination_random_n_comm, 
                        "random_big" = combination_random_big) 

# different investments:
individual_investment_max = vector(length = sum(optimal_combination))
individual_investment_max[] = global_investment/sum(optimal_combination)

individual_investment_max_big = vector(length = sum(combination_random_big==1))
individual_investment_max_big[] = global_investment/sum(combination_random_big==1)

list_individual_investment_max = list("optimal" = individual_investment_max * runif(n = sum(optimal_combination), min = 1, max = 1.5), 
                                      "random_n_comm" = individual_investment_max * runif(n = sum(combination_random_n_comm==1), min = 1, max = 1.5), 
                                      "random_big" = individual_investment_max_big * runif(n = sum(combination_random_big==1), min = 1, max = 1.5))

############################# outputs #############################

list_matrix_coefficients_2 = list()
list_matrix_coefficients_3 = list()
list_matrix_coefficients_4 = list()

list_individual_investment_selected = list()

############################# run optimizations #############################



for (i in 1:length(list_combination)) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  
  
  # check that:
  individual_investment_max = list_individual_investment_max[[i]]
  print(sum(individual_investment_max) >= global_investment)
  
  # individual_investment_max = individual_investment[pre_optimal_combination_free==1]  
  individual_investment_selected = calculate_individual_investment(combination_i, global_investment, individual_investment_max)
  list_individual_investment_selected[[i]] = individual_investment_selected
  
  # list_matrix_coefficients_2[[i]] = calculate_matrix_coefficients(optimizer_number = 2, df_gen_sunny, df_cons_selected_sunny, n_community, individual_investment_selected = individual_investment_selected)
  # list_matrix_coefficients_3[[i]] = calculate_matrix_coefficients(optimizer_number = 3, df_gen_sunny, df_cons_selected_sunny, n_community, individual_investment_selected = individual_investment_selected)
  list_matrix_coefficients_4[[i]] = calculate_matrix_coefficients(optimizer_number = 4, df_gen_sunny, df_cons_selected_sunny, n_community, individual_investment_selected, df_local_time, df_purchase_price_one_day)
}

# this problem is appearing:
# Error in matrix_coefficients_4[n_sunny_hours_start:(n_sunny_hours_start +  : 
# number of items to replace is not a multiple of replacement length
# seems like it is solved

