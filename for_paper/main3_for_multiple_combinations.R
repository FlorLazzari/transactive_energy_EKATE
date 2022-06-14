############################# MAIN 3 #############################

# objective: 
# first calculate the optimum coefficients for each combination
# then select the best combination


n_community = ncol(df_cons_selected_users)

# case 2: everyone investing the same
individual_investment_max = vector(length = n_community)
individual_investment_max[] = global_investment/n_community

individual_investment_max = individual_investment_max * runif(n = n_community, min = 1, max = 1.5)

# list_matrix_coefficients_4 = list()
# list_payback_surplus = list()

# for (i in 1:nrow(combination)) {

# i = 1
# combination_i = combination[i, ]
combination_i = combination
n_community = sum(combination_i)
df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
df_cons_selected_users = df_cons[, combination_i==1]  

# check that:
print(sum(individual_investment_max) >= global_investment)

# individual_investment_max = individual_investment[pre_optimal_combination_free==1]  
individual_investment_selected = calculate_individual_investment(combination_i, global_investment, individual_investment_max)

matrix_coefficients_4 = calculate_matrix_coefficient_4_one_year(df_local_time, df_cons_selected_sunny, df_gen_sunny, df_purchase_price_one_day, n_community, individual_investment_selected)
# list_matrix_coefficients_4[[i]] = matrix_coefficients_4

# HERE!

## evaluation:
payback_surplus = calulate_payback_surplus_for_matrix(matrix_coefficients_4, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected)
# list_payback_surplus[[i]] = payback_surplus 

# }

# selected_i = which.min(payback_surplus$surplus)
# optimum_combination = combination[selected_i, ]

