## CASE 2: community objective = REDUCE GLOBAL SURPLUS 

# import from main_many_users:
pre_optimal_combinations

hourly_surplus = apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus = colSums(hourly_surplus)

############################# select a combination #############################

combination_selected = pre_optimal_combinations[1, ]

df_cons_selected_sunny = df_cons_sunny[,combination_selected==1]
df_cons_selected = df_cons[,combination_selected==1]

individual_investment_max = individual_investment[combination_selected==1]  

# df = data.frame("time" = df_local_time$time[df_local_time$sunny],
#                 "gen" = df_gen_sunny)
# df = cbind(df_gen, df_cons_selected, "time" = df_local_time$time)
# plot_initial(df)

individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)

############################# select first day #############################

# to check surplus
n_sunny_hours_start = 1
# for (month_i in 1:12) {
for (month_i in 1:5) {
  # for (date_i in 1:2) {
  for (date_i in 1:1) {

    df_local_time_first_day = df_local_time[df_local_time$month %in% month_i & df_local_time$date %in% date_i, ] 
    n_sunny_hours = sum(df_local_time_first_day$sunny)
    
    df_cons_selected_sunny_one_day = df_cons_selected_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1), ]
    df_cons_sunny_one_day = df_cons_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1), ]
    df_gen_sunny_one_day = df_gen_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1)]
    
    # checking 
    print(sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny_one_day, df_cons = df_cons_sunny_one_day)))
    n_sunny_hours_start = n_sunny_hours_start + n_sunny_hours 
  }
}

n_community = ncol(df_cons_selected_sunny)  

hourly_surplus = calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny_one_day, df_cons = df_cons_sunny_one_day)
pre_surplus = sum(hourly_surplus)


############################# OPTIMIZER 3: repartition based on surplus (preoptimization) #############################

matrix_coefficients_3 = calculate_matrix_coefficients(df_gen_sunny_one_day, df_cons_selected_sunny_one_day)
matrix_coefficients_3 = matrix(matrix_coefficients_3, nrow = n_sunny_hours, ncol = n_community)

# pre_payback = calculate_payback_betas_daily(df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, matrix_coefficients_3)
# sum(exp(pre_payback - 0))
# df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_3)

############################# OPTIMIZER 1: no optimization of repartition (equitative distribution) #############################

matrix_coefficients_1 = matrix_coefficients_3
matrix_coefficients_1[,] = 1/n_community

############################# OPTIMIZER 2: repartition based on investment #############################

ratio_investment = as.numeric(individual_investment_selected/sum(individual_investment_selected))
matrix_coefficients_2 = matrix(1, nrow = n_sunny_hours) %*% matrix(ratio_investment, ncol = n_community)

############################# OPTIMIZER 4: optimum repartition taking into account the payback #############################

# trying:
df_cons_selected_sunny_original = df_cons_selected_sunny 
df_gen_sunny_original = df_gen_sunny

# df_cons_selected_sunny = df_cons_selected_sunny_original
# df_gen_sunny = df_gen_sunny_original

# coefficients_criteria = optimize_hourly_betas_multi_objective_per_combination(hourly, combination_selected, df_gen_sunny, df_cons_selected_sunny, individual_investment_max)

# optimize_hourly_betas_multi_objective_per_combination:
dim = calculate_dim(hourly=T, n_community, n_sunny_hours)
# no sense in doing this optimization for CASE 2:
# optim <- nsga2R_flor(fn = purrr::partial(fitness_MO, 
#                                          df_gen_sunny = df_gen_sunny_one_day,
#                                          df_cons_selected_sunny = df_cons_selected_sunny_one_day,
#                                          individual_investment_selected = individual_investment_selected),
#                      varNo = dim, 
#                      objDim = 2, 
#                      # generations = 100,
#                      generations = 50,
#                      popSize = 200,
#                      cprob = 0.8,
#                      mprob = 0.2, 
#                      lowerBounds = rep(0, dim), 
#                      upperBounds = rep(1, dim))
# matrix_coefficients_4 = selection_according_to_criteria_2(optim, n_community, n_sunny_hours)

matrix_coefficients_4 = matrix_coefficients_3

############################# generate plot #############################

# CASE 2.1) GRAPH showing: GLOBAL SURPLUS

# checking:
# plot_matrix(name = "1", matrix_coefficients_1)
# plot_matrix(name = "2", matrix_coefficients_2)
# plot_matrix(name = "3", matrix_coefficients_3)
# plot_matrix(name = "4", matrix_coefficients_4)

value_vector = c(sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_1, df_gen_sunny_one_day, df_cons_selected_sunny_one_day)),
                 sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_2, df_gen_sunny_one_day, df_cons_selected_sunny_one_day)),
                 sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_3, df_gen_sunny_one_day, df_cons_selected_sunny_one_day)),
                 sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_4, df_gen_sunny_one_day, df_cons_selected_sunny_one_day))
                 )

comparison = data.frame("i_matrix" = factor(1:4), 
                        "value" = value_vector
                        )

p <- ggplot() +
  geom_bar(aes(x = comparison$i_matrix, y = comparison$value, fill = comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/comparison_CASE2"), plot = p, device = "pdf", width = 8, height = 3)


