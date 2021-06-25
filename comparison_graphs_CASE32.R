## CASE 3: community objective = MINIMUM INDIVIDUAL PAYBACK (dont care about global surplus)
# 3.2) investors: residential houses => some users invest more than others

individual_investment_max = individual_investment[combination_selected==1]  
individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)

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

# IMPORTANT
# when the investment is proportional to the consumption => there is almost no difference in the payback of pre_surplus (first optimization) and optimization_betas (second optimization)
# TODO: a good scenario would be to define a very different investment compared to the consumption

# compare these 2 cases:
individual_investment_selected_original = individual_investment_selected
individual_investment_selected = sample(individual_investment_selected)

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

matrix_coefficients_4 = selection_according_to_criteria_2(optim, n_community, n_sunny_hours, criteria = 1)

############################# generate plot #############################

# CASE 3.2) GRAPHS showing: INDIVIDUAL PAYBACK - INDIVIDUAL INVESTMENT

# checking:
# plot_matrix(name = "1", matrix_coefficients_1)
# plot_matrix(name = "2", matrix_coefficients_2)
# plot_matrix(name = "3", matrix_coefficients_3)
# plot_matrix(name = "4", matrix_coefficients_4)

# INDIVIDUAL PAYBACK
comparison_1 = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                          "i_matrix" = factor(1), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_1)))

comparison_2 = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                          "i_matrix" = factor(2), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_2)))

comparison_3 = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                          "i_matrix" = factor(3), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_3)))

comparison_4 = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                          "i_matrix" = factor(4), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_4)))

comparison = rbind(comparison_1, comparison_2, comparison_3, comparison_4)

# p <- ggplot() +
#   geom_bar(aes(x = comparison$user,  y = comparison$value, fill = comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
# ggsave(filename = paste0("graphs/presentation_barna/payback_comparison_disaggregated"), plot = p, device = "pdf", width = 8, height = 3)

p <- ggplot() +
  geom_bar(aes(x = comparison$i_matrix,  y = comparison$value, fill = comparison$user), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) +
  labs(x = "Method", y = "Payback [years]", fill = "Users")  
ggsave(filename = paste0("graphs/presentation_barna/comparison_payback_CASE32"), plot = p, device = "pdf", width = 8, height = 3)

# INDIVIDUAL INVESTMENT
value_vector = as.numeric(individual_investment_selected)
comparison = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                        "value" = value_vector)

p <- ggplot() +
  geom_bar(aes(x = comparison$user, y = comparison$value), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) +
  labs(x = "Method", y = "Investment [euros]")   
ggsave(filename = paste0("graphs/presentation_barna/comparison_investment_CASE32"), plot = p, device = "pdf", width = 8, height = 3)


