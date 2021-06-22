## CASE 3: community objective = MINIMUM INDIVIDUAL PAYBACK (dont care about global surplus)
# 3.1) investors: residential houses => all users invest the same


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
value_vector = c()


comparison_1 = data.frame("user" = factor(1:length(value_vector)), 
                          "i_matrix" = factor(1), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_1)))

comparison_2 = data.frame("user" = factor(1:length(value_vector)), 
                          "i_matrix" = factor(2), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_2)))

comparison_3 = data.frame("user" = factor(1:length(value_vector)), 
                          "i_matrix" = factor(3), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_3)))

comparison_4 = data.frame("user" = factor(1:length(value_vector)), 
                          "i_matrix" = factor(4), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_4)))

comparison = rbind(comparison_1, comparison_2, comparison_3, comparison_4)

p <- ggplot() +
  geom_bar(aes(x = comparison$user,  y = comparison$value, fill = comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/payback_comparison_disaggregated"), plot = p, device = "pdf", width = 8, height = 3)

p <- ggplot() +
  geom_bar(aes(x = comparison$i_matrix,  y = comparison$value, fill = comparison$user), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/payback_comparison_disaggregated"), plot = p, device = "pdf", width = 8, height = 3)



# p <- ggplot() +
#   geom_bar(aes(x = comparison$i_matrix, y = comparison$value, fill = comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
# ggsave(filename = paste0("graphs/comparison_payback_CASE32"), plot = p, device = "pdf", width = 8, height = 3)

# INDIVIDUAL INVESTMENT
value_vector = as.numeric(individual_investment_selected)

comparison = data.frame("user" = factor(1:length(value_vector)), 
                        "value" = value_vector)

p <- ggplot() +
  geom_bar(aes(x = comparison$user, y = comparison$value), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/comparison_investment_CASE32"), plot = p, device = "pdf", width = 8, height = 3)

















# checking
pre_payback = calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_3)
sum(exp(pre_payback - 0))
sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny_one_day, df_cons = df_cons_sunny_one_day))
sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_3, df_gen_sunny_one_day, df_cons_selected_sunny_one_day))

# checking
opt_payback = calculate_payback_betas_daily(df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, coefficients_criteria)
sum(exp(opt_payback - 0))
sum(calculate_surplus_hourly_individual_betas(coefficients_criteria, df_gen_sunny_one_day, df_cons_selected_sunny_one_day))
###




# 1 BIG PROBLEM: the matrix is too homogenous...
# I guess with minus days it can try more different values?
# yes, the problem is when the vector to optimize is tooooo long
# 2 BIG PROBLEM: the payback is wrongly assuming the length of the df is equal to one day!!

plot_matrix(name = "1", coefficients_criteria)
plot_matrix(name = "2", matrix_coefficients_3)


# new_payback[j, combination_selected!=0] = calculate_payback_betas(df_cons_selected_sunny, df_gen_sunny, individual_investment_max, matrix_coefficients = coefficients_criteria)
# surplus = sum(calculate_surplus_hourly_individual_betas(coefficients_criteria, df_gen_sunny, df_cons_selected_sunny))

############################# CASE's comparison plots #############################

matrix_coefficients_list = list()

matrix_coefficients_list[[1]] = matrix_coefficients_3 
matrix_coefficients_list[[2]] = matrix_coefficients_non_optimum
matrix_coefficients_list[[3]] = matrix_coefficients_optimum 

# case: no solar repartition
matrix_coefficients_list[[4]] = matrix_coefficients_optimum
matrix_coefficients_list[[4]][] = 0

plot_comparison_coefficients_upgraded(df_gen, df_gen_sunny, df_cons_selected, df_cons_selected_sunny, matrix_coefficients_list, df_local_time, individual_investment_selected)

# IMPORTANT
# when the investment is proportional to the consumption => there is almost no difference in the payback of pre_surplus (first optimization) and optimization_betas (second optimization)
# TODO: a good scenario would be to define a very different investment compared to the consumption



# case: if cons 2 and cons 9 are exaclty the same:
# why is the optimization_MO giving very different surpluses??
# but the cost they are obtaining is exaclty the same..

# TODO
# important! when changing the payback ideal the payback_years obtained are exactly the same
# for simplicity, using payback years = 0 sounds the easiest a quickest way to procede
# the problem now is how to understand the balance between the surplus and the payback
# surplus is linear
# payback is exponential
# to do this I will use a combination which has surplus > 0 

# from here I can see that the optimum tends tu be simmilar to the "matrix_coefficients_non_optimum" (where the repartition is equi-distributed 1/n_community) 
# I think this is because the investment is proportional to the consumption..
# will try changing this



