## CASE 4: community objective = MINIMUM INDIVIDUAL PAYBACK (dont care about global surplus)
# 4.2) investors: residential houses => some users invest more than others

# individual_investment_max = individual_investment[combination_selected==1]  
# individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)
# individual_investment_selected = sample(individual_investment_selected)

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

# optimize_hourly_betas_multi_objective_per_combination:
dim = calculate_dim(hourly=T, n_community, n_sunny_hours)
optim <- nsga2R_flor(fn = purrr::partial(fitness_MO,
                                         df_gen_sunny = df_gen_sunny_one_day,
                                         df_cons_selected_sunny = df_cons_selected_sunny_one_day,
                                         individual_investment_selected = individual_investment_selected),
                     varNo = dim,
                     objDim = 2,
                     # generations = 100,
                     generations = 100,
                     popSize = 200,
                     cprob = 0.8,
                     mprob = 0.2,
                     lowerBounds = rep(0, dim),
                     upperBounds = rep(1, dim))

# criteria 2 = reasonable surplus & payback
matrix_coefficients_4 = selection_according_to_criteria_2(optim, n_community, n_sunny_hours, criteria = 2)

############################# generate plot #############################

# CASE 4.2) GRAPHS showing: INDIVIDUAL PAYBACK - INDIVIDUAL INVESTMENT

# MATRIX REPRESENTATION

color_limits = c(min(matrix_coefficients_1, matrix_coefficients_2, matrix_coefficients_3, matrix_coefficients_4), 
                 max(matrix_coefficients_1, matrix_coefficients_2, matrix_coefficients_3, matrix_coefficients_4)
)
plot_matrix(name = "1", matrix_coefficients_1, color_limits = color_limits)
plot_matrix(name = "2", matrix_coefficients_2, color_limits = color_limits)
plot_matrix(name = "3", matrix_coefficients_3, color_limits = color_limits)
plot_matrix(name = "4", matrix_coefficients_4, color_limits = color_limits)

# DAILY SOLAR ASSIGNATION 0
df_gen_assigned_one_day_1 = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_1)
df_gen_assigned_one_day_2 = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_2)
df_gen_assigned_one_day_3 = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_3)
df_gen_assigned_one_day_4 = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_4)

color_limits = c(min(df_gen_assigned_one_day_1, df_gen_assigned_one_day_2, df_gen_assigned_one_day_3, df_gen_assigned_one_day_4), 
                 max(df_gen_assigned_one_day_1, df_gen_assigned_one_day_2, df_gen_assigned_one_day_3, df_gen_assigned_one_day_4)
                 )

plot_matrix(name = "1_gen", as.matrix(df_gen_assigned_one_day_1), color_limits = color_limits)
plot_matrix(name = "2_gen", as.matrix(df_gen_assigned_one_day_2), color_limits = color_limits)
plot_matrix(name = "3_gen", as.matrix(df_gen_assigned_one_day_3), color_limits = color_limits)
plot_matrix(name = "4_gen", as.matrix(df_gen_assigned_one_day_4), color_limits = color_limits)

# DAILY SOLAR ASSIGNATION 1

# chosen day:
rows_one_day = (df_local_time$month == 3 & df_local_time$date == 2)

df_local_time_one_day = df_local_time[rows_one_day,]
df_gen_one_day = df_gen[rows_one_day,"energy"]
df_cons_selected_one_day = df_cons_selected[rows_one_day,]

df_gen_assigned_one_day = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_1)
plot_disaggregated_daily_mean_community_betas(name = paste0("solar_assignation_", 1), df_gen_assigned = df_gen_assigned_one_day, df_cons_selected_users = df_cons_selected_one_day, df_local_time = df_local_time_one_day)

df_gen_assigned_one_day = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_2)
plot_disaggregated_daily_mean_community_betas(name = paste0("solar_assignation_", 2), df_gen_assigned = df_gen_assigned_one_day, df_cons_selected_users = df_cons_selected_one_day, df_local_time = df_local_time_one_day)

df_gen_assigned_one_day = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_3)
plot_disaggregated_daily_mean_community_betas(name = paste0("solar_assignation_", 3), df_gen_assigned_one_day, df_cons_selected_one_day, df_local_time_one_day)

df_gen_assigned_one_day = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_4)
plot_disaggregated_daily_mean_community_betas(name = paste0("solar_assignation_", 4), df_gen_assigned_one_day, df_cons_selected_one_day, df_local_time_one_day)

# GLOBAL SURPLUS

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
ggsave(filename = paste0("graphs/presentation_barna/comparison_CASE42"), plot = p, device = "pdf", width = 8, height = 3)

# GLOBAL PAYBACK

# TODO: function to calculate global payback

# INDIVIDUAL PAYBACK

comparison_1 = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                          "i_matrix" = factor(1), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_day = df_cons_selected_sunny_one_day, df_gen_day = df_gen_sunny_one_day, individual_investment = individual_investment_selected, matrix_coefficients = matrix_coefficients_1))
                          )

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

p <- ggplot() +
  geom_bar(aes(x = comparison$i_matrix,  y = comparison$value, fill = comparison$user), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/presentation_barna/comparison_payback_CASE42"), plot = p, device = "pdf", width = 8, height = 3)

# INDIVIDUAL INVESTMENT
value_vector = as.numeric(individual_investment_selected)
comparison = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                        "value" = value_vector)

p <- ggplot() +
  geom_bar(aes(x = comparison$user, y = comparison$value), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/presentation_barna/comparison_investment_CASE42"), plot = p, device = "pdf", width = 8, height = 3)



















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







