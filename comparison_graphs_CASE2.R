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

# checking 
sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny))

n_sunny_hours = length(df_gen_sunny)
n_community = ncol(df_cons_selected)  

############################# OPTIMIZER 3: repartition based on surplus (preoptimization) #############################

pre_matrix_coefficients = calculate_matrix_coefficients(df_gen_sunny, df_cons_selected_sunny)
pre_matrix_coefficients = matrix(pre_matrix_coefficients, nrow = n_sunny_hours, ncol = n_community)

# pre_payback = calculate_payback_betas(df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, pre_matrix_coefficients)
# sum(exp(pre_payback - 0))
# df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = pre_matrix_coefficients)

############################# OPTIMIZER 1: no optimization of repartition (equitative distribution) #############################

matrix_coefficients_non_optimum = pre_matrix_coefficients
matrix_coefficients_non_optimum[,] = 1/n_community

############################# OPTIMIZER 2: repartition based on investment #############################

matrix_coefficients_non_optimum = pre_matrix_coefficients
matrix_coefficients_non_optimum[,] = 1/n_community


############################# OPTIMIZER 4: optimum repartition taking into account the payback #############################

# trying:
df_cons_selected_sunny_original = df_cons_selected_sunny 
df_gen_sunny_original = df_gen_sunny


df_cons_selected_sunny = df_cons_selected_sunny[1:10, ]
df_gen_sunny = df_gen_sunny[1:10]

# coefficients_criteria = optimize_hourly_betas_multi_objective_per_combination(hourly, combination_selected, df_gen_sunny, df_cons_selected_sunny, individual_investment_max)

# optimize_hourly_betas_multi_objective_per_combination:
dim = calculate_dim(hourly=T, n_community, n_sunny_hours)

optim <- nsga2R_flor(fn = purrr::partial(fitness_MO, 
                                         df_gen_sunny = df_gen_sunny,
                                         df_cons_selected_sunny = df_cons_selected_sunny,
                                         individual_investment_selected = individual_investment_selected),
                     varNo = dim, 
                     objDim = 2, 
                     # generations = 100,
                     generations = 50,
                     popSize = 200,
                     cprob = 0.8,
                     mprob = 0.2, 
                     lowerBounds = rep(0, dim), 
                     upperBounds = rep(1, dim))

# checking
pre_payback = calculate_payback_betas(df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, pre_matrix_coefficients)
sum(exp(pre_payback - 0))
sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny))
sum(calculate_surplus_hourly_individual_betas(pre_matrix_coefficients, df_gen_sunny, df_cons_selected_sunny))


# coefficients_criteria = selection_according_to_criteria(optim, n_community, n_sunny_hours)

# selection_according_to_criteria:
df_pareto_objectives = data.frame(optim$objectives)  
colnames(df_pareto_objectives) = c("surplus", "payback")

rank_1 = (optim$paretoFrontRank == 1)

# df_pareto_objectives = df_pareto_objectives[rank_1, ]
ggplot(df_pareto_objectives) +
  geom_point(aes(x = surplus, y = payback))

z_star = data.frame("surplus" = min(df_pareto_objectives$surplus), 
                    "payback" = min(df_pareto_objectives$payback))

m = -((max(df_pareto_objectives$payback) - z_star$payback)/(max(df_pareto_objectives$surplus) - z_star$surplus))
c = z_star$payback - m*z_star$surplus 

lineal = function(x, m, c){
  y = m*x + c
  return(y)
}

x_lineal = c( (z_star$surplus - 0.1 * max(df_pareto_objectives$surplus)) : (max(df_pareto_objectives$surplus) + 0.1 * max(df_pareto_objectives$surplus)) )
y_lineal = lineal(x = x_lineal, m, c)

# x_lineal = z_star$surplus
# y_lineal = z_star$payback

df_pareto_objectives_rank_1 = df_pareto_objectives[rank_1, ]

rank_1_criteria = calculate_criteria_selected_row(df_pareto_objectives_rank_1, z_star)


df_pareto_betas = data.frame(optim$parameters)  
df_pareto_betas_rank_1 = df_pareto_betas[rank_1, ]

betas_with_criteria = as.numeric(df_pareto_betas_rank_1[rank_1_criteria, ])

coefficients = matrix(data = betas_with_criteria, ncol = n_community, nrow = n_sunny_hours, byrow = T)
coefficients = coefficients/rowSums(coefficients)

objectives_with_criteria = df_pareto_objectives_rank_1[rank_1_criteria, ]
plot_multi_objective_criteria_selection(df_pareto_objectives, z_star, x_lineal, y_lineal, objectives_with_criteria)

coefficients_criteria = coefficients

# checking
pre_payback = calculate_payback_betas(df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, coefficients_criteria)
sum(exp(pre_payback - 0))
sum(calculate_surplus_hourly_individual_betas(coefficients_criteria, df_gen_sunny, df_cons_selected_sunny))
###






combination_optimum = matrix(1, nrow = nrow(coefficients_criteria)) %*% combination_selected
combination_optimum[combination_optimum!=0] = coefficients_criteria


# 1 BIG PROBLEM: the matrix is too homogenous... I guess with minus days it can try more different values?
# 2 BIG PROBLEM: the payback is wrongly assuming the length of the df is equal to one day!!

plot_matrix(name = "1", coefficients_criteria)
plot_matrix(name = "2", pre_matrix_coefficients)


# new_payback[j, combination_selected!=0] = calculate_payback_betas(df_cons_selected_sunny, df_gen_sunny, individual_investment_max, matrix_coefficients = coefficients_criteria)
# surplus = sum(calculate_surplus_hourly_individual_betas(coefficients_criteria, df_gen_sunny, df_cons_selected_sunny))

############################# CASE's comparison plots #############################

matrix_coefficients_list = list()

matrix_coefficients_list[[1]] = pre_matrix_coefficients 
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


# comparison of matrixes:
# TODO: graphical comparison of matrixes?
# pre_matrix_coefficients vs matrix_coefficients_optimum 

plot_matrix(name = "1", matrix_coefficients_list[[1]])
plot_matrix(name = "2", matrix_coefficients_list[[2]])
plot_matrix(name = "3", matrix_coefficients_list[[3]])


