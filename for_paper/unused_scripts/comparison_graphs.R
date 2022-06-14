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

############################# CASE 1: repartition based on surplus (preoptimization) #############################

# checking:
pre_surplus_ = calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus_2 = sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny))

pre_matrix_coefficients = calculate_matrix_coefficients(df_gen_sunny, df_cons_selected_sunny)
pre_matrix_coefficients = matrix(pre_matrix_coefficients, nrow = n_sunny_hours, ncol = n_community)

pre_surplus__ = calculate_surplus_hourly_individual_betas(pre_matrix_coefficients, df_gen_sunny, df_cons_selected_sunny)

sum(pre_surplus__)

pre_payback = calculate_payback_betas(df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, pre_matrix_coefficients)

df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = pre_matrix_coefficients)
plot_solar_consumption_daily_mean_betas(name = "preoptimization", df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_sunny, df_local_time)
plot_disaggregated_daily_mean_community_betas(name = "preoptimization", df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected, df_local_time)

############################# CASE 2: no optimization of repartition (equitative distribution) #############################

matrix_coefficients_non_optimum = pre_matrix_coefficients
matrix_coefficients_non_optimum[,] = 1/n_community

df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_non_optimum)
plot_solar_consumption_daily_mean_betas(name = "equitative_distribution", df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_sunny, df_local_time)
plot_disaggregated_daily_mean_community_betas(name = "equitative_distribution", df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected, df_local_time)

# plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)

############################# CASE 3: optimum repartition taking into account the payback #############################

# optimal_combination_using_2_GAs$new_surplus
# optimal_combination_using_2_GAs$new_payback

matrix_coefficients_optimum = optimal_combination_using_2_GAs$new_optimum_coefficients[[i]]
df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_optimum)
plot_solar_consumption_daily_mean_betas(name = "taking_into_account_payback", df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_sunny, df_local_time)
plot_disaggregated_daily_mean_community_betas(name = "taking_into_account_payback", df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)

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


