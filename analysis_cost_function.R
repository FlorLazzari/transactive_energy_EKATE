# analisis cost function MANUALLY


# TODO: include the restriction regarding the sale to the grid!!! this is included in all the script, right?? check the functions script

# TODO:
# should choose a combination in which the decision is clear and work here to improve the cost function
# combination selected: 
# surplus = 0
# one of the consumers has cons = 0 (cons_32)


############################# import libraries #############################

library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(GA)
library(parallel)
library(purrr)
source("functions.R")

############################# import data #############################

load("~/Documents/projects/EKATE/transactive_energy_EKATE/workspace/workspace_analysis_cost_function.RData")

# just checking:
possible_combinations = c()
for (i in 1:nrow(pre_optimal_combinations)) {
  combination_selected = pre_optimal_combinations[i, ]
  individual_investment_max = individual_investment[combination_selected==1]
  possible_combinations[i] = (sum(individual_investment_max) > global_investment)

}

pre_optimal_combinations_to_study = as.numeric(which(possible_combinations &  c(pre_surplus != 0)))

pre_surplus[pre_optimal_combinations_to_study[9]]

############################# select a combination #############################

# import: pre_optimal_combinations
# select a combination from
# i in 1:nrow(pre_optimal_combinations) 

# hourly_surplus = apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
# pre_surplus = colSums(hourly_surplus)

# looking for a combination with surplus
# tail(pre_surplus)
# length(pre_surplus)
# pre_surplus[pre_surplus<5 & pre_surplus>4]
# which(pre_surplus<5 & pre_surplus>4)

# took 1 as an example:
i = pre_optimal_combinations_to_study[5]

combination_selected = pre_optimal_combinations[i, ]
df_cons_selected_sunny = df_cons_sunny[,combination_selected==1]
df_cons_selected = df_cons[,combination_selected==1]

individual_investment_max = individual_investment[combination_selected==1]  

df = data.frame("time" = df_local_time$time[df_local_time$sunny],
                "gen" = df_gen_sunny)
df = cbind(df_gen, df_cons_selected, "time" = df_local_time$time)
plot_initial(df)

# TODO: include this in the pre_optimization?
# should satisfy this condition:
print(sum(individual_investment_max) > global_investment)
individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)

# checking 
sum(calculate_surplus_hourly_community(combination = pre_optimal_combinations[i, ], df_gen = df_gen_sunny, df_cons = df_cons_sunny))
pre_surplus[i]

n_sunny_hours = length(df_gen_sunny)
n_community = ncol(df_cons_selected)  

############################# CASE 1: optimization based on surplus (preoptimization) #############################

# checking:
pre_surplus_ = calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus_2 = sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny))

pre_matrix_coefficients = calculate_matrix_coefficients(df_gen_sunny, df_cons_selected_sunny)
# TODO: this should be inside the calculate_matrix_coefficients
pre_matrix_coefficients = matrix(pre_matrix_coefficients, nrow = n_sunny_hours, ncol = n_community)

pre_surplus__ = calculate_surplus_hourly_individual_betas(pre_matrix_coefficients, df_gen_sunny, df_cons_selected_sunny)

sum(pre_surplus__)

pre_payback = calculate_payback_betas(df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, pre_matrix_coefficients)

df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = pre_matrix_coefficients)
plot_solar_consumption_daily_mean_betas(name = "preoptimization", df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_sunny, df_local_time)
plot_disaggregated_daily_mean_community_betas(name = "preoptimization", df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected, df_local_time)

# THIS IS WORKING! but it is optional:
# plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)

# this should be done in the end:
# plot_economic_comparison_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], matrix_coefficients = pre_matrix_coefficients, df_local_time = df_local_time)

############################# CASE 2: no optimization (equitative distribution) #############################

matrix_coefficients_non_optimum = pre_matrix_coefficients
matrix_coefficients_non_optimum[,] = 1/n_community

df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_non_optimum)
plot_solar_consumption_daily_mean_betas(name = "equitative_distribution", df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_sunny, df_local_time)
plot_disaggregated_daily_mean_community_betas(name = "equitative_distribution", df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected, df_local_time)

# plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)

# plot_economic_comparison_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], matrix_coefficients = pre_matrix_coefficients, df_local_time = df_local_time)

############################# CASE 3: optimization taking into account the payback #############################

dim = calculate_dim(hourly, n_community, n_sunny_hours)

coefficients_criteria = optimize_hourly_betas_multi_objective_per_combination(combination_selected = combination_selected, n_community,df_gen_sunny = df_gen_sunny, df_cons_selected_sunny = df_cons_selected_sunny, individual_investment_max)

new_payback = calculate_payback_betas(df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, matrix_coefficients = coefficients_criteria)
surplus = sum(calculate_surplus_hourly_individual_betas(coefficients_criteria, df_gen_sunny, df_cons_selected_sunny))

# TODO: this should be added all in a function
# checking:
matrix_coefficients_optimum = coefficients_criteria
df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_optimum)
plot_solar_consumption_daily_mean_betas(name = "taking_into_account_payback", df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_sunny, df_local_time)
plot_disaggregated_daily_mean_community_betas(name = "taking_into_account_payback", df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)

# plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)
# plot_economic_comparison_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], matrix_coefficients = matrix_coefficients_optimum, df_local_time = df_local_time)

############################# CASE's comparison plots #############################

# this function contains the "plot_economic_comparison_betas"


matrix_coefficients_list = list()

matrix_coefficients_list[[1]] = pre_matrix_coefficients 
matrix_coefficients_list[[2]] = matrix_coefficients_non_optimum
matrix_coefficients_list[[3]] = matrix_coefficients_optimum 
# matrix_coefficients_list[[4]] = matrix_coefficients_optimum
# matrix_coefficients_list[[4]][] = 0

plot_comparison_coefficients_upgraded(df_gen, df_gen_sunny, df_cons_selected, df_cons_selected_sunny, matrix_coefficients_list, df_local_time)

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

plot_matrix(name = "3", matrix_coefficients_list[[3]])



