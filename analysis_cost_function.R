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

load("~/Documents/projects/EKATE/transactive_energy_EKATE/workspace/testing_cost_function.RData")


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

i = 7083

combination_selected = pre_optimal_combinations[i, ]
df_cons_selected = df_cons_sunny[,combination_selected==1]
individual_investment_max = individual_investment[combination_selected==1]  

df = data.frame("time" = df_local_time$time[df_local_time$sunny],
                "gen" = df_gen_sunny)
df = cbind(df, df_cons_selected)

plot_initial(df)

# TODO: include this in the pre_optimization?
# should satisfy this condition:
print(sum(individual_investment_max) > global_investment)
individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)

# checking 
sum(calculate_surplus_hourly_community(combination = pre_optimal_combinations[i, ], df_gen = df_gen_sunny, df_cons = df_cons_sunny))
pre_surplus[i]

  
############################# CASE 1: optimization based on surplus (preoptimization) #############################

# checking:
pre_surplus_ = calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus_2 = sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny))

pre_matrix_coefficients = calculate_matrix_coefficients(df_gen_sunny, df_cons_selected)
# TODO: this should be inside the calculate_matrix_coefficients
pre_matrix_coefficients = matrix(pre_matrix_coefficients, nrow = n_sunny_hours, ncol = n_community)

pre_surplus = calculate_surplus_hourly_individual_betas(pre_matrix_coefficients, df_gen_sunny, df_cons_selected)

sum(pre_surplus)

pre_payback = calculate_payback_betas(df_cons_selected, df_gen_sunny, individual_investment_selected, pre_matrix_coefficients)

# TODO: this should be added all in a function
# checking:
df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = pre_matrix_coefficients)
plot_solar_consumption_daily_mean_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected, df_local_time)
plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)
plot_disaggregated_daily_mean_community_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)
plot_economic_comparison_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], matrix_coefficients = pre_matrix_coefficients, df_local_time = df_local_time)


############################# CASE 2: no optimization (equitative distribution) #############################

n_community = ncol(df_gen_assigned)
matrix_coefficients_non_optimum = matrix_coefficients
matrix_coefficients_non_optimum[,] = 1/n_community

# TODO: this should be added all in a function
# checking:
df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_non_optimum)
plot_solar_consumption_daily_mean_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected, df_local_time)
plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)
plot_disaggregated_daily_mean_community_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)
plot_economic_comparison_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], matrix_coefficients = pre_matrix_coefficients, df_local_time = df_local_time)

############################# CASE 3: optimization taking into account the payback #############################

n_community = as.numeric(n_community_vector[i])
n_sunny_hours = nrow(df_cons_selected)      
dim = calculate_dim(hourly, n_community, n_sunny_hours)

# TODO:
# try suggestions:
# including the suggestion or not including it gives simmilar results
# try different weight_surplus

optim_results <- ga(type = "real-valued", fitness = fitness_2_betas, 
                    lower = array(0, dim = dim), upper = array(1, dim = dim),  
                    df_gen_day = df_gen_sunny, df_cons_selected_day = df_cons_selected, combination = combination_selected, 
                    individual_investment = individual_investment_selected, 
                    weight_surplus = 0.1, payback_ideal = 0,
                    # suggestions = as.vector(pre_matrix_coefficients),
                    popSize = 100, maxiter = 500, run = 500)

coefficients_optimum <- optim_results@solution[1, ]
coefficients_optimum = matrix(data = coefficients_optimum, ncol = n_community, nrow = n_sunny_hours, byrow = T)

matrix_coefficients_optimum_0.1 = coefficients_optimum/rowSums(coefficients_optimum)

matrix_coefficients_optimum = matrix_coefficients_optimum_0.1

# TODO: this should be added all in a function
# checking:
df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_optimum)
plot_solar_consumption_daily_mean_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected, df_local_time)
plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)
plot_disaggregated_daily_mean_community_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)
plot_economic_comparison_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], matrix_coefficients = matrix_coefficients_optimum, df_local_time = df_local_time)


############################# CASE's comparison plots #############################

# TODO: will change the fitness_2_betas and observe how this plots change
plot_comparison_coefficients(df_gen = df_gen, df_cons_selected_users = df_cons[, combination_selected==1], matrix_coefficients_1, matrix_coefficients_2, df_local_time)

payback_ideal = 0
weight_surplus = 0.1

payback_years = calculate_payback_betas(df_cons[df_local_time$sunny, combination_selected==1], df_gen[df_local_time$sunny,], individual_investment_selected, matrix_coefficients = matrix_coefficients_non_optimum)
surplus = colSums(calculate_surplus_hourly_individual_betas(matrix_coefficients_non_optimum, df_gen[df_local_time$sunny,], df_cons[df_local_time$sunny, combination_selected==1]))

cost_payback = sum(exp(payback_years - payback_ideal))
cost_surplus = sum(surplus)

score = weight_surplus * cost_surplus + (1-weight_surplus) * cost_payback

print(payback_years)
print(surplus)
print(cost_payback)
print(cost_surplus)
print(score)

###

payback_years = calculate_payback_betas(df_cons[df_local_time$sunny, combination_selected==1], df_gen[df_local_time$sunny,], individual_investment_selected, matrix_coefficients = pre_matrix_coefficients)
surplus = colSums(calculate_surplus_hourly_individual_betas(pre_matrix_coefficients, df_gen[df_local_time$sunny,], df_cons[df_local_time$sunny, combination_selected==1]))

cost_payback = sum(exp(payback_years - payback_ideal))
cost_surplus = sum(surplus)

score = weight_surplus * cost_surplus + (1-weight_surplus) * cost_payback

print(payback_years)
print(surplus)
print(cost_payback)
print(cost_surplus)
print(score)

###

payback_years = calculate_payback_betas(df_cons[df_local_time$sunny, combination_selected==1], df_gen[df_local_time$sunny,], individual_investment_selected, matrix_coefficients = matrix_coefficients_optimum)
surplus = colSums(calculate_surplus_hourly_individual_betas(matrix_coefficients_optimum, df_gen[df_local_time$sunny,], df_cons[df_local_time$sunny, combination_selected==1]))

cost_payback = sum(exp(payback_years - payback_ideal))
cost_surplus = sum(surplus)

score = weight_surplus * cost_surplus + (1-weight_surplus) * cost_payback

print(payback_years)
print(surplus)
print(cost_payback)
print(cost_surplus)
print(score)

# from here I can see that the optimum tends tu be simmilar to the "matrix_coefficients_non_optimum" (where the repartition is equi-distributed 1/n_community) 
# I think this is because the investment is proportional to the consumption..
# will try changing this

plot_comparison_coefficients(df_gen = df_gen, df_cons_selected_users = df_cons[, combination_selected==1], matrix_coefficients_1 = pre_matrix_coefficients, matrix_coefficients_2 = matrix_coefficients_optimum, df_local_time)



# TODO:
# try suggestions:
# including the suggestion or not including it gives simmilar results
# try different weight_surplus
# payback ideal as a function of the minimum payback obtained in the optim_1??


payback_ideal = min(calculate_payback_betas(df_cons[df_local_time$sunny, combination_selected==1], df_gen[df_local_time$sunny,], individual_investment_selected, matrix_coefficients = pre_matrix_coefficients))/2
surplus_ideal = colSums(calculate_surplus_hourly_individual_betas(pre_matrix_coefficients, df_gen[df_local_time$sunny,], df_cons[df_local_time$sunny, combination_selected==1]))

optim_results <- ga(type = "real-valued", fitness = fitness_2_betas, 
                    lower = array(0, dim = dim), upper = array(1, dim = dim),  
                    df_gen_day = df_gen_sunny, df_cons_selected_day = df_cons_selected, combination = combination_selected, 
                    individual_investment = individual_investment_selected, 
                    weight_surplus = 0.1, payback_ideal = payback_ideal, 
                    # suggestions = as.vector(pre_matrix_coefficients),
                    popSize = 100, maxiter = 1000) # run = 1000

coefficients_optimum <- optim_results@solution[1, ]
coefficients_optimum = matrix(data = coefficients_optimum, ncol = n_community, nrow = n_sunny_hours, byrow = T)

matrix_coefficients_optimum_0.1_payback_payback_ideal = coefficients_optimum/rowSums(coefficients_optimum)


matrix_coefficients_optimum = matrix_coefficients_optimum_0.1


print_scores_matrix(matrix_coefficients_optimum = matrix_coefficients_optimum_0.1, weight_surplus = 0.1, payback_ideal = 0)
print_scores_matrix(matrix_coefficients_optimum = pre_matrix_coefficient, weight_surplus = 0.1, payback_ideal = 0)
print_scores_matrix(matrix_coefficients_optimum = matrix_coefficients_non_optimum, weight_surplus = 0.1, payback_ideal = 0)


pre_surplus = calculate_surplus_hourly_individual_betas(pre_matrix_coefficients, df_gen_sunny, df_cons_selected)

print_scores_matrix = function(matrix_coefficients_optimum, weight_surplus, payback_ideal){
  payback_years = calculate_payback_betas(df_cons[df_local_time$sunny, combination_selected==1], df_gen[df_local_time$sunny,], individual_investment_selected, matrix_coefficients = matrix_coefficients_optimum)
  surplus = colSums(calculate_surplus_hourly_individual_betas(matrix_coefficients_optimum, df_gen[df_local_time$sunny,], df_cons[df_local_time$sunny, combination_selected==1]))
  
  cost_payback = sum(exp(payback_years - payback_ideal))
  cost_surplus = sum(surplus)
  
  score = weight_surplus * cost_surplus + (1-weight_surplus) * cost_payback
  
  print("payback years")
  print(payback_years)
  print("cost payback")
  print(cost_payback)
  
  print("surplus")
  print(surplus)
  print("cost surplus")
  print(cost_surplus)
  
  print("score")
  print(score)

  print(payback_years - payback_ideal)
}

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

plot_comparison_coefficients(df_gen = df_gen, df_cons_selected_users = df_cons[, combination_selected==1], matrix_coefficients_1 = pre_matrix_coefficients, matrix_coefficients_2 = matrix_coefficients_optimum, df_local_time)











# comparison of matrixes:
# TODO: graphical comparison of matrixes?
# pre_matrix_coefficients vs matrix_coefficients_optimum 






