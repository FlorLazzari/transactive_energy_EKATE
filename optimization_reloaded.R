# import libraries
library(lubridate)
library(ggplot2)
library(reshape2)
library(GA)
library(parallel)
# and functions
source("functions_new.R")

##########################################################################################################
# example

filename_gen_1 = "202005081411_charts_compare.csv"
filename_cons_1 = "202005081413_charts_compare.csv"
filename_cons_2 = "202005081655_charts_compare.csv"
filename_cons_3 = "202005081656_charts_compare.csv"
filename_cons_4 = "202005081658_charts_compare.csv"
filenames_list = list(filename_gen_1, filename_cons_1, filename_cons_2, filename_cons_3, filename_cons_4)

df = lapply(X = filenames_list, FUN = import_one_user)

df_gen_1 = df[[1]]
df_cons_1 = df[[2]]
df_cons_2 = df[[3]]
df_cons_3 = df[[4]]
df_cons_4 = df[[5]]

df_day_1 = generate_fake_data_test2(df_gen_1, df_cons_1, df_cons_2, df_cons_3, df_cons_4)

p = initial_plot(df_day_1)

df_gen = data.frame("gen_1" = df_day_1[, "gen_1"])
df_cons = df_day_1[, grep(pattern = "cons", x = colnames(df_day_1))]

# Size of the proposed community 
n_community = 3

n_periods = 1
periods <- calculate_params_period(n_periods)
individual_investment = rep(5000, ncol(df_cons))

#######################################################################################
# analytical optimization without taking account the economic aspect

tic = Sys.time()
optimal_combination_analytical <- optimize_analytical(n_periods, periods, n_community, df_gen, df_cons)
toc = Sys.time()
time_analytical = toc-tic
surplus_analytical = sum(calculate_surplus_individual(df_gen, df_cons, optimal_combination_analytical))
individual_profit_GA = calculate_individual_profit(df_gen, df_cons, optimal_combination_analytical)
rate_payback_analytical = individual_profit_analytical/individual_investment

#######################################################################################
# GA optimization without taking account the economic aspect

# for the case of 16 users:
n_binary_rep = 4

tic = Sys.time()
optimal_combination_GA <- optimize_GA(n_periods, periods, n_community, n_binary_rep, df_gen, df_cons)
toc = Sys.time()
time_GA = toc-tic
surplus_GA = sum(calculate_surplus_individual(df_gen, df_cons, optimal_combination_GA))
individual_profit_GA = calculate_individual_profit(df_gen, df_cons, optimal_combination_GA)
rate_payback_GA = individual_profit_GA/individual_investment

# the algorithm gives as a result all the local minimums.. some criateria to select one of these minms?
# The payback!
#######################################################################################
# GA optimization taking account the economic aspect
# number of "while" iterations, should change this strategy

n_balance_iterations = 30
# TODO: this is totally invented, no criteria
tolerance_rate_payback = 0.01
increment_payback = 0.1

tic = Sys.time()
optimal_combination_GA_payback <- optimize_GA_payback(n_periods, periods, n_community, n_binary_rep, df_gen, df_cons)
toc = Sys.time()
time_GA_payback = toc-tic
surplus_GA_payback = sum(calculate_surplus_individual(df_gen, df_cons, optimal_combination_GA_payback))
individual_profit_GA_payback = calculate_individual_profit(df_gen, df_cons, optimal_combination_GA_payback)
rate_payback_GA_payback = individual_profit_GA_payback/individual_investment


