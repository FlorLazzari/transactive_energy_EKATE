# import libraries
library(lubridate)
library(ggplot2)
library(reshape2)
library(GA)
library(parallel)
library(purrr)
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
df_day_1$cons_14 = df_day_1$cons_14 * 1.6

p = initial_plot(df_day_1)

df_gen = data.frame("gen_1" = df_day_1[, "gen_1"])
df_cons = df_day_1[, grep(pattern = "cons", x = colnames(df_day_1))]

# Size of the proposed community
# TODO: This "maximum size" should be determined by the maximum of df_gen
n_community = 4

n_periods = 1
periods <- calculate_params_period(n_periods)

# a good estimation of the overall investment is:  
# 1000*kWpico
# if the max consumption is in summer I can approximate: 
global_investment = max(df_gen)*1100

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

n_binary_rep = log(ncol(df_cons), base=2)

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

n_binary_rep = log(ncol(df_cons), base=2)

tic = Sys.time()
# optimal_combination_GA_payback <- optimize_GA_payback(n_periods, periods, n_community, n_binary_rep, df_gen, df_cons, )
toc = Sys.time()
time_GA_payback = toc-tic
surplus_GA_payback = sum(calculate_surplus_individual(df_gen, df_cons, optimal_combination_GA_payback))
individual_profit_GA_payback = calculate_individual_profit(df_gen, df_cons, optimal_combination_GA_payback)
rate_payback_GA_payback = individual_profit_GA_payback/individual_investment

#######################################################################################
# GA optimization taking account the economic aspect
# using 2 GA 

# for the case of 16 users:
n_binary_rep = log(ncol(df_cons), base=2)

tic = Sys.time()
optimal_combination_using_2_GAs <- optimize_using_2_GAs(n_community, n_binary_rep, df_gen, df_cons, individual_investment)
toc = Sys.time()
time_GA_payback = toc-tic
one_year_margin_optimum = calculate_one_year_margin(df_gen, df_cons[, optimal_combination_using_2_GAs!=0], optimal_combination_using_2_GAs[optimal_combination_using_2_GAs!=0], investment_selected)
number_years_payback_optimum = individual_investment[optimal_combination_using_2_GAs!=0]/one_year_margin_optimum
surplus_hourly_individual_optimum = calculate_surplus_hourly_individual(df_gen, df_cons[, optimal_combination_using_2_GAs!=0], optimal_combination_using_2_GAs[optimal_combination_using_2_GAs!=0])
surplus_individual_optimum = colSums(surplus_hourly_individual_optimum)
surplus_optimum = sum(surplus_hourly_individual_optimum)

print(optimal_combination_using_2_GAs)
print(number_years_payback_optimum)
print(surplus_individual_optimum)
print(surplus_optimum)

#######################################################################################
# GA optimization taking account the economic aspect
# using 2 GA - ALL IN ONE (one_cost_function)

n_binary_rep = log(ncol(df_cons), base=2)

tic = Sys.time()
optimal_combination_using_2_GAs_one_cost_function <- optimize_using_2_GAs_one_cost_function(n_community, n_binary_rep, df_gen, df_cons, individual_investment)
toc = Sys.time()
time_GA_payback = toc-tic
one_year_margin_optimum_2 = calculate_one_year_margin(df_gen, df_cons[, optimal_combination_using_2_GAs_one_cost_function!=0], optimal_combination_using_2_GAs_one_cost_function[optimal_combination_using_2_GAs_one_cost_function!=0], investment_selected)
number_years_payback_optimum_2 = individual_investment[optimal_combination_using_2_GAs_one_cost_function!=0]/one_year_margin_optimum_2
surplus_hourly_individual_optimum_2 = calculate_surplus_hourly_individual(df_gen, df_cons[, optimal_combination_using_2_GAs_one_cost_function!=0], optimal_combination_using_2_GAs_one_cost_function[optimal_combination_using_2_GAs_one_cost_function!=0])
surplus_individual_optimum_2 = colSums(surplus_hourly_individual_optimum_2)
surplus_optimum_2 = sum(surplus_hourly_individual_optimum_2)

print(optimal_combination_using_2_GAs_one_cost_function)
print(number_years_payback_optimum_2)
print(surplus_individual_optimum_2)
print(surplus_optimum_2)

#######################################################################################
# NEW
# nested GA 
# with new definition of surplus energy
n_binary_rep = log(ncol(df_cons), base=2)
# TODO: should change this
individual_investment = rep(global_investment/n_community, ncol(df_cons))

tic = Sys.time()
optimal_combination_using_2nested_GAs <- optimize_using_2nested_GAs(n_community, n_binary_rep, df_gen, df_cons, individual_investment)
toc = Sys.time()
time_GA_payback = toc-tic


#######################################################################################
# NEW
# nested GA with selection of best answers in the begining  
n_binary_rep = log(ncol(df_cons), base=2)
# TODO: should change this
individual_investment = rep(global_investment/n_community, ncol(df_cons))

optimal_combination_using_2nested_GAs <- optimize_using_2nested_GAs_withBestSoltuionSelection(n_community, n_binary_rep, df_gen, df_cons, individual_investment)
surplus = sum(calculate_surplus_hourly_individual(df_gen, df_cons, optimal_combination_using_2nested_GAs))  

############
coefficients_x = optimal_combination_using_2nested_GAs[optimal_combination_using_2nested_GAs != 0]

df_gen_assigned = calculate_gen_assigned(df_gen, optimal_combination_using_2nested_GAs[optimal_combination_using_2nested_GAs != 0])
df_cons_selected = df_cons[, optimal_combination_using_2nested_GAs != 0]
individual_investment_selected = individual_investment[optimal_combination_using_2nested_GAs != 0]  

surplus_x <- df_gen_assigned - df_cons_selected
surplus_x[surplus_x < 0] = 0

cost_surplus = sum(surplus_x)

purchase_price = 0.14859
sale_price = 0.0508

cost_old = colSums(purchase_price*df_cons_selected)

grid_x = df_cons_selected - df_gen_assigned
grid_x[grid_x < 0] = 0

cost_sun = purchase_price*colSums(grid_x) - sale_price * coefficients_x * sum(surplus_x)

profit_period = cost_old - cost_sun
length_period = nrow(df_cons_selected)
profit_one_year = profit_period * 24*360 / length_period

payback_years = individual_investment_selected / profit_one_year 
payback_ideal = 4

cost_payback = sum(exp(payback_years - payback_ideal))
score <- cost_surplus * cost_payback
############  

payback_years_2 = payback_years
coefficients_x_2 = coefficients_x
surplus_2 = surplus


# for the mixed integer linear programming
# write.csv(x = t(df_day_1), file = "df_day_1", row.names = FALSE)



