############################# import libraries #############################

library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(GA)
library(parallel)
library(purrr)
source("functions.R")

############################# data reading #############################

filename_gen_1 = "data/202005081411_charts_compare.csv"
# cons_1 is too high
filename_cons_1 = "data/202005081413_charts_compare.csv"
filename_cons_2 = "data/202005081655_charts_compare.csv"
filename_cons_3 = "data/202005081656_charts_compare.csv"
filename_cons_4 = "data/202005081658_charts_compare.csv"
# new:
filename_cons_5 = "data/202103171643_charts_historic.csv"
# gen_5 is too low
filename_gen_5 = "data/202103171649_charts_historic.csv"
filename_cons_6 = "data/202103171657_charts_historic.csv"
# gen_7 is too low
filename_gen_7 = "data/202103171735_charts_historic_generation.csv"
filename_cons_7 = "data/202103171735_charts_historic_cons.csv"
filename_cons_8 = "data/202103171814_charts_historic.csv"
filename_cons_9 = "data/202103171831_charts_historic.csv"
filename_cons_10 = "data/202103171835_charts_historic.csv"
# gen_10 is too low
filename_gen_10 = "data/202103171838_charts_historic.csv"
filename_cons_11 = "data/202103171858_charts_historic.csv"
filename_cons_12 = "data/202103171905_charts_historic.csv"

filename_cons_13 = "data/202103181018_charts_historic.csv"
filename_cons_14 = "data/202103181023_charts_historic.csv"
filename_cons_15 = "data/202103181029_charts_historic.csv"
filename_cons_16 = "data/202103181032_charts_historic.csv"
filename_cons_17 = "data/202103181037_charts_historic.csv"
filename_cons_18 = "data/202103181040_charts_historic.csv"


filenames_list = list(filename_gen_1, filename_cons_2, filename_cons_3, filename_cons_4, filename_cons_5, filename_cons_6, filename_cons_7, filename_cons_8, filename_cons_9, filename_cons_11, filename_cons_12, filename_cons_13, filename_cons_14, filename_cons_15, filename_cons_16, filename_cons_17, filename_cons_18)
df = lapply(X = filenames_list, FUN = import_one_user)
df_month_1 = select_month(df, m=7)
df_month_1 = eliminate_outliers(df_month_1)
df_month_1 = reducing_consumption_fake(df_month_1)

p = plot_initial(df_month_1)

df_gen = data.frame("gen_1" = df_month_1[, "gen_1"])
df_cons = df_month_1[, grep(pattern = "cons", x = colnames(df_month_1))]

# changing NAs to 0
df_gen[is.na(df_gen)] = 0
df_cons[is.na(df_cons)] = 0


# should always use summer months to calculate the community max
n_community_max = calculate_n_community_max(generation = df_gen$gen_1, df_cons, time = df_month_1$time)
# n_community_max = 6

global_investment = max(df[[1]]$energy, na.rm = T)*1100

#######################################################################################

# generating fake info here: (should ask Eloi for new data)
df_cons = cbind(df_cons, df_cons)
colnames(df_cons)[17:32] = paste0(rep("cons_", 16),17:32)

n_binary_rep = log(ncol(df_cons), base=2)
# TODO: should change this
individual_investment = sapply(df_cons, max, na.rm = TRUE)*1100
  
# checking:
# sum(sapply(df_cons, max, na.rm = TRUE)*1100) > global_investment


# TODO: 
# why the first run has this error? is it still appearing?
# Error in gareal_lsSelection_Rcpp(object) :
#   Too few positive probabilities!
tic = Sys.time()
optimal_combination_using_2_GAs <- optimize_using_2_GAs_withBestSolutionSelection(n_community_max = n_community_max, n_binary_rep, df_gen, df_cons, global_investment, individual_investment)
toc = Sys.time()
toc-tic

# To check convergence and execution time
time = rep(0, 5)
optimum_surplus = rep(0, 5)
for (i in c(1:5)) {
  tic = Sys.time()
  optimal_combination_using_2_GAs <- optimize_using_2_GAs_withBestSolutionSelection(n_community_max = n_community_max, n_binary_rep, df_gen, df_cons, global_investment, individual_investment)
  toc = Sys.time()
  time[i] = toc-tic

  best_combination = selection_best_combination(optimal_combination_using_2_GAs)
  optimum_surplus[i] = best_combination$surplus
  
  plot_best_combination(best_combination, iteration = i)
}

plot_optimization1_vs_optimization2(optimal_combination_using_2_GAs)

best_combination = select_best_combinations(optimal_combination_using_2_GAs)
df_gen_assigned = calculate_gen_assigned(df_gen = df_gen, combination = best_combination$optimum_coefficients)
df_gen_assigned_selected = df_gen_assigned[,best_combination$optimum_coefficients != 0]

df_cons_selected = df_cons[,best_combination$optimum_coefficients != 0]

plot_solar_consumption_daily_mean(df_gen = df_gen, df_gen_assigned = df_gen_assigned_selected, time = df_month_1$time)
plot_disaggregated_daily_mean_per_user(df_gen_assigned = df_gen_assigned_selected, df_cons_selected = df_cons_selected, time = df_month_1[, "time"])
plot_disaggregated_daily_mean_community(df_gen_assigned = df_gen_assigned_selected, df_cons_selected = df_cons_selected, time = df_month_1[, "time"])
# the economic difference is negligible but this makes sense because the betas are not hourly yet (?)
# with hourly betas this should change
plot_economic_comparison(df_gen = df_gen, optimum_combination = best_combination$optimum_coefficients[best_combination$optimum_coefficients != 0], df_cons_selected = df_cons[best_combination$optimum_coefficients != 0])







# for the mixed integer linear programming
# write.csv(x = t(df_day_1), file = "df_day_1", row.names = FALSE)



