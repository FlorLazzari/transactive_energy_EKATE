############################# import libraries #############################

library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(GA)
library(parallel)
library(purrr)
library(nsga2R)
source("functions.R")

############################# select year periods #############################

selected_year_generation = seq(from = as.POSIXct("2020-01-01 00:00:00"), to = as.POSIXct("2020-12-31 00:00:00"), by = "hour")
selected_year_consumption = seq(from = as.POSIXct("2018-01-01 00:00:00"), to = as.POSIXct("2018-12-31 00:00:00"), by = "hour")

############################# data reading (barcelona - PV generation) #############################

filename_gen_1 = "data/202105251031_charts_compare.csv"

df_gen = import_one_user(filename_1 = filename_gen_1)
colnames(df_gen) = c("time", "gen_1")
df_gen$gen_1[df_gen$gen_1 == 0] = NA
df_gen = eliminate_outliers(df_gen)

p = plot_initial(df_gen)

df_gen = df_gen[as.Date(df_gen$time) %in% as.Date(selected_year_generation), ]

df_local_time_gen = data.frame("time" = selected_year_generation, 
                               "month" = month(selected_year_generation),
                               "date" = as.Date(selected_year_generation), 
                               "hour" = hour(selected_year_generation))

# problem here:
# df_local_time_gen$time[df_local_time_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST")]
# df_gen$time[df_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST")]

# df_gen[7150:7160,]
# df_gen_2[7150:7160,]

# will remove the second value 

df_gen = merge(x = data.frame("time" = df_local_time_gen[, "time"]), y = df_gen, by.x = "time", all.x = T)

to_remove = which(df_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST"))
df_gen = df_gen[-to_remove, ]
p = plot_initial(df_gen)

df_local_time_gen$sunny = (df_gen$gen_1 != 0 & !is.na(df_gen$gen_1))
df_gen_sunny = df_gen[df_local_time_gen$sunny,]
p = plot_initial(df_gen)

# TODO: this could be altering the results:
df_gen[is.na(df_gen$gen_1),"gen_1"] = 0

# define characteristic days for each month 
# TODO: defining week and week end characteristic days doesnt make sense for generation
df_gen_characteristic = calculate_characteristic_days(df = df_gen, number_selected_year = unique(year(selected_year_generation)))
df_gen = dplyr::bind_rows(df_gen_characteristic, .id = "column_label")

df_local_time = data.frame(#"time" = selected_year_generation, 
                           "month" = as.numeric(df_gen$column_label),
                           "date" = rep(c(rep(1, 24), rep(2, 24)), 12), 
                           "hour" = df_gen$hour)

df_local_time$sunny = (df_gen$energy != 0)

df_gen = df_gen[, c(1, 3)]
df_gen_sunny = df_gen[df_local_time$sunny, 2]

# checking:
# plot(x = 1:nrow(df_gen_characteristic[[12]]), y = df_gen_characteristic[[12]]$energy)

############################# data reading (genome project) #############################

selected_year_consumption = seq(from = as.POSIXct("2017-01-01 00:00:00"), to = as.POSIXct("2017-12-31 00:00:00"), by = "hour")
df_meter = import_data_genome_project(selected_year_consumption)
df_meter_characteristic = data.frame("time_int"=sort(rep(c(1:12), 24*2)))

for (i in 2:ncol(df_meter)) {
  # print(i)
  list_meter_characteristic = calculate_characteristic_days(df = df_meter[, c(1, i)], number_selected_year = unique(year(selected_year_consumption)))  
  # will discard the users that have any month without characteristic data
  if (any(is.na(list_meter_characteristic))) {
    # print("no")
  } else{
    df_meter_characteristic_i = dplyr::bind_rows(list_meter_characteristic, .id = "column_label")
    df_meter_characteristic = cbind(df_meter_characteristic, df_meter_characteristic_i[, 3])
    colnames(df_meter_characteristic)[ncol(df_meter_characteristic)] = paste0("cons_",ncol(df_meter_characteristic)-1)
  }
} 

# TODO: should give more importance to the week days, what if I multiply by 5 the price for the weekdays and by 2 the price for the weekend days?

# TODO
n_users = 128 
df_cons = df_meter_characteristic[, 2:(n_users+1)]

df_cons_sunny = df_cons[df_local_time$sunny, ]
# df_cons_sunny_2 = data.frame(df_cons_sunny, row.names = NULL)
# rownames(df_cons_sunny) = NULL

############################# define n_community_max #############################

# should always use summer months to calculate the community max
n_community_max = calculate_n_community_max(generation = df_gen$energy, consumption = df_cons[, c(2:ncol(df_cons))])
# n_community_max = 6

global_investment = max(df_gen$energy, na.rm = T)*1100

############################# run algo #############################

n_binary_rep = log(ncol(df_cons), base=2)
# TODO: should change this
individual_investment = sapply(df_cons, max, na.rm = TRUE)*1100

# checking:
# sum(sapply(df_cons, max, na.rm = TRUE)*1100) > global_investment

# tic = Sys.time()
# optimal_combination_using_2_GAs <- optimize_hourly_betas_multi_objective(hourly = T, weight_surplus = 0.5, n_community_max = n_community_max, n_binary_rep = n_binary_rep, df_gen_sunny = df_gen_sunny, df_cons_sunny = df_cons_sunny, global_investment = global_investment, individual_investment = individual_investment)
# toc = Sys.time()
# toc-tic

pre_optimal_combinations <- optimization_1(hourly = T, n_community = n_community_max, n_binary_rep = n_binary_rep, df_gen = df_gen_sunny, df_cons = df_cons_sunny)

n_community_per_combination_order = order(rowSums(pre_optimal_combinations))
pre_optimal_combinations = pre_optimal_combinations[n_community_per_combination_order, ]
n_community_vector = rowSums(pre_optimal_combinations)
hourly_surplus = apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus = colSums(hourly_surplus)

j = 1
vector_i = c()

for (i in 1:nrow(pre_optimal_combinations)) {

  combination_selected = pre_optimal_combinations[i, ]
  df_cons_selected_sunny = df_cons_sunny[,combination_selected==1]
  individual_investment_max = individual_investment[combination_selected==1]  
  
  if (sum(individual_investment_max) > global_investment) { 
    vector_i = c(vector_i, i)
    j = j + 1  
  }
}

pre_optimal_combinations = pre_optimal_combinations[vector_i,]

