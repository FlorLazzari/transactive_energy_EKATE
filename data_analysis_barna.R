# meeting barna november


############################# MAIN 1 #############################

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

# selected_year_generation = seq(from = as.POSIXct("2020-01-01 00:00:00"), to = as.POSIXct("2020-12-31 00:00:00"), by = "hour")
# selected_year_consumption = seq(from = as.POSIXct("2018-01-01 00:00:00"), to = as.POSIXct("2018-12-31 00:00:00"), by = "hour")

############################# data reading (barcelona - PV generation) #############################

# there are 8 PV installations
# we can compare the results of doing a community per PV generation 
# (show the optimum community is different for each PV generation, with the same group of 128 users)

list_tunning = list("2" = list("max_cut" = 6
                               ),
                    "12" = list("max_cut" = 13
                               )
                    )

filename_gen = paste0("data_update/",names(list_tunning)[[2]],"_PV.csv")
df_gen = import_one_user(filename_1 = filename_gen)
# df_gen$energy[df_gen$energy == 0] = NA
df_gen = eliminate_outliers(df = df_gen, max_cut = list_tunning[[2]]$max_cut)
p = plot_generation(df_gen)
# df_gen = df_gen[as.Date(df_gen$time) %in% as.Date(selected_year_generation), ]

# solving the problem of the timezone 
# df_gen$time[df_gen$time[2] %in% as.POSIXct("2020-10-25 04:00:00 CEST")]


# df_local_time_gen = df_local_time_gen[-7155,]

# problem here:
# df_local_time_gen$time[df_local_time_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST")]
# df_gen$time[df_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST")]

# will remove the second value 

# df_gen = merge(x = data.frame("time" = df_local_time_gen[, "time"]), y = df_gen, by.x = "time", all.x = T)

to_remove_1 = which(as.character(df_gen$time) %in% "2017-10-29 02:00:00")
to_remove_2 = which(as.character(df_gen$time) %in% "2018-10-28 02:00:00")
to_remove_3 = which(as.character(df_gen$time) %in% "2019-10-27 02:00:00")
to_remove_4 = which(as.character(df_gen$time) %in% "2020-10-25 02:00:00")
to_remove_5 = which(as.character(df_gen$time) %in% "2021-10-31 02:00:00")

# I remove the values because they are inventing values (0.1) 
# df_gen[(to_remove_5-5):(to_remove_5+5), ]

df_gen = df_gen[-c(to_remove_1, to_remove_2, to_remove_3, to_remove_4, to_remove_5), ]

df_local_time_gen = data.frame("time" = df_gen$time, 
                               "month" = month(df_gen$time),
                               "date" = as.Date(df_gen$time, tz = "CET"), 
                               "hour" = hour(df_gen$time))

df_local_time_gen$sunny = (df_gen$energy != 0 & !is.na(df_gen$energy))
df_gen_sunny = df_gen[df_local_time_gen$sunny,]
# p = plot_initial(name = "test_initial_2", df_gen)

# TODO: this could be altering the results:
# df_gen[is.na(df_gen$gen_1),"gen_1"] = 0

# define characteristic days for each month 
# TODO: defining week and week end characteristic days doesnt make sense for generation
df_gen_characteristic = calculate_characteristic_days_years_2(df_gen_complete = df_gen, years = unique(year(df_gen$time)))
df_gen = dplyr::bind_rows(df_gen_characteristic, .id = "column_label")

plot_generation(df_generation = data.frame("time" = 1:nrow(df_gen), "energy" = df_gen$energy))

df_local_time = data.frame(#"time" = selected_year_generation, 
  # "season" = c(rep("winter", 3*2*24), rep("mid_season", 3*2*24), rep("summer", 3*2*24), rep("mid_season", 3*2*24)),
  "month" = as.numeric(df_gen$column_label),
  "date" = rep(c(rep(1, 24), rep(2, 24)), 12), 
  "hour" = df_gen$hour,
  "sunny" = (df_gen$energy > 0.4)
)

df_gen = df_gen[, c(1, 3)]
df_gen_sunny = df_gen[df_local_time$sunny, 2]

# checking:
# plot(x = 1:nrow(df_gen_characteristic[[12]]), y = df_gen_characteristic[[12]]$energy)

############################# tariff structure #############################

# TODO: look for reference
# TODO: consider the weekends?
df_purchase_price_one_day = data.frame("price" = c(rep(0.15,8), rep(0.18,2), rep(0.26,4), rep(0.15,4), rep(0.26,4), rep(0.18,2)),
                                       "time" = c(0:23))
df_purchase_price = rbind(df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day)

purchase_price_sunny = df_purchase_price[df_local_time$sunny, "price"]

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

# TODO! check duplicates
# df_meter_characteristic_test = df_meter_characteristic[!duplicated(as.list(df_meter_characteristic))]


# filter users with flat curves:
filter = colSums(apply(X = df_meter_characteristic, MARGIN = 2, FUN = diff) == 0)
# colSums(df_meter_characteristic < 1)
df_meter_characteristic_filtered = df_meter_characteristic[ ,filter < nrow(df_meter_characteristic)*0.1]
# colMeans(df_meter_characteristic_filtered)

# reduce the mean consumption:
big_cols = colMeans(df_meter_characteristic_filtered) > 10

# mean USA residential consumption = 10kWh
df_meter_characteristic_filtered[, big_cols] = df_meter_characteristic_filtered[, big_cols] * (9/ as.numeric(colMeans(df_meter_characteristic_filtered)[big_cols]))
# colMeans(df_meter_characteristic_filtered)

df_cons = df_meter_characteristic_filtered[, grep(pattern = "cons", x = colnames(df_meter_characteristic_filtered))[1:n_users]]
# colMeans(df_cons)

df_cons_sunny = df_cons[df_local_time$sunny, ]
# df_cons_sunny_2 = data.frame(df_cons_sunny, row.names = NULL)
# rownames(df_cons_sunny) = NULL

############################# define n_community_max #############################

# TODO: recalculate this n_community, should not be max, should be mean
# should always use summer months to calculate the community max
# df_gen_energy_original = df_gen$energy 
df_gen$energy = df_gen_energy_original * 5
df_gen_sunny = df_gen[df_local_time$sunny, 2]
n_community_max = calculate_n_community_max(generation = df_gen$energy, consumption = df_cons)
global_investment = max(df_gen$energy, na.rm = T)*1100

# colMeans(df_cons_sunny)
# mean(df_gen_sunny)

############################# MAIN 2 #############################

############################# ordering #############################
# TODO: ordering
n_binary_rep = log(ncol(df_cons_sunny), base=2)

############################# run model #############################
combination = optimization_1_to_analize(n_community = n_community_max, 
                                        n_binary_rep = n_binary_rep, 
                                        df_gen_to_optimize = df_gen_sunny, 
                                        df_cons_to_optimize = df_cons_sunny, 
                                        max_iter = 50)

# surplus = colSums(apply(X = as.matrix(df_list_combination_ordering), MARGIN = 2, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny))
# surplus = as.character(round(surplus, digits = 2))

# plot the aggregated consumption of the whole community without the PV but with the lines of each consumer

df_cons_selected_sunny = df_cons_sunny[, combination[1, ]==1]
matrix_coefficients_3 = calculate_matrix_coefficients(optimizer_number = 3, df_gen_sunny, df_cons_selected_sunny, n_community = sum(combination[1, ]))
# calulate_payback_surplus_for_matrix(matrix_coefficients_3, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected)
df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_3)
df_cons_selected_users = df_cons[, combination[1, ]==1]  

plot_disaggregated_community_betas_year_area(name = "test",
                                             df_gen_assigned = df_gen_assigned,
                                             df_cons_selected_users = df_cons_selected_users,
                                             df_local_time = df_local_time)







