# analysis selection users main

# main 1: data reading
# main 2: convergence studies

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

# p = plot_initial(name = "test_initial_0", df_gen)

df_gen = df_gen[as.Date(df_gen$time) %in% as.Date(selected_year_generation), ]

df_local_time_gen = data.frame("time" = selected_year_generation, 
                               "month" = month(selected_year_generation),
                               "date" = as.Date(selected_year_generation, tz = "CET"), 
                               "hour" = hour(selected_year_generation))
# df_local_time_gen = df_local_time_gen[-7155,]

# problem here:
# df_local_time_gen$time[df_local_time_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST")]
# df_gen$time[df_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST")]

# will remove the second value 

df_gen = merge(x = data.frame("time" = df_local_time_gen[, "time"]), y = df_gen, by.x = "time", all.x = T)

to_remove = which(df_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST"))
df_gen = df_gen[-to_remove, ]

to_remove = which(df_local_time_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST"))
df_local_time_gen = df_local_time_gen[-to_remove, ]
# p = plot_initial(name = "test_initial_1", df_gen)

df_local_time_gen$sunny = (df_gen$gen_1 != 0 & !is.na(df_gen$gen_1))
df_gen_sunny = df_gen[df_local_time_gen$sunny,]
# p = plot_initial(name = "test_initial_2", df_gen)

# TODO: this could be altering the results:
df_gen[is.na(df_gen$gen_1),"gen_1"] = 0

# define characteristic days for each month 
# TODO: defining week and week end characteristic days doesnt make sense for generation
df_gen_characteristic = calculate_characteristic_days(df = df_gen, number_selected_year = unique(year(selected_year_generation)))
df_gen = dplyr::bind_rows(df_gen_characteristic, .id = "column_label")


df_local_time = data.frame(#"time" = selected_year_generation, 
  "season" = c(rep("winter", 3*2*24), rep("mid_season", 3*2*24), rep("summer", 3*2*24), rep("mid_season", 3*2*24)),
  "month" = as.numeric(df_gen$column_label),
  "date" = rep(c(rep(1, 24), rep(2, 24)), 12), 
  "hour" = df_gen$hour)

df_local_time$sunny = (df_gen$energy != 0)

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


df_cons = df_meter_characteristic_filtered[, grep(pattern = "cons", x = colnames(df_meter_characteristic_filtered))[1:n_users]]



df_cons_sunny = df_cons[df_local_time$sunny, ]
# df_cons_sunny_2 = data.frame(df_cons_sunny, row.names = NULL)
# rownames(df_cons_sunny) = NULL

############################# define n_community_max #############################

# TODO: recalculate this n_community, should not be max, should be mean
# should always use summer months to calculate the community max
df_gen_energy_original = df_gen$energy
df_gen$energy = df_gen_energy_original*1.5
# df_gen = df_gen[, c(1, 3)]
df_gen_sunny = df_gen[df_local_time$sunny, 2]
n_community_max = calculate_n_community_max(generation = df_gen$energy, consumption = df_cons[, c(1:ncol(df_cons))])

n_community_max = 8

global_investment = max(df_gen$energy, na.rm = T)*1100

# TODO: should change the curve of df_gen_sunny because it has a very strange pattern in summer
p <- ggplot() +
  geom_line(aes(x = 1:length(df_gen_sunny), y = df_gen_sunny)) 
