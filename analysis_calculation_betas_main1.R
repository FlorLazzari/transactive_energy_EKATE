# meeting barna november

# objective: show results for all their PV installations
# important: in the community graph paint with different color the consumption of the building which is sharing the PV  

# TODO: 
# second: check the consumers graphs (their are picks everywhere)
# third: run the 2nd optimization for the random selections 
# finally: will run everything for all the installations and choose one to show

############################# MAIN 1 #############################

building = 1

# there are 8 PV installations
# we can compare the results of doing a community per PV generation 
# (show the optimum community is different for each PV generation, with the same group of 128 users)

list_tunning_PV = list("2" = list("max_cut" = 6), 
                       "7" = list("max_cut" = 50),
                       "12" = list("max_cut" = 13),
                       "13" = list("max_cut" = 6),
                       "17" = list("max_cut" = 150),
                       "21" = list("max_cut" = 25)
)

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

############################# data reading (genome project) #############################

selected_year_consumption = seq(from = as.POSIXct("2017-01-01 00:00:00"), to = as.POSIXct("2017-12-31 00:00:00"), by = "hour")
df_meter = import_data_genome_project(selected_year_consumption)

colnames(df_meter)[2:ncol(df_meter)] = paste0("cons_",1:(ncol(df_meter)-1))

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
n_users = 128

# checking:
# df_meter_characteristic_test = df_meter_characteristic[!duplicated(as.list(df_meter_characteristic))]

# filter users with flat curves:
filter = colSums(apply(X = df_meter_characteristic, MARGIN = 2, FUN = diff) == 0)
# colSums(df_meter_characteristic < 1)
df_meter_characteristic_filtered = df_meter_characteristic[ ,filter < nrow(df_meter_characteristic)*0.1]
# colMeans(df_meter_characteristic_filtered)


# De acuerdo con Red Eléctrica de España (REE),
# el consumo de kWh mensual de un hogar español es de unos 270 kWh,
# lo que hace un total de 3272 kWh/año aproximadamente

# In 2020, the average annual electricity consumption for a U.S. residential utility customer was 10715 kilowatthours (kWh), an average of about 893 kWh per month
# 893/30 =~ 30 per day

# spain = 3200
# USA = 10700

# will use 4000

original = df_meter_characteristic_filtered

readjusting = (4000/360)/24

big_cols = colMeans(df_meter_characteristic_filtered) > readjusting & colMeans(df_meter_characteristic_filtered) < 10
df_meter_characteristic_filtered[, big_cols] = (readjusting) * (df_meter_characteristic_filtered[big_cols]/as.numeric(colMeans(df_meter_characteristic_filtered)[big_cols]))

huge_cols = colMeans(df_meter_characteristic_filtered) > 10
df_meter_characteristic_filtered[, huge_cols] = (readjusting/3) * (df_meter_characteristic_filtered[huge_cols]/as.numeric(colMeans(df_meter_characteristic_filtered)[huge_cols]))

# checking:
mean(colMeans(df_meter_characteristic_filtered))
readjusting
hist(colMeans(df_meter_characteristic_filtered))

# select the last ones
df_meter_characteristic_filtered = df_meter_characteristic_filtered[order(colMeans(df_meter_characteristic_filtered))]
df_cons = df_meter_characteristic_filtered[, grep(pattern = "cons", x = colnames(df_meter_characteristic_filtered))[1:n_users]]
# mean(colMeans(df_cons))
# hist(colMeans(df_cons))

# TODO:
# df_cons = eliminate_outliers(df = df_cons, max_cut = list_tunning[[2]]$max_cut)
# plot_generation(df_generation = data.frame("time" = 1:nrow(df_cons), "energy" = df_cons$cons_6))

############################# data reading (barcelona - PV generation) #############################

filename_gen = paste0("data_update/",names(list_tunning_PV)[[building]],"_PV.csv")
df_gen = import_one_user(filename_1 = filename_gen)
df_gen = eliminate_outliers(df = df_gen, max_cut = list_tunning_PV[[building]]$max_cut)
# plot_generation(df_gen)

to_remove_1 = which(as.character(df_gen$time) %in% "2017-10-29 02:00:00")
to_remove_2 = which(as.character(df_gen$time) %in% "2018-10-28 02:00:00")
to_remove_3 = which(as.character(df_gen$time) %in% "2019-10-27 02:00:00")
to_remove_4 = which(as.character(df_gen$time) %in% "2020-10-25 02:00:00")
to_remove_5 = which(as.character(df_gen$time) %in% "2021-10-31 02:00:00")

df_gen_complete = df_gen[-c(to_remove_1, to_remove_2, to_remove_3, to_remove_4, to_remove_5), ]

# define characteristic days for each month 
# TODO: defining week and week end characteristic days doesnt make sense for generation
df_gen_characteristic = calculate_characteristic_days_years_2(df_gen_complete = df_gen_complete, years = unique(year(df_gen_complete$time)))
df_gen = dplyr::bind_rows(df_gen_characteristic, .id = "column_label")

# plot_generation(df_generation = data.frame("time" = 1:nrow(df_gen), "energy" = df_gen$energy))

df_local_time = data.frame(#"time" = selected_year_generation, 
  "month" = as.numeric(df_gen$column_label),
  "date" = rep(c(rep(1, 24), rep(2, 24)), 12), 
  "hour" = df_gen$hour,
  "sunny" = (df_gen$energy > 0.4)
)

df_gen = df_gen[, c(1, 3)]
df_gen_sunny = df_gen[df_local_time$sunny, 2]
df_cons_sunny = df_cons[df_local_time$sunny, ]

############################# tariff structure #############################

# TODO: look for reference
# TODO: consider the weekends?
# df_purchase_price_one_day_ = data.frame("price" = c(rep(0.15,8), rep(0.18,2), rep(0.26,4), rep(0.15,4), rep(0.26,4), rep(0.18,2)),
#                                        "time" = c(0:23))

df_purchase_price_one_day = data.frame("price" = c(0.1957, 0.1986, 0.1948, 0.1890, 0.1891, 0.2014, 0.2192, 0.2316, 0.2498, 0.2429, 0.2391, 0.2198, 0.2354, 0.2375, 0.2317, 0.2440, 0.2468, 0.2598, 0.2905, 0.2990, 0.2978, 0.2738, 0.2553, 0.2534),
                                       "time" = c(0:23)
                                       )

df_purchase_price = rbind(df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day, df_purchase_price_one_day)

purchase_price_sunny = df_purchase_price[df_local_time$sunny, "price"]

############################# define n_community_min #############################

# TODO: recalculate this n_community, should always use summer months to calculate the community max

n_community_min = calculate_n_community_min(generation = df_gen$energy, consumption = df_cons)
# checking:
# p_cons_max = plot_generation(df_generation = data.frame("time" = 1:nrow(df_cons), "energy" = df_cons[ ,which.max(colMeans(df_cons))]))


############################# cheating re-define df_gen #############################

# redefining the gen curve to make it more interesting 
df_gen_energy_original = df_gen$energy
# identical(df_gen_energy_original, df_gen_complete$energy)
ampliffier = 45/max(df_gen_energy_original) 

if (n_community_min < 3) {
  print("amplifier")
  df_gen$energy = df_gen_energy_original * ampliffier
  n_community_min = calculate_n_community_min(generation = df_gen$energy, consumption = df_cons)
  plot_generation(df_generation = data.frame("time" = 1:nrow(df_gen), "energy" = df_gen$energy))
  df_gen_sunny = df_gen[df_local_time$sunny, 2]
}

global_investment = max(df_gen$energy, na.rm = T)*1400

# colMeans(df_cons_sunny)
# mean(df_gen_sunny)

# TODO: try thetwo versions, one in which the surplus is small but non-cero and the other in which it is cero!
# n_community_min = n_community_min+1

