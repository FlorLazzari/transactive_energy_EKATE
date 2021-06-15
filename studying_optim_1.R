
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

# TODO: what is making the algo take too long?

n_community_max = 3

df_cons_sunny = df_cons_sunny[,1:8]
n_binary_rep = log(ncol(df_cons_sunny), base=2)

combinations = list()

for (i in 1:3) {
  pre_optimal_combinations <- optimization_1(n_community = n_community_max, 
                                             n_binary_rep = n_binary_rep, 
                                             df_gen = df_gen_sunny, 
                                             df_cons = df_cons_sunny)
  
  combinations[[i]] = as.data.frame(pre_optimal_combinations)
}


hola <- function(x){
  sums = colSums(x)
  normalized_sums = sums/nrow(x)
  return(normalized_sums)
}

combinations_comparison = lapply(X = combinations, FUN = hola)



combinations_comparison = as.data.frame(combinations_comparison)
colnames(combinations_comparison) = c(1:ncol(combinations_comparison))
combinations_comparison$user = 1:nrow(combinations_comparison)

combinations_comparison_plot = melt(data = combinations_comparison, id.vars = "user") 

combinations_comparison_plot$user = factor(combinations_comparison_plot$user)
combinations_comparison_plot$variable = factor(combinations_comparison_plot$variable)

p <- ggplot() +
  geom_bar(aes(x = combinations_comparison_plot$user,  y = combinations_comparison_plot$value, fill = combinations_comparison_plot$variable), alpha = 0.5, width = 1, stat = "identity", position=position_dodge()) 

hola_4 <- function(x){
  
  y = c(0) 
  
  for (i in 1:7) {
    y_i = 10^(-i)
    y = c(y, y_i)
  }
  
  y = as.data.frame(y)
  
  result = as.matrix(x)%*%as.matrix(y)
  return(result)
}

c = lapply(X = combinations, FUN = hola_4)

length(c[[1]])
length(unique(c[[1]]))

c[[1]] %in% unique(c[[1]])[5]

# this shows evidence that a lot of combinations are being repeated
# TODO: study the way in which we are selecting the combinations
c[[1]][5]
c[[1]][11]
combinations[[1]][5, ]
combinations[[1]][11, ]

# conclusion: the combinations are repeating!
