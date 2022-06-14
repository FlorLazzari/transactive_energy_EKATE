# COEFICIENTES ESTÁTICOS
# Predeterminados, proporcionales a la inversión inicial. Este tipo de repartición permite que cada inversor 
# obtenga un porcentaje de la generación, aún en los momentos en los que su consumo es nulo, 
# lo cual lleva a grandes excedentes para la comunidad.
# 
# COEFICIENTES DINÁMICOS (horarios)
# Se calculan valorando tanto la inversión inicial de cada usuario como la 
# proporción de consumo con respecto al consumo acumulado de todas los participantes. 
# De esta forma se permite que el consorcio sea rentable para cada inversor y que 
# el excedente de generación fotovoltaica global se minimice.    

##########################################################################################################
# import libraries
library(lubridate)
library(ggplot2)
library(reshape2)
library(GA)
library(parallel)
# and functions
source("functions.R")

##########################################################################################################
# import data (used buildings which are in a radius of 500mts)
filename_1 = "202005081411_charts_compare.csv"
filename_2 = "202005081413_charts_compare.csv"

df_pv_generation_0 <- read.csv(file = filename_1, header = TRUE)
df_cons_0 <- read.csv(file = filename_2, header = TRUE)

colnames(df_pv_generation_0) <- c("time", "gen_0")
colnames(df_cons_0) <- c("time", "cons_1")

df_pv_generation_0$time <- as.POSIXct(as.character(df_pv_generation_0$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
df_cons_0$time <- as.POSIXct(as.character(df_cons_0$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 

filename_cons_1 = "202005081655_charts_compare.csv"
filename_cons_2 = "202005081656_charts_compare.csv"
filename_cons_3 = "202005081658_charts_compare.csv"

df_cons_1 <- read.csv(file = filename_cons_1, header = TRUE)
df_cons_2 <- read.csv(file = filename_cons_2, header = TRUE)
df_cons_3 <- read.csv(file = filename_cons_3, header = TRUE)

colnames(df_cons_1) <- c("time", "cons_2")
colnames(df_cons_2) <- c("time", "cons_3")
colnames(df_cons_3) <- c("time", "cons_4")

df_cons_1$time <- as.POSIXct(as.character(df_cons_1$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
df_cons_2$time <- as.POSIXct(as.character(df_cons_2$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
df_cons_3$time <- as.POSIXct(as.character(df_cons_3$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 

# select only a random day just to start (day 1 had some problems for user 3):
df_cons_1_day_1 <- df_cons_1[grepl(pattern = "2019-06-01", df_cons_1$time), ]
df_cons_2_day_1 <- df_cons_2[grepl(pattern = "2019-06-01", df_cons_2$time), ]
df_cons_3_day_1 <- df_cons_3[grepl(pattern = "2019-06-01", df_cons_3$time), ]
# CHEATING
df_cons_3_day_1$cons_4 <- df_cons_3_day_1$cons_4*c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8,
                                                   1.8, 1.8, 2.7, 2.7, 2.7, 2.7, 2.7, 1.8, 1.8, 1.8, 1.8, 1.8)
df_pv_generation_0_day_1 <- df_pv_generation_0[grepl(pattern = "2019-06-01", df_pv_generation_0$time), ]
df_cons_0 <- df_cons_0[grepl(pattern = "2019-06-01", df_cons_0$time), ] 
# CHEATING
df_cons_0$cons_1 <- df_cons_0$cons_1 * 0.3
df_cons_0$cons_1 <- df_cons_0$cons_1*c(1.8, 1.8, 1.8, 1.8, 1.8, 2.7, 2.7, 2.7, 1.8, 1.8, 1.8, 1.8,
                                       0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)

df_day_1 <- merge(df_cons_1_day_1, df_cons_2_day_1, by = "time")
df_day_1 <- merge(df_day_1, df_cons_3_day_1, by = "time")
df_day_1 <- merge(df_day_1, df_pv_generation_0_day_1, by = "time")
df_day_1 <- merge(df_day_1, df_cons_0, by = "time")

df_day_1_plot <- df_day_1 
colnames(df_day_1_plot)[2] <- 2 
colnames(df_day_1_plot)[3] <- 3 
colnames(df_day_1_plot)[4] <- 4 
colnames(df_day_1_plot)[5] <- "PV_generation" 
colnames(df_day_1_plot)[6] <- 1 

df_day_1_plot <- df_day_1_plot[, order(colnames(df_day_1_plot))]
df_plot <- melt(data = df_day_1_plot, id.vars = "time", variable.name = "series")
df_sum <- data.frame("time" = df_day_1$time,
                     "consumption_sum" = rowSums(df_day_1[, c(2, 3, 4, 6)]))
ggplot() + 
  geom_line(aes(hour(df_plot$time), df_plot$value , color = df_plot$series)) +
  geom_area(aes(x = hour(df_sum$time), y = df_sum$consumption_sum), alpha = 0.5) +
  labs(x = "Time [h]", y = "Electrical energy [kWh]", "title" = "Electrical Generation and Consumption", color = "")  

time <- df_day_1[, grepl(pattern = "time", x = colnames(df_day_1))]
generation <- df_day_1[, grepl(pattern = "gen", x = colnames(df_day_1))]

##########################################################################################################
# VERSION 1 and 2

# Objective: 
# which are the n users (for an FIXED n) that minimice the total_surplus?
# which are the corresponding fixed_coefficients for each of these users previously obtained?

# Size of the proposed community 
n = 2

##########################################################################################################
# for a small example I can solve the minimization problem looking for the ANALYTICAL SOLUTION

df_surplus <- data.frame("time" = time)
vector_values <- c(1:(sum(grepl(pattern = "cons", x = colnames(df_day_1)))))

for (i in vector_values) {
  consumption <- df_day_1[, grep(pattern = paste0("cons_", i), x = colnames(df_day_1))]
  surplus <- ifelse(generation - consumption >= 0, generation - consumption, 0)
  df_surplus_aux <- data.frame("time" = time,
                               "surplus" = surplus)
  colnames(df_surplus_aux)[2] <- paste0("user_",i)
  df_surplus <- merge(df_surplus, df_surplus_aux, by = "time")
}

library("gtools")
df_total_surplus_combinations <- data.frame("time" = time)
df_combinations <- as.data.frame(combinations(n = length(vector_values), r = n, vector_values))

for (i in 1:nrow(df_combinations)) {
  set_users <- df_combinations[i, ] 
  total_consumption <- rowSums(df_day_1[,grepl(pattern = paste0(set_users, collapse = "|"), x = colnames(df_day_1))])  
  total_surplus <- ifelse(generation - total_consumption >= 0, generation - total_consumption, 0)     
  df_total_surplus_aux <- data.frame("time" = time, 
                                     "cons" = total_consumption,
                                     "surplus" = total_surplus)
  name <- paste(set_users, collapse = "_")
  colnames(df_total_surplus_aux)[2] <- paste0("cons_", name)
  colnames(df_total_surplus_aux)[3] <- paste0("surplus_", name)
  df_total_surplus_combinations <- merge(df_total_surplus_combinations, df_total_surplus_aux, by = "time")
}

df_total_surplus_cut <- df_total_surplus_combinations[, grepl(pattern = "*time*|*surplus*", x = colnames(df_total_surplus_combinations))]
daily_surplus <- colSums(x = df_total_surplus_cut[, !(grepl("time", colnames(df_total_surplus_cut)))])

ordered_daily_surplus <- daily_surplus[order(daily_surplus)]

optimum_combination <- ordered_daily_surplus[1]
names_optimum_combination <- strsplit(x = as.character(names(optimum_combination)), split = "_")[[1]]
vector_names_optimum_combination <- as.numeric(names_optimum_combination[!grepl(pattern = "surplus", x = names_optimum_combination)])

df_consumption_optimum_combination <- df_day_1[, grep(pattern = paste0(vector_names_optimum_combination, collapse = "|"), x = colnames(df_day_1))]

# Calculate stat_coeffs (VERSION 1)
# daily_user_consumption <- colSums(x = df_consumption_optimum_combination)
# total_daily_user_consumption <- sum(daily_user_consumption)
# static_coefficients <- daily_user_consumption/total_daily_user_consumption

# Calculate stat_coeffs (VERSION 2)
df_surplus_only_users <- df_surplus[, !(grepl(pattern = "time", x = colnames(df_surplus)))]
df_sum_surplus <- rowSums(df_surplus_only_users[, grepl(pattern = paste0(as.character(vector_names_optimum_combination), collapse = "|"), x = colnames(df_surplus_only_users))])
static_coefficients_hourly <- 1 - df_surplus_only_users[, grepl(pattern = paste0(as.character(vector_names_optimum_combination), collapse = "|"), x = colnames(df_surplus_only_users))]/df_sum_surplus
static_coefficients_daily <- colSums(static_coefficients_hourly, na.rm = TRUE)/sum(rowSums(static_coefficients_hourly), na.rm = T)

# now I will calculate the surplus if this HOURLY coefficients where used to assign 
# the energy repartition
df_new_assignation_using_hourly_coeffs <- data.frame("time" = time)

for (i in vector_names_optimum_combination) {
  consumption <- df_day_1[, grep(pattern = paste0("cons_", i), x = colnames(df_day_1))]
  generation_to_assign <- generation * static_coefficients_hourly[, paste0("user_",i)]
  generation_assigned <- ifelse(generation_to_assign >= consumption, consumption, generation_to_assign)
  generation_assigned[is.na(generation_assigned)] <- 0
  consumption_grid <- ifelse(consumption - generation_assigned >= 0, consumption - generation_assigned, 0) 
  surplus <- generation - generation_assigned
  
  df_surplus_aux <- data.frame("time" = time, 
                               "gen_assigned" = generation_assigned,
                               "cons_grid" = consumption_grid, 
                               "surplus" = surplus)
  
  colnames(df_surplus_aux)[2] <- paste0("gen_assigned_",i) 
  colnames(df_surplus_aux)[3] <- paste0("cons_grid_",i) 
  colnames(df_surplus_aux)[4] <- paste0("surplus_",i) 
  df_new_assignation_using_hourly_coeffs <- merge(df_new_assignation_using_hourly_coeffs, df_surplus_aux, by = "time")
}

df_gen_assigned <- df_new_assignation_using_hourly_coeffs[, grepl(pattern = "*time*|*gen_assigned*", x = colnames(df_new_assignation_using_hourly_coeffs))]
# HARDCODED only to do the plot:
colnames(df_gen_assigned)[2] <- 1 
colnames(df_gen_assigned)[3] <- 4 

df_gen_assigned_total <- rowSums(x = df_gen_assigned[, !(grepl("time", colnames(df_gen_assigned)))])
surplus_total_hourly <- generation - df_gen_assigned_total
surplus_total_sum_hourly <- sum(surplus_total_hourly)  
print(surplus_total_sum_hourly)

df_plot_gen_assigned <- melt(data = df_gen_assigned, id.vars = "time", variable.name = "series")
df_plot_generation <- melt(df_day_1[, grep(pattern = paste0(c("0","time"), collapse = "|"), x = colnames(df_day_1))], id.vars = "time", variable.name = "series")

p <- ggplot() +
  geom_line(aes(x = hour(df_plot_generation$time), y = df_plot_generation$value)) +
  geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) + 
  labs(x = "Time [h]", y = "PV generation [kWh]", "title" = "PV dynamic assignation", fill = "User")  

# now I will calculate the surplus if this DAILY coefficients where used to assign 
# the energy repartition
df_new_assignation_using_daily_coeffs <- data.frame("time" = time)

for (i in vector_names_optimum_combination) {
  consumption <- df_day_1[, grep(pattern = paste0("cons_", i), x = colnames(df_day_1))]
  generation_to_assign <- generation * as.numeric(static_coefficients_daily[paste0("user_",i)])
  generation_assigned <- ifelse(generation_to_assign >= consumption, consumption, generation_to_assign)
  consumption_grid <- ifelse(consumption - generation_assigned >= 0, consumption - generation_assigned, 0) 
  surplus <- generation - generation_assigned
  df_surplus_aux <- data.frame("time" = time, 
                               "gen_assigned" = generation_assigned,
                               "cons_grid" = consumption_grid, 
                               "surplus" = surplus)
  
  colnames(df_surplus_aux)[2] <- paste0("gen_assigned_",i) 
  colnames(df_surplus_aux)[3] <- paste0("cons_grid_",i) 
  colnames(df_surplus_aux)[4] <- paste0("surplus_",i) 
  df_new_assignation_using_daily_coeffs <- merge(df_new_assignation_using_daily_coeffs, df_surplus_aux, by = "time")
}

df_gen_assigned <- df_new_assignation_using_daily_coeffs[, grepl(pattern = "*time*|*gen_assigned*", x = colnames(df_new_assignation_using_daily_coeffs))]
# HARDCODED:
colnames(df_gen_assigned)[2] <- 1 
colnames(df_gen_assigned)[3] <- 4 

df_gen_assigned_total <- rowSums(x = df_gen_assigned[, !(grepl("time", colnames(df_gen_assigned)))])
surplus_total <- generation - df_gen_assigned_total
surplus_total_sum_daily <- sum(surplus_total)
print(surplus_total_sum_daily)

df_plot_gen_assigned <- melt(data = df_gen_assigned, id.vars = "time", variable.name = "series")
df_plot_generation <- melt(df_day_1[, grep(pattern = paste0(c("0","time"), collapse = "|"), x = colnames(df_day_1))], id.vars = "time", variable.name = "series")

ggplot() +
  geom_line(aes(x = hour(df_plot_generation$time), y = df_plot_generation$value)) +
  geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) +
  labs(x = "Time [h]", y = "PV generation [kWh]", "title" = "PV static assignation", fill = "User")  

##########################################################################################################
# but when the number of consumers enlarges I need to use an OPTIMIZATION ALGORITHM
# (the problem is that the genetic algorithm sometimes doesnt converge)

min_repartition = 0
max_repartition = 95

features <- list("0" = list(levels = c(as.character(seq(from = min_repartition, to = max_repartition, by = 1)), NA), class = "discrete"),
                 "1" = list(levels = c(as.character(seq(from = min_repartition, to = max_repartition, by = 1)), NA), class = "discrete"),
                 "2" = list(levels = c(as.character(seq(from = min_repartition, to = max_repartition, by = 1)), NA), class = "discrete"),
                 "3" = list(levels = c(as.character(seq(from = min_repartition, to = max_repartition, by = 1)), NA), class = "discrete")
)

df_cons <- df_day_1[, c("cons_1", "cons_2", "cons_3", "cons_4")]

df_generation <- data.frame("cons_1" = generation, 
                            "cons_2" = generation, 
                            "cons_3" = generation, 
                            "cons_4" = generation)

nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){length(i[["levels"]])},features)))
class_per_feature = mapply(function(i){i[['class']]},features)
nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features)
levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
names_per_feature = names(features)

optimization_results <- suppressMessages(
  ga(
    type = "binary",
    fitness = optimizer,
    nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){length(i[["levels"]])},features))),
    class_per_feature = mapply(function(i){i[['class']]},features),
    nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
    levels_per_feature = lapply(function(i){i[["levels"]]}, X = features), 
    names_per_feature = names(features),
    # df_price = df_price,  
    selection = gabin_tourSelection,
    df_generation = df_generation, 
    df_cons = df_cons,
    # suggestions = suggestions,
    keepBest = TRUE,
    popSize = 64,
    maxiter = 20,
    monitor = gaMonitor,
    parallel = 16,
    elitism = 0.08,
    pmutation = 0.05
  )
)

optimization_results <- as.numeric(decodeValueFromBin(binary_representation = optimization_results_MPC@solution[1,],
                                                        class_per_feature = mapply(function(i){i[['class']]},features),
                                                        nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
                                                        levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
))





