# PLAN 1

library(ggplot2)

# Import data of PV generation 
filename_1 = "202005081411_charts_compare.csv"
df_generation_0 <- read.csv(file = filename_1, header = TRUE)
colnames(df_generation_0) <- c("time", "gen_0")
df_generation_0$time <- as.POSIXct(as.character(df_generation_0$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid")
df_generation_0_day_1 <- df_generation_0[grepl(pattern = "2019-06-01", df_generation_0$time), ]

# Generate artificial data of consumption (some curves are based on the generation curve and others are totally artificial)
time <- df_generation_0_day_1$time
generation <- df_generation_0_day_1$gen_0
cons_A_1 <- generation
cons_A_2 <- generation * 0.5
# include white noise in this case just to be able to calculate chisq.test
# TODO: think about including white noise in all of the cases 
area_under_curve_gen = AUC(x = 1:length(generation), y = generation, method = "spline")
constant_value = area_under_curve_gen/24
white_noise = rnorm(length(generation), mean = 0.001 * constant_value, sd = 0.005  * constant_value)
cons_A_3 <- rep(constant_value, times = length(generation)) + white_noise
cons_A_4 <- c(dnorm(c(0:11), mean = 5, sd = 1.5), dnorm(c(12:23), mean = 17, sd = 1.5))
area_under_curve_cons_A_4 = AUC(x = 1:length(generation), y = cons_A_4, method = "spline")
factor_A_4 = area_under_curve_gen/area_under_curve_cons_A_4 
cons_A_4 <- c(dnorm(c(0:11), mean = 5, sd = 1.5), dnorm(c(12:23), mean = 17, sd = 1.5)) * factor_A_4
cons_A_5 <- c(dnorm(c(0:19), mean = 10, sd = 1.5), dnorm(c(20:23), mean = 21, sd = 0.5))
area_under_curve_cons_A_5 = AUC(x = 1:length(generation), y = cons_A_5, method = "spline")
factor_A_5 = area_under_curve_gen/area_under_curve_cons_A_5 
cons_A_5 <- c(dnorm(c(0:19), mean = 10, sd = 1.5), dnorm(c(20:23), mean = 21, sd = 0.5)) * factor_A_5

# Data frame containing the artificial data generated previously and the generation data
df_day_1_A <- data.frame("time" = time,
                         "gen_0" = generation,
                         "cons_1" = cons_A_1, 
                         "cons_2" = cons_A_2, 
                         "cons_3" = cons_A_3,
                         "cons_4" = cons_A_4,
                         "cons_5" = cons_A_5)

# TO PLOT:
# df_plot <- melt(data = df_day_1_A, id.vars = "time", variable.name = "series")
# ggplot(df_plot) + geom_line(aes(time, cons, color = series)) + 
#   # facet_wrap(series ~ ., scales = "free_y") +
#   theme()

# Objective 1: 
# which are the n users (for an n fixed) that minimice the total_surplus?
# which are the corresponding fixed_coefficients for each of thesen user previously obtained?

# Size of the proposed community 
n = 3

# CHEATING TO GENERATE BIG POSITIVE SURPLUS
generation <- generation * 3

df_surplus <- data.frame("time" = time)
vector_values <- c(1:(sum(grepl(pattern = "cons", x = colnames(df_day_1_A)))))
for (i in vector_values) {
  
  # i = 2
  consumption <- df_day_1_A[, grep(pattern = paste0("cons_", i), x = colnames(df_day_1_A))]
  # TODO: I think the new definition of distance should be included here 
  surplus <- ifelse(generation - consumption >= 0, generation - consumption, 0)       
  df_surplus_aux <- data.frame("time" = time, 
                               "surplus" = surplus)
  colnames(df_surplus_aux)[2] <- paste0("user_",i) 
  df_surplus <- merge(df_surplus, df_surplus_aux, by = "time")
  # df_surplus[, grep(pattern = paste0("cons_",i), colnames(df_surplus))] <- as.data.frame(surplus)
  
}
 
df_plot <- melt(data = df_surplus, id.vars = "time", variable.name = "series")

# TODO: check why the geom_line is != to the geom_area
# to me the geom_area is more reasonable
# ggplot(df_plot) +
#   # geom_line(aes(time, value, color = series)) + 
#   geom_area(aes(time, value, fill = series)) +
#   # facet_wrap(series ~ ., scales = "free_y") +
#   theme()

library("gtools")
df_total_surplus_combinations <- data.frame("time" = time)
df_combinations <- as.data.frame(combinations(n = length(vector_values), r = n, vector_values))
df_surplus_only_users <- df_surplus[, !(grepl(pattern = "time", x = colnames(df_surplus)))]

for (i in 1:nrow(df_combinations)) {
  
  set_users <- df_combinations[i, ] 
  total_consumption <- rowSums(df_surplus_only_users[,as.numeric(set_users)])  
  total_surplus <- ifelse(generation - total_consumption >= 0, generation - total_consumption, 0)     
  df_total_surplus_aux <- data.frame("time" = time, 
                                     "cons" = total_consumption,
                                     "surplus" = total_surplus)
  name <- paste(set_users[1],set_users[2],set_users[3], sep = "_")
  colnames(df_total_surplus_aux)[2] <- paste0("cons_", name)
  colnames(df_total_surplus_aux)[3] <- paste0("surplus_", name)

  df_total_surplus_combinations <- merge(df_total_surplus_combinations, df_total_surplus_aux, by = "time")
}

df_total_surplus_cut <- df_total_surplus_combinations[, grepl(pattern = "*time*|*surplus*", x = colnames(df_total_surplus_combinations))]

# df_plot <- melt(data = df_total_surplus_cut, id.vars = "time", variable.name = "series")
# 
# ggplot(df_plot) +
#   geom_line(aes(time, value, color = series)) +
#   theme()

daily_surplus <- colSums(x = df_total_surplus_cut[, !(grepl("time", colnames(df_total_surplus_cut)))])

ordered_daily_surplus <- daily_surplus[order(daily_surplus)]

optimum_combination <- ordered_daily_surplus[1]
names_optimum_combination <- strsplit(x = as.character(names(optimum_combination)), split = "_")[[1]]
vector_names_optimum_combination <- as.numeric(names_optimum_combination[!grepl(pattern = "surplus", x = names_optimum_combination)])

df_consumption_optimum_combination <- df_day_1_A[, grep(pattern = paste0(vector_names_optimum_combination, collapse = "|"), x = colnames(df_day_1_A))]

# Calculate stat_coeffs (VERSION 1)
# daily_user_consumption <- colSums(x = df_consumption_optimum_combination)
# total_daily_user_consumption <- sum(daily_user_consumption)
# static_coefficients <- daily_user_consumption/total_daily_user_consumption

# Calculate stat_coeffs (VERSION 2)
# To calculate the coefficients it is important to remember that the only users that should
# be taken into account are the ones that have been selected
df_surplus_total <- rowSums(df_surplus_only_users[, grepl(pattern = paste0(as.character(vector_names_optimum_combination), collapse = "|"), x = colnames(df_surplus_only_users))])
static_coefficients_hourly <- df_surplus_only_users/df_surplus_total
static_coefficients_daily <- colSums(static_coefficients_hourly, na.rm = TRUE)/sum(rowSums(static_coefficients_hourly), na.rm = T)

# now I will calculate the surplus if this HOURLY coefficients where used to assign 
# the energy repartition
df_new_assignation_using_hourly_coeffs <- data.frame("time" = time)

for (i in vector_names_optimum_combination) {
  
  # i = 1
  consumption <- df_day_1_A[, grep(pattern = paste0("cons_", i), x = colnames(df_day_1_A))]
  generation_assigned <- generation * static_coefficients_hourly[, paste0("user_",i)]
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
  # df_surplus[, grep(pattern = paste0("cons_",i), colnames(df_surplus))] <- as.data.frame(surplus)
  
}

df_gen_assigned <- df_new_assignation_using_hourly_coeffs[, grepl(pattern = "*time*|*gen_assigned*", x = colnames(df_new_assignation_using_hourly_coeffs))]
df_gen_assigned_total <- rowSums(x = df_gen_assigned[, !(grepl("time", colnames(df_gen_assigned)))])
surplus_total <- generation - df_gen_assigned_total

df_plot <- melt(data = df_gen_assigned, id.vars = "time", variable.name = "series")
ggplot(df_plot) + geom_area(aes(time, value, fill = series)) +
  # facet_wrap(series ~ ., scales = "free_y") +
  theme()



# now I will calculate the surplus if this DAILY coefficients where used to assign 
# the energy repartition
df_new_assignation_using_daily_coeffs <- data.frame("time" = time)

for (i in vector_names_optimum_combination) {
  
  # i = 2
  consumption <- df_day_1_A[, grep(pattern = paste0("cons_", i), x = colnames(df_day_1_A))]
  generation_assigned <- generation * as.numeric(static_coefficients_daily[paste0("user_",i)])
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
  # df_surplus[, grep(pattern = paste0("cons_",i), colnames(df_surplus))] <- as.data.frame(surplus)
  
}

df_gen_assigned <- df_new_assignation_using_daily_coeffs[, grepl(pattern = "*time*|*gen_assigned*", x = colnames(df_new_assignation_using_daily_coeffs))]
df_gen_assigned_total <- rowSums(x = df_gen_assigned[, !(grepl("time", colnames(df_gen_assigned)))])
surplus_total <- generation - df_gen_assigned_total

df_plot <- melt(data = df_gen_assigned, id.vars = "time", variable.name = "series")
ggplot(df_plot) + geom_area(aes(time, value, fill = series)) +
  # facet_wrap(series ~ ., scales = "free_y") +
  theme()



