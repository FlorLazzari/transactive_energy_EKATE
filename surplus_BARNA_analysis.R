# COEFICIENTES ESTÁTICOS
# Predeterminados, proporcionales a la inversión inicial. Este tipo de repartición permite que cada inversor obtenga un porcentaje de la generación, aún en los momentos en los que su consumo es nulo, lo cual lleva a grandes excedentes para la comunidad.
# 
# COEFICIENTES DINÁMICOS (horarios)
# Se calculan valorando tanto la inversión inicial de cada usuario como la proporción de consumo con respecto al consumo acumulado de todas los participantes. De esta forma se permite que el consorcio sea rentable para cada inversor y que el excedente de generación fotovoltaica global se minimice.    



library(ggplot2)

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

# artificial data just to check 
# => works perfect because the assigned coefficient for each user is 0.7 and 0.3 !!  
# df_cons_5 <- data.frame("time" = df_day_1$time,
#                         "cons_5" = df_day_1$gen_0*0.7)
# df_cons_6 <- data.frame("time" = df_day_1$time,
#                         "cons_6" = df_day_1$gen_0*0.3)
# df_day_1 <- merge(df_day_1, df_cons_5, by = "time")
# df_day_1 <- merge(df_day_1, df_cons_6, by = "time")

library(reshape2)
# plot done!

# HARDCODED:
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
p <- ggplot() + geom_line(aes(hour(df_plot$time), df_plot$value , color = df_plot$series)) +
      geom_area(aes(x = hour(df_sum$time), y = df_sum$consumption_sum), alpha = 0.5) +
      labs(x = "Time [h]", y = "Electrical energy [kWh]", "title" = "Electrical Generation and Consumption", color = "")  

ggsave(filename = "electrical_generation_and_consumption.pdf", plot = p, width = 10)

surplus_all_users <- ifelse(df_pv_generation_0_day_1$gen_0 - df_sum$consumption_sum >= 0, df_pv_generation_0_day_1$gen_0 - df_sum$consumption_sum, 0)
sum(surplus_all_users)

##############################################################
# PLAN 1

time <- df_day_1[, grepl(pattern = "time", x = colnames(df_day_1))]
generation <- df_day_1[, grepl(pattern = "gen", x = colnames(df_day_1))]

# Objective 1: 
# which are the n users (for an n fixed) that minimice the total_surplus?
# which are the corresponding fixed_coefficients for each of thesen user previously obtained?

# Size of the proposed community 
n = 2

df_surplus <- data.frame("time" = time)
vector_values <- c(1:(sum(grepl(pattern = "cons", x = colnames(df_day_1)))))
for (i in vector_values) {

  # i = 2
  consumption <- df_day_1[, grep(pattern = paste0("cons_", i), x = colnames(df_day_1))]
  # TODO: I think the new definition of distance should be included here
  surplus <- ifelse(generation - consumption >= 0, generation - consumption, 0)
  df_surplus_aux <- data.frame("time" = time,
                               "surplus" = surplus)
  colnames(df_surplus_aux)[2] <- paste0("user_",i)
  df_surplus <- merge(df_surplus, df_surplus_aux, by = "time")
  # df_surplus[, grep(pattern = paste0("cons_",i), colnames(df_surplus))] <- as.data.frame(surplus)

}

# df_plot <- melt(data = df_surplus, id.vars = "time", variable.name = "series")

# TODO: check why the geom_line is != to the geom_area
# to me the geom_area is more reasonable
# ggplot(df_plot) +
#   geom_line(aes(time, value, color = series)) +
#   # geom_area(aes(time, value, fill = series)) +
#   # facet_wrap(series ~ ., scales = "free_y") +
#   theme()

library("gtools")
df_total_surplus_combinations <- data.frame("time" = time)
df_combinations <- as.data.frame(combinations(n = length(vector_values), r = n, vector_values))
# df_surplus_only_users <- df_surplus[, !(grepl(pattern = "time", x = colnames(df_surplus)))]

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
# To calculate the coefficients it is important to remember that the only users that should
# be taken into account are the ones that have been selected
df_surplus_only_users <- df_surplus[, !(grepl(pattern = "time", x = colnames(df_surplus)))]
df_sum_surplus <- rowSums(df_surplus_only_users[, grepl(pattern = paste0(as.character(vector_names_optimum_combination), collapse = "|"), x = colnames(df_surplus_only_users))])
# df_surplus_total_aux <- generation - rowSums(df_day_1[, grepl(pattern = paste0(as.character(vector_names_optimum_combination), collapse = "|"), x = colnames(df_day_1))])
# df_surplus_total <- ifelse(df_surplus_total_aux >= 0 , df_surplus_total_aux, 0 )
static_coefficients_hourly <- 1 - df_surplus_only_users[, grepl(pattern = paste0(as.character(vector_names_optimum_combination), collapse = "|"), x = colnames(df_surplus_only_users))]/df_sum_surplus
static_coefficients_daily <- colSums(static_coefficients_hourly, na.rm = TRUE)/sum(rowSums(static_coefficients_hourly), na.rm = T)
static_coefficients_daily[1] <- 0.5
static_coefficients_daily[2] <- 0.5

# now I will calculate the surplus if this HOURLY coefficients where used to assign 
# the energy repartition
df_new_assignation_using_hourly_coeffs <- data.frame("time" = time)

for (i in vector_names_optimum_combination) {
  
  # i = 1
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
  # df_surplus[, grep(pattern = paste0("cons_",i), colnames(df_surplus))] <- as.data.frame(surplus)
  
}

df_gen_assigned <- df_new_assignation_using_hourly_coeffs[, grepl(pattern = "*time*|*gen_assigned*", x = colnames(df_new_assignation_using_hourly_coeffs))]
# HARDCODED:
colnames(df_gen_assigned)[2] <- 1 
colnames(df_gen_assigned)[3] <- 4 

df_gen_assigned_total <- rowSums(x = df_gen_assigned[, !(grepl("time", colnames(df_gen_assigned)))])
surplus_total_hourly <- generation - df_gen_assigned_total
surplus_total_sum_hourly <- sum(surplus_total_hourly)  
print(surplus_total_sum_hourly)

df_plot_gen_assigned <- melt(data = df_gen_assigned, id.vars = "time", variable.name = "series")
df_plot_generation <- melt(df_day_1[, grep(pattern = paste0(c("0","time"), collapse = "|"), x = colnames(df_day_1))], id.vars = "time", variable.name = "series")

# plot done!
# p <- ggplot() +
#       geom_area(aes(x = df_plot_gen_assigned$time, y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series)) +
#       geom_line(aes(x = df_plot_generation$time, y = df_plot_generation$value))  

p <- ggplot() +
  # geom_line(aes(x = df_plot_gen_assigned$time, y = df_plot_gen_assigned$value, color = df_plot_gen_assigned$series)) +
  geom_line(aes(x = hour(df_plot_generation$time), y = df_plot_generation$value)) +
  geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) + 
  labs(x = "Time [h]", y = "PV generation [kWh]", "title" = "PV dynamic assignation", fill = "User")  

ggsave(filename = "dynamic_assigned.pdf", plot = p, width = 10)

# now I will calculate the surplus if this DAILY coefficients where used to assign 
# the energy repartition
df_new_assignation_using_daily_coeffs <- data.frame("time" = time)

for (i in vector_names_optimum_combination) {
  
  # i = 2
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
  # df_surplus[, grep(pattern = paste0("cons_",i), colnames(df_surplus))] <- as.data.frame(surplus)
  
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
# df_plot <- rbind(df_plot_gen_assigned, df_plot_generation)
# df_plot$series <- as.factor(df_plot$series)

# plot done!
# p <- ggplot() +
#       geom_area(aes(x = df_plot_gen_assigned$time, y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series)) +
#       geom_line(aes(x = df_plot_generation$time, y = df_plot_generation$value))  
  
# ifelse(df_plot_gen_assigned$series  

p <- ggplot() +
  # geom_line(aes(x = df_plot_gen_assigned$time, y = df_plot_gen_assigned$value, color = df_plot_gen_assigned$series)) +
  geom_line(aes(x = hour(df_plot_generation$time), y = df_plot_generation$value)) +
  geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) +
  labs(x = "Time [h]", y = "PV generation [kWh]", "title" = "PV static assignation", fill = "User")  

ggsave(filename = "static_assignation.pdf", plot = p,  width = 10)






# plot done!
df_plot <- data.frame()
# Comparing methods plotting only for each user
for (i in vector_names_optimum_combination) {
  
  # i = 1
  df_plot_consumptions <- melt(data = df_day_1[, grep(pattern = paste0(c(i, "time"), collapse = "|"), x = colnames(df_day_1))], id.vars = "time", variable.name = "series")
  df_gen_assigned_daily <- df_new_assignation_using_daily_coeffs[, grepl(pattern = paste0(c(paste0(c("cons_grid_",i), collapse = ""), "time"), collapse = "|"), x = colnames(df_new_assignation_using_daily_coeffs))]
  colnames(df_gen_assigned_daily)[2] <- paste0("cons_grid_daily_", i) 
  df_plot_gen_assigned_daily <- melt(data = df_gen_assigned_daily, id.vars = "time", variable.name = "series")
  
  df_gen_assigned_hourly <- df_new_assignation_using_hourly_coeffs[, grepl(pattern = paste0(c(paste0(c("cons_grid_",i), collapse = ""), "time"), collapse = "|"), x = colnames(df_new_assignation_using_daily_coeffs))]
  colnames(df_gen_assigned_hourly)[2] <- paste0("cons_grid_hourly_", i) 
  df_plot_gen_assigned_hourly <- melt(data = df_gen_assigned_hourly, id.vars = "time", variable.name = "series")
  
  df_plot <- rbind(df_plot, df_plot_consumptions, df_plot_gen_assigned_daily, df_plot_gen_assigned_hourly)
}

# TODO: this only works for 2 users
# df_plot$user <- ifelse(grepl(pattern = as.character(vector_names_optimum_combination[1]), x = as.character(df_plot$series)), vector_names_optimum_combination[1], vector_names_optimum_combination[2])
# 
# p <- ggplot(df_plot) +
#       geom_line(aes(x = time, y = value, colour = series))  +
#       facet_wrap(facets = "user", nrow = 2)
# 
# ggsave(filename = "hola.pdf", plot = p,  width = 10)

# TODO:
# plot of accumulated grid consumption
df_gen_assigned <- df_new_assignation_using_daily_coeffs[, grepl(pattern = "*time*|*gen_assigned*", x = colnames(df_new_assignation_using_daily_coeffs))]
df_gen_assigned_total <- rowSums(x = df_gen_assigned[, !(grepl("time", colnames(df_gen_assigned)))])


# plot done!
# plot showing that the surplus using hourly coefficients is better

group = c("static", "dynamic")
value = c(surplus_total_sum_daily, surplus_total_sum_hourly)

df_plot <- data.frame("group" = group,
                      "surplus" = value
                      )
# TODO: check sign!
percentage <- round((surplus_total_sum_daily - surplus_total_sum_hourly) / surplus_total_sum_hourly, digits = 2)*100

# Barplot
bp <- ggplot(df_plot, aes(x = group, y = surplus, fill = group))+
  geom_bar(width = 1, stat = "identity", alpha = 0.6) + 
  # ggtitle(paste0("Surplus gain = ", percentage, "%")) + 
  labs(x = "Type of coefficient", y = "PV surplus [kWh]", title = paste0("Surplus gain = ", percentage, "%")) +
  theme(legend.position = "none")
  
ggsave(filename = "surplus_gain_different_coefficients.pdf", plot = bp, width = 4)


ordered_daily_surplus <- daily_surplus[order(daily_surplus)]
df_plot <- melt(ordered_daily_surplus)
vector_names_combination <- c()

for (i in 1:length(ordered_daily_surplus)) {
  names_combination <- strsplit(x = as.character(names(ordered_daily_surplus)), split = "_")[[i]]
  names_combination <- names_combination[!grepl(pattern = "surplus", x = names_combination)]
  names_combination <- paste0(names_combination, collapse = "_")
  vector_names_combination <- c(vector_names_combination, names_combination)
}

df_plot$combination <- vector_names_combination 
rownames(df_plot) <- NULL
# df_plot$value <- as.factor(df_plot$value)

mean_surplus <- mean(df_plot$value)
min_surplus <- df_plot$value[1]
percentage <- round((mean_surplus - min_surplus) / mean_surplus, digits = 2)*100

# plot done!
bp <- ggplot(df_plot, aes(x = combination, y = value, fill = combination))+
  geom_bar(width = 1, stat = "identity", alpha = 0.6) + 
  # ggtitle(paste0("PV Surplus gain = ", percentage, "%")) +
  labs(x = "Possible user combination", y = "PV surplus [kWh]", "title" = paste0("PV Surplus gain = ", percentage, "%")) + 
  theme(legend.position = "none")

ggsave(filename = "possible_user_combination.pdf", plot = bp, width = 4)
