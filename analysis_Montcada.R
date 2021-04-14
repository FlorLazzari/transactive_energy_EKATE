library(ggplot2)

filename_consumption = "consumption_escuela.csv"
df_cons = read.csv(file = filename_consumption, header = TRUE)
colnames(df_cons) = c("time", "cons")
# head(df_cons)

filename_generation = "generation_escuela.csv"
df_gen = read.csv(file = filename_generation, header = TRUE)
colnames(df_gen) = c("time", "gen")
# head(df_gen)

df_escuela = merge(x = df_cons, y = df_gen, by = "time")
# head(df_escuela)
# time_interval = 15 min
df_escuela$time = as.POSIXct(x = df_escuela$time/1000, origin = "1970-01-01", tz = "Europe/Madrid")

measuring_start_time = as.POSIXct(x = "2019-10-17 00:15:00", tz = "Europe/Madrid")
covid_start_time = as.POSIXct(x = "2020-02-01 00:15:00", tz = "Europe/Madrid")

# df safe covid and with measures
df_escuela = df_escuela[df_escuela$time < covid_start_time & df_escuela$time > measuring_start_time, ]

# filter negative values 
df_escuela = df_escuela[df_escuela$cons > 0, ]

df_escuela$surplus = ifelse((df_escuela$gen - df_escuela$cons) > 0,
                             df_escuela$gen - df_escuela$cons, 
                             0)

df_escuela$cons_PV =  ifelse(df_escuela$gen > df_escuela$cons, 
                             df_escuela$cons, 
                             df_escuela$gen)

df_escuela$cons_grid = df_escuela$cons - df_escuela$cons_PV

 
ggplot(df_escuela) +
  geom_line(aes(x = time, y = surplus)) +
  labs(x = "Time", y = "Energy [kWh]", title = "Hitorical Time Series") +
  theme(legend.title = element_blank())

# df_surplus = df_escuela[!is.na(df_escuela$surplus) & df_escuela$surplus > 2, ]
# 
# ggplot() +
#   geom_histogram(aes(hour(df_surplus$time)))
# 
# ggplot() +
#   geom_histogram(aes(df_surplus$surplus))
# 
# ggplot() +
#   geom_histogram(aes(month(df_surplus$time)))

ggplot(df_escuela) +
  geom_line(aes(x = time, y = cons, color = "Consumption")) +
  geom_line(aes(x = time, y = gen, color = "Generation")) + 
  labs(x = "Time", y = "Energy [kWh]", title = "Hitorical Time Series") +
  scale_color_manual(values = c("red", "green")) + 
  theme(legend.title = element_blank())


ggplot(df_escuela) +
  geom_line(aes(x = time, y = cons_PV, color = "Consumption")) +
  geom_line(aes(x = time, y = gen, color = "Generation")) + 
  labs(x = "Time", y = "Energy [kWh]", title = "Hitorical Time Series") +
  scale_color_manual(values = c("red", "green")) + 
  theme(legend.title = element_blank())

df_escuela$hour = hour(df_escuela$time) + 0.01*minute(df_escuela$time)

#  aggregated graph (dont show)
# df_hourly <- aggregate(df_escuela[, c("cons", "cons_PV", "gen", "surplus", "cons_grid")], list(df_escuela$hour), FUN = mean)
# colnames(df_hourly)[1] = "hour"
# df_plot <- melt(data = df_hourly[, c("hour", "cons_grid", "surplus", "cons_PV")], id.vars = "hour", variable.name = "series")
# df_hourly$consumption = df_hourly$cons

df_hourly <- aggregate(df_escuela[, c("cons", "cons_PV", "gen", "surplus")], list(df_escuela$hour), FUN = mean)
colnames(df_hourly)[1] = "hour"
df_plot <- melt(data = df_hourly[, c("hour", "surplus", "cons_PV")], id.vars = "hour", variable.name = "series")
df_hourly$consumption = df_hourly$cons

p <- ggplot() +
  geom_line(aes(x = df_hourly$hour, y = df_hourly$gen)) +
  # geom_line(aes(x = df_hourly$hour, y = df_hourly$consumption)) +
  geom_area(aes(x = df_plot$hour, y = df_plot$value, fill = df_plot$series), alpha = 0.5) + 
  labs(x = "Time [h]", y = "Energy [kWh]", "title" = "Mean PV assignation", fill = " ")  

# first I will do it for one day and then I will do the "hourly" sum (15 min)
vector_month = unique(month(df_escuela$time))
vector_days = c(1, 10, 20, 25)

for (month_i in vector_month) {
  for (day_i in vector_days) {
    
    df_escuela_one_month = df_escuela[month(df_escuela$time) == month_i, ]
    df_escuela_one_day = df_escuela_one_month[day(df_escuela_one_month$time) == day_i, ]
    
    if (nrow(df_escuela_one_day) != 0) {
      df_hourly <- aggregate(df_escuela_one_day[, c("cons", "cons_PV", "gen", "surplus", "cons_grid")], list(df_escuela_one_day$hour), FUN = sum)
      colnames(df_hourly)[1] = "hour"
      df_plot <- melt(data = df_hourly[, c("hour", "cons_grid", "surplus", "cons_PV")], id.vars = "hour", variable.name = "series")
      
      df_hourly$consumption = df_hourly$cons
      
      p <- ggplot() +
        geom_line(aes(x = df_hourly$hour, y = df_hourly$gen)) +
        geom_line(aes(x = df_hourly$hour, y = df_hourly$consumption), linetype = "dashed") +
        geom_area(aes(x = df_plot$hour, y = df_plot$value, fill = df_plot$series), alpha = 0.5) + 
        labs(x = "Time [h]", y = "Energy [kWh]", "title" = "Grid and PV consumption for one day", fill = "")  
      ggsave(filename = paste0("PV_assignation_month_",month_i,"_day_",day_i), plot = p, device = "jpg", width = 4, height = 2)
    }
  }
}


vector_month = unique(month(df_escuela$time))

df_ratio = data.frame("hour" = df_hourly$hour)
for (month_i in vector_month){
  
  # month_i = 11
  df_escuela_one_month = df_escuela[month(df_escuela$time) == month_i, ]
  
  df_hourly <- aggregate(df_escuela_one_month[, c("cons", "cons_PV", "gen", "surplus", "cons_grid")], list(df_escuela_one_month$hour), FUN = mean)
  colnames(df_hourly)[1] = "hour"
  df_plot <- melt(data = df_hourly[, c("hour", "cons_grid", "surplus", "cons_PV")], id.vars = "hour", variable.name = "series")
  
  df_hourly$consumption = df_hourly$cons
  
  df_plot <- melt(data = df_hourly[, c("hour", "surplus", "cons_PV")], id.vars = "hour", variable.name = "series")
  
  p2 <- ggplot() +
    geom_area(aes(x = df_plot$hour, y = df_plot$value, fill = df_plot$series), alpha = 0.5) + 
    labs(x = "Time [h]", y = "Energy [kWh]", "title" = "Mean PV assignation for a month", fill = "") +
    scale_fill_manual(values=c("#00b159", "#56B4E9"))
  ggsave(filename = paste0("PV_assignation_",month_i), plot = p2, device = "jpg", width = 4, height = 2)
  
  p3 <- ggplot() +
    geom_area(aes(x = df_hourly$hour, y = df_hourly$cons_PV), alpha = 0.5, fill = "#00b159") + 
    geom_hline(aes(yintercept = max(df_hourly$cons_PV)), color = "#00b159") +
    ylim(c(0,max(c(df_hourly$cons_PV,df_hourly$surplus))*1.05)) +
    labs(x = "Time [h]", y = "Energy [kWh]", "title" = "Surplus", fill = "") 

  p4 <- ggplot() +
    geom_area(aes(x = df_hourly$hour, y = df_hourly$surplus), alpha = 0.5, fill = "#56B4E9") + 
    geom_hline(aes(yintercept = max(df_hourly$surplus)), color = "#56B4E9") +
    ylim(c(0,max(c(df_hourly$cons_PV,df_hourly$surplus))*1.05)) +
    labs(x = "Time [h]", y = "Energy [kWh]", "title" = "PV consumption", fill = "") 
  
  p4_bis = grid.arrange(p3,p4,ncol=2)
  ggsave(filename = paste0("PV_assignation_separate_",month_i) , plot = p4_bis, device = "jpg", width = 7, height = 2)
    
  df_hourly$ratio = df_hourly$cons_PV/df_hourly$gen
  
  p5 <- ggplot() +
    geom_line(aes(x = df_hourly$hour, y = df_hourly$ratio)) + 
    ylim(c(0, 1)) +
    labs(x = "Time [h]", y = "", title = "PV_cons/PV_generation", fill = "") 
  ggsave(filename = paste0("PV_ratio_",month_i), plot = p5, device = "jpg", width = 3, height = 2)

  df_ratio = cbind(df_ratio, df_hourly$ratio)
  
}

df_ratio = df_ratio[2:length(vector_month)+1] 
colnames(df_ratio) = vector_month

p <- ggplot(df_ratio, aes(x = dose, y = len)) + 
  geom_boxplot()
p


