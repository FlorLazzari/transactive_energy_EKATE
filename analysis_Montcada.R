library(ggplot2)
library(lubridate)

filename_consumption = "consumption_escuela.csv"
df_cons = read.csv(file = filename_consumption, header = TRUE)
colnames(df_cons) = c("time", "cons")
head(df_cons)

filename_generation = "generation_escuela.csv"
df_gen = read.csv(file = filename_generation, header = TRUE)
colnames(df_gen) = c("time", "gen")
head(df_gen)

df_escuela = merge(x = df_cons, y = df_gen, by = "time")
head(df_escuela)
# time_interval = 15 min
df_escuela$time = as.POSIXct(x = df_escuela$time/1000, origin = "1970-01-01", tz = "Europe/Madrid")

measuring_start_time = as.POSIXct(x = "2019-10-17 00:15:00", tz = "Europe/Madrid")
covid_start_time = as.POSIXct(x = "2020-02-01 00:15:00", tz = "Europe/Madrid")

# df safe covid and with measures
df_escuela = df_escuela[df_escuela$time < covid_start_time & df_escuela$time > measuring_start_time, ]

# filter negative values 
df_escuela = df_escuela[df_escuela$cons > 0, ]

df_escuela$dif = ifelse((df_escuela$gen - df_escuela$cons) > 0,
                        df_escuela$gen - df_escuela$cons, 
                        NA)

ggplot(df_escuela) +
  geom_line(aes(x = time, y = dif)) +
  labs(x = "Time", y = "Energy [kWh]", title = "Hitorical Time Series") +
  theme(legend.title = element_blank())

df_surplus = df_escuela[!is.na(df_escuela$dif) & df_escuela$dif > 2, ]

hour_dif = hour(df_surplus$time) 
ggplot() +
  geom_histogram(aes(hour_dif))

dif = df_surplus$dif 
ggplot() +
  geom_histogram(aes(dif))

month_dif = month(df_escuela$time[!is.na(df_escuela$dif)]) 
ggplot() +
  geom_histogram(aes(month_dif))

ggplot(df_escuela) +
  geom_line(aes(x = time, y = cons, color = "Consumption")) +
  geom_line(aes(x = time, y = gen, color = "Generation")) + 
  labs(x = "Time", y = "Energy [kWh]", title = "Hitorical Time Series") +
  scale_color_manual(values = c("red", "green")) + 
  theme(legend.title = element_blank())

df_escuela$cons_PV =  ifelse(df_escuela$gen > df_escuela$cons, df_escuela$cons, df_escuela$gen)

ggplot(df_escuela) +
  geom_line(aes(x = time, y = cons_PV, color = "Consumption")) +
  geom_line(aes(x = time, y = gen, color = "Generation")) + 
  labs(x = "Time", y = "Energy [kWh]", title = "Hitorical Time Series") +
  scale_color_manual(values = c("red", "green")) + 
  theme(legend.title = element_blank())

df_escuela$ratio =  ifelse(df_escuela$gen != 0, df_escuela$cons_PV/df_escuela$gen, NA) 

ggplot(df_escuela) +
  geom_histogram(aes(ratio))
  