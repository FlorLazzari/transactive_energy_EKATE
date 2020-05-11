filename_1 = "202005081411_charts_compare.csv"
filename_2 = "202005081413_charts_compare.csv"

df_1 <- read.csv(file = filename_1, header = TRUE)
df_2 <- read.csv(file = filename_2, header = TRUE)

df_1 <- df_1[2:nrow(df_1),]
df_2 <- df_2[2:nrow(df_2),]

colnames(df_1) <- c("time", "value")
colnames(df_2) <- c("time", "value")

df_1$time <- as.POSIXct(as.character(df_1$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
df_2$time <- as.POSIXct(as.character(df_2$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 

df_1$value <- as.numeric(as.character(df_1$value))
df_2$value <- as.numeric(as.character(df_2$value))

# quick filter
df_2$value <- ifelse(df_2$value>2000, NA, df_2$value)

df <- merge(df_1, df_2, by = "time")

library(reshape2)
library(ggplot2)
df_plot <- melt(df,  id.vars = "time", variable.name = "series")
ggplot(df_plot) + geom_line(aes(time,value, color = series)) + facet_grid(series ~ ., scales = "free_y") + theme(legend.position = "none")

# I can imagine 2 ways of attacking this problem:
# 1) comparing curves daily 
# 2) comparing curves anualy (or lets say with larger periods than a day)

# the first proposal will be to it anually and then this idea can be shortened to daily easily

df$date <- as.Date(df$time)
colnames(df)[grepl(pattern = "value.x", x = colnames(df))] <- "pv_generation"
colnames(df)[grepl(pattern = "value.y", x = colnames(df))] <- "consumption"

# select only day 1 just to start:
df_day_1 <- df[grepl(pattern = "2019-05-01", df$date), ]
df_plot <- melt(df_day_1,  id.vars = "time", variable.name = "series")
ggplot(df_plot) + geom_line(aes(time,value, color = series)) + facet_grid(series ~ ., scales = "free_y") + theme(legend.position = "none")

filename_cons_1 = "202005081655_charts_compare.csv"
filename_cons_2 = "202005081656_charts_compare.csv"
filename_cons_3 = "202005081658_charts_compare.csv"

df_cons_1 <- read.csv(file = filename_cons_1, header = TRUE)
df_cons_2 <- read.csv(file = filename_cons_2, header = TRUE)
df_cons_3 <- read.csv(file = filename_cons_3, header = TRUE)

colnames(df_cons_1) <- c("time", "value")
colnames(df_cons_2) <- c("time", "value")
colnames(df_cons_3) <- c("time", "value")

df_cons_1$time <- as.POSIXct(as.character(df_cons_1$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
df_cons_2$time <- as.POSIXct(as.character(df_cons_2$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
df_cons_3$time <- as.POSIXct(as.character(df_cons_3$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 



############ TODO: SELECCIONAR EL DIA 1 PARA LOS 3 CONS Y COMPARAR CUAL ES LA MEJOR CURVA

# select only day 1 just to start:
df_cons_1_day_1 <- df_cons_1[grepl(pattern = "2019-05-01", df$date), ]
df_plot <- melt(df_day_1,  id.vars = "time", variable.name = "series")
ggplot(df_plot) + geom_line(aes(time,value, color = series)) + facet_grid(series ~ ., scales = "free_y") + theme(legend.position = "none")

