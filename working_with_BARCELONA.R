library(ggplot2)

filename_1 = "202005081411_charts_compare.csv"
filename_2 = "202005081413_charts_compare.csv"

df_pv_generation_0 <- read.csv(file = filename_1, header = TRUE)
df_cons_0 <- read.csv(file = filename_2, header = TRUE)

colnames(df_pv_generation_0) <- c("time", "value_pv_gen_0")
colnames(df_cons_0) <- c("time", "value_0")

df_pv_generation_0$time <- as.POSIXct(as.character(df_pv_generation_0$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
df_cons_0$time <- as.POSIXct(as.character(df_cons_0$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 

# df_pv_generation_0$date <- as.Date(df_pv_generation_0$time)
# df_cons_0$date <- as.Date(df_cons_0$time)

# df_pv_generation$value <- as.numeric(as.character(df_pv_generation_0$value))
# df_cons_0$value <- as.numeric(as.character(df_cons_0  $value))
# 
# # quick filter
# df_cons_0$value <- ifelse(df_2$value>2000, NA, df_2$value)
# 
# library(reshape2)
# library(ggplot2)
# df_plot <- melt(df,  id.vars = "time", variable.name = "series")
# ggplot(df_plot) + geom_line(aes(time,value, color = series)) + facet_grid(series ~ ., scales = "free_y") + theme(legend.position = "none")
# 
# # I can imagine 2 ways of attacking this problem:
# # 1) comparing curves daily 
# # 2) comparing curves anualy (or lets say with larger periods than a day)
# 
# # the first proposal will be to do it anually and then this idea can be shortened to daily easily
# 
# df$date <- as.Date(df$time)
# colnames(df)[grepl(pattern = "value.x", x = colnames(df))] <- "pv_generation"
# colnames(df)[grepl(pattern = "value.y", x = colnames(df))] <- "consumption"
# 
# # select only day 1 just to start:
# df_day_1 <- df[grepl(pattern = "2019-05-01", df$date), ]
# df_plot <- melt(df_day_1,  id.vars = "time", variable.name = "series")
# ggplot(df_plot) + geom_line(aes(time,value, color = series)) + facet_grid(series ~ ., scales = "free_y") + theme(legend.position = "none")

filename_cons_1 = "202005081655_charts_compare.csv"
filename_cons_2 = "202005081656_charts_compare.csv"
filename_cons_3 = "202005081658_charts_compare.csv"

df_cons_1 <- read.csv(file = filename_cons_1, header = TRUE)
df_cons_2 <- read.csv(file = filename_cons_2, header = TRUE)
df_cons_3 <- read.csv(file = filename_cons_3, header = TRUE)

colnames(df_cons_1) <- c("time", "value_1")
colnames(df_cons_2) <- c("time", "value_2")
colnames(df_cons_3) <- c("time", "value_3")

df_cons_1$time <- as.POSIXct(as.character(df_cons_1$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
df_cons_2$time <- as.POSIXct(as.character(df_cons_2$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
df_cons_3$time <- as.POSIXct(as.character(df_cons_3$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 

# df_cons_1$date <- as.Date(df_cons_1$time)
# df_cons_2$date <- as.Date(df_cons_2$time)
# df_cons_3$date <- as.Date(df_cons_3$time)

# select only a random day just to start (day 1 had some problems for user 3):
df_cons_1_day_1 <- df_cons_1[grepl(pattern = "2019-06-01", df_cons_1$time), ]
df_cons_2_day_1 <- df_cons_2[grepl(pattern = "2019-06-01", df_cons_2$time), ]
df_cons_3_day_1 <- df_cons_3[grepl(pattern = "2019-06-01", df_cons_3$time), ]
df_pv_generation_0_day_1 <- df_pv_generation_0[grepl(pattern = "2019-06-01", df_pv_generation_0$time), ]


# df_cons_1_day_1 <- df_cons_1[grepl(pattern = "2019-06-01", df_cons_1$date), ]
# df_cons_2_day_1 <- df_cons_2[grepl(pattern = "2019-06-01", df_cons_2$date), ]
# df_cons_3_day_1 <- df_cons_3[grepl(pattern = "2019-06-01", df_cons_3$date), ]
# df_pv_generation_0_day_1 <- df_pv_generation_0[grepl(pattern = "2019-06-01", df_pv_generation_0$date), ]

df_day_1 <- merge(df_cons_1_day_1, df_cons_2_day_1, by = "time")
df_day_1 <- merge(df_day_1, df_cons_3_day_1, by = "time")
df_day_1 <- merge(df_day_1, df_pv_generation_0_day_1, by = "time")

library(reshape2)
df_plot <- melt(data = df_day_1, id.vars = "time", variable.name = "series")

ggplot(df_plot) + geom_line(aes(time,value, color = series)) + 
  # facet_wrap(series ~ ., scales = "free_y") +
  theme(legend.position = "none")




installation_size <- 10 

consumer_1 <- pv_generator
consumer_2 <- pv_generator * 0.5
consumer_3 <- rep(1, times = length(pv_generator))
consumer_4 <- c(dnorm(c(0:11), mean = 5, sd = 1.5), dnorm(c(12:23), mean = 17, sd = 1.5))
consumer_5 <- c(dnorm(c(0:19), mean = 10, sd = 1.5), dnorm(c(20:23), mean = 21, sd = 0.5))

n_consumers <- 2

df_consumers <- data.frame("time" = time,
                           "consumer_1" = consumer_1, 
                           "consumer_2" = consumer_2, 
                           "consumer_3" = consumer_3)







# start applying the different distances

# comparison functions:

pv_generation <- df_day_1$pv_generation
fft_pv_generation = fft(z = pv_generation)
fft_pv_generation_Mod = Mod(fft_pv_generation)

vector_values <- c(1:3)
df_stats <- data.frame(user = factor(),
                       frechet_sum = numeric(), 
                       frechet_max = numeric(), 
                       kolm = numeric(), 
                       itakura = numeric(), 
                       correlation = numeric(), 
                       covariance = numeric(), 
                       chi_squared_stat = numeric(), 
                       chi_squared_pval = numeric(), 
                       mnse = numeric())

# todo:
for (i in vector_values) {
  value <- df_day_1[, grep(pattern = paste0("value_", i), x = colnames(df_day_1))]
  frechet_sum <- as.numeric(distFrechet(Px = c(1:24), Py = pv_generation, Qx = c(1:24), Qy = value, FrechetSumOrMax="sum"))
  frechet_max <- as.numeric(distFrechet(Px = c(1:24), Py = pv_generation, Qx = c(1:24), Qy = value, FrechetSumOrMax="max"))
  kolm <- as.numeric(ks.test(x = pv_generation, y = value))[2]
  fft <- fft(z = df_day_1$value_1)
  fft_Mod <- Mod(fft)
  itakura <- as.numeric(itakura.dist(fft_pv_generation_Mod, fft_Mod, scale=TRUE))[1]
  correlation <- as.numeric(cor(x = pv_generation, y = value))
  covariance <- as.numeric(cov(x = pv_generation, y = value))
  chi_squared <- chisq.test(x = pv_generation, y = value)
  chi_squared_stat <- as.numeric(chi_squared$statistic)
  chi_squared_pval <- as.numeric(chi_squared$p.value)
  mnse <- as.numeric(mNSE(pv_generation, value))
  df_stats <- rbind(df_stats, data.frame(user = i,
                                         frechet_sum = frechet_sum, 
                                         frechet_max = frechet_max,
                                         kolm = kolm,
                                         itakura = itakura, 
                                         correlation = correlation,
                                         covariance = covariance, 
                                         chi_squared_stat = chi_squared_stat,
                                         chi_squared_pval = chi_squared_pval, 
                                         mnse = mnse)
                    )
}


df_plot_1 <- melt(data = df_stats, id.vars =  "user")
df_plot_1$user = as.factor(df_plot_1$user)
bar_stats <- ggplot(df_plot_1) + 
  geom_bar(stat = "identity", width = 1, aes(x = variable, y = value, fill = user)) +
  facet_wrap(facets = "variable", scales = "free")

df_plot_2 <- melt(data = df_day_1, id.vars = "time", variable.name = "series")
original_plots <- ggplot(df_plot_2) + geom_line(aes(time,value, color = series)) + facet_wrap(facets = "series")
  # facet_grid(series ~ ., scales = "free_y")  
  
library("gridExtra")
grid.arrange(original_plots, bar_stats, nrow = 1)








