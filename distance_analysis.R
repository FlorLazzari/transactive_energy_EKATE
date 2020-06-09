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


#################################################

# library("kmlShape")
library("SimilarityMeasures")

library("dgof")
library("seewave")
library("hydroGOF")

# desing of artificial (A) df to understand the coefficients

library("DescTools")
library("pracma")


time <- df_day_1$time
pv_generator <- df_pv_generation_0_day_1$value_pv_gen_0
cons_A_1 <- pv_generator
cons_A_2 <- pv_generator * 0.5
# include white noise in this case just to be able to calculate chisq.test
# TODO: think about including white noise in all of the cases 
area_under_curve_pv_gen = AUC(x = 1:length(pv_generator), y = pv_generator, method = "spline")

constant_value = area_under_curve_pv_gen/24
white_noise = rnorm(length(pv_generator), mean = 0.001 * constant_value, sd = 0.005  * constant_value)
cons_A_3 <- rep(constant_value, times = length(pv_generator)) + white_noise

cons_A_4 <- c(dnorm(c(0:11), mean = 5, sd = 1.5), dnorm(c(12:23), mean = 17, sd = 1.5))
area_under_curve_cons_A_4 = AUC(x = 1:length(pv_generator), y = cons_A_4, method = "spline")
factor_A_4 = area_under_curve_pv_gen/area_under_curve_cons_A_4 
cons_A_4 <- c(dnorm(c(0:11), mean = 5, sd = 1.5), dnorm(c(12:23), mean = 17, sd = 1.5)) * factor_A_4

cons_A_5 <- c(dnorm(c(0:19), mean = 10, sd = 1.5), dnorm(c(20:23), mean = 21, sd = 0.5))
area_under_curve_cons_A_5 = AUC(x = 1:length(pv_generator), y = cons_A_5, method = "spline")
factor_A_5 = area_under_curve_pv_gen/area_under_curve_cons_A_5 
cons_A_5 <- c(dnorm(c(0:19), mean = 10, sd = 1.5), dnorm(c(20:23), mean = 21, sd = 0.5)) * factor_A_5

df_day_1_A <- data.frame("time" = time,
                         "value_pv_gen_0" = pv_generator,
                         "value_1" = cons_A_1, 
                         "value_2" = cons_A_2, 
                         "value_3" = cons_A_3,
                         "value_4" = cons_A_4,
                         "value_5" = cons_A_5
                         )

df_plot <- melt(data = df_day_1_A, id.vars = "time", variable.name = "series")

ggplot(df_plot) + geom_line(aes(time,value, color = series)) + 
  # facet_wrap(series ~ ., scales = "free_y") +
  theme()





# start applying the different distances

# comparison functions:

vector_values <- c(1:(ncol(df_day_1_A) - 2))
df_stats <- data.frame(user = factor(),
                       frechet_new = numeric(),
                       # frechet_sum = numeric(), 
                       # frechet_max = numeric(), 
                       kolm = numeric(), 
                       itakura = numeric(), 
                       correlation = numeric(), 
                       covariance = numeric(), 
                       chi_squared_stat = numeric(), 
                       chi_squared_pval = numeric(), 
                       mnse = numeric(), 
                       euc_dist = numeric())

pv_gen <- df_day_1[, grep(pattern = "value_pv_gen_0", x = colnames(df_day_1))]
fft_pv_gen = fft(z = pv_gen)
fft_pv_gen_Mod = Mod(fft_pv_gen)


# todo:
for (i in vector_values) {
  # i = 3
  value <- df_day_1_A[, grep(pattern = paste0("value_", i), x = colnames(df_day_1_A))]
  # value <- df_day_1[, grep(pattern = paste0("value_", i), x = colnames(df_day_1))]

  frechet_new <- as.numeric(Frechet(traj1 = as.matrix(pv_gen), traj2 = as.matrix(value)))
  
  # in the notebook I cant install this package                                      
  # frechet_sum <- as.numeric(distFrechet(Px = 1:length(pv_gen), Py = pv_gen, Qx = 1:length(pv_gen), Qy = value, FrechetSumOrMax="sum"))
  # frechet_max <- as.numeric(distFrechet(Px = 1:length(pv_gen), Py = pv_gen, Qx = 1:length(pv_gen), Qy = value, FrechetSumOrMax="max"))
  kolm <- as.numeric(ks.test(x = pv_gen, y = value))[2]
  fft <- fft(z = value)
  fft_Mod <- Mod(fft)
  # TODO: should understand why I am staying with half of the Fourier transform 
  # itakura_using_complete_fft <- as.numeric(itakura.dist(fft_pv_gen_Mod, fft_Mod, scale=TRUE))[1]
  itakura <- as.numeric(itakura.dist(fft_pv_gen_Mod[1:(length(fft_Mod)/2)], fft_Mod[1:(length(fft_Mod)/2)], scale=TRUE))[1]
  # sig_itakura <- sigmoid(itakura, a = 1, b = 0)
  # sig_itakura <- sigmoid(itakura, a = 1, b = 100)
  # itakura <- sig_itakura
  
  correlation <- as.numeric(cor(x = pv_gen, y = value))
  covariance <- as.numeric(cov(x = pv_gen, y = value))
  chi_squared <- chisq.test(x = pv_gen, y = value)
  chi_squared_stat <- as.numeric(chi_squared$statistic)
  chi_squared_pval <- as.numeric(chi_squared$p.value)
  mnse <- as.numeric(mNSE(pv_gen, value))
  # TODO:
  # euc_dist <- proxy::dist(x = t(as.matrix(d)),
  #                         y = as.matrix(centroids_df[cluster_predicted, 2:25]),1:2,as.numeric)
  
  euc_dist <- as.numeric(proxy::dist(x = t(pv_gen), y = t(value)))
  
  df_stats <- rbind(df_stats, data.frame(user = i,
                                         frechet_new = frechet_new,
                                         # frechet_sum = frechet_sum, 
                                         # frechet_max = frechet_max,
                                         kolm = kolm,
                                         itakura = itakura, 
                                         correlation = correlation,
                                         covariance = covariance, 
                                         chi_squared_stat = chi_squared_stat,
                                         chi_squared_pval = chi_squared_pval, 
                                         mnse = mnse, 
                                         euc_dist = euc_dist)
                   )
}


df_plot_1 <- melt(data = df_stats, id.vars =  "user")
df_plot_1$user = as.factor(df_plot_1$user)
bar_stats <- ggplot(df_plot_1) + 
  geom_bar(stat = "identity", width = 1, aes(x = variable, y = value, fill = user)) +
  facet_wrap(facets = "variable", scales = "free")

df_plot_2 <- melt(data = df_day_1_A[, !(colnames(df_day_1_A) %in% "value_pv_gen_0")], id.vars = "time", variable.name = "series")
original_plots_cons <- ggplot(df_plot_2) + geom_line(aes(time,value, color = series)) + facet_wrap(facets = "series")
  # facet_grid(series ~ ., scales = "free_y")  

plot_gen <- ggplot(df_day_1_A[, (colnames(df_day_1_A) %in% c("time", "value_pv_gen_0"))]) +
  geom_line(aes(time, value_pv_gen_0))
  
library("gridExtra")
grid.arrange(plot_gen, original_plots_cons, bar_stats, nrow = 1)


df_sigmoid_stats <- data.frame()

for (i in 2:(ncol(df_stats) - 1)) {

  aux <- sigmoid(df_stats[, i], a = 1, b = 0)
  df_sigmoid_stats <- rbind(df_sigmoid_stats, aux) 

}

df_sigmoid_stats <- t(df_sigmoid_stats)
rownames(df_sigmoid_stats) <- NULL

colnames(df_sigmoid_stats) <- c("frechet_new", "kolm", "itakura", "corzrelation", "covariance",
                                "chi_squared_stat", "chi_squared_pval", "mnse", "euc_dist")

df_sigmoid_stats <- cbind(df_sigmoid_stats, data.frame(user = c(1:5)))

df_sigmoid_stats <- as.data.frame(df_sigmoid_stats)

df_plot_4 <- melt(data = df_sigmoid_stats, id.vars =  "user")
df_plot_4$user = as.factor(df_plot_4$user)
bar_sigmoid_stats <- ggplot(df_plot_4) + 
  geom_bar(stat = "identity", width = 1, aes(x = variable, y = value, fill = user)) +
  facet_wrap(facets = "variable", scales = "free")

grid.arrange(original_plots_cons, bar_stats, bar_sigmoid_stats, nrow = 1)

library("rdetools")
  
# example:
# d <- sincdata(24, noise = 0.1)
# K <- rbfkernel(d$X, sigma = 1, Y = d$y)

