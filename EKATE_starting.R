library("ggplot2")

installation_size <- 10
pv_generator <- installation_size * dnorm(c(0:23), mean = 14, sd = 3)
# plot(pv_generator)

consumer_1 <- pv_generator
consumer_2 <- pv_generator * 0.5
consumer_3 <- rep(1, times = length(pv_generator))  

n_consumers <- 2

df_consumers <- data.frame("consumer_1" = consumer_1, 
                           "consumer_2" = consumer_2, 
                           "consumer_3" = consumer_3)
# Frechet distance

# Kolmogoroc-Smirnov test

# Omega-squared test

# Bellman mathematical programming

# FFT

# Coeficient of correlation (r)

# Chi-squared

# Nash coefficient



# compare total

