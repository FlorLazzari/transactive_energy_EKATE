library("ggplot2")

installation_size <- 10 
pv_generator <- installation_size * dnorm(c(0:23), mean = 14, sd = 0.5)
# plot(pv_generator)

time = c(0:23)

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

# comparison functions:

# Frechet distance
library("kmlShape")
distFrechet(Px = time, Py = pv_generator, Qx = time, Qy = consumer_1, FrechetSumOrMax="sum")
distFrechet(Px = time, Py = pv_generator, Qx = time, Qy = consumer_1, FrechetSumOrMax="max")
distFrechet(Px = time, Py = pv_generator, Qx = time, Qy = consumer_2, FrechetSumOrMax="sum")
distFrechet(Px = time, Py = pv_generator, Qx = time, Qy = consumer_2, FrechetSumOrMax="max")
distFrechet(Px = time, Py = pv_generator, Qx = time, Qy = consumer_3, FrechetSumOrMax="sum")
distFrechet(Px = time, Py = pv_generator, Qx = time, Qy = consumer_3, FrechetSumOrMax="max")

# Kolmogorov-Smirnov test
library("dgof")
# Do x and y come from the same distribution?
ks.test(x = pv_generator, y = consumer_1)
ks.test(x = pv_generator, y = consumer_2)
ks.test(x = pv_generator, y = consumer_3)

# Omega-squared test
# abandoned 

# Bellman mathematical programming
# abandoned 

# FFT
# should check the interpretation 
# the absolute value is calculated because gives the amplitud of the spectrum
# because the absolute value of the sins and cosins obtained by the FFT are = 1

# The Fourier transform (FT) decomposes a function (often a function of time, or a signal) 
# into its constituent frequencies. A special case is the expression of a musical chord in 
# terms of the volumes and frequencies of its constituent notes. The term Fourier transform 
# refers to both the frequency domain representation and the mathematical operation that associates 
# the frequency domain representation to a function of time. The Fourier transform of a function of 
# time is itself a complex-valued function of frequency, whose magnitude (modulus) represents the 
# amount of that frequency present in the original function, and whose argument is the phase offset 
# of the basic sinusoid in that frequency. 

# Functions that are localized in the time domain have Fourier transforms that are spread out across the 
# frequency domain and vice versa, a phenomenon known as the uncertainty principle. 

# https://stat.ethz.ch/pipermail/r-help/2005-August/078163.html

fft_pv_generator = fft(z = pv_generator)
fft_pv_generator_Mod = Mod(fft_pv_generator)
fft_pv_generator_Arg = Arg(fft_pv_generator)
plot(fft_pv_generator_Arg)

fft_consumer_2 = fft(z = consumer_2)
fft_consumer_2 = Mod(fft_consumer_2)
plot(fft_consumer_2)

fft_consumer_3 = fft(z = consumer_3)
fft_consumer_3 = Mod(fft_consumer_3)
plot(fft_consumer_3)

fft_consumer_4 = fft(z = consumer_4)
fft_consumer_4 = Mod(fft_consumer_4)
plot(fft_consumer_4)

fft_consumer_5 = fft(z = consumer_5)
fft_consumer_5 = abs(fft_consumer_5)
plot(fft_consumer_5)

library("seewave")
itakura.dist(fft_pv_generator_Mod, fft_consumer_1, scale=TRUE)
itakura.dist(fft_pv_generator, fft_consumer_2, scale=TRUE)
itakura.dist(fft_pv_generator, fft_consumer_3, scale=TRUE)
itakura.dist(fft_pv_generator, fft_consumer_4, scale=TRUE)
itakura.dist(fft_pv_generator, fft_consumer_5, scale=TRUE)

# D1 = The I-S distance of 'spec2' with respect to 'spec1' (i.e. D(spec1 || spec2))
# D2 = The I-S distance of 'spec1' with respect to 'spec2' (i.e. D(spec2 || spec1))
# D = The symmetric distance (i.e. D = 0.5*(D1+D2))
# If scale = TRUE the distance is divided by the length of spec1 (or spec2).  

# the recomendation is to use the spec function instead of the FFT.
# but when using the "spec" function a different result is obtained
# the spec function is developed to study waves, could it be that if there is no periodicity 
# this will not work correclty?
a = spec(wave = pv_generator,f = 1)


# Coeficient of correlation (r)
# detects "shape" and is not affected by the amplitude 
cor(x = pv_generator, y = consumer_1)
cor(x = pv_generator, y = consumer_2)
# problem: for non variance curves (constants) it cant be calculated
cor(x = pv_generator, y = consumer_3)
# negative correlation index
plot(pv_generator, consumer_4)
cor(x = pv_generator, y = consumer_4)
plot(pv_generator, consumer_5)
ggplot() + geom_line(aes(x = time, y = pv_generator)) + geom_line(aes(x = time, y = consumer_5))
cor(x = pv_generator, y = consumer_5)

# detects "shape" and is IS affected by the amplitude 
cov(x = pv_generator, y = consumer_1)
cov(x = pv_generator, y = consumer_2)
cov(x = pv_generator, y = consumer_3)
cov(x = pv_generator, y = consumer_4)

# Chi-squared
chisq.test(x = pv_generator, y = consumer_1)
chisq.test(x = pv_generator, y = consumer_2)
chisq.test(x = pv_generator, y = consumer_3)
chisq.test(x = pv_generator, y = consumer_4)

# Nash coefficient
library("hydroGOF")
mNSE(pv_generator, consumer_1)
mNSE(pv_generator, consumer_2)
mNSE(pv_generator, consumer_3)
mNSE(pv_generator, consumer_4)
mNSE(pv_generator, consumer_5)

# compare total

