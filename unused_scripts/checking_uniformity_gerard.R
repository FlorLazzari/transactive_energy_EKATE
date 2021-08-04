n <- 40

#option 1
a <- c(runif(n-1),0,1)
a <- round(a,digits = 2)
#option 2
a <- c(0,1,sample(seq(0,1,0.05),n-1,replace=T))
#option 3 (It doesn't convince me because it forces the allowed coefficients (0.02 to 0.2))
a <- c(0,1,sample(c(0,0.06,0.08,0.1,0.12,0.14,0.16,0.18,0.2,0.25,0.3,0.4,0.5,0.6,0.8,1),n-1,replace=T))

library("dplyr")

b <- sort(a)
b <- b - dplyr::lag(b, 1)
b <- b[!is.na(b)]
sum(b)
length(b)
b


df <- t(mapply(function(i){
  a <- runif(100,0,10)
  a/sum(a)
},1:10000))
hist(df[,1])

df <- t(mapply(function(i){
  a <- c(runif(100-1),0,1)
  a <- round(a,digits=3)
  b <- sort(a)
  b <- b-lag(b,1)
  b <- b[!is.na(b)]
  b
},1:10000))
hist(df[,1])