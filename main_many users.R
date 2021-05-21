############################# import libraries #############################

library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(GA)
library(parallel)
library(purrr)
library(nsga2R)
source("functions.R")


############################# data reading #############################

filename_gen_1 = "data/202005081411_charts_compare.csv"
# cons_1 is too high
filename_cons_1 = "data/202005081413_charts_compare.csv"
filename_cons_2 = "data/202005081655_charts_compare.csv"
filename_cons_3 = "data/202005081656_charts_compare.csv"
filename_cons_4 = "data/202005081658_charts_compare.csv"
# new:
filename_cons_5 = "data/202103171643_charts_historic.csv"
# gen_5 is too low
filename_gen_5 = "data/202103171649_charts_historic.csv"
filename_cons_6 = "data/202103171657_charts_historic.csv"
# gen_7 is too low
filename_gen_7 = "data/202103171735_charts_historic_generation.csv"
filename_cons_7 = "data/202103171735_charts_historic_cons.csv"
filename_cons_8 = "data/202103171814_charts_historic.csv"
filename_cons_9 = "data/202103171831_charts_historic.csv"
filename_cons_10 = "data/202103171835_charts_historic.csv"
# gen_10 is too low
filename_gen_10 = "data/202103171838_charts_historic.csv"
filename_cons_11 = "data/202103171858_charts_historic.csv"
filename_cons_12 = "data/202103171905_charts_historic.csv"

filename_cons_13 = "data/202103181018_charts_historic.csv"
filename_cons_14 = "data/202103181023_charts_historic.csv"
filename_cons_15 = "data/202103181029_charts_historic.csv"
filename_cons_16 = "data/202103181032_charts_historic.csv"
filename_cons_17 = "data/202103181037_charts_historic.csv"
filename_cons_18 = "data/202103181040_charts_historic.csv"


### test
# TODO:
# check the time "CET/CEST" is this working ok?
# resize the data set so it has the same structure as the other ones
filename_cons_inergy = "data/wetransfer-2f7820/ExportHourlyConsumption.csv"
# df = import_data_inergy(filename_1 = filename_cons_inergy)
### 

filenames_list = list(filename_gen_1, filename_cons_2, filename_cons_3, filename_cons_4, filename_cons_5, filename_cons_6, filename_cons_7, filename_cons_8, filename_cons_9, filename_cons_11, filename_cons_12, filename_cons_13, filename_cons_14, filename_cons_15, filename_cons_16, filename_cons_17, filename_cons_18)
df = lapply(X = filenames_list, FUN = import_one_user)
df_month_1 = select_month(df, m=7)
df_month_1 = eliminate_outliers(df_month_1)
df_month_1 = reducing_consumption_fake(df_month_1)

p = plot_initial(df_month_1)

# TODO:
# this will be the typical consumption patterns for month 1 
# (to start only 2 typical consumption patterns will be used, this is why the df_month_1 will be of length 24*2)
df_day_1 = df_month_1[(day(df_month_1[, "time"]) %in% c(4)), ]
# df_month_1 = df_month_1[1:24,]

p = plot_initial(df_day_1)

df_gen = data.frame("gen_1" = df_day_1[, "gen_1"])
df_cons = df_day_1[, grep(pattern = "cons", x = colnames(df_day_1))]

# changing NAs to 0
df_gen[is.na(df_gen)] = 0
df_cons[is.na(df_cons)] = 0

df_local_time = data.frame("time" = df_day_1[, "time"], 
                           "date" = day(df_day_1[, "time"]), 
                           "hour" = hour(df_day_1[, "time"]), 
                           "sunny" = (df_gen$gen_1 != 0))

df_gen_sunny = df_gen[df_local_time$sunny,]

# should always use summer months to calculate the community max
n_community_max = calculate_n_community_max(generation = df_gen$gen_1, df_cons, time = df_day_1$time)
n_community_max = 6

global_investment = max(df[[1]]$energy, na.rm = T)*1100

#######################################################################################

d = 4
m = 7 
date = as.Date(paste0("2017-", m, "-", d))


meter_public = import_data_genome_project_public()

df_day_1_bis = meter_public[as.Date(meter_public$time) %in% date, ]
# ncol(df_day_1_bis)
df_day_1_bis = df_day_1_bis[colSums(is.na(df_day_1_bis)) != nrow(df_day_1_bis)]
# ncol(df_day_1_bis)
# filter:
# df_day_1_bis = df_day_1_bis[, c(1, which(as.numeric(apply(X = df_day_1_bis, MARGIN = 2, FUN = min)) - as.numeric(apply(X = df_day_1_bis, MARGIN = 2, FUN = max)) != 0))]
# ncol(df_day_1_bis)
df_day_1_bis$time = as.POSIXct(df_day_1_bis$time)
df_day_1 = df_day_1[colSums(is.na(df_day_1)) != nrow(df_day_1)]

# TODO
# n_users = 128/2 
# TODO:
# cant merge because it is not the same year
# df_day_1_merged = merge(x = df_day_1, y = df_day_1_bis)

df_day_1_merged = cbind(df_day_1, df_day_1_bis[, 2:ncol(df_day_1_bis)])

colnames(df_day_1_merged) = c("time", "gen_1", paste0("cons_",1:(ncol(df_day_1_merged)-2) ))
# p = plot_initial(df_day_1_merged)

#######################################################################################

meter_office = import_data_genome_project_office()

df_day_1_bis = meter_office[as.Date(meter_office$time) %in% date, ]
# ncol(df_day_1_bis)
df_day_1_bis = df_day_1_bis[colSums(is.na(df_day_1_bis)) != nrow(df_day_1_bis)]
# ncol(df_day_1_bis)
# filter:
# df_day_1_bis = df_day_1_bis[, c(1, which(as.numeric(apply(X = df_day_1_bis, MARGIN = 2, FUN = min)) - as.numeric(apply(X = df_day_1_bis, MARGIN = 2, FUN = max)) != 0))]
# ncol(df_day_1_bis)
df_day_1_bis$time = as.POSIXct(df_day_1_bis$time)
df_day_1 = df_day_1[colSums(is.na(df_day_1)) != nrow(df_day_1)]

# TODO
n_users = 128 
# TODO:
# cant merge because it is not the same year
# df_day_1_merged2 = merge(x = df_day_1, y = df_day_1_bis[, 1:((n_users + 3) -ncol(df_day_1_merged))])

df_day_1_merged2 = cbind(df_day_1_merged, df_day_1_bis[, 2:((n_users + 3) -ncol(df_day_1_merged))])

colnames(df_day_1_merged2) = c("time", "gen_1", paste0("cons_",1:n_users))
df_day_1_merged = df_day_1_merged2
# p = plot_initial(df_day_1_merged)

#######################################################################################

# generating fake info here: (should ask Eloi for new data)
df_cons = df_day_1_merged[,grep(pattern = "cons", x = colnames(df_day_1_merged))]
df_cons_sunny = df_cons[df_local_time$sunny,]

n_binary_rep = log(ncol(df_cons), base=2)
# TODO: should change this
individual_investment = sapply(df_cons, max, na.rm = TRUE)*1100

# checking:
# sum(sapply(df_cons, max, na.rm = TRUE)*1100) > global_investment

# TODO: 
# why the first run has this error? is it still appearing?
# Error in gareal_lsSelection_Rcpp(object) :
#   Too few positive probabilities!
# (when hourly = F)

# TODO: should define 2 setting features:
# level of hippiesm (weight_surplus)
# understand and set the parameters of each GA!



tic = Sys.time()
optimal_combination_using_2_GAs <- optimize_hourly_betas_multi_objective(hourly = T, weight_surplus = 0.5, n_community_max = n_community_max, n_binary_rep = n_binary_rep, df_gen_sunny = df_gen_sunny, df_cons_sunny = df_cons_sunny, global_investment = global_investment, individual_investment = individual_investment)
toc = Sys.time()
toc-tic
# TODO: define more filters to reduce the number of possible combinations that will be introduced in the following optimization


# for the mixed integer linear programming
# write.csv(x = t(df_day_1), file = "df_day_1", row.names = FALSE)



