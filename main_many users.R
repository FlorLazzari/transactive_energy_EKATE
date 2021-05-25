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


############################# select year #############################

# TODO: check the date is different from the time

selected_year = seq(from = as.POSIXct("2017-01-01 00:00:00"), to = as.POSIXct("2017-12-31 00:00:00"), by = "hour")

df_local_time = data.frame("time" = selected_year, 
                           "month" = month(selected_year),
                           "date" = as.Date(selected_year), 
                           "hour" = hour(selected_year))

############################# data reading (barcelona - PV generation) #############################

filename_gen_1 = "data/202005081411_charts_compare_changedTo2017.csv"
df_gen = import_one_user(filename_1 = filename_gen_1)
# df = eliminate_outliers(df)

# p = plot_initial(df_gen)

colnames(df_gen) = c("time", "gen_1")

# changing NAs to 0
df_gen[is.na(df_gen)] = 0

df_gen = df_gen[as.Date(df_gen$time) %in% df_local_time$date, ]

df_local_time$date %in% as.Date(df_gen$time)

merge(x = data.frame("time" = df_local_time[, "time"]), y = df_gen, by = "time")
 
df_local_time$sunny = (df_gen$gen_1 != 0)

df_gen_sunny = df_gen[df_local_time$sunny,]

############################# data reading (genome project - public) #############################

meter_public = import_data_genome_project_public()



df_day_1_bis = meter_public[as.Date(meter_public$time) %in% dates, ]

head(meter_public)

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

############################# data reading (genome project - office) #############################

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

############################# define characteristic days #############################

# define 14 characteristic days ( 14 = 2 (week and weekend) * 12 (months) )

df_cons = df_day_1_merged[,grep(pattern = "cons", x = colnames(df_day_1_merged))]
df_cons_sunny = df_cons[df_local_time$sunny,]

############################# define n_community_max #############################

# should always use summer months to calculate the community max
n_community_max = calculate_n_community_max(generation = df_gen$gen_1, df_cons, time = df_day_1$time)
n_community_max = 6

global_investment = max(df[[1]]$energy, na.rm = T)*1100


############################# run algo #############################

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

optimal_combination_using_2_GAs$pre_surplus[optimal_combination_using_2_GAs$vector_i]



length(optimal_combination_using_2_GAs$new_optimum_coefficients)
length(optimal_combination_using_2_GAs$new_surplus)
nrow(optimal_combination_using_2_GAs$new_payback)
length(optimal_combination_using_2_GAs$vector_i)








############################# select a combination #############################

# for i in c(1:length(optimal_combination_using_2_GAs$new_optimum_coefficients))

i = 1

combination_selected = zeros(n = 1, m = n_users)
combination_selected[which(!is.na(optimal_combination_using_2_GAs$new_payback[i, ]))] = 1

df_cons_selected_sunny = df_cons_sunny[,combination_selected==1]
df_cons_selected = df_cons[,combination_selected==1]

individual_investment_max = individual_investment[combination_selected==1]  

df = data.frame("time" = df_local_time$time[df_local_time$sunny],
                "gen" = df_gen_sunny)
df = cbind(df_gen, df_cons_selected, "time" = df_local_time$time)
plot_initial(df)

individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)

# checking 
sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny))
optimal_combination_using_2_GAs$pre_surplus[optimal_combination_using_2_GAs$vector_i[i]]

n_sunny_hours = length(df_gen_sunny)
n_community = ncol(df_cons_selected)  

############################# CASE 1: optimization based on surplus (preoptimization) #############################

# checking:
pre_surplus_ = calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus_2 = sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny))

pre_matrix_coefficients = calculate_matrix_coefficients(df_gen_sunny, df_cons_selected_sunny)
pre_matrix_coefficients = matrix(pre_matrix_coefficients, nrow = n_sunny_hours, ncol = n_community)

pre_surplus__ = calculate_surplus_hourly_individual_betas(pre_matrix_coefficients, df_gen_sunny, df_cons_selected_sunny)

sum(pre_surplus__)

pre_payback = calculate_payback_betas(df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, pre_matrix_coefficients)

df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = pre_matrix_coefficients)
plot_solar_consumption_daily_mean_betas(name = "preoptimization", df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_sunny, df_local_time)
plot_disaggregated_daily_mean_community_betas(name = "preoptimization", df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected, df_local_time)

############################# CASE 2: no optimization (equitative distribution) #############################

matrix_coefficients_non_optimum = pre_matrix_coefficients
matrix_coefficients_non_optimum[,] = 1/n_community

df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_non_optimum)
plot_solar_consumption_daily_mean_betas(name = "equitative_distribution", df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_sunny, df_local_time)
plot_disaggregated_daily_mean_community_betas(name = "equitative_distribution", df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected, df_local_time)

# plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)

############################# CASE 3: optimization taking into account the payback #############################

# optimal_combination_using_2_GAs$new_surplus
# optimal_combination_using_2_GAs$new_payback

matrix_coefficients_optimum = optimal_combination_using_2_GAs$new_optimum_coefficients[[i]]
df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_optimum)
plot_solar_consumption_daily_mean_betas(name = "taking_into_account_payback", df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_sunny, df_local_time)
plot_disaggregated_daily_mean_community_betas(name = "taking_into_account_payback", df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, combination_selected==1], df_local_time)

############################# CASE's comparison plots #############################

matrix_coefficients_list = list()

matrix_coefficients_list[[1]] = pre_matrix_coefficients 
matrix_coefficients_list[[2]] = matrix_coefficients_non_optimum
matrix_coefficients_list[[3]] = matrix_coefficients_optimum 

# case: no solar repartition
matrix_coefficients_list[[4]] = matrix_coefficients_optimum
matrix_coefficients_list[[4]][] = 0

plot_comparison_coefficients_upgraded(df_gen, df_gen_sunny, df_cons_selected, df_cons_selected_sunny, matrix_coefficients_list, df_local_time, individual_investment_selected)

# IMPORTANT
# when the investment is proportional to the consumption => there is almost no difference in the payback of pre_surplus (first optimization) and optimization_betas (second optimization)
# TODO: a good scenario would be to define a very different investment compared to the consumption



# case: if cons 2 and cons 9 are exaclty the same:
# why is the optimization_MO giving very different surpluses??
# but the cost they are obtaining is exaclty the same..

# TODO
# important! when changing the payback ideal the payback_years obtained are exactly the same
# for simplicity, using payback years = 0 sounds the easiest a quickest way to procede
# the problem now is how to understand the balance between the surplus and the payback
# surplus is linear
# payback is exponential
# to do this I will use a combination which has surplus > 0 

# from here I can see that the optimum tends tu be simmilar to the "matrix_coefficients_non_optimum" (where the repartition is equi-distributed 1/n_community) 
# I think this is because the investment is proportional to the consumption..
# will try changing this


# comparison of matrixes:
# TODO: graphical comparison of matrixes?
# pre_matrix_coefficients vs matrix_coefficients_optimum 

plot_matrix(name = "1", matrix_coefficients_list[[1]])
plot_matrix(name = "2", matrix_coefficients_list[[2]])
plot_matrix(name = "3", matrix_coefficients_list[[3]])


