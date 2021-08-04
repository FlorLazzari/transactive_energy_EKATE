# COEFICIENTES ESTÁTICOS
# Predeterminados, proporcionales a la inversión inicial. Este tipo de repartición permite que cada inversor 
# obtenga un porcentaje de la generación, aún en los momentos en los que su consumo es nulo, 
# lo cual lleva a grandes excedentes para la comunidad.
# 
# COEFICIENTES DINÁMICOS (horarios)
# Se calculan valorando tanto la inversión inicial de cada usuario como la 
# proporción de consumo con respecto al consumo acumulado de todas los participantes. 
# De esta forma se permite que el consorcio sea rentable para cada inversor y que 
# el excedente de generación fotovoltaica global se minimice.    

##########################################################################################################
# import libraries
library(lubridate)
library(ggplot2)
library(reshape2)
library(GA)
library(parallel)
# and functions
source("functions.R")

##########################################################################################################

filename_gen_1 = "202005081411_charts_compare.csv"
filename_cons_1 = "202005081413_charts_compare.csv"
filename_cons_2 = "202005081655_charts_compare.csv"
filename_cons_3 = "202005081656_charts_compare.csv"
filename_cons_4 = "202005081658_charts_compare.csv"
filenames_list = list(filename_gen_1, filename_cons_1, filename_cons_2, filename_cons_3, filename_cons_4)

df = lapply(X = filenames_list, FUN = import_one_user)

df_gen_1 = df[[1]]
df_cons_1 = df[[2]]
df_cons_2 = df[[3]]
df_cons_3 = df[[4]]
df_cons_4 = df[[5]]

df_day_1 = generate_fake_data_test1(df_gen_1, df_cons_1, df_cons_2, df_cons_3, df_cons_4)

df_day_1$cons_1 = df_day_1$cons_1*10
df_day_1$cons_4 = df_day_1$cons_4*0.1

p = initial_plot(df_day_1)

##########################################################################################################
# VERSION 1 and 2

# Objective: 
# which are the n users (for a FIXED n) that minimice the total_surplus?
# which are the corresponding fixed_coefficients for each of these users previously obtained?

# Size of the proposed community 
n_community = 3

df_gen = data.frame("gen_1" = df_day_1[, "gen_1"])
df_cons = df_day_1[, grep(pattern = "cons", x = colnames(df_day_1))]

##########################################################################################################
# for a small example I can solve the minimization problem looking for the ANALYTICAL SOLUTION
optimum_combination <- analytical_solution(n_community, df_gen, df_cons)
coeffs = calculate_coefficients(optimum_combination, n_community, df_gen, df_cons)
combination = rep(0, length(df_cons))
combination[coeffs$user] = coeffs$optimum_coefficients
gen_assigned <- calculate_gen_assigned(df_gen, combination)
sum(calculate_surplus(df_gen, df_cons, combination))

##########################################################################################################
# but when the number of consumers enlarges I need to use an OPTIMIZATION ALGORITHM

# to do the calculation of daily repartition => n_periods = 1 
n_periods = 1
periods <- calculate_params_period(n_periods)
optimal_combination_GA <- optimize_repartition_GA_real_valued(n_periods, periods, n_community, df_gen, df_cons)
df_gen_assigned_GA <- calculate_gen_assigned(df_gen, as.numeric(optimal_combination_GA))
sum(calculate_surplus(df_gen, df_cons, optimal_combination_GA))


# visualization of results

plot_assignation(df_gen, df_gen_assigned_GA)
plot_assignation(df_gen, df_gen_assigned_GA)


# to check
df_optimal_combination <- optimize_repartition_GA_binary(n_periods, periods, n_community, df_gen, df_cons)











