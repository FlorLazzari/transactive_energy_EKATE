# import libraries
library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(GA)
library(parallel)
library(purrr)
# and functions
source("functions_new.R")

##########################################################################################################
# example

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

# TODO:
# cons_1 y gen_1 corresponden al museo de diseño de barcelona -> son muy altos comparados al resto
# la instalación es realmente muy grande
# y el consumo base en enorme (porque es un museo enorme), con lo cual la instalación, por más de que sea enorme, no llega a cubrir los consumos del propio edificio 
# (no es autosuficiente)--> qué sentido tiene repartir la energía?
# con lo cual --> escenario fake: voy a usar la generación y no voy a usar el consumo del museo 

# TODO: ojo! la instalación que estoy considerando tiene mas de 100kWh... la legislación no lo permite

filenames_list = list(filename_gen_1, filename_cons_2, filename_cons_3, filename_cons_4, filename_cons_5, filename_cons_6, filename_cons_7, filename_cons_8, filename_cons_9, filename_cons_11, filename_cons_12, filename_cons_13, filename_cons_14, filename_cons_15, filename_cons_16, filename_cons_17, filename_cons_18)
df = lapply(X = filenames_list, FUN = import_one_user)
df_month_1 = select_month(df, m=7)
df_month_1 = eliminate_outliers(df_month_1)
df_month_1 = reducing_consumption_fake(df_month_1)

p = initial_plot(df_month_1)

df_gen = data.frame("gen_1" = df_month_1[, "gen_1"])
df_cons = df_month_1[, grep(pattern = "cons", x = colnames(df_month_1))]

# TODO:
# changing NAs to 0
df_gen[is.na(df_gen)] = 0
df_cons[is.na(df_cons)] = 0

# Size of the proposed community
# TODO: This "size of the community" should be determined by something related to the maximum of df_gen
# now it is set to be fixed, should be a maximum.. should change the place where the coefficients are calculated

# TODO: working here, should order my ideas! what do I want to calculate?

# should always summer months to calculate the community max
n_community_max = calculate_n_community_max(generation = df_gen$gen_1, df_cons, time = df_month_1$time)
n_community_max = 6

# TODO
# a good estimation of the overall investment is:  
# 1000*kWpico
# if the max consumption is in summer I can approximate: 
global_investment = max(df[[1]]$energy, na.rm = T)*1100

#######################################################################################

df_cons = cbind(df_cons, df_cons)
colnames(df_cons)[17:32] = paste0(rep("cons_", 16),17:32)

n_binary_rep = log(ncol(df_cons), base=2)
# TODO: should change this
individual_investment = sapply(df_cons, max, na.rm = TRUE)*1100
  
# checking:
# sum(sapply(df_cons, max, na.rm = TRUE)*1100) > global_investment

tic = Sys.time()
# TODO: 
# why the first run has this error? is it still appearing?
# Error in gareal_lsSelection_Rcpp(object) : 
#   Too few positive probabilities! 

optimal_combination_using_2_GAs <- optimize_using_2_GAs_withBestSolutionSelection(n_community_max = n_community_max, n_binary_rep, df_gen, df_cons, global_investment, individual_investment)
toc = Sys.time()
toc-tic

# TODO: important! run the "optimize_using_2_GAs_withBestSolutionSelection" several times and see if the results are simmilar
# TODO: how to compare if the results are simmilar? should understand a plot to show this...
# TODO: compare in function of the number n_community (different colors for the n_community)

plot_results(optimal_combination_using_2_GAs){
  
  # pre_optimum_coefficients = optimal_combination_using_2_GAs$pre_optimum_coefficients
  # pre_surplus = optimal_combination_using_2_GAs$pre_surplus
  # pre_payback = optimal_combination_using_2_GAs$pre_payback
  # 
  # index_pre_order = order(new_surplus, decreasing = F)
  # 
  # pre_optimum_coefficients = pre_optimum_coefficients[index_pre_order[1:5],]
  # pre_surplus = pre_surplus[index_pre_order[1:5]]
  # pre_payback = pre_payback[index_pre_order[1:5], ]
  

  new_optimum_coefficients = optimal_combination_using_2_GAs$new_optimum_coefficients
  new_surplus = optimal_combination_using_2_GAs$new_surplus
  new_payback = optimal_combination_using_2_GAs$new_payback
  
  index = order(new_surplus, decreasing = F)[order(new_surplus, decreasing = F) %in% which(new_surplus < 1000000)]

  new_surplus = new_surplus[index]
  new_payback = new_payback[index, ]

  payback_ideal = 4
  cost_payback = rowSums(exp(new_payback - payback_ideal))

  # TODO: study carefully this graph, does this make sense?
  ggplot() + geom_point(aes(x = 1:length(new_surplus), y = new_surplus))
  ggplot() + geom_point(aes(x = cost_payback, y = new_surplus))

}


# TODO: analysis comparing: "winter" - "summer" 
# (do the optimization per month, because the billing is monthly and do the analysis seasonally)

best_combination = selection_best_combination(optimal_combination_using_2_GAs)
df_gen_assigned = calculate_gen_assigned(df_gen = df_gen, combination = best_combination$optimum_coefficients)
df_gen_assigned_selected = df_gen_assigned[,best_combination$optimum_coefficients != 0]

df_cons_selected = df_cons[,best_combination$optimum_coefficients != 0]

plot_solar_consumption_daily_mean(df_gen = df_gen, df_gen_assigned = df_gen_assigned_selected, time = df_month_1$time)
plot_disaggregated_daily_mean_per_user(df_gen_assigned = df_gen_assigned_selected, df_cons_selected = df_cons_selected, time = df_month_1[, "time"])

plot_disaggregated_daily_mean_community(df_gen_assigned = df_gen_assigned_selected, df_cons_selected = df_cons_selected, time = df_month_1[, "time"])

# TODO: how to show our novelty?? 
# compare self consumption and surplus with repartition considering:
# .standard coefficient (lo que te dice red electrica si no haces ningun analisis) and our coeff 
# .the rest of the best combinations selected
# compare cost of electricity in 25 years with and without community (solar assignation)

# how to compare with other selection?? 
# .compare with a random selection of the users?? this doesnt make much sense to me 
# .compare with a selection in which none of this users exist? I should calculate the same optimization but filling with zeros in the columns of the opt_combination in the df_cons 


# TODO: working here
# TODO: should do a graph to show someway the economic benefit
plot_comparison <- function(df_gen = df_gen, optimum_combination = best_combination$optimum_coefficients[best_combination$optimum_coefficients != 0], df_cons_selected = df_cons[best_combination$optimum_coefficients != 0]){
  
  df_gen_assigned_selected = calculate_gen_assigned(df_gen = df_gen, combination = combination)
  solar_consumption = calculate_solar_consumption(df_gen_assigned_selected, df_cons_selected)
  solar_surplus <- df_gen_assigned_selected - df_cons_selected
  solar_surplus[solar_surplus < 0] = 0
  grid = df_cons_selected - df_gen_assigned_selected
  grid[grid < 0] = 0
  # self_consumption_percentage_mean = colSums(solar_consumption) / colSums(df_cons_selected)
  # surplus_percentage_mean = colSums(solar_surplus) / colSums(df_gen_assigned_selected)  
  # sum(solar_surplus)

  ##################################
  
  combination_non_optimum = rep(1/n_community,n_community)
  
  df_gen_assigned_selected_non_optimum = calculate_gen_assigned(df_gen = df_gen, combination = combination)
  solar_consumption_non_optimum = calculate_solar_consumption(df_gen_assigned_selected, df_cons_selected)
  solar_surplus_non_optimum <- df_gen_assigned_selected - df_cons_selected
  solar_surplus_non_optimum[solar_surplus_non_optimum < 0] = 0
  grid_non_optimum = df_cons_selected - df_gen_assigned_selected_non_optimum
  grid_non_optimum[grid_non_optimum < 0] = 0
  # self_consumption_percentage_mean = colSums(solar_consumption) / colSums(df_cons_selected)
  # surplus_percentage_mean = colSums(solar_surplus) / colSums(df_gen_assigned_selected)  
  # sum(solar_surplus)
  
  ##################################

  purchase_price = 0.14859
  sale_price = 0.0508
  
  cost_old = colSums(purchase_price*df_cons_selected)
  cost_sun = purchase_price*colSums(grid) - sale_price * combination * sum(solar_surplus)
  cost_sun_non_optimum = purchase_price*colSums(grid_non_optimum) - sale_price * non_optimum_combination * sum(solar_surplus_non_optimum)
  
  length_period = nrow(df_cons_selected)
  cost_old_one_year = cost_old * 24*360 / length_period
  cost_sun_one_year = cost_sun * 24*360 / length_period
  cost_sun_one_year_non_optimum = cost_sun_non_optimum * 24*360 / length_period

  cost_old_20_years = cost_old_one_year * 20
  cost_sun_20_years = cost_sun_one_year * 20
  cost_sun_non_optimum_one_year = cost_sun_one_year_non_optimum * 20
  
  # bar graph: what would you have paid in the following 20 years?
  # .with the optimum community
  # .with the non optimum community
  # .without the community

}


############


# TODO: cada comunidad energética definirá su objetivo como comunidad
# por ejemplo:
# quiero optimizar payback y que los coeficientes sean fijos
# quiero optimizar el surplus y que los coefs sean dinámicos
# balance de hippiesmo y capitalismo
# flexibilidad de riesgo, cuanto riesgo estamos dispuestos a asumir


# TODO:
# se podría hacer que se cobre un beta distinto para la venta de excedentes?
# de modo que todos cobren el mismo porcentaje de los excedentes totales sumados de la comunidad



# for the mixed integer linear programming
# write.csv(x = t(df_day_1), file = "df_day_1", row.names = FALSE)



