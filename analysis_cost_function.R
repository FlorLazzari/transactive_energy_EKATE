# analisis cost function MANUALLY

############################# import libraries #############################

library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(GA)
library(parallel)
library(purrr)
source("functions.R")

load("~/Documents/projects/EKATE/transactive_energy_EKATE/workspace/testing_cost_function.RData")

#############################  #############################

# checking:

df = data.frame("time" = df_local_time$time[df_local_time$sunny],
                "gen" = df_gen)
df = cbind(df, df_cons_selected)

plot_initial(df)

# TODO:
# should choose a combination in which the decision is clear and work here to improve the cost function






new_surplus = c()
new_optimum_coefficients = list()
new_payback = df_cons[0,1:ncol(df_cons)]


optim_results <- ga(type = "real-valued", fitness = fitness_2_betas, 
                    lower = array(0, dim = dim), upper = array(1, dim = dim),  
                    df_gen_day = df_gen, df_cons_selected_day = df_cons_selected, combination = combination_selected, 
                    individual_investment = individual_investment_selected, 
                    weight_surplus = weight_surplus, 
                    popSize = 100, maxiter = 10, run = 10)

coefficients_optimum <- optim_results@solution[1, ]
coefficients_optimum = matrix(data = coefficients_optimum, ncol = n_community, nrow = n_sunny_hours, byrow = T)
# coefficients_x = matrix(data = x, ncol = n_community, nrow = n_sunny_hours, byrow = T)

coefficients_optimum = coefficients_optimum/rowSums(coefficients_optimum)

combination_optimum = matrix(1, nrow = nrow(coefficients_optimum)) %*% combination_selected
combination_optimum[combination_optimum!=0] = coefficients_optimum

new_optimum_coefficients[[j]] = coefficients_optimum

new_payback[j, combination_selected!=0] = calculate_payback_betas(df_cons_selected, df_gen, individual_investment_selected, matrix_coefficients = coefficients_optimum)

surplus = sum(calculate_surplus_hourly_individual_betas(coefficients_optimum, df_gen, df_cons_selected))

new_surplus <- c(new_surplus, surplus)


fitness_2_betas <- function(x, combination, df_gen_day, df_cons_selected_day, individual_investment, weight_surplus){
  
  # x = runif(dim, 0, 1)
  
  n_sunny_hours = nrow(df_cons_selected_day)
  n_community = ncol(df_cons_selected_day)
  
  coefficients_x = matrix(data = x, ncol = n_community, nrow = n_sunny_hours, byrow = T)
  coefficients_x = coefficients_x/rowSums(coefficients_x)
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_day, matrix_coefficients = coefficients_x)
  
  surplus_x <- df_gen_assigned - df_cons_selected_day
  surplus_x[surplus_x < 0] = 0
  
  cost_surplus = sum(surplus_x)
  
  purchase_price = 0.14859
  sale_price = 0.0508
  
  cost_old = colSums(purchase_price*df_cons_selected_day)
  
  grid_x = df_cons_selected_day - df_gen_assigned
  grid_x[grid_x < 0] = 0
  
  cost_sun = purchase_price*colSums(grid_x) - sale_price * colSums(surplus_x)
  
  profit_period = cost_old - cost_sun
  profit_one_year = profit_period * 360 
  
  payback_years = individual_investment / profit_one_year 
  
  payback_ideal = 4
  
  cost_payback = sum(exp(payback_years - payback_ideal))
  # TODO: add something like this
  cost_payback_2 = max(payback_years) - min(payback_years) 
  
  # score <- cost_surplus * cost_payback
  
  score <- weight_surplus * cost_surplus + (1-weight_surplus) * cost_payback
  return(-score)
}
