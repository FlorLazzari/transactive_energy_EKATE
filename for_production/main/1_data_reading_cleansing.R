# data_reading_cleansing

# INPUTS: 
selected_year_generation = seq(from = as.POSIXct("2020-01-01 00:00:00"), to = as.POSIXct("2020-12-31 00:00:00"), by = "hour")
selected_year_consumption = seq(from = as.POSIXct("2017-01-01 00:00:00"), to = as.POSIXct("2017-12-31 00:00:00"), by = "hour")

# PV generation data: 
filename_gen = "data/PV_generation_data.csv"

# PV installation cost data:
filename_instal_cost = "data/PV_instal_cost_data.csv"

# consumption data: 
filename_cons = "data/consumption_data.csv"

# individual investments:
filename_investments = "data/investments.csv"

# tarriff data: 
filename_price = "data/price_data.csv"

# OUTPUTS:
cons_to_plot = "cons_2"
print_plots = T




# PROCESS:

# 1) IMPORT LIBRARIES  
# 2) PV GENERATION DATA  
#   2.1) read PV generation data 
#   2.2) clean PV generation data
#   2.3) define PV generation characteristic day for each month (naive - using hourly mean)
#   2.4) select PV generation on sunny hours
# 3) PV INSTALLATION COST

# 4) CONSUMPTION DATA
#   3.1) read consumption data 
#   3.2) clean consumption data
#   3.3) define consumption characteristic dayS for each month (naive - using hourly mean)
#   3.4) select consumption on sunny hours
# 5) INDIVIDUAL INVESTMENTS

# 4) TARIFF (PRICE) DATA
#   4.1) read price data 
#   4.2) select price on sunny hours

global_investment = max(df_gen$energy, na.rm = T)*1100


version = 0

#### 1) IMPORT LIBRARIES ####

# library(lubridate)
# library(ggplot2)
# library(ggpubr)
# library(reshape2)
# library(purrr)
library(dplyr)
source("functions.R")

version = 0

#### 2) PV GENERATION ####  

#### 2.1) read PV generation data ####
df_gen_0 = import_generation(filename_gen)

# checking instance: plots raw electricity generated vs time 
plot_energy_time(name = paste("generation_raw_", version), print_plots, df_gen_0)

#### 2.2) clean PV generation data #### 
df_gen_1 = eliminate_outliers(df_gen_0)
df_gen_2 = cut_selected_days(df_gen_1, selected_year_generation)

df_local_time_gen_0 = define_local_time_gen(selected_year_generation)
df_local_time_gen_1 = solve_local_time_problems(df_local_time_gen_0)

df_gen_3 = merge(x = data.frame("time" = df_local_time_gen_1$time), y = df_gen_2, by.x = "time", all.x = T)

# checking instance: plots cleaned electricity generated vs time 
plot_energy_time(name = paste("generation_cleaned_", version), print_plots, df_gen_3)

#### 2.3) define PV generation characteristic days for each month (naive - using hourly mean) ####  
# TODO: defining week and week end characteristic days doesnt make sense for generation
df_gen_characteristic = calculate_characteristic_days_gen(df = df_gen_3, number_selected_year = unique(year(selected_year_generation)))
df_local_time_characteristic = create_local_time_characteristic()
df_local_time_characteristic$sunny = (df_gen_characteristic != 0)

# checking instance: plots cleaned electricity generated vs time 
plot_energy_time(name = paste("generation_characteristic_", version), print_plots, data.frame("time" = 1:length(df_gen_characteristic), "energy" = df_gen_characteristic))

#### 2.4) select PV generation on sunny hours #### 
df_gen_characteristic_sunny = df_gen_characteristic[(df_local_time_characteristic$sunny)]

# checking instance: plots characteristic generation vs time 
plot_energy_time(name = paste("generation_characteristic_sunny_", version), print_plots, data.frame("time" = 1:length(df_gen_characteristic_sunny), "energy" = df_gen_characteristic_sunny))

#### 3) PV INSTALLATION COST

filename_instal_cost = "data/PV_instal_cost_data.csv"

#### 3) CONSUMPTION #### 
 
#### 3.1) read consumption data #### 
df_cons_0 = import_data_consumption(filename_cons, selected_year_consumption)

#### 3.2) clean consumption data #### 
df_cons_1 = select_data_consumption(df_cons_0)

#### 3.3) define consumption characteristic dayS for each month (naive - using hourly mean) #### 
df_cons_characteristic_0 = calculate_characteristic_days_cons(df = df_cons_1, number_selected_year = unique(year(selected_year_consumption)))
# TODO: should give more importance to the week days, what if I multiply by 5 the price for the weekdays and by 2 the price for the weekend days?

# checking instance: plots characteristic electricity consumed vs time (of cons_to_plot) 
plot_energy_time(name = paste("consumption_characteristic_", version), print_plots, data.frame("time" = 1:nrow(df_cons_characteristic_0), "energy" = df_cons_characteristic_0[, cons_to_plot]))

#### 3.4) filter consumers #### 

# TODO: check duplicates
# df_cons_characteristic_test = df_cons_characteristic[!duplicated(as.list(df_cons_characteristic))]

df_cons_characteristic_1 = filter_flat_curves(df_cons_characteristic_0)
df_cons_characteristic_2 = select_n_users(df_cons_characteristic_1, n_users = 128)

#### 3.5) select consumption on sunny hours #### 

df_cons_characteristic_sunny = df_cons_characteristic_2[df_local_time_characteristic$sunny, ]

# checking instance: plots characteristic electricity consumed vs time (of cons_to_plot) 
plot_energy_time(name = paste("consumption_characteristic_sunny_", version), print_plots, data.frame("time" = 1:nrow(df_cons_characteristic_0), "energy" = df_cons_characteristic_sunny[, cons_to_plot]))

#### 5) INDIVIDUAL INVESTMENTS
filename_investments = "data/investments.csv"

import_data_investment <- function(filename_investments){
  df <- read.csv(file = filename_investments, header = TRUE)
  return(df)
}



#### 4) TARIFF (PRICE) #### 

#### 4.1) read price data #### 

# TODO: look for reference
# TODO: consider the weekends?
# TODO: use data_update/jun_dic_price.csv
# this data comes from https://www.esios.ree.es/ (fuente confiable para poner en paper)


df_purchase_price = import_data_price(filename_price)

#### 4.2) select price on sunny hours #### 

df_purchase_price_sunny = df_purchase_price[df_local_time$sunny, "price"]
