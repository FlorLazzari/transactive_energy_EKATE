# Process description ##########################################################

# 1) REQUESTED INPUTS
#   1.1) for plots 
#   1.2) file names 
#   1.3) import libraries 

# ACTOR: PV 
# 2) DATA: PV GENERATION   
#   2.1) read  
#   2.2) clean 
#   2.3) define characteristic day for each month (naive - using hourly mean)
#   2.4) select sunny hours
# 3) DATA: PV INSTALLATION COST 
#   3.1) read  

# ACTOR: PARTICIPANTS
# 4) DATA: CONSUMPTION 
#   4.1) read  
#   4.2) clean 
#   4.3) define characteristic dayS for each month (naive - using hourly mean)
#   4.4) filter consumers  

# 5) INDIVIDUAL INVESTMENTS
#   5.1) read  

# ACTOR: GRID
# 6) DATA: PRICE 
#   6.1) read  
#   6.2) select sunny hours

# 7) OUTPUT  
#   7.1) merge info
#   7.2) order 
#   7.3) save  

# checking instances:
# if (print_plots == T) => plots will be generated in the directory .../graphs

############ 1) REQUESTED INPUTS ############

### set working directory & version ####
rm(list = ls())
setwd("~/Nextcloud/Flor/projects/EKATE/transactive_energy_EKATE/for_paper_NEW/main")
version = 1

### 1.1) for plots ####
print_plots = T
# cons_to_plot = "cons_2"

### 1.2) file names ####

# ACTOR: PV 
filename_gen = "data/PV_generation_data.csv"
filename_instal_cost = "data/PV_instal_cost_data.csv"

# ACTOR: PARTICIPANTS
filename_cons = "data/consumption_data.csv"
filename_investments = "data/investments_data.csv"

# ACTOR: GRID
filename_price = "data/price_data.csv"
filename_sale_price = "data/sale_price_data.csv"

selected_year_generation = seq(from = as.POSIXct("2021-01-01 00:00:00"), to = as.POSIXct("2021-12-31 23:00:00"), by = "hour")
selected_year_consumption = selected_year_generation

### 1.3) import libraries ####

library(lubridate)
library(ggplot2)
library(reshape2)
source("functions.R")


############ 2) PV GENERATION ############  

#### 2.1) read ####
df_gen = import_data_generation(filename_gen, selected_year_generation = F, time_format = "%Y-%m-%d %H:%M:%S")

# checking instance: plots raw electricity generated vs time 
plot_raw(name = "generation", print_plots, df = df_gen, version)  

#### 2.2) clean #### 
# df_gen = eliminate_outliers(df_gen)

selected_year_generation = unique(year(df_gen$time))
# df_local_time_gen = define_local_time_gen(selected_year_generation)
# df_local_time_gen = solve_local_time_problems(df_local_time_gen, hours_to_remove = "2015-10-25 02:00:00 CEST")
# df_gen = merge(x = data.frame("time" = df_local_time_gen$time), y = df_gen, by.x = "time", all.x = T)

# checking instance: plots cleaned electricity generated vs time 
# plot_energy_time(name = paste0("generation_cleaned_(version",version,")"), print_plots, data.frame("time" = df_gen[,1], "energy" = df_gen[,2]))

#### 2.3) define characteristic days for each month (naive - using hourly mean) ####  
# TODO: defining week and week end characteristic days doesnt make sense for generation
df_gen_characteristic = calculate_characteristic_days_gen(df = df_gen, number_selected_year = unique(year(df_gen$time)))

# checking instance: plots cleaned electricity generated vs time 
plot_characteristic(name = "generation", print_plots, data.frame("time" = 1:nrow(df_gen_characteristic), "energy" = df_gen_characteristic$energy), version)
plot_monthly_characteristic(name = "generation", print_plots, binary_week = T, df_gen_characteristic, "energy", version)


#### 2.4) select sunny hours ####  
df_gen_characteristic$sunny = df_gen_characteristic$energy != 0

# checking instance: plots characteristic electricity consumed vs time (of cons_to_plot) 
# plot_energy_time(name = paste0("consumption_characteristic_sunny_(version",version,")"), print_plots, data.frame("time" = 1:nrow(df_cons_characteristic), "energy" = df_cons_characteristic$sunny[cons_to_plot]))


############ 3) PV INSTALLATION COST ############

#### 3.1) read ####
filename_instal_cost = "data/PV_instal_cost_data.csv"
df_instal_cost = import_data_instal_cost(filename_instal_cost) 

############ 4) CONSUMPTION ############

#### 4.1) read #### 
df_cons = import_data_consumption(filename_cons, selected_year_consumption = F, time_format = "%m/%d/%Y %H:%M")

#### 4.2) clean #### 
# df_cons = clean_specific_data_consumption(need_cleaning = F, df_cons)  

#### 4.3) define characteristic dayS for each month (naive - using hourly mean) #### 
df_cons_characteristic = calculate_characteristic_days_cons(df = df_cons, number_selected_year = unique(year(df_cons$time)))

# TODO: should give more importance to the week days, what if I multiply by 5 the price for the weekdays and by 2 the price for the weekend days?

# checking instance: plots characteristic electricity consumed vs time (of cons_to_plot) 
cons_to_plot = "cons_1"
plot_monthly_characteristic(name = paste0(cons_to_plot,"_week"), print_plots, binary_week = T, df_cons_characteristic, cons_to_plot, version)
plot_monthly_characteristic(name = paste0(cons_to_plot,"_endWeek"), print_plots, binary_week = F, df_cons_characteristic, cons_to_plot, version)

cons_to_plot = "cons_2"
plot_monthly_characteristic(name = paste0(cons_to_plot,"_week"), print_plots, binary_week = T, df_cons_characteristic, cons_to_plot, version)
plot_monthly_characteristic(name = paste0(cons_to_plot,"_endWeek"), print_plots, binary_week = F, df_cons_characteristic, cons_to_plot, version)

cons_to_plot = "cons_3"
plot_monthly_characteristic(name = paste0(cons_to_plot,"_week"), print_plots, binary_week = T, df_cons_characteristic, cons_to_plot, version)
plot_monthly_characteristic(name = paste0(cons_to_plot,"_endWeek"), print_plots, binary_week = F, df_cons_characteristic, cons_to_plot, version)

cons_to_plot = "cons_4"
plot_monthly_characteristic(name = paste0(cons_to_plot,"_week"), print_plots, binary_week = T, df_cons_characteristic, cons_to_plot, version)
plot_monthly_characteristic(name = paste0(cons_to_plot,"_endWeek"), print_plots, binary_week = F, df_cons_characteristic, cons_to_plot, version)

cons_to_plot = "cons_5"
plot_monthly_characteristic(name = paste0(cons_to_plot,"_week"), print_plots, binary_week = T, df_cons_characteristic, cons_to_plot, version)
plot_monthly_characteristic(name = paste0(cons_to_plot,"_endWeek"), print_plots, binary_week = F, df_cons_characteristic, cons_to_plot, version)

cons_to_plot = "cons_6"
plot_monthly_characteristic(name = paste0(cons_to_plot,"_week"), print_plots, binary_week = T, df_cons_characteristic, cons_to_plot, version)
plot_monthly_characteristic(name = paste0(cons_to_plot,"_endWeek"), print_plots, binary_week = F, df_cons_characteristic, cons_to_plot, version)

cons_to_plot = "cons_7"
plot_monthly_characteristic(name = paste0(cons_to_plot,"_week"), print_plots, binary_week = T, df_cons_characteristic, cons_to_plot, version)
plot_monthly_characteristic(name = paste0(cons_to_plot,"_endWeek"), print_plots, binary_week = F, df_cons_characteristic, cons_to_plot, version)

cons_to_plot = "cons_8"
plot_monthly_characteristic(name = paste0(cons_to_plot,"_week"), print_plots, binary_week = T, df_cons_characteristic, cons_to_plot, version)
plot_monthly_characteristic(name = paste0(cons_to_plot,"_endWeek"), print_plots, binary_week = F, df_cons_characteristic, cons_to_plot, version)


#### 4.4) filter consumers #### 

# TODO: check duplicates
# df_cons_characteristic_test = df_cons_characteristic[!duplicated(as.list(df_cons_characteristic))]
# df_cons_characteristic = filter_flat_curves(df_cons_characteristic)


############ 5) INDIVIDUAL INVESTMENTS ############

#### 5.1) read ####
# individual_investment = c(1500, 1200, 1000, 800, 1000)
filename_investment_maximum_data = "data/investment_maximum_data.csv"
df_investment_maximum_data = import_data_investment(filename_investment_maximum_data)


############ 6) PRICE ############ 

#### 6.1) read #### 
# TODO: look for reference
# TODO: consider the weekends?
# TODO: use data_update/jun_dic_price.csv
# this data comes from https://www.esios.ree.es/ (fuente confiable para poner en paper)

# TODO: this should have time in c("month", "week", "hour")
df_purchase_price = import_data_price(filename_price)

#### 6.2) select sunny hours #### 
# df_purchase_price_sunny = df_purchase_price[(df_gen_characteristic != 0), "price"]

#### 6.3) select sunny hours #### 
sale_price = import_data_sale_price(filename_sale_price)

df_price = cbind(df_purchase_price, sale_price)

df_price_characteristic = calculate_characteristic_days_price(df = df_price, number_selected_year = 2021)

plot_monthly_characteristic(name = "purchase_price_week", print_plots, binary_week = T, df_price_characteristic, "purchase_price", version)
plot_monthly_characteristic(name = "purchase_price_weekEnd", print_plots, binary_week = F, df_price_characteristic, "purchase_price", version)

plot_monthly_characteristic(name = "sale_price_week", print_plots, binary_week = T, df_price_characteristic, "sale_price", version)
plot_monthly_characteristic(name = "sale_price_weekEnd", print_plots, binary_week = F, df_price_characteristic, "sale_price", version)


############ 8) OUTPUT ############ 

#### 8.1) merge info #### 
df_local_time_characteristic = create_local_time_characteristic(number_selected_year = unique(year(selected_year_consumption)))
df_characteristic = merge(df_local_time_characteristic, df_gen_characteristic, by = c("month", "week", "hour"))
df_characteristic = merge(df_characteristic, df_cons_characteristic, by = c("month", "week", "hour"))
df_characteristic = merge(df_characteristic, df_price_characteristic, by = c("month", "week", "hour"))

#### 8.2) order #### 
df_characteristic = df_characteristic[order(df_characteristic$hour), ]
df_characteristic = df_characteristic[order(df_characteristic$week), ]
df_characteristic = df_characteristic[order(df_characteristic$month), ]

# TODO: add: df_instal_cost

#### 8.3) save #### 
individual_investment = as.numeric(df_investment_maximum_data)

save(df_characteristic, individual_investment,  
     file = paste0("workspace/workspace1_(version",version,").RData"))


