# Process description ##########################################################

# 1) REQUESTED INPUTS 
#   1.1) for plots 
#   1.2) load workspace1 
#   1.3) import libraries
#   1.4) community objective

# 2) NUMBER OF PARTICIPANTS  
#   2.1) estimate number of participants #### 
#   2.2) calculate lenght of binary representation needed #### 

# 3) ORDER PARTICIPANTS 
#   3.1) order by self_consumption 

# 4) OPTIMIZE 
#   4.1) calculate weights for days  
#   4.2) optimize the combination  
#   4.3) combination names  

# 5) OUTPUT 
#   5.1) select info 
#   5.2) order 
#   5.3) save 

# checking instances:
# if (print_plots == T) => plots will be generated in the directory .../graphs

############ 1) REQUESTED INPUTS ############

### set working directory & version ####
rm(list = ls())
setwd("~/Nextcloud/Flor/projects/EKATE/transactive_energy_EKATE/for_inergy/main")
version = 4

### 1.1) for plots ####
print_plots = T

### 1.2) load workspace1 ####
load(file = paste0("workspace/workspace1_(version",version,").RData"))
# los meses deberian depender del objetivo de la comunidad
# gen_sunny = df_gen_characteristic[(df_gen_characteristic$sunny == T) & (df_gen_characteristic$month %in% c(6, 7, 8)), "energy"]
gen_sunny = df_characteristic[(df_characteristic$sunny == T), "energy"]
cons_sunny = df_characteristic[df_characteristic$sunny == T, grep("cons", colnames(df_characteristic))]

### 1.3) import libraries ####
library(GA)
source("functions.R")

### 1.4) community objective ####
# community_objective = "novel" or "environmental" or "profitable"
community_objective = "novel"

############ 2) NUMBER OF PARTICIPANTS ############ 

#### 2.1) estimate number of participants #### 
# to improve the results check the hist(individual_self_sufficiency) inside the function
n_community = calculate_n_community(community_objective, generation = gen_sunny, consumption = cons_sunny)
n_community = 6
# FOR THIS CASE: the warning is advising us that the most probable is that the result of the optimization will be to select all the consumers (but remember this is an estimation)

#### 2.2) calculate lenght of binary representation needed #### 
n_binary_rep = ceiling(log(ncol(cons_sunny), base=2))
cons_sunny_complete = complete_consumption(cons_sunny, n_binary_rep)

############ 3) ORDER PARTICIPANTS ############ 

#### 3.1) order by self_consumption #### 
df_cons_characteristic_sunny_ordered = order_consumers(cons_sunny_complete, gen_sunny)

############ 4) OPTIMIZE ############ 

#### 4.1) calculate weights for days #### 
weights_n_days = df_characteristic$n_days[df_characteristic$sunny == T] 

#### 4.2) optimize the combination #### 
best_combinations = optimize_combination(n_community, n_binary_rep, df_gen_to_optimize = gen_sunny, df_cons_to_optimize = df_cons_characteristic_sunny_ordered, weights_n_days)
# important! reducing the population size reduced a lot the computation time
# combination = best_combinations[1, ]
combination = colSums(best_combinations)

# version 4
combination_1 = which.max(combination) 
combination_aux = combination
combination_aux[combination_1] = 0

combination_2 = which.max(combination_aux) 
combination_aux2 = combination_aux
combination_aux2[combination_2] = 0

combination_3 = which.max(combination_aux2) 
combination_aux3 = combination_aux2
combination_aux3[combination_3] = 0

combination_4 = which.max(combination_aux3) 
combination_aux4 = combination_aux3
combination_aux4[combination_4] = 0

combination_5 = which.max(combination_aux4) 
combination_aux5 = combination_aux4
combination_aux5[combination_5] = 0

combination_6 = which.max(combination_aux5) 
combination_aux6 = combination_aux5
combination_aux6[combination_6] = 0



combination = rep(0, times = length(combination))
combination[c(combination_1, combination_2, combination_3, combination_4, combination_5, combination_6)] = 1

combination = rep(0, times = length(combination))
combination[c(3, 4, 5, 6, 7, 8)] = 1

# checked! identical(best_combinations[9,], combination) 

#### 4.3) combination names #### 
names_combination = which(colnames(cons_sunny) %in% colnames(df_cons_characteristic_sunny_ordered)[which(combination == 1)]) 
combination_aux = rep(0, times=ncol(cons_sunny))
combination_aux[names_combination] = 1

optimal_combination = combination_aux

# to check:
# TODO: generate a plot
weighted_surplus = weights_n_days * calculate_surplus_hourly_community(combination = optimal_combination, df_gen = gen_sunny, df_cons = cons_sunny)
surplus = sum(weighted_surplus, na.rm = T)


############ 5) OUTPUT ############ 

#### 5.1) select info #### 
selected_cons = colnames(df_characteristic)[grep("cons", colnames(df_characteristic))][optimal_combination==1]
df_characteristic_selected = df_characteristic[,c("month","week","hour","n_days","energy","sunny", selected_cons, "purchase_price", "sale_price")]
plot_monthly_characteristic_selected_consumptions(print_plots, df_characteristic_selected)

# redefining the investment
filename_instal_cost = "data/PV_instal_cost_data.csv"
df_instal_cost = import_data_instal_cost(filename_instal_cost) 
individual_investment_selected = individual_investment[optimal_combination==1]

individual_investment_selected = df_instal_cost * individual_investment_selected / sum(individual_investment_selected)


#### 5.2) order #### 
# df_characteristic_selected = df_characteristic_selected[order(df_characteristic_selected$hour), ]
# df_characteristic_selected = df_characteristic_selected[order(df_characteristic_selected$week), ]
# df_characteristic_selected = df_characteristic_selected[order(df_characteristic_selected$month), ]

#### 5.3) save #### 
save(df_characteristic_selected, individual_investment_selected,
     file = paste0("workspace/workspace2_(version",version,").RData"))

