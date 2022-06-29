# Process description ##########################################################

# 1) REQUESTED INPUTS
#   1.1) for plots 
#   1.2) load workspace1 
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
#   4.5) select sunny hours  

# 5) INDIVIDUAL INVESTMENTS
#   5.1) read  

# ACTOR: GRID
# 6) DATA: PRICE 
#   6.1) read  
#   6.2) select sunny hours

# 7) OUTPUT  
#   7.1) save  

# 6 checking instances:
# if (print_plots == T) => plots will be generated in the directory .../graphs

############ 1) REQUESTED INPUTS ############

### 1.1) for plots ####
print_plots = T
version = 2

### 1.2) load workspace1 ####
load(file = paste0("workspace/workspace1_(version",version,").RData"))

### 1.3) import libraries ####
library(GA)
source("functions.R")


# community_objective = "novel" or "environmental" or "profitable"

############ 2) MAXIMUM NUMBER OF PARTICIPANTS ############ 

#### 2.1) calculate number of participants #### 

# los meses deberian depender de el objetivo de la comunidad
# gen_sunny = df_gen_characteristic[(df_gen_characteristic$sunny == T) & (df_gen_characteristic$month %in% c(6, 7, 8)), "energy"]
gen_sunny = df_characteristic[(df_characteristic$sunny == T), "energy"]
cons_sunny = df_characteristic[df_characteristic$sunny == T, grep("cons", colnames(df_characteristic))]

# to improve the results check the hist(individual_self_sufficiency) inside the function
n_community = calculate_n_community(community_objective = "environmental", generation = gen_sunny, consumption = cons_sunny)
# the warning is advising us that the most probable is that the result of the optimization will be to select all the consumers (but remember this is an estimation)

#### 2.1) calculate lenght of binary representation needed #### 

n_binary_rep = ceiling(log(ncol(cons_sunny), base=2))
cons_sunny_complete = complete_consumption(cons_sunny, n_binary_rep)


############ 2) order by self_consumption ############ 

df_cons_characteristic_sunny_ordered = order_consumers(cons_sunny_complete, gen_sunny)

############ 2) optimization ############ 
combination = optimization_1_to_analize(n_community = n_community, n_binary_rep = n_binary_rep, df_gen_to_optimize = gen_sunny, df_cons_to_optimize = df_cons_characteristic_sunny_ordered, max_iter = 50)
    
# TODO: include all the combinations
# combination = combination[1, ]

names_combination = which(colnames(cons_sunny) %in% colnames(df_cons_characteristic_sunny_ordered)[which(combination == 1)]) 
combination_aux = rep(0, times=ncol(cons_sunny))
combination_aux[names_combination] = 1

optimal_combination = combination_aux

# to check:
# TODO: generate a plot
# surplus = colSums(apply(X = as.matrix(combination_aux), MARGIN = 2, FUN = calculate_surplus_hourly_community, df_gen = df_gen_characteristic_sunny, df_cons = df_cons_characteristic_sunny))

selected_cons = colnames(df_characteristic)[grep("cons", colnames(df_characteristic))][optimal_combination==1]

df_characteristic_selected = df_characteristic[,c("month","week","hour","n_days","energy","sunny", selected_cons)]

############ 7) OUTPUT ############ 

plot_energy_time_week_selected(name = paste0("consumption_characteristic_(version",version,")"), print_plots, df_characteristic_selected)

#### 7.1) save #### 
save(df_characteristic_selected, 
     file = paste0("workspace/workspace2_(version",version,").RData"))





