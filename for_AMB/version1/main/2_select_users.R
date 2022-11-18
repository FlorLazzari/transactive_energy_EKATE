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
version = 1

### 1.2) load workspace1 ####
load(file = paste0("workspace/workspace1_(version",version,").RData"))

### 1.3) import libraries ####
library(GA)
source("functions.R")


# community_objective = "novel" or "environmental" or "profitable"

############ 2) MAXIMUM NUMBER OF PARTICIPANTS ############ 

#### 2.1) calculate number of participants #### 

# to improve the results check the hist(individual_self_sufficiency) inside the function
n_community = calculate_n_community(community_objective = "novel", generation = df_gen_characteristic_sunny, consumption = df_cons_characteristic_sunny)

#### 2.1) calculate lenght of binary representation needed #### 

n_binary_rep = log(ncol(df_cons_characteristic_sunny), base=2)


############ 2) order by self_consumption ############ 

df_cons_characteristic_sunny_ordered = order_consumers(df_cons_characteristic_sunny)

############ 2) optimization ############ 
combination = optimization_1_to_analize(n_community = n_community, n_binary_rep = n_binary_rep, df_gen_to_optimize = df_gen_characteristic_sunny, df_cons_to_optimize = df_cons_characteristic_sunny_ordered, max_iter = 50)
    
# TODO: include all the combinations
combination = combination[1, ]

names_combination = which(colnames(df_cons_characteristic_sunny) %in% colnames(df_cons_characteristic_sunny_ordered)[which(combination == 1)])
combination_aux = rep(0, times=ncol(df_cons_characteristic_sunny))
combination_aux[names_combination] = 1

optimal_combination = combination_aux

# to check:
# TODO: generate a plot
# surplus = colSums(apply(X = as.matrix(combination_aux), MARGIN = 2, FUN = calculate_surplus_hourly_community, df_gen = df_gen_characteristic_sunny, df_cons = df_cons_characteristic_sunny))



############ 7) OUTPUT ############ 

#### 7.1) save #### 
save(combination, 
     file = paste0("workspace/workspace2_(version",version,").RData"))





