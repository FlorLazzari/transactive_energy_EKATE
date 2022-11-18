# Process description ##########################################################

############ 1) REQUESTED INPUTS ############

### set working directory & version ####
rm(list = ls())
setwd("~/Nextcloud/Flor/projects/EKATE/transactive_energy_EKATE/for_AMB/main")
version = 3

### 1.1) for plots ####
print_plots = T

### 1.2) load workspace1 (?) & workspace2 ####
# think there is no need to load the workspace1 (could errase it)
# load(file = paste0("workspace/workspace1_(version",version,").RData"))
load(file = paste0("workspace/workspace2_(version",version,").RData"))
# gen_sunny = df_characteristic_selected[(df_characteristic_selected$sunny == T), "energy"]
# cons = df_characteristic_selected[, grep("cons", colnames(df_characteristic_selected))]
# cons_sunny = df_characteristic_selected[df_characteristic_selected$sunny == T, grep("cons", colnames(df_characteristic_selected))]
# local_time = df_characteristic_selected[, c("month", "week", "hour", "sunny")]
# purchase_price = df_characteristic_selected$price
# n_community = sum(grepl("cons", colnames(df_characteristic_selected)))
# individual_investment = individual_investment_selected 

### 1.3) import libraries ####
library(ggpubr)
library(reshape2)
library(nsga2R)
source("functions.R")

### 1.4) set community objective ####
# community_objective = "environmental"
# community_objective = "none"
community_objective = "constant_environmental"


############ 2) OPTIMIZE ############ 


matrix_coefficients = calculate_allocation_coefficients(community_objective = "novel", individual_investment_selected, df_characteristic_selected)


############ 2) ALLOCATE ENERGY ############ 

# calculate generation allocated
gen_allocated = calculate_gen_assigned_betas(df_gen_day = df_characteristic_selected$energy, matrix_coefficients = matrix_coefficients)
colnames(gen_allocated) = paste0("alloc_",1:ncol(gen_allocated))
df_characteristic_selected_allocated = cbind(df_characteristic_selected, gen_allocated)


# calculate solar consumption
solar_consumption = calculate_solar_consumption(df_gen_assigned = df_characteristic_selected_allocated[, grep("alloc", colnames(df_characteristic_selected_allocated))], 
                                                df_cons_selected = df_characteristic_selected_allocated[, grep("cons", colnames(df_characteristic_selected_allocated))])
colnames(solar_consumption) = paste0("solarC_",1:ncol(solar_consumption))
df_characteristic_selected_allocated = cbind(df_characteristic_selected_allocated, solar_consumption)


# calculate solar surplus
solar_surplus <- gen_allocated - solar_consumption
# solar_surplus[solar_surplus < 0] = 0
colnames(solar_surplus) = paste0("surplus_",1:ncol(solar_surplus))
df_characteristic_selected_allocated = cbind(df_characteristic_selected_allocated, solar_surplus)


# calculate grid consumption
grid = df_characteristic_selected_allocated[, grep("cons", colnames(df_characteristic_selected_allocated))] - solar_consumption
# grid[grid < 0] = 0
colnames(grid) = paste0("gridC_",1:ncol(grid))
df_characteristic_selected_allocated = cbind(df_characteristic_selected_allocated, grid)

# TODO!!!
# calculate the individual payback
# paybacks = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
#                                              individual_investment = individual_investment_selected, 
#                                              matrix_coefficients = list_matrix_to_use[[i]]))
# 


############ ...) OUTPUT ############ 

#### ....1) select info #### 

#### ....2) save #### 
save(df_characteristic_selected_allocated, 
     file = paste0("workspace/workspace3_(version",version,").RData"))


### 3) plots ### 

# 1) community plot

# should calculate mean weighting with the n_days (now only consiering week days)
plot_REC_mean(name = as.character(version), df_characteristic_selected_allocated, week_selected = T)

plot_REC_monthly_facets(name = as.character(version), df_characteristic_selected_allocated, week_selected = T) 


# 2) three individual plots

# user_min = which.min(payback)
# user_near_mean = which.min(abs(mean(payback) - payback))
# user_max = which.max(payback)
# selected_participants = c(user_min, usea_near_mean, user_max)

selected_3participants = c(4, 3, 5)

plot_disaggregated_three_participants(name, df_characteristic_selected_allocated, week_selected, selected_3participants, individual_investment_selected, paybacks)











