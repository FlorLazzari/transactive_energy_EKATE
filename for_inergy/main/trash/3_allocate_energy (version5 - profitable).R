# Process description ##########################################################

############ 1) REQUESTED INPUTS ############

### set working directory & version ####
rm(list = ls())
setwd("~/Nextcloud/Flor/projects/EKATE/transactive_energy_EKATE/for_inergy/main")
version = 5

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
# community_objective = "constant_environmental"
community_objective = "profitable"


############ 2) OPTIMIZE ############ 

tic = Sys.time()
matrix_coefficients = calculate_allocation_coefficients(community_objective, individual_investment_selected, df_characteristic_selected, plot_scenarios = F)
toc = Sys.time()

processing_time = toc - tic
# save(matrix_coefficients,
#      file = paste0("workspace/matrixcoefficients_(version",version,").RData"))
# load(file = paste0("workspace/matrixcoefficients_(version",version,").RData"))


# nrow(matrix_coefficients)
# nrow(df_characteristic_selected)
# min(matrix_coefficients)
# max(matrix_coefficients)

# TODO, hardcoded here:
print(colnames(df_characteristic_selected)[grep("cons_", colnames(df_characteristic_selected))])
colnames(matrix_coefficients) = paste0("coeff_",c(2, 8))

df_characteristic_selected_matrix = cbind(df_characteristic_selected, matrix_coefficients)

df_plot = df_characteristic_selected_matrix[, grep("hour|coeff", colnames(df_characteristic_selected_matrix))]

df_plot = aggregate(x = df_plot, by = list(df_plot$hour), FUN = 
            function(x){
              return(mean(x, na.rm = T))
            })

df_plot = df_plot[, c(-1, -2)]

# df_matrix_test_1 = df_characteristic_selected_matrix[(df_characteristic_selected_matrix$month == 1 & df_characteristic_selected_matrix$week == T), c("1", "2", "hour")]
# df_matrix_test_1 = df_matrix_test_1[order(df_matrix_test_1$hour), ]
# df_matrix_test_1 = as.matrix(df_matrix_test_1[, c(1,2)])
plot_matrix(name = paste0("mean_",community_objective), as.matrix(df_plot), version)


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

# df_matrix_test_4 = df_characteristic_selected_matrix[, c("1", "2", "hour")]
# df_matrix_test_4 = df_matrix_test_4[order(df_matrix_test_4$hour), ]
# df_matrix_test_4 = as.matrix(df_matrix_test_4[, c(1,2)])


# working here: 
payback_years = calculate_payback_betas_whole_year_all(df_characteristic_selected_allocated, individual_investment_selected)

############ ...) OUTPUT ############ 

#### ....1) select info #### 

#### ....2) save #### 
save(df_characteristic_selected_allocated, processing_time, payback_years,
     file = paste0("workspace/workspace3_(version",version,").RData"))


### 3) plots ### 

# 1) community plot

# should calculate mean weighting with the n_days (now only consiering week days)
plot_mean_allcoted_community(df_characteristic_selected_allocated, week_selected = T, version)
plot_monthly_allocated_community(df_characteristic_selected_allocated, week_selected = T, version) 


# 2) three individual plots

# participant 1 = 3
plot_mean_allocated_participant(participant = 3, df_characteristic_selected_allocated[, grep("month|week|hour|surplus_1|solarC_1|gridC_1", colnames(df_characteristic_selected_allocated))], version)
# participant 2 = 7
plot_mean_allocated_participant(participant = 7, df_characteristic_selected_allocated[, grep("month|week|hour|surplus_2|solarC_2|gridC_2", colnames(df_characteristic_selected_allocated))], version)


plot_monthly_allocated_participant(participant = 3, df_characteristic_selected_allocated[, grep("month|week|hour|surplus_1|solarC_1|gridC_1", colnames(df_characteristic_selected_allocated))], week_selected = F, version) 
plot_monthly_allocated_participant(participant = 3, df_characteristic_selected_allocated[, grep("month|week|hour|surplus_1|solarC_1|gridC_1", colnames(df_characteristic_selected_allocated))], week_selected = T, version) 

plot_monthly_allocated_participant(participant = 7, df_characteristic_selected_allocated[, grep("month|week|hour|surplus_2|solarC_2|gridC_2", colnames(df_characteristic_selected_allocated))], week_selected = F, version) 
plot_monthly_allocated_participant(participant = 7, df_characteristic_selected_allocated[, grep("month|week|hour|surplus_2|solarC_2|gridC_2", colnames(df_characteristic_selected_allocated))], week_selected = T, version) 


# there are only 2 participants:
selected_3participants = c(1, 2)

plot_allocation_participants(df_characteristic_selected_allocated[df_characteristic_selected_allocated$week == T, ], selected_3participants, individual_investment_selected, paybacks = payback_years, version)

# community stats:
surplus = sum(df_characteristic_selected_allocated[, grepl("surplus",colnames(df_characteristic_selected_allocated))])
avoided_emissions = calculate_avoided_emissions(df_characteristic_selected_allocated)

self_sufficiency_community_mean = calculate_self_sufficiency_community_mean(df_characteristic_selected_allocated)
individual_self_sufficiency = calculate_self_sufficiency_individual(df_characteristic_selected_allocated)
self_sufficiency = mean(self_sufficiency_individual)


self_consumption_community_mean = calculate_self_consumption_community_mean(df_characteristic_selected_allocated)
individual_self_consumption = calculate_self_consumption_individual(df_characteristic_selected_allocated)
self_consumption = mean(self_consumption_individual)


max_payback = max(payback_years)
mean_payback = mean(payback_years)
min_payback = min(payback_years)
diff_max_min_payback = max_payback - min_payback

# individual:
individual_surplus = colSums(df_characteristic_selected_allocated[, grepl("surplus",colnames(df_characteristic_selected_allocated))])
individual_avoided_emissions = calculate_avoided_emissions_individual(df_characteristic_selected_allocated)
individual_investment_selected 
individual_paybacks_years = paybacks_years
self_sufficiency_individual
self_consumption_individual

save(surplus, 
     avoided_emissions, 
     self_sufficiency, 
     self_consumption, 
     max_payback, 
     mean_payback, 
     min_payback,
     diff_max_min_payback,
     individual_surplus, 
     individual_avoided_emissions, 
     individual_investment_selected, 
     individual_paybacks_years, 
     self_sufficiency_individual, 
     self_consumption_individual,
     file = paste0("workspace/stats3_(version",version,").RData"))


