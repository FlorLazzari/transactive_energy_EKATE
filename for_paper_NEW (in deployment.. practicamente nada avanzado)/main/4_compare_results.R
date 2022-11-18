# comparison


rm(list = ls())
setwd("~/Nextcloud/Flor/projects/EKATE/transactive_energy_EKATE/for_inergy/main")

source("functions.R")
library(ggplot2)
library(lubridate)
library(reshape2)
library(ggpubr)




df_comparison = data.frame("variable" = factor(levels = c("Profitable", "Sustainable", "Novel")), 
                           "value" = numeric(), 
                           "stat" = factor(levels = c("Solar excess [kWh]", "Avoided CO2 [kg]", "Self-consumption [%]", "Self-sufficiency [%]", 
                                                      "Mean payback [years]", "Max payback [years]", "Delta payback [years]")))


version = 8
load(file = paste0("workspace/stats3_(version",version,").RData"))

df_comparison[1, ] = c("Novel", surplus, "Solar excess [kWh]") 
df_comparison[2, ] = c("Novel", avoided_emissions, "Avoided CO2 [kg]") 
df_comparison[3, ] = c("Novel", self_consumption*100, "Self-consumption [%]") 
df_comparison[4, ] = c("Novel", self_sufficiency*100, "Self-sufficiency [%]")
df_comparison[5, ] = c("Novel", mean_payback, "Mean payback [years]") 
df_comparison[6, ] = c("Novel", max_payback, "Max payback [years]") 
df_comparison[7, ] = c("Novel", diff_max_min_payback, "Delta payback [years]") 


df_individual = data.frame("cons_3" = numeric(), 
                           "cons_7" = numeric())

df_individual[1, ] = round(as.numeric(individual_avoided_emissions), 2)
df_individual[2, ] = round(as.numeric(individual_investment_selected), 2)
df_individual[3, ] = round(as.numeric(individual_paybacks), 2)
df_individual[4, ] = rounnd(as.numeric(individual_self_consumption*100), 2)
df_individual[5, ] = round(as.numeric(individual_self_sufficiency*100), 2)
df_individual[6, ] = round(as.numeric(individual_surplus*100), 2)

yearly_profit = as.numeric(individual_investment_selected)/as.numeric(individual_paybacks)

df_individual[7, ] = round(yearly_profit, 2)


  
row.names(df_individual) = c("avoided_emissions", 
                             "investment", 
                             "payback", 
                             "self_consumption", 
                             "self_sufficiency", 
                             "yearly_surplus", 
                             "yearly_profit")


write.csv(df_individual,"stats_novel.csv", row.names = TRUE)



version = 7
# version = 9

load(file = paste0("workspace/stats3_(version",version,").RData"))

df_comparison[8, ] = c("Sustainable", surplus, "Solar excess [kWh]") 
df_comparison[9, ] = c("Sustainable", avoided_emissions, "Avoided CO2 [kg]") 
df_comparison[10, ] = c("Sustainable", self_consumption*100, "Self-consumption [%]") 
df_comparison[11, ] = c("Sustainable", self_sufficiency*100, "Self-sufficiency [%]")
df_comparison[12, ] = c("Sustainable", mean_payback, "Mean payback [years]") 
df_comparison[13, ] = c("Sustainable", max_payback, "Max payback [years]") 
df_comparison[14, ] = c("Sustainable", diff_max_min_payback, "Delta payback [years]") 




df_individual = data.frame("cons_1" = numeric(), 
                           "cons_2" = numeric(), 
                           "cons_3" = numeric(), 
                           "cons_4" = numeric(), 
                           "cons_5" = numeric(), 
                           "cons_6" = numeric(), 
                           "cons_7" = numeric(), 
                           "cons_8" = numeric())

df_individual[1, ] = as.numeric(individual_avoided_emissions)
df_individual[2, ] = as.numeric(individual_investment_selected)
df_individual[3, ] = as.numeric(individual_paybacks)
df_individual[4, ] = as.numeric(individual_self_consumption*100)
df_individual[5, ] = as.numeric(individual_self_sufficiency*100)
df_individual[6, ] = as.numeric(individual_surplus*100)

yearly_profit = as.numeric(individual_investment_selected)/as.numeric(individual_paybacks)

df_individual[7, ] = yearly_profit

row.names(df_individual) = c("avoided_emissions", 
                             "investment", 
                             "payback", 
                             "self_consumption", 
                             "self_sufficiency", 
                             "yearly_surplus", 
                             "yearly_profit")

write.csv(df_individual,"stats_ALL_hourly.csv", row.names = TRUE)




version = 9

load(file = paste0("workspace/stats3_(version",version,").RData"))

df_comparison[8, ] = c("Sustainable", surplus, "Solar excess [kWh]") 
df_comparison[9, ] = c("Sustainable", avoided_emissions, "Avoided CO2 [kg]") 
df_comparison[10, ] = c("Sustainable", self_consumption*100, "Self-consumption [%]") 
df_comparison[11, ] = c("Sustainable", self_sufficiency*100, "Self-sufficiency [%]")
df_comparison[12, ] = c("Sustainable", mean_payback, "Mean payback [years]") 
df_comparison[13, ] = c("Sustainable", max_payback, "Max payback [years]") 
df_comparison[14, ] = c("Sustainable", diff_max_min_payback, "Delta payback [years]") 




df_individual = data.frame("cons_1" = numeric(), 
                           "cons_2" = numeric(), 
                           "cons_3" = numeric(), 
                           "cons_4" = numeric(), 
                           "cons_5" = numeric(), 
                           "cons_6" = numeric(), 
                           "cons_7" = numeric(), 
                           "cons_8" = numeric())

df_individual[1, ] = as.numeric(individual_avoided_emissions)
df_individual[2, ] = as.numeric(individual_investment_selected)
df_individual[3, ] = as.numeric(individual_paybacks)
df_individual[4, ] = as.numeric(individual_self_consumption*100)
df_individual[5, ] = as.numeric(individual_self_sufficiency*100)
df_individual[6, ] = as.numeric(individual_surplus*100)

yearly_profit = as.numeric(individual_investment_selected)/as.numeric(individual_paybacks)

df_individual[7, ] = yearly_profit

row.names(df_individual) = c("avoided_emissions", 
                             "investment", 
                             "payback", 
                             "self_consumption", 
                             "self_sufficiency", 
                             "yearly_surplus", 
                             "yearly_profit")

write.csv(df_individual,"stats_ALL_yearly.csv", row.names = TRUE)


# version = 6
# load(file = paste0("workspace/stats3_(version",version,").RData"))
# 
# df_comparison[15, ] = c("Profitable", surplus, "Solar excess [kWh]")
# df_comparison[16, ] = c("Profitable", avoided_emissions, "Avoided CO2 [kg]")
# df_comparison[17, ] = c("Profitable", self_consumption*100, "Self-consumption [%]")
# df_comparison[18, ] = c("Profitable", self_sufficiency*100, "Self-sufficiency [%]")
# df_comparison[19, ] = c("Profitable", mean_payback, "Mean payback [years]")
# df_comparison[20, ] = c("Profitable", max_payback, "Max payback [years]")
# df_comparison[21, ] = c("Profitable", diff_max_min_payback, "Delta payback [years]")

# df_plot = df_comparison

df_comparison$value = round(as.numeric(df_comparison$value), 2)

plot_comparison_stats_cut(df_comparison)


# plot_comparison_stats(name = "", list_surplus, list_max_payback, list_mean_payback, list_diff_max_min_payback)
# plot_comparison_stats(name = "_emissions", list_sunny_emissions, list_max_payback, list_mean_payback, list_diff_max_min_payback)
# plot_comparison_stats(name = "_avoided_emissions", list_avoided_emissions, list_max_payback, list_mean_payback, list_diff_max_min_payback)
# plot_comparison_stats_complete(name = "_complete3", list_surplus, list_avoided_emissions, list_self_sufficiency, list_self_consumption, list_max_payback, list_mean_payback, list_diff_max_min_payback)

individual_avoided_emissions
individual_investment_selected
individual_paybacks
individual_self_consumption*100
individual_self_sufficiency*100
individual_surplus*100

