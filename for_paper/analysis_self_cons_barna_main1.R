# meeting barna november

# objective: show results for all their PV installations
# important: in the community graph paint with different color the consumption of the building which is sharing the PV  

# TODO: 
# second: check the consumers graphs (their are picks everywhere)
# third: run the 2nd optimization for the random selections 

############################# MAIN 1 #############################

############################# import libraries #############################

library(lubridate)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(GA)
library(parallel)
library(purrr)
library(nsga2R)
source("functions.R")

#############################   #############################

list_tunning_PV = list("2" = list("max_cut" = 6), 
                       "7" = list("max_cut" = 50),
                       "12" = list("max_cut" = 13),
                       "13" = list("max_cut" = 6),
                       "17" = list("max_cut" = 150),
                       "21" = list("max_cut" = 25)
)

list_tunning_consumption = list("2" = list("max_cut" = 230),
                                "7" = list("max_cut" = 100),
                                "12" = list("max_cut" = 150),
                                "13" = list("max_cut" = 70),
                                "17" = list("max_cut" = 550),
                                "21" = list("max_cut" = 150)
)


for (building in 1:length(list_tunning_PV)) {
  
  # building = 1
  
  ############################# data reading (barcelona - PV generation) #############################

  filename_gen = paste0("data_update/",names(list_tunning_PV)[[building]],"_PV.csv")
  df_gen = import_one_user(filename_1 = filename_gen)
  df_gen = eliminate_outliers(df = df_gen, max_cut = list_tunning_PV[[building]]$max_cut)

  to_remove_1 = which(as.character(df_gen$time) %in% "2017-10-29 02:00:00")
  to_remove_2 = which(as.character(df_gen$time) %in% "2018-10-28 02:00:00")
  to_remove_3 = which(as.character(df_gen$time) %in% "2019-10-27 02:00:00")
  to_remove_4 = which(as.character(df_gen$time) %in% "2020-10-25 02:00:00")
  to_remove_5 = which(as.character(df_gen$time) %in% "2021-10-31 02:00:00")
  
  df_gen_complete = df_gen[-c(to_remove_1, to_remove_2, to_remove_3, to_remove_4, to_remove_5), ]

  plot_generation(df_gen)
  
  ############################# data reading (barcelona - consumption) #############################

  filename_cons = paste0("data_update/",names(list_tunning_consumption)[[building]],".csv")
  df_cons_prosumer = import_one_user(filename_1 = filename_cons)
  df_cons_prosumer = eliminate_outliers(df = df_cons_prosumer, max_cut = list_tunning_consumption[[building]]$max_cut)

  to_remove_1 = which(as.character(df_cons_prosumer$time) %in% "2017-10-29 02:00:00")
  to_remove_2 = which(as.character(df_cons_prosumer$time) %in% "2018-10-28 02:00:00")
  to_remove_3 = which(as.character(df_cons_prosumer$time) %in% "2019-10-27 02:00:00")
  to_remove_4 = which(as.character(df_cons_prosumer$time) %in% "2020-10-25 02:00:00")
  to_remove_5 = which(as.character(df_cons_prosumer$time) %in% "2021-10-31 02:00:00")
  
  df_cons_prosumer_complete = df_cons_prosumer[-c(to_remove_1, to_remove_2, to_remove_3, to_remove_4, to_remove_5), ]
  
  plot_generation(df_cons_prosumer_complete)
  
  plot_generation_consumption(name = names(list_tunning_consumption)[building], df_generation = df_gen_complete, df_consumption = df_cons_prosumer)
  plot_consumption_generation_daily_mean(name = names(list_tunning_consumption)[building], df_gen_complete, df_cons_prosumer_complete)
}
