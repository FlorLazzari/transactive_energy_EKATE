############################################################
# data reading 


import_one_user <- function(filename_1){
  df <- read.csv(file = filename_1, header = TRUE)
  colnames(df) <- c("time", "energy")
  df$time <- as.POSIXct(as.character(df$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
  return(df)
}


generate_fake_data_test1 <- function(df_gen_1, df_cons_1, df_cons_2, df_cons_3, df_cons_4){
  
  # select only a random day just to start (day 1 had some problems for user 3):
  df_cons_2_day_1 <- df_cons_2[grepl(pattern = "2019-06-01", df_cons_2$time), ]
  df_cons_3_day_1 <- df_cons_3[grepl(pattern = "2019-06-01", df_cons_3$time), ]
  df_cons_4_day_1 <- df_cons_4[grepl(pattern = "2019-06-01", df_cons_4$time), ]
  # CHEATING
  df_cons_4_day_1$energy <- df_cons_4_day_1$energy*c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8,
                                                     1.8, 1.8, 2.7, 2.7, 2.7, 2.7, 2.7, 1.8, 1.8, 1.8, 1.8, 1.8)
  df_gen_1_day_1 <- df_gen_1[grepl(pattern = "2019-06-01", df_gen_1$time), ]
  df_cons_1_day_1 <- df_cons_1[grepl(pattern = "2019-06-01", df_cons_1$time), ] 
  # CHEATING
  df_cons_1_day_1$energy <- df_cons_1_day_1$energy * 0.3
  df_cons_1_day_1$energy <- df_cons_1_day_1$energy*c(1.8, 1.8, 1.8, 1.8, 1.8, 2.7, 2.7, 2.7, 1.8, 1.8, 1.8, 1.8,
                                                   0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
  
  list_df = list(df_gen_1_day_1, df_cons_1_day_1, df_cons_2_day_1, df_cons_3_day_1, df_cons_4_day_1)
  vector_colnames = c("gen_1", "cons_1", "cons_2", "cons_3", "cons_4")
  
  df_day_1 <- list_df[[1]]
  colnames(df_day_1)[2] = vector_colnames[1]
  for (i in 2:length(list_df)) {
    df_day_1 <- merge(df_day_1, list_df[[i]], by = "time")
    colnames(df_day_1)[i+1] = vector_colnames[i]
  }
  
  return(df_day_1)
}


calculate_gen_assigned <- function(df_gen, combination){
  df_gen = as.data.frame(as.matrix(df_gen)%*%matrix(1, ncol =length(combination)))
  df_gen_assigned <- as.data.frame(df_gen * combination[col(df_gen)])  
  return(df_gen_assigned)
}


calculate_surplus <- function(df_gen, df_cons, combination){
  not_selected_cons = (combination == 0) 
  df_cons[, not_selected_cons] = 0
  
  # df_gen_assigned <- calculate_gen_assigned(df_gen, combination)
  # individual_hourly_surplus <- df_gen_assigned - df_cons
  # hourly_surplus <- rowSums(individual_hourly_surplus)
  # hourly_surplus[hourly_surplus < 0] = 0
  
  cons_total = rowSums(df_cons)
  hourly_surplus = df_gen - cons_total
  hourly_surplus[hourly_surplus < 0] = 0

  return(hourly_surplus)
}

calculate_params_period <- function(n_periods){
  n_hours = floor(24/n_periods)
  init = (array(0:(n_periods-1)) * n_hours) + 1
  end = (array(1:(n_periods)) * n_hours) 
  return(data.frame(init, end))  
}

############################################################
# GA functions

calculate_combination_GA <- function(x, n_community){
  selected = order(x, decreasing = T)[1:n_community]
  combination = array(0, length(x))
  combination[selected] = x[1:n_community]
  total = sum(combination)
  combination = combination/total
  return(combination)    
}

fitness <- function(x, n_community, df_gen, df_cons){
  
  # n_community = 2, amount of participans forming part of the community
  # vector of length N (in this case N = 4, amount of participants
  # for example
  # x = c(0.2, 0.6, 0.5, 0.3) 
  
  combination <- calculate_combination_GA(x, n_community)
  # hourly_surplus <- calculate_surplus(df_gen, df_cons, combination)  
  hourly_surplus <- calculate_surplus_individual(df_gen, df_cons, combination)

  score <- sum(hourly_surplus)  
  
  return(-score)
}


optimize_repartition_GA_real_valued <- function(n_periods, periods, n_community, df_gen, df_cons){
  
  # initialize df for results
  df_optimal_combination <- df_cons[0,]
  
  for (j in 1:n_periods) {
    
    init = periods$init[j] 
    end = periods$end[j]
    
    df_gen_period = df_gen[init:end, ]
    df_cons_period = df_cons[init:end, ]
    if (sum(df_gen_period) > 0)  {
      optim_results <- ga(type = "real-valued", fitness = fitness, 
                          lower = array(0, dim = ncol(df_cons)), upper = array(1, dim = ncol(df_cons)),  
                          n_community = n_community, df_gen = df_gen_period, df_cons = df_cons_period, 
                          popSize = 100, maxiter = 200, run = 100)
      
      # the algorithm gives as a result all the local minimums.. some criateria to select one of these minms?
      solution <- optim_results@solution[1, ]
      optimal_combination <- calculate_combination_GA(solution, n_community)
    } else{
      optimal_combination = array(0, dim = ncol(df_cons))
    }
    df_optimal_combination <- rbind(df_optimal_combination, optimal_combination) 
  }
  
  colnames(df_optimal_combination) <- colnames(df_cons)
  return(df_optimal_combination)
}


# optimize_repartition_GA_binary <- function(n_periods, periods, n_community, df_gen, df_cons){
#   
#   # initialize df for results
#   df_optimal_combination <- df_cons[0,]
#   
#   for (j in 1:n_periods) {
#     
#     init = periods$init[j] 
#     end = periods$end[j]
#     
#     df_gen_period = df_gen[init:end, ]
#     df_cons_period = df_cons[init:end, ]
#     if (sum(df_gen_period) > 0)  {
#       
#       min_repartition = 0
#       max_repartition = 95
#       
#       features <- list("0" = list(levels = c(as.character(seq(from = min_repartition, to = max_repartition, by = 1)), NA), class = "discrete"),
#                        "1" = list(levels = c(as.character(seq(from = min_repartition, to = max_repartition, by = 1)), NA), class = "discrete"),
#                        "2" = list(levels = c(as.character(seq(from = min_repartition, to = max_repartition, by = 1)), NA), class = "discrete"),
#                        "3" = list(levels = c(as.character(seq(from = min_repartition, to = max_repartition, by = 1)), NA), class = "discrete")
#       )
# 
#       nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){length(i[["levels"]])},features)))
#       class_per_feature = mapply(function(i){i[['class']]},features)
#       nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features)
#       levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
#       names_per_feature = names(features)
#       
#       
#       optim_results <- ga(
#         type = "binary",
#         fitness = optimizer,
#         nBits = sum(mapply(function(x) { nchar(toBin(x)) }, mapply(function(i){length(i[["levels"]])},features))),
#         class_per_feature = mapply(function(i){i[['class']]},features),
#         nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
#         levels_per_feature = lapply(function(i){i[["levels"]]}, X = features), 
#         names_per_feature = names(features),
#         # df_price = df_price,  
#         selection = gabin_tourSelection,
#         df_gen = df_gen_period, 
#         df_cons = df_cons_period,
#         # suggestions = suggestions,
#         keepBest = TRUE,
#         popSize = 64,
#         maxiter = 20,
#         monitor = gaMonitor,
#         parallel = 16,
#         elitism = 0.08,
#         pmutation = 0.05
#       )
#       
#       optim_results <- as.numeric(decodeValueFromBin(binary_representation = optim_results@solution[1, ],
#                                                             class_per_feature = mapply(function(i){i[['class']]},features),
#                                                             nclasses_per_feature = mapply(function(i){length(i[["levels"]])},features),
#                                                             levels_per_feature = lapply(function(i){i[["levels"]]}, X = features)
#       ))
#       
#       
#       # the algorithm gives as a result all the local minimums.. some criateria to select one of these minms?
#       solution <- optim_results@solution[1, ]
#       optimal_combination <- calculate_combination_GA(solution, n_community)
#     } else{
#       optimal_combination = array(0, dim = ncol(df_cons))
#     }
#     df_optimal_combination <- rbind(df_optimal_combination, optimal_combination) 
#   }
#   
#   colnames(df_optimal_combination) <- colnames(df_cons)
#   return(df_optimal_combination)
# }
# 
# decodeValueFromBin <- function(binary_representation, class_per_feature, nclasses_per_feature, 
#                                levels_per_feature = NULL, min_per_feature = NULL, max_per_feature = NULL){
#   
#   bitOrders <- mapply(function(x) { nchar(toBin(x)) }, nclasses_per_feature)
#   #binary_representation <- X
#   binary_representation <- split(binary_representation, rep.int(seq.int(bitOrders), times = bitOrders))
#   orders <- sapply(binary_representation, function(x) { binary2decimal(gray2binary(x)) })
#   orders <- mapply(function(x){min(orders[x],nclasses_per_feature[x])},1:length(orders))
#   orders <- mapply(
#     function(x){
#       switch(class_per_feature[x],
#              "discrete"= levels_per_feature[[x]][orders[x]+1],
#              "int"= floor(seq(min_per_feature[x],max_per_feature[x],
#                               by=if(nclasses_per_feature[x]>0){
#                                 (max_per_feature[x]-min_per_feature[x])/(nclasses_per_feature[x])
#                               }else{1}
#              )[orders[x]+1]),
#              "float"= seq(min_per_feature[x],max_per_feature[x],
#                           by=if(nclasses_per_feature[x]>0){
#                             (max_per_feature[x]-min_per_feature[x])/(nclasses_per_feature[x])
#                           }else{1})[orders[x]+1]
#       )
#     }
#     ,1:length(orders))
#   return(unname(orders))
# }
# 
# decodeBinFromValue <- function(values, class_per_feature, nclasses_per_feature,
#                                levels_per_feature = NULL, min_per_feature = NULL, max_per_feature = NULL){
#   # values=c(0,400)
#   # class_per_feature=c("int","int")
#   # nclasses_per_feature=c(4,5)
#   # min_per_feature=c(0,0)
#   # max_per_feature=c(400,500)
#   #
#   
#   values <- mapply(
#     function(x){
#       
#       switch(class_per_feature[x],
#              "discrete"= which(levels_per_feature[[x]] %in% values[x])-1,
#              "int"= which(seq(min_per_feature[x],max_per_feature[x],
#                               by=(max_per_feature[x]-min_per_feature[x])/(nclasses_per_feature[x])) %in% values[x])-1,
#              "float"= which(seq(min_per_feature[x],max_per_feature[x],
#                                 by=(max_per_feature[x]-min_per_feature[x])/(nclasses_per_feature[x])) %in% values[x])-1
#       )
#     }
#     ,1:length(values))
#   
#   bitOrders <- mapply(function(x) { nchar(toBin(x)) }, nclasses_per_feature)
#   binary_representation <- unlist(c(sapply(1:length(values), FUN=function(x) { binary2gray(decimal2binary(values[x],bitOrders[x])) })))
#   
#   return(binary_representation)
# }
# 
# toBin<-function(x){ as.integer(paste(rev( as.integer(intToBits(x))),collapse="")) }

############################################################

initial_plot <- function(df){
  
  df_plot <- df 
  colnames(df)[2] <- "PV_generation" 
  
  df_plot <- melt(data = df_plot, id.vars = "time", variable.name = "series")
  df_sum <- data.frame("time" = df$time,
                       "consumption_sum" = rowSums(df[, c(2, 3, 4, 6)]))
  p <- ggplot() + 
    geom_line(aes(hour(df_plot$time), df_plot$value , color = df_plot$series)) +
    geom_area(aes(x = hour(df_sum$time), y = df_sum$consumption_sum), alpha = 0.5) +
    labs(x = "Time [h]", y = "Electrical energy [kWh]", "title" = "Electrical Generation and Consumption", color = "")  
  
  return(p)
}



plot_assignation <- function(df_gen, df_gen_assigned){
  
  df_plot_gen_assigned <- melt(data = df_gen_assigned, variable.name = "series")
  
  p <- ggplot() +
    geom_line(aes(x = 1:24, y = df_gen[, 1])) +
    geom_area(aes(x = rep(x = 1:24, times = ncol(df_gen_assigned)), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) +
    # geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) +
    labs(x = "Time [h]", y = "PV generation [kWh]", "title" = "PV static assignation", fill = "User")  
  return(p)
}


############################################################


# TODO: this only takes into account one generator
# analytical_solution <- function(df){
#   
#   generation = df[, grep(pattern = "gen", x = colnames(df))]
#   
#   df_surplus <- data.frame("time" = df[,"time"])
#   vector_cons <- c(1:(sum(grepl(pattern = "cons", x = colnames(df)))))
#   
#   for (i in vector_cons) {
#     consumption <- df[, paste0("cons_", i)]
#     surplus <- ifelse(generation - consumption >= 0, generation - consumption, 0)
#     df_surplus_aux <- data.frame("time" = time,
#                                  "surplus" = surplus)
#     colnames(df_surplus_aux)[2] <- paste0("user_",i)
#     df_surplus <- merge(df_surplus, df_surplus_aux, by = "time")
#   }
#   
#   library("gtools")
#   df_total_surplus_combinations <- data.frame("time" = df[,"time"])
#   df_combinations <- as.data.frame(combinations(n = length(vector_cons), r = n_community, vector_cons))
#   
#   for (i in 1:nrow(df_combinations)) {
#     set_users <- df_combinations[i, ] 
#     total_consumption <- rowSums(df[,grepl(pattern = paste0(set_users, collapse = "|"), x = colnames(df))])  
#     total_surplus <- ifelse(generation - total_consumption >= 0, generation - total_consumption, 0)     
#     df_total_surplus_aux <- data.frame("time" = df$time, 
#                                        "cons" = total_consumption,
#                                        "surplus" = total_surplus)
#     name <- paste(set_users, collapse = "_")
#     colnames(df_total_surplus_aux)[2] <- paste0("cons_", name)
#     colnames(df_total_surplus_aux)[3] <- paste0("surplus_", name)
#     df_total_surplus_combinations <- merge(df_total_surplus_combinations, df_total_surplus_aux, by = "time")
#   }
#   
#   df_total_surplus_cut <- df_total_surplus_combinations[, grepl(pattern = "*time*|*surplus*", x = colnames(df_total_surplus_combinations))]
#   daily_surplus <- colSums(x = df_total_surplus_cut[, !(grepl("time", colnames(df_total_surplus_cut)))])
#   
#   ordered_daily_surplus <- daily_surplus[order(daily_surplus)]
#   
#   optimum_combination <- ordered_daily_surplus[1]
#   names_optimum_combination <- strsplit(x = as.character(names(optimum_combination)), split = "_")[[1]]
#   vector_names_optimum_combination <- as.numeric(names_optimum_combination[!grepl(pattern = "surplus", x = names_optimum_combination)])
#   
#   df_consumption_optimum_combination <- df[, grep(pattern = paste0(vector_names_optimum_combination, collapse = "|"), x = colnames(df))]
#   
#   
#   ## calculate_coefficients
#   
#   # Calculate stat_coeffs (VERSION 1)
#   # daily_user_consumption <- colSums(x = df_consumption_optimum_combination)
#   # total_daily_user_consumption <- sum(daily_user_consumption)
#   # static_coefficients <- daily_user_consumption/total_daily_user_consumption
#   
#   # Calculate stat_coeffs (VERSION 2)
#   df_surplus_only_users <- df_surplus[, !(grepl(pattern = "time", x = colnames(df_surplus)))]
#   df_sum_surplus <- rowSums(df_surplus_only_users[, grepl(pattern = paste0(as.character(vector_names_optimum_combination), collapse = "|"), x = colnames(df_surplus_only_users))])
#   static_coefficients_hourly <- 1 - df_surplus_only_users[, grepl(pattern = paste0(as.character(vector_names_optimum_combination), collapse = "|"), x = colnames(df_surplus_only_users))]/df_sum_surplus
#   static_coefficients_daily <- colSums(static_coefficients_hourly, na.rm = TRUE)/sum(rowSums(static_coefficients_hourly), na.rm = T)
#   
#   # now I will calculate the surplus if this HOURLY coefficients where used to assign 
#   # the energy repartition
#   df_new_assignation_using_hourly_coeffs <- data.frame("time" = time)
#   
#   for (i in vector_names_optimum_combination) {
#     consumption <- df[, grep(pattern = paste0("cons_", i), x = colnames(df))]
#     generation_to_assign <- generation * static_coefficients_hourly[, paste0("user_",i)]
#     generation_assigned <- ifelse(generation_to_assign >= consumption, consumption, generation_to_assign)
#     generation_assigned[is.na(generation_assigned)] <- 0
#     consumption_grid <- ifelse(consumption - generation_assigned >= 0, consumption - generation_assigned, 0) 
#     surplus <- generation - generation_assigned
#     
#     df_surplus_aux <- data.frame("time" = time, 
#                                  "gen_assigned" = generation_assigned,
#                                  "cons_grid" = consumption_grid, 
#                                  "surplus" = surplus)
#     
#     colnames(df_surplus_aux)[2] <- paste0("gen_assigned_",i) 
#     colnames(df_surplus_aux)[3] <- paste0("cons_grid_",i) 
#     colnames(df_surplus_aux)[4] <- paste0("surplus_",i) 
#     df_new_assignation_using_hourly_coeffs <- merge(df_new_assignation_using_hourly_coeffs, df_surplus_aux, by = "time")
#   }
#   
#   df_gen_assigned <- df_new_assignation_using_hourly_coeffs[, grepl(pattern = "*time*|*gen_assigned*", x = colnames(df_new_assignation_using_hourly_coeffs))]
#   
#   
#   # HARDCODED only to do the plot:
#   colnames(df_gen_assigned)[2] <- 1 
#   colnames(df_gen_assigned)[3] <- 4 
#   
#   df_gen_assigned_total <- rowSums(x = df_gen_assigned[, !(grepl("time", colnames(df_gen_assigned)))])
#   surplus_total_hourly <- generation - df_gen_assigned_total
#   surplus_total_sum_hourly <- sum(surplus_total_hourly)  
#   print(surplus_total_sum_hourly)
#   
#   df_plot_gen_assigned <- melt(data = df_gen_assigned, id.vars = "time", variable.name = "series")
#   df_plot_generation <- melt(df[, grep(pattern = paste0(c("0","time"), collapse = "|"), x = colnames(df))], id.vars = "time", variable.name = "series")
#   
#   p <- ggplot() +
#     geom_line(aes(x = hour(df_plot_generation$time), y = df_plot_generation$value)) +
#     geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) + 
#     labs(x = "Time [h]", y = "PV generation [kWh]", "title" = "PV dynamic assignation", fill = "User")  
#   
#   # now I will calculate the surplus if this DAILY coefficients where used to assign 
#   # the energy repartition
#   df_new_assignation_using_daily_coeffs <- data.frame("time" = time)
#   
#   for (i in vector_names_optimum_combination) {
#     consumption <- df[, grep(pattern = paste0("cons_", i), x = colnames(df))]
#     generation_to_assign <- generation * as.numeric(static_coefficients_daily[paste0("user_",i)])
#     generation_assigned <- ifelse(generation_to_assign >= consumption, consumption, generation_to_assign)
#     consumption_grid <- ifelse(consumption - generation_assigned >= 0, consumption - generation_assigned, 0) 
#     surplus <- generation - generation_assigned
#     df_surplus_aux <- data.frame("time" = time, 
#                                  "gen_assigned" = generation_assigned,
#                                  "cons_grid" = consumption_grid, 
#                                  "surplus" = surplus)
#     
#     colnames(df_surplus_aux)[2] <- paste0("gen_assigned_",i) 
#     colnames(df_surplus_aux)[3] <- paste0("cons_grid_",i) 
#     colnames(df_surplus_aux)[4] <- paste0("surplus_",i) 
#     df_new_assignation_using_daily_coeffs <- merge(df_new_assignation_using_daily_coeffs, df_surplus_aux, by = "time")
#   }
#   
#   df_gen_assigned <- df_new_assignation_using_daily_coeffs[, grepl(pattern = "*time*|*gen_assigned*", x = colnames(df_new_assignation_using_daily_coeffs))]
#   # HARDCODED:
#   colnames(df_gen_assigned)[2] <- 1 
#   colnames(df_gen_assigned)[3] <- 4 
#   
#   return(df_gen_assigned)
# }









analytical_solution <- function(n_community, df_gen, df_cons){
  
  library("gtools")
  df_combinations <- as.data.frame(combinations(n = length(df_cons), r = n_community))
  df_combinations$surplus = 0
  
  combination = rep(0, length(df_cons))
  for (i in 1:nrow(df_combinations)) {
    set_users <- as.numeric(df_combinations[i, c(1:n_community)]) 
    combination_users = combination
    combination_users[set_users] = 1/n_community
    hourly_surplus <- calculate_surplus(df_gen, df_cons, combination = combination_users)
    total_surplus <- sum(hourly_surplus)
    df_combinations$surplus[i] = total_surplus 
  }

  df_combinations <- df_combinations[order(df_combinations$surplus, decreasing = F), ]
  optimum_combination <- df_combinations[1, ]
  return(optimum_combination)
}  


calculate_coefficients <- function(optimum_combination, n_community, df_gen, df_cons){
  
  ## calculate_coefficients
  # Calculate stat_coeffs (VERSION 1)
  # daily_user_consumption <- colSums(x = df_consumption_optimum_combination)
  # total_daily_user_consumption <- sum(daily_user_consumption)
  # static_coefficients <- daily_user_consumption/total_daily_user_consumption
  
  df_individual <- data.frame("user" = as.numeric(optimum_combination[,(1:n_community)]), 
                               "surplus" = 0)
  
  combination = rep(0, length(df_cons))
  # Calculate stat_coeffs (VERSION 2)
  for (i in 1:n_community) {
    set_users <- as.numeric(optimum_combination[,(1:n_community)]) 
    combination_users = combination
    combination_users[set_users[i]] = 1
    hourly_surplus <- calculate_surplus(df_gen, df_cons, combination = combination_users)
    total_surplus <- sum(hourly_surplus)
    df_individual$surplus[i] = total_surplus 
  }
  
  df_individual$optimum_coefficients = sum(df_individual$surplus)/df_individual$surplus
  df_individual$optimum_coefficients = df_individual$optimum_coefficients/sum(df_individual$optimum_coefficients)
  return(df_individual)
}
  



#   # static_coefficients_hourly <- 1 - df_surplus_only_users[, grepl(pattern = paste0(as.character(vector_names_optimum_combination), collapse = "|"), x = colnames(df_surplus_only_users))]/df_sum_surplus
#   # static_coefficients_daily <- colSums(static_coefficients_hourly, na.rm = TRUE)/sum(rowSums(static_coefficients_hourly), na.rm = T)
#   
#   # now I will calculate the surplus if this HOURLY coefficients where used to assign 
#   # the energy repartition
#   df_new_assignation_using_hourly_coeffs <- data.frame("time" = time)
#   
#   for (i in vector_names_optimum_combination) {
#     consumption <- df[, grep(pattern = paste0("cons_", i), x = colnames(df))]
#     generation_to_assign <- generation * static_coefficients_hourly[, paste0("user_",i)]
#     generation_assigned <- ifelse(generation_to_assign >= consumption, consumption, generation_to_assign)
#     generation_assigned[is.na(generation_assigned)] <- 0
#     consumption_grid <- ifelse(consumption - generation_assigned >= 0, consumption - generation_assigned, 0) 
#     surplus <- generation - generation_assigned
#     
#     df_surplus_aux <- data.frame("time" = time, 
#                                  "gen_assigned" = generation_assigned,
#                                  "cons_grid" = consumption_grid, 
#                                  "surplus" = surplus)
#     
#     colnames(df_surplus_aux)[2] <- paste0("gen_assigned_",i) 
#     colnames(df_surplus_aux)[3] <- paste0("cons_grid_",i) 
#     colnames(df_surplus_aux)[4] <- paste0("surplus_",i) 
#     df_new_assignation_using_hourly_coeffs <- merge(df_new_assignation_using_hourly_coeffs, df_surplus_aux, by = "time")
#   }
#   
#   df_gen_assigned <- df_new_assignation_using_hourly_coeffs[, grepl(pattern = "*time*|*gen_assigned*", x = colnames(df_new_assignation_using_hourly_coeffs))]
#   
#   
#   # HARDCODED only to do the plot:
#   colnames(df_gen_assigned)[2] <- 1 
#   colnames(df_gen_assigned)[3] <- 4 
#   
#   df_gen_assigned_total <- rowSums(x = df_gen_assigned[, !(grepl("time", colnames(df_gen_assigned)))])
#   surplus_total_hourly <- generation - df_gen_assigned_total
#   surplus_total_sum_hourly <- sum(surplus_total_hourly)  
#   print(surplus_total_sum_hourly)
#   
#   df_plot_gen_assigned <- melt(data = df_gen_assigned, id.vars = "time", variable.name = "series")
#   df_plot_generation <- melt(df[, grep(pattern = paste0(c("0","time"), collapse = "|"), x = colnames(df))], id.vars = "time", variable.name = "series")
#   
#   p <- ggplot() +
#     geom_line(aes(x = hour(df_plot_generation$time), y = df_plot_generation$value)) +
#     geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) + 
#     labs(x = "Time [h]", y = "PV generation [kWh]", "title" = "PV dynamic assignation", fill = "User")  
#   
#   # now I will calculate the surplus if this DAILY coefficients where used to assign 
#   # the energy repartition
#   df_new_assignation_using_daily_coeffs <- data.frame("time" = time)
#   
#   for (i in vector_names_optimum_combination) {
#     consumption <- df[, grep(pattern = paste0("cons_", i), x = colnames(df))]
#     generation_to_assign <- generation * as.numeric(static_coefficients_daily[paste0("user_",i)])
#     generation_assigned <- ifelse(generation_to_assign >= consumption, consumption, generation_to_assign)
#     consumption_grid <- ifelse(consumption - generation_assigned >= 0, consumption - generation_assigned, 0) 
#     surplus <- generation - generation_assigned
#     df_surplus_aux <- data.frame("time" = time, 
#                                  "gen_assigned" = generation_assigned,
#                                  "cons_grid" = consumption_grid, 
#                                  "surplus" = surplus)
#     
#     colnames(df_surplus_aux)[2] <- paste0("gen_assigned_",i) 
#     colnames(df_surplus_aux)[3] <- paste0("cons_grid_",i) 
#     colnames(df_surplus_aux)[4] <- paste0("surplus_",i) 
#     df_new_assignation_using_daily_coeffs <- merge(df_new_assignation_using_daily_coeffs, df_surplus_aux, by = "time")
#   }
#   
#   df_gen_assigned <- df_new_assignation_using_daily_coeffs[, grepl(pattern = "*time*|*gen_assigned*", x = colnames(df_new_assignation_using_daily_coeffs))]
#   # HARDCODED:
#   colnames(df_gen_assigned)[2] <- 1 
#   colnames(df_gen_assigned)[3] <- 4 
#   
#   return(df_gen_assigned)
# }



