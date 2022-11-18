############################# read data #############################

import_data_generation <- function(filename_gen, selected_year_generation, time_format){
  df_gen <- read.csv(file = filename_gen, header = TRUE)
  colnames(df_gen) <- c("time", "energy")
  df_gen$time <- as.POSIXct(as.character(df_gen$time), format = time_format, tz = "Europe/Madrid") 
  df_gen$energy <- as.numeric(df_gen$energy)

  if (selected_year_generation == F) {
  } else{
    df_gen = df_gen[as.Date(df_gen$time) %in% as.Date(selected_year_generation), ]
  }

  return(df_gen)
}

import_data_instal_cost <- function(filename_instal_cost_data){
  df = as.numeric(read.csv(file = filename_instal_cost_data, header = F))
  return(df)
}


import_data_consumption <- function(filename_cons, selected_year_consumption, time_format){
  df_cons = read.csv(file = filename_cons, header = TRUE)
  colnames(df_cons)[1] = "time" 
  
  # df_cons$time <- as.POSIXct(as.character(df_cons$time), format = time_format, tz = "Europe/Madrid") 
  df_cons$time <- as.POSIXct(as.character(df_cons$time))
  df_cons$time <- as.POSIXct(df_cons$time, format = time_format, tz = "Europe/Madrid") 

  if (selected_year_consumption == F) {
  } else{
    df_cons = df_cons[as.Date(df_cons$time) %in% as.Date(selected_year_consumption), ]
  }
  return(df_cons) 
}


import_data_price <- function(filename_price){
 
  df <- read.csv(file = filename_price, header = TRUE)
  colnames(df) <- c("time", "purchase_price")
  return(df)
}


import_data_investment <- function(filename_investments){
  df <- read.csv(file = filename_investments, header = TRUE)
  return(df)
}


import_data_sale_price <- function(filename_sale_price_data){
  df = as.numeric(read.csv(file = filename_sale_price_data, header = F))
  return(df)
}


############################# clean data #############################

  
clean_specific_data_consumption <- function(need_cleaning = F, meter = 0){  
  
  if (need_cleaning == F) {
    return(meter)
  } else{
  # filename = "/home/florencia/Nextcloud/Flor/proyects/EKATE/building-data-genome-project-2/data/meters/cleaned/electricity_cleaned.csv"
  # meter = read.csv(file = filename, header = TRUE)
  meter = meter[, c(1, grep(pattern = "public|office|education", x = colnames(meter)))]
  
  colnames(meter)[1] = "time" 
  
  # selecting users:
  hourly_mean = colMeans(meter[2:ncol(meter)], na.rm = T)
  
  meter = meter[, c(1,order(hourly_mean)+1)]
  
  # filter columns with nas:
  meter = meter[colSums(is.na(meter)) != nrow(meter)]
  
  # meter = meter[, c(1:as.numeric(which(colMeans(meter[2:ncol(meter)], na.rm = T) > 300)[1]))]
  last = as.numeric(which(colMeans(meter[2:ncol(meter)], na.rm = T) > 25))
  # last = as.numeric(which(colMeans(meter[2:ncol(meter)], na.rm = T) > 20))
  meter = meter[, c(1:last[1])]
  first = as.numeric(which(colMeans(meter[2:ncol(meter)], na.rm = T) < 2))
  # meter = meter[, c(1,first[length(first)]:ncol(meter))]
  
  # checking:
  # colMeans(meter[, 2:ncol(meter)], na.rm = T)
  # ncol(meter)
  
  meter$time = as.POSIXct(meter$time)
  
  # plot(meter_public[, 2])
  
  meter = meter[as.Date(meter$time) %in% as.Date(selected_year_consumption), ]
  
  # df_cons_sunny_desordered = df_cons_sunny[, number_order]
  
  return(meter)
  }
}


eliminate_outliers <- function(df, max_cut=160){
  df_mod = df
  df_mod[df_mod > max_cut] = NA
  df_mod[, 1] = df[, 1]
  return(df_mod)
}


filter_flat_curves <- function(df_cons_characteristic){
  filter = colSums(apply(X = df_cons_characteristic, MARGIN = 2, FUN = diff) == 0)
  df_cons_characteristic_filtered = df_cons_characteristic[ ,filter < nrow(df_cons_characteristic)*0.1]
  return(df_cons_characteristic_filtered)
}


############################# characteristic curves #############################


define_local_time_gen <- function(selected_year_generation){
  df_local_time_gen = data.frame("time" = selected_year_generation, 
                                 "month" = lubridate::month(selected_year_generation),
                                 "date" = as.Date(selected_year_generation, tz = "CET"), 
                                 "hour" = hour(selected_year_generation))
  return(df_local_time_gen)
}


solve_local_time_problems <- function(df_raw, hours_to_remove = "2020-10-25 02:00:00 CEST"){
  # df_local_time_gen = df_local_time_gen[-7155,]
  # problem here:
  # df_local_time_gen$time[df_local_time_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST")]
  # df_gen$time[df_gen$time %in% as.POSIXct("2020-10-25 02:00:00 CEST")]
  # will remove the second value 
  
  to_remove = which(df_raw$time %in% as.POSIXct(hours_to_remove))
  df = df_raw[-to_remove, ]
  
  return(df)
}


create_local_time_characteristic <- function(number_selected_year){
  df_calendar = data.frame("month" = sort(rep(c(1:12), 24*2)),
                           "week" = rep(c(rep(F, 24), rep(T, 24)), 12),
                           "hour" = rep(rep(0:23), 24))
  
  df_months_stats = calculate_months_stats(number_selected_year)
  df_local_time = merge(x = df_calendar, y = df_months_stats, by = c("month", "week","hour"))
  df_local_time[order(df_local_time$month), ]
  
  return(df_local_time)
}


calculate_characteristic_days_cons <- function(df, number_selected_year){
  
  df_characteristic = data.frame("month" = sort(rep(c(1:12), 24*2)),
                                 "week" = rep(c(rep(F, 24), rep(T, 24)), 12),
                                 "hour" = rep(rep(0:23), 24))
  
  for (i in 2:ncol(df)) {
    # print(i)
    list_meter_characteristic = calculate_characteristic_days_2(df = df[, c(1, i)], number_selected_year)  
    # will discard the users that have any month without characteristic data
    # if (any(is.na(list_meter_characteristic))) {
    #   # print("no")
    # } else{
    df_characteristic_i = dplyr::bind_rows(list_meter_characteristic, .id = "month")
    df_characteristic = merge(df_characteristic, df_characteristic_i, by = c("month", "week", "hour"))
    colnames(df_characteristic)[ncol(df_characteristic)] = paste0("cons_",ncol(df_characteristic)-1)
    # }
  } 
  
  colnames(df_characteristic)[4:ncol(df_characteristic)] = colnames(df)[2:ncol(df)]
  
  return(df_characteristic)
}


calculate_characteristic_days_price <- function(df, number_selected_year){
  
  df_characteristic = data.frame("month" = sort(rep(c(1:12), 24*2)),
                                 "week" = rep(c(rep(F, 24), rep(T, 24)), 12),
                                 "hour" = rep(rep(0:23), 24))
  
  for (i in 2:ncol(df)) {
    # print(i)
    list_meter_characteristic = calculate_characteristic_days_2(df = df[, c(1, i)], number_selected_year)  
    # will discard the users that have any month without characteristic data
    # if (any(is.na(list_meter_characteristic))) {
    #   # print("no")
    # } else{
    df_characteristic_i = dplyr::bind_rows(list_meter_characteristic, .id = "month")
    df_characteristic = merge(df_characteristic, df_characteristic_i, by = c("month", "week", "hour"))
    colnames(df_characteristic)[ncol(df_characteristic)] = paste0("cons_",ncol(df_characteristic)-1)
    # }
  } 
  
  colnames(df_characteristic)[4:ncol(df_characteristic)] = colnames(df)[2:ncol(df)]
  
  return(df_characteristic)
}


calculate_characteristic_days_gen <- function(df, number_selected_year){
  
  colnames(df) = c("time", "energy")
  df_characteristic = list()
  for (m in 1:12) {
    
    # m = 1
    if (m < 11) {
      days_month = seq(from = as.Date(paste0(as.character(number_selected_year),"-",m,"-01")), to = as.Date(paste0(as.character(number_selected_year),"-",m+1,"-01")), by = "day")    
    }else{
      days_month = seq(from = as.Date(paste0(as.character(number_selected_year),"-",m,"-01")), to = as.Date(paste0(as.character(number_selected_year+1),"-",1,"-01")), by = "day")      
    }
    
    days_month = days_month[-length(days_month)]
    
    ##
    
    df_mean = data.frame("week" = T, "hour" = 0:23, "energy" = NA)
    
    df_month = df[as.Date(df$time) %in% days_month, ]
    df_month_clean = df_month[!is.na(df_month$energy), ]
    
    
    if((any(!(0:23 %in% unique(hour(df_month_clean$time))) ))) {
      
      df_characteristic[[m]] = NA
      
    }else{
      
      df_mean_incomplete = aggregate(df_month_clean$energy, by = list(hour(df_month_clean$time)), FUN = mean)
      colnames(df_mean_incomplete) = c("hour", "energy")
      
      df_mean[df_mean$hour %in% df_mean_incomplete$hour, "energy"] = df_mean_incomplete$energy
      df_mean = df_mean
      
      df_mean_end_week = df_mean
      df_mean_end_week$week = F
      
      df_characteristic[[m]] = rbind(df_mean_end_week, df_mean)
    }
  }
  
  df_characteristic = dplyr::bind_rows(df_characteristic, .id = "column_label")
  colnames(df_characteristic)[1] = "month"
  
  return(df_characteristic)
}



############################# select_users #############################


calculate_n_community <- function(community_objective = "novel", generation, consumption){
  
  # minimum self consumption: 0.2?
  # TODO: a small problem when the peak is a platau
  
  # max_insolation = aggregate(x = df[[1]]$energy, by = list(date(df[[1]]$time)), FUN = max)
  # max_insolation_hours = aggregate(x = df[[1]]$energy, by = list(date(df[[1]]$time)), FUN = which.max)
  
  # generation_smooth = as.numeric(stats::smooth(generation, "3RSR"))
  
  first_derivative = sign(diff(generation))
  peak_insolation_hours = (diff(first_derivative) == -2)
  
  high_insolation_hours = max(generation, na.rm = T)/2 < generation  
  
  # completing borders:
  peak_insolation_hours = c(T, peak_insolation_hours, T)
  max_insolation_hours = peak_insolation_hours & high_insolation_hours
  
  # checking:
  # peaks = generation
  # peaks[!max_insolation_hours] = NA
  # plot(generation, type = "l")
  # points(peaks)
  
  # n_community_max = 1 + ceiling(1/mean(colMeans(consumption[max_insolation_hours, ] / generation_smooth[max_insolation_hours])))
  
  individual_self_sufficiency = colMeans(consumption[max_insolation_hours, ] / generation[max_insolation_hours], na.rm = T)  
  individual_self_sufficiency_quantiles = quantile(individual_self_sufficiency, na.rm = T)
  
  # the stat we choose depends on our objective
  
  if (community_objective == "environmental"){
    # community_objective = only environmental (we want all the energy to be consumed)
    
    individual_self_sufficiency_stat = as.numeric(individual_self_sufficiency_quantiles[2])
    n_community = 1/individual_self_sufficiency_stat
    n_community = ceiling(n_community) 
    
  } else if (community_objective == "profitable"){
    # community_objective = only profitability (we want all the participants to have low payback)    
    
    individual_self_sufficiency_stat = as.numeric(individual_self_sufficiency_quantiles[4])
    n_community = 1/individual_self_sufficiency_stat
    n_community = floor(n_community) 
    
  } else if (community_objective == "novel"){
    # community_objective = only profitability (we want all the participants to have low payback)    
    
    individual_self_sufficiency_stat = as.numeric(individual_self_sufficiency_quantiles[3])
    n_community = 1/individual_self_sufficiency_stat
    n_community = round(n_community) 
  } else {
    print("Error --- misspelled")
    n_community = 0 
  }
  
  # hist(individual_self_sufficiency)
  # mean(individual_self_sufficiency)
  
  if (n_community>ncol(consumption)) {
    print(paste("Warning --- optimal n_community =",n_community))
    n_community = ncol(consumption)
  }
  return(n_community)
}


order_consumers <- function(df_cons_characteristic_sunny, df_gen_characteristic_sunny){
  self_consumption_per_user = c()
  
  for (i in 1:ncol(df_cons_characteristic_sunny)) {
    df_cons_characteristic_sunny_i = df_cons_characteristic_sunny[, i]
    self_consumption_no_limits = df_gen_characteristic_sunny/df_cons_characteristic_sunny_i
    self_consumption = ifelse(self_consumption_no_limits > 1, 1, self_consumption_no_limits)
    self_consumption = mean(self_consumption)
    self_consumption_per_user[i] = self_consumption
  }
  df_cons_characteristic_sunny_ordered = df_cons_characteristic_sunny[, order(self_consumption_per_user)]
  
  return(df_cons_characteristic_sunny_ordered)
}


optimize_combination <- function(n_community, n_binary_rep, df_gen_to_optimize, df_cons_to_optimize, weights_n_days){
  
  # dim_search_ga = calculate_combinatorics(n = ncol(df_cons_to_optimize), m = n_community) * factorial(n_community)
  # dim_search_solution = calculate_combinatorics(n = ncol(df_cons_to_optimize), m = n_community) * factorial(n_community)
  
  optim_results <- GA::ga(type = "binary", fitness = fitness_combination, 
                          nBits = n_binary_rep*n_community,
                          n_community = n_community, df_gen_to_optimize = df_gen_to_optimize, df_cons_to_optimize = df_cons_to_optimize, n_binary_rep = n_binary_rep, weights_n_days = weights_n_days,  
                          popSize = 100, 
                          run = 50,
                          maxiter = 500,
                          crossover = purrr::partial(bee_uCrossover_binary, n_binary_rep = n_binary_rep, n_community = n_community), 
                          # keepBest = T,
                          pmutation = 0.08
  )
  
  solutions = optim_results@solution
  combinations = t(apply(X = as.matrix(solutions), MARGIN = 1, FUN = calculate_combination_for_GA_binary, n_community = n_community, n_binary_rep = n_binary_rep, df_cons = df_cons_to_optimize))
  combinations = combinations[!duplicated(combinations), ]
  
  return(combinations)
}


fitness_combination <- function(x, n_community, n_binary_rep, df_gen_to_optimize, df_cons_to_optimize, weights_n_days){
  
  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep*n_community)
  # x = rep(1, n_binary_rep*n_community)
  # x = x_solution[1, ]
  # x = round(runif(nBits, 0, 1))
  
  combination = calculate_combination_for_GA_binary(x, n_community = n_community, n_binary_rep = n_binary_rep, df_cons = df_cons_to_optimize)
  
  weighted_surplus = weights_n_days * calculate_surplus_hourly_community(combination = combination, df_gen = df_gen_to_optimize, df_cons = df_cons_to_optimize)
  surplus = sum(weighted_surplus, na.rm = T)
  
  # surplus = sum(calculate_surplus_hourly_community(combination = combination, df_gen = df_gen_to_optimize, df_cons = df_cons_to_optimize))
  score <- surplus
  
  # surplus = colSums(apply(X = as.matrix(combinations), MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_to_optimize, df_cons = df_cons_to_optimize))
  
  return(-score)
}


calculate_combination_for_GA_binary <- function(x, n_community, n_binary_rep,  df_cons){
  
  combination = rep(0, ncol(df_cons))
  
  for (j in 1:n_community) {
    user = binary2decimal(x[((j-1)*n_binary_rep + 1):(j*n_binary_rep)]) + 1  
    # print(user)
    combination[user] = 1
  }
  return(combination)
}


calculate_surplus_hourly_community <- function(combination, df_gen, df_cons){
  
  # df_cons_selected = df_cons_to_optimize
  df_cons_selected = df_cons
  
  not_selected_cons = (combination == 0) 
  df_cons_selected[, not_selected_cons] = 0
  
  df_cons_community = as.numeric(rowSums(df_cons_selected))
  
  community_hourly_surplus <- df_gen - df_cons_community
  community_hourly_surplus[community_hourly_surplus < 0] = 0
  
  
  return(community_hourly_surplus)
}


# TODO: study more about crossovers and elaborate a better one
bee_uCrossover_binary <- function(object, parents, n_binary_rep, n_community){
  
  # data = c(ifelse(pracma::rand(n = 1, m = n_binary_rep_box*n_community*2) > 0.5, 1, 0))
  # data = c(1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  # parents <- matrix(data = data, ncol = n_community*n_binary_rep)
  
  parents <- object@population[parents,,drop = FALSE]
  u <- unlist(lapply(X = 1:n_community,function(i)rep(runif(1), n_binary_rep)))
  children <- parents
  children[1:2, u > 0.5] <- children[2:1, u > 0.5]
  out <- list(children = children, fitness = rep(NA,2))  
  
  return(out)
}


############################# allocate energy #############################


calculate_allocation_coefficients <- function(community_objective = "novel", individual_investment, df_characteristic_selected, plot_scenarios = F){
  
  n_community = sum(grepl("cons", colnames(df_characteristic_selected)))
  gen_sunny = df_characteristic_selected[(df_characteristic_selected$sunny == T), "energy"]
  cons_sunny = df_characteristic_selected[df_characteristic_selected$sunny == T, grep("cons", colnames(df_characteristic_selected))]
  matrix_coefficients = matrix(0, nrow = nrow(df_characteristic_selected), ncol = n_community)
  
  gen = df_characteristic_selected[, "energy"]
  cons = df_characteristic_selected[, grep("cons", colnames(df_characteristic_selected))]

  if (community_objective == "none"){
    matrix_coefficients[,] = 1/n_community
    
  } else if (community_objective == "profitable"){
    # needs individual_investment_selected
    ratio_investment = as.numeric(individual_investment/sum(individual_investment))
    matrix_coefficients = matrix(1, nrow = length(gen)) %*% matrix(ratio_investment, ncol = n_community)
    
  } else if (community_objective == "environmental"){
    # df_gen_assigned_sunny <- calculate_gen_assigned(gen_sunny, combination = rep(1, n_community))
    # optimum_coefficients = cons_sunny/df_gen_assigned_sunny
    # optimum_coefficients = optimum_coefficients/rowSums(optimum_coefficients, na.rm = T)
    # matrix_coefficients = as.matrix(optimum_coefficients)
    
    df_gen_assigned <- calculate_gen_assigned(gen, combination = rep(1, n_community))
    optimum_coefficients = cons/df_gen_assigned
    optimum_coefficients = optimum_coefficients/rowSums(optimum_coefficients, na.rm = T)
    optimum_coefficients[is.na(optimum_coefficients)] = 0
    matrix_coefficients = as.matrix(optimum_coefficients)
    
  } else if (community_objective == "constant_environmental"){
    # df_gen_assigned_sunny <- calculate_gen_assigned(gen_sunny, combination = rep(1, n_community))
    # optimum_coefficients = cons_sunny/df_gen_assigned_sunny
    # optimum_coefficients = optimum_coefficients/rowSums(optimum_coefficients, na.rm = T)
    # optimum_coefficients_mean = colMeans(optimum_coefficients, na.rm = T)
    # matrix_coefficients = as.matrix(optimum_coefficients_mean)
    # matrix_coefficients = matrix(1, nrow = nrow(df_gen_assigned_sunny)) %*% t(matrix_coefficients)

    df_gen_assigned <- calculate_gen_assigned(gen, combination = rep(1, n_community))
    optimum_coefficients = cons/df_gen_assigned
    optimum_coefficients = optimum_coefficients/rowSums(optimum_coefficients, na.rm = T)
    optimum_coefficients_mean = colMeans(optimum_coefficients, na.rm = T)
    matrix_coefficients = as.matrix(optimum_coefficients_mean)
    matrix_coefficients = matrix(1, nrow = nrow(df_gen_assigned)) %*% t(matrix_coefficients)
        
  } else if (community_objective == "novel"){
    # matrix_coefficients = calculate_matrix_coefficient_4(df_local_time = local_time, df_cons_selected_sunny = cons_sunny, df_gen_sunny = gen_sunny, 
    #                                                      df_purchase_price_one_day = purchase_price_one_day, n_community = n_community, 
    #                                                      individual_investment_selected = individual_investment)
    
    matrix_coefficients = calculate_matrix_coefficient_novel(plot_scenarios, df_characteristic_selected, n_community, individual_investment_selected)
    
  }
  
  return(matrix_coefficients)
}


calculate_matrix_coefficient_novel <- function(plot_scenarios, df_characteristic_selected, n_community, individual_investment_selected){
  # copy from calculate_matrix_coefficient_4
  
  matrix_coefficients = matrix(0, ncol = length(individual_investment_selected), nrow = nrow(df_characteristic_selected))
  # n_sunny_hours_start = 1
  
  for (month in 1:12) {
    for (week in c(F, T)) {
      
      print(month)
      print(week)

      cons_sunny_one_day = df_characteristic_selected[df_characteristic_selected$month %in% month & df_characteristic_selected$week %in% week & df_characteristic_selected$sunny == T, grep("cons", colnames(df_characteristic_selected))]
      gen_sunny_one_day = df_characteristic_selected$energy[df_characteristic_selected$month %in% month & df_characteristic_selected$week %in% week & df_characteristic_selected$sunny == T]
      purchase_price_sunny_one_day = df_characteristic_selected$purchase_price[df_characteristic_selected$month %in% month & df_characteristic_selected$week %in% week & df_characteristic_selected$sunny == T]
      weights_n_days_one_day = df_characteristic_selected$n_days[df_characteristic_selected$month %in% month & df_characteristic_selected$week %in% week & df_characteristic_selected$sunny == T]
      n_sunny_hours = length(gen_sunny_one_day)
      
      dim = calculate_dim(hourly=T, n_community, n_sunny_hours)
      
      
      if (any(is.na(cons_sunny_one_day))) {
        
        matrix_coefficients_month_week = matrix(0, ncol = length(cons_sunny_one_day), nrow = nrow(cons_sunny_one_day))
        
      } else{
        
        optim <- nsga2R_flor(fn = purrr::partial(fitness_MO,
                                                 gen_sunny_one_day = gen_sunny_one_day,
                                                 cons_sunny_one_day = cons_sunny_one_day,
                                                 purchase_price_sunny_one_day = purchase_price_sunny_one_day,
                                                 weights_n_days_one_day = weights_n_days_one_day,
                                                 individual_investment_selected = individual_investment_selected),
                             varNo = dim,
                             objDim = 2,
                             # generations = 500,
                             # TODO: number of generations 
                             generations = 10,
                             popSize = 200,
                             cprob = 0.2,
                             mprob = 0.2,
                             lowerBounds = rep(0, dim),
                             upperBounds = rep(1, dim))
        
        matrix_coefficients_month_week = choose_scenarios_normalized(optim, plot = plot_scenarios, n_community, n_sunny_hours, criteria = 2, name_plot = paste0(as.character(month),"_",as.character(week)))
      }
      
      matrix_coefficients[df_characteristic_selected$month %in% month & df_characteristic_selected$week %in% week & df_characteristic_selected$sunny,] = matrix_coefficients_month_week
      
      # n_sunny_hours_start = n_sunny_hours_start + n_sunny_hours 
    }
  }
  
  return(matrix_coefficients)
}


nsga2R_flor <- function (fn, varNo, objDim, lowerBounds = rep(-Inf, varNo), 
                         upperBounds = rep(Inf, varNo), popSize = 100, tourSize = 2, 
                         generations = 20, cprob = 0.7, XoverDistIdx = 5, mprob = 0.2, 
                         MuDistIdx = 10){
  cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
  cat("\n")
  # cat("initializing the population")
  # cat("\n")
  parent <- t(sapply(1:popSize, function(u) array(runif(length(lowerBounds), 
                                                        lowerBounds, upperBounds))))
  
  parent <- cbind(parent, t(apply(parent, 1, fn)))
  # cat("ranking the initial population")
  # cat("\n")
  ranking <- fastNonDominatedSorting(parent[, (varNo + 1):(varNo + 
                                                             objDim)])
  rnkIndex <- integer(popSize)
  i <- 1
  while (i <= length(ranking)) {
    rnkIndex[ranking[[i]]] <- i
    i <- i + 1
  }
  parent <- cbind(parent, rnkIndex)
  # cat("crowding distance calculation")
  # cat("\n")
  objRange <- apply(parent[, (varNo + 1):(varNo + objDim)], 
                    2, max) - apply(parent[, (varNo + 1):(varNo + objDim)], 
                                    2, min)
  cd <- crowdingDist4frnt(parent, ranking, objRange)
  parent <- cbind(parent, apply(cd, 1, sum))
  for (iter in 1:generations) {
    # cat("---------------generation---------------", iter, 
    #     "starts")
    # cat("\n")
    # cat("tournament selection")
    # cat("\n")
    matingPool <- tournamentSelection(parent, popSize, tourSize)
    # cat("crossover operator")
    # cat("\n")
    childAfterX <- boundedSBXover(matingPool[, 1:varNo], 
                                  lowerBounds, upperBounds, cprob, XoverDistIdx)
    # cat("mutation operator")
    # cat("\n")
    childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, 
                                       upperBounds, mprob, MuDistIdx)
    # cat("evaluate the objective fns of childAfterM")
    # cat("\n")
    childAfterM <- cbind(childAfterM, t(apply(childAfterM, 
                                              1, fn)))
    # cat("Rt = Pt + Qt")
    # cat("\n")
    parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
    # cat("ranking again")
    # cat("\n")
    ranking <- fastNonDominatedSorting(parentNext[, (varNo + 
                                                       1):(varNo + objDim)])
    i <- 1
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i
      i <- i + 1
    }
    parentNext <- cbind(parentNext, rnkIndex)
    # cat("crowded comparison again")
    # cat("\n")
    objRange <- apply(parentNext[, (varNo + 1):(varNo + 
                                                  objDim)], 2, max) - apply(parentNext[, (varNo + 
                                                                                            1):(varNo + objDim)], 2, min)
    cd <- crowdingDist4frnt(parentNext, ranking, objRange)
    parentNext <- cbind(parentNext, apply(cd, 1, sum))
    parentNext.sort <- parentNext[order(parentNext[, varNo + 
                                                     objDim + 1], -parentNext[, varNo + objDim + 2]), 
                                  ]
    # cat("environmental selection")
    # cat("\n")
    parent <- parentNext.sort[1:popSize, ]
    # cat("---------------generation---------------", iter, 
    #     "ends")
    # cat("\n")
    if (iter != generations) {
      # cat("\n")
      # cat("********** new iteration *********")
      # cat("\n")
    }
    else {
      # cat("********** stop the evolution *********")
      # cat("\n")
    }
  }
  result = list(functions = fn, parameterDim = varNo, objectiveDim = objDim, 
                lowerBounds = lowerBounds, upperBounds = upperBounds, 
                popSize = popSize, tournamentSize = tourSize, generations = generations, 
                XoverProb = cprob, XoverDistIndex = XoverDistIdx, mutationProb = mprob, 
                mutationDistIndex = MuDistIdx, parameters = parent[, 
                                                                   1:varNo], objectives = parent[, (varNo + 1):(varNo + 
                                                                                                                  objDim)], paretoFrontRank = parent[, varNo + objDim + 
                                                                                                                                                       1], crowdingDistance = parent[, varNo + objDim + 
                                                                                                                                                                                       2])
  class(result) = "nsga2R"
  cat("********** END *********")
  return(result)
}


# TODO: change by purchase_price_sunny everywhere!
fitness_MO <- function(x, gen_sunny_one_day, cons_sunny_one_day, purchase_price_sunny_one_day, weights_n_days_one_day, individual_investment_selected){
  
  # fn: the fitness function to be minimized
  # varNo: Number of decision variables
  # objDim: Number of objective functions
  # lowerBounds: Lower bounds of each decision variable
  # upperBounds: Upper bounds of each decision variable
  # popSize: Size of solution(?) population
  # generations: Number of generations
  # cprob: crossover prob
  # mprob: mutation prob
  
  # x = runif(dim, 0, 1)
  
  n_sunny_hours = nrow(cons_sunny_one_day)
  n_community = ncol(cons_sunny_one_day)
  
  coefficients_x = matrix(data = x, ncol = n_community, nrow = n_sunny_hours, byrow = T)
  coefficients_x = coefficients_x/rowSums(coefficients_x)
  
  df_gen_assigned = calculate_gen_assigned_betas(gen_sunny_one_day, matrix_coefficients = coefficients_x)
  
  surplus_x <- df_gen_assigned - cons_sunny_one_day
  surplus_x[surplus_x < 0] = 0
  
  # weighted_surplus:
  # f1_surplus = weights_n_days_one_day * surplus_x
  
  # has no sense to calculate weighted surplus here because I will end up calculating payback for one day:
  f1_surplus = sum(surplus_x)
  
  # hardcoded: 
  sale_price = 0.0508
  
  cost_old = colSums(purchase_price_sunny_one_day * cons_sunny_one_day)
  
  grid_x = cons_sunny_one_day - df_gen_assigned
  grid_x[grid_x < 0] = 0
  
  # legal framework constraint: you cant sell more than what you consumed
  surplus_x_to_sell = ifelse(colSums(surplus_x) < colSums(grid_x), colSums(surplus_x), colSums(grid_x))
  
  # changed this: 
  # cost_new = purchase_price_sunny_one_day*colSums(grid_x) - sale_price * surplus_x_to_sell
  cost_new = colSums(purchase_price_sunny_one_day*grid_x) - sale_price * surplus_x_to_sell
  
  # assuming period is a DAY
  # profit_period = cost_old - cost_new
  
  # it is impossible that cost_new is lower than cost_old, right?  
  # profit_one_day = ifelse(cost_old - cost_new > 0, cost_old - cost_new, 0)
  profit_one_day = cost_old - cost_new
  
  # hypothetical: considering all days are like this one
  profit_hypothetical_one_year = profit_one_day * 360 
  
  ##### TODO PAYBACK
  payback_years = individual_investment_selected / profit_hypothetical_one_year 
  
  # TODO:
  # payback_years[is.na(payback_years)] = 10
  # payback_years[payback_years > 15] = 15
  # payback_years[payback_years > 50] = 50
  
  payback_ideal = 0
  f2_payback = sum(exp(payback_years - payback_ideal))
  
  # f2_payback = sd(payback_years)
  # cost_payback_2 = max(payback_years) - min(payback_years) 
  
  # score <- weight_surplus * cost_surplus + (1-weight_surplus) * cost_payback
  
  return(c(f1_surplus, f2_payback))
}


calculate_gen_assigned_betas <- function(df_gen_day, matrix_coefficients){
  
  # TODO!!!
  # TODO: this should work for matrix_coefficients and matrix_coefficients_2
  
  # hourly:
  # matrix_coefficients_1 
  # non hourly:
  # matrix_coefficients_2
  
  df_gen = as.data.frame(as.matrix(df_gen_day)%*%matrix(1, ncol = ncol(matrix_coefficients)))
  df_gen_assigned <- as.data.frame(df_gen * matrix_coefficients)  
  
  # checking:
  # df_gen_assigned/df_gen
  
  return(df_gen_assigned)
}


choose_scenarios_normalized <- function(optim, plot, n_community, n_sunny_hours, criteria, name_plot){
  
  df_pareto_objectives = data.frame(optim$objectives)  
  colnames(df_pareto_objectives) = c("surplus", "payback")
  rank_1 = (optim$paretoFrontRank == 1)
  
  df_pareto_objectives_normalized = apply(df_pareto_objectives, MARGIN = 2, FUN = normalization)
  df_pareto_objectives_normalized = as.data.frame(df_pareto_objectives_normalized)
  
  z_star = data.frame("surplus" = min(df_pareto_objectives$surplus),
                      "payback" = min(df_pareto_objectives$payback))
  
  z_star_normalized = data.frame("surplus" = min(df_pareto_objectives_normalized$surplus),
                                 "payback" = min(df_pareto_objectives_normalized$payback))
  
  df_pareto_objectives_rank_1 = df_pareto_objectives[rank_1, ]
  df_pareto_objectives_normalized_rank_1 = df_pareto_objectives_normalized[rank_1, ]
  
  if(criteria == 1) {
    rank_1_criteria = calculate_selected_row_criteria_1(df_pareto_objectives_rank_1, z_star)
  }else if(criteria == 2) {
    # TODO: fix something with scales between variables
    rank_1_criteria_normalized = calculate_selected_row_criteria_2(df_pareto_objectives_normalized_rank_1, z_star_normalized)
  }
  
  df_pareto_betas = data.frame(optim$parameters)  
  df_pareto_betas_rank_1 = df_pareto_betas[rank_1, ]
  
  betas_with_criteria = as.numeric(df_pareto_betas_rank_1[rank_1_criteria_normalized,])
  coefficients = matrix(data = betas_with_criteria, ncol = n_community, nrow = n_sunny_hours, byrow = T)
  coefficients = coefficients/rowSums(coefficients)
  
  objectives_with_criteria_normalized = df_pareto_objectives_normalized_rank_1[rank_1_criteria_normalized, ]
  
  if (plot == T) {
    plot_multi_objective_criteria_selection(name = name_plot, df_pareto_objectives_normalized, z_star_normalized, objectives_with_criteria_normalized)
  }
  
  return(coefficients)
}


calculate_selected_row_criteria_1 = function(df_pareto_objectives_rank_1, z_star){
  
  # TODO: the calculus of the linear should be included here:
  # z_star_long = data.frame(matrix(1, nrow = nrow(df_pareto_objectives_rank_1)) %*% as.matrix(z_star))   
  
  criteria_selected_row = as.numeric(which.min(df_pareto_objectives_rank_1$payback))
  rank_1_criteria = 1:nrow(df_pareto_objectives_rank_1) %in% criteria_selected_row
  return(rank_1_criteria)
}


calculate_selected_row_criteria_2 = function(df_pareto_objectives_rank_1, z_star){
  
  # TODO: the calculus of the linear should be included here:
  z_star_long = data.frame(matrix(1, nrow = nrow(df_pareto_objectives_rank_1)) %*% as.matrix(z_star))   
  
  criteria_selected_row = as.numeric(which.min(rowSums((df_pareto_objectives_rank_1 - z_star_long)**2)))
  rank_1_criteria = 1:nrow(df_pareto_objectives_rank_1) %in% criteria_selected_row
  return(rank_1_criteria)
}



############################# operative #############################


calculate_surplus_hourly_individual <- function(df_gen, df_cons, combination){
  not_selected_cons = (combination == 0) 
  df_cons[, not_selected_cons] = 0
  
  df_gen_assigned <- calculate_gen_assigned(df_gen, combination)
  individual_hourly_surplus <- df_gen_assigned - df_cons
  individual_hourly_surplus[individual_hourly_surplus < 0] = 0
  
  # hourly_surplus <- rowSums(individual_hourly_surplus)
  
  # cons_total = rowSums(df_cons)
  # hourly_surplus = df_gen - cons_total
  # hourly_surplus[hourly_surplus < 0] = 0
  
  return(individual_hourly_surplus)
}


calculate_gen_assigned <- function(df_gen, combination){
  df_gen = as.data.frame(as.matrix(df_gen)%*%matrix(1, ncol =length(combination)))
  df_gen_assigned <- as.data.frame(df_gen * combination[col(df_gen)])  
  return(df_gen_assigned)
}
















normalize_genome <- function(genome, n_community, n_sunny_hours, dim, popSize){
  genome = as.numeric(t(genome))
  genome_matrix = matrix(data = genome, ncol = n_community, nrow = n_sunny_hours*popSize, byrow = T)
  genome_matrix = genome_matrix/rowSums(genome_matrix)
  # checking
  # rowSums(genome_matrix)
  genome = matrix(data = t(genome_matrix), ncol = dim, byrow = T)
  return(genome)
}


complete_consumption <- function(df_cons_characteristic_sunny, n_binary_rep){
  needed_ncol_cons = 2^n_binary_rep
  if ( ncol(df_cons_characteristic_sunny)>= needed_ncol_cons ) {
    return(df_cons_characteristic_sunny) 
  } else {
    df_cons_characteristic_sunny[, (ncol(df_cons_characteristic_sunny)+1):needed_ncol_cons] = 0
    colnames(df_cons_characteristic_sunny) = paste0("cons_",1:needed_ncol_cons)
    return(df_cons_characteristic_sunny) 
  }
}



############################# analysis ############################## 


# TODO:
convergence_speed <- function(n_community_max, n_binary_rep, df_gen, df_cons, global_investment, individual_investment){
  
  # To check convergence and execution time
  time = rep(0, 5)
  optimum_surplus = rep(0, 5)
  for (i in c(1:5)) {
    tic = Sys.time()
    optimal_combination_using_2_GAs <- optimize_hourly_betas(n_community_max, n_binary_rep, df_gen, df_cons, global_investment, individual_investment)
    toc = Sys.time()
    time[i] = toc-tic
    
    best_combination = selection_best_combination(optimal_combination_using_2_GAs)
    optimum_surplus[i] = best_combination$surplus
    
    plot_best_combination(best_combination, iteration = i)
  }
}


############################# stats ############################## 


calculate_avoided_emissions <- function(df_characteristic_selected_allocated){
  # comes from calculate_daily_avoided_emissions
  
  df_solar_consumption = df_characteristic_selected_allocated[, grepl("solarC", colnames(df_characteristic_selected_allocated))]
  n_days = df_characteristic_selected_allocated$n_days
  df_avoided_emissions = sum(df_solar_consumption * n_days * 0.357)
  
  return(df_avoided_emissions)
}


calculate_avoided_emissions_individual <- function(df_characteristic_selected_allocated){
  # comes from calculate_daily_avoided_emissions
  
  df_solar_consumption = df_characteristic_selected_allocated[, grepl("solarC", colnames(df_characteristic_selected_allocated))]
  n_days = df_characteristic_selected_allocated$n_days
  df_avoided_emissions = colSums(df_solar_consumption * n_days * 0.357)
  
  return(df_avoided_emissions)
}


calculate_self_consumption_individual <- function(df_characteristic_selected_allocated){

  solar_consumption = colSums(df_characteristic_selected_allocated[, grepl("solarC", colnames(df_characteristic_selected_allocated))])
  solar_alloc = colSums(df_characteristic_selected_allocated[, grepl("alloc", colnames(df_characteristic_selected_allocated))])
  
  solar_self_consumption = solar_consumption / solar_alloc 
  
  return(solar_self_consumption)
}


calculate_self_consumption_community_mean <- function(df_characteristic_selected_allocated){
  
  solar_consumption = sum(df_characteristic_selected_allocated[, grepl("solarC", colnames(df_characteristic_selected_allocated))])
  solar_alloc = sum(df_characteristic_selected_allocated[, grepl("alloc", colnames(df_characteristic_selected_allocated))])
  
  solar_self_consumption = solar_consumption / solar_alloc 
  
  return(solar_self_consumption)
}


calculate_self_sufficiency_individual <- function(df_characteristic_selected_allocated){

  solar_consumption = colSums(df_characteristic_selected_allocated[, grepl("solarC", colnames(df_characteristic_selected_allocated))])
  cons = colSums(df_characteristic_selected_allocated[, grepl("cons", colnames(df_characteristic_selected_allocated))])
  
  solar_self_consumption = solar_consumption / cons 
  
  return(solar_self_consumption)
}


calculate_self_sufficiency_community_mean <- function(df_characteristic_selected_allocated){
  
  solar_consumption = sum(df_characteristic_selected_allocated[, grepl("solarC", colnames(df_characteristic_selected_allocated))])
  cons = sum(df_characteristic_selected_allocated[, grepl("cons", colnames(df_characteristic_selected_allocated))])
  
  solar_self_consumption = solar_consumption / cons 
  
  return(solar_self_consumption)
}



############################# plot ############################## 


plot_raw <- function(name = "", print_plots, df, version){
  if (print_plots == F) {
    return()
  } else{
    df_plot <- df 
    df_plot[is.na(df_plot$energy), "energy"] = 0

    p <- ggplot() + 
      geom_line(aes(x = df_plot$time, y = df_plot$energy)) +
      labs(x = "Time [h]", y = "Electrical energy [kWh]", color = "")  
    ggsave(filename = paste0("graphs/version",version,"/1_raw_",name,".pdf") , plot = p, device = "pdf", width = 6, height = 3)
    return()  
  }
}


plot_characteristic <- function(name = "", print_plots, df, version){
  if (print_plots == F) {
    return()
  } else{
    df_plot <- df 
    df_plot[is.na(df_plot$energy), "energy"] = 0

    p <- ggplot() + 
      geom_line(aes(x = df_plot$time, y = df_plot$energy)) +
      labs(x = "Time [h]", y = "Electrical energy [kWh]", color = "")  
    ggsave(filename = paste0("graphs/version",version,"/1_characteristic_",name,".pdf") , plot = p, device = "pdf", width = 6, height = 3)
    return()  
  }
}


plot_monthly_characteristic <- function(name, print_plots, binary_week = T, df_cons_characteristic, cons_to_plot, version){
  
  if (print_plots == T) {
    df_cons_characteristic_week = df_cons_characteristic[df_cons_characteristic$week == binary_week, c("month", "hour", cons_to_plot)]
    
    df_plot = df_cons_characteristic_week
    df_plot = reshape2::melt(df_plot, id.vars = c("hour", "month"))
    
    df_plot$month = as.factor(df_plot$month)
    levels(df_plot$month) = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") 
    
    p3 <- ggplot(df_plot, aes(x = hour, y = value)) +
      geom_line() +
      facet_grid(month ~ .) + #, scales = "free") +   
      labs(x = "Time [h]", y = "Energy [kWh]", fill = "") +
      # scale_y_continuous(position = "right") +
      theme(text = element_text(size=14), legend.position="bottom")
    ggsave(filename = paste0("graphs/version",version,"/1_monthly_characteristic_",name,".pdf") , plot = p3, device = "pdf", width = 6, height = 9)

    return()  
  } else{
    return()  
  }
}


plot_monthly_characteristic_selected_consumptions <- function(print_plots, df_characteristic){
  
  if (print_plots == T) {
    y_end = max(df_characteristic[grep("cons", colnames(df_characteristic))], na.rm = T)
    for (cons_to_plot in colnames(df_characteristic)[grep("cons", colnames(df_characteristic))]) {
      for (binary_week in c(T, F)) {
        df_cons_characteristic_week = df_characteristic[df_characteristic$week == binary_week, c("month", "hour", cons_to_plot)]
        
        df_plot = df_cons_characteristic_week
        df_plot = melt(df_plot, id.vars = c("hour", "month"))
        
        df_plot$month = as.factor(df_plot$month)
        levels(df_plot$month) = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") 
        
        p3 <- ggplot(df_plot, aes(x = hour, y = value)) +
          geom_line() +
          facet_grid(month ~ .) + #, scales = "free") +   
          labs(x = "Time [h]", y = "Energy [kWh]", fill = "") +
          # scale_y_continuous(position = "right") +
          ylim(0,y_end) + 
          theme(text = element_text(size=14), legend.position="bottom")
        ggsave(filename = paste0("graphs/version",version,"/2_monthly_characteristic_",cons_to_plot,"_",binary_week,".pdf"), plot = p3, device = "pdf", width = 8, height = 10)
      }
    }
    return()  
    
  } else{
    return()  
  }
}


plot_multi_objective_criteria_selection_scenarios <- function(name, df_scenarios, df_pareto_objectives, z_star, objectives_with_criteria){
  
  r = (sum(c(objectives_with_criteria$surplus, objectives_with_criteria$payback) - z_star)**2)**0.5  
  
  theta = seq(from = 0, to = (2*pi), length.out = 100)
  x_circular = z_star$surplus + r * cos(theta)
  y_circular = z_star$payback + r * sin(theta)
  
  p = ggplot() +
    geom_point(aes(x = df_pareto_objectives[, "surplus"], y = df_pareto_objectives[, "payback"])) +
    geom_point(aes(x = z_star$surplus, y = z_star$payback), shape = 4) +
    geom_point(aes(x = df_scenarios$surplus[1], y = df_scenarios$payback[1]), shape = 1) +
    geom_point(aes(x = df_scenarios$surplus[2], y = df_scenarios$payback[2]), shape = 2) +
    geom_point(aes(x = df_scenarios$surplus[3], y = df_scenarios$payback[3]), shape = 3) 
  # geom_line(aes(x = x_lineal, y = y_lineal)) +
  # geom_point(aes(x = objectives_with_criteria$surplus, y = objectives_with_criteria$payback), shape = 5, size = 3) 
  # geom_point(aes(x = x_circular, y = y_circular))
  print(df_scenarios$surplus[1], df_scenarios$payback[1])
  print(df_scenarios$surplus[2], df_scenarios$payback[2])
  print(df_scenarios$surplus[3], df_scenarios$payback[3])
  
  ggsave(filename = paste0("graphs/multi_objective_criteria_",name,".pdf"), plot = p, width = 6, height = 3)
  
  return(p)
}


plot_multi_objective_criteria_selection <- function(name, df_pareto_objectives, z_star, objectives_with_criteria){
  
  r = (sum(c(objectives_with_criteria$surplus, objectives_with_criteria$payback) - z_star)**2)**0.5  
  
  theta = seq(from = 0, to = (2*pi), length.out = 100)
  x_circular = z_star$surplus + r * cos(theta)
  y_circular = z_star$payback + r * sin(theta)
  
  p = ggplot() +
    geom_point(aes(x = df_pareto_objectives$surplus, y = df_pareto_objectives$payback)) +
    geom_point(aes(x = z_star$surplus, y = z_star$payback), shape = 4) +
    geom_point(aes(x = objectives_with_criteria$surplus, y = objectives_with_criteria$payback), shape = 5, size = 3) +
    labs(x = "f1 (Solar Excess)", fill = "f2 (Payback)") 
  ggsave(filename = paste0("graphs/multi_objective_criteria_",name,".pdf"), plot = p, width = 6, height = 3)
  
  return(p)
}


# plot_matrix <- function(name, matrix_coefficients, color_limits){
plot_matrix <- function(name, matrix_coefficients, version){
  
  rownames(matrix_coefficients) = NULL
  longData = melt(matrix_coefficients)
  longData = longData[longData$value!=0,]
  
  p <- ggplot(longData, aes(x = Var2, y = Var1)) + 
    geom_raster(aes(fill=value)) + 
    # scale_fill_gradient(low="grey90", high="red", limits = color_limits) +
    scale_fill_gradient(low="grey90", high="red") +
    labs(x="users", y="daytime", title="") +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  ggsave(filename = paste0("graphs/version",version,"/3_matrix_allocation_",name,".pdf"), plot = p, device = "pdf", width = 6, height = 3)
  return()
}


plot_mean_allcoted_community <- function(df_characteristic_selected_allocated, week_selected, version){
  # comes from plot_disaggregated_community_betas_year_area_mean_final
  
  df_plot3_aux = df_characteristic_selected_allocated[, grep("month|week|hour|surplus|solarC|gridC", colnames(df_characteristic_selected_allocated))]
  
  df_plot3_aux$Solar_surplus = rowSums(df_plot3_aux[,grep("surplus", colnames(df_plot3_aux))])
  df_plot3_aux$Solar_consumption = rowSums(df_plot3_aux[,grep("solarC", colnames(df_plot3_aux))])
  df_plot3_aux$Grid_consumption = rowSums(df_plot3_aux[,grep("gridC", colnames(df_plot3_aux))])
  
  df_plot3 = df_plot3_aux[, grep("month|week|hour|Solar_surplus|Solar_consumption|Grid_consumption", colnames(df_plot3_aux))]
  
  # TODO: should change this:
  df_plot3 = df_plot3[df_plot3$week == week_selected, ]
  # TODO: should change this:
  df_plot3_mean_agg = aggregate(x = df_plot3, by = list(df_plot3$hour), FUN = 
                                  function(x){
                                    return(mean(x, na.rm = T))
                                    })
  
  df_plot3_mean_agg = df_plot3_mean_agg[, c(-1, -2, -3)]
  df_plot3_mean = melt(df_plot3_mean_agg, id.vars = "hour")

  levels(df_plot3_mean$variable) = c("Solar Excess", "Solar Consumption", "Grid Consumption")
  
  self_sufficiency = sum(df_plot3_mean_agg$Solar_consumption) / ( sum(df_plot3_mean_agg$Solar_consumption) + sum(df_plot3_mean_agg$Grid_consumption) ) 
  solar_self_consumption = sum(df_plot3_mean_agg$Solar_consumption) / ( sum(df_plot3_mean_agg$Solar_consumption) + sum(df_plot3_mean_agg$Solar_surplus) ) 
  
  p3 <- ggplot(df_plot3_mean, aes(x = hour, y = value, fill = variable)) +
    geom_area(alpha = 0.5) + 
    labs(x = "Time [h]", y = "Energy [kWh]", "title" = paste0("Self-consumption = ", (round(solar_self_consumption, digits = 2)*100),"% & Self-sufficiency = ", (round(self_sufficiency, digits = 2)*100),"%"), fill = "") +
    # scale_y_continuous(position = "right") +
    theme(text = element_text(size=14), legend.position="bottom") 
  
  ggsave(filename = paste0("graphs/version",version,"/3_mean_allocated_community.pdf"), plot = p3, device = "pdf", width = 14, height = 5)

}


plot_mean_allcoted_community_bis <- function(df_characteristic_selected_allocated, week_selected, version){
  # comes from plot_disaggregated_community_betas_year_area_mean_final
  
  df_plot3_aux = df_characteristic_selected_allocated[, grep("month|week|hour|surplus|solarC|gridC", colnames(df_characteristic_selected_allocated))]
  
  df_plot3_aux$Solar_surplus = rowSums(df_plot3_aux[,grep("surplus", colnames(df_plot3_aux))])
  df_plot3_aux$Solar_consumption = rowSums(df_plot3_aux[,grep("solarC", colnames(df_plot3_aux))])
  df_plot3_aux$Grid_consumption = rowSums(df_plot3_aux[,grep("gridC", colnames(df_plot3_aux))])
  
  df_plot3 = df_plot3_aux[, grep("month|week|hour|Grid_consumption|Solar_surplus|Solar_consumption", colnames(df_plot3_aux))]
  
  # TODO: should change this:
  df_plot3 = df_plot3[df_plot3$week == week_selected, ]
  # TODO: should change this:
  df_plot3_mean_agg = aggregate(x = df_plot3, by = list(df_plot3$hour), FUN = 
                                  function(x){
                                    return(mean(x, na.rm = T))
                                  })
  
  df_plot3_mean_agg = df_plot3_mean_agg[, c(-1, -2, -3)]
  df_plot3_mean = melt(df_plot3_mean_agg, id.vars = "hour")

  df_plot3_mean$variable = factor(df_plot3_mean$variable, levels = c("Solar_surplus", "Grid_consumption",  "Solar_consumption"))
  # df_plot3_mean = df_plot3_mean[sort(df_plot3_mean$variable, by = c("Solar_surplus", "Grid_consumption",  "Solar_consumption")), ]
  
  levels(df_plot3_mean$variable) = c("Solar Excess", "Grid Consumption", "Solar Consumption")
  # levels(df_plot3_mean$variable) = levels(df_plot3_mean$variable)[c(3, 2, 1)]
  
  
  df_plot3_aux2 = df_characteristic_selected_allocated[, grep("month|week|hour|cons", colnames(df_characteristic_selected_allocated))]
  df_plot3_aux2$Consumption = rowSums(df_plot3_aux2[,grep("cons", colnames(df_plot3_aux2))])
  
  df_plot3_aux2 = df_plot3_aux2[, grep("month|week|hour|Consumption", colnames(df_plot3_aux2))]
  
  
  df_plot3_aux2 = df_plot3_aux2[df_plot3_aux2$week == week_selected, ]

  df_plot3_mean_agg2 = aggregate(x = df_plot3_aux2, by = list(df_plot3_aux2$hour), FUN = 
                                  function(x){
                                    return(mean(x, na.rm = T))
                                  })
  
  df_plot3_mean_agg2 = df_plot3_mean_agg2[, c(-1, -2, -3)]
  df_plot3_mean_agg2 = melt(df_plot3_mean_agg2, id.vars = "hour")
  
    
  self_sufficiency = sum(df_plot3_mean_agg$Solar_consumption) / ( sum(df_plot3_mean_agg$Solar_consumption) + sum(df_plot3_mean_agg$Grid_consumption) ) 
  solar_self_consumption = sum(df_plot3_mean_agg$Solar_consumption) / ( sum(df_plot3_mean_agg$Solar_consumption) + sum(df_plot3_mean_agg$Solar_surplus) ) 
  
  # p3 <- ggplot(df_plot3_mean, aes(x = hour, y = value, fill = variable)) +
  #   geom_area(alpha = 0.5) + 
  #   labs(x = "Time [h]", y = "Energy [kWh]", "title" = paste0("Self-consumption = ", (round(solar_self_consumption, digits = 2)*100),"% & Self-sufficiency = ", (round(self_sufficiency, digits = 2)*100),"%"), fill = "") +
  #   theme(text = element_text(size=14), legend.position="bottom") +
  #   scale_fill_discrete(breaks = c("Solar_surplus", "Solar_consumption",  "Grid_consumption")) +
  #   geom_line(x = df_plot3_mean_agg2$hour, y = df_plot3_mean_agg2$value)


  p3 <- ggplot() +
    geom_area(aes(x = df_plot3_mean$hour, y = df_plot3_mean$value, fill = df_plot3_mean$variable), alpha = 0.5) +
    scale_fill_manual(values = c("#F8766D","#619CFF","#00BA38")) + #,"orange","tan","purple","darkgray"))
    labs(x = "Time [h]", y = "Energy [kWh]", "title" = paste0("Self-consumption = ", (round(solar_self_consumption, digits = 2)*100),"% & Self-sufficiency = ", (round(self_sufficiency, digits = 2)*100),"%"), fill = "") +
    theme(text = element_text(size=14), legend.position="bottom") +
    # scale_fill_discrete(breaks = c("Solar_surplus", "Solar_consumption",  "Grid_consumption")) +
    geom_line(aes(x = df_plot3_mean_agg2$hour, y = df_plot3_mean_agg2$value))
  
  
    
  ggsave(filename = paste0("graphs/version",version,"/3_mean_allocated_communityCHANGEDCOLORS.pdf"), plot = p3, device = "pdf", width = 14, height = 5)
  
}


plot_mean_allocated_participant <- function(participant, df_characteristic_selected_allocated, version){
  # comes from plot_disaggregated_community_betas_year_area_mean_final
  
  df_plot3_aux = df_characteristic_selected_allocated[, grep("month|week|hour|surplus|solarC|gridC", colnames(df_characteristic_selected_allocated))]
  
  df_plot3_aux$Solar_surplus = df_plot3_aux[,grep("surplus", colnames(df_plot3_aux))]
  df_plot3_aux$Solar_consumption = df_plot3_aux[,grep("solarC", colnames(df_plot3_aux))]
  df_plot3_aux$Grid_consumption = df_plot3_aux[,grep("gridC", colnames(df_plot3_aux))]
  
  df_plot3 = df_plot3_aux[, grep("month|week|hour|Solar_surplus|Solar_consumption|Grid_consumption", colnames(df_plot3_aux))]
  
  # TODO: should change this:
  # df_plot3 = df_plot3[df_plot3$week == week_selected, ]
  # TODO: should change this:
  df_plot3_mean_agg = aggregate(x = df_plot3, by = list(df_plot3$hour), FUN = 
                                  function(x){
                                    return(mean(x, na.rm = T))
                                  })
  
  df_plot3_mean_agg = df_plot3_mean_agg[, c(-1, -2, -3)]
  df_plot3_mean = melt(df_plot3_mean_agg, id.vars = "hour")
  
  levels(df_plot3_mean$variable) = c("Solar Excess", "Solar Consumption", "Grid Consumption")
  
  self_sufficiency = sum(df_plot3_mean_agg$Solar_consumption) / ( sum(df_plot3_mean_agg$Solar_consumption) + sum(df_plot3_mean_agg$Grid_consumption) ) 
  solar_self_consumption = sum(df_plot3_mean_agg$Solar_consumption) / ( sum(df_plot3_mean_agg$Solar_consumption) + sum(df_plot3_mean_agg$Solar_surplus) ) 
  
  p3 <- ggplot(df_plot3_mean, aes(x = hour, y = value, fill = variable)) +
    geom_area(alpha = 0.5) + 
    labs(x = "Time [h]", y = "Energy [kWh]", "title" = paste0("Self-consumption = ", (round(solar_self_consumption, digits = 2)*100),"% & Self-sufficiency = ", (round(self_sufficiency, digits = 2)*100),"%"), fill = "") +
    # scale_y_continuous(position = "right") +
    theme(text = element_text(size=14), legend.position="bottom") 
  
  ggsave(filename = paste0("graphs/version",version,"/3_mean_allocated_participant_",participant,".pdf"), plot = p3, device = "pdf", width = 14, height = 5)
  
}


plot_monthly_allocated_community <- function(df_characteristic_selected_allocated, week_selected, version){
  # comes from plot_disaggregated_community_betas_year_area_facets_3
  
  df_plot3_aux = df_characteristic_selected_allocated[, grep("month|week|hour|surplus|solarC|gridC", colnames(df_characteristic_selected_allocated))]
  
  df_plot3_aux$Solar_surplus = rowSums(df_plot3_aux[,grep("surplus", colnames(df_plot3_aux))])
  df_plot3_aux$Solar_consumption = rowSums(df_plot3_aux[,grep("solarC", colnames(df_plot3_aux))])
  df_plot3_aux$Grid_consumption = rowSums(df_plot3_aux[,grep("gridC", colnames(df_plot3_aux))])
  
  df_plot3 = df_plot3_aux[, grep("month|week|hour|Solar_surplus|Solar_consumption|Grid_consumption", colnames(df_plot3_aux))]
  
  # TODO: should change this:
  df_plot3 = df_plot3[df_plot3$week == week_selected, ]
  # TODO: should change this:
  # df_plot3_mean_agg = aggregate(x = df_plot3, by = list(df_plot3$hour), FUN = 
  #                                 function(x){
  #                                   return(mean(x, na.rm = T))
  #                                 })
  # df_plot3_mean_agg = df_plot3_mean_agg[, c(-2)]
  
  df_plot3 = df_plot3[, c(-2)]
  df_plot3 = melt(df_plot3, id.vars = c("hour", "month"))
  
  df_plot3$month = as.factor(df_plot3$month)
  levels(df_plot3$month) = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") 
  levels(df_plot3$variable) = c("Solar Excedent", "Solar Consumption", "Grid Consumption")
  
  p3 <- ggplot(df_plot3, aes(x = hour, y = value, fill = variable)) +
    geom_area(alpha = 0.5) +
    facet_grid(month ~ .) + #, scales = "free") +   
    labs(x = "Time [h]", y = "Energy [kWh]", fill = "") +
    # scale_y_continuous(position = "right") +
    theme(text = element_text(size=14), legend.position="bottom")
  ggsave(filename = paste0("graphs/version",version,"/3_monthly_allocated_community.pdf"), plot = p3, device = "pdf", width = 8, height = 10)

  return()
}


plot_monthly_allocated_participant <- function(participant, df_characteristic_selected_allocated, week_selected, version){
  # comes from plot_disaggregated_community_betas_year_area_facets_3
  
  df_plot3_aux = df_characteristic_selected_allocated[, grep("month|week|hour|surplus|solarC|gridC", colnames(df_characteristic_selected_allocated))]
  
  df_plot3_aux$Solar_surplus = df_plot3_aux[,grep("surplus", colnames(df_plot3_aux))]
  df_plot3_aux$Solar_consumption = df_plot3_aux[,grep("solarC", colnames(df_plot3_aux))]
  df_plot3_aux$Grid_consumption = df_plot3_aux[,grep("gridC", colnames(df_plot3_aux))]
  
  df_plot3 = df_plot3_aux[, grep("month|week|hour|Solar_surplus|Solar_consumption|Grid_consumption", colnames(df_plot3_aux))]
  
  # TODO: should change this:
  df_plot3 = df_plot3[df_plot3$week == week_selected, ]
  # TODO: should change this:
  # df_plot3_mean_agg = aggregate(x = df_plot3, by = list(df_plot3$hour), FUN = 
  #                                 function(x){
  #                                   return(mean(x, na.rm = T))
  #                                 })
  # df_plot3_mean_agg = df_plot3_mean_agg[, c(-2)]
  
  df_plot3 = df_plot3[, c(-2)]
  df_plot3 = melt(df_plot3, id.vars = c("hour", "month"))
  
  df_plot3$month = as.factor(df_plot3$month)
  levels(df_plot3$month) = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec") 
  levels(df_plot3$variable) = c("Solar Excedent", "Solar Consumption", "Grid Consumption")
  
  p3 <- ggplot(df_plot3, aes(x = hour, y = value, fill = variable)) +
    geom_area(alpha = 0.5) +
    facet_grid(month ~ .) + #, scales = "free") +   
    labs(x = "Time [h]", y = "Energy [kWh]", fill = "") +
    # scale_y_continuous(position = "right") +
    theme(text = element_text(size=14), legend.position="bottom")
  ggsave(filename = paste0("graphs/version",version,"/3_monthly_allocated_participant_",participant,"_",week_selected,".pdf"), plot = p3, device = "pdf", width = 8, height = 10)
  
  return()
}


plot_allocation_participants <- function(df_characteristic_selected_allocated, selected_3participants, individual_investment_selected, paybacks, version){
  # comes from plot_disaggregated_community_betas_year_area_mean_final (the second part)

  df_characteristic_selected_allocated_aux = df_characteristic_selected_allocated[, grep("month|week|hour", colnames(df_characteristic_selected_allocated))]
  df_characteristic_selected_allocated_aux = cbind(df_characteristic_selected_allocated_aux, df_characteristic_selected_allocated[, grep(paste0(selected_3participants, collapse = "|"), colnames(df_characteristic_selected_allocated))])
  
  individual_investment_selected_3participants = individual_investment_selected[selected_3participants] 
  payback_selected = paybacks[selected_3participants]
  
  investment_order = order(individual_investment_selected_3participants, decreasing = T)
  investment_ordered = individual_investment_selected_3participants[order(individual_investment_selected_3participants, decreasing = T)]
  
  
  df_solar_consumption =  df_characteristic_selected_allocated_aux[, grep("hour|solarC", colnames(df_characteristic_selected_allocated_aux))]
  df_gen_assigned =  df_characteristic_selected_allocated_aux[, grep("hour|alloc", colnames(df_characteristic_selected_allocated_aux))]

  df_solar_consumption_ordered =  df_solar_consumption[, c(1, 1+investment_order)]
  df_gen_assigned_ordered =  df_gen_assigned[, c(1, 1+investment_order)]
  
  payback_selected = payback_selected[investment_order]

  colnames(df_solar_consumption_ordered) = c("hour", 1:(ncol(df_solar_consumption_ordered)-1))
  colnames(df_gen_assigned_ordered) = c("hour", 1:(ncol(df_solar_consumption_ordered)-1))
  
  df_solar_consumption_mean = aggregate(x = df_solar_consumption_ordered, by = list(df_solar_consumption_ordered$hour), FUN = function(x){return(mean(x, na.rm = T))})
  df_gen_assigned_mean = aggregate(x = df_gen_assigned_ordered, by = list(df_gen_assigned_ordered$hour), FUN = function(x){return(mean(x, na.rm = T))})
  
  df_purchase_price = df_characteristic_selected_allocated[, grep("hour|purchase_price", colnames(df_characteristic_selected_allocated))]
  df_purchase_price_mean = aggregate(x = df_purchase_price, by = list(df_purchase_price$hour), FUN = function(x){return(mean(x, na.rm = T))})
  df_purchase_price_mean = df_purchase_price_mean[, c(2, 3)]
  
  df_plot_solar_consumption_mean <- melt(data = df_solar_consumption_mean[, -grep(pattern = "Group.1", x = colnames(df_solar_consumption_mean))], variable.name = "series", id.vars = "hour")
  df_plot_gen_assigned_mean <- melt(data = df_gen_assigned_mean[, -grep(pattern = "Group.1", x = colnames(df_gen_assigned_mean))], variable.name = "series", id.vars = "hour")
  # df_plot_price_mean = melt(data = df_purchase_price_mean[, -grep(pattern = "Group.1", x = colnames(df_gen_assigned_mean))], variable.name = "series", id.vars = "hour")
  
    
  df_plot_solar_assigned_cons_mean = cbind(df_plot_gen_assigned_mean, df_plot_solar_consumption_mean$value)
  
  colnames(df_plot_solar_assigned_cons_mean)[c(3,4)] = c("solar_assig", "solar_cons")

  
  df_plot_solar_assigned_cons_mean = merge(df_plot_solar_assigned_cons_mean, df_purchase_price_mean, by = "hour")
  
  # investment:
  df_plot_investment = data.frame("series" = unique(df_plot_solar_consumption_mean$series), 
                                  "value" = investment_ordered)
  
  p1 <- ggplot(df_plot_investment, aes(x = series, y = value, fill = series))+
    geom_bar(stat = "identity", alpha = 0.5)  + 
    theme(text = element_text(size=14), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.text = element_blank(), legend.title = element_blank(), legend.position = "none") + 
    labs(y = "Investment [$]" , fill = "User") +
    # ylim(0, 950) +
    scale_fill_brewer(palette="Dark2")
  # ggsave(filename = paste0("graphs/investment",name), plot = p1, device = "pdf", width = 5, height = 3)
  
  # assignment:
  # colnames(df_purchase_price_one_day)[2] = "hour"
  # df_plot_solar_assigned_cons_mean = merge(x = df_plot_solar_assigned_cons_mean, y = df_purchase_price_one_day)
  
  
  levels(df_plot_solar_assigned_cons_mean$series) = as.factor(round(payback_selected, digits = 2))
  
  scale_factor = (max(df_plot_solar_assigned_cons_mean$solar_assig) - min(df_plot_solar_assigned_cons_mean$solar_assig))/ (max(df_plot_solar_assigned_cons_mean$purchase_price) - min(df_plot_solar_assigned_cons_mean$purchase_price))

  # changed to standarize the scale :)
  # scale_factor = (0.50 - 0) / (0.299 - 0.2198)
  
  # mean_price = (max(df_plot_solar_assigned_cons_mean$purchase_price) + 0)/ 2
  mean_price = (max(df_plot_solar_assigned_cons_mean$purchase_price) + min(df_plot_solar_assigned_cons_mean$purchase_price))/ 2
  # mean_price = mean(df_plot_solar_assigned_cons_mean$purchase_price)
  
  # mean_solar_assig = mean(df_plot_solar_assigned_cons_mean$solar_assig)
  mean_solar_assig = (max(df_plot_solar_assigned_cons_mean$solar_assig) + min(df_plot_solar_assigned_cons_mean$solar_assig))/ 2
  
  p2 <- ggplot(df_plot_solar_assigned_cons_mean) +
    # geom_line(aes(x = df_gen_mean$hour, y = df_gen_mean$energy)) +
    geom_area(aes(x = hour, y = solar_assig, fill = series), position = 'stack', alpha = 0.5) + #, linetype = 1, colour="black", show.legend = FALSE) +
    geom_area(aes(x = hour, y = solar_cons, fill = series), position = 'stack', alpha = 0.5) + #, linetype = 1, colour="black", show.legend = FALSE) +
    geom_line(aes(x = hour, y = ((purchase_price - mean_price) * scale_factor + mean_solar_assig)), linetype = 2) + #, colour = "black")  +
    facet_grid(df_plot_solar_assigned_cons_mean$series ~ .) + 
    # TODO: should change the scale_factor here:
    scale_y_continuous("Solar Allocated Energy [kWh]",
                       sec.axis = sec_axis(~ (. - mean_solar_assig) / scale_factor + mean_price, 
                                           name = "Price [$/kWh]")) +
    # scale_x_discrete() +
    scale_x_continuous(limits = c(4, 20)) +
    theme(strip.background = element_blank(), strip.text = element_blank()) + 
    labs(x = "Time [h]", fill = "Ind Payback [years]") +
    scale_fill_brewer(palette="Dark2")

  # scale_y_continuous("Solar Generation [kWh]", sec.axis = sec_axis(~./scale_factor, name = "Price [$/kWh]")) +
  
    
  library(ggpubr)
  p_1_2 <- ggarrange(p1, p2, widths = c(1, 3), ncol = 2)
  ggsave(filename = paste0("graphs/version",version,"/3_allocation.pdf"), plot = p_1_2, device = "pdf", width = 14, height = 5)
  

  return()
}


plot_comparison_stats_cut <- function(df_comparison){
  # comes from plot_comparison_stats_cut

  df_plot = df_comparison[grepl("Solar excess|Avoided CO2|Self-consumption|Self-sufficiency", df_comparison$stat), ]
    
  p1 <- ggplot(df_plot, aes(x = variable, y = value, fill = variable)) +
    geom_bar(alpha = 0.5, stat = "identity", width = 0.5) + 
    facet_grid(stat ~ ., scales = "free", switch = "y") + 
    # labs(x = TeX("$\\beta$ assignment"), y = "Global Solar Excess [kWh]", fill = "") + 
    labs(x = "Environmental", y = "", fill = "EC") +
    # geom_text(aes(label = round(value, digits = 1)), position=position_dodge(width=0.9), vjust=0.5) + 
    scale_y_continuous(position = "right") +
    # theme(text = element_text(size=17), legend.position= "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
    theme(text = element_text(size=17), legend.position= "none", axis.text.x=element_blank(), axis.ticks.x=element_blank())
  

  
  df_plot = df_comparison[grepl("payback", df_comparison$stat), ]


  p2 <- ggplot(df_plot, aes(x = variable, y = value, fill = variable)) +
    geom_bar(alpha = 0.5, stat = "identity", width = 0.5) + 
    facet_grid(stat ~ ., scales = "free", switch = "y") + 
    # labs(x = TeX("$\\beta$ assignment"), y = "Global Solar Excess [kWh]", fill = "") + 
    labs(x = "Economic", y = "", fill = "REC") +
    # geom_text(aes(label = round(value, digits = 1)), position=position_dodge(width=0.9), vjust=0.5) + 
    scale_y_continuous(position = "right") +
    # theme(text = element_text(size=17), legend.position="right", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
    theme(text = element_text(size=17), legend.position="right", axis.text.x=element_blank(), axis.ticks.x=element_blank())
  
  # ggsave(filename = paste0("graphs/general_comparison_cut_2",name,".pdf"), plot = p1, device = "pdf", width = 5, height = 7)
  ggsave(filename = paste0("graphs/general_comparison_cut_2.pdf"), plot = p1, device = "pdf", width = 4.8, height = 8.5)
  
  p_1_2 <- ggarrange(p1, p2, widths = c(1.4, 2), ncol = 2)
  ggsave(filename = paste0("graphs/general_comparison_cut_new.pdf"), plot = p_1_2, device = "pdf", width = 9, height = 8.5)
  
  
  return()
}  


############################# AUX - main #############################


select_best_combinations_betas <- function(optimal_combination_using_2_GAs){
  
  # pre_optimum_coefficients = optimal_combination_using_2_GAs$pre_optimum_coefficients
  # pre_surplus = optimal_combination_using_2_GAs$pre_surplus
  # pre_payback = optimal_combination_using_2_GAs$pre_payback
  new_optimum_coefficients = optimal_combination_using_2_GAs$new_optimum_coefficients
  new_surplus = optimal_combination_using_2_GAs$new_surplus
  new_payback = optimal_combination_using_2_GAs$new_payback
  
  index_order = order(new_surplus, decreasing = F)
  
  optimum_coefficients = new_optimum_coefficients[[index_order[1]]]
  surplus = new_surplus[index_order[1]]
  payback = new_payback[index_order[1], ]
  
  return(list("optimum_coefficients" = optimum_coefficients,
              "surplus" = surplus, 
              "payback" = payback))
}  


calculate_characteristic_days <- function(df, number_selected_year){
  
  colnames(df) = c("time", "energy")
  df_characteristic = list()
  for (m in 1:12) {
    
    # m = 1
    if (m < 11) {
      days_month = seq(from = as.Date(paste0(as.character(number_selected_year),"-",m,"-01")), to = as.Date(paste0(as.character(number_selected_year),"-",m+1,"-01")), by = "day")    
    }else{
      days_month = seq(from = as.Date(paste0(as.character(number_selected_year),"-",m,"-01")), to = as.Date(paste0(as.character(number_selected_year+1),"-",1,"-01")), by = "day")      
    }
    
    days_month = days_month[-length(days_month)]
    
    days_month_week = days_month[weekdays(days_month, abbreviate = T) %in% c("lun", "mar", "mié", "jue", "vie")]
    days_month_end_week = days_month[weekdays(days_month, abbreviate = T) %in% c("sáb", "dom")]
    
    ##
    
    df_mean_1 = data.frame("hour" = 0:23, "energy" = 0)
    
    df_month_week = df[as.Date(df$time) %in% days_month_week, ]
    df_month_week_clean = df_month_week[!is.na(df_month_week$energy), ]
    
    ##
    
    df_mean_2 = data.frame("hour" = 0:23, "energy" = 0)
    
    df_month_end_week = df[as.Date(df$time) %in% days_month_end_week, ]
    df_month_end_week_clean = df_month_end_week[!is.na(df_month_end_week$energy), ]
    
    
    if((any(!(0:23 %in% unique(hour(df_month_week_clean$time))))) | (any(!(0:23 %in% unique(hour(df_month_end_week_clean$time)))))){

      df_characteristic[[m]] = NA
    
    }else{
      
      df_mean_incomplete = aggregate(df_month_week_clean$energy, by = list(hour(df_month_week_clean$time)), FUN = mean)
      colnames(df_mean_incomplete) = c("hour", "energy")
      
      df_mean_1[df_mean_1$hour %in% df_mean_incomplete$hour, "energy"] = df_mean_incomplete$energy
      df_mean_week = df_mean_1
      
      ##
      
      df_mean_incomplete = aggregate(df_month_end_week_clean$energy, by = list(hour(df_month_end_week_clean$time)), FUN = mean)
      colnames(df_mean_incomplete) = c("hour", "energy")
      
      df_mean_2[df_mean_2$hour %in% df_mean_incomplete$hour, "energy"] = df_mean_incomplete$energy
      df_mean_end_week = df_mean_2
      
      df_characteristic[[m]] = rbind(df_mean_week, df_mean_end_week)
    }
  }
  
  return(df_characteristic)
}


calculate_characteristic_days_2 <- function(df, number_selected_year){
  
  colnames(df) = c("time", "energy")
  df_characteristic = list()
  for (m in 1:12) {
    
    # m = 1
    if (m < 11) {
      days_month = seq(from = as.Date(paste0(as.character(number_selected_year),"-",m,"-01")), to = as.Date(paste0(as.character(number_selected_year),"-",m+1,"-01")), by = "day")    
    }else{
      days_month = seq(from = as.Date(paste0(as.character(number_selected_year),"-",m,"-01")), to = as.Date(paste0(as.character(number_selected_year+1),"-",1,"-01")), by = "day")      
    }
    
    days_month = days_month[-length(days_month)]
    
    days_month_week = days_month[weekdays(days_month, abbreviate = T) %in% c("lun", "mar", "mié", "jue", "vie")]
    days_month_end_week = days_month[weekdays(days_month, abbreviate = T) %in% c("sáb", "dom")]
    ##
    
    df_mean_1 = data.frame("week" = T, "hour" = 0:23, "energy" = NA)
    
    df_month_week = df[as.Date(df$time) %in% days_month_week, ]
    df_month_week_clean = df_month_week[!is.na(df_month_week$energy), ]
    
    ##
    
    df_mean_2 = data.frame("week" = F, "hour" = 0:23, "energy" = NA)
    
    df_month_end_week = df[as.Date(df$time) %in% days_month_end_week, ]
    df_month_end_week_clean = df_month_end_week[!is.na(df_month_end_week$energy), ]

    if((any(!(0:23 %in% unique(hour(df_month_week_clean$time))))) | (any(!(0:23 %in% unique(hour(df_month_end_week_clean$time)))))){
      
      df_characteristic[[m]] = rbind(df_mean_1, df_mean_2)

    }else{
      
      df_mean_incomplete = aggregate(df_month_week_clean$energy, by = list(hour(df_month_week_clean$time)), FUN = mean)
      colnames(df_mean_incomplete) = c("hour", "energy")
      
      df_mean_1[df_mean_1$hour %in% df_mean_incomplete$hour, "energy"] = df_mean_incomplete$energy
      df_mean_week = df_mean_1
      
      ##
      
      df_mean_incomplete = aggregate(df_month_end_week_clean$energy, by = list(hour(df_month_end_week_clean$time)), FUN = mean)
      colnames(df_mean_incomplete) = c("hour", "energy")
      
      df_mean_2[df_mean_2$hour %in% df_mean_incomplete$hour, "energy"] = df_mean_incomplete$energy
      df_mean_end_week = df_mean_2
      
      df_characteristic[[m]] = rbind(df_mean_end_week, df_mean_week)
    }
  }
  
  return(df_characteristic)
}


calculate_months_stats <- function(number_selected_year){
  
  list_months_stats = list()
  for (m in 1:12) {
    
    # m = 11
    if (m < 12) {
      days_month = seq(from = as.Date(paste0(as.character(number_selected_year),"-",m,"-01")), to = as.Date(paste0(as.character(number_selected_year),"-",m+1,"-01")), by = "day")    
    }else{
      days_month = seq(from = as.Date(paste0(as.character(number_selected_year),"-",m,"-01")), to = as.Date(paste0(as.character(number_selected_year+1),"-",1,"-01")), by = "day")      
    }
    
    days_month = days_month[-length(days_month)]
    
    days_month_week = days_month[weekdays(days_month, abbreviate = T) %in% c("lun", "mar", "mié", "jue", "vie")]
    n_days_month_week = length(days_month_week)
    days_month_end_week = days_month[weekdays(days_month, abbreviate = T) %in% c("sáb", "dom")]
    n_days_month_end_week = length(days_month_end_week)
    ##
    
    df_week = data.frame("hour" = 0:23, "n_days" = n_days_month_week, "week" = T)
    df_end_week = data.frame("hour" = 0:23, "n_days" = n_days_month_end_week, "week" = F)

    list_months_stats[[m]] = rbind(df_week, df_end_week)
  }
  
  df_months_stats = dplyr::bind_rows(list_months_stats, .id = "month")
  
  return(df_months_stats)
}






calculate_characteristic_days_years <- function(df_gen_complete, years){

  df_characteristic = vector("list", length(years))
  names(df_characteristic) <- years
  df_characteristic[[names(df_characteristic)[1]]] = list()
  
  for (number_selected_year in as.character(years)) {
    
    # number_selected_year = as.character(years)[2]
    for (m in 1:12) {
      
      # m = 1
      if (m < 11) {
        days_month = seq(from = as.Date(paste0(number_selected_year,"-",m,"-01")), to = as.Date(paste0(number_selected_year,"-",m+1,"-01")), by = "day")    
      }else{
        days_month = seq(from = as.Date(paste0(number_selected_year,"-",m,"-01")), to = as.Date(paste0(as.character(as.numeric(number_selected_year)+1),"-",1,"-01")), by = "day")      
      }
      
      days_month = days_month[-length(days_month)]
      
      days_month_week = days_month[weekdays(days_month, abbreviate = T) %in% c("lun", "mar", "mié", "jue", "vie")]
      days_month_end_week = days_month[weekdays(days_month, abbreviate = T) %in% c("sáb", "dom")]
      
      ##
      
      df_mean_1 = data.frame("hour" = 0:23, "energy" = 0)
      
      df_month_week = df_gen_complete[as.Date(df_gen_complete$time) %in% days_month_week, ]
      df_month_week_clean = df_month_week[!is.na(df_month_week$energy), ]
      
      ##
      
      df_mean_2 = data.frame("hour" = 0:23, "energy" = 0)
      
      df_month_end_week = df_gen_complete[as.Date(df_gen_complete$time) %in% days_month_end_week, ]
      df_month_end_week_clean = df_month_end_week[!is.na(df_month_end_week$energy), ]
      
      
      if((any(!(0:23 %in% unique(hour(df_month_week_clean$time))))) | (any(!(0:23 %in% unique(hour(df_month_end_week_clean$time)))))){
        
        df_characteristic[[number_selected_year]][[m]] = NA
      }else{
        
        df_mean_incomplete = aggregate(df_month_week_clean$energy, by = list(hour(df_month_week_clean$time)), FUN = mean)
        colnames(df_mean_incomplete) = c("hour", "energy")
        
        df_mean_1[df_mean_1$hour %in% df_mean_incomplete$hour, "energy"] = df_mean_incomplete$energy
        df_mean_week = df_mean_1
        
        ##
        
        df_mean_incomplete = aggregate(df_month_end_week_clean$energy, by = list(hour(df_month_end_week_clean$time)), FUN = mean)
        colnames(df_mean_incomplete) = c("hour", "energy")
        
        df_mean_2[df_mean_2$hour %in% df_mean_incomplete$hour, "energy"] = df_mean_incomplete$energy
        df_mean_end_week = df_mean_2
        
        df_characteristic[[number_selected_year]][[m]] = rbind(df_mean_week, df_mean_end_week)
      }
    }
  }

  return(df_characteristic)
}


calculate_characteristic_days_years_2 <- function(df_gen_complete, years){
  
  df_characteristic = list()
  # number_selected_year = as.character(years)[2]
  
  for (m in 1:12) {
    # m = 3
    # TODO: change to <=
    if (m <= 11) {
      days_month = c()
      for (number_selected_year in years) {
        days_month_aux = as.character(seq(from = as.Date(paste0(number_selected_year,"-",m,"-01")), to = as.Date(paste0(number_selected_year,"-",m+1,"-01")), by = "day"))    
        days_month_aux = days_month_aux[-grep(pattern = paste0(m+1,"-01"), x = days_month_aux)]
        days_month = c(days_month, days_month_aux)
        # print(m)
        # print(number_selected_year)
        # print(days_month)
      }
    }else{
      days_month = c()
      for (number_selected_year in years) {
        days_month_aux = as.character(seq(from = as.Date(paste0(number_selected_year,"-",m,"-01")), to = as.Date(paste0(number_selected_year+1,"-01-01")), by = "day"))      
        days_month_aux = days_month_aux[-grep(pattern = "-01-01", x = days_month_aux)]
        days_month = c(days_month, days_month_aux)
        # print(m)
      }
    }

    days_month = as.POSIXct(days_month, tz = "CET")
    
    days_month_week = days_month[weekdays(days_month, abbreviate = T) %in% c("lun", "mar", "mié", "jue", "vie")]
    days_month_end_week = days_month[weekdays(days_month, abbreviate = T) %in% c("sáb", "dom")]
    
    ##
    
    df_mean_1 = data.frame("hour" = 0:23, "energy" = 0)
    
    df_month_week = df_gen_complete[as.Date(df_gen_complete$time) %in% as.Date(days_month_week), ]
    df_month_week_clean = df_month_week[!is.na(df_month_week$energy), ]
    
    ##
    
    df_mean_2 = data.frame("hour" = 0:23, "energy" = 0)
    
    df_month_end_week = df_gen_complete[as.Date(df_gen_complete$time) %in% as.Date(days_month_end_week), ]
    df_month_end_week_clean = df_month_end_week[!is.na(df_month_end_week$energy), ]

    if((any(!(0:23 %in% unique(hour(df_month_week_clean$time))))) | (any(!(0:23 %in% unique(hour(df_month_end_week_clean$time)))))){
      
      df_characteristic[[m]] = NA
    }else{
      
      df_mean_incomplete = aggregate(df_month_week_clean$energy, by = list(hour(df_month_week_clean$time)), FUN = mean)
      colnames(df_mean_incomplete) = c("hour", "energy")
      
      df_mean_1[df_mean_1$hour %in% df_mean_incomplete$hour, "energy"] = df_mean_incomplete$energy
      df_mean_week = df_mean_1
      
      ##
      
      df_mean_incomplete = aggregate(df_month_end_week_clean$energy, by = list(hour(df_month_end_week_clean$time)), FUN = mean)
      colnames(df_mean_incomplete) = c("hour", "energy")
      
      df_mean_2[df_mean_2$hour %in% df_mean_incomplete$hour, "energy"] = df_mean_incomplete$energy
      df_mean_end_week = df_mean_2
      
      df_characteristic[[m]] = rbind(df_mean_week, df_mean_end_week)
    }
  }

  return(df_characteristic)
}


define_boxes <- function(df_cons, df_gen, df_local_time, n_community){
  
  summer_months = c(7, 8, 9)
  winter_months = c(1, 2, 3)
  mid_season_months = c(4, 5, 6, 10, 11, 12)
  list_season = list("summer" = summer_months, "mid_season" = mid_season_months, "winter" = winter_months)
  list_n_season_days = list("summer" = c(rep(x = 1, times = n_community-6), 2), "mid_season" = c(1, 1, 2), "winter" = c(1, 2))
  
  df_cons_to_remove = df_cons
  list_box_season = list()
  
  
  # TODO IMPORTANT
  # instead of filling:
  # first ALL the summer, 
  # then ALL the mid_season, 
  # end ALL the winter
  
  # I will try filling:
  # the FIRST for the summer, 
  # then the FIRST for the mid_season, 
  # then the FIRST for the winter,
  # ...
  # then the SECOND for the summmer, ....
  
  # using this method Im not priorizong soo much the summer users

  
  
  i = 1
  for (season in names(list_season)) {
    # season = "summer"
    months = list_season[[season]]
    
    # for the generation the mean will consider ALL THE DAYS in the season
    df_gen_selected = as.data.frame(df_gen[df_local_time$month %in% months, "energy"])
    df_gen_selected = aggregate(x = df_gen_selected, by = list(rep(0:23, length(months)*2)), FUN = mean)
    df_gen_selected_complete = df_gen_selected[,2:ncol(df_gen_selected)]
    df_gen_selected = df_gen_selected_complete[df_gen_selected_complete != 0]
    df_gen_selected_norm = df_gen_selected/sum(df_gen_selected)
    
    n_season_days = list_n_season_days[[season]]
    
    for (day in n_season_days) {
      # day = 1
      
      df_cons_selected = df_cons_to_remove[df_local_time$month %in% months & df_local_time$date %in% day, ]
      df_cons_selected = aggregate(x = df_cons_selected, by = list(rep(0:23, length(months))), FUN = mean)
      df_cons_selected = df_cons_selected[,2:ncol(df_cons_selected)]
      df_cons_selected = df_cons_selected[df_gen_selected_complete != 0, ]
      df_cons_selected_norm = apply(X = df_cons_selected, MARGIN = 2, FUN = function(x){x/sum(x)})
      
      # df_gen_selected = as.matrix(df_gen_selected) %*% matrix(data = 1, nrow = 1, ncol = ncol(df_cons_selected))
      distance <- pracma::distmat(X = t(df_cons_selected_norm), Y = df_gen_selected_norm)
      
      # checking:
      # df_cons_selected_1 = df_cons_selected[, order(distance)[1]]
      # ggplot() + 
      #   geom_point(aes(x = 1:length(df_cons_selected_1), y = df_cons_selected_1)) +
      #   geom_point(aes(x = 1:length(df_gen_selected), y = df_gen_selected), color = "green")
      
      selected_users = order(distance)[1:8]
      mean_cons_order = order(colMeans(df_cons_selected[, selected_users]), decreasing = T)
      
      box_season_to_remove = selected_users[mean_cons_order]
      box_season_to_save = colnames(df_cons_to_remove)[box_season_to_remove]
      
      list_box_season[[paste(i, paste(season, day, sep = "_"), sep = ".")]] = box_season_to_save
      
      df_cons_to_remove = df_cons_to_remove[, -box_season_to_remove]
      i = i + 1
    }
  }
  
  return(list_box_season)
}


define_smart_order <- function(df_cons, df_gen, df_local_time, n_community){
  
  df_gen_sunny = df_gen[df_local_time$sunny, "energy"]
  df_cons_sunny = df_cons_sunny

  # df_gen_selected = as.data.frame(df_gen[df_local_time$month %in% months, "energy"])
  # df_gen_selected = aggregate(x = df_gen_selected, by = list(rep(0:23, length(months)*2)), FUN = mean)
  # df_gen_selected_complete = df_gen_selected[,2:ncol(df_gen_selected)]
  # df_gen_selected = df_gen_selected_complete[df_gen_selected_complete != 0]
  # df_gen_selected_norm = df_gen_selected/sum(df_gen_selected)
  # 
  # df_cons_selected = df_cons_to_remove[df_local_time$month %in% months & df_local_time$date %in% day, ]
  # df_cons_selected = aggregate(x = df_cons_selected, by = list(rep(0:23, length(months))), FUN = mean)
  # df_cons_selected = df_cons_selected[,2:ncol(df_cons_selected)]
  # df_cons_selected = df_cons_selected[df_gen_selected_complete != 0, ]
  # df_cons_selected_norm = apply(X = df_cons_selected, MARGIN = 2, FUN = function(x){x/sum(x)})
  
  # df_gen_selected = as.matrix(df_gen_selected) %*% matrix(data = 1, nrow = 1, ncol = ncol(df_cons_selected))
  # distance <- pracma::distmat(X = t(df_cons_selected_norm), Y = df_gen_selected_norm)
  
  distance <- pracma::distmat(X = t(df_cons_sunny), Y = df_gen_sunny)

  # TODO: plot the sun generation curve and all the rest of the curves with continuous color scale according to the distance  
  # checking:
  # df_cons_selected_1 = df_cons_selected[, order(distance)[1]]
  # ggplot() + 
  #   geom_point(aes(x = 1:length(df_cons_selected_1), y = df_cons_selected_1)) +
  #   geom_point(aes(x = 1:length(df_gen_selected), y = df_gen_selected), color = "green")
  
  ordering = order(distance)
  df_cons_sunny_ordered = df_cons_sunny[, ordering]
  # mean_cons_order = order(colMeans(df_cons_sunny), decreasing = T)

  return(df_cons_sunny_ordered)
}


############################# AUX - operative #############################



calculate_individual_investment <- function(combination, global_investment, individual_investment_max){
  
  individual_investment = individual_investment_max * ( global_investment/sum(individual_investment_max) )  
  return(individual_investment)  
}


calculate_payback_betas_whole_year_all <- function(df_characteristic_selected_allocated, individual_investment_selected){
  
  list_profit_monthly = list()
  
  # Im  doing it this way because the surplus to sell cant compensate during the whole year, have to be sold every month
  # in the fitness_MO this is done separateldy for each month
  for (month_selected in c(1:12)) {
    
    # month_selected = 1
    rows_month = df_characteristic_selected_allocated$month == month_selected
    profit_monthly = calculate_profit_monthly_all_participants(df_characteristic_selected_allocated_month = df_characteristic_selected_allocated[rows_month, ])      
    list_profit_monthly[[month_selected]] = profit_monthly 
  }
  
  df_profit_monthly <- data.frame(matrix(unlist(list_profit_monthly), nrow=length(list_profit_monthly), byrow=TRUE))
  
  profit_one_year = colSums(df_profit_monthly)
  
  payback_years = individual_investment_selected / profit_one_year 
  # payback_years[is.na(payback_years)] = 1000 
  
  return(payback_years)
}


calculate_profit_monthly_all_participants <- function(df_characteristic_selected_allocated_month){
  
  sale_price = df_characteristic_selected_allocated_month$sale_price
  purchase_price = df_characteristic_selected_allocated_month$purchase_price
  surplus = df_characteristic_selected_allocated_month[, grep("surplus", colnames(df_characteristic_selected_allocated))]
  df_cons_selected = df_characteristic_selected_allocated_month[, grep("cons", colnames(df_characteristic_selected_allocated))]  
  grid = df_characteristic_selected_allocated_month[, grep("grid", colnames(df_characteristic_selected_allocated))]
  n_days = df_characteristic_selected_allocated_month$n_days
  
  # cost old:
  df_cons_selected_monthly = n_days*df_cons_selected
  cost_old = colSums(purchase_price*df_cons_selected_monthly)
  
  # cost new:
  surplus_monthly =  n_days*surplus
  grid_monthly =  n_days*grid
  surplus_to_sell = ifelse(colSums(surplus_monthly) < colSums(grid_monthly), colSums(surplus_monthly), colSums(grid_monthly))
  
  # cost_new = colSums(purchase_price*grid) - sale_price * surplus_to_sell
  # TODO: if the sale price is not unique...?? 
  cost_new = colSums(purchase_price*grid_monthly) - unique(sale_price) * surplus_to_sell 
  
  profit_monthly = cost_old - cost_new

  return(profit_monthly)
}


calculate_payback_betas <- function(purchase_price_sunny, df_cons_selected_sunny, df_gen_sunny, individual_investment, matrix_coefficients){
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients)
  
  surplus_x <- df_gen_assigned - df_cons_selected_sunny
  surplus_x[surplus_x < 0] = 0

  sale_price = 0.0508
  
  cost_old = colSums(purchase_price_sunny*df_cons_selected_sunny)
  
  grid_x = df_cons_selected_sunny - df_gen_assigned
  grid_x[grid_x < 0] = 0
  
  surplus_x_to_sell = ifelse(colSums(surplus_x) < colSums(grid_x), colSums(surplus_x), colSums(grid_x))
  
  cost_sun = colSums(purchase_price_sunny*grid_x) - sale_price * surplus_x_to_sell
  
  profit_period = cost_old - cost_sun
  ##### TODO PAYBACK: this 360 should change
  # 360/24 = 15
  profit_one_year = profit_period * 15 

  payback_years = individual_investment / profit_one_year 
  
  # TODO: 
  payback_years[is.na(payback_years)] = 1000 
  
  return(payback_years)
}


calculate_payback_betas_daily <- function(purchase_price_sunny, df_cons_selected_day, df_gen_day, individual_investment, matrix_coefficients){
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_day, matrix_coefficients)
  
  surplus_x <- df_gen_assigned - df_cons_selected_day
  surplus_x[surplus_x < 0] = 0
  
  sale_price = 0.0508
  
  cost_old = colSums(purchase_price_sunny*df_cons_selected_day)
  
  grid_x = df_cons_selected_day - df_gen_assigned
  grid_x[grid_x < 0] = 0
  
  surplus_x_to_sell = ifelse(colSums(surplus_x) < colSums(grid_x), colSums(surplus_x), colSums(grid_x))
  

  # changed this:
  cost_sun = colSums(purchase_price_sunny*grid_x) - sale_price * surplus_x_to_sell
  # cost_sun = purchase_price*colSums(grid_x) - sale_price * surplus_x_to_sell
  
  profit_period = cost_old - cost_sun
  profit_one_year = profit_period * 360 
  
  payback_years = individual_investment / profit_one_year 
  
  # payback_years = sd(payback_years)
  # TODO: 
  payback_years[is.na(payback_years)] = 1000

  return(payback_years)
}


calculate_dim <- function(hourly, n_community, n_sunny_hours){
  if (hourly == T) {
    dim = n_community*n_sunny_hours
  } else{
    dim = n_community
  }
  return(dim)
}


selection_according_to_criteria <- function(optim, n_community, n_sunny_hours){
  
  # now, which argument should I choose to select one point in the pareto front or another?
  # this will be the hippiesm-capitalist variable
  
  # TODO: first try to understand how to define the 0.5 point (weight) in the pareto curve 
  
  # df_pareto_objectives = data.frame(optim$objectives[optim$paretoFrontRank == 1, ])  
  
  df_pareto_objectives = data.frame(optim$objectives)  
  colnames(df_pareto_objectives) = c("surplus", "payback")
  
  rank_1 = (optim$paretoFrontRank == 1)
  
  # TODO: work here!! DEFINE THE CRITERIA FUNCTION
  # df_pareto_objectives = df_pareto_objectives[rank_1, ]
  # ggplot(df_pareto_objectives) +
  #   geom_point(aes(x = surplus, y = payback))
  
  # TODO:
  # should compare the objectives (with the weights) for the different df_pareto_parameters (from each row)
  # and decide with with row to select as the optim_betas 
  # for example, if we selected the row = 1:
  
  # TODO: here should select one of all of the possibilities in rank_1
  
  # z_star = data.frame("surplus" = df_pareto_objectives$surplus[which.max(df_pareto_objectives$payback)],
  #                     "payback" = df_pareto_objectives$payback[which.max(df_pareto_objectives$surplus)])
  
  z_star = data.frame("surplus" = min(df_pareto_objectives$surplus), 
                      "payback" = min(df_pareto_objectives$payback))
  
  
  # TODO: take the mean of all the differences? starting from top to bottom, ending in the middle
  m = -((max(df_pareto_objectives$payback) - z_star$payback)/(max(df_pareto_objectives$surplus) - z_star$surplus))
  c = z_star$payback - m*z_star$surplus 
  
  lineal = function(x, m, c){
    y = m*x + c
    return(y)
  }
  
  x_lineal = c( (z_star$surplus - 0.1 * max(df_pareto_objectives$surplus)) : (max(df_pareto_objectives$surplus) + 0.1 * max(df_pareto_objectives$surplus)) )
  y_lineal = lineal(x = x_lineal, m, c)
  
  # x_lineal = z_star$surplus
  # y_lineal = z_star$payback
  
  df_pareto_objectives_rank_1 = df_pareto_objectives[rank_1, ]
  
  rank_1_criteria = calculate_criteria_selected_row(df_pareto_objectives_rank_1, z_star)
  
  
  df_pareto_betas = data.frame(optim$parameters)  
  df_pareto_betas_rank_1 = df_pareto_betas[rank_1, ]
  
  betas_with_criteria = as.numeric(df_pareto_betas_rank_1[rank_1_criteria, ])
  
  coefficients = matrix(data = betas_with_criteria, ncol = n_community, nrow = n_sunny_hours, byrow = T)
  coefficients = coefficients/rowSums(coefficients)
  
  objectives_with_criteria = df_pareto_objectives_rank_1[rank_1_criteria, ]
  plot_multi_objective_criteria_selection(df_pareto_objectives, z_star, x_lineal, y_lineal, objectives_with_criteria)
  
  return(coefficients)
}

normalization <-function(x){
  x_moved = x - min(x)
  if (max(x_moved) == 0) {
    x_norm = 0
  }else{
    x_norm = x_moved/max(x_moved)
  }
  
  return(x_norm)
}
selection_according_to_criteria_2 <- function(optim, n_community, n_sunny_hours, criteria, name_plot){
  
  df_pareto_objectives = data.frame(optim$objectives)  
  colnames(df_pareto_objectives) = c("surplus", "payback")
  
  rank_1 = (optim$paretoFrontRank == 1)
  
  # df_pareto_objectives = scale(df_pareto_objectives, center = T, scale = T)
  # df_pareto_objectives = as.data.frame(df_pareto_objectives)
  
  # z_norm <-function(x){
  #   (x - mean(x)) / sd(x)
  # }
  # apply(df_pareto_objectives, MARGIN = 2, FUN = z_norm)
  # this is the same as using the scale() above

  df_pareto_objectives_normalized = apply(df_pareto_objectives, MARGIN = 2, FUN = normalization)
  df_pareto_objectives_normalized = as.data.frame(df_pareto_objectives_normalized)
  
  df_pareto_objectives_z_norm = scale(df_pareto_objectives, center = T, scale = T)
  df_pareto_objectives_z_norm = as.data.frame(df_pareto_objectives_z_norm)

  # df_pareto_objectives = df_pareto_objectives[rank_1, ]
  # ggplot(df_pareto_objectives) +
  #   geom_point(aes(x = surplus, y = payback))

  # z_star = data.frame("surplus" = min(df_pareto_objectives$surplus), 
  #                     "payback" = min(df_pareto_objectives$payback))

  z_star_normalized = data.frame("surplus" = min(df_pareto_objectives_normalized$surplus), 
                                 "payback" = min(df_pareto_objectives_normalized$payback))

  # z_star_z_norm = data.frame("surplus" = min(df_pareto_objectives_z_norm$surplus), 
  #                            "payback" = min(df_pareto_objectives_z_norm$payback))

  df_pareto_objectives_normalized_rank_1 = df_pareto_objectives_normalized[rank_1, ]
  # df_pareto_objectives_z_norm_rank_1 = df_pareto_objectives_z_norm[rank_1, ]
  
  # criteria = 0 should never enter here
  if(criteria == 1) {
    rank_1_criteria = calculate_selected_row_criteria_1(df_pareto_objectives_rank_1, z_star)
  }else if(criteria == 2) {
    # TODO: fix something with scales between variables
    # rank_1_criteria[] = F
    # rank_1_criteria[32] = T
    rank_1_criteria_normalized = calculate_selected_row_criteria_2(df_pareto_objectives_normalized_rank_1, z_star)
    # rank_1_criteria_z_norm = calculate_selected_row_criteria_2(df_pareto_objectives_normalized_rank_1, z_star)
  }
  
  df_pareto_betas = data.frame(optim$parameters)  
  df_pareto_betas_rank_1 = df_pareto_betas[rank_1, ]
  
  betas_with_criteria = as.numeric(df_pareto_betas_rank_1[rank_1_criteria, ])
  
  coefficients = matrix(data = betas_with_criteria, ncol = n_community, nrow = n_sunny_hours, byrow = T)
  coefficients = coefficients/rowSums(coefficients)
  
  objectives_with_criteria = df_pareto_objectives_rank_1[rank_1_criteria, ]
  plot_multi_objective_criteria_selection(name = name_plot, df_pareto_objectives, z_star, objectives_with_criteria)
  
  objectives_with_criteria_pre_processing = df_pareto_objectives_rank_1_pre_processing[rank_1_criteria, ]
  plot_multi_objective_criteria_selection(name = paste0("pre_processing_",name_plot), df_pareto_objectives_pre_processing, z_star_pre_processing, objectives_with_criteria_pre_processing)
  
  
  return(coefficients)
}


choose_scenarios <- function(optim, n_community, n_sunny_hours, criteria, name_plot){
  
  df_pareto_objectives = data.frame(optim$objectives)  
  colnames(df_pareto_objectives) = c("surplus", "payback")
  rank_1 = (optim$paretoFrontRank == 1)
  
  df_pareto_objectives_normalized = apply(df_pareto_objectives, MARGIN = 2, FUN = normalization)
  df_pareto_objectives_normalized = as.data.frame(df_pareto_objectives_normalized)
  
  # df_pareto_objectives_z_norm = scale(df_pareto_objectives, center = T, scale = T)
  # df_pareto_objectives_z_norm = as.data.frame(df_pareto_objectives_z_norm)
  
  z_star = data.frame("surplus" = min(df_pareto_objectives$surplus),
                      "payback" = min(df_pareto_objectives$payback))
  
  z_star_normalized = data.frame("surplus" = min(df_pareto_objectives_normalized$surplus),
                                 "payback" = min(df_pareto_objectives_normalized$payback))
  
  # z_star_z_norm = data.frame("surplus" = min(df_pareto_objectives_z_norm$surplus), 
  #                            "payback" = min(df_pareto_objectives_z_norm$payback))
  
  df_pareto_objectives_rank_1 = df_pareto_objectives[rank_1, ]
  df_pareto_objectives_normalized_rank_1 = df_pareto_objectives_normalized[rank_1, ]
  # df_pareto_objectives_z_norm_rank_1 = df_pareto_objectives_z_norm[rank_1, ]
  
  # criteria = 0 should never enter here
  if(criteria == 1) {
    rank_1_criteria = calculate_selected_row_criteria_1(df_pareto_objectives_rank_1, z_star)
  }else if(criteria == 2) {
    # TODO: fix something with scales between variables
    # rank_1_criteria = calculate_selected_row_criteria_2(df_pareto_objectives_rank_1, z_star)
    rank_1_criteria_normalized = calculate_selected_row_criteria_2(df_pareto_objectives_normalized_rank_1, z_star_normalized)
    # rank_1_criteria_z_norm = calculate_selected_row_criteria_2(df_pareto_objectives_z_norm_rank_1, z_star_z_norm)
  }
  
  df_pareto_betas = data.frame(optim$parameters)  
  df_pareto_betas_rank_1 = df_pareto_betas[rank_1, ]
  
  betas_with_criteria = as.numeric(df_pareto_betas_rank_1[rank_1_criteria_normalized,])
  coefficients = matrix(data = betas_with_criteria, ncol = n_community, nrow = n_sunny_hours, byrow = T)
  coefficients = coefficients/rowSums(coefficients)
  
  # objectives_with_criteria = df_pareto_objectives_rank_1[rank_1_criteria, ]
  
  objectives_with_criteria = df_pareto_objectives_rank_1[rank_1_criteria_normalized, ]
  # objectives_with_criteria_normalized = df_pareto_objectives_normalized_rank_1[rank_1_criteria_normalized, ]
  # objectives_with_criteria_z_norm = df_pareto_objectives_rank_1[rank_1_criteria_z_norm, ]
  
  # df_scenarios = data.frame("surplus" = 0, "payback" = 0)
  # # df_scenarios[1, ] = objectives_with_criteria
  # df_scenarios[2, ] = objectives_with_criteria_normalized
  # df_scenarios[3, ] = objectives_with_criteria_z_norm
  
  plot_multi_objective_criteria_selection(name = name_plot, df_pareto_objectives, z_star, objectives_with_criteria)
  
  # objectives_with_criteria_pre_processing = df_pareto_objectives_rank_1_pre_processing[rank_1_criteria, ]
  # plot_multi_objective_criteria_selection(name = paste0("pre_processing_",name_plot), df_pareto_objectives_pre_processing, z_star_pre_processing, objectives_with_criteria_pre_processing)
  
  return(coefficients)
}


calculate_surplus_hourly_individual_betas <- function(matrix_coefficients, df_gen_day, df_cons_selected_day){
  
  df_gen_assigned <- calculate_gen_assigned_betas(df_gen_day, matrix_coefficients)
  individual_hourly_surplus <- df_gen_assigned - df_cons_selected_day
  individual_hourly_surplus[individual_hourly_surplus < 0] = 0
  
  return(individual_hourly_surplus)
}


############################# AUX - plot #############################


calculate_solar_consumption <- function(df_gen_assigned, df_cons_selected){
  surplus <- df_gen_assigned - df_cons_selected
  surplus[surplus < 0] = 0
  
  df_solar_consumption = df_gen_assigned - surplus  
  return(df_solar_consumption)
}


