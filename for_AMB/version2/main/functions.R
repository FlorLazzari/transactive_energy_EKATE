############################# read data #############################

import_data_generation <- function(filename_gen, selected_year_generation, time_format){
  df_gen <- read.csv(file = filename_gen, header = TRUE)
  colnames(df_gen) <- c("time", "energy")
  df_gen$time <- as.POSIXct(as.character(df_gen$time), format = time_format, tz = "Europe/Madrid") 
  df_gen$energy <- as.numeric(df_gen$energy)

  df_gen = df_gen[as.Date(df_gen$time) %in% as.Date(selected_year_generation), ]

  return(df_gen)
}


import_data_consumption <- function(filename_cons, selected_year_consumption, time_format){
  df_cons = read.csv(file = filename_cons, header = TRUE)
  colnames(df_cons)[1] = "time" 
  df_cons$time <- as.POSIXct(as.character(df_cons$time), format = time_format, tz = "Europe/Madrid") 
  df_cons = df_cons[as.Date(df_cons$time) %in% as.Date(selected_year_consumption), ]
  return(df_cons) 
}


import_data_price <- function(filename_price){
 
  df <- read.csv(file = filename_price, header = TRUE)
  colnames(df) <- c("price", "hour")
  return(df)
}


import_data_investment <- function(filename_investments){
  df <- read.csv(file = filename_investments, header = TRUE)
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

############################# operative #############################




calculate_self_suff_and_cons <- function(matrix_coefficients, df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time){
  
  df_local_time$time = 1:nrow(df_local_time)
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients)
  colnames(df_gen_assigned) = colnames(df_cons_selected_users)
  
  # calculate solar consumption and surplus
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned = df_gen_assigned, df_cons_selected = df_cons_selected_sunny)
  
  solar_surplus <- df_gen_assigned - df_cons_selected_sunny
  solar_surplus[solar_surplus < 0] = 0
  
  grid = df_cons_selected_sunny - df_gen_assigned
  grid[grid < 0] = 0
  
  ## add hour column
  df_solar_consumption$time = df_local_time$time[df_local_time$sunny]
  solar_surplus$time = df_local_time$time[df_local_time$sunny]
  grid$time = df_local_time$time[df_local_time$sunny]
  df_cons_selected_users$time = df_local_time$time
  
  grid = rbind(grid, df_cons_selected_users[!(df_cons_selected_users$time %in% grid$time),])
  
  df_aux = df_cons_selected_users[!(df_cons_selected_users$time %in% df_solar_consumption$time),] 
  df_aux[,-ncol(df_aux)] = 0
  
  df_solar_consumption = rbind(df_solar_consumption, df_aux)
  solar_surplus = rbind(solar_surplus, df_aux)
  
  solar_surplus = solar_surplus[order(solar_surplus$time), ]
  df_solar_consumption = df_solar_consumption[order(df_solar_consumption$time), ]
  grid = grid[order(grid$time), ]
  
  solar_surplus = as.numeric(rowSums(solar_surplus[, 1:(ncol(solar_surplus)-1)]))
  df_solar_consumption = as.numeric(rowSums(df_solar_consumption[, 1:(ncol(df_solar_consumption)-1)]))
  grid = as.numeric(rowSums(grid[, 1:(ncol(grid)-1)]))
  
  
  df_plot3 <- data.frame("month" = df_local_time$month,
                         "date" = df_local_time$date,
                         "hour" = df_local_time$hour,
                         "Solar_surplus" = solar_surplus,
                         "Solar_consumption" = df_solar_consumption,
                         "Grid_consumption" = grid
  )
  
  df_plot3_mean_agg = aggregate(x = df_plot3, by = list(df_plot3$hour), FUN = function(x){return(mean(x, na.rm = T))})
  df_plot3_mean_agg = df_plot3_mean_agg[, c(-1, -2, -3)]

  return(df_plot3_mean_agg)
}

calculate_self_sufficiency <- function(matrix_coefficients, df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time){
  
  df_mean = calculate_self_suff_and_cons(matrix_coefficients, df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time)
  self_sufficiency = sum(df_mean$Solar_consumption) / ( sum(df_mean$Solar_consumption) + sum(df_mean$Grid_consumption) ) 

  return(self_sufficiency)
}


calculate_self_consumption <- function(matrix_coefficients, df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time){
  
  df_mean = calculate_self_suff_and_cons(matrix_coefficients, df_gen_sunny, df_cons_selected_sunny, df_cons_selected_users, df_local_time)
  solar_self_consumption = sum(df_mean$Solar_consumption) / ( sum(df_mean$Solar_consumption) + sum(df_mean$Solar_surplus) ) 
  
  return(solar_self_consumption)
}




calculate_daily_avoided_emissions <- function(matrix_coefficients, df_gen_sunny, df_cons_selected_users, df_local_time){
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients)
  
  m = unique(month(df_local_time$time))
  
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned = df_gen_assigned, df_cons_selected = df_cons_selected_users)
  
  df_gen$hour = df_local_time$hour
  df_solar_consumption$hour = df_local_time$hour[df_local_time$sunny]
  
  df_gen_mean = aggregate(x = df_gen, by = list(df_gen$hour), FUN = mean)
  df_solar_consumption_mean = aggregate(x = df_solar_consumption, by = list(df_solar_consumption$hour), FUN = mean)
  df_solar_consumption_mean <- melt(data = df_solar_consumption_mean[, -grep(pattern = "Group.1", x = colnames(df_solar_consumption_mean))], variable.name = "series", id.vars = "hour")
  
  df_avoided_emissions = sum(df_solar_consumption_mean$value) * 0.357
  
  return(df_avoided_emissions)
}


calculate_yearly_avoided_emissions <- function(matrix_coefficients, df_gen_sunny, df_cons_selected_users, df_local_time){
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients)

  m = unique(month(df_local_time$time))
  
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned = df_gen_assigned, df_cons_selected = df_cons_selected_users)
  
  df_gen$hour = df_local_time$hour
  df_solar_consumption$hour = df_local_time$hour[df_local_time$sunny]
  
  df_gen_mean = aggregate(x = df_gen, by = list(df_gen$hour), FUN = mean)
  df_solar_consumption_mean = aggregate(x = df_solar_consumption, by = list(df_solar_consumption$hour), FUN = mean)
  df_solar_consumption_mean <- melt(data = df_solar_consumption_mean[, -grep(pattern = "Group.1", x = colnames(df_solar_consumption_mean))], variable.name = "series", id.vars = "hour")
  
  df_avoided_emissions = sum(df_solar_consumption_mean$value) * 0.357

  df_yearly_avoided_emissions = df_avoided_emissions*360
  
  return(df_yearly_avoided_emissions)
}


calculate_sunny_emissions <- function(matrix_coefficients, df_gen_sunny, df_cons_selected_sunny){
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients)
  
  df_grid = df_cons_selected_sunny - df_gen_assigned
  df_grid[df_grid < 0] = 0
  
  df_sunny_emissions = df_grid*0.357
  
  return(df_sunny_emissions)
}


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


normalize_genome <- function(genome, n_community, n_sunny_hours, dim, popSize){
  genome = as.numeric(t(genome))
  genome_matrix = matrix(data = genome, ncol = n_community, nrow = n_sunny_hours*popSize, byrow = T)
  genome_matrix = genome_matrix/rowSums(genome_matrix)
  # checking
  # rowSums(genome_matrix)
  genome = matrix(data = t(genome_matrix), ncol = dim, byrow = T)
  return(genome)
}


# IMPORTANT: 
# the nsga2R_flor_constraint gets worst results than the nsga2R_flor 

# changed all these variables:
# fn = purrr::partial(fitness_MO_constraint,
#                     df_gen_sunny = df_gen_sunny_one_day,
#                     df_cons_selected_sunny = df_cons_selected_sunny_one_day,
#                     purchase_price_sunny = purchase_price_sunny,
#                     individual_investment_selected = individual_investment_selected)


# dim = calculate_dim(hourly=T, n_community, n_sunny_hours)
# fn = purrr::partial(fitness_MO_constraint,
#                                          df_gen_sunny = df_gen_sunny_one_day,
#                                          df_cons_selected_sunny = df_cons_selected_sunny_one_day,
#                                          purchase_price_sunny = purchase_price_sunny,
#                                          individual_investment_selected = individual_investment_selected)
# varNo = dim
# objDim = 2
# generations = 100
# popSize = 50
# cprob = 0.8
# mprob = 0.2
# lowerBounds = rep(0, dim)
# upperBounds = rep(1, dim)

nsga2R_flor_constraint <- function (fn = fitness_MO_constraint, 
                                    varNo = 5, 
                                    objDim = 2, 
                                    lowerBounds = rep(0, varNo), 
                                    upperBounds = rep(10, varNo), 
                                    popSize = 20, 
                                    tourSize = 2, 
                                    generations = 10, 
                                    cprob = 0.7, 
                                    XoverDistIdx = 5, 
                                    mprob = 0.2,
                                    MuDistIdx = 10){
  
  
  cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
  cat("\n")
  # cat("initializing the population")
  # cat("\n")
  
  parent <- t(sapply(1:popSize, function(u) array(runif(length(lowerBounds),
                                                        lowerBounds, upperBounds))))
  
  # parent <- array(runif(length(lowerBounds)*popSize,lowerBounds, upperBounds))
  
  parent = normalize_genome(parent, n_community, n_sunny_hours, dim, popSize)
  # normalization here:
  # - I am probably altering the distribution of the random generation
  # - but the crossover will have more sense
  # (CONCLUSION: it is still a dirty solution, but it is better than the previous one.. no?) 
  
  
  # a = aggregate(x = parent[1, ], list(c(rep(0, 2), rep(1, 3))), sum)
  # apply(X = parent, MARGIN = 1, function(x){x/})
  
  # for the case of int:
  # TODO: this should include all the hours
  # parent <- t(sapply(1:popSize, function(u) 
  #   array(sample.int(n = unique(upperBounds), size = length(lowerBounds), replace = T))))
  
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
    
    # TODO: work from here onwards: should not appear floats, only integers
    childAfterX <- boundedSBXover(matingPool[, 1:varNo], 
                                  lowerBounds, upperBounds, cprob, XoverDistIdx)
    
    # cat("mutation operator")
    # cat("\n")
    childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, 
                                       upperBounds, mprob, MuDistIdx)
    
    
    # normalization here:
    childAfterM = normalize_genome(childAfterM, n_community, n_sunny_hours, dim, popSize)
    
    # checking:
    # rowSums(childAfterM)
    
    # cat("evaluate the objective fns of childAfterM")
    # cat("\n")
    childAfterM <- cbind(childAfterM, t(apply(childAfterM, 
                                              1, fn)))
    
    # cat("Rt = Pt + Qt")
    # cat("\n")
    parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
    
    # checking:
    # rowSums(parentNext[,1:varNo])
    
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


calculate_matrix_coefficient_4_one_year <- function (df_local_time, df_cons_selected_sunny, df_gen_sunny, df_purchase_price_one_day, n_community, individual_investment_selected){
  
  matrix_coefficients_4 = matrix(0, nrow = length(df_gen_sunny), ncol = n_community)
  
  n_sunny_hours_start = 1
  for (month_i in 1:12) {
    for (date_i in 1:2) {
      
      print(month_i)
      print(date_i)
      
      df_local_time_first_day = df_local_time[df_local_time$month %in% month_i & df_local_time$date %in% date_i, ] 
      n_sunny_hours = sum(df_local_time_first_day$sunny)
      
      df_cons_selected_sunny_one_day = df_cons_selected_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1), ]
      df_gen_sunny_one_day = df_gen_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1)]
      
      purchase_price_sunny_one_day = df_purchase_price_one_day[df_local_time_first_day$sunny,"price"]
      
      # optimize_hourly_betas_multi_objective_per_combination:
      dim = calculate_dim(hourly=T, n_community, n_sunny_hours)
      optim <- nsga2R_flor(fn = purrr::partial(fitness_MO,
                                               df_gen_sunny = df_gen_sunny_one_day,
                                               df_cons_selected_sunny = df_cons_selected_sunny_one_day,
                                               purchase_price_sunny = purchase_price_sunny_one_day,
                                               individual_investment_selected = individual_investment_selected),
                           varNo = dim,
                           objDim = 2,
                           # generations = 100,
                           generations = 100,
                           popSize = 200,
                           cprob = 0.8,
                           mprob = 0.2,
                           lowerBounds = rep(0, dim),
                           upperBounds = rep(1, dim))
      
      matrix_coefficients_month_date = choose_scenarios(optim, n_community, n_sunny_hours, criteria = 2, name_plot = paste0(as.character(month_i),"_",as.character(date_i)))
      matrix_coefficients_4[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1),] = matrix_coefficients_month_date
      
      n_sunny_hours_start = n_sunny_hours_start + n_sunny_hours 
    }
  }
  return(matrix_coefficients_4)
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


############################# fitness #############################

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


# TODO: change by purchase_price_sunny everywhere!
fitness_MO_original <- function(x, df_gen_sunny, df_cons_selected_sunny, purchase_price_sunny, individual_investment_selected){
  
  # checking:
  # fitness_MO(runif(dim, 0, 1),
  #            df_gen_day = df_gen_day,
  #            df_cons_selected_day = df_cons_selected_day,
  #            individual_investment_selected = individual_investment_selected)
  
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
  
  n_sunny_hours = nrow(df_cons_selected_sunny)
  n_community = ncol(df_cons_selected_sunny)
  
  coefficients_x = matrix(data = x, ncol = n_community, nrow = n_sunny_hours, byrow = T)
  coefficients_x = coefficients_x/rowSums(coefficients_x)
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients = coefficients_x)
  
  surplus_x <- df_gen_assigned - df_cons_selected_sunny
  surplus_x[surplus_x < 0] = 0

  # should errase this
  f1_surplus = sum(surplus_x)

  # working here:
  # weighted_surplus_x = weights_n_days * calculate_surplus_hourly_community(combination = combination, df_gen = df_gen_to_optimize, df_cons = df_cons_to_optimize)
  # f1_surplus = sum(weighted_surplus_x)

  
  # changed this:
  # purchase_price = 0.14859
  # purchase_price = c(0.14859, 0.14859, 0.14859, 
  #                    0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 
  #                    0.14859*1.2, 0.14859*1.2, 0.14859*1.2, 0.14859*1.2,
  #                    0.14859*1.5) 
  sale_price = 0.0508
  
  cost_old = colSums(purchase_price_sunny*df_cons_selected_sunny)
  
  grid_x = df_cons_selected_sunny - df_gen_assigned
  grid_x[grid_x < 0] = 0
  
  surplus_x_to_sell = ifelse(colSums(surplus_x) < colSums(grid_x), colSums(surplus_x), colSums(grid_x))
  
  # changed this: 
  # cost_sun = purchase_price*colSums(grid_x) - sale_price * surplus_x_to_sell
  cost_sun = colSums(purchase_price_sunny*grid_x) - sale_price * surplus_x_to_sell
  
  # assuming period is a DAY
  # profit_period = cost_old - cost_sun
  
  # TODO: is this changing something?
  profit_period = ifelse(cost_old - cost_sun > 0, cost_old - cost_sun, 0)
  
  profit_one_year = profit_period * 360 
  
  
  ##### TODO PAYBACK
  payback_years = individual_investment_selected / profit_one_year 
  
  # TODO:
  payback_years[is.na(payback_years)] = 10
  payback_years[payback_years > 50] = 10
  # payback_years[payback_years > 50] = 50
  
  payback_ideal = 0
  f2_payback = sum(exp(payback_years - payback_ideal))
  
  # f2_payback = sd(payback_years)
  # cost_payback_2 = max(payback_years) - min(payback_years) 
  
  # score <- weight_surplus * cost_surplus + (1-weight_surplus) * cost_payback
  
  return(c(f1_surplus, f2_payback))
}




fitness_MO_constraint <- function(x, df_gen_sunny, df_cons_selected_sunny, purchase_price_sunny, individual_investment_selected){
  
  # checking:
  # fitness_MO(runif(dim, 0, 1),
  #            df_gen_day = df_gen_day,
  #            df_cons_selected_day = df_cons_selected_day,
  #            individual_investment_selected = individual_investment_selected)
  
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
  
  n_sunny_hours = nrow(df_cons_selected_sunny)
  n_community = ncol(df_cons_selected_sunny)
  
  coefficients_x = matrix(data = x, ncol = n_community, nrow = n_sunny_hours, byrow = T)
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients = coefficients_x)
  
  # checking:
  # print(rowSums(coefficients_x))
  
  surplus_x <- df_gen_assigned - df_cons_selected_sunny
  surplus_x[surplus_x < 0] = 0
  
  # TODO:
  f1_surplus = sum(surplus_x)
  
  # changed this:
  # purchase_price = 0.14859
  # purchase_price = c(0.14859, 0.14859, 0.14859, 
  #                    0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 
  #                    0.14859*1.2, 0.14859*1.2, 0.14859*1.2, 0.14859*1.2,
  #                    0.14859*1.5) 
  sale_price = 0.0508
  
  cost_old = colSums(purchase_price_sunny*df_cons_selected_sunny)
  
  grid_x = df_cons_selected_sunny - df_gen_assigned
  grid_x[grid_x < 0] = 0
  
  surplus_x_to_sell = ifelse(colSums(surplus_x) < colSums(grid_x), colSums(surplus_x), colSums(grid_x))
  
  # changed this: 
  # cost_sun = purchase_price*colSums(grid_x) - sale_price * surplus_x_to_sell
  cost_sun = colSums(purchase_price_sunny*grid_x) - sale_price * surplus_x_to_sell
  
  # assuming period is a DAY
  profit_period = cost_old - cost_sun
  profit_one_year = profit_period * 360 
  
  
  ##### TODO PAYBACK
  payback_years = individual_investment_selected / profit_one_year 
  
  # TODO:
  payback_years[is.na(payback_years)] = 10
  payback_years[payback_years > 50] = 10
  
  payback_ideal = 0
  # TODO:
  f2_payback = sum(exp(payback_years - payback_ideal))
  
  # f2_payback = sd(payback_years)
  
  # TODO: add something like this
  # cost_payback_2 = max(payback_years) - min(payback_years) 
  
  # score <- weight_surplus * cost_surplus + (1-weight_surplus) * cost_payback
  
  
  # return(c(-f1_surplus, -f2_payback))
  # took the minus sign because the optimization algo is set to minimize:
  return(c(f1_surplus, f2_payback))
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



############################# plot ############################## 


plot_energy_time <- function(name = "test", print_plots, df){
  if (print_plots == F) {
    return()
  } else{
    df_plot <- df 
    df_plot[is.na(df_plot$energy), "energy"] = 0
    
    
    p <- ggplot() + 
      geom_line(aes(x = df_plot$time, y = df_plot$energy)) +
      labs(x = "Time [h]", y = "Electrical energy [kWh]", color = "")  
    ggsave(filename = paste0("graphs/plot_energy_",name) , plot = p, device = "pdf", width = 6, height = 3)
    return()  
  }
}


plot_matrix <- function(name, matrix_coefficients = matrix_coefficients_list[[3]], color_limits){
  
  rownames(matrix_coefficients) = NULL
  longData = melt(matrix_coefficients)
  longData = longData[longData$value!=0,]
  
  p <- ggplot(longData, aes(x = Var2, y = Var1)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_gradient(low="grey90", high="red", limits = color_limits) +
    labs(x="users", y="daytime", title="") +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  ggsave(filename = paste0("graphs/matrix_",name), plot = p, device = "pdf", width = 6, height = 3)
  return()
}


plot_REC_mean <- function(name, df_characteristic_selected_allocated, week_selected){
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
  
  ggsave(filename = paste0("graphs/REC_mean",name,".pdf"), plot = p_3, device = "pdf", width = 14, height = 5)

}


plot_REC_monthly_facets <- function(name, df_characteristic_selected_allocated, week_selected){
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
  # ggsave(filename = paste0("graphs/community",name,".pdf"), plot = p3, device = "pdf", width = 8, height = 10)

  return()
}


plot_disaggregated_three_participants <- function(name, df_characteristic_selected_allocated, week_selected, selected_3participants, individual_investment_selected, paybacks){
  # comes from plot_disaggregated_community_betas_year_area_mean_final (the second part)

  df_characteristic_selected_allocated_aux = df_characteristic_selected_allocated[, grep("month|week|hour", colnames(df_characteristic_selected_allocated))]
  
  df_characteristic_selected_allocated_aux = cbind(df_characteristic_selected_allocated_aux, df_characteristic_selected_allocated[, grep(paste0(selected_3participants, collapse = "|"), colnames(df_characteristic_selected_allocated))])
  
  individual_investment_selected_3participants = individual_investment_selected_3participants[selected_3participants] 
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
  
  df_plot_solar_consumption_mean <- melt(data = df_solar_consumption_mean[, -grep(pattern = "Group.1", x = colnames(df_solar_consumption_mean))], variable.name = "series", id.vars = "hour")
  df_plot_gen_assigned_mean <- melt(data = df_gen_assigned_mean[, -grep(pattern = "Group.1", x = colnames(df_gen_assigned_mean))], variable.name = "series", id.vars = "hour")
  
  df_plot_solar_assigned_cons_mean = cbind(df_plot_gen_assigned_mean, df_plot_solar_consumption_mean$value)
  
  colnames(df_plot_solar_assigned_cons_mean)[c(3,4)] = c("solar_assig", "solar_cons")

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
  
  # scale_factor = (max(df_plot_solar_assigned_cons_mean$solar_assig) - min(df_plot_solar_assigned_cons_mean$solar_assig))/ (max(df_plot_solar_assigned_cons_mean$price) - min(df_plot_solar_assigned_cons_mean$price))
  
  # changed to standarize the scale :)
  scale_factor = (0.50 - 0) / (0.299 - 0.2198)
  
  mean_price = mean(df_plot_solar_assigned_cons_mean$price)
  # mean_solar_assig = mean(df_plot_solar_assigned_cons_mean$solar_assig)
  mean_solar_assig = mean(c(0, 0.65))
  
  p2 <- ggplot(df_plot_solar_assigned_cons_mean) +
    # geom_line(aes(x = df_gen_mean$hour, y = df_gen_mean$energy)) +
    geom_area(aes(x = hour, y = solar_assig, fill = series), position = 'stack', alpha = 0.5) + #, linetype = 1, colour="black", show.legend = FALSE) +
    geom_area(aes(x = hour, y = solar_cons, fill = series), position = 'stack', alpha = 0.5) + #, linetype = 1, colour="black", show.legend = FALSE) +
    # geom_line(aes(x = hour, y = ((price - mean_price) * scale_factor + mean_solar_assig)), linetype = 2) + #, colour = "black")  +
    facet_grid(df_plot_solar_assigned_cons_mean$series ~ .) + 
    # TODO: should change the scale_factor here:
    # scale_y_continuous("Solar Generation [kWh]", 
    #                    limits = c(0, 0.7), 
    #                    sec.axis = sec_axis(~ (. - mean_solar_assig) / scale_factor + mean_price, name = "Price [$/kWh]")) +
    scale_x_continuous(limits = c(8, 19)) +
    theme(strip.background = element_blank(), strip.text = element_blank()) + 
    labs(x = "Time [h]", fill = "Ind Payback") +
    scale_fill_brewer(palette="Dark2")

  # scale_y_continuous("Solar Generation [kWh]", sec.axis = sec_axis(~./scale_factor, name = "Price [$/kWh]")) +
  
    
  library(ggpubr)
  p_1_2 <- ggarrange(p1, p2, widths = c(3, 1, 2.5), ncol = 3)
  ggsave(filename = paste0("graphs/final",name,".pdf"), plot = p_1_2_3, device = "pdf", width = 14, height = 5)
  

  return()
}
  

plot_simple_users <- function(){
  
  week_days = df_local_time$date == 2
  
  df_local_time_sunny = df_local_time[df_local_time$sunny == T, ]
  week_days_of_sunny = which(df_local_time_sunny$date ==2)
  
  df_cons_to_plot =  df_cons_sunny_test[week_days_of_sunny, combinations==1]
  
  df_local_time_week = df_local_time[week_days, ]
  df_local_time_week$time = 1:nrow(df_local_time_week)
  
  df_cons_to_plot$hour =  df_local_time_week$time[df_local_time_week$sunny == T]
  df_cons_to_plot = melt(data = df_cons_to_plot, id.vars = "hour")
  
  # TODO: should not complete the lines in between
  ggplot(df_cons_to_plot) +  
    geom_point(aes(x = hour, y = value)) + facet_grid(variable ~ .)
  
}


plot_comparison_stats_cut <- function(name, list_surplus, list_avoided_emissions, list_self_sufficiency, list_self_consumption, list_max_payback, list_mean_payback, list_diff_max_min_payback){
  
  selections = c("(1) Random selection", "(2) Random large selection", "(3) Optimization 1 selection")
  assignments = c("(a) Investment", "(b) Solar excess", "(c) Optimization 2")
  
  selections_bis = c("Profitable", "Sustainable", "Novel")
  #  will select only the ones Im interested in showing:
  
  selected_list_surplus = c(list_surplus$random_n_comm$investment, list_surplus$random_big$solar_excess, list_surplus$optimal$solar_excess_and_payback)
  df_plot_surplus_selected = data.frame("variable" = factor(selections_bis, levels = c("Profitable", "Sustainable", "Novel")), 
                                        "value" = selected_list_surplus, "stat" = factor("Solar excess [kWh]"))
  
  selected_avoided_emissions = c(list_avoided_emissions$random_n_comm$investment, list_avoided_emissions$random_big$solar_excess, list_avoided_emissions$optimal$solar_excess_and_payback)
  df_plot_avoided_emissions_selected = data.frame("variable" = factor(selections_bis, levels = c("Profitable", "Sustainable", "Novel")),  
                                                  "value" = selected_avoided_emissions, "stat" = factor("Avoided CO2 [kg]"))
  
  selected_self_sufficiency = c(list_self_sufficiency$random_n_comm$investment, list_self_sufficiency$random_big$solar_excess, list_self_sufficiency$optimal$solar_excess_and_payback)
  selected_self_sufficiency = selected_self_sufficiency*100
  
  df_plot_self_sufficiency_selected = data.frame("variable" = factor(selections_bis, levels = c("Profitable", "Sustainable", "Novel")),  
                                                 "value" = selected_self_sufficiency, "stat" = factor("Self-sufficiency [%]"))
  
  selected_self_consumption = c(list_self_consumption$random_n_comm$investment, list_self_consumption$random_big$solar_excess, list_self_consumption$optimal$solar_excess_and_payback)
  selected_self_consumption = selected_self_consumption*100
  
  df_plot_self_consumption_selected = data.frame("variable" = factor(selections_bis, levels = c("Profitable", "Sustainable", "Novel")), 
                                                 "value" = selected_self_consumption, "stat" = factor("Self-consumption [%]"))
  
  df_plot = rbind(df_plot_surplus_selected, df_plot_self_consumption_selected, df_plot_avoided_emissions_selected, df_plot_self_sufficiency_selected)

  p1 <- ggplot(df_plot, aes(x = variable, y = value, fill = variable)) +
    geom_bar(alpha = 0.5, stat = "identity", width = 0.5) + 
    facet_grid(stat ~ ., scales = "free", switch = "y") + 
    # labs(x = TeX("$\\beta$ assignment"), y = "Global Solar Excess [kWh]", fill = "") + 
    labs(x = "Environmental", y = "", fill = "EC") +
    # geom_text(aes(label = round(value, digits = 1)), position=position_dodge(width=0.9), vjust=0.5) + 
    scale_y_continuous(position = "right") +
    # theme(text = element_text(size=17), legend.position= "none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
    theme(text = element_text(size=17), legend.position= "none", axis.text.x=element_blank(), axis.ticks.x=element_blank())
  ggsave(filename = paste0("graphs/general_comparison_cut_1",name,".pdf"), plot = p1, device = "pdf", width = 3, height = 8.5)
  
  
  
  
  selected_mean_payback = c(list_mean_payback$random_n_comm$investment, list_mean_payback$random_big$solar_excess, list_mean_payback$optimal$solar_excess_and_payback)
  df_plot_mean_payback_selected = data.frame("variable" = factor(selections_bis, levels = c("Profitable", "Sustainable", "Novel")), 
                                             "value" = selected_mean_payback, "stat" = factor("Mean payback [years]"))
  
  selected_max_payback = c(list_max_payback$random_n_comm$investment, list_max_payback$random_big$solar_excess, list_max_payback$optimal$solar_excess_and_payback)
  df_plot_max_payback_selected = data.frame("variable" = factor(selections_bis, levels = c("Profitable", "Sustainable", "Novel")), 
                                            "value" = selected_max_payback, "stat" = factor("Max payback [years]"))

  selected_diff_max_min_payback = c(list_diff_max_min_payback$random_n_comm$investment, list_diff_max_min_payback$random_big$solar_excess, list_diff_max_min_payback$optimal$solar_excess_and_payback)
  df_plot_diff_max_min_payback_selected = data.frame("variable" = factor(selections_bis, levels = c("Profitable", "Sustainable", "Novel")),  
                                                     "value" = selected_diff_max_min_payback, "stat" = factor("Delta payback [years]"))
  
  df_plot = rbind(df_plot_mean_payback_selected, df_plot_max_payback_selected, df_plot_diff_max_min_payback_selected)

  
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
  ggsave(filename = paste0("graphs/general_comparison_cut_2",name,".pdf"), plot = p1, device = "pdf", width = 4.8, height = 8.5)
  
  p_1_2 <- ggarrange(p1, p2, widths = c(1.4, 2), ncol = 2)
  ggsave(filename = paste0("graphs/general_comparison_cut_new",name,".pdf"), plot = p_1_2, device = "pdf", width = 9, height = 8.5)
  
  
  return()
}


plot_energy_time_week <- function(name, print_plots, binary_week = T, df_cons_characteristic, cons_to_plot){
  
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
    ggsave(filename = paste0("graphs/energy_time_",name,".pdf"), plot = p3, device = "pdf", width = 8, height = 10)
    return()  
  } else{
    return()  
  }
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
    
    # m = 1
    if (m < 11) {
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


optimize_hourly_betas_multi_objective_per_combination <- function(hourly, combination_selected, df_gen_sunny, df_cons_selected_sunny, individual_investment_max){
  
  # x just selects the users, no need to calculate the optimum combination here
  # optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
  
  n_community = sum(combination_selected)
  
  # this should not be calculated here, should be introduced outside
  # individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)
  
  n_sunny_hours = nrow(df_cons_selected_sunny)      
  dim = calculate_dim(hourly, n_community, n_sunny_hours)

  # TODO: how does the algorithm knows that the GA's type is = "real-valued"??
  
  optim <- nsga2R_flor(fn = purrr::partial(fitness_MO, 
                                                    df_gen_sunny = df_gen_sunny,
                                                    df_cons_selected_sunny = df_cons_selected_sunny,
                                                    individual_investment_selected = individual_investment_selected),
                                varNo = dim, 
                                objDim = 2, 
                                generations = 100,
                                popSize = 200,
                                cprob = 0.8,
                                mprob = 0.2, 
                                lowerBounds = rep(0, dim), 
                                upperBounds = rep(1, dim))
  
  coefficients_criteria = selection_according_to_criteria(optim, n_community, n_sunny_hours)
  return(coefficients_criteria)
}


calculate_combinatorics = function(n, m){
  comb = factorial(n)/(factorial(n-m)*factorial(m))  
  return(comb)
}


# calculate_matrix_coefficients <- function(method, df_gen, df_cons_selected){
#   df_gen_assigned <- calculate_gen_assigned(df_gen, combination = rep(1, length(df_cons_selected)))
#   
#   optimum_coefficients = df_cons_selected/df_gen_assigned
#   optimum_coefficients = optimum_coefficients/rowSums(optimum_coefficients, na.rm = T)
#   
#   matrix_coefficients = as.matrix(optimum_coefficients)
#   
#   return(matrix_coefficients)
# }

calculate_matrix_coefficients <- function(optimizer_number, df_gen_sunny, df_cons_selected_sunny, n_community, individual_investment_selected = 0, df_local_time = 0, df_purchase_price_one_day = 0){
  
  matrix_coefficients = matrix(0, nrow = length(df_gen_sunny), ncol = n_community)
  
  if (optimizer_number == 1){
    matrix_coefficients[,] = 1/n_community
    
  } else if (optimizer_number == 2){
    # needs individual_investment_selected
    ratio_investment = as.numeric(individual_investment_selected/sum(individual_investment_selected))
    matrix_coefficients = matrix(1, nrow = length(df_gen_sunny)) %*% matrix(ratio_investment, ncol = n_community)
    
  } else if (optimizer_number == 3){
    df_gen_assigned_sunny <- calculate_gen_assigned(df_gen_sunny, combination = rep(1, n_community))
    optimum_coefficients = df_cons_selected_sunny/df_gen_assigned_sunny
    optimum_coefficients = optimum_coefficients/rowSums(optimum_coefficients, na.rm = T)
    matrix_coefficients = as.matrix(optimum_coefficients)
  
  } else if (optimizer_number == 4){
    matrix_coefficients = calculate_matrix_coefficient_4(df_local_time = df_local_time, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                   df_purchase_price_one_day = df_purchase_price_one_day, n_community = n_community, 
                                   individual_investment_selected = individual_investment_selected)
  
  }
  
  return(matrix_coefficients)
}


# using only "sunny" data frames (saving computational time but is messy)
calculate_allocation_coefficients_original <- function(community_objective = "novel", gen_sunny, cons_sunny, n_community, individual_investment, local_time, purchase_price_one_day){
  # copy from calculate_matrix_coefficients
  
  matrix_coefficients = matrix(0, nrow = length(gen_sunny), ncol = n_community)
  
  if (community_objective == "none"){
    matrix_coefficients[,] = 1/n_community
    
  } else if (community_objective == "profitable"){
    # needs individual_investment_selected
    ratio_investment = as.numeric(individual_investment/sum(individual_investment))
    matrix_coefficients = matrix(1, nrow = length(gen_sunny)) %*% matrix(ratio_investment, ncol = n_community)
    
  } else if (community_objective == "environmental"){
    df_gen_assigned_sunny <- calculate_gen_assigned(gen_sunny, combination = rep(1, n_community))
    optimum_coefficients = cons_sunny/df_gen_assigned_sunny
    optimum_coefficients = optimum_coefficients/rowSums(optimum_coefficients, na.rm = T)
    matrix_coefficients = as.matrix(optimum_coefficients)

  } else if (community_objective == "constant_environmental"){
    df_gen_assigned_sunny <- calculate_gen_assigned(gen_sunny, combination = rep(1, n_community))
    optimum_coefficients = cons_sunny/df_gen_assigned_sunny
    optimum_coefficients = optimum_coefficients/rowSums(optimum_coefficients, na.rm = T)
    optimum_coefficients_mean = colMeans(optimum_coefficients, na.rm = T)
    matrix_coefficients = as.matrix(optimum_coefficients_mean)
    matrix_coefficients = matrix(1, nrow = nrow(df_gen_assigned_sunny)) %*% t(matrix_coefficients)
    
  } else if (community_objective == "novel"){
    matrix_coefficients = calculate_matrix_coefficient_4(df_local_time = local_time, df_cons_selected_sunny = cons_sunny, df_gen_sunny = gen_sunny, 
                                                         df_purchase_price_one_day = purchase_price_one_day, n_community = n_community, 
                                                         individual_investment_selected = individual_investment)
  }
  
  return(matrix_coefficients)
}


calculate_allocation_coefficients <- function(community_objective = "novel", individual_investment, df_characteristic_selected){
  
  n_community = sum(grepl("cons", colnames(df_characteristic_selected)))
  gen_sunny = df_characteristic_selected[(df_characteristic_selected$sunny == T), "energy"]
  cons_sunny = df_characteristic_selected[df_characteristic_selected$sunny == T, grep("cons", colnames(df_characteristic_selected))]
  matrix_coefficients = matrix(0, nrow = length(gen_sunny), ncol = n_community)

  if (community_objective == "none"){
    matrix_coefficients[,] = 1/n_community
    
  } else if (community_objective == "profitable"){
    # needs individual_investment_selected
    ratio_investment = as.numeric(individual_investment/sum(individual_investment))
    matrix_coefficients = matrix(1, nrow = length(gen_sunny)) %*% matrix(ratio_investment, ncol = n_community)
    
  } else if (community_objective == "environmental"){
    df_gen_assigned_sunny <- calculate_gen_assigned(gen_sunny, combination = rep(1, n_community))
    optimum_coefficients = cons_sunny/df_gen_assigned_sunny
    optimum_coefficients = optimum_coefficients/rowSums(optimum_coefficients, na.rm = T)
    matrix_coefficients = as.matrix(optimum_coefficients)
    
  } else if (community_objective == "constant_environmental"){
    df_gen_assigned_sunny <- calculate_gen_assigned(gen_sunny, combination = rep(1, n_community))
    optimum_coefficients = cons_sunny/df_gen_assigned_sunny
    optimum_coefficients = optimum_coefficients/rowSums(optimum_coefficients, na.rm = T)
    optimum_coefficients_mean = colMeans(optimum_coefficients, na.rm = T)
    matrix_coefficients = as.matrix(optimum_coefficients_mean)
    matrix_coefficients = matrix(1, nrow = nrow(df_gen_assigned_sunny)) %*% t(matrix_coefficients)
    
  } else if (community_objective == "novel"){
    # matrix_coefficients = calculate_matrix_coefficient_4(df_local_time = local_time, df_cons_selected_sunny = cons_sunny, df_gen_sunny = gen_sunny, 
    #                                                      df_purchase_price_one_day = purchase_price_one_day, n_community = n_community, 
    #                                                      individual_investment_selected = individual_investment)
  
    matrix_coefficients = calculate_matrix_coefficient_novel(df_characteristic_selected, n_community, individual_investment_selected)
    
  }
  
  return(matrix_coefficients)
}


calculate_matrix_coefficient_novel <- function(df_characteristic_selected, n_community, individual_investment_selected){
  # copy from calculate_matrix_coefficient_4
  
  matrix_coefficients = matrix(0, ncol = length(individual_investment_selected), nrow = nrow(df_characteristic_selected))
  # n_sunny_hours_start = 1
  
  for (month in 1:12) {
    for (week in c(F, T)) {
      
      print(month)
      print(week)
      
      cons_sunny_one_day = df_characteristic_selected[df_characteristic_selected$month %in% month & df_characteristic_selected$week %in% week & df_characteristic_selected$sunny == T, grep("cons", colnames(df_characteristic_selected))]
      gen_sunny_one_day = df_characteristic_selected$energy[df_characteristic_selected$month %in% month & df_characteristic_selected$week %in% week & df_characteristic_selected$sunny == T]
      purchase_price_sunny_one_day = df_characteristic_selected$price[df_characteristic_selected$month %in% month & df_characteristic_selected$week %in% week & df_characteristic_selected$sunny == T]
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
        
        matrix_coefficients_month_week = choose_scenarios_normalized(optim, plot = T, n_community, n_sunny_hours, criteria = 2, name_plot = paste0(as.character(month),"_",as.character(week)))
      }

      matrix_coefficients[df_characteristic_selected$month %in% month & df_characteristic_selected$week %in% week & df_characteristic_selected$sunny,] = matrix_coefficients_month_week
      
      # n_sunny_hours_start = n_sunny_hours_start + n_sunny_hours 
    }
  }
  
  return(matrix_coefficients)
}


calculate_matrix_coefficient_4 <- function(df_local_time, df_cons_selected_sunny, df_gen_sunny, df_purchase_price_one_day, n_community, individual_investment_selected){
  
  matrix_coefficients_4 = matrix(ncol = ncol(df_cons_selected_sunny), nrow = nrow(df_cons_selected_sunny))
  n_sunny_hours_start = 1

  for (month in 1:12) {
    for (week in c(F, T)) {
      
      # month = 5
      # week = F
      
      print(month)
      print(week)
      
      df_local_time_first_day = df_local_time[df_local_time$month %in% month & df_local_time$week %in% week, ] 
      n_sunny_hours = sum(df_local_time_first_day$sunny)

      df_cons_selected_sunny_one_day = df_cons_selected_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1), ]
      df_gen_sunny_one_day = df_gen_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1)]
      
      purchase_price_sunny_one_day = df_purchase_price_one_day[df_local_time_first_day$sunny,"price"]
      
      dim = calculate_dim(hourly=T, n_community, n_sunny_hours)
      optim <- nsga2R_flor(fn = purrr::partial(fitness_MO_original,
                                               df_gen_sunny = df_gen_sunny_one_day,
                                               df_cons_selected_sunny = df_cons_selected_sunny_one_day,
                                               # purchase_price_sunny = purchase_price_sunny,
                                               purchase_price_sunny = purchase_price_sunny_one_day,
                                               individual_investment_selected = individual_investment_selected),
                           varNo = dim,
                           objDim = 2,
                           generations = 500,
                           popSize = 200,
                           cprob = 0.2,
                           mprob = 0.2,
                           lowerBounds = rep(0, dim),
                           upperBounds = rep(1, dim))
      
      matrix_coefficients_month_date = choose_scenarios_normalized(optim, plot = T, n_community, n_sunny_hours, criteria = 2, name_plot = paste0(as.character(month),"_",as.character(week)))
      matrix_coefficients_4[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1),] = matrix_coefficients_month_date
      
      n_sunny_hours_start = n_sunny_hours_start + n_sunny_hours 
    }
  }
  
  return(matrix_coefficients_4)
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


calulate_payback_surplus_for_matrix <- function(matrix_coefficients, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected){
  
  payback = calculate_payback_betas(purchase_price_sunny, df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, matrix_coefficients)
  payback = sum(exp(payback - 0))
  
  surplus = sum(calculate_surplus_hourly_individual_betas(matrix_coefficients, df_gen_sunny, df_cons_selected_sunny))
  return(data.frame("payback" = payback, "surplus" = surplus))
}


calulate_payback_surplus_for_matrix_by_user <- function(matrix_coefficients, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected){
  
  payback = calculate_payback_betas(purchase_price_sunny, df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, matrix_coefficients)
  surplus = colSums(calculate_surplus_hourly_individual_betas(matrix_coefficients, df_gen_sunny, df_cons_selected_sunny))
  
  return(list("payback" = payback, "surplus" = surplus))
}

