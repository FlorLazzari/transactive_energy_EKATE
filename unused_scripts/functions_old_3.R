# date: 17/8/2021

############################# data reading #############################

import_data_genome_project_public <- function(){

  filename = "~/Documents/projects/EKATE/building-data-genome-project-2/data/meters/cleaned/electricity_cleaned.csv"

  meter = read.csv(file = filename, header = TRUE)
  meter_public = meter[, c(1, grep(pattern = "public", x = colnames(meter)))]

  colnames(meter_public)[1] = "time"

  # selecting users:
  hourly_mean = colMeans(meter_public[2:ncol(meter_public)], na.rm = T)

  meter_public = meter_public[, c(1,order(hourly_mean)+1)]
  meter_public = meter_public[, c(1:as.numeric(which(colMeans(meter_public[2:ncol(meter_public)], na.rm = T) > 25)[1]))]

  # plot(meter_public[, 2])
  meter_public$time = as.POSIXct(meter_public$time)

  return(meter_public)
}


import_data_genome_project_public <- function(selected_year_consumption){

  filename = "~/Documents/projects/EKATE/building-data-genome-project-2/data/meters/cleaned/electricity_cleaned.csv"

  meter = read.csv(file = filename, header = TRUE)
  meter_public = meter[, c(1, grep(pattern = "public", x = colnames(meter)))]

  colnames(meter_public)[1] = "time"

  # selecting users:
  hourly_mean = colMeans(meter_public[2:ncol(meter_public)], na.rm = T)

  meter_public = meter_public[, c(1,order(hourly_mean)+1)]
  meter_public = meter_public[, c(1:as.numeric(which(colMeans(meter_public[2:ncol(meter_public)], na.rm = T) > 25)[1]))]

  # plot(meter_public[, 2])
  meter_public$time = as.POSIXct(meter_public$time)

  meter_public = meter_public[as.Date(meter_public$time) %in% as.Date(selected_year_consumption), ]

  # filter columns with all nas:
  meter_public = meter_public[colSums(is.na(meter_public)) != nrow(meter_public)]

  # filter columns with some na: (this filters too much)
  # meter_public = meter_public[colSums(is.na(meter_public)) == 0]

  return(meter_public)
}


import_data_genome_project_office <- function(selected_year_consumption){

  filename = "~/Documents/projects/EKATE/building-data-genome-project-2/data/meters/cleaned/electricity_cleaned.csv"

  meter = read.csv(file = filename, header = TRUE)
  meter_office = meter[, c(1, grep(pattern = "office", x = colnames(meter)))]

  colnames(meter_office)[1] = "time"

  # selecting users:
  hourly_mean = colMeans(meter_office[2:ncol(meter_office)], na.rm = T)

  meter_office = meter_office[, c(1,order(hourly_mean)+1)]
  meter_office = meter_office[, c(1:as.numeric(which(colMeans(meter_office[2:ncol(meter_office)], na.rm = T) > 25)[1]))]

  meter_office$time = as.POSIXct(meter_office$time)

  # plot(meter_public[, 2])

  meter_office = meter_office[as.Date(meter_office$time) %in% as.Date(selected_year_consumption), ]

  # filter columns with nas:
  meter_office = meter_office[colSums(is.na(meter_office)) != nrow(meter_office)]

  return(meter_office)
}


############################# operative #############################


optimize_hourly_betas_multi_objective <- function(hourly, weight_surplus, n_community_max, n_binary_rep, df_gen_sunny, df_cons_sunny, global_investment, individual_investment){
  # TODO: understand the criteria of the keepBest = T
  # keepBesta logical argument specifying if best solutions at each iteration should be savedin a slot calledbestSol. Seega-class.

  # tic = Sys.time()
  pre_optimal_combinations <- optimization_1(hourly, n_community = n_community_max, n_binary_rep = n_binary_rep, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
  # toc = Sys.time()
  # print(toc-tic)

  # not all of the combinations are of the size = n_community_max (some are smaller)

  # TODO: separate the combinations according to the number n_community_per_combination
  n_community_per_combination_order = order(rowSums(pre_optimal_combinations))

  # checking:
  # n_community_per_combination[n_community_per_combination_order]

  pre_optimal_combinations = pre_optimal_combinations[n_community_per_combination_order, ]
  n_community_vector = rowSums(pre_optimal_combinations)

  hourly_surplus = apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
  pre_surplus = colSums(hourly_surplus)

  # checking:
  # colSums(pre_optimal_combinations)
  # nrow(pre_optimal_combinations)
  # hist(as.numeric(lapply(X = surplus, FUN = sum)))
  # sum(df_gen)

  # new_optimum_coefficients = pre_optimum_coefficients
  new_surplus = c()
  new_optimum_coefficients = list()

  new_payback = df_cons_sunny[0,1:ncol(df_cons_sunny)]

  # pre_payback = pre_optimum_coefficients

  j = 1
  vector_i = c()

  tic = Sys.time()
  for (i in 1:nrow(pre_optimal_combinations)) {
  # for (i in 1:1) {

    combination_selected = pre_optimal_combinations[i, ]
    df_cons_selected_sunny = df_cons_sunny[,combination_selected==1]
    individual_investment_max = individual_investment[combination_selected==1]

    # why is this "if" here and not in the end of the pre_optimization? guess it would be the same.. can try moving it
    if (sum(individual_investment_max) > global_investment) {

      coefficients_criteria = optimize_hourly_betas_multi_objective_per_combination(hourly, combination_selected, df_gen_sunny, df_cons_selected_sunny, individual_investment_max)

      combination_optimum = matrix(1, nrow = nrow(coefficients_criteria)) %*% combination_selected
      combination_optimum[combination_optimum!=0] = coefficients_criteria

      new_optimum_coefficients[[j]] = coefficients_criteria
      new_payback[j, combination_selected!=0] = calculate_payback_betas_daily(df_cons_selected_sunny, df_gen_sunny, individual_investment_max, matrix_coefficients = coefficients_criteria)
      surplus = sum(calculate_surplus_hourly_individual_betas(coefficients_criteria, df_gen_sunny, df_cons_selected_sunny))
      new_surplus <- c(new_surplus, surplus)

      vector_i = c(vector_i, i)
      j = j + 1
    }
  }
  toc = Sys.time()
  print(toc-tic)

  results = list(
    # "pre_optimum_coefficients" = pre_optimum_coefficients,
    "pre_surplus" = pre_surplus,
    # "pre_payback" = pre_payback,
    "new_optimum_coefficients" = new_optimum_coefficients,
    "new_surplus" = new_surplus,
    "new_payback" = new_payback,
    "vector_i" = vector_i)

  return(results)
}

optimize_hourly_betas <- function(hourly, weight_surplus, n_community_max, n_binary_rep, df_gen, df_cons, global_investment, individual_investment){
  
  # TODO: understand the criteria of the keepBest = T
  # keepBesta logical argument specifying if best solutions at each iteration should be savedin a slot calledbestSol. Seega-class.
  
  # TODO: trying the hourly
  pre_optimal_combinations <- optimization_1(hourly, n_community = n_community_max, n_binary_rep = n_binary_rep, df_gen = df_gen, df_cons = df_cons)
  
  # not all of the combinations are of the size = n_community_max (some are smaller)
  
  # TODO: separate the combinations according to the number n_community_per_combination
  n_community_per_combination_order = order(rowSums(pre_optimal_combinations))
  
  # checking:
  # n_community_per_combination[n_community_per_combination_order]
  
  pre_optimal_combinations = pre_optimal_combinations[n_community_per_combination_order, ]
  n_community_vector = rowSums(pre_optimal_combinations)
  
  # will calculate everything for one day
  # coefficients will be a matrix of dim = 24*n_community:
  # will do a for loop to iterate for all the "types of days"
  
  hourly_surplus = apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen, df_cons = df_cons)
  pre_surplus = colSums(hourly_surplus)
  
  # checking:
  # colSums(pre_optimal_combinations)
  # nrow(pre_optimal_combinations)
  # hist(as.numeric(lapply(X = surplus, FUN = sum)))
  # sum(df_gen)
  
  ##### TODO PAYBACK
  # new_optimum_coefficients = pre_optimum_coefficients
  new_surplus = c()
  new_optimum_coefficients = list()
  new_payback = df_cons[0,1:ncol(df_cons)]
  
  # pre_payback = pre_optimum_coefficients
  
  j = 1
  vector_i = c()
  
  for (i in 1:nrow(pre_optimal_combinations)) {
    
    combination_selected = pre_optimal_combinations[i, ]
    df_cons_selected = df_cons[,combination_selected==1]
    individual_investment_max = individual_investment[combination_selected==1]  
    
    if (sum(individual_investment_max) > global_investment) {
      
      n_community = as.numeric(n_community_vector[i])
      
      # x just selects the users, no need to calculate the optimum combination here
      # I think calculating the coeffs should be inside the "inside GA"
      # optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
      
      individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)
      
      # just checking
      # if (any(individual_investment_selected > individual_investment_max)){
      #   print("fail")
      # }
      
      # pre_payback[i, combination_selected!=0] = calculate_payback(df_cons_selected, df_gen, individual_investment_selected, pre_optimum_coefficients[i,combination_selected!=0])
      
      # d = 0
      
      # sunny_hours_index = which(df_gen != 0)
      # df_gen_day = df_gen[sunny_hours_index + (d*24),]
      # df_cons_selected_user = df_cons_selected[sunny_hours_index + d*24,]
      
      n_sunny_hours = nrow(df_cons_selected)      
      dim = calculate_dim(hourly, n_community, n_sunny_hours)
      
      optim_results <- ga(type = "real-valued", fitness = fitness_2_betas, 
                          lower = array(0, dim = dim), upper = array(1, dim = dim),  
                          df_gen_day = df_gen, df_cons_selected_day = df_cons_selected, combination = combination_selected, 
                          individual_investment = individual_investment_selected, 
                          weight_surplus = weight_surplus, 
                          popSize = 100, maxiter = 10, run = 10)
      
      coefficients_optimum <- optim_results@solution[1, ]
      coefficients_optimum = matrix(data = coefficients_optimum, ncol = n_community, nrow = n_sunny_hours, byrow = T)
      # coefficients_x = matrix(data = x, ncol = n_community, nrow = n_sunny_hours, byrow = T)
      
      coefficients_optimum = coefficients_optimum/rowSums(coefficients_optimum)
      
      combination_optimum = matrix(1, nrow = nrow(coefficients_optimum)) %*% combination_selected
      combination_optimum[combination_optimum!=0] = coefficients_optimum
      
      new_optimum_coefficients[[j]] = coefficients_optimum
      
      new_payback[j, combination_selected!=0] = calculate_payback_betas(df_cons_selected, df_gen, individual_investment_selected, matrix_coefficients = coefficients_optimum)
      
      surplus = sum(calculate_surplus_hourly_individual_betas(coefficients_optimum, df_gen, df_cons_selected))
      
      new_surplus <- c(new_surplus, surplus)
      
      vector_i = c(vector_i, i)
      j = j + 1
    }
  }
  
  results = list(
    # "pre_optimum_coefficients" = pre_optimum_coefficients, 
    "pre_surplus" = pre_surplus,
    # "pre_payback" = pre_payback, 
    "new_optimum_coefficients" = new_optimum_coefficients, 
    "new_surplus" = new_surplus, 
    "new_payback" = new_payback, 
    "vector_i" = vector_i)
  
  return(results)
}


bee_uCrossover_float_betas <- function(object, parents, n_binary_rep, n_community){

  data = runif(n_community*24*2, 0, 1)

  # data = c(1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  parents <- matrix(data = data, nrow = 2, byrow = T)

  parents <- object@population[parents,,drop = FALSE]
  u <- unlist(lapply(X = 1:n_community,function(i)rep(runif(1), 24)))
  children <- parents
  children[1:2, u > 0.5] <- children[2:1, u > 0.5]
  out <- list(children = children, fitness = rep(NA,2))

  return(out)
}


fitness_1 <- function(x, n_community, n_binary_rep, df_gen, df_cons){

  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep*n_community)
  # x = rep(1, n_binary_rep*n_community)

  combination = calculate_combination_for_GA_binary(x, n_community = n_community, n_binary_rep = n_binary_rep)

  optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)

  surplus = sum(calculate_surplus_hourly_individual(df_gen, df_cons, optimum_coefficients))
  score <- surplus

  return(-score)
}


fitness_1_betas <- function(x, n_community, n_binary_rep, df_gen, df_cons){

  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep*n_community)
  # x = rep(1, n_binary_rep*n_community)

  combination = calculate_combination_for_GA_binary(x, n_community = n_community, n_binary_rep = n_binary_rep, df_cons = df_cons)

  surplus = sum(calculate_surplus_hourly_community(combination = combination, df_gen = df_gen, df_cons = df_cons))
  score <- surplus

  return(-score)
}


fitness_2_betas <- function(x, combination, df_gen_day, df_cons_selected_day, individual_investment_selected, weight_surplus, payback_ideal){

  # x = runif(dim, 0, 1)

  n_sunny_hours = nrow(df_cons_selected_day)
  n_community = ncol(df_cons_selected_day)

  coefficients_x = matrix(data = x, ncol = n_community, nrow = n_sunny_hours, byrow = T)
  coefficients_x = coefficients_x/rowSums(coefficients_x)

  df_gen_assigned = calculate_gen_assigned_betas(df_gen_day, matrix_coefficients = coefficients_x)

  # checking:
  # coefficients_x
  # df_gen_assigned/df_gen
  # identical(rowSums(df_gen_assigned), df_gen)

  surplus_x <- df_gen_assigned - df_cons_selected_day
  surplus_x[surplus_x < 0] = 0

  # TODO:
  cost_surplus = sum(surplus_x)

  purchase_price = 0.14859
  sale_price = 0.0508

  cost_old = colSums(purchase_price*df_cons_selected_day)

  grid_x = df_cons_selected_day - df_gen_assigned
  grid_x[grid_x < 0] = 0

  # checking: this should be zero
  # max(abs(-as.matrix(df_cons_selected) + as.matrix(grid_x) + as.matrix(df_gen_assigned) - as.matrix(surplus_x)))

  # TODO: change the "sale_price * coefficients_x * sum(surplus_x)"
  # this makes no sense with hourly betas
  surplus_x_to_sell = ifelse(colSums(surplus_x) < colSums(grid_x), colSums(surplus_x), colSums(grid_x))

  cost_sun = purchase_price*colSums(grid_x) - sale_price * surplus_x_to_sell

  profit_period = cost_old - cost_sun
  profit_one_year = profit_period * 360

  ##### TODO PAYBACK
  payback_years = individual_investment_selected / profit_one_year

  # TODO:
  payback_years[is.na(payback_years)] = 100

  # TODO: payback ideal?
  # payback_ideal = 1

  # TODO:
  cost_payback = sum(exp(payback_years - payback_ideal))
  # TODO: add something like this
  cost_payback_2 = max(payback_years) - min(payback_years)

  # score <- cost_surplus * cost_payback

  score <- weight_surplus * cost_surplus + (1-weight_surplus) * cost_payback
  # score <- 1

  return(-score)
}

############################# AUX - operative #############################


calculate_coefficients <- function(df_gen, df_cons, combination){
  
  # individual_hourly_surplus = calculate_surplus_hourly_individual(df_gen, df_cons, combination)
  # # TODO: check with pencil and paper that this is the correct way to calculate the coefficients
  # # this is the same as taking the mean of the hourly coefficients but, 
  # # I would like to take the coefficient of the hour(s) of maximum generation
  # surplus = sum(individual_hourly_surplus)
  # optimum_coefficients = surplus/colSums(individual_hourly_surplus)
  # optimum_coefficients[is.infinite(optimum_coefficients)] = NA
  # optimum_coefficients = optimum_coefficients/sum(optimum_coefficients, na.rm = T)
  # optimum_coefficients[is.na(optimum_coefficients)] = 0
  
  not_selected_cons = (combination == 0) 
  df_cons[, not_selected_cons] = 0
  
  df_gen_assigned <- calculate_gen_assigned(df_gen, combination)
  
  optimum_coefficients = df_cons/df_gen_assigned
  optimum_coefficients = optimum_coefficients/sum(optimum_coefficients, na.rm = T)
  
  return(optimum_coefficients)
}



