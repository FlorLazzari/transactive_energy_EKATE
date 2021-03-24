############################################################
# data reading 

# > min(as.Date(df_cons_1$time))
# [1] "2019-04-30"
# > max(as.Date(df_cons_1$time))
# [1] "2020-05-01"


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


generate_fake_data_test2 <- function(df_gen_1, df_cons_1, df_cons_2, df_cons_3, df_cons_4){
  
  df_day_1 = generate_fake_data_test1(df_gen_1, df_cons_1, df_cons_2, df_cons_3, df_cons_4)
  
  df_day_1$cons_5 = df_day_1$cons_1*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_6 = df_day_1$cons_1*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_7 = df_day_1$cons_1*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_8 = df_day_1$cons_1*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_9 = df_day_1$cons_1*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_10 = df_day_1$cons_1*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_11 = df_day_1$cons_4*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_12 = df_day_1$cons_4*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_13 = df_day_1$cons_4*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_14 = df_day_1$cons_4*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_15 = df_day_1$cons_4*runif(nrow(df_day_1), 0.0, 1.0)
  df_day_1$cons_16 = df_day_1$cons_4*runif(nrow(df_day_1), 0.0, 1.0)
  
  return(df_day_1)
}


select_month <- function(df, m=6){
  
  # select only a random month just to start:
  dates = seq(from = as.Date(paste0("2019-", as.character(m), "-01")), to = as.Date(paste0("2019-", as.character(m+1), "-01")), by = "day")

  # df_month_1 <- df[as.Date(df$time) %in% dates, ]
  df_month_1 = df[[1]][as.Date(df[[1]]$time) %in% dates, ]
  colnames(df_month_1)[2] = "gen_1"
  
  vector_colnames = paste0("cons_", 1:(length(df)-1))
  
  for (i in 2:length(df)) {
    df_month_1 <- merge(df_month_1, df[[i]], by = "time")
    colnames(df_month_1)[i+1] = vector_colnames[i-1]
  }

  return(df_month_1)
}


eliminate_outliers <- function(df, max_cut=160){
  df_mod = df
  df_mod[df_mod > max_cut] = NA
  df_mod[, 1] = df[, 1]
  return(df_mod)
}


reducing_consumption_fake <- function(df){
  
  max_gen = max(df$gen_1, na.rm = T)

  cols_high = unique(ceiling(which(df > max_gen)/nrow(df_month_1)))
  cols_high = cols_high[-1]
  
  df[,cols_high] = df[,cols_high]/3  
  
  return(df)
}


############################################################
# auxiliary functions


calculate_params_period <- function(n_periods){
  n_hours = floor(24/n_periods)
  init = (array(0:(n_periods-1)) * n_hours) + 1
  end = (array(1:(n_periods)) * n_hours) 
  return(data.frame(init, end))  
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


calculate_individual_profit <- function(df_gen, df_cons, optimum_coefficients){
  
  surplus_individual = calculate_surplus_hourly_individual(df_gen, df_cons, optimum_coefficients)  
  surplus_individual = rowSums(surplus_individual)
  # as.matrix(optimum_coefficients) %*% as.matrix(rowSums(surplus_individual))
  
  surplus_individual = as.data.frame(as.matrix(surplus_individual)%*%matrix(1, ncol =length(optimum_coefficients)))
  
  surplus_assigned = as.data.frame(surplus_individual * optimum_coefficients[col(surplus_individual)])  
  profit = colSums(surplus_assigned)
  
  return(profit) 
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


calculate_surplus_hourly <- function(df_gen, df_cons, combination){
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


############################################################
# operative functions


optimize_GA <- function(n_community, n_binary_rep, df_gen, df_cons, individual_investment){
  
  # problem of using the permutation => the population size will enlarge even more because a big number of possibilities (n_community!) 
  # represent the same scenario (porque en nuestro caso no nos importa el orden)
  # optim_results <- ga(type = "permutation", fitness = fitness, 
  #                     # nBits = 3,
  #                     lower = 1, upper = ncol(df_cons),
  #                     n_community = n_community, df_gen = df_gen_period, df_cons = df_cons_period, 
  #                     # popSize = 100, maxiter = 1000, run = 100)
  #                     popSize = 10, maxiter = 5, run = 100)
  # 
  # problem of using the binary with the positions of the users => should contrain the population only to those which sum(x) = n_community 
  # (would be eliminting most of the individuals generated by the GA) 
  # optim_results <- ga(type = "binary", fitness = fitness, 
  #                     nBits = 4,
  #                     # lower = rep(0, length(df_cons)), upper = rep(1, length(df_cons)),
  #                     # lower = 0, upper = 3,
  #                     n_community = n_community, df_gen = df_gen_period, df_cons = df_cons_period, 
  #                     # popSize = 100, maxiter = 1000, run = 100)
  #                     popSize = 10, maxiter = 5, run = 100)

  
  # TODO: how to make the crossover in "pieces" (uCrossover)
  optim_results <- ga(type = "binary", fitness = fitness, 
                      nBits = n_binary_rep*n_community,
                      n_community = n_community, df_gen = df_gen, df_cons = df_cons, 
                      # popSize = 100, maxiter = 1000, run = 100)
                      popSize = 50, maxiter = 50, run = 100,
                      crossover = purrr::partial(bee_uCrossover, n_binary_rep = n_binary_rep, n_community = n_community))
  # 
  # TODO: understand: popSize should be simmilar to the combinatorial??
  # look for: relation between popSize and dimension of the space (number of possible combinations) 

  x_solution = as.numeric(optim_results@solution[1, ])
  combination = calculate_combination_for_GA_binary(x_solution)

  optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
  
  return(optimum_coefficients)
}


# TODO: study more about crossovers and elaborate a better one
bee_uCrossover <- function(object, parents, n_binary_rep, n_community){
  
  # data = c(1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  # parents <- matrix(data = data, ncol = n_community*n_binary_rep)

  parents <- object@population[parents,,drop = FALSE]
  u <- unlist(lapply(X = 1:n_community,function(i)rep(runif(1), n_binary_rep)))
  children <- parents
  children[1:2, u > 0.5] <- children[2:1, u > 0.5]
  out <- list(children = children, fitness = rep(NA,2))  
  
  return(out)
}


optimize_GA_payback <- function(n_periods, periods, n_community, df_gen, df_cons, individual_investment){
  
  optimum_coefficients = optimize_GA(n_periods, periods, n_community, df_gen, df_cons, individual_investment)
  
  # compare the individual economical benefit (depends on the coefficients) with the rate_payback
  profit = calculate_individual_profit(df_gen, df_cons, optimum_coefficients)
  rate_payback = profit/individual_investment
  
  # TODO: this should change, computationally very expensive
  i = 0
  while (any(rate_payback[rate_payback!=0] < tolerance_rate_payback) & i<n_balance_iterations){
    
    which_low_payback  = which(rate_payback %in% rate_payback[(rate_payback<tolerance_rate_payback) & (rate_payback!= 0)])
    
    optimum_coefficients[which_low_payback] = optimum_coefficients[which_low_payback]*(1+increment_payback)
    optimum_coefficients = optimum_coefficients/sum(optimum_coefficients)
    
    surplus = sum(calculate_surplus_hourly_individual(df_gen, df_cons, optimum_coefficients))
    profit = calculate_individual_profit(df_gen, df_cons, optimum_coefficients)
    rate_payback = profit/individual_investment
    i = i + 1
  }
  return(optimum_coefficients)
}


optimize_using_2_GAs <- function(n_community, n_binary_rep, df_gen, df_cons, individual_investment){

  surplus_coefficients_optimum = optimize_GA(n_community, n_binary_rep, df_gen, df_cons, individual_investment)
  surplus_optimum = sum(calculate_surplus_hourly_individual(df_gen, df_cons, surplus_coefficients_optimum))
  
  # just checking
  investment_selected = rep(5000, ncol(df_cons[, surplus_coefficients_optimum!=0]))
  one_year_margin_optimum = calculate_one_year_margin(df_gen, df_cons[, surplus_coefficients_optimum!=0], surplus_coefficients_optimum[surplus_coefficients_optimum!=0], investment_selected)
  number_years_payback_optimum = investment_selected/one_year_margin_optimum
  surplus_hourly_individual_optimum = calculate_surplus_hourly_individual(df_gen, df_cons, surplus_coefficients_optimum)
  surplus_individual_optimum = colSums(surplus_hourly_individual_optimum)
  
  print(surplus_coefficients_optimum)
  print(number_years_payback_optimum)
  print(surplus_individual_optimum)
  print(surplus_optimum)
  
  # p2 = surplus_plots(df_gen, df_cons[, surplus_coefficients_optimum!=0], surplus_coefficients_optimum[surplus_coefficients_optimum!=0])

  payback_surplus_coefficients_optimum = optimize_GA_2(n_periods, periods, n_community, df_gen, df_cons, individual_investment, surplus_coefficients_optimum, surplus_optimum)
  
  surplus_optimum_2 = sum(calculate_surplus_hourly_individual(df_gen, df_cons[, surplus_coefficients_optimum!=0], payback_surplus_coefficients_optimum))

  one_year_margin_optimum_2 = calculate_one_year_margin(df_gen, df_cons[, surplus_coefficients_optimum!=0], payback_surplus_coefficients_optimum, investment_selected)
  number_years_payback_optimum_2 = investment_selected/one_year_margin_optimum_2
  surplus_hourly_individual_optimum_2 = calculate_surplus_hourly_individual(df_gen, df_cons[, surplus_coefficients_optimum!=0], payback_surplus_coefficients_optimum)
  surplus_individual_optimum_2 = colSums(surplus_hourly_individual_optimum_2)
  
  print(payback_surplus_coefficients_optimum)
  print(number_years_payback_optimum_2)
  print(surplus_individual_optimum_2)
  print(surplus_optimum_2)
  
  # p3 = surplus_plots(df_gen, df_cons[, surplus_coefficients_optimum!=0], payback_surplus_coefficients_optimum)
  
  coefficients_optimum = surplus_coefficients_optimum
  coefficients_optimum[coefficients_optimum!=0] = payback_surplus_coefficients_optimum
  
  return(coefficients_optimum)
}


optimize_using_2_GAs_one_cost_function <- function(n_community, n_binary_rep, df_gen, df_cons, individual_investment){

  # this will obly me to rewrite a better cost function
  
  one_year_investment_return_optimum = 500
  number_years_payback_optimum = individual_investment/one_year_investment_return_optimum
  
  optim_results <- ga(type = "real-valued", fitness = fitness_one_cost_function, 
                      lower = array(0, dim = ncol(df_cons)), upper = array(1, dim = ncol(df_cons)),  
                      n_community = n_community, df_gen = df_gen, df_cons = df_cons, 
                      individual_investment = individual_investment, number_years_payback_optimum = number_years_payback_optimum, 
                      popSize = 100, maxiter = 200, run = 100)
  
  solution <- optim_results@solution[1, ]
  coefficients_optimum <- calculate_combination_real_valued(solution, n_community)

  return(coefficients_optimum)
}


optimize_using_2nested_GAs <- function(n_community, n_binary_rep, df_gen, df_cons, individual_investment){

  optim_results <- ga(type = "binary", fitness = fitness_nested_outside, 
                      nBits = n_binary_rep*n_community,
                      n_community = n_community, df_gen = df_gen, df_cons = df_cons, individual_investment = individual_investment, 
                      # popSize = 100, maxiter = 1000, run = 100)
                      popSize = 50, maxiter = 50, run = 100,
                      crossover = purrr::partial(bee_uCrossover, n_binary_rep = n_binary_rep, n_community = n_community))
  # TODO: understand: popSize should be simmilar to the combinatorial??
  # look for: relation between popSize and dimension of the space (number of possible combinations) 
  # TODO: work a better crossover, now is being done a uCrossover in "pieces"
  
  x_solution = as.numeric(optim_results@solution[1, ])
  combination = calculate_combination_for_GA_binary(x_solution)
  
  optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
  
  return(optimum_coefficients)
}


# optimize_using_2nested_GAs_withBestSoltuionSelection <- function(n_community, n_binary_rep, df_gen, df_cons, global_investment, individual_investment){
# 
#   # TODO: understand the criteria of the keepBest = T
#   # pre_optimal_combinations <- pre_optimization_for_nested(n_community, n_binary_rep, df_gen, df_cons)
#   # optimum_coefficients = t((apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_coefficients, df_gen = df_gen, df_cons = df_cons)))
#   # surplus = apply(X = optimum_coefficients, MARGIN = 1, FUN = calculate_surplus_hourly, df_gen = df_gen, df_cons = df_cons)
#   # accepted_surplus = max(as.numeric(lapply(X = surplus, FUN = sum)))
# 
#   # checking:
#   # colSums(pre_optimal_combinations)
#   # nrow(pre_optimal_combinations)
#   # hist(as.numeric(lapply(X = surplus, FUN = sum)))
#   # sum(df_gen)
# 
#   # TODO: should look a better way to define the accepted_surplus
#   # taking into account a more iterations in pre_optimal..
#   # taking into account the total solar generation (a percentage of the solar generation accepted..?)
#   
#   accepted_surplus = 200
#   
#   optim_results <- ga(type = "binary", fitness = fitness_nested_outside, 
#                       nBits = n_binary_rep*n_community,
#                       n_community = n_community, df_gen = df_gen, df_cons = df_cons, global_investment = global_investment, individual_investment = individual_investment, accepted_surplus = accepted_surplus, 
#                       # popSize = 100, maxiter = 1000, run = 100)
#                       popSize = 10, maxiter = 15, run = 20,
#                       crossover = purrr::partial(bee_uCrossover, n_binary_rep = n_binary_rep, n_community = n_community))
#   # TODO: understand: popSize should be simmilar to the combinatorial??
#   # maxiter = the maximum number of iterations to run before the GA search is halted.
#   # run = the  number  of  consecutive  generations  without  any  improvement  in  the  bestfitness value before the GA is stopped
#   # look for: relation between popSize and dimension of the space (number of possible combinations) 
#   # TODO: work a better crossover, now is being done a uCrossover in "pieces"
#   
#   x_solution = as.numeric(optim_results@solution[1, ])
#   combination = calculate_combination_for_GA_binary(x_solution)
#   
#   df_cons_selected = df_cons[,combination==1]
#   individual_investment_selected = individual_investment[combination==1]  
#   
#   coefficients = calculate_coefficients(df_gen = df_gen, df_cons = df_cons, combination = combination)
#   surplus_min_x = sum(calculate_surplus_hourly_individual(df_gen, df_cons, coefficients))
#   
# 
#   # to calculate the optimum_coefficients:
#   optim_results <- ga(type = "real-valued", fitness = fitness_nested_inside, 
#                         lower = array(0, dim = n_community), upper = array(1, dim = n_community),  
#                         df_gen = df_gen, df_cons = df_cons_selected, combination = combination, 
#                         individual_investment = individual_investment_selected,
#                         popSize = 100, maxiter = 5, run = 5)
#   
#   coefficients_optimum <- optim_results@solution[1, ]
#   coefficients_optimum = coefficients_optimum/sum(coefficients_optimum)
#   
#   combination_optimum = combination
#   combination_optimum[combination_optimum!=0] = coefficients_optimum
# 
#   return(combination_optimum)
# }


optimize_using_2_GAs_withBestSoltuionSelection <- function(n_community, n_binary_rep, df_gen, df_cons, global_investment, individual_investment){
  
  # TODO: understand the criteria of the keepBest = T
  pre_optimal_combinations <- pre_optimization_for_nested(n_community, n_binary_rep, df_gen, df_cons)
  pre_optimum_coefficients = t((apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_coefficients, df_gen = df_gen, df_cons = df_cons)))
  hourly_surplus = apply(X = pre_optimum_coefficients, MARGIN = 1, FUN = calculate_surplus_hourly_individual, df_gen = df_gen, df_cons = df_cons)
  pre_surplus = as.numeric(lapply(X = hourly_surplus, FUN = sum))
  
  # checking:
  # colSums(pre_optimal_combinations)
  # nrow(pre_optimal_combinations)
  # hist(as.numeric(lapply(X = surplus, FUN = sum)))
  # sum(df_gen)

  new_optimum_coefficients = pre_optimum_coefficients
  new_surplus = rep(0, nrow(pre_optimal_combinations))
  
  new_payback = pre_optimum_coefficients
  pre_payback = pre_optimum_coefficients
  
  # here I will copy the "fitness_nested_outside":
  for (i in 1:nrow(pre_optimal_combinations)) {
    
    combination_selected = pre_optimal_combinations[i, ]
    
    df_cons_selected = df_cons[,combination_selected==1]
    
    individual_investment_max = individual_investment[combination_selected==1]  
    
    coefficients = calculate_coefficients(df_gen = df_gen, df_cons = df_cons, combination = combination_selected)
    surplus_min_x = sum(calculate_surplus_hourly_individual(df_gen, df_cons, coefficients))
    
    # this "if" is to:
    # solve problem: for the cases in which combination is less than n_community: 
    # a way to "preoptimize inside"
    
    if (sum(combination_selected) == n_community &
        sum(individual_investment_max) > global_investment) {
      
      # x just selects the users, no need to calculate the optimum combination here
      # I think calculating the coeffs should be inside the "inside GA"
      # optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
      
      individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)
      
      pre_payback[i, combination_selected!=0] = calculate_payback(df_cons_selected, df_gen, individual_investment_selected, pre_optimum_coefficients[i,combination_selected!=0])
      
      optim_results <- ga(type = "real-valued", fitness = fitness_nested_inside, 
                          lower = array(0, dim = n_community), upper = array(1, dim = n_community),  
                          df_gen = df_gen, df_cons_selected = df_cons_selected, combination = combination_selected, 
                          individual_investment = individual_investment_selected,
                          popSize = 100, maxiter = 5, run = 5)
      
      coefficients_optimum <- optim_results@solution[1, ]
      coefficients_optimum = coefficients_optimum/sum(coefficients_optimum)
      
      combination_optimum = combination_selected
      combination_optimum[combination_optimum!=0] = coefficients_optimum
      
      new_optimum_coefficients[i, ] = combination_optimum
      
      new_payback[i, combination_selected!=0] = calculate_payback(df_cons_selected, df_gen, individual_investment_selected, new_optimum_coefficients[i,combination_selected!=0])
      
      surplus = sum(calculate_surplus_hourly_individual(df_gen, df_cons, combination_optimum))
      new_surplus[i] <- surplus 
    } else{
      new_surplus[i] <- 1000000
    }
  }

  results = list("pre_optimum_coefficients" = pre_optimum_coefficients, 
                 "pre_surplus" = pre_surplus, 
                 "pre_payback" = pre_payback, 
                 "new_optimum_coefficients" = new_optimum_coefficients, 
                 "new_surplus" = new_surplus, 
                 "new_payback" = new_payback)

  return(results)
}


pre_optimization_for_nested <- function(n_community, n_binary_rep, df_gen, df_cons){
    
  optim_results <- ga(type = "binary", fitness = fitness, 
                      nBits = n_binary_rep*n_community,
                      n_community = n_community, df_gen = df_gen, df_cons = df_cons, 
                      # popSize = 100, maxiter = 1000, run = 100)
                      popSize = 200, maxiter = 100, run = 50,
                      crossover = purrr::partial(bee_uCrossover, n_binary_rep = n_binary_rep, n_community = n_community), 
                      keepBest = T,
                      pmutation = 0.3 
                      )
  # TODO: understand: popSize should be simmilar to the combinatorial??
  # look for: relation between popSize and dimension of the space (number of possible combinations) 
  # TODO: work a better crossover, now is being done a uCrossover in "pieces"
  
  # TODO: check how are the bestSolutions find
  x_solution = optim_results@bestSol
  
  x_solution = as.vector(unlist(x_solution))
  n_solutions = length(x_solution)/(n_binary_rep * n_community)
  factor = as.factor(rep(c(1:n_solutions), n_binary_rep * n_community)[order(rep(c(1:n_solutions), n_binary_rep * n_community))])
  solutions = t(as.data.frame(split(x = x_solution, f = factor)))
  
  solutions = solutions[!duplicated(solutions), ]
  combinations = t(apply(X = as.matrix(solutions), MARGIN = 1, FUN = calculate_combination_for_GA_binary))
  
  return(combinations)
}


fitness_nested_outside <- function(x, n_community, df_gen, df_cons, global_investment, individual_investment, accepted_surplus=NULL){
  
  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep*n_community)
  # x = c(1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0)
  combination = calculate_combination_for_GA_binary(x)
  df_cons_selected = df_cons[,combination==1]
  
  individual_investment_max = individual_investment[combination==1]  

  coefficients = calculate_coefficients(df_gen = df_gen, df_cons = df_cons, combination = combination)
  surplus_min_x = sum(calculate_surplus_hourly_individual(df_gen, df_cons, coefficients))
  
  # this "if" is to:
  # solve problem: for the cases in which combination is less than n_community: 
  # a way to "preoptimize inside"
  if (surplus_min_x < accepted_surplus & 
      sum(combination) == n_community &
      sum(individual_investment_max) > global_investment) {
    
    # x just selects the users, no need to calculate the optimum combination here
    # I think calculating the coeffs should be inside the "inside GA"
    # optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)

    individual_investment_selected = calculate_individual_investment(combination, global_investment, individual_investment_max)
    
    optim_results <- ga(type = "real-valued", fitness = fitness_nested_inside, 
                        lower = array(0, dim = n_community), upper = array(1, dim = n_community),  
                        df_gen = df_gen, df_cons_selected = df_cons_selected, combination = combination, 
                        individual_investment = individual_investment_selected,
                        popSize = 100, maxiter = 5, run = 5)
    
    coefficients_optimum <- optim_results@solution[1, ]
    coefficients_optimum = coefficients_optimum/sum(coefficients_optimum)
    
    combination_optimum = combination
    combination_optimum[combination_optimum!=0] = coefficients_optimum
    
    surplus = sum(calculate_surplus_hourly_individual(df_gen, df_cons, combination_optimum))
    score <- surplus 
  } else{
    print(surplus)
    print(surplus_min_x < accepted_surplus) 
    print()
    print(sum(combination) == n_community)
    print(sum(individual_investment_max) > global_investment)
   
    score <- 1000000
  }

  return(-score)
}


fitness_nested_inside <- function(x, combination, df_gen, df_cons_selected, individual_investment){
  # n_community = 4, amount of participans forming part of the community
  # vector of length N (in this case N = 16, amount of participants
  # for example
  # x = c(0.2, 0.6, 0.5, 0.3) 
  
  coefficients_x = x/sum(x)
  df_gen_assigned = calculate_gen_assigned(df_gen, coefficients_x)
  
  ####################
  surplus_x <- df_gen_assigned - df_cons_selected
  surplus_x[surplus_x < 0] = 0
  
  # TODO:
  cost_surplus = sum(surplus_x)
  
  ####################

  purchase_price = 0.14859
  sale_price = 0.0508
  
  cost_old = colSums(purchase_price*df_cons_selected)
  
  grid_x = df_cons_selected - df_gen_assigned
  grid_x[grid_x < 0] = 0
  
  # checking: this should be zero
  # max(abs(-as.matrix(df_cons_selected) + as.matrix(grid_x) + as.matrix(df_gen_assigned) - as.matrix(surplus_x)))
  cost_sun = purchase_price*colSums(grid_x) - sale_price * coefficients_x * sum(surplus_x)
  
  profit_period = cost_old - cost_sun
  length_period = nrow(df_cons_selected)
  profit_one_year = profit_period * 24*360 / length_period
  
  payback_years = individual_investment / profit_one_year 
  
  # TODO: payback ideal?
  payback_ideal = 4
  
  # TODO:
  cost_payback = sum(exp(payback_years - payback_ideal))
  # TODO: add something like this
  cost_payback_2 = max(payback_years) - min(payback_years) 
  
  score <- cost_surplus * cost_payback
  # score <- 1

  return(-score)
}


calculate_payback <- function(df_cons_selected, df_gen, individual_investment, coefficients_x){
  
  df_gen_assigned = calculate_gen_assigned(df_gen, coefficients_x)
  
  
  surplus_x <- df_gen_assigned - df_cons_selected
  surplus_x[surplus_x < 0] = 0
  
  
  purchase_price = 0.14859
  sale_price = 0.0508
  
  cost_old = colSums(purchase_price*df_cons_selected)
  
  grid_x = df_cons_selected - df_gen_assigned
  grid_x[grid_x < 0] = 0
  
  # checking: this should be zero
  # max(abs(-as.matrix(df_cons_selected) + as.matrix(grid_x) + as.matrix(df_gen_assigned) - as.matrix(surplus_x)))
  cost_sun = purchase_price*colSums(grid_x) - sale_price * coefficients_x * sum(surplus_x)
  
  profit_period = cost_old - cost_sun
  length_period = nrow(df_cons_selected)
  profit_one_year = profit_period * 24*360 / length_period
  
  payback_years = individual_investment / profit_one_year 
  
  return(payback_years)
}


fitness_one_cost_function <- function(x, n_community, df_gen, df_cons, individual_investment, number_years_payback_optimum){

  # n_community = 4, amount of participans forming part of the community
  # vector of length N (in this case N = 16, amount of participants
  # for example
  # x = c(0.2, 0.6, 0.5, 0.3, 0.2, 0.6, 0.5, 0.3, 0.2, 0.6, 0.5, 0.3, 0.2, 0.6, 0.5, 0.3) 
  
  coefficients_x <- calculate_combination_real_valued(x, n_community)

  # selects the users:
  which_coefficients_x_selected = coefficients_x!=0
  coefficients_x_selected = coefficients_x[which_coefficients_x_selected]
  df_cons_selected = df_cons[which_coefficients_x_selected]

  surplus_optimum = sum(df_gen)*0.03
  
  surplus_x = calculate_surplus_hourly_individual(df_gen = df_gen, df_cons = df_cons_selected, combination = coefficients_x_selected)
  surplus_x = sum(surplus_x)
  
  cost_surplus = exp((surplus_x/sum(df_gen) - surplus_optimum/sum(df_gen))*10)
  cost_surplus = mean(cost_surplus)
  
  investment_selected = individual_investment[which_coefficients_x_selected]

  one_year_margin_x = calculate_one_year_margin(df_gen, df_cons_selected, coefficients_x_selected, investment_selcted)
  number_years_payback_x = investment_selected/one_year_margin_x

  number_years_payback_optimum_selected = number_years_payback_optimum[which_coefficients_x_selected]
  
  cost_payback = exp(number_years_payback_x-number_years_payback_optimum_selected)
  cost_payback = mean(cost_payback)
  
  score <- sum(cost_surplus) + sum(cost_payback)  
  
  return(-score)
}


calculate_individual_investment <- function(combination, global_investment, individual_investment_max){
  
  individual_investment = individual_investment_max * ( global_investment/sum(individual_investment_max) )  
  return(individual_investment)  
}


calculate_combination_real_valued <- function(x, n_community){
  selected = order(x, decreasing = T)[1:n_community]
  combination = array(0, length(x))
  combination[selected] = x[1:n_community]
  total = sum(combination)
  combination = combination/total
  return(combination)    
}


surplus_plots <- function(df_gen, df_cons, surplus_coefficients_optimum){
  
  gen_assigned = calculate_gen_assigned(df_gen = df_gen, combination = surplus_coefficients_optimum)  
  
  grid_energy = df_cons - gen_assigned 
  grid_energy[grid_energy < 0] = 0 
  
  surplus_hourly <- gen_assigned - df_cons
  surplus_hourly[surplus_hourly < 0] = 0
  
  df_plot = gen_assigned
  df_plot$time = 1:nrow(df_plot)
  
  df_plot <- melt(data = df_plot, id.vars = "time", variable.name = "series")
  
  df_gen_plot = df_gen
  df_gen_plot$time = 1:nrow(df_gen_plot)
  
  p1 <- ggplot() + 
    geom_line(aes(df_gen_plot$time, df_gen_plot$gen_1)) +
    geom_area(aes(df_plot$time, df_plot$value, fill = df_plot$series)) 
    # geom_area(aes(x = hour(df_sum$time), y = df_sum$consumption_sum), alpha = 0.5) +
    # labs(x = "Time [h]", y = "Electrical energy [kWh]", "title" = "Electrical Generation and Consumption", color = "")  
  
  
  gen_used = gen_assigned - surplus_hourly

  df_plot = gen_used
  df_plot$time = 1:nrow(df_plot)
  
  df_plot <- melt(data = df_plot, id.vars = "time", variable.name = "series")

  p2 <- ggplot() + 
    geom_line(aes(df_gen_plot$time, df_gen_plot$gen_1)) +
    geom_area(aes(df_plot$time, df_plot$value, fill = df_plot$series)) 
  # geom_area(aes(x = hour(df_sum$time), y = df_sum$consumption_sum), alpha = 0.5) +
  # labs(x = "Time [h]", y = "Electrical energy [kWh]", "title" = "Electrical Generation and Consumption", color = "")  
    

  
  # df_plot = surplus_hourly
  # df_plot$time = 1:nrow(df_plot)
  # 
  # df_plot <- melt(data = df_plot, id.vars = "time", variable.name = "series")
  # 
  # # TODO: should be pie chart
  # p3 <- ggplot() + 
  #   geom_line(aes(df_gen_plot$time, df_gen_plot$gen_1)) +
  #   geom_area(aes(df_plot$time, df_plot$value, fill = df_plot$series)) 
  # labs(x = "Time [h]", y = "Electrical energy [kWh]", "title" = "Electrical Generation and Consumption", color = "")  

  
  
  # surplus = sum(surplus_hourly)
  # surplus_individual_billing = surplus_coefficients_optimum*surplus
  # 
  # df_plot = surplus_individual_billing
  # df_plot$time = 1:nrow(df_plot)
  # 
  # df_plot <- melt(data = df_plot, id.vars = "time", variable.name = "series")
  # 
  # # TODO: should be another pie chart
  # p4 <- ggplot() + 
  #   geom_line(aes(df_gen_plot$time, df_gen_plot$gen_1)) +
  #   geom_area(aes(df_plot$time, df_plot$value, fill = df_plot$series))
  # labs(x = "Time [h]", y = "Electrical energy [kWh]", "title" = "Electrical Generation and Consumption", color = "")  


  return(p2)
}




optimize_GA_2 <- function(n_periods, periods, n_community, df_gen, df_cons, individual_investment, surplus_coefficients_optimum, surplus_optimum){
  
  df_cons_selected = df_cons[surplus_coefficients_optimum!=0]
  
  # TODO: here is an example, should be a vector
  investment_selected = rep(5000, ncol(df_cons_selected))

  one_year_investment_return_optimum = 500
  number_years_payback_optimum = investment_selected/one_year_investment_return_optimum
  
  optim_results <- ga(type = "real-valued", fitness = fitness_2, 
                      lower = array(0, dim = ncol(df_cons_selected)), upper = array(1, dim = ncol(df_cons_selected)),  
                      n_community = n_community, df_gen = df_gen, df_cons_selected = df_cons_selected, 
                      investment_selected = investment_selected, number_years_payback_optimum = number_years_payback_optimum, surplus_optimum = surplus_optimum, 
                      popSize = 100, maxiter = 200, run = 100)

  optimum_coefficients = as.numeric(optim_results@solution[1, ])
  optimum_coefficients = optimum_coefficients/sum(optimum_coefficients)
  
  return(optimum_coefficients)
}


fitness_2 <- function(x, n_community, df_gen, df_cons_selected, investment_selected, number_years_payback_optimum, surplus_optimum){
  
  # TODO: start with the optimum_coefficients??
  
  # example:
  # self_consumption = calculate_gen_assigned(df_gen = df_gen, combination = optimum_coefficients)  
  # x = c(0.1, 0.2, 0.01)
  
  # TODO: Im normalizing the gene (applying a non-linear transformation to the gene created!). How does this alter the GA performance??
  # TODO: after studing how long it takes to converge.. try other methods, think on this :) 
  
  coefficients = x/sum(x)
  
  one_year_margin_x = calculate_one_year_margin(df_gen, df_cons_selected, coefficients, investment_selected)
  number_years_payback_x = investment_selected/one_year_margin_x
  

  
  # TODO: how to define the score function here???
  # IDEA: has to take into account: 
  # should not move much from the original "optimal" global surplus
  # the payback for all the users should be simmilar?
  # the payback should be proportional to the investment
  
  surplus_x = calculate_surplus_hourly_individual(df_gen = df_gen, df_cons = df_cons_selected, combination = coefficients)
  surplus_x = sum(surplus_x)

  cost_surplus = exp((surplus_x/sum(df_gen) - surplus_optimum/sum(df_gen))*10)
  cost_surplus = mean(cost_surplus)
  

  
  
  # comparing inside iterations
  
  # number_years_payback_x = c(10, 9, 11)
  # number_years_payback_x = c(2, 2, 12)
  # number_years_payback_x = c(10, 9, 10)
  # number_years_payback_x = c(10, 9, 9)
  # number_years_payback_x = c(9, 9, 9)
  # 
  # number_years_payback_optimum = c(10, 10, 10)
  # cost_payback = exp(number_years_payback_x-number_years_payback_optimum)
  # mean(cost_payback)  

  # number_years_payback_x = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 15)
  # number_years_payback_x = c(7, 7, 7, 7, 7, 7, 7, 9, 10, 15)
  # number_years_payback_x = c(7, 7, 7, 7, 7, 7, 7, 9, 10, 15)
  # number_years_payback_x = c(9, 10, 9, 10, 9, 10, 9, 10, 9, 15)
  # number_years_payback_x = c(7, 7, 7, 7, 7, 7, 7, 9, 10, 7)
  # number_years_payback_x = c(9, 10, 9, 10, 9, 10, 9, 10, 9, 7)
  # 
  # number_years_payback_optimum = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
  
  cost_payback = exp(number_years_payback_x-number_years_payback_optimum)
  # prod(cost_payback)  
  cost_payback = mean(cost_payback)

  score <- sum(cost_surplus) + sum(cost_payback)  
  
  return(-score)
}

calculate_one_year_margin <- function(df_gen, df_cons_selected, coefficients, investment_selected){
  
  # TODO: should change this to the discriminacion horaria in 3 ranges
  # example: tarifa plana 
  fixed_charge = calculate_fixed_charge(5.75) 
  
  cost = 0.073
  access_toll = 0.044
    
  tariff_price = rep(cost + access_toll, nrow(df_cons_selected))
  variable_charge_without_PV = as.numeric(t(as.matrix(tariff_price)) %*% as.matrix(df_cons_selected))
  
  electric_tax = 0.0511
  meter_renting = 0.81
  iva = 0.21

  price_without_PV = (1+electric_tax)*(variable_charge_without_PV + fixed_charge) + meter_renting
  price_without_PV = (1+iva)*price_without_PV
  
  gen_assigned = calculate_gen_assigned(df_gen = df_gen, combination = coefficients)  

  grid_energy = df_cons_selected - gen_assigned 
  grid_energy[grid_energy < 0] = 0 

  price_energy_grid_with_PV = as.numeric(t(as.matrix(tariff_price)) %*% as.matrix(grid_energy))   
  
  surplus_hourly <- gen_assigned - df_cons_selected
  surplus_hourly[surplus_hourly < 0] = 0
  surplus = sum(surplus_hourly)
  surplus_individual_billing = coefficients*surplus
  
  # TODO: check price
  PV_sale_price = 0.04
  sale_surplus_PV = PV_sale_price * surplus_individual_billing
    
  variable_charge_with_PV = price_energy_grid_with_PV - sale_surplus_PV

  price_with_PV = (1+electric_tax)*(variable_charge_with_PV + fixed_charge) + meter_renting
  price_with_PV = (1+iva)*price_with_PV  

  # TODO: change this to do automatic
  margin_one_month = price_without_PV - price_with_PV
  margin_one_year = margin_one_month*12
  
  return(margin_one_year)
} 



calculate_fixed_charge <- function(contracted_power){
  # TODO: get info to complete this
  if (contracted_power == 5.75) {
    fixed_charge = 19.45
    return(fixed_charge)
  }  
}


fitness_payback <- function(x, n_community, df_gen, df_cons, rate_payback){
  
  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep*n_community)
  # x = rep(1, n_binary_rep*n_community)
  
  combination = calculate_combination_for_GA_binary(x)
  
  optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
  
  surplus = sum(calculate_surplus_hourly_individual(df_gen, df_cons, optimum_coefficients))
  score <- surplus
  
  # compare the individual economical benefit (depends on the coefficients) with the rate_payback
  profit = calculate_individual_profit(df_gen, df_cons, optimum_coefficients)
  rate_payback = profit/individual_investment
  
  i = 0
  if (sum(rate_payback[rate_payback!=0] < tolerance_rate_payback) == n_community){
    score <- 1000
  } else{
    while (any(rate_payback[rate_payback!=0] < tolerance_rate_payback) & i<n_balance_iterations){
      
      which_low_payback  = which(rate_payback %in% rate_payback[(rate_payback<tolerance_rate_payback) & (rate_payback!= 0)])
      
      optimum_coefficients[which_low_payback] = optimum_coefficients[which_low_payback]*(1+increment_payback)
      optimum_coefficients = optimum_coefficients/sum(optimum_coefficients)
      
      surplus = sum(calculate_surplus_hourly_individual(df_gen, df_cons, optimum_coefficients))
      profit = calculate_individual_profit(df_gen, df_cons, optimum_coefficients)
      rate_payback = profit/individual_investment
      i = i + 1
    }
    
    score <- surplus
  }
  
  return(-score)
}


calculate_combination_for_GA_binary <- function(x){
  
  combination = rep(0, ncol(df_cons))
  
  for (j in 1:n_community) {
    user = binary2decimal(x[((j-1)*n_binary_rep + 1):(j*n_binary_rep)]) + 1  
    # print(user)
    combination[user] = 1
  }
  return(combination)
}


calculate_combination_for_GA_permutation <- function(x, n_community){
  combination <- rep(0, length(x))
  for (i in 1:n_community) {
    combination[x[i]] = 1 
  }
  return(combination)
}


calculate_coefficients <- function(df_gen, df_cons, combination){
  
  individual_hourly_surplus = calculate_surplus_hourly_individual(df_gen, df_cons, combination)
  # TODO: check with pencil and paper that this is the correct way to calculate the coefficients
  # this is the same as taking the mean of the hourly coefficients but, 
  # I would like to take the coefficient of the hour(s) of maximum generation
  surplus = sum(individual_hourly_surplus)
  optimum_coefficients = surplus/colSums(individual_hourly_surplus)
  optimum_coefficients[is.infinite(optimum_coefficients)] = NA
  optimum_coefficients = optimum_coefficients/sum(optimum_coefficients, na.rm = T)
  optimum_coefficients[is.na(optimum_coefficients)] = 0
  return(optimum_coefficients)
}


# calculate_coefficients_2 <- function(df_gen, df_cons, combination){
#   
#   surplus_hourly = calculate_surplus_hourly(df_gen, df_cons, combination)
#   # TODO: check with pencil and paper that this is the correct way to calculate the coefficients
#   # this is the same as taking the mean of the hourly coefficients but, 
#   # I would like to take the coefficient of the hour(s) of maximum generation
#   surplus = sum(surplus_hourly)
#   optimum_coefficients = surplus/colSums(individual_hourly_surplus)
#   optimum_coefficients[is.infinite(optimum_coefficients)] = NA
#   optimum_coefficients = optimum_coefficients/sum(optimum_coefficients, na.rm = T)
#   optimum_coefficients[is.na(optimum_coefficients)] = 0
#   return(optimum_coefficients)
# }


fitness <- function(x, n_community, df_gen, df_cons){
  
  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep*n_community)
  # x = rep(1, n_binary_rep*n_community)
  
  combination = calculate_combination_for_GA_binary(x)
  
  optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
  
  surplus = sum(calculate_surplus_hourly_individual(df_gen, df_cons, optimum_coefficients))
  score <- surplus

  return(-score)
}


fitness_payback <- function(x, n_community, df_gen, df_cons, rate_payback){

  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep*n_community)
  # x = rep(1, n_binary_rep*n_community)
  
  combination = calculate_combination_for_GA_binary(x)
  
  optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
  
  surplus = sum(calculate_surplus_hourly_individual(df_gen, df_cons, optimum_coefficients))
  score <- surplus
  
  # compare the individual economical benefit (depends on the coefficients) with the rate_payback
  profit = calculate_individual_profit(df_gen, df_cons, optimum_coefficients)
  rate_payback = profit/individual_investment
  
  i = 0
  if (sum(rate_payback[rate_payback!=0] < tolerance_rate_payback) == n_community){
    score <- 1000
  } else{
    while (any(rate_payback[rate_payback!=0] < tolerance_rate_payback) & i<n_balance_iterations){
      
      which_low_payback  = which(rate_payback %in% rate_payback[(rate_payback<tolerance_rate_payback) & (rate_payback!= 0)])
      
      optimum_coefficients[which_low_payback] = optimum_coefficients[which_low_payback]*(1+increment_payback)
      optimum_coefficients = optimum_coefficients/sum(optimum_coefficients)
      
      surplus = sum(calculate_surplus_hourly_individual(df_gen, df_cons, optimum_coefficients))
      profit = calculate_individual_profit(df_gen, df_cons, optimum_coefficients)
      rate_payback = profit/individual_investment
      i = i + 1
    }

    score <- surplus
  }
  
  return(-score)
}


selection_best_combination <- function(optimal_combination_using_2_GAs){
  
  pre_optimum_coefficients = optimal_combination_using_2_GAs$pre_optimum_coefficients
  pre_surplus = optimal_combination_using_2_GAs$pre_surplus
  pre_payback = optimal_combination_using_2_GAs$pre_payback
  new_optimum_coefficients = optimal_combination_using_2_GAs$new_optimum_coefficients
  new_surplus = optimal_combination_using_2_GAs$new_surplus
  new_payback = optimal_combination_using_2_GAs$new_payback
  
  index_order = order(new_surplus, decreasing = F)
  
  optimum_coefficients = new_optimum_coefficients[index_order[1],]
  surplus = new_surplus[index_order[1]]
  payback = new_payback[index_order[1], ]
  
  return(list("optimum_coefficients" = optimum_coefficients,
              "surplus" = surplus, 
              "payback" = payback))
}  


############################################################
# plot


initial_plot <- function(df){
  
  df_plot <- df 
  colnames(df)[2] <- "PV_generation" 
  
  df_plot <- melt(data = df_plot, id.vars = "time", variable.name = "series")
  # df_sum <- data.frame("time" = df$time,
  #                      "consumption_sum" = rowSums(df[, c(2, 3, 4, 6)]))
  p <- ggplot() + 
    geom_line(aes(df_plot$time, df_plot$value , color = df_plot$series)) +
    # geom_area(aes(x = df_sum$time, y = df_sum$consumption_sum), alpha = 0.5) +
    labs(x = "Time [h]", y = "Electrical energy [kWh]", "title" = "Electrical Generation and Consumption", color = "")  
  
  return(p)
}


plot_assignation_monthly <- function(df_gen, df_gen_assigned){
  
  df_plot_gen_assigned <- melt(data = df_gen_assigned, variable.name = "series")
  
  p <- ggplot() +
    geom_line(aes(x = 1:nrow(df_gen), y = df_gen[, 1])) +
    geom_area(aes(x = rep(x = 1:nrow(df_gen), times = ncol(df_gen_assigned)), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) +
    # geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) +
    labs(x = "Time [h]", y = "PV generation [kWh]", "title" = "PV static assignation", fill = "User")  
  return(p)
}


plot_solar_consumption_monthly <- function(df_gen, df_gen_assigned, df_cons){
  
  surplus <- df_gen_assigned - df_cons_selected
  surplus[surplus < 0] = 0
  
  df_solar_consumption = df_gen_assigned - surplus  

  df_solar_consumption <- melt(data = df_solar_constumption, variable.name = "series")
  
  p <- ggplot() +
    geom_line(aes(x = 1:nrow(df_gen), y = df_gen[, 1])) +
    geom_area(aes(x = rep(x = 1:nrow(df_gen_assigned), times = ncol(df_gen_assigned)), y = df_solar_consumption$value, fill = df_solar_consumption$series), alpha = 0.5) +
    # geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) +
    labs(x = "Time [h]", y = "PV generation [kWh]", "title" = "PV static assignation", fill = "User")  

  return(p)
}


plot_assignation_daily_mean <- function(df_gen, df_gen_assigned, time, optimum_coefficients_selected){
  
  df_gen_assigned = df_gen_assigned[, order(optimum_coefficients_selected, decreasing = T)]
  
  m = unique(month(time))
  df_gen$hour = hour(time) 
  df_gen_assigned$hour = hour(time)

  df_gen_mean = aggregate(x = df_gen, by = list(df_gen$hour), FUN = mean)
  df_gen_assigned_mean = aggregate(x = df_gen_assigned, by = list(df_gen_assigned$hour), FUN = mean)
  
  df_plot_gen_assigned <- melt(data = df_gen_assigned_mean[, -grep(pattern = "Group.1", x = colnames(df_gen_assigned_mean))], variable.name = "series", id.vars = "hour")
  
  p <- ggplot() +
    geom_line(aes(x = df_gen_mean[, "hour"], y = df_gen_mean[, "gen_1"])) +
    geom_area(aes(x = df_plot_gen_assigned[, "hour"], y = df_plot_gen_assigned[, "value"], fill = df_plot_gen_assigned[,"series"]), alpha = 0.5) +
    # geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) +
    labs(x = "Time [h]", y = "PV generation [kWh]", "title" = paste0("PV assignation for month ",m), fill = "User")  
  return(p)
}

calculate_solar_consumption <- function(df_gen_assigned, df_cons_selected){
  surplus <- df_gen_assigned - df_cons_selected
  surplus[surplus < 0] = 0
  
  df_solar_consumption = df_gen_assigned - surplus  
  return(df_solar_consumption)
}


plot_solar_consumption_daily_mean <- function(df_gen, df_gen_assigned, time){
  
  m = unique(month(time))

  df_solar_consumption = calculate_solar_consumption(df_gen_assigned, df_cons_selected)

  df_gen$hour = hour(time) 
  df_solar_consumption$hour = hour(time)
  
  df_gen_mean = aggregate(x = df_gen, by = list(df_gen$hour), FUN = mean)
  df_solar_consumption_mean = aggregate(x = df_solar_consumption, by = list(df_solar_consumption$hour), FUN = mean)
  
  df_plot_solar_consumption_mean <- melt(data = df_solar_consumption_mean[, -grep(pattern = "Group.1", x = colnames(df_solar_consumption_mean))], variable.name = "series", id.vars = "hour")
  
  p <- ggplot() +
    geom_line(aes(x = df_gen_mean[, "hour"], y = df_gen_mean[, "gen_1"])) +
    geom_area(aes(x = df_plot_solar_consumption_mean[, "hour"], y = df_plot_solar_consumption_mean[, "value"], fill = df_plot_solar_consumption_mean[,"series"]), alpha = 0.5) +
    labs(x = "Time [h]", y = "PV generation [kWh]", "title" = paste0("PV assignation for month ",m), fill = "User")  
  
  return(p)
}


# TODO: these pie plots doesnt say much
plot_pie_solar_consumption <- function(df_gen_assigned, df_cons_selected, time){

  m = unique(month(time))
  
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned, df_cons_selected)
  
  df_plot_solar_consumption <- data.frame(
    group = colnames(df_solar_consumption),
    value = colMeans(df_solar_consumption)
  )
  
  p <- ggplot() +
    geom_bar(aes(x = "", y = df_plot_solar_consumption[, "value"], fill = df_plot_solar_consumption[,"group"]), alpha = 0.5, width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    labs("title" = paste0("PV mean hourly consumption for month ",m), fill = "User")  

  return(p)
}


plot_pie_gen_assigned <- function(df_gen_assigned, df_cons_selected, time){
  
  m = unique(month(time))

  df_plot_gen_assigned <- data.frame(
    group = colnames(df_gen_assigned),
    value = colMeans(df_gen_assigned)
  )
  
  p <- ggplot() +
    geom_bar(aes(x = "", y = df_plot_gen_assigned[, "value"], fill = df_plot_gen_assigned[,"group"]), alpha = 0.5, width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    labs("title" = paste0("PV mean hourly consumption for month ",m), fill = "User")  
  
  return(p)
}


#####################

plot_disaggregated_daily_mean_per_user <- function(df_gen_assigned, df_cons_selected, time){
  
  # calculate solar consumption and surplus
  solar_consumption = calculate_solar_consumption(df_gen_assigned, df_cons_selected)

  solar_surplus <- df_gen_assigned - df_cons_selected
  solar_surplus[solar_surplus < 0] = 0
  
  grid = df_cons_selected - df_gen_assigned
  grid[grid < 0] = 0
  
  # add hour column
  solar_consumption$hour = hour(time)
  solar_surplus$hour = hour(time)
  grid$hour = hour(time)
  
  # aggregate
  solar_consumption_mean = aggregate(x = solar_consumption, by = list(solar_consumption$hour), FUN = mean)
  solar_surplus_mean = aggregate(x = solar_surplus, by = list(solar_surplus$hour), FUN = mean) 
  grid_mean = aggregate(x = grid, by = list(grid$hour), FUN = mean) 

  # to calculate the self consumption and surplus 
  df_cons_selected$hour = hour(time)
  df_cons_selected_mean = aggregate(x = df_cons_selected, by = list(df_cons_selected$hour), FUN = mean) 
  
  df_gen_assigned$hour = hour(time)
  df_gen_assigned_mean = aggregate(x = df_gen_assigned, by = list(df_gen_assigned$hour), FUN = mean)
  
  daily_hour <- solar_surplus_mean$hour

  plots_list = list()
  
  for (user in 1:(ncol(df_cons_selected)-1)) {

    solar_consumption_mean_user <- solar_consumption_mean[, c(1+user)]
    grid_mean_user <- grid_mean[, c(1+user)]
    solar_surplus_mean_user <- solar_surplus_mean[, c(1+user)]
    
    df_plot <- data.frame("hour" = daily_hour,
                          "Solar_surplus" = solar_surplus_mean_user,
                          "Solar_consumption" = solar_consumption_mean_user,
                          "Grid_consumption" = grid_mean_user
                          )
      
    df_plot = melt(df_plot, id.vars = "hour")
    
    # TODO: understand which of the 2 is the correct one
    # self_consumption_percentage_mean_1 = mean( df_plot[grep(x = df_plot$variable, pattern = "solar_consumption"), "value"] / df_cons_selected_mean[, user+1]) 
    self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, user+1])
    
    surplus_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "surplus"), "value"]) / sum(df_gen_assigned_mean[, user+1])  

    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, user+1])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", "title" = paste0("User ", user,": self consumption = ", round(self_consumption_percentage_mean, digits = 2), " & surplus = ", round(surplus_percentage_mean, digits = 2)), fill = "")  
    
    ggsave(filename = paste0("graphs/user_",user), plot = p, device = "pdf")
    
  }
  return()
}


plot_disaggregated_daily_mean_community <- function(df_gen_assigned, df_cons_selected, time){
  
  # calculate solar consumption and surplus
  solar_consumption = calculate_solar_consumption(df_gen_assigned, df_cons_selected)
  
  solar_surplus <- df_gen_assigned - df_cons_selected
  solar_surplus[solar_surplus < 0] = 0
  
  grid = df_cons_selected - df_gen_assigned
  grid[grid < 0] = 0
  
  # add hour column
  solar_consumption$hour = hour(time)
  solar_surplus$hour = hour(time)
  grid$hour = hour(time)
  
  # aggregate
  solar_consumption_mean = aggregate(x = solar_consumption, by = list(solar_consumption$hour), FUN = mean)
  solar_surplus_mean = aggregate(x = solar_surplus, by = list(solar_surplus$hour), FUN = mean) 
  grid_mean = aggregate(x = grid, by = list(grid$hour), FUN = mean) 
  
  # to calculate the self consumption and surplus 
  df_cons_selected$hour = hour(time)
  df_cons_selected_mean = aggregate(x = df_cons_selected, by = list(df_cons_selected$hour), FUN = mean) 
  
  df_gen_assigned$hour = hour(time)
  df_gen_assigned_mean = aggregate(x = df_gen_assigned, by = list(df_gen_assigned$hour), FUN = mean)
  
  daily_hour <- solar_surplus_mean$hour

  ########################
  
  solar_consumption_mean_community <- rowSums(solar_consumption_mean[, 2:(n_community+1)])
  grid_mean_community <- rowSums(grid_mean[, 2:(n_community+1)])  
  solar_surplus_mean_community <- rowSums(solar_surplus_mean[, 2:(n_community+1)])  
  
  df_plot <- data.frame("hour" = daily_hour,
                        "Solar_surplus" = solar_surplus_mean_community,
                        "Solar_consumption" = solar_consumption_mean_community,
                        "Grid_consumption" = grid_mean_community
                        )
  
  df_plot = melt(df_plot, id.vars = "hour")
  
  # TODO: understand which of the 2 is the correct one
  # self_consumption_percentage_mean_1 = mean( df_plot[grep(x = df_plot$variable, pattern = "solar_consumption"), "value"] / df_cons_selected_mean[, user+1]) 
  self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, user+1])
  
  surplus_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "surplus"), "value"]) / sum(df_gen_assigned_mean[, user+1])  
  
  index_order = order(colSums(df_cons_selected_mean[2:5]))
  
  p <- ggplot() +
    geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
    geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[, 1+index_order[2]])) +
    geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[, 1+index_order[2]] + df_cons_selected_mean[, 1+index_order[3]] )) +
    geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[, 1+index_order[2]] + df_cons_selected_mean[, 1+index_order[3]] + df_cons_selected_mean[, 1+index_order[4]])) +
    geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
    grids() + 
    labs(x = "Time [h]", y = "Energy [kWh]", "title" = paste0("User ", user,": self consumption = ", round(self_consumption_percentage_mean, digits = 2), " & surplus = ", round(surplus_percentage_mean, digits = 2)), fill = "")  
    
  ggsave(filename = paste0("graphs/community_",), plot = p, device = "pdf")
    
  return()
}
