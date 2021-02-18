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


generate_fake_data_test3 <- function(df_gen_1, df_cons_1, df_cons_2, df_cons_3, df_cons_4){
  
  # TODO: 32 bits?
  df_day_1 = generate_fake_data_test2(df_gen_1, df_cons_1, df_cons_2, df_cons_3, df_cons_4)

  return(df_day_1)
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
  
  surplus_individual = calculate_surplus_individual(df_gen, df_cons, optimum_coefficients)  
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


optimize_GA <- function(n_periods, periods, n_community, df_gen, df_cons, individual_investment){
  
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
                      popSize = 50, maxiter = 50, run = 100)
  
  # TODO: understand: popSize should be simmilar to the combinatorial??

  x_solution = as.numeric(optim_results@solution[1, ])
  combination = calculate_combination_for_GA_binary(x_solution)

  optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
  
  return(optimum_coefficients)
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
    
    surplus = sum(calculate_surplus_individual(df_gen, df_cons, optimum_coefficients))
    profit = calculate_individual_profit(df_gen, df_cons, optimum_coefficients)
    rate_payback = profit/individual_investment
    i = i + 1
  }
  return(optimum_coefficients)
}


optimize_GA_new <- function(n_periods, periods, n_community, df_gen, df_cons, individual_investment){
  
  optimum_surplus_coefficients = optimize_GA(n_periods, periods, n_community, df_gen, df_cons, individual_investment)
  
  optimum_payback_surplus_coefficients = optimize_GA_2(n_periods, periods, n_community, df_gen, df_cons, individual_investment, optimum_surplus_coefficients)  

}

optimize_GA_2 <- function(n_periods, periods, n_community, df_gen, df_cons, individual_investment, optimum_surplus_coefficients){
  
  df_cons_selected = df_cons[optimum_surplus_coefficients!=0]
  
  optim_results <- ga(type = "real-valued", fitness = fitness_2, 
                      lower = array(0, dim = ncol(df_cons_selected)), upper = array(1, dim = ncol(df_cons_selected)),  
                      n_community = n_community, df_gen = df_gen_period, df_cons_selected = df_cons_selected, 
                      popSize = 100, maxiter = 200, run = 100)

  optimum_coefficients = as.numeric(optim_results@solution[1, ])

  return(optimum_coefficients)
}


fitness_2 <- function(x, n_community, df_gen, df_cons_selected){
  
  # TODO: start with the optimum_coefficients??
  
  # example:
  # self_consumption = calculate_gen_assigned(df_gen = df_gen, combination = optimum_coefficients)  
  # x = c(0.1, 0.2, 0.01)
  
  # TODO: Im normalizing the gene (applying a non-linear transformation to the gene created!). How does this alter the GA performance??
  # TODO: after studing how long it takes to converge.. try other methods, think on this :) 
  
  coefficients = x/sum(x)
  

    
  calculate_payback(df_gen, df_cons_selected, coefficients, gen_assigned)
   

  score <- sum(hourly_surplus)  
  
  return(-score)
}

calculate_payback <- function(tariff_price, df_cons_selected, coefficients){
  # example: tarifa plana
  fixed_charge = calculate_fixed_charge(5.75) 
  
  cost = 0.073
  access_toll = 0.044
    
  tariff_price = rep(cost + access_toll, nrow(df_cons_selected))
  price_without_PV = t(as.matrix(tariff_price)) %*% as.matrix(df_cons_selected)
  
  
  gen_assigned = calculate_gen_assigned(df_gen = df_gen, combination = coefficients)  

  grid_energy = df_cons_selected - gen_assigned 
  grid_energy[grid_energy < 0] = 0 
  
  price_energy_grid_with_PV = t(as.matrix(tariff_price)) %*% as.matrix(grid_energy)   
  
  surplus_hourly = calculate_surplus_hourly_individual(df_gen = df_gen, df_cons = df_cons_selected, combination = coefficients)
  surplus = sum(surplus_hourly)
  surplus_individual_billing = coefficients*surplus
  
  # TODO: check price
  PV_sale_price = 0.04
  sale_surplus_PV = PV_sale_price * surplus_individual_billing
    
  price_with_PV = price_energy_grid_with_PV - sale_surplus_PV
  
} 


calculate_price_energy_without_PV <- function(df_cons_selected, tariff_price){
  
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
  
  surplus = sum(calculate_surplus_individual(df_gen, df_cons, optimum_coefficients))
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
      
      surplus = sum(calculate_surplus_individual(df_gen, df_cons, optimum_coefficients))
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
  
  individual_hourly_surplus = calculate_surplus_individual(df_gen, df_cons, combination)
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


fitness <- function(x, n_community, df_gen, df_cons, rate_payback){
  
  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep*n_community)
  # x = rep(1, n_binary_rep*n_community)
  
  combination = calculate_combination_for_GA_binary(x)
  
  optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
  
  surplus = sum(calculate_surplus_individual(df_gen, df_cons, optimum_coefficients))
  score <- surplus

  return(-score)
}


fitness_payback <- function(x, n_community, df_gen, df_cons, rate_payback){

  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep*n_community)
  # x = rep(1, n_binary_rep*n_community)
  
  combination = calculate_combination_for_GA_binary(x)
  
  optimum_coefficients = calculate_coefficients(df_gen, df_cons, combination)
  
  surplus = sum(calculate_surplus_individual(df_gen, df_cons, optimum_coefficients))
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
      
      surplus = sum(calculate_surplus_individual(df_gen, df_cons, optimum_coefficients))
      profit = calculate_individual_profit(df_gen, df_cons, optimum_coefficients)
      rate_payback = profit/individual_investment
      i = i + 1
    }

    score <- surplus
  }
  
  return(-score)
}


############################################################
# plot


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

