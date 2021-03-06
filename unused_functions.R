# functions that are not being used

############################# data reading #############################


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


############################# operative #############################

# PROBLEM: Tooo simple
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


# PROBLEM: Tooo slow
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


optimize_GA_2 <- function(n_periods, periods, n_community, df_gen, df_cons, individual_investment, surplus_coefficients_optimum, surplus_optimum){
  
  df_cons_selected = df_cons[surplus_coefficients_optimum!=0]
  
  # TODO: here is an example, should be a vector
  investment_selected = rep(5000, ncol(df_cons_selected))
  
  one_year_investment_return_optimum = 500
  number_years_payback_optimum = investment_selected/one_year_investment_return_optimum
  
  optim_results <- ga(type = "real-valued", fitness = fitness_bis, 
                      lower = array(0, dim = ncol(df_cons_selected)), upper = array(1, dim = ncol(df_cons_selected)),  
                      n_community = n_community, df_gen = df_gen, df_cons_selected = df_cons_selected, 
                      investment_selected = investment_selected, number_years_payback_optimum = number_years_payback_optimum, surplus_optimum = surplus_optimum, 
                      popSize = 100, maxiter = 200, run = 100)
  
  optimum_coefficients = as.numeric(optim_results@solution[1, ])
  optimum_coefficients = optimum_coefficients/sum(optimum_coefficients)
  
  return(optimum_coefficients)
}


############################# fitness #############################


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


fitness_bis <- function(x, n_community, df_gen, df_cons_selected, investment_selected, number_years_payback_optimum, surplus_optimum){
  
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


############################# auxiliary #############################


calculate_params_period <- function(n_periods){
  n_hours = floor(24/n_periods)
  init = (array(0:(n_periods-1)) * n_hours) + 1
  end = (array(1:(n_periods)) * n_hours) 
  return(data.frame(init, end))  
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


calculate_combination_real_valued <- function(x, n_community){
  selected = order(x, decreasing = T)[1:n_community]
  combination = array(0, length(x))
  combination[selected] = x[1:n_community]
  total = sum(combination)
  combination = combination/total
  return(combination)    
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


calculate_combination_for_GA_permutation <- function(x, n_community){
  combination <- rep(0, length(x))
  for (i in 1:n_community) {
    combination[x[i]] = 1 
  }
  return(combination)
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


############################# plot #############################


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

