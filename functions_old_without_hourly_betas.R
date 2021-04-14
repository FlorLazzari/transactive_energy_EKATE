############################# operative #############################


optimize_using_2_GAs_withBestSolutionSelection <- function(n_community_max, n_binary_rep, df_gen, df_cons, global_investment, individual_investment){
  
  # TODO: understand the criteria of the keepBest = T
  # keepBesta logical argument specifying if best solutions at each iteration should be savedin a slot calledbestSol. Seega-class.
  
  pre_optimal_combinations <- optimization_1(n_community = n_community_max, n_binary_rep = n_binary_rep, df_gen = df_gen, df_cons = df_cons)
  # not all of the combinations are of the size = n_community_max (some are smaller)
  
  # TODO: separate the combinations according to the number n_community_per_combination
  n_community_per_combination_order = order(rowSums(pre_optimal_combinations))
  
  # checking:
  # n_community_per_combination[n_community_per_combination_order]
  
  pre_optimal_combinations = pre_optimal_combinations[n_community_per_combination_order, ]
  n_community_vector = rowSums(pre_optimal_combinations)
  
  pre_optimum_coefficients = t((apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_coefficients, df_gen = df_gen, df_cons = df_cons)))
  # but all of the coefficients sum 1
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
  
  for (i in 1:nrow(pre_optimal_combinations)) {
    
    combination_selected = pre_optimal_combinations[i, ]
    df_cons_selected = df_cons[,combination_selected==1]
    individual_investment_max = individual_investment[combination_selected==1]  
    coefficients = calculate_coefficients(df_gen = df_gen, df_cons = df_cons, combination = combination_selected)
    # surplus_min_x = sum(calculate_surplus_hourly_individual(df_gen, df_cons, coefficients))
    
    # if (sum(combination_selected) == n_community &
    #     sum(individual_investment_max) > global_investment) {
    
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
      
      pre_payback[i, combination_selected!=0] = calculate_payback(df_cons_selected, df_gen, individual_investment_selected, pre_optimum_coefficients[i,combination_selected!=0])
      
      optim_results <- ga(type = "real-valued", fitness = fitness_2, 
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


optimization_1 <- function(n_community, n_binary_rep, df_gen, df_cons){
  
  optim_results <- ga(type = "binary", fitness = fitness_1, 
                      nBits = n_binary_rep*n_community,
                      n_community = n_community, df_gen = df_gen, df_cons = df_cons, n_binary_rep = n_binary_rep,  
                      # popSize = 100, maxiter = 1000, run = 100)
                      popSize = 200, maxiter = 100, run = 50,
                      crossover = purrr::partial(bee_uCrossover_binary, n_binary_rep = n_binary_rep, n_community = n_community), 
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
  combinations = t(apply(X = as.matrix(solutions), MARGIN = 1, FUN = calculate_combination_for_GA_binary, n_community = n_community, n_binary_rep = n_binary_rep))
  
  return(combinations)
}


############################# fitness #############################


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


fitness_2 <- function(x, combination, df_gen, df_cons_selected, individual_investment){
  # n_community = 4, amount of participans forming part of the community
  # vector of length N (in this case N = 16, amount of participants
  # for example
  # x = c(0.2, 0.6, 0.5, 0.3) 
  
  coefficients_x = x/sum(x)
  df_gen_assigned = calculate_gen_assigned(df_gen, coefficients_x)
  
  surplus_x <- df_gen_assigned - df_cons_selected
  surplus_x[surplus_x < 0] = 0
  
  # TODO:
  cost_surplus = sum(surplus_x)
  
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


############################# plot ############################## 


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
    
    ggsave(filename = paste0("graphs/user_",user), plot = p, device = "pdf", width = 5, height = 3)
    
  }
  return()
}


############################# AUX - main #############################


select_best_combinations <- function(optimal_combination_using_2_GAs){
  
  # pre_optimum_coefficients = optimal_combination_using_2_GAs$pre_optimum_coefficients
  # pre_surplus = optimal_combination_using_2_GAs$pre_surplus
  # pre_payback = optimal_combination_using_2_GAs$pre_payback
  new_optimum_coefficients = optimal_combination_using_2_GAs$new_optimum_coefficients
  new_surplus = optimal_combination_using_2_GAs$new_surplus
  new_payback = optimal_combination_using_2_GAs$new_payback
  
  index_order = order(new_surplus, decreasing = F)
  
  optimum_coefficients = new_optimum_coefficients[index_order[1],]
  # optimum_coefficients = new_optimum_coefficients[[index_order[1]]]
  surplus = new_surplus[index_order[1]]
  payback = new_payback[index_order[1], ]
  
  return(list("optimum_coefficients" = optimum_coefficients,
              "surplus" = surplus, 
              "payback" = payback))
}  


############################# AUX - operative #############################


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






