calculate_gen_assigned <- function(df_gen, combination){
  df_gen = as.data.frame(as.matrix(df_gen)%*%matrix(1, ncol =length(combination)))
  df_gen_assigned <- as.data.frame(df_gen * combination[col(df_gen)])  
  return(df_gen_assigned)
}

calculate_surplus <- function(df_gen, df_cons, combination){
  not_selected_cons = (combination == 0) 
  df_cons[, not_selected_cons] = 0
  df_gen_assigned <- calculate_gen_assigned(df_gen, combination) 
  total_surplus <- rowSums(df_gen_assigned - df_cons)
  hourly_surplus <- ifelse(total_surplus >= 0, total_surplus, 0)
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
  selected = order(x, decreasing = T)[1:n]
  combination = array(0, length(x))
  combination[selected] = x[1:n]
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
  hourly_surplus <- calculate_surplus(df_gen, df_cons, combination)
  
  score <- sum(hourly_surplus)  
  
  return(-score)
}

optimize_repartition_GA <- function(n_periods, periods, n_community, df_gen, df_cons){
  
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
                          popSize = 100, maxiter = 1000, run = 100)
      
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


############################################################

plot_assignation <- function(df_gen, df_gen_assigned){
  
  df_plot_gen_assigned <- melt(data = df_gen_assigned, variable.name = "series")
  
  p <- ggplot() +
    geom_line(aes(x = 1:24, y = df_gen[, 1])) +
    geom_area(aes(x = rep(x = 1:24, times = ncol(df_gen_assigned)), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) +
    # geom_area(aes(x = hour(df_plot_gen_assigned$time), y = df_plot_gen_assigned$value, fill = df_plot_gen_assigned$series), alpha = 0.5) +
    labs(x = "Time [h]", y = "PV generation [kWh]", "title" = "PV static assignation", fill = "User")  
  return(p)
}


