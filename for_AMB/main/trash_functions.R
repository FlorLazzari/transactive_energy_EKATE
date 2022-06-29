import_one_user <- function(filename_1){
  df <- read.csv(file = filename_1, header = TRUE)
  colnames(df) <- c("time", "energy")
  df$time <- as.POSIXct(as.character(df$time), format = "%d-%m-%Y %H:%M", tz = "Europe/Madrid") 
  return(df)
}


import_data_inergy <- function(filename_1){
  df <- read.csv(file = filename_1, header = TRUE, sep = ";")
  colnames(df) <- c("id", "tariff", "time", "energy")
  df$time <- as.POSIXct(as.character(df$time), format = "%d/%m/%Y %H:%M", tz = "Europe/Madrid") 
  return(df)
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


reducing_consumption_fake <- function(df){
  
  max_gen = max(df$gen_1, na.rm = T)
  
  cols_high = unique(ceiling(which(df > max_gen)/nrow(df_month_1)))
  cols_high = cols_high[-1]
  
  df[,cols_high] = df[,cols_high]/3  
  
  return(df)
}


remove_cero_values <- function(df_gen_raw){
  df_gen_raw$gen[df_gen_raw$gen == 0] = NA
  df_gen = df_gen_raw
  return(df_gen)
}


cut_selected_days <- function(df_gen_raw, selected_year_generation){
  df_gen = df_gen_raw[as.Date(df_gen_raw$time) %in% as.Date(selected_year_generation), ]
  return(df_gen)
}


generate_sunny <- function(df_local_time_gen, df_gen){
  df_local_time_gen$sunny = !is.na(df_gen$gen)
  return(df_local_time_gen)
}


cut_sunny<- function(df_local_time_gen, df_gen){
  df_gen = df_gen[df_local_time_gen$sunny,]
  return(df_gen)
}


select_n_users <- function(df_cons_characteristic_filtered, n_users){
  df_cons = df_cons_characteristic_filtered[, grep(pattern = "cons", x = colnames(df_cons_characteristic_filtered))[1:n_users]]  
  return(df_cons)
}


optimization_1 <- function(hourly, n_community, n_binary_rep, df_gen, df_cons){
  
  # if (hourly == T) {
  #   fitness = fitness_1_betas
  # } else{
  #   fitness = fitness_1
  # }
  
  dim_search_ga = calculate_combinatorics(n = ncol(df_cons), m = n_community) * factorial(n_community)
  dim_search_solution = calculate_combinatorics(n = ncol(df_cons), m = n_community) * factorial(n_community)
  
  dim_search_ga = 600
  
  optim_results <- ga(type = "binary", fitness = fitness_1_betas, 
                      nBits = n_binary_rep*n_community,
                      n_community = n_community, df_gen = df_gen, df_cons = df_cons, n_binary_rep = n_binary_rep,  
                      # popSize = 100, maxiter = 1000, run = 100)
                      popSize = dim_search_ga, maxiter = 400, run = 50,
                      crossover = purrr::partial(bee_uCrossover_binary, n_binary_rep = n_binary_rep, n_community = n_community), 
                      keepBest = T,
                      pmutation = 0.7
  )
  
  # TODO: understand: popSize should be simmilar to the combinatorial??
  # look for: relation between popSize and dimension of the space (number of possible combinations) 
  # TODO: work a better crossover, now is being done a uCrossover in "pieces"
  # TODO: check how are the bestSolutions found
  x_solution = optim_results@bestSol
  
  x_solution = as.vector(unlist(x_solution))
  n_solutions = length(x_solution)/(n_binary_rep * n_community)
  factor = as.factor(rep(c(1:n_solutions), n_binary_rep * n_community)[order(rep(c(1:n_solutions), n_binary_rep * n_community))])
  solutions = t(as.data.frame(split(x = x_solution, f = factor)))
  combinations = t(apply(X = as.matrix(solutions), MARGIN = 1, FUN = calculate_combination_for_GA_binary, n_community = n_community, n_binary_rep = n_binary_rep, df_cons = df_cons))
  
  # combinations are repeated in the cases, for example: 
  # combinations = ( 3 -- 2 -- 1) or (1 -- 2 -- 3)
  # combinations = ( 3 -- 3 -- 1) or (1 -- 1 -- 3)
  # the difference is due to the repetition:
  # combinatorics counting as different elements to the repeted elements: n! / (n-p)!               CASE1: the search space of the GA 
  # combinatorics without counting as different elements to the repeted elements: n! / p! (n-p)!    CASE2: the useful solutions for us 
  # if p! is big => the differences between combinations_complete and combinations will tend to be bigger 
  
  # the important number is: how many are we loosing by implementig the algo in this way??
  combinations_complete = combinations
  combinations = combinations[!duplicated(combinations), ]
  
  surplus = colSums(apply(X = as.matrix(combinations), MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen, df_cons = df_cons))
  
  surplus_ordered = surplus[order(surplus)]
  # hist(surplus_ordered)
  # q_05 = quantile(x = surplus_ordered, probs = 0.5)
  # surplus_ordered_filtered = surplus_ordered[surplus_ordered < q_05]
  
  n_filter = min(50,length(surplus_ordered))
  surplus_ordered_filtered = surplus_ordered[1:n_filter]
  
  combinations_ordered = combinations[order(surplus), ]
  # combinations_ordered_filtered = combinations[surplus_ordered < q_05, ]
  combinations_ordered_filtered = combinations_ordered[1:n_filter, ]
  
  return(combinations_ordered_filtered)
}


optimization_1_to_analize_convergence <- function(n_community, n_binary_rep, df_gen_to_optimize, df_cons_to_optimize, max_run){
  
  # dim_search_ga = calculate_combinatorics(n = ncol(df_cons_to_optimize), m = n_community) * factorial(n_community)
  # dim_search_solution = calculate_combinatorics(n = ncol(df_cons_to_optimize), m = n_community) * factorial(n_community)
  dim_search_ga = 600
  
  tic = Sys.time()
  optim_results <- ga(type = "binary", fitness = fitness_1_betas, 
                      nBits = n_binary_rep*n_community,
                      n_community = n_community, df_gen_to_optimize = df_gen_to_optimize, df_cons_to_optimize = df_cons_to_optimize, n_binary_rep = n_binary_rep,  
                      # popSize = 100, maxiter = 1000, run = 100)
                      popSize = dim_search_ga, 
                      maxiter = 400, 
                      run = max_run,
                      crossover = purrr::partial(bee_uCrossover_binary, n_binary_rep = n_binary_rep, n_community = n_community), 
                      keepBest = T,
                      pmutation = 0.7
  )
  toc = Sys.time()
  time = toc-tic
  
  solutions = optim_results@solution
  selected_combination = t(apply(X = as.matrix(solutions), MARGIN = 1, FUN = calculate_combination_for_GA_binary, n_community = n_community, n_binary_rep = n_binary_rep, df_cons = df_cons_to_optimize))
  
  x_solution = optim_results@bestSol
  # I had problems with this => changed to the transpose solution
  # x_solution = as.vector(unlist(x_solution))
  x_solution = lapply(X = x_solution, FUN = t)
  x_solution = as.vector(unlist(x_solution, recursive = F))
  n_solutions = length(x_solution)/(n_binary_rep * n_community)
  factor = as.factor(rep(c(1:n_solutions), n_binary_rep * n_community)[order(rep(c(1:n_solutions), n_binary_rep * n_community))])
  solutions = t(as.data.frame(split(x = x_solution, f = factor)))
  best_combinations = t(apply(X = as.matrix(solutions), MARGIN = 1, FUN = calculate_combination_for_GA_binary, n_community = n_community, n_binary_rep = n_binary_rep, df_cons = df_cons_to_optimize))
  
  return(list("best_combinations" = best_combinations, "selected_combination" = selected_combination, "time" = time))
}


optimization_1_boxes <- function(n_community, n_binary_rep_box, list_box_season, df_gen_to_optimize, df_cons_to_optimize){
  
  # dim_search_ga = calculate_combinatorics(n = ncol(df_cons), m = n_community) * factorial(n_community)
  # dim_search_solution = calculate_combinatorics(n = ncol(df_cons), m = n_community) * factorial(n_community)
  # dim_search_solution = factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)
  
  dim_search_ga = 600
  optim_results <- ga(type = "binary", fitness = fitness_1_betas_boxes, 
                      nBits = n_binary_rep_box*n_community,
                      n_community = n_community, n_binary_rep_box = n_binary_rep_box, list_box_season = list_box_season, df_gen = df_gen_to_optimize, df_cons = df_cons_to_optimize,    
                      # popSize = 100, maxiter = 1000, run = 100)
                      popSize = dim_search_ga, maxiter = 400, run = 20,
                      crossover = purrr::partial(bee_uCrossover_binary, n_binary_rep = n_binary_rep_box, n_community = n_community), 
                      # keepBest = T,
                      pmutation = 0.7
  )
  
  # bestSol if keepBest = TRUE, the best solutions at each iteration
  # keeps the 50 best solutions for every iteration
  # without using keepBest then I only see the 50 best for the last iteration
  
  solutions = optim_results@solution
  combinations = t(apply(X = as.matrix(solutions), MARGIN = 1, FUN = calculate_combination_for_GA_binary_boxes, n_community = n_community, n_binary_rep_box = n_binary_rep_box, list_box_season = list_box_season, df_cons = df_cons_to_optimize))
  
  return(combinations)
}


optimization_1_boxes_to_analize_convergence <- function(n_community, n_binary_rep_box, list_box_season, df_gen_to_optimize, df_cons_to_optimize){
  
  # dim_search_ga = calculate_combinatorics(n = ncol(df_cons), m = n_community) * factorial(n_community)
  # dim_search_solution = calculate_combinatorics(n = ncol(df_cons), m = n_community) * factorial(n_community)
  # dim_search_solution = factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)*factorial(8)
  
  dim_search_ga = 600
  optim_results <- ga(type = "binary", fitness = fitness_1_betas_boxes, 
                      nBits = n_binary_rep_box*n_community,
                      n_community = n_community, n_binary_rep_box = n_binary_rep_box, list_box_season = list_box_season, df_gen = df_gen_to_optimize, df_cons = df_cons_to_optimize,    
                      # popSize = 100, maxiter = 1000, run = 100)
                      popSize = dim_search_ga, maxiter = 400, run = 20,
                      crossover = purrr::partial(bee_uCrossover_binary, n_binary_rep = n_binary_rep_box, n_community = n_community), 
                      keepBest = T,
                      pmutation = 0.7
  )
  
  # bestSol if keepBest = TRUE, the best solutions at each iteration
  # keeps the 50 best solutions for every iteration
  # without using keepBest then I only see the 50 best for the last iteration
  solutions = optim_results@bestSol
  solutions = data.frame(matrix(unlist(solutions), nrow=length(solutions), byrow=TRUE))
  
  # solutions = optim_results@solution
  best_combinations = t(apply(X = as.matrix(solutions), MARGIN = 1, FUN = calculate_combination_for_GA_binary_boxes, n_community = n_community, n_binary_rep_box = n_binary_rep_box, list_box_season = list_box_season, df_cons = df_cons))
  
  return(best_combinations)
}


# optimization_1_to_analize <- function(n_community, n_binary_rep, df_gen_to_optimize, df_cons_to_optimize){
#   
#   dim_search_ga = calculate_combinatorics(n = ncol(df_cons_to_optimize), m = n_community) * factorial(n_community)
#   dim_search_solution = calculate_combinatorics(n = ncol(df_cons_to_optimize), m = n_community) * factorial(n_community)
#   
#   dim_search_ga = 600
#   
#   optim_results <- ga(type = "binary", fitness = fitness_1_betas, 
#                       nBits = n_binary_rep*n_community,
#                       n_community = n_community, df_gen = df_gen_to_optimize, df_cons = df_cons_to_optimize, n_binary_rep = n_binary_rep,  
#                       # popSize = 100, maxiter = 1000, run = 100)
#                       popSize = dim_search_ga, maxiter = 400, run = 50,
#                       crossover = purrr::partial(bee_uCrossover_binary, n_binary_rep = n_binary_rep, n_community = n_community), 
#                       pmutation = 0.7
#   )
#   
#   x_solution = optim_results@solution
#   
#   # x_solution = as.vector(unlist(x_solution))
#   # n_solutions = length(x_solution)/(n_binary_rep * n_community)
#   # factor = as.factor(rep(c(1:n_solutions), n_binary_rep * n_community)[order(rep(c(1:n_solutions), n_binary_rep * n_community))])
#   # solutions = t(as.data.frame(split(x = x_solution, f = factor)))
#   # combinations = t(apply(X = as.matrix(solutions), MARGIN = 1, FUN = calculate_combination_for_GA_binary, n_community = n_community, n_binary_rep = n_binary_rep, df_cons = df_cons_to_optimize))
#   
#   combinations = t(apply(X = as.matrix(x_solution), MARGIN = 1, FUN = calculate_combination_for_GA_binary, n_community = n_community, n_binary_rep = n_binary_rep, df_cons = df_cons_to_optimize))
#   
#   # the important number is: how many are we loosing by implementig the algo in this way??
#   combinations_complete = combinations
#   combinations = combinations[!duplicated(combinations), ]
#   
#   # surplus_ordered = surplus[order(surplus)]
#   # hist(surplus_ordered)
#   # q_05 = quantile(x = surplus_ordered, probs = 0.5)
#   # surplus_ordered_filtered = surplus_ordered[surplus_ordered < q_05]
#   
#   # n_filter = min(50,length(surplus_ordered))
#   # surplus_ordered_filtered = surplus_ordered[1:n_filter]
#   
#   # combinations_ordered = combinations[order(surplus), ]
#   # combinations_ordered_filtered = combinations[surplus_ordered < q_05, ]
#   # combinations_ordered_filtered = combinations_ordered[1:n_filter, ]
#   
#   return(combinations)
# }


# fitness_MO_quantos <- function(x, df_gen_sunny, df_cons_selected_sunny, purchase_price_sunny, individual_investment_selected){
#   
#   # checking:
#   # fitness_MO(runif(dim, 0, 1),
#   #            df_gen_day = df_gen_day,
#   #            df_cons_selected_day = df_cons_selected_day,
#   #            individual_investment_selected = individual_investment_selected)
#   
#   # fn: the fitness function to be minimized
#   # varNo: Number of decision variables
#   # objDim: Number of objective functions
#   # lowerBounds: Lower bounds of each decision variable
#   # upperBounds: Upper bounds of each decision variable
#   # popSize: Size of solution(?) population
#   # generations: Number of generations
#   # cprob: crossover prob
#   # mprob: mutation prob
#   
#   coefficients_x = matrix(data = 0, ncol = n_community, nrow = n_sunny_hours, byrow = T)
#   coefficients_x_row = table(c(1:n_community))
#   n_size = 25
#   
#   for (i in 1:n_sunny_hours) {
#     coefficients_x_row[] = 0
#     # this will be generated by the GA:
#     x = sample.int(n = n_community, size = n_size, replace = T)
#     each_space = 1/n_size
#     coefficients_x_row_table = table(x)
#     coefficients_x_row[(names(coefficients_x_row) %in% names(coefficients_x_row_table))] = coefficients_x_row_table
#     coefficients_x_row = coefficients_x_row*each_space
#     coefficients_x[i,] = as.numeric(coefficients_x_row)
#   }
# 
#   # checking:
#   rowSums(coefficients_x)
#   
#   n_sunny_hours = nrow(df_cons_selected_sunny)
#   n_community = ncol(df_cons_selected_sunny)
#   
#   coefficients_x = matrix(data = x, ncol = n_community, nrow = n_sunny_hours, byrow = T)
#   coefficients_x = coefficients_x/rowSums(coefficients_x)
#   
#   df_gen_assigned = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients = coefficients_x)
#   
#   surplus_x <- df_gen_assigned - df_cons_selected_sunny
#   surplus_x[surplus_x < 0] = 0
#   
#   # TODO:
#   f1_surplus = sum(surplus_x)
#   
#   # changed this:
#   # purchase_price = 0.14859
#   # purchase_price = c(0.14859, 0.14859, 0.14859, 
#   #                    0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 
#   #                    0.14859*1.2, 0.14859*1.2, 0.14859*1.2, 0.14859*1.2,
#   #                    0.14859*1.5) 
#   sale_price = 0.0508
#   
#   cost_old = colSums(purchase_price_sunny*df_cons_selected_sunny)
#   
#   grid_x = df_cons_selected_sunny - df_gen_assigned
#   grid_x[grid_x < 0] = 0
#   
#   surplus_x_to_sell = ifelse(colSums(surplus_x) < colSums(grid_x), colSums(surplus_x), colSums(grid_x))
#   
#   # changed this: 
#   # cost_sun = purchase_price*colSums(grid_x) - sale_price * surplus_x_to_sell
#   cost_sun = colSums(purchase_price_sunny*grid_x) - sale_price * surplus_x_to_sell
#   
#   # assuming period is a DAY
#   profit_period = cost_old - cost_sun
#   profit_one_year = profit_period * 360 
#   
#   
#   ##### TODO PAYBACK
#   payback_years = individual_investment_selected / profit_one_year 
#   
#   # TODO:
#   payback_years[is.na(payback_years)] = 10
#   payback_years[payback_years > 50] = 10
#   
#   payback_ideal = 0
#   # TODO:
#   f2_payback = sum(exp(payback_years - payback_ideal))
#   
#   # f2_payback = sd(payback_years)
#   
#   # TODO: add something like this
#   # cost_payback_2 = max(payback_years) - min(payback_years) 
#   
#   # score <- weight_surplus * cost_surplus + (1-weight_surplus) * cost_payback
#   
#   
#   # return(c(-f1_surplus, -f2_payback))
#   # took the minus sign because the optimization algo is set to minimize:
#   return(c(f1_surplus, f2_payback))
# }


fitness_1_betas_boxes <- function(x, n_community, n_binary_rep_box, list_box_season, df_gen, df_cons){
  
  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep_box*n_community)
  # x = rep(1, n_binary_rep_box*n_community)
  # x = c(ifelse(pracma::rand(n = 1, m = n_binary_rep_box*n_community) > 0.5, 1, 0))
  
  combination = calculate_combination_for_GA_binary_boxes(x = x, n_community = n_community, n_binary_rep = n_binary_rep_box, list_box_season = list_box_season, df_cons = df_cons)
  
  surplus = sum(calculate_surplus_hourly_community(combination = combination, df_gen = df_gen, df_cons = df_cons))
  score <- surplus
  
  return(-score)
}


calculate_combination_for_GA_binary_boxes <- function(x, n_community, n_binary_rep_box, list_box_season, df_cons){
  
  combination = rep(0, ncol(df_cons))
  
  # TODO: change this so it doesnt have to check inside the boxes:
  for (j in 1:n_community) {
    # j = 1
    binary_user_inside_box = GA::gray2binary(x[((j-1)*n_binary_rep_box + 1):(j*n_binary_rep_box)])   
    user_inside_box = GA::binary2decimal(binary_user_inside_box) + 1
    user = list_box_season[[j]][user_inside_box] 
    user_number = as.numeric(gsub(x = user, pattern = "cons_", replacement = ""))
    combination[user_number] = 1
  }
  return(combination)
}
