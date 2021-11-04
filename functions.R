############################# data reading #############################

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


import_data_genome_project <- function(selected_year_consumption){
  
  filename = "data/electricity_cleaned.csv"
  # filename = "/home/florencia/Nextcloud/Flor/proyects/EKATE/building-data-genome-project-2/data/meters/cleaned/electricity_cleaned.csv"
  meter = read.csv(file = filename, header = TRUE)
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


optimization_1_to_analize <- function(n_community, n_binary_rep, df_gen_to_optimize, df_cons_to_optimize, max_iter){
  
  # dim_search_ga = calculate_combinatorics(n = ncol(df_cons_to_optimize), m = n_community) * factorial(n_community)
  # dim_search_solution = calculate_combinatorics(n = ncol(df_cons_to_optimize), m = n_community) * factorial(n_community)
  
  dim_search_ga = 600
  
  optim_results <- ga(type = "binary", fitness = fitness_1_betas, 
                      nBits = n_binary_rep*n_community,
                      n_community = n_community, df_gen_to_optimize = df_gen_to_optimize, df_cons_to_optimize = df_cons_to_optimize, n_binary_rep = n_binary_rep,  
                      # popSize = 100, maxiter = 1000, run = 100)
                      popSize = dim_search_ga, 
                      # maxiter = 400, run = 50,
                      maxiter = max_iter,
                      crossover = purrr::partial(bee_uCrossover_binary, n_binary_rep = n_binary_rep, n_community = n_community), 
                      # keepBest = T,
                      pmutation = 0.7
  )
  
  solutions = optim_results@solution
  combinations = t(apply(X = as.matrix(solutions), MARGIN = 1, FUN = calculate_combination_for_GA_binary, n_community = n_community, n_binary_rep = n_binary_rep, df_cons = df_cons_to_optimize))
  combinations = combinations[!duplicated(combinations), ]
  
  return(combinations)
}


optimization_1_to_analize_convergence <- function(n_community, n_binary_rep, df_gen_to_optimize, df_cons_to_optimize, max_run){
  
  dim_search_ga = calculate_combinatorics(n = ncol(df_cons_to_optimize), m = n_community) * factorial(n_community)
  dim_search_solution = calculate_combinatorics(n = ncol(df_cons_to_optimize), m = n_community) * factorial(n_community)
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


############################# fitness #############################


fitness_1_betas <- function(x, n_community, n_binary_rep, df_gen_to_optimize, df_cons_to_optimize){
  
  # tests -> lower number, bigger number:
  # x = rep(0, n_binary_rep*n_community)
  # x = rep(1, n_binary_rep*n_community)
  # x = x_solution[1, ]
  
  combination = calculate_combination_for_GA_binary(x, n_community = n_community, n_binary_rep = n_binary_rep, df_cons = df_cons_to_optimize)
  surplus = sum(calculate_surplus_hourly_community(combination = combination, df_gen = df_gen_to_optimize, df_cons = df_cons_to_optimize))
  
  # surplus = sum(calculate_surplus_hourly_community(combination = combination, df_gen = df_gen_to_optimize, df_cons = df_cons_to_optimize))
  score <- surplus
  
  # surplus = colSums(apply(X = as.matrix(combinations), MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_to_optimize, df_cons = df_cons_to_optimize))
  
  return(-score)
}


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


# TODO: change by purchase_price_sunny everywhere!
fitness_MO <- function(x, df_gen_sunny, df_cons_selected_sunny, purchase_price_sunny, individual_investment_selected){
  
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
  # profit_period = cost_old - cost_sun
  
  # TODO: is this changing something?
  profit_period = ifelse(cost_old - cost_sun > 0, cost_old - cost_sun, 0)
  
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


plot_initial <- function(name, df){
  
  df_plot <- df 
  colnames(df_plot)[1] <- "PV_generation" 
  colnames(df_plot)[2:(ncol(df)-1)] = 1:(ncol(df)-2)
  
  df_plot <- melt(data = df_plot, id.vars = "time", variable.name = "series")
  # df_sum <- data.frame("time" = df$time,
  #                      "consumption_sum" = rowSums(df[, c(2, 3, 4, 6)]))
  p <- ggplot() + 
    geom_line(aes(df_plot$time, df_plot$value , color = df_plot$series)) +
    # geom_area(aes(x = df_sum$time, y = df_sum$consumption_sum), alpha = 0.5) +
    labs(x = "Time [h]", y = "Electrical energy [kWh]", "title" = "Electrical Generation and Consumption", color = "")  
  ggsave(filename = paste0("graphs/initial_",name) , plot = p, device = "pdf", width = 6, height = 3)
  
  return(p)
}


plot_generation <- function(df_generation){
  
  df_plot <- df_generation 
  
  p <- ggplot() + 
    geom_line(aes(x = df_plot$time, y = df_plot$energy)) +
    labs(x = "Time [h]", y = "Electrical energy [kWh]", "title" = "Electrical Generation and Consumption", color = "")  
  # ggsave(filename = paste0("graphs/initial_",name) , plot = p, device = "pdf", width = 6, height = 3)
  
  return(p)
}


plot_best_combination <- function(best_combination, iteration){
  
  optimum_coefficients = as.numeric(best_combination$optimum_coefficients)
  p <- ggplot() +
    geom_bar(aes(x = 1:length(optimum_coefficients), y = optimum_coefficients), alpha = 0.5, width = 1, stat = "identity") +
    scale_x_continuous(breaks = 1:length(optimum_coefficients)) 
  ggsave(filename = paste0("graphs/optimum_coefficients_iteration",iteration), plot = p, device = "pdf", width = 6, height = 3)
  
  optimum_payback = as.numeric(best_combination$payback)
  p <- ggplot() +
    geom_bar(aes(x = 1:length(optimum_payback), y = optimum_payback), alpha = 0.5, width = 1, stat = "identity") +
    scale_x_continuous(breaks = 1:length(optimum_payback)) 
  ggsave(filename = paste0("graphs/optimum_payback_iteration",iteration), plot = p, device = "pdf", width = 6, height = 3)
}


plot_solar_consumption_daily_mean_betas <- function(name, df_gen, df_gen_assigned, df_cons_selected_users, df_local_time){
  
  m = unique(month(df_local_time$time))
  
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned, df_cons_selected_users)
  
  df_gen$hour = df_local_time$hour
  df_solar_consumption$hour = df_local_time$hour[df_local_time$sunny]
  
  df_gen_mean = aggregate(x = df_gen, by = list(df_gen$hour), FUN = mean)
  df_solar_consumption_mean = aggregate(x = df_solar_consumption, by = list(df_solar_consumption$hour), FUN = mean)
  
  df_plot_solar_consumption_mean <- melt(data = df_solar_consumption_mean[, -grep(pattern = "Group.1", x = colnames(df_solar_consumption_mean))], variable.name = "series", id.vars = "hour")
  
  p <- ggplot() +
    geom_line(aes(x = df_gen_mean[, "hour"], y = df_gen_mean[, "gen_1"])) +
    geom_area(aes(x = df_plot_solar_consumption_mean[, "hour"], y = df_plot_solar_consumption_mean[, "value"], fill = df_plot_solar_consumption_mean[,"series"]), alpha = 0.5) +
    labs(x = "Time [h]", y = "PV generation [kWh]", "title" = paste0("PV assignation for month ",m), fill = "User")  
  
  ggsave(filename = paste0("graphs/solar_assignation_",name), plot = p, device = "pdf", width = 5, height = 3)
  
  return()
}


plot_disaggregated_daily_mean_per_user_betas <- function(df_gen_assigned, df_cons_selected_users, df_local_time){
  
  
  df_cons_selected_users_sunny = df_cons_selected_users[df_local_time$sunny, ]
  colnames(df_gen_assigned) = colnames(df_cons_selected_users)
  
  
  # calculate solar consumption and surplus
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned, df_cons_selected_users_sunny)
  
  solar_surplus <- df_gen_assigned - df_cons_selected_users_sunny
  solar_surplus[solar_surplus < 0] = 0
  
  grid = df_cons_selected_users_sunny - df_gen_assigned
  grid[grid < 0] = 0
  
  # add hour column
  df_solar_consumption$hour = df_local_time$hour[df_local_time$sunny]
  solar_surplus$hour = df_local_time$hour[df_local_time$sunny]
  grid$hour = df_local_time$hour[df_local_time$sunny]
  df_cons_selected_users$hour = df_local_time$hour
  
  grid = rbind(grid, df_cons_selected_users[!df_cons_selected_users$hour %in% grid$hour,])
  
  df_aux = df_cons_selected_users[!df_cons_selected_users$hour %in% df_solar_consumption$hour,] 
  df_aux[,-ncol(df_aux)] = 0
  
  df_solar_consumption = rbind(df_solar_consumption, df_aux)
  solar_surplus = rbind(solar_surplus, df_aux)
  
  # aggregate
  solar_consumption_mean = aggregate(x = df_solar_consumption, by = list(df_solar_consumption$hour), FUN = mean)
  solar_surplus_mean = aggregate(x = solar_surplus, by = list(solar_surplus$hour), FUN = mean) 
  grid_mean = aggregate(x = grid, by = list(grid$hour), FUN = mean) 
  
  # to calculate the self consumption and surplus 
  df_cons_selected_users$hour = df_local_time$hour
  df_cons_selected_mean = aggregate(x = df_cons_selected_users, by = list(df_cons_selected_users$hour), FUN = mean) 
  
  df_gen_assigned$hour = df_local_time$hour[df_local_time$sunny]
  df_gen_assigned_mean = aggregate(x = df_gen_assigned, by = list(df_gen_assigned$hour), FUN = mean)
  
  plots_list = list()
  
  for (user in 1:(ncol(df_cons_selected_users)-1)) {
    
    solar_consumption_mean_user <- solar_consumption_mean[, c(1+user)]
    grid_mean_user <- grid_mean[, c(1+user)]
    solar_surplus_mean_user <- solar_surplus_mean[, c(1+user)]
    
    df_plot <- data.frame("hour" = df_local_time$hour,
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


plot_disaggregated_daily_mean_community_betas <- function(name, df_gen_assigned, df_cons_selected_users, df_local_time){
  
  df_cons_selected_users_sunny = df_cons_selected_users[df_local_time$sunny, ]
  colnames(df_gen_assigned) = colnames(df_cons_selected_users)
  
  
  # calculate solar consumption and surplus
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned = df_gen_assigned, df_cons_selected = df_cons_selected_users_sunny)
  
  solar_surplus <- df_gen_assigned - df_cons_selected_users_sunny
  solar_surplus[solar_surplus < 0] = 0
  
  grid = df_cons_selected_users_sunny - df_gen_assigned
  grid[grid < 0] = 0
  
  # add hour column
  df_solar_consumption$hour = df_local_time$hour[df_local_time$sunny]
  solar_surplus$hour = df_local_time$hour[df_local_time$sunny]
  grid$hour = df_local_time$hour[df_local_time$sunny]
  df_cons_selected_users$hour = df_local_time$hour
  
  grid = rbind(grid, df_cons_selected_users[!df_cons_selected_users$hour %in% grid$hour,])
  
  df_aux = df_cons_selected_users[!df_cons_selected_users$hour %in% df_solar_consumption$hour,] 
  df_aux[,-ncol(df_aux)] = 0
  
  df_solar_consumption = rbind(df_solar_consumption, df_aux)
  solar_surplus = rbind(solar_surplus, df_aux)
  
  # aggregate
  solar_consumption_mean = aggregate(x = df_solar_consumption, by = list(df_solar_consumption$hour), FUN = mean)
  solar_surplus_mean = aggregate(x = solar_surplus, by = list(solar_surplus$hour), FUN = mean) 
  grid_mean = aggregate(x = grid, by = list(grid$hour), FUN = mean) 
  
  # to calculate the self consumption and surplus 
  df_cons_selected_users$hour = df_local_time$hour
  df_cons_selected_mean = aggregate(x = df_cons_selected_users, by = list(df_cons_selected_users$hour), FUN = mean) 
  
  n_community = ncol(df_gen_assigned)
  
  df_gen_assigned$hour = df_local_time$hour[df_local_time$sunny]
  df_gen_assigned_mean = aggregate(x = df_gen_assigned, by = list(df_gen_assigned$hour), FUN = mean)
  
  ##
  
  solar_consumption_mean_community <- rowSums(solar_consumption_mean[, 2:(n_community+1)])
  grid_mean_community <- rowSums(grid_mean[, 2:(n_community+1)])  
  solar_surplus_mean_community <- rowSums(solar_surplus_mean[, 2:(n_community+1)])  
  
  df_plot <- data.frame("hour" =  df_local_time$hour,
                        "Solar_surplus" = solar_surplus_mean_community,
                        "Solar_consumption" = solar_consumption_mean_community,
                        "Grid_consumption" = grid_mean_community
  )
  
  df_plot = melt(df_plot, id.vars = "hour")
  
  # TODO: understand which of the 2 is the correct one
  # self_consumption_percentage_mean_1 = mean( df_plot[grep(x = df_plot$variable, pattern = "solar_consumption"), "value"] / df_cons_selected_mean[, user+1]) 
  # self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, user+1])
  
  # self_consumption_percentage_mean_1 = mean(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"] / rowSums(df_cons_selected_mean[, 2:(n_community+1)]))
  self_consumption_percentage_max = max(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"] / rowSums(df_cons_selected_mean[, 2:(n_community+1)]))
  self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, 2:(n_community+1)])
  
  surplus_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "surplus"), "value"]) / sum(df_gen_assigned_mean[, 2:(n_community+1)])  
  
  index_order = order(colSums(df_cons_selected_mean[, 2:(n_community+1)]))
  
  # how can I automatize this??
  if (n_community == 2) {
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 3){
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 4){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 5){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)),fill = "")  
  }else if (n_community == 6){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 7){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 8){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 9){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 10){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]] + df_cons_selected_mean[,1+index_order[10]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 11){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]] + df_cons_selected_mean[,1+index_order[10]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]] + df_cons_selected_mean[,1+index_order[10]] + df_cons_selected_mean[,1+index_order[11]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else{ 
    print("n_community != 2:9")
  }
  ggsave(filename = paste0("graphs/community_",name), plot = p, device = "pdf", width = 5, height = 3)
  return()
}


plot_disaggregated_daily_mean_community_betas_yearly_weekdays <- function(name, df_gen_assigned, df_cons_selected_users, df_local_time){
  
  df_cons_selected_users_sunny = df_cons_selected_users[df_local_time$sunny, ]
  colnames(df_gen_assigned) = colnames(df_cons_selected_users)
  
  
  # calculate solar consumption and surplus
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned = df_gen_assigned, df_cons_selected = df_cons_selected_users_sunny)
  
  solar_surplus <- df_gen_assigned - df_cons_selected_users_sunny
  solar_surplus[solar_surplus < 0] = 0
  
  grid = df_cons_selected_users_sunny - df_gen_assigned
  grid[grid < 0] = 0
  
  # # add hour column
  # df_solar_consumption$hour = df_local_time$hour[df_local_time$sunny]
  # solar_surplus$hour = df_local_time$hour[df_local_time$sunny]
  # grid$hour = df_local_time$hour[df_local_time$sunny]
  # df_cons_selected_users$hour = df_local_time$hour
  
  grid = rbind(grid, df_cons_selected_users[!df_cons_selected_users$hour %in% grid$hour,])
  
  df_aux = df_cons_selected_users[!df_cons_selected_users$hour %in% df_solar_consumption$hour,] 
  df_aux[,-ncol(df_aux)] = 0
  
  df_solar_consumption = rbind(df_solar_consumption, df_aux)
  solar_surplus = rbind(solar_surplus, df_aux)
  
  # aggregate
  # solar_consumption_mean = aggregate(x = df_solar_consumption, by = list(df_solar_consumption$hour), FUN = mean)
  # solar_surplus_mean = aggregate(x = solar_surplus, by = list(solar_surplus$hour), FUN = mean) 
  # grid_mean = aggregate(x = grid, by = list(grid$hour), FUN = mean) 
  
  # to calculate the self consumption and surplus 
  # df_cons_selected_users$hour = df_local_time$hour
  # df_cons_selected_mean = aggregate(x = df_cons_selected_users, by = list(df_cons_selected_users$hour), FUN = mean) 
  
  n_community = ncol(df_gen_assigned)
  
  # df_gen_assigned$hour = df_local_time$hour[df_local_time$sunny]
  # df_gen_assigned_mean = aggregate(x = df_gen_assigned, by = list(df_gen_assigned$hour), FUN = mean)
  
  ##
  
  solar_consumption_mean_community <- rowSums(solar_consumption_mean[, 2:(n_community+1)])
  grid_mean_community <- rowSums(grid_mean[, 2:(n_community+1)])  
  solar_surplus_mean_community <- rowSums(solar_surplus_mean[, 2:(n_community+1)])  
  
  df_plot <- data.frame("hour" =  df_local_time$hour,
                        "Solar_surplus" = solar_surplus_mean_community,
                        "Solar_consumption" = solar_consumption_mean_community,
                        "Grid_consumption" = grid_mean_community
  )
  
  df_plot = melt(df_plot, id.vars = "hour")
  
  # TODO: understand which of the 2 is the correct one
  # self_consumption_percentage_mean_1 = mean( df_plot[grep(x = df_plot$variable, pattern = "solar_consumption"), "value"] / df_cons_selected_mean[, user+1]) 
  # self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, user+1])
  
  # self_consumption_percentage_mean_1 = mean(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"] / rowSums(df_cons_selected_mean[, 2:(n_community+1)]))
  self_consumption_percentage_max = max(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"] / rowSums(df_cons_selected_mean[, 2:(n_community+1)]))
  self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, 2:(n_community+1)])
  
  surplus_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "surplus"), "value"]) / sum(df_gen_assigned_mean[, 2:(n_community+1)])  
  
  index_order = order(colSums(df_cons_selected_mean[, 2:(n_community+1)]))
  
  # how can I automatize this??
  if (n_community == 2) {
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 3){
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 4){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 5){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)),fill = "")  
  }else if (n_community == 6){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 7){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 8){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 9){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 10){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]] + df_cons_selected_mean[,1+index_order[10]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 11){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]] + df_cons_selected_mean[,1+index_order[10]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]] + df_cons_selected_mean[,1+index_order[7]] + df_cons_selected_mean[,1+index_order[8]] + df_cons_selected_mean[,1+index_order[9]] + df_cons_selected_mean[,1+index_order[10]] + df_cons_selected_mean[,1+index_order[11]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else{ 
    print("n_community != 2:9")
  }
  ggsave(filename = paste0("graphs/community_",name), plot = p, device = "pdf", width = 5, height = 3)
  return()
}


# plot_economic_comparison_betas <- function(df_gen, df_gen_assigned, df_cons_selected_users, matrix_coefficients = best_combination$optimum_coefficients, df_local_time){
#   
#   df_cons_selected_users_sunny = df_cons_selected_users[df_local_time$sunny, ]
#   colnames(df_gen_assigned) = colnames(df_cons_selected_users)
# 
#   # calculate solar consumption and surplus
#   # df_solar_consumption = calculate_solar_consumption(df_gen_assigned, df_cons_selected_users_sunny)
#   
#   solar_surplus <- df_gen_assigned - df_cons_selected_users_sunny
#   solar_surplus[solar_surplus < 0] = 0
#   
#   grid = df_cons_selected_users_sunny - df_gen_assigned
#   grid[grid < 0] = 0
#   
#   # add hour column
#   # df_solar_consumption$hour = df_local_time$hour[df_local_time$sunny]
#   # solar_surplus$hour = df_local_time$hour[df_local_time$sunny]
#   grid$hour = df_local_time$hour[df_local_time$sunny]
#   df_cons_selected_users$hour = df_local_time$hour
#   # 
#   grid = rbind(grid, df_cons_selected_users[!df_cons_selected_users$hour %in% grid$hour,])
#   # 
#   # df_aux = df_cons_selected_users[!df_cons_selected_users$hour %in% df_solar_consumption$hour,] 
#   # df_aux[,-ncol(df_aux)] = 0
#   # 
#   # # df_solar_consumption = rbind(df_solar_consumption, df_aux)
#   # solar_surplus = rbind(solar_surplus, df_aux)
#   
#   n_community = ncol(df_gen_assigned)
#   
#   ###
#   
#   # n_community = length(optimum_combination)
#   matrix_coefficients_non_optimum = matrix_coefficients
#   matrix_coefficients_non_optimum[,] = 1/n_community
#     
#   df_gen_assigned_non_optimum = calculate_gen_assigned_betas(df_gen_day = df_gen[df_local_time$sunny,], matrix_coefficients = matrix_coefficients_non_optimum)
#   colnames(df_gen_assigned_non_optimum) = colnames(df_gen_assigned)
# 
#   # calculate solar consumption and surplus
#   # df_solar_consumption_non_optimum = calculate_solar_consumption(df_gen_assigned_non_optimum, df_cons_selected_users_sunny)
#   
#   # colnames(df_solar_consumption_non_optimum) = colnames(df_solar_consumption)
#   
#   solar_surplus_non_optimum <- df_gen_assigned_non_optimum - df_cons_selected_users_sunny
#   solar_surplus_non_optimum[solar_surplus_non_optimum < 0] = 0
#   
#   grid_non_optimum = df_cons_selected_users_sunny - df_gen_assigned_non_optimum
#   grid_non_optimum[grid_non_optimum < 0] = 0
#   
#   # add hour column
#   # df_solar_consumption_non_optimum$hour = df_local_time$hour[df_local_time$sunny]
#   # solar_surplus_non_optimum$hour = df_local_time$hour[df_local_time$sunny]
#   grid_non_optimum$hour = df_local_time$hour[df_local_time$sunny]
#   # 
#   grid_non_optimum = rbind(grid_non_optimum, df_cons_selected_users[!df_cons_selected_users$hour %in% grid_non_optimum$hour,])
#   
#   # df_solar_consumption_non_optimum = rbind(df_solar_consumption_non_optimum, df_aux)
#   # solar_surplus_non_optimum = rbind(solar_surplus_non_optimum, df_aux)
# 
#   # solar_consumption_non_optimum = calculate_solar_consumption(df_gen_assigned_selected, df_cons_selected)
#   # solar_surplus_non_optimum <- df_gen_assigned_selected - df_cons_selected
#   # solar_surplus_non_optimum[solar_surplus_non_optimum < 0] = 0
#   # grid_non_optimum = df_cons_selected - df_gen_assigned_selected_non_optimum
#   # grid_non_optimum[grid_non_optimum < 0] = 0
# 
#   ###
#   df_cons_selected_users = df_cons_selected_users[, -ncol(df_cons_selected_users)]
#   grid = grid[, -ncol(grid)]
#   grid_non_optimum = grid_non_optimum[, -ncol(grid_non_optimum)]
#   
#   solar_surplus_to_sell = ifelse(colSums(solar_surplus) < colSums(grid), colSums(solar_surplus), colSums(grid))
#   solar_surplus_to_sell_non_optimum = ifelse(colSums(solar_surplus_non_optimum) < colSums(grid_non_optimum), colSums(solar_surplus_non_optimum), colSums(grid_non_optimum))
# 
#   purchase_price = 0.14859
#   sale_price = 0.0508
# 
#   cost_old = colSums(purchase_price*df_cons_selected_users)
# 
#   cost_sun = purchase_price*colSums(grid) - sale_price * solar_surplus_to_sell
#   cost_sun_non_optimum = purchase_price*colSums(grid_non_optimum) - sale_price * solar_surplus_to_sell_non_optimum
#     
#   cost_old_one_year = cost_old * 360 
#   cost_sun_one_year = cost_sun * 360 
#   cost_sun_non_optimum_one_year = cost_sun_non_optimum * 360 
#   
#   cost_old_20_years = cost_old_one_year * 20
#   cost_sun_20_years = cost_sun_one_year * 20
#   cost_sun_non_optimum_20_years = cost_sun_non_optimum_one_year * 20
# 
#   # bar graph: what would you have paid in the following 20 years?
#   # .with the optimum community
#   # .with the non optimum community
#   # .without the community
#   
#   costs_comparison = as.data.frame(rbind(cost_old_20_years, cost_sun_20_years, cost_sun_non_optimum_20_years))
#   costs_comparison$names = rownames(costs_comparison)
#   costs_comparison = melt(data = costs_comparison, id.vars = "names") 
#        
#   p <- ggplot() +
#     geom_bar(aes(x = costs_comparison$variable,  y = costs_comparison$value, fill = costs_comparison$names), alpha = 0.5, width = 1, stat = "identity", position=position_dodge()) 
#     # geom_bar(aes(x = 1:nrow(costs_comparison), y = costs_comparison$cost_sun_20_years), alpha = 0.5, width = 1, stat = "identity") +
#     # geom_bar(aes(x = 1:nrow(costs_comparison), y = costs_comparison$cost_sun_non_optimum_one_year), alpha = 0.5, width = 1, stat = "identity") +
#     # scale_x_continuous(breaks = 1:nrow(costs_comparison)) 
#   ggsave(filename = paste0("graphs/costs_comparison"), plot = p, device = "pdf", width = 8, height = 3)
# }


plot_optimization1_vs_optimization2 <- function(optimal_combination_using_2_GAs){
  
  new_optimum_coefficients = optimal_combination_using_2_GAs$new_optimum_coefficients
  new_surplus = optimal_combination_using_2_GAs$new_surplus
  new_payback = optimal_combination_using_2_GAs$new_payback
  
  index = order(new_surplus, decreasing = F)[order(new_surplus, decreasing = F) %in% which(new_surplus < 1000000)]
  # index = order(new_surplus, decreasing = F)[order(new_surplus, decreasing = F)]
  
  new_surplus = new_surplus[index]
  new_payback = new_payback[index, ]
  
  payback_ideal = 4
  cost_payback = rowSums(exp(new_payback - payback_ideal))
  
  # TODO: study carefully this graph, does this make sense?
  p <- ggplot() + geom_point(aes(x = 1:length(new_surplus), y = new_surplus))
  ggsave(filename = "graphs/new_surplus.pdf", plot = p, device = "pdf", width = 5, height = 3)
  p <- ggplot() + geom_point(aes(x = 1:length(cost_payback), y = cost_payback))
  ggsave(filename = "graphs/new_payback.pdf", plot = p, device = "pdf", width = 5, height = 3)
  # p <- ggplot() + geom_point(aes(x = cost_payback, y = new_surplus))
  p <- ggplot() + geom_point(aes(x = new_surplus, y = cost_payback))
  ggsave(filename = "graphs/new_surplus_vs_payback.pdf", plot = p, device = "pdf", width = 5, height = 3)
  
  pre_optimum_coefficients = optimal_combination_using_2_GAs$pre_optimum_coefficients
  pre_surplus = optimal_combination_using_2_GAs$pre_surplus
  pre_payback = optimal_combination_using_2_GAs$pre_payback
  
  index_pre = order(pre_surplus, decreasing = F)[order(pre_surplus, decreasing = F) %in% which(pre_surplus < 1000000)]
  # index_pre = order(pre_surplus, decreasing = F)[order(pre_surplus, decreasing = F)]
  
  pre_surplus = pre_surplus[index_pre]
  pre_payback = pre_payback[index_pre, ]
  
  pre_cost_payback = rowSums(exp(pre_payback - payback_ideal))
  
  # TODO: study carefully this graph, does this make sense?
  p <- ggplot() + geom_point(aes(x = 1:length(pre_surplus), y = pre_surplus))
  ggsave(filename = "graphs/pre_surplus.pdf", plot = p, device = "pdf", width = 5, height = 3)
  p <- ggplot() + geom_point(aes(x = 1:length(pre_cost_payback), y = pre_cost_payback))
  ggsave(filename = "graphs/pre_payback.pdf", plot = p, device = "pdf", width = 5, height = 3)
  # p <- ggplot() + geom_point(aes(x = pre_cost_payback, y = pre_surplus))
  p <- ggplot() + geom_point(aes(x = pre_surplus, y = pre_cost_payback))
  ggsave(filename = "graphs/pre_surplus_vs_payback.pdf", plot = p, device = "pdf", width = 5, height = 3)
  
  pre_surplus = optimal_combination_using_2_GAs$pre_surplus
  pre_payback = optimal_combination_using_2_GAs$pre_payback
  
  pre_surplus = pre_surplus[index]
  pre_cost_payback = pre_cost_payback[index] 
  surplus_added = new_surplus - pre_surplus 
  p <- ggplot() + geom_point(aes(x = pre_surplus, y = new_surplus))
  p <- ggplot() + geom_point(aes(x = pre_surplus, y = surplus_added))
  p <- ggplot() + geom_point(aes(x = pre_cost_payback, y = surplus_added))
  p <- ggplot() + geom_point(aes(x = cost_payback, y = surplus_added))
  p <- ggplot() + geom_point(aes(x = surplus_added, y = cost_payback))
  
  p <- ggplot() + geom_point(aes(x = pre_payback, y = cost_payback))
  
  return()
}


plot_comparison_coefficients <- function(df_gen, df_gen_sunny, df_cons_selected_users_sunny, df_cons_selected_users, matrix_coefficients_1 , matrix_coefficients_2, df_local_time){
  
  # df_gen_sunny = df_gen[df_local_time$sunny, ]
  # df_cons_selected_users_sunny = df_cons_selected_users[df_local_time$sunny, ]
  
  df_gen_assigned_1 = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients_1)
  colnames(df_gen_assigned_1) = colnames(df_cons_selected_users)
  
  solar_surplus_1 <- df_gen_assigned_1 - df_cons_selected_users_sunny
  solar_surplus_1[solar_surplus_1 < 0] = 0
  
  grid_1 = df_cons_selected_users_sunny - df_gen_assigned_1
  grid_1[grid_1 < 0] = 0
  
  ###
  
  df_gen_assigned_2 = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients_2)
  colnames(df_gen_assigned_2) = colnames(df_cons_selected_users)
  
  solar_surplus_2 <- df_gen_assigned_2 - df_cons_selected_users_sunny
  solar_surplus_2[solar_surplus_2 < 0] = 0
  
  grid_2 = df_cons_selected_users_sunny - df_gen_assigned_2
  grid_2[grid_2 < 0] = 0
  
  ###
  
  df_cons_selected_users$hour = df_local_time$hour
  
  grid_1$hour = df_local_time$hour[df_local_time$sunny]
  grid_1 = rbind(grid_1, df_cons_selected_users[!df_cons_selected_users$hour %in% grid_1$hour,])
  
  grid_2$hour = df_local_time$hour[df_local_time$sunny]
  grid_2 = rbind(grid_2, df_cons_selected_users[!df_cons_selected_users$hour %in% grid_2$hour,])
  
  ###
  
  df_cons_selected_users = df_cons_selected_users[, -ncol(df_cons_selected_users)]
  grid_1 = grid_1[, -ncol(grid_1)]
  grid_2 = grid_2[, -ncol(grid_2)]
  
  solar_surplus_to_sell_1 = ifelse(colSums(solar_surplus_1) < colSums(grid_1), colSums(solar_surplus_1), colSums(grid_1))
  solar_surplus_to_sell_2 = ifelse(colSums(solar_surplus_2) < colSums(grid_2), colSums(solar_surplus_2), colSums(grid_2))
  
  purchase_price = 0.14859
  sale_price = 0.0508
  
  cost_old = colSums(purchase_price*df_cons_selected_users)
  
  cost_sun_1 = purchase_price*colSums(grid_1) - sale_price * solar_surplus_to_sell_1
  cost_sun_2 = purchase_price*colSums(grid_2) - sale_price * solar_surplus_to_sell_2
  
  cost_old_one_year = cost_old * 360 
  cost_sun_1_one_year = cost_sun_1 * 360 
  cost_sun_2_one_year = cost_sun_2 * 360 
  
  cost_old_20_years = cost_old_one_year * 20
  cost_sun_1_20_years = cost_sun_1_one_year * 20
  cost_sun_2_20_years = cost_sun_2_one_year * 20
  
  # bar graph: what would you have paid in the following 20 years?
  # .with the optimum community
  # .with the non optimum community
  # .without the community
  
  costs_comparison = as.data.frame(rbind(cost_old_20_years, cost_sun_1_20_years, cost_sun_2_20_years))
  costs_comparison$names = rownames(costs_comparison)
  costs_comparison = melt(data = costs_comparison, id.vars = "names") 
  
  p <- ggplot() +
    geom_bar(aes(x = costs_comparison$variable,  y = costs_comparison$value, fill = costs_comparison$names), alpha = 0.5, width = 1, stat = "identity", position=position_dodge()) 
  ggsave(filename = paste0("graphs/costs_comparison"), plot = p, device = "pdf", width = 8, height = 3)
  
  solar_surplus_1 = colSums(solar_surplus_1)
  solar_surplus_2 = colSums(solar_surplus_2)
  solar_surplus_old = solar_surplus_2
  solar_surplus_old[] = 0
  
  surplus_comparison = as.data.frame(rbind(solar_surplus_old, solar_surplus_1, solar_surplus_2))
  surplus_comparison$names = rownames(surplus_comparison)
  surplus_comparison = melt(data = surplus_comparison, id.vars = "names") 
  
  p <- ggplot() +
    geom_bar(aes(x = surplus_comparison$variable,  y = surplus_comparison$value, fill = surplus_comparison$names), alpha = 0.5, width = 1, stat = "identity", position=position_dodge()) 
  ggsave(filename = paste0("graphs/surplus_comparison"), plot = p, device = "pdf", width = 8, height = 3)
}


plot_comparison_coefficients_upgraded <- function(df_gen, df_gen_sunny, df_cons_selected, df_cons_selected_sunny, matrix_coefficients_list, df_local_time, individual_investment_selected){
  
  purchase_price = 0.14859
  sale_price = 0.0508
  
  df_costs_comparison = df_cons_selected 
  df_costs_comparison[,] = 0
  df_costs_comparison = df_costs_comparison[1:length(matrix_coefficients_list),]
  df_costs_comparison$i_matrix = 0
  
  df_solar_surplus_comparison = df_cons_selected 
  df_solar_surplus_comparison[,] = 0
  df_solar_surplus_comparison = df_solar_surplus_comparison[1:length(matrix_coefficients_list),]
  df_solar_surplus_comparison$i_matrix = 0
  
  df_payback_years_comparison = df_cons_selected
  df_payback_years_comparison[,] = 0
  df_payback_years_comparison = df_payback_years_comparison[1:length(matrix_coefficients_list),]
  df_payback_years_comparison$i_matrix = 0
  
  cost_old = colSums(purchase_price*df_cons_selected)
  cost_old_one_year = cost_old * 360
  
  for (i in 1:length(matrix_coefficients_list)) {
    
    matrix_coefficients = matrix_coefficients_list[[i]]
    
    df_gen_assigned = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients)
    colnames(df_gen_assigned) = colnames(df_cons_selected)
    
    solar_surplus <- df_gen_assigned - df_cons_selected_sunny
    solar_surplus[solar_surplus < 0] = 0
    
    grid = df_cons_selected_sunny - df_gen_assigned
    grid[grid < 0] = 0
    
    df_cons_selected$hour = df_local_time$hour
    
    grid$hour = df_local_time$hour[df_local_time$sunny]
    grid = rbind(grid, df_cons_selected[!df_cons_selected$hour %in% grid$hour,])
    
    df_cons_selected = df_cons_selected[, -ncol(df_cons_selected)]
    grid = grid[, -ncol(grid)]
    
    solar_surplus_to_sell = ifelse(colSums(solar_surplus) < colSums(grid), colSums(solar_surplus), colSums(grid))
    
    cost_sun = purchase_price*colSums(grid) - sale_price * solar_surplus_to_sell
    cost_sun_one_year = cost_sun * 360 
    cost_sun_20_years = cost_sun_one_year * 20
    
    df_costs_comparison[i, ] = c(cost_sun_20_years, i) 
    df_solar_surplus_comparison[i, ] = c(colSums(solar_surplus), i)
    
    profit_period = cost_old - cost_sun
    profit_one_year = profit_period * 360 
    
    payback_years_comparison = individual_investment_selected / profit_one_year
    df_payback_years_comparison[i, ] = c(payback_years_comparison, i)
    # TODO: for the case where there is no PV installation (coeff = 0) if I keep payback = 100000 as in the cost function then the plot only shows this value
    df_payback_years_comparison[i, df_payback_years_comparison[i, ] > 10**13] = NA 
  }
  
  costs_comparison = melt(data = df_costs_comparison, id.vars = "i_matrix") 
  costs_comparison$i_matrix = factor(costs_comparison$i_matrix)
  
  p <- ggplot() +
    geom_bar(aes(x = costs_comparison$variable,  y = costs_comparison$value, fill = costs_comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/costs_comparison_disaggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  
  surplus_comparison = melt(data = df_solar_surplus_comparison, id.vars = "i_matrix") 
  surplus_comparison$i_matrix = factor(df_solar_surplus_comparison$i_matrix)
  
  p <- ggplot() +
    geom_bar(aes(x = surplus_comparison$variable,  y = surplus_comparison$value, fill = surplus_comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/surplus_comparison_disaggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  ##
  
  costs_comparison_aggregated = data.frame("i_matrix" = df_costs_comparison$i_matrix, "value" =rowSums(df_costs_comparison[, -ncol(df_costs_comparison)]))
  costs_comparison_aggregated$i_matrix = factor(costs_comparison_aggregated$i_matrix)
  p <- ggplot() +
    geom_bar(aes(x = costs_comparison_aggregated$i_matrix, y = costs_comparison_aggregated$value, fill = costs_comparison_aggregated$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/costs_comparison_aggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  surplus_comparison_aggregated = data.frame("i_matrix" = df_solar_surplus_comparison$i_matrix, "value" =rowSums(df_solar_surplus_comparison[, -ncol(df_solar_surplus_comparison)]))
  surplus_comparison_aggregated$i_matrix = factor(surplus_comparison_aggregated$i_matrix)
  p <- ggplot() +
    geom_bar(aes(x = surplus_comparison_aggregated$i_matrix, y = surplus_comparison_aggregated$value, fill = costs_comparison_aggregated$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/surplus_comparison_aggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  ## the costs comparison aggregated is almost the same for all the scenarios
  
  investment_comparison = data.frame("i_matrix" = df_costs_comparison$i_matrix, "value" = individual_investment_selected)
  investment_comparison$i_matrix = factor(investment_comparison$i_matrix)
  p <- ggplot() +
    geom_bar(aes(x = investment_comparison$i_matrix, y = investment_comparison$value, fill = investment_comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7))
  ggsave(filename = paste0("graphs/individual_investment_comparison"), plot = p, device = "pdf", width = 8, height = 3)
  
  ##
  
  payback_comparison = melt(data = df_payback_years_comparison, id.vars = "i_matrix") 
  payback_comparison$i_matrix = factor(df_payback_years_comparison$i_matrix)
  
  p <- ggplot() +
    geom_bar(aes(x = payback_comparison$variable,  y = payback_comparison$value, fill = payback_comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/payback_comparison_disaggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  ##
  
  payback_ideal = 0
  payback_comparison_exp = data.frame("i_matrix" = df_payback_years_comparison$i_matrix, "value" = rowSums(exp(df_payback_years_comparison[, -ncol(df_payback_years_comparison)] - payback_ideal)))
  payback_comparison_exp$i_matrix = factor(payback_comparison_exp$i_matrix)
  
  p <- ggplot() +
    geom_bar(aes(x = payback_comparison_exp$i_matrix, y = payback_comparison_exp$value, fill = payback_comparison_exp$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/payback_comparison_aggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  return()
}


plot_comparison_coefficients_final <- function(df_gen, df_gen_sunny, df_cons_selected, df_cons_selected_sunny, matrix_coefficients_list, df_local_time, individual_investment_selected){
  
  purchase_price = 0.14859
  sale_price = 0.0508
  
  df_costs_comparison = df_cons_selected 
  df_costs_comparison[,] = 0
  df_costs_comparison = df_costs_comparison[1:length(matrix_coefficients_list),]
  df_costs_comparison$i_matrix = 0
  
  df_solar_surplus_comparison = df_cons_selected 
  df_solar_surplus_comparison[,] = 0
  df_solar_surplus_comparison = df_solar_surplus_comparison[1:length(matrix_coefficients_list),]
  df_solar_surplus_comparison$i_matrix = 0
  
  df_payback_years_comparison = df_cons_selected
  df_payback_years_comparison[,] = 0
  df_payback_years_comparison = df_payback_years_comparison[1:length(matrix_coefficients_list),]
  df_payback_years_comparison$i_matrix = 0
  
  cost_old = colSums(purchase_price*df_cons_selected)
  cost_old_one_year = cost_old * 360
  
  for (i in 1:length(matrix_coefficients_list)) {
    
    matrix_coefficients = matrix_coefficients_list[[i]]
    
    df_gen_assigned = calculate_gen_assigned_betas(df_gen_sunny, matrix_coefficients)
    colnames(df_gen_assigned) = colnames(df_cons_selected)
    
    solar_surplus <- df_gen_assigned - df_cons_selected_sunny
    solar_surplus[solar_surplus < 0] = 0
    
    grid = df_cons_selected_sunny - df_gen_assigned
    grid[grid < 0] = 0
    
    df_cons_selected$hour = df_local_time$hour
    
    grid$hour = df_local_time$hour[df_local_time$sunny]
    grid = rbind(grid, df_cons_selected[!df_cons_selected$hour %in% grid$hour,])
    
    df_cons_selected = df_cons_selected[, -ncol(df_cons_selected)]
    grid = grid[, -ncol(grid)]
    
    solar_surplus_to_sell = ifelse(colSums(solar_surplus) < colSums(grid), colSums(solar_surplus), colSums(grid))
    
    cost_sun = purchase_price*colSums(grid) - sale_price * solar_surplus_to_sell
    cost_sun_one_year = cost_sun * 360 
    cost_sun_20_years = cost_sun_one_year * 20
    
    df_costs_comparison[i, ] = c(cost_sun_20_years, i) 
    df_solar_surplus_comparison[i, ] = c(colSums(solar_surplus), i)
    
    profit_period = cost_old - cost_sun
    profit_one_year = profit_period * 360 
    
    payback_years_comparison = individual_investment_selected / profit_one_year
    df_payback_years_comparison[i, ] = c(payback_years_comparison, i)
    # TODO: for the case where there is no PV installation (coeff = 0) if I keep payback = 100000 as in the cost function then the plot only shows this value
    df_payback_years_comparison[i, df_payback_years_comparison[i, ] > 10**13] = NA 
  }
  
  costs_comparison = melt(data = df_costs_comparison, id.vars = "i_matrix") 
  costs_comparison$i_matrix = factor(costs_comparison$i_matrix)
  
  p <- ggplot() +
    geom_bar(aes(x = costs_comparison$variable,  y = costs_comparison$value, fill = costs_comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/costs_comparison_disaggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  
  surplus_comparison = melt(data = df_solar_surplus_comparison, id.vars = "i_matrix") 
  surplus_comparison$i_matrix = factor(df_solar_surplus_comparison$i_matrix)
  
  p <- ggplot() +
    geom_bar(aes(x = surplus_comparison$variable,  y = surplus_comparison$value, fill = surplus_comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/surplus_comparison_disaggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  ##
  
  costs_comparison_aggregated = data.frame("i_matrix" = df_costs_comparison$i_matrix, "value" =rowSums(df_costs_comparison[, -ncol(df_costs_comparison)]))
  costs_comparison_aggregated$i_matrix = factor(costs_comparison_aggregated$i_matrix)
  p <- ggplot() +
    geom_bar(aes(x = costs_comparison_aggregated$i_matrix, y = costs_comparison_aggregated$value, fill = costs_comparison_aggregated$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/costs_comparison_aggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  surplus_comparison_aggregated = data.frame("i_matrix" = df_solar_surplus_comparison$i_matrix, "value" =rowSums(df_solar_surplus_comparison[, -ncol(df_solar_surplus_comparison)]))
  surplus_comparison_aggregated$i_matrix = factor(surplus_comparison_aggregated$i_matrix)
  p <- ggplot() +
    geom_bar(aes(x = surplus_comparison_aggregated$i_matrix, y = surplus_comparison_aggregated$value, fill = costs_comparison_aggregated$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/surplus_comparison_aggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  ## the costs comparison aggregated is almost the same for all the scenarios
  
  investment_comparison = data.frame("i_matrix" = df_costs_comparison$i_matrix, "value" = individual_investment_selected)
  investment_comparison$i_matrix = factor(investment_comparison$i_matrix)
  p <- ggplot() +
    geom_bar(aes(x = investment_comparison$i_matrix, y = investment_comparison$value, fill = investment_comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7))
  ggsave(filename = paste0("graphs/individual_investment_comparison"), plot = p, device = "pdf", width = 8, height = 3)
  
  ##
  
  payback_comparison = melt(data = df_payback_years_comparison, id.vars = "i_matrix") 
  payback_comparison$i_matrix = factor(df_payback_years_comparison$i_matrix)
  
  p <- ggplot() +
    geom_bar(aes(x = payback_comparison$variable,  y = payback_comparison$value, fill = payback_comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/payback_comparison_disaggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  ##
  
  payback_ideal = 0
  payback_comparison_exp = data.frame("i_matrix" = df_payback_years_comparison$i_matrix, "value" = rowSums(exp(df_payback_years_comparison[, -ncol(df_payback_years_comparison)] - payback_ideal)))
  payback_comparison_exp$i_matrix = factor(payback_comparison_exp$i_matrix)
  
  p <- ggplot() +
    geom_bar(aes(x = payback_comparison_exp$i_matrix, y = payback_comparison_exp$value, fill = payback_comparison_exp$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
  ggsave(filename = paste0("graphs/payback_comparison_aggregated"), plot = p, device = "pdf", width = 8, height = 3)
  
  return()
}


# plot_multi_objective_criteria_selection <- function(df_pareto_objectives, z_star, x_lineal, y_lineal, objectives_with_criteria){
plot_multi_objective_criteria_selection <- function(name, df_pareto_objectives, z_star, objectives_with_criteria){
  
  r = (sum(c(objectives_with_criteria$surplus, objectives_with_criteria$payback) - z_star)**2)**0.5  
  
  theta = seq(from = 0, to = (2*pi), length.out = 100)
  x_circular = z_star$surplus + r * cos(theta)
  y_circular = z_star$payback + r * sin(theta)
  
  p = ggplot() +
    geom_point(aes(x = df_pareto_objectives$surplus, y = df_pareto_objectives$payback)) +
    geom_point(aes(x = z_star$surplus, y = z_star$payback), shape = 4) +
    # geom_line(aes(x = x_lineal, y = y_lineal)) +
    geom_point(aes(x = objectives_with_criteria$surplus, y = objectives_with_criteria$payback), shape = 5, size = 3) 
  # geom_point(aes(x = x_circular, y = y_circular))
  ggsave(filename = paste0("graphs/multi_objective_criteria_",name,".pdf"), plot = p, width = 6, height = 3)
  
  return(p)
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


plot_tariff_signal <- function(){
  
  purchase_price = c(0.14859, 0.14859, 0.14859,  0.14859, 0.14859, 0.14859, 0.14859, 0.14859,   
                     0.14859*1.2, 0.14859*1.2, 
                     0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 0.14859*1.5,
                     0.14859*1.2, 0.14859*1.2, 0.14859*1.2, 0.14859*1.2,
                     0.14859*1.5, 0.14859*1.5, 0.14859*1.5, 0.14859*1.5,
                     0.14859*1.2, 0.14859*1.2
  ) 
  
  p <- ggplot() + 
    geom_line(aes(x=0:23, y=purchase_price)) +
    labs(x="daytime", y="", title="") +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  ggsave(filename = "graphs/tariff_signal.pdf", plot = p, device = "pdf", width = 6, height = 3)
  return()
}


plot_iterations_convergence <- function(name, best_combinations){
  
  nrow(best_combinations)
  # sum(!duplicated(best_combinations))
  
  # TODO: solve this problem
  
  surplus = colSums(apply(X = as.matrix(best_combinations), MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny))
  cheating = surplus < 1000  
  surplus = surplus[cheating]
  # apply(X = names(surplus), MARGIN = 1, FUN = function(X){strsplit(X, split = "X")})
  
  p = ggplot() + 
    geom_line(aes(x = 1:length(surplus), y = log(surplus))) + 
    labs(x = "Iteration", y = "Log(surplus)")  
  
  ggsave(filename = paste0("graphs/convergence_",name), plot = p, device = "pdf", width = 6, height = 3)  
  
  return()
}


plot_table_convergence <- function(name, table_combination_ordered){
  
  p <- ggplot() +
    geom_bar(aes(x = names(table_combination_ordered), y = table_combination_ordered), alpha = 0.5, width = 0.8, stat = "identity")   
  
  ggsave(filename = paste0("graphs/table_convergence_",name), plot = p, device = "pdf", width = 6, height = 3)
  return()
}



plot_iterations_convergence_surplus <- function(name, list_best_surplus){
  
  library(reshape)
  library(zoo)
  # list_best_surplus_plot = melt(data = list_best_surplus, id.variable)
  
  list_mean = list() 
  list_min = list()
  list_max = list()
  
  for (ordering in names(list_best_surplus)) {
    df_best_surplus_plot = melt(list_best_surplus[[ordering]])
    df_best_surplus_plot$value = log(df_best_surplus_plot$value)
    # df_best_surplus_plot$value = df_best_surplus_plot$value
    df_cast <- cast(df_best_surplus_plot, variable~L1)  
    
    df_cast = na.locf(df_cast)
    
    list_mean[[ordering]] = t(as.data.frame(rowMeans(df_cast, na.rm = T)))
    colnames(list_mean[[ordering]]) = 1:length(list_mean[[ordering]])
    rownames(list_mean[[ordering]]) = NULL
    
    list_min[[ordering]] = t(as.data.frame(apply(df_cast, 1, FUN = min, na.rm = T)))
    colnames(list_min[[ordering]]) = 1:length(list_min[[ordering]])
    rownames(list_min[[ordering]]) = NULL
    
    list_max[[ordering]] = t(as.data.frame(apply(df_cast, 1, FUN = max, na.rm = T)))
    colnames(list_max[[ordering]]) = 1:length(list_max[[ordering]])
    rownames(list_max[[ordering]]) = NULL
  }
  
  list_mean = melt(list_mean)
  list_mean = list_mean[, 2:4]
  
  list_min = melt(list_min)
  list_min = list_min[, 2:4]
  
  list_max = melt(list_max)
  list_max = list_max[, 2:4]
  
  # p <- ggplot(ndata, aes(Period, fit)) + 
  #   geom_line(aes(colour=group)) + 
  #   geom_ribbon(aes(ymin=fit-1.96*se, ymax=fit+1.96*se, fill=group), alpha=.2) 
  
  # p = ggplot() + 
  #   geom_point(aes(x = list_best_surplus_plot$variable, y = log(list_best_surplus_plot$value), color = list_best_surplus_plot$L1)) +
  #   labs(x = "Iteration", y = "Log(surplus)")
  # ggsave(filename = paste0("graphs/convergence_",name), plot = p, device = "pdf", width = 6, height = 3)
  
  p = ggplot() + 
    geom_line(aes(x = list_mean$X2, y = list_mean$value, color = list_mean$L1)) +
    geom_ribbon(aes(x = list_min$X2, ymin = list_min$value, ymax = list_max$value, fill = list_mean$L1), alpha=.2) +
    labs(x = "Iteration", y = "Log(surplus)")
  ggsave(filename = paste0("graphs/iterations_surplus_convergence_",name), plot = p, device = "pdf", width = 6, height = 3)
  
  return()
}


plot_disaggregated_community_betas_year_lines <- function(name, df_gen_assigned, df_cons_selected_users, df_local_time){
  
  df_local_time$time = 1:nrow(df_local_time)
  
  df_cons_selected_users_sunny = df_cons_selected_users[df_local_time$sunny, ]
  colnames(df_gen_assigned) = colnames(df_cons_selected_users)
  
  # calculate solar consumption and surplus
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned = df_gen_assigned, df_cons_selected = df_cons_selected_users_sunny)
  
  solar_surplus <- df_gen_assigned - df_cons_selected_users_sunny
  solar_surplus[solar_surplus < 0] = 0
  
  grid = df_cons_selected_users_sunny - df_gen_assigned
  grid[grid < 0] = 0
  
  # # add hour column
  df_solar_consumption$time = df_local_time$time[df_local_time$sunny]
  solar_surplus$time = df_local_time$time[df_local_time$sunny]
  grid$time = df_local_time$time[df_local_time$sunny]
  df_cons_selected_users$time = df_local_time$time
  
  grid = rbind(grid, df_cons_selected_users[!df_cons_selected_users$time %in% grid$time,])
  
  df_aux = df_cons_selected_users[!df_cons_selected_users$time %in% df_solar_consumption$time,] 
  df_aux[,-ncol(df_aux)] = 0
  
  df_solar_consumption = rbind(df_solar_consumption, df_aux)
  solar_surplus = rbind(solar_surplus, df_aux)
  
  solar_surplus = solar_surplus[order(solar_surplus$time), ]
  df_solar_consumption = df_solar_consumption[order(df_solar_consumption$time), ]
  grid = grid[order(grid$time), ]
  
  solar_surplus = as.numeric(rowSums(solar_surplus[, 1:(ncol(solar_surplus)-1)]))
  df_solar_consumption = as.numeric(rowSums(df_solar_consumption[, 1:(ncol(df_solar_consumption)-1)]))
  grid = as.numeric(rowSums(grid[, 1:(ncol(grid)-1)]))
  
  n_community = ncol(df_gen_assigned)
  
  df_plot <- data.frame("time" = df_local_time$time,
                        "Solar_surplus" = solar_surplus,
                        "Solar_consumption" = df_solar_consumption,
                        "Grid_consumption" = grid
  )
  
  
  df_plot = melt(df_plot, id.vars = "time")
  
  # TODO: understand which of the 2 is the correct one
  # self_consumption_percentage_mean_1 = mean( df_plot[grep(x = df_plot$variable, pattern = "solar_consumption"), "value"] / df_cons_selected_mean[, user+1]) 
  # self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, user+1])
  
  # self_consumption_percentage_mean_1 = mean(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"] / rowSums(df_cons_selected_mean[, 2:(n_community+1)]))
  self_consumption_percentage_max = max(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"] / rowSums(df_cons_selected_mean[, 2:(n_community+1)]))
  self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, 2:(n_community+1)])
  
  surplus_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "surplus"), "value"]) / sum(df_gen_assigned_mean[, 2:(n_community+1)])  
  
  index_order = order(colSums(df_cons_selected_users[, 1:n_community]))
  
  # how can I automatize this??
  # }else if (n_community == 7){ 
  p <- ggplot() +
    geom_line(aes(x = df_cons_selected_users[, "time"], y = df_cons_selected_users[, index_order[1]])) +
    geom_line(aes(x = df_cons_selected_users[, "time"], y = df_cons_selected_users[, index_order[1]] + df_cons_selected_users[,index_order[2]])) +
    geom_line(aes(x = df_cons_selected_users[, "time"], y = df_cons_selected_users[, index_order[1]] + df_cons_selected_users[, index_order[2]] + df_cons_selected_users[, index_order[3]])) +
    geom_line(aes(x = df_cons_selected_users[, "time"], y = df_cons_selected_users[, index_order[1]] + df_cons_selected_users[, index_order[2]] + df_cons_selected_users[, index_order[3]]  + df_cons_selected_users[, index_order[4]])) +
    geom_line(aes(x = df_cons_selected_users[, "time"], y = df_cons_selected_users[, index_order[1]] + df_cons_selected_users[, index_order[2]] + df_cons_selected_users[, index_order[3]]  + df_cons_selected_users[, index_order[4]] + df_cons_selected_users[, index_order[5]])) +
    geom_line(aes(x = df_cons_selected_users[, "time"], y = df_cons_selected_users[, index_order[1]] + df_cons_selected_users[, index_order[2]] + df_cons_selected_users[, index_order[3]]  + df_cons_selected_users[, index_order[4]] + df_cons_selected_users[, index_order[5]] + df_cons_selected_users[, index_order[6]])) +
    geom_line(aes(x = df_cons_selected_users[, "time"], y = df_cons_selected_users[, index_order[1]] + df_cons_selected_users[, index_order[2]] + df_cons_selected_users[, index_order[3]]  + df_cons_selected_users[, index_order[4]] + df_cons_selected_users[, index_order[5]] + df_cons_selected_users[, index_order[6]] + df_cons_selected_users[, index_order[7]])) +
    geom_area(aes(x = df_plot[, "time"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) 
  
  p <- ggplot() +
    geom_area(aes(x = df_plot[, "time"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) 
  
  # grids() + 
  # labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  # ggsave(filename = paste0("graphs/community_",name), plot = p, device = "pdf", width = 5, height = 3)
  return()
}


plot_disaggregated_community_betas_year_area <- function(name, df_gen_assigned, df_cons_selected_users, df_local_time){
  
  df_local_time$time = 1:nrow(df_local_time)
  
  df_cons_selected_users_sunny = df_cons_selected_users[df_local_time$sunny, ]
  colnames(df_gen_assigned) = colnames(df_cons_selected_users)
  
  # calculate solar consumption and surplus
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned = df_gen_assigned, df_cons_selected = df_cons_selected_users_sunny)
  
  solar_surplus <- df_gen_assigned - df_cons_selected_users_sunny
  solar_surplus[solar_surplus < 0] = 0
  
  grid = df_cons_selected_users_sunny - df_gen_assigned
  grid[grid < 0] = 0
  
  # # add hour column
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
  
  n_community = ncol(df_gen_assigned)
  
  df_plot <- data.frame("time" = df_local_time$time,
                        "Solar_surplus" = solar_surplus,
                        "Solar_consumption" = df_solar_consumption,
                        "Grid_consumption" = grid
  )
  
  
  df_plot = melt(df_plot, id.vars = "time")
  
  # TODO: understand which of the 2 is the correct one
  # self_consumption_percentage_mean_1 = mean( df_plot[grep(x = df_plot$variable, pattern = "solar_consumption"), "value"] / df_cons_selected_mean[, user+1]) 
  # self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, user+1])
  
  # self_consumption_percentage_mean_1 = mean(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"] / rowSums(df_cons_selected_mean[, 2:(n_community+1)]))
  
  # self_consumption_percentage_max = max(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"] / rowSums(df_cons_selected_mean[, 2:(n_community+1)]))
  # self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, 2:(n_community+1)])
  
  # surplus_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "surplus"), "value"]) / sum(df_gen_assigned_mean[, 2:(n_community+1)])  
  
  index_order = order(colSums(df_cons_selected_users[, 1:n_community]))
  p <- ggplot() +
    geom_area(aes(x = df_plot[, "time"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
    grids() +
    labs(x = "Time [h]", y = "Energy [kWh]", fill = "Type") 
  
  ggsave(filename = paste0("graphs/community_",name), plot = p, device = "pdf", width = 5, height = 3)
  
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




############################# AUX - main #############################


calculate_n_community_max <- function(generation, consumption){
  
  # minimum self consumption: 0.2?
  # TODO: a small problem when the peak is a platau
  
  # max_insolation = aggregate(x = df[[1]]$energy, by = list(date(df[[1]]$time)), FUN = max)
  # max_insolation_hours = aggregate(x = df[[1]]$energy, by = list(date(df[[1]]$time)), FUN = which.max)
  
  first_derivative = sign(diff(generation))
  # max_insolation_hours = which(diff(a) == -2)
  
  peak_insolation_hours = (diff(first_derivative) == -2)
  # TODO: this makes sense if we are studing only one month (change the 2 for a 3 is we are studing one year)
  high_insolation_hours = max(generation, na.rm = T)/2 < generation  
  
  # completing borders:
  peak_insolation_hours = c(T, peak_insolation_hours, T)
  max_insolation_hours = peak_insolation_hours & high_insolation_hours
  
  # checking:
  # peaks = generation
  # peaks[!max_insolation_hours] = NA
  # plot(generation, type = "l")
  # points(peaks)
  
  # n_community_max = 1 + ceiling(1/mean(colMeans(consumption[max_insolation_hours, ] / generation[max_insolation_hours])))
  n_community_mean = floor(1/mean(colMeans(consumption[max_insolation_hours, ] / generation[max_insolation_hours]))) - 1
  
  return(n_community_mean)
}


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


calculate_combination_for_GA_binary <- function(x, n_community, n_binary_rep,  df_cons){
  
  combination = rep(0, ncol(df_cons))
  
  for (j in 1:n_community) {
    user = binary2decimal(x[((j-1)*n_binary_rep + 1):(j*n_binary_rep)]) + 1  
    # print(user)
    combination[user] = 1
  }
  return(combination)
}


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


calculate_payback_betas <- function(purchase_price_sunny, df_cons_selected_day, df_gen_day, individual_investment, matrix_coefficients){
  
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_day, matrix_coefficients)
  
  surplus_x <- df_gen_assigned - df_cons_selected_day
  surplus_x[surplus_x < 0] = 0

  sale_price = 0.0508
  
  cost_old = colSums(purchase_price_sunny*df_cons_selected_day)
  
  grid_x = df_cons_selected_day - df_gen_assigned
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

calculate_matrix_coefficients <- function(optimizer_number, df_gen_sunny, df_cons_selected_sunny, n_community, individual_investment_selected = 0, df_local_time){
  
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
    calculate_matrix_coefficient_4(df_local_time, df_cons_selected_sunny, df_gen_sunny, df_purchase_price_one_day, n_community, individual_investment_selected)
  
  }
  
  return(matrix_coefficients)
}

calculate_matrix_coefficient_4 <- function(df_local_time, df_cons_selected_sunny, df_gen_sunny, df_purchase_price_one_day, n_community, individual_investment_selected){
  
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
      
      dim = calculate_dim(hourly=T, n_community, n_sunny_hours)
      optim <- nsga2R_flor(fn = purrr::partial(fitness_MO,
                                               df_gen_sunny = df_gen_sunny_one_day,
                                               df_cons_selected_sunny = df_cons_selected_sunny_one_day,
                                               # purchase_price_sunny = purchase_price_sunny,
                                               purchase_price_sunny = purchase_price_sunny_one_day,
                                               individual_investment_selected = individual_investment_selected),
                           varNo = dim,
                           objDim = 2,
                           generations = 100,
                           popSize = 200,
                           cprob = 0.8,
                           mprob = 0.2,
                           lowerBounds = rep(0, dim),
                           upperBounds = rep(1, dim))
      
      matrix_coefficients_month_date = choose_scenarios_normalized(optim, plot = F, n_community, n_sunny_hours, criteria = 2, name_plot = paste0(as.character(month_i),"_",as.character(date_i)))
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

