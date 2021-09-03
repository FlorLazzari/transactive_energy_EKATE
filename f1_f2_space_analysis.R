## select combination and day to study

# import from main_many_users:
# pre_optimal_combinations

hourly_surplus = apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus = colSums(hourly_surplus)

# TODO: should repeat this for several combinations... 10? (to start 10 sounds reasonable)

############################# select combination #############################

# combination_selected = pre_optimal_combinations[2, ]
combination_selected = pre_optimal_combinations[which.min(pre_surplus), ]

df_cons_selected_sunny = df_cons_sunny[,combination_selected==1]
df_cons_selected = df_cons[,combination_selected==1]

individual_investment_max = individual_investment[combination_selected==1]  
individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)

hourly_surplus = calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus = sum(hourly_surplus)

n_community = sum(combination_selected)

############################# OPTIMIZER 1: no optimization of repartition (equitative distribution) #############################

list_matrix_coefficients = list()
df_payback_surplus = data.frame("payback" = numeric(), "surplus" = numeric())

for (optimizer in (1:3)){
  list_matrix_coefficients[[optimizer]] = calculate_matrix_coefficients(optimizer_number = optimizer, df_gen_sunny, df_cons_selected_sunny, n_community, individual_investment_selected = individual_investment_selected)
  df_payback_surplus[optimizer, ] = rbind(calulate_payback_surplus_for_matrix(list_matrix_coefficients[[optimizer]], df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected))
}

############################# OPTIMIZER 4: optimum repartition taking into account the payback #############################

# run optimization for each characteristic the days:

matrix_coefficients_4 = matrix(0, nrow = length(df_gen_sunny), ncol = n_community)
matrix_coefficients_4_constraint = matrix(0, nrow = length(df_gen_sunny), ncol = n_community)

n_sunny_hours_start = 1
# for (month_i in 1:12) {
  for (date_i in 1:2) {
    
    # print(month_i)
    # print(date_i)
    # month_i = 2
    # date_i = 2
    
    df_local_time_first_day = df_local_time[df_local_time$month %in% month_i & df_local_time$date %in% date_i, ] 
    n_sunny_hours = sum(df_local_time_first_day$sunny)
    
    df_cons_selected_sunny_one_day = df_cons_selected_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1), ]
    df_cons_sunny_one_day = df_cons_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1), ]
    df_gen_sunny_one_day = df_gen_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1)]
    
    purchase_price_sunny_one_day = df_purchase_price_one_day[df_local_time_first_day$sunny,"price"]
    
    # optimize_hourly_betas_multi_objective_per_combination:
    dim = calculate_dim(hourly=T, n_community, n_sunny_hours)

    df_random_space = data.frame("surplus" = numeric(), "payback" = numeric())
    for (i in 1:8000) {
      df_random_space[i,] = fitness_MO(x = runif(dim, 0, 1), df_gen_sunny = df_gen_sunny_one_day, df_cons_selected_sunny = df_cons_selected_sunny_one_day, purchase_price_sunny = purchase_price_sunny, individual_investment_selected = individual_investment_selected)
    }

    optim <- nsga2R_flor(fn = purrr::partial(fitness_MO,
                                             df_gen_sunny = df_gen_sunny_one_day,
                                             df_cons_selected_sunny = df_cons_selected_sunny_one_day,
                                             purchase_price_sunny = purchase_price_sunny,
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
    
    ggplot() +
      geom_point(aes(x = df_random_space$surplus, y = log(df_random_space$payback))) +      
      # xlim(min(optim$objectives[,1]), max(optim$objectives[,1])) + 
      # xlim(0, 8) +
      # ylim(min(optim$objectives[,2]), max(optim$objectives[,2]))
      # ylim(5.4, 6.5) + 
      geom_point(aes(x = optim$objectives[,1], y = log(optim$objectives[,2])), colour = "red") 
    
    
    # ggplot() +
    #   geom_point(aes(x = df_random_space$surplus, y = df_random_space$payback)) +      
    #   # xlim(min(optim$objectives[,1]), max(optim$objectives[,1])) + 
    #   # xlim(2, 30) +
    #   # ylim(min(optim$objectives[,2]), max(optim$objectives[,2]))
    #   # ylim(200, 1000) 
    #   geom_point(aes(x = optim$objectives[,1], y = optim$objectives[,2]), colour = "red") 
    # 
    ggplot() +
      geom_point(aes(x = optim$objectives[,1], y = log(optim$objectives[,2])), colour = "red")
    
    ggplot() +
      geom_point(aes(x = optim$objectives[,1], y = log(optim$objectives[,2])), colour = "red")
    
    matrix_coefficients_month_date = choose_scenarios(optim, n_community, n_sunny_hours, criteria = 2, name_plot = paste0(as.character(month_i),"_",as.character(date_i)))
    matrix_coefficients_4[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1),] = matrix_coefficients_month_date
    
    n_sunny_hours_start = n_sunny_hours_start + n_sunny_hours
  }
}

################################################
# comparison matrix_4:

sum(colMeans(matrix_coefficients_4))
sum(colMeans(matrix_coefficients_4_constraint))

for (col in 1:ncol(matrix_coefficients_4) ){
  
  p = ggplot() +
    geom_histogram(aes(x = matrix_coefficients_4[, col])) 
  ggsave(filename = paste0("hist",col), plot = p, device = "pdf")
  
  q = ggplot() +
    geom_histogram(aes(x = matrix_coefficients_4_constraint[, col])) 
  ggsave(filename = paste0("hist_constraint",col), plot = q, device = "pdf")
}

for (row in 1:14){
  
  p = ggplot() +
    geom_histogram(aes(x = matrix_coefficients_4[row, ])) 
  ggsave(filename = paste0("hist",row), plot = p, device = "pdf")
  
  q = ggplot() +
    geom_histogram(aes(x = matrix_coefficients_4_constraint[row ,])) 
  ggsave(filename = paste0("hist_constraint",row), plot = q, device = "pdf")
}

payback_4 = calculate_payback_betas(purchase_price_sunny, df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, matrix_coefficients_4)
payback_4 = sum(exp(payback_4 - 0))
surplus_4 = sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_4, df_gen_sunny, df_cons_selected_sunny))

payback_4_constraint = calculate_payback_betas(purchase_price_sunny, df_cons_selected_sunny, df_gen_sunny, individual_investment_selected, matrix_coefficients_4_constraint)
payback_4_constraint = sum(exp(payback_4_constraint - 0))
surplus_4 = sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_4, df_gen_sunny, df_cons_selected_sunny))




