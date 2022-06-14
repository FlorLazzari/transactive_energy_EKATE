# evaluation of 2 different ways of formulating the nsga-II

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

############################# optimization for each characteristic day #############################

matrix_coefficients_4 = matrix(0, nrow = length(df_gen_sunny), ncol = n_community)
matrix_coefficients_4_constraint = matrix(0, nrow = length(df_gen_sunny), ncol = n_community)

n_sunny_hours_start = 1
for (month_i in 1:12) {
  for (date_i in 1:2) {
    
    print(month_i)
    print(date_i)
    # month_i = 1
    # date_i = 2
    
    df_local_time_first_day = df_local_time[df_local_time$month %in% month_i & df_local_time$date %in% date_i, ] 
    n_sunny_hours = sum(df_local_time_first_day$sunny)
    
    df_cons_selected_sunny_one_day = df_cons_selected_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1), ]
    df_cons_sunny_one_day = df_cons_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1), ]
    df_gen_sunny_one_day = df_gen_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1)]
    
    purchase_price_sunny_one_day = df_purchase_price_one_day[df_local_time_first_day$sunny,"price"]
    
    # optimize_hourly_betas_multi_objective_per_combination:
    dim = calculate_dim(hourly=T, n_community, n_sunny_hours)
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
    
    # criteria 2 = reasonable surplus & payback
    # TODO: is this working?
    # work in "selection_according_to_criteria"
    # matrix_coefficients_month_date = selection_according_to_criteria_2(optim, n_community, n_sunny_hours, criteria = 2, name_plot = paste0("normalization", as.character(month_i),"_",as.character(date_i)))
    
    matrix_coefficients_month_date = choose_scenarios(optim, n_community, n_sunny_hours, criteria = 2, name_plot = paste0(as.character(month_i),"_",as.character(date_i)))
    matrix_coefficients_4[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1),] = matrix_coefficients_month_date
    
    ################################################
    
    optim_constrain <- nsga2R_flor_constraint(fn = purrr::partial(fitness_MO_constraint,
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
    
    # criteria 2 = reasonable surplus & payback
    # TODO: is this working?
    # work in "selection_according_to_criteria"
    # matrix_coefficients_month_date = selection_according_to_criteria_2(optim, n_community, n_sunny_hours, criteria = 2, name_plot = paste0("normalization", as.character(month_i),"_",as.character(date_i)))
    
    matrix_coefficients_month_date = choose_scenarios(optim_constrain, n_community, n_sunny_hours, criteria = 2, name_plot = paste0("optim_constraint_",as.character(month_i),"_",as.character(date_i)))
    matrix_coefficients_4_constraint[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1),] = matrix_coefficients_month_date
    
    ################################################
    
    
    # matrix_coefficients_month_date = choose_scenarios_normalizing(df_scenarios, optim, n_community, n_sunny_hours, criteria = 2, name_plot = paste0(as.character(month_i),"_",as.character(date_i)))
    # matrix_coefficients_4[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1),] = matrix_coefficients_month_date
    
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

