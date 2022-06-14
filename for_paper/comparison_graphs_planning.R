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


############################# OPTIMIZER 2: repartition based on investment #############################

matrix_coefficients_2 = calculate_matrix_coefficients(optimizer_number = 2, df_gen_sunny, df_cons_selected_sunny, n_community, individual_investment_selected)
df_payback_surplus_2 = calulate_payback_surplus_for_matrix(matrix_coefficients_2, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected) 

############################# OPTIMIZER 3: repartition based on surplus (preoptimization) #############################

matrix_coefficients_3 = calculate_matrix_coefficients(optimizer_number = 3, df_gen_sunny, df_cons_selected_sunny, n_community)
calulate_payback_surplus_for_matrix(matrix_coefficients_3, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected) 

############################# OPTIMIZER 4: optimum repartition taking into account the payback #############################

# run optimization for each characteristic the days:

matrix_coefficients_4 = matrix(0, nrow = length(df_gen_sunny), ncol = n_community)
matrix_coefficients_4_constraint = matrix(0, nrow = length(df_gen_sunny), ncol = n_community)





############################# choose the best combination for each optimization #############################



############################# generate plot #############################

# CASE 4.2) GRAPHS showing: INDIVIDUAL PAYBACK - INDIVIDUAL INVESTMENT

# DAILY SOLAR ASSIGNATION 0
df_gen_assigned_one_day_1 = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_1)
df_gen_assigned_one_day_2 = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_2)
df_gen_assigned_one_day_3 = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_3)
df_gen_assigned_one_day_4 = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny_one_day, matrix_coefficients = matrix_coefficients_4)

# GLOBAL SURPLUS

value_vector = c(sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_1, df_gen_sunny_one_day, df_cons_selected_sunny_one_day)),
                 sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_2, df_gen_sunny_one_day, df_cons_selected_sunny_one_day)),
                 sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_3, df_gen_sunny_one_day, df_cons_selected_sunny_one_day)),
                 sum(calculate_surplus_hourly_individual_betas(matrix_coefficients_4, df_gen_sunny_one_day, df_cons_selected_sunny_one_day))
)

comparison = data.frame("i_matrix" = factor(1:4), 
                        "value" = value_vector
)

p <- ggplot() +
  geom_bar(aes(x = comparison$i_matrix, y = comparison$value, fill = comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/test/comparison_planning"), plot = p, device = "pdf", width = 8, height = 3)

# GLOBAL PAYBACK

# TODO: function to calculate global payback

# INDIVIDUAL PAYBACK

comparison_1 = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                          "i_matrix" = factor(1), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_day = df_cons_selected_sunny_one_day, df_gen_day = df_gen_sunny_one_day, individual_investment = individual_investment_selected, matrix_coefficients = matrix_coefficients_1))
)

comparison_2 = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                          "i_matrix" = factor(2), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_2)))

comparison_3 = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                          "i_matrix" = factor(3), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_3)))

comparison_4 = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                          "i_matrix" = factor(4), 
                          "value" = as.numeric(calculate_payback_betas_daily(df_cons_selected_sunny_one_day, df_gen_sunny_one_day, individual_investment_selected, matrix_coefficients_4)))

comparison = rbind(comparison_1, comparison_2, comparison_3, comparison_4)

p <- ggplot() +
  geom_bar(aes(x = comparison$i_matrix,  y = comparison$value, fill = comparison$user), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/test/comparison_planning"), plot = p, device = "pdf", width = 8, height = 3)

# INDIVIDUAL INVESTMENT
value_vector = as.numeric(individual_investment_selected)
comparison = data.frame("user" = factor(1:ncol(df_cons_selected)), 
                        "value" = value_vector)

p <- ggplot() +
  geom_bar(aes(x = comparison$user, y = comparison$value), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/test/comparison_planning"), plot = p, device = "pdf", width = 8, height = 3)




















