# should paste the analysis from:  comparison_graphs_CASE42.R




df_local_time$time = 1:nrow(df_local_time)
df_cons_to_plot =  df_cons_ordered_sunny[, pre_optimal_combination_free==1]
df_cons_to_plot$hour =  df_local_time$time[df_local_time$sunny == T]
df_cons_to_plot = melt(data = df_cons_to_plot, id.vars = "hour")

ggplot(df_cons_to_plot) +  
  geom_line(aes(x = hour, y = value)) + facet_grid(variable ~ .)


################## first group of users
list_selected_users = list("1" = c(114, 117, 118, 119, 120, 125, 127), 
                           "2" = c(114, 117, 118, 119, 120, 122, 125), 
                           "3" = c(107, 114, 117, 118, 119, 120, 125))

# list_matrix_coefficients_4 = list() 

for (i in 2:3) {
  selected_users = list_selected_users[[i]]
  
  df_cons_selected_sunny = df_cons_sunny[, selected_users]    
  matrix_coefficients_3 = calculate_matrix_coefficients(optimizer_number = 3, df_gen_sunny, df_cons_selected_sunny, n_community)
  
  # evaluation:
  calulate_payback_surplus_for_matrix(matrix_coefficients_3, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected)
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_3)
  df_cons_selected_users = df_cons[, selected_users]  
  plot_disaggregated_community_betas_year_area(name = paste0("combination_",i,"_optimization_1"), df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_users, df_local_time = df_local_time)
  
  # apply optimization 2:
  # investment proportional to the consumption during solar hours:
  individual_investment_1 = sapply(df_cons_selected_sunny, max, na.rm = TRUE)*1100
  # now everyone is investing the same:
  individual_investment_2 = individual_investment_1
  individual_investment_2[] = global_investment/n_community
  individual_investment_max = individual_investment_2
  
  # check that:
  print(sum(individual_investment_max) >= global_investment)
  
  pre_optimal_combination_free = rep(0, times=128)
  pre_optimal_combination_free[selected_users] = 1
  # individual_investment_max = individual_investment[pre_optimal_combination_free==1]  
  individual_investment_selected = calculate_individual_investment(pre_optimal_combination_free, global_investment, individual_investment_max)
  
  matrix_coefficients_4 = calculate_matrix_coefficient_4_one_year(df_local_time, df_cons_selected_sunny, df_gen_sunny, df_purchase_price_one_day, n_community, individual_investment_selected)
  list_matrix_coefficients_4[[i]] = matrix_coefficients_4
  calulate_payback_surplus_for_matrix(matrix_coefficients_4, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected) 
  
  # evaluation:
  calulate_payback_surplus_for_matrix(matrix_coefficients_4, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected)
  df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen_sunny, matrix_coefficients = matrix_coefficients_4)
  df_cons_selected_users = df_cons[, selected_users]  
  plot_disaggregated_community_betas_year_area(name = paste0("combination_",i,"_optimization_2"), df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons_selected_users, df_local_time = df_local_time)
}

i = 3
selected_users = list_selected_users[[i]]
df_cons_selected_sunny = df_cons_sunny[, selected_users]    
matrix_coefficients_3 = calculate_matrix_coefficients(optimizer_number = 3, df_gen_sunny, df_cons_selected_sunny, n_community)

calulate_payback_surplus_for_matrix_by_user(matrix_coefficients_3, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected)
calulate_payback_surplus_for_matrix_by_user(list_matrix_coefficients_4[[i]], df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected) 



############################# comparisons #############################

# comparison matrix_4:

# for (col in 1:ncol(matrix_coefficients_4) ){
#   
#   p = ggplot() +
#     geom_histogram(aes(x = matrix_coefficients_4[, col])) 
#   ggsave(filename = paste0("hist",col), plot = p, device = "pdf")
#   
#   q = ggplot() +
#     geom_histogram(aes(x = matrix_coefficients_4_constraint[, col])) 
#   ggsave(filename = paste0("hist_constraint",col), plot = q, device = "pdf")
# }
# 
# 
# for (row in 1:14){
#   
#   p = ggplot() +
#     geom_histogram(aes(x = matrix_coefficients_4[row, ])) 
#   ggsave(filename = paste0("hist",row), plot = p, device = "pdf")
#   
#   q = ggplot() +
#     geom_histogram(aes(x = matrix_coefficients_4_constraint[row ,])) 
#   ggsave(filename = paste0("hist_constraint",row), plot = q, device = "pdf")
# }
