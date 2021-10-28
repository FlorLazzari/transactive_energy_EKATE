
############################# defining boxes to select users #############################
# TODO:
n_community = n_community_max

# TODO IMPORTANT: for the second model: REDEFINE THE define_boxes
list_box_season = define_boxes(df_cons, df_gen, df_local_time, n_community)
n_binary_rep_box = log(8, base=2)
df_cons_ordered = df_cons[, list_box_season[[1]]]
for (i in 2:length(list_box_season)) {
  df_cons_ordered = cbind(df_cons_ordered, df_cons[, list_box_season[[i]]])
}
colnames(df_cons_ordered) = paste0("cons_",1:ncol(df_cons_ordered))
list_box_season_ordered = define_boxes(df_cons = df_cons_ordered, df_gen, df_local_time, n_community)

df_cons_ordered_sunny = df_cons_ordered[df_local_time$sunny, ]


############################# defining model 2: free #############################
n_binary_rep = log(ncol(df_cons_ordered_sunny), base=2)

# the orden doesnt change:
# df_cons_ordered_sunny_old = df_cons_ordered_sunny
# df_cons_ordered_sunny = df_cons_ordered_sunny[, order(runif(n = ncol(df_cons_ordered_sunny)))]

pre_optimal_combination_free = optimization_1_to_analize(n_community = n_community, n_binary_rep = n_binary_rep, df_gen_to_optimize = df_gen_sunny, df_cons_to_optimize = df_cons_ordered_sunny)



############################# defining model 2: free NO BOXES #############################

n_binary_rep = log(ncol(df_cons_sunny), base=2)
n_community = 7

# the orden doesnt change:
# df_cons_ordered_sunny_old = df_cons_ordered_sunny
# df_cons_ordered_sunny = df_cons_ordered_sunny[, order(runif(n = ncol(df_cons_ordered_sunny)))]



list_results = list()
for (i in 1:10) {
  pre_optimal_combination_free = optimization_1_to_analize_convergence(n_community = n_community, n_binary_rep = n_binary_rep, df_gen_to_optimize = df_gen_sunny, df_cons_to_optimize = df_cons_sunny)
  pre_optimal_combination_free = pre_optimal_combination_free[!duplicated(pre_optimal_combination_free), ]
  surplus = sum(calculate_surplus_hourly_community(combination = pre_optimal_combination_free, df_gen = df_gen_sunny, df_cons = df_cons_sunny))
  list_results[[i]] = list("combinations" = pre_optimal_combination_free, "surplus" = surplus) 
  print(list_results[[i]])
}

# selected combinations:
# which(list_results[[2]]$combinations == 1)
# c(114, 117, 118, 119, 120, 125, 127)
# c(114, 117, 118, 119, 120, 122, 125)
# c(107, 114, 117, 118, 119, 120, 125)

# pre_optimal_combination_free_1 = best_combinations

# problems: the users are being repeated
rowSums(pre_optimal_combination_free_1)
colSums(apply(X = as.matrix(pre_optimal_combination_free_1), MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny))

rowSums(pre_optimal_combination_free_2)
colSums(apply(X = as.matrix(pre_optimal_combination_free_2), MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny))

sum(calculate_surplus_hourly_community(combination = pre_optimal_combination_free_2[1, ], df_gen = df_gen_sunny, df_cons = df_cons_sunny))


df_local_time$time = 1:nrow(df_local_time)
df_cons_to_plot =  df_cons_ordered_sunny[, pre_optimal_combination_free==1]
df_cons_to_plot$hour =  df_local_time$time[df_local_time$sunny == T]
df_cons_to_plot = melt(data = df_cons_to_plot, id.vars = "hour")

ggplot(df_cons_to_plot) +  
  geom_line(aes(x = hour, y = value)) + facet_grid(variable ~ .)



surplus = 
surplus = sum(calculate_surplus_hourly_community(combination = pre_optimal_combination_free, df_gen = df_gen_sunny, df_cons = df_cons_ordered_sunny))


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
pre_optimal_combinations_2 = rbind(pre_optimal_combination_boxes, pre_optimal_combination_free)
nrow(pre_optimal_combination_2)
sum(!duplicated(pre_optimal_combinations_2))
surplus = colSums(apply(X = as.matrix(pre_optimal_combinations_2), MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_ordered_sunny))

plot_planning(name_file = "1", list_box_season_ordered, df_cons_ordered_selected, df_gen, df_local_time)

#  esta va a ser mi idea:
# 1) voy a probar distintos modelos de planning que me van a dar distintos resultados (estaría bien que fueran mínimo 3 resultados) (el último puede ser definiendo las cajitas de forma distinta)
# 2) pruebo convergencia para los 3 modelos
# 3) de ahí aplico la optimización 2 y veo los resultados (para que los resultados sean consistentes entonces el payback tiene que estar bien calculado si o si (no solo tomar la mean!))




# comparison matrix_4:

# sum(colMeans(matrix_coefficients_4))
# sum(colMeans(matrix_coefficients_4_constraint))

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





# this is all WRONG:
# ok, important! 
# when I defined the boxes I did improve the algo
# the improvement is given by the selection of users, not by the ordering (or should I try ordering them as a whole?? and not as boxes??)
# I should show the comparison with this optimizations:
# convergence and surplus

# want to prove that when I dont do the selection correctly, then boxes selection work better
# to do that then the boxes should make sense with the investment


n_community = n_community_max
df_cons_sunny_test = df_cons[df_local_time$sunny, ]

n_community = 7
n_binary_rep = log(ncol(df_cons_sunny_test), base=2)



list_results = list()
for (i in 1:10) {
  pre_optimal_combination_free_test = optimization_1_to_analize_convergence(n_community = n_community, n_binary_rep = n_binary_rep, df_gen_to_optimize = df_gen_sunny, df_cons_to_optimize = df_cons_sunny_test)
  surplus = sum(calculate_surplus_hourly_community(combination = pre_optimal_combination_free_test, df_gen = df_gen_sunny, df_cons = df_cons_sunny_test))
  list_results[[i]] = list("combinations" = pre_optimal_combination_free_test, "surplus" = surplus) 
  print(list_results[[i]])
}


optimization_1_to_analize_convergence(n_community = n_community, n_binary_rep = n_binary_rep, df_gen_to_optimize = df_gen_sunny, df_cons_to_optimize = df_cons_sunny_test)

# now I will try using the rest of the users






