############################# MAIN 4 #############################

############################# generate plot combination: with characteristic df #############################

############################# inputs #############################

list_matrix_coefficients_2 
list_matrix_coefficients_3 
list_matrix_coefficients_4 

list_individual_investment_selected 

############################# outputs #############################

vector_surplus_2 = c()
vector_surplus_3 = c()
vector_surplus_4 = c()

payback_2 = list()
payback_3 = list()
payback_4 = list()

############################# plot 1 #############################

# this plot is not very understandable.. so I will take statistics to better reproduce the results 

df_payback_2 = data.frame("i_matrix" = factor(), 
                       "combination" = factor(), 
                       "user" = factor(), 
                       "value" = as.numeric())

df_payback_3 = data.frame("i_matrix" = factor(), 
                       "combination" = factor(), 
                       "user" = factor(), 
                       "value" = as.numeric())

df_payback_4 = data.frame("i_matrix" = factor(), 
                       "combination" = factor(), 
                       "user" = factor(), 
                       "value" = as.numeric())

for (i in 1:length(list_combination)) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  

  payback_2 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                                 individual_investment = list_individual_investment_selected[[i]], 
                                                 matrix_coefficients = list_matrix_coefficients_2[[i]]))
  df_payback_2 = rbind(df_payback_2, data.frame("i_matrix" = factor(2), "combination" = i, "user" = factor(1:ncol(df_cons_selected_users)), "value" = payback_2))

  payback_3 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                                 individual_investment = list_individual_investment_selected[[i]], 
                                                 matrix_coefficients = list_matrix_coefficients_3[[i]]))
  df_payback_3 = rbind(df_payback_3, data.frame("i_matrix" = factor(3), "combination" = i, "user" = factor(1:ncol(df_cons_selected_users)), "value" = payback_3))

  payback_4 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                                 individual_investment = list_individual_investment_selected[[i]], 
                                                 matrix_coefficients = list_matrix_coefficients_4[[i]]))
  df_payback_4 = rbind(df_payback_4, data.frame("i_matrix" = factor(4), "combination" = i, "user" = factor(1:ncol(df_cons_selected_users)), "value" = payback_4))
}


plot_comparison_payback <- function(name, comparison){
  p <- ggplot() +
    geom_bar(aes(x = comparison$combination,  y = comparison$value, fill = comparison$user), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) +
    ylim(0, 9)
  ggsave(filename = paste0("graphs/comparison_payback_",name), plot = p, device = "pdf", width = 8, height = 3)

  return()  
}

plot_comparison_payback(name = "2", comparison = df_payback_2)
plot_comparison_payback(name = "3", comparison = df_payback_3)
plot_comparison_payback(name = "4", comparison = df_payback_4)

############################# plot 2: surplus #############################

# list_surplus = list()
# 
# df_characteristic = vector("list", length(years))
# names(df_characteristic) <- years

list_surplus = vector("list", 3)
names(list_surplus) = c("optimal", "random_n_comm", "random_big") 

for (i in 1:length(list_combination)) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  
  
  surplus_2 = sum(calculate_surplus_hourly_individual_betas(list_matrix_coefficients_2[[i]], df_gen_sunny, df_cons_selected_sunny))
  surplus_3 = sum(calculate_surplus_hourly_individual_betas(list_matrix_coefficients_3[[i]], df_gen_sunny, df_cons_selected_sunny))
  surplus_4 = sum(calculate_surplus_hourly_individual_betas(list_matrix_coefficients_4[[i]], df_gen_sunny, df_cons_selected_sunny))
  
  list_surplus[[i]] = data.frame("investment" = surplus_2, "solar_excess" = surplus_3, "solar_excess_and_payback" = surplus_4)
}

############################# plot 3: payback stats #############################

list_max_payback = vector("list", 3)
list_mean_payback = vector("list", 3)
list_min_payback_aux = vector("list", 3)
list_diff_max_min_payback = vector("list", 3)

names(list_max_payback) = c("optimal", "random_n_comm", "random_big") 
names(list_mean_payback) = c("optimal", "random_n_comm", "random_big")
names(list_min_payback_aux) = c("optimal", "random_n_comm", "random_big")
names(list_diff_max_min_payback) = c("optimal", "random_n_comm", "random_big")  

for (i in 1:length(list_combination)) {
  combination_i = list_combination[[i]]
  n_community = sum(combination_i)
  df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
  df_cons_selected_users = df_cons[, combination_i==1]  
  
  payback_2 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                                 individual_investment = list_individual_investment_selected[[i]], 
                                                 matrix_coefficients = list_matrix_coefficients_2[[i]]))
  
  payback_3 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                                 individual_investment = list_individual_investment_selected[[i]], 
                                                 matrix_coefficients = list_matrix_coefficients_3[[i]]))

  
  payback_4 = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, 
                                                 individual_investment = list_individual_investment_selected[[i]], 
                                                 matrix_coefficients = list_matrix_coefficients_4[[i]]))

  list_max_payback[[i]] = data.frame("investment" = max(payback_2), "solar_excess" = max(payback_3), "solar_excess_and_payback" = max(payback_4))
  list_mean_payback[[i]] = data.frame("investment" = mean(payback_2), "solar_excess" = mean(payback_3), "solar_excess_and_payback" = mean(payback_4))
  list_min_payback_aux[[i]] = data.frame("investment" = min(payback_2), "solar_excess" = min(payback_3), "solar_excess_and_payback" = min(payback_4))
  list_diff_max_min_payback[[i]] = list_max_payback[[i]] - list_min_payback_aux[[i]]
  
}


plot_comparison_stats(name = "new", list_surplus, list_max_payback, list_mean_payback, list_diff_max_min_payback)

plot_comparison_stats <- function(name, list_surplus, list_max_payback, list_mean_payback, list_diff_max_min_payback){
  
  # TODO: SHOULD CHAMGE THE PLACE WHERE THE LIST IS GENERATIONG... hardcoded solution!

  df_plot_surplus = melt(list_surplus)
  df_plot_surplus$L1 = factor(df_plot_surplus$L1, levels = c("random_n_comm", "random_big", "optimal") )

  p1 <- ggplot(df_plot_surplus, aes(x = variable, y = value, fill = variable)) +
    geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) + 
    facet_grid(L1 ~ .) + 
    labs(x = "", y = "Surplus [kWh]", fill = "") + 
    ylim(0, 250) +
    geom_text(aes(label = round(value, digits = 1)), position=position_dodge(width=0.9), vjust=-0.25)
  ggsave(filename = paste0("graphs/comparison_surplus_",name), plot = p1, device = "pdf", width = 8, height = 7)

  
  df_plot_max_payback = melt(list_max_payback)
  df_plot_max_payback$L1 = factor(df_plot_max_payback$L1, levels = c("random_n_comm", "random_big", "optimal") )
  
  p2 <- ggplot(df_plot_max_payback, aes(x = variable, y = value, fill = variable)) +
    geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) + 
    facet_grid(L1 ~ .) + 
    labs(x = "", y = "Payback [years]", fill = "") +
    ylim(0, 14) +
    geom_text(aes(label = round(value, digits = 1)), position=position_dodge(width=0.9), vjust=-0.25)
  ggsave(filename = paste0("graphs/comparison_max_payback_",name), plot = p2, device = "pdf", width = 8, height = 7)
    

  df_plot_mean_payback = melt(list_mean_payback)
  df_plot_mean_payback$L1 = factor(df_plot_mean_payback$L1, levels = c("random_n_comm", "random_big", "optimal") )

  p3 <- ggplot(df_plot_mean_payback, aes(x = variable, y = value, fill = variable)) +
    geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) + 
    facet_grid(L1 ~ .) + 
    labs(x = "", y = "Payback [years]", fill = "") +
    ylim(0, 7) +
    geom_text(aes(label = round(value, digits = 1)), position=position_dodge(width=0.9), vjust=-0.25)
  ggsave(filename = paste0("graphs/comparison_mean_payback_",name), plot = p3, device = "pdf", width = 8, height = 7)
  

  df_plot_diff_max_min_payback = melt(list_diff_max_min_payback)
  df_plot_diff_max_min_payback$L1 = factor(df_plot_diff_max_min_payback$L1, levels = c("random_n_comm", "random_big", "optimal") )
  
  p4 <- ggplot(df_plot_diff_max_min_payback, aes(x = variable, y = value, fill = variable)) +
    geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) + 
    facet_grid(L1 ~ .) + 
    labs(x = "", y = "Payback [years]", fill = "") +
    ylim(0, 14) +
    geom_text(aes(label = round(value, digits = 1)), position=position_dodge(width=0.9), vjust=-0.25)
  ggsave(filename = paste0("graphs/comparison_diff_max_min_payback_",name), plot = p4, device = "pdf", width = 8, height = 7)

  return()
}

plot_comparison_stats_separate <- function(name, list_surplus, list_max_payback, list_mean_payback, list_diff_max_min_payback){

  for (i in 1:length(list_max_payback)) {

    p <- ggplot() +
      geom_bar(aes(x = factor(1:length(list_surplus[[i]])), y = list_surplus[[i]], fill = factor(1:length(list_surplus[[i]]))), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) + 
      ylim(0, 250) +
      labs(x = "Combination of users", y = "Surplus [kWh]", fill = "")
    ggsave(filename = paste0("graphs/comparison_surplus_combination_",i,"_",name), plot = p, device = "pdf", width = 8, height = 3)
    
    
    p <- ggplot() +
      geom_bar(aes(x = factor(1:length(list_max_payback[[i]])), y = list_max_payback[[i]], fill = factor(1:length(list_max_payback[[i]]))), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) + 
      ylim(0, 13) +
      labs(x = "Combination of users", y = "Payback [years]")
    ggsave(filename = paste0("graphs/comparison_max_payback_combination_",i,"_",name), plot = p, device = "pdf", width = 8, height = 3)
    # Sys.sleep(1)
    
    q <- ggplot() +
      geom_bar(aes(x = factor(1:length(list_mean_payback[[i]])), y = list_mean_payback[[i]], fill = factor(1:length(list_mean_payback[[i]]))), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) +
      ylim(0, 15) +
      labs(x = "Combination of users", y = "Payback [years]")
    ggsave(filename = paste0("graphs/comparison_mean_payback_combination_",i,"_",name), plot = q, device = "pdf", width = 8, height = 3)
    # Sys.sleep(1)  
    
    t <- ggplot() +
      geom_bar(aes(x = factor(1:length(list_diff_max_min_payback[[i]])), y = list_diff_max_min_payback[[i]], fill = factor(1:length(list_diff_max_min_payback[[i]]))), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) +
      ylim(0, 13) +
      labs(x = "Combination of users", y = "Payback [years]")
    ggsave(filename = paste0("graphs/comparison_diff_max_min_combination_",i,"_",name), plot = t, device = "pdf", width = 8, height = 3)
    # Sys.sleep(1)
  }

  return()  
}




# missing the histogram with the mean distribution consumption of users, to show that the analysis is not biased
# should be in the folder: "1_data_analysis_barna" 

# TODO: calculate percentages to emphasise in the final conclusions of the paper!!! :)

# tomorrow write the conclusions in the overleaf





# INDIVIDUAL INVESTMENT
for (i in 1:length(list_combination)) {
  value_vector = as.numeric(list_individual_investment_selected[[2]])
  comparison = data.frame("user" = factor(1:ncol(df_cons_selected_users)), 
                          "value" = value_vector)
}

p <- ggplot() +
  geom_bar(aes(x = comparison$user, y = comparison$value), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/presentation_barna/comparison_investment_CASE42"), plot = p, device = "pdf", width = 8, height = 3)







# will have to work on this but will not be for the presentation
# TODO: most probably this results will be worst than the previous ones.. 
# so for the barna presentation I will hurry to show this

############################# generate plot combination: with complete df #############################

############################# plot 1 #############################
df_gen_original = df_gen_complete = df_gen_original



df_cons_selected_sunny = df_cons_sunny[, combination_i==1]
df_cons_selected_users = df_cons[, combination_i==1]  

selected_users = colnames(df_meter) %in% colnames(df_cons_selected_users)

df_cons_selected_complete = df_meter[, selected_users]

df_gen = df_gen_complete[year(df_gen_complete$time) %in% 2017, ]

df_gen_aux = data.frame("time" = selected_year_consumption, 
                        "energy" = NA)

df_gen = merge(x = df_gen_aux, y = df_gen, by = df_gen_aux)


selected_year_consumption = seq(from = as.POSIXct("2017-01-01 00:00:00"), to = as.POSIXct("2017-12-31 00:00:00"), by = "hour")

df_local_time = data.frame("time" = selected_year_consumption,
                           "month" = as.numeric(month(selected_year_consumption)),
                           "date" = as.numeric(day(selected_year_consumption)), 
                           "hour" = as.numeric(hour(selected_year_consumption)),
                           "sunny" = (df_gen_complete$energy > 0.4)
)

df_gen_sunny = df_gen[df_local_time$sunny, 2]
p = plot_generation(df_generation = data.frame("time" = 1:nrow(df_gen), "energy" = df_gen$energy))

df_cons_sunny = df_cons[df_local_time$sunny, ]



df_cons_selected_complete_sunny = df_meter[, selected_users]





list_matrix_coefficients = list()
df_payback_surplus = data.frame("payback" = numeric(), "surplus" = numeric())

for (optimizer in (1:3)){
  list_matrix_coefficients[[optimizer]] = calculate_matrix_coefficients(optimizer_number = optimizer, df_gen_sunny, df_cons_selected_sunny, n_community, individual_investment_selected = individual_investment_selected)
  df_payback_surplus[optimizer, ] = rbind(calulate_payback_surplus_for_matrix(list_matrix_coefficients[[optimizer]], df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected))
}

list_matrix_coefficients[[4]] = matrix_coefficients_4
df_payback_surplus[4, ] = calulate_payback_surplus_for_matrix(matrix_coefficients_4, df_gen_sunny, df_cons_selected_sunny, n_community, purchase_price_sunny, individual_investment_selected)

list_matrix_coefficients[[5]] = matrix_coefficients_3_random_n_comm
individual_investment_selected_random_n_comm = individual_investment_selected
df_payback_surplus[5, ] = rbind(calulate_payback_surplus_for_matrix(list_matrix_coefficients[[5]], df_gen_sunny, df_cons_random_n_comm_sunny, n_community, purchase_price_sunny, individual_investment_selected_random_n_comm))

individual_investment_selected_random_big = vector(length = ncol(df_cons_random_big_sunny))
individual_investment_selected_random_big[] = global_investment/ncol(df_cons_random_big_sunny)

list_matrix_coefficients[[6]] = matrix_coefficients_3_random_big
df_payback_surplus[6, ] = rbind(calulate_payback_surplus_for_matrix(list_matrix_coefficients[[6]], df_gen_sunny, df_cons_random_big_sunny, n_community = ncol(df_cons_random_big_sunny), purchase_price_sunny, individual_investment_selected_random_big))

comparison = data.frame("i_matrix" = factor(1:6), 
                        "value" = df_payback_surplus$surplus)

p <- ggplot() +
  geom_bar(aes(x = comparison$i_matrix, y = comparison$value, fill = comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/comparison_CASE42"), plot = p, device = "pdf", width = 8, height = 3)

p <- ggplot() +
  geom_bar(aes(x = comparison$i_matrix, y = comparison$value, fill = comparison$i_matrix), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/comparison_CASE42"), plot = p, device = "pdf", width = 8, height = 3)


############################# plot 2 #############################

comparison_1 = data.frame("user" = factor(1:ncol(df_cons_selected_users)), 
                          "i_matrix" = factor(1), 
                          "value" = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, individual_investment = individual_investment_selected, matrix_coefficients = list_matrix_coefficients[[1]]))
)

comparison_2 = data.frame("user" = factor(1:ncol(df_cons_selected_users)), 
                          "i_matrix" = factor(2), 
                          "value" = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, individual_investment = individual_investment_selected, matrix_coefficients = list_matrix_coefficients[[2]]))
)

comparison_3 = data.frame("user" = factor(1:ncol(df_cons_selected_users)), 
                          "i_matrix" = factor(3), 
                          "value" = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, individual_investment = individual_investment_selected, matrix_coefficients = list_matrix_coefficients[[3]]))
)

comparison_4 = data.frame("user" = factor(1:ncol(df_cons_selected_users)), 
                          "i_matrix" = factor(4), 
                          "value" = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_selected_sunny, df_gen_sunny = df_gen_sunny, individual_investment = individual_investment_selected, matrix_coefficients = list_matrix_coefficients[[4]]))
)

comparison_5 = data.frame("user" = factor(1:ncol(df_cons_random_n_comm_users)), 
                          "i_matrix" = factor(5), 
                          "value" = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_random_n_comm_sunny, df_gen_sunny = df_gen_sunny, individual_investment = individual_investment_selected, matrix_coefficients = list_matrix_coefficients[[5]]))
)

comparison_6 = data.frame("user" = factor(1:ncol(df_cons_random_big_sunny)), 
                          "i_matrix" = factor(6), 
                          "value" = as.numeric(calculate_payback_betas(purchase_price_sunny = purchase_price_sunny, df_cons_selected_sunny = df_cons_random_big_sunny, df_gen_sunny = df_gen_sunny, individual_investment = individual_investment_selected_random_big, matrix_coefficients = list_matrix_coefficients[[6]]))
)

comparison = rbind(comparison_1, comparison_2, comparison_3, comparison_4, comparison_5, comparison_6)

p <- ggplot() +
  geom_bar(aes(x = comparison$i_matrix,  y = comparison$value, fill = comparison$user), alpha = 0.5, width = 0.5, stat = "identity", position=position_dodge(width=0.7)) 
ggsave(filename = paste0("graphs/comparison_payback_CASE42"), plot = p, device = "pdf", width = 8, height = 3)
