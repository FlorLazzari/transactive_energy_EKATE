## select combination and day to study

# import from main_many_users:
pre_optimal_combinations

hourly_surplus = apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus = colSums(hourly_surplus)

############################# select combination #############################

# combination_selected = pre_optimal_combinations[2, ]
combination_selected = pre_optimal_combinations[which.min(pre_surplus), ]

df_cons_selected_sunny = df_cons_sunny[,combination_selected==1]
df_cons_selected = df_cons[,combination_selected==1]

individual_investment_max = individual_investment[combination_selected==1]  
individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)

hourly_surplus = calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus = sum(hourly_surplus)


############################# select the month and day to generate the plots for the operational comparison #############################

# to check surplus
n_sunny_hours_start = 1
# for (month_i in 1:12) {
for (month_i in 1:3) {
  for (date_i in 1:2) {

    # month_i = 7
    # date_i = 1
    
    df_local_time_first_day = df_local_time[df_local_time$month %in% month_i & df_local_time$date %in% date_i, ] 
    n_sunny_hours = sum(df_local_time_first_day$sunny)
    
    df_cons_selected_sunny_one_day = df_cons_selected_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1), ]
    df_cons_sunny_one_day = df_cons_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1), ]
    df_gen_sunny_one_day = df_gen_sunny[n_sunny_hours_start:(n_sunny_hours_start + n_sunny_hours - 1)]
    
    df = cbind(df_gen_sunny_one_day, df_cons_selected_sunny_one_day, "time" = 1:length(df_gen_sunny_one_day))
    plot_initial(name = paste0(month_i,"_",date_i), df)

    # checking 
    print(sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny_one_day, df_cons = df_cons_sunny_one_day)))
    n_sunny_hours_start = n_sunny_hours_start + n_sunny_hours 
  }
}

n_community = ncol(df_cons_selected_sunny)  

hourly_surplus = calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny_one_day, df_cons = df_cons_sunny_one_day)
pre_surplus = sum(hourly_surplus)


############################# generation of fake data #############################

# # TODO TODO TODO
# # generate fake data to show graph!
# 
# typical_pattern_1 = c(0.6, 0.5,0.4, 0.2, 0.2, 0.2, 0.4, 0.4, 0.35, 0.4, 0.4, 0.45, 0.4, 0.5, 0.55, 0.6, 0.55, 0.55, 0.5, 0.6, 0.75, 0.8, 0.75, 0.7) * 10
# typical_pattern_2 = c(0.6, 0.5,0.4, 0.2, 0.2, 0.2, 0.4, 0.4, 0.45, 0.55, 0.4, 0.35, 0.4, 0.5, 0.55, 0.6, 0.55, 0.55, 0.5, 0.6, 0.75, 0.8, 0.75, 0.7) * 10 
# typical_pattern_3 = c(0.6, 0.5,0.4, 0.2, 0.2, 0.2, 0.4, 0.4, 0.45, 0.5, 0.6, 0.75, 0.8, 0.75, 0.7, 0.6, 0.55, 0.55,0.55, 0.4, 0.35, 0.4, 0.5, 0.55) * 10
# 
# 
# df_cons_selected_sunny_one_day[, 1] = typical_pattern_1[6:(6+nrow(df_cons_selected_sunny_one_day)-1)]
# df_cons_selected_sunny_one_day[, 2] = typical_pattern_2[6:(6+nrow(df_cons_selected_sunny_one_day)-1)]
# df_cons_selected_sunny_one_day[, 3] = typical_pattern_3[6:(6+nrow(df_cons_selected_sunny_one_day)-1)]
# 
# which.max(df_gen_sunny_one_day)
# max(df_gen_sunny_one_day)
# sum(df_cons_selected_sunny_one_day[which.max(df_gen_sunny_one_day),])
# 
# # df_cons_selected_sunny_one_day[which.max(df_gen_sunny_one_day),4:11] = df_cons_selected_sunny_one_day[which.max(df_gen_sunny_one_day),4:11]*0.5
# # df_cons_selected_sunny_one_day[,4:11] = df_cons_selected_sunny_one_day[,4:11]*0.5
# # 
# # df_gen_sunny_one_day = df_gen_sunny_one_day_original
# # df_gen_sunny_one_day_original = df_gen_sunny_one_day
# 
# # df_gen_sunny_one_day = df_gen_sunny_one_day*0.9
# 
# df_cons_sunny_one_day[, combination_selected == 1] = df_cons_selected_sunny_one_day
# 
# hourly_surplus = calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny_one_day, df_cons = df_cons_sunny_one_day)
# pre_surplus = sum(hourly_surplus)
# 
# 
# df = cbind(df_gen_sunny_one_day*0.2, df_cons_selected_sunny_one_day, "time" = 1:length(df_gen_sunny_one_day))
# plot_initial(name = paste0(month_i,"_",date_i), df)
