## select combination and day to study

# import from main_many_users:
pre_optimal_combinations

hourly_surplus = apply(X = pre_optimal_combinations, MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny)
pre_surplus = colSums(hourly_surplus)

############################# select a combination #############################

combination_selected = pre_optimal_combinations[1, ]

df_cons_selected_sunny = df_cons_sunny[,combination_selected==1]
df_cons_selected = df_cons[,combination_selected==1]

individual_investment_max = individual_investment[combination_selected==1]  
individual_investment_selected = calculate_individual_investment(combination_selected, global_investment, individual_investment_max)

############################# select one day #############################

# to check surplus
n_sunny_hours_start = 1
# for (month_i in 1:12) {
for (month_i in 1:12) {
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
    # print(sum(calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny_one_day, df_cons = df_cons_sunny_one_day)))
    n_sunny_hours_start = n_sunny_hours_start + n_sunny_hours 
  }
}

n_community = ncol(df_cons_selected_sunny)  

hourly_surplus = calculate_surplus_hourly_community(combination = combination_selected, df_gen = df_gen_sunny_one_day, df_cons = df_cons_sunny_one_day)
pre_surplus = sum(hourly_surplus)
