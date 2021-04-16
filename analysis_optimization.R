# analysis

# TODO: work on plots to show that the hourly betas are working correctly

# plot_optimization1_vs_optimization2(optimal_combination_using_2_GAs)

best_combination = select_best_combinations_betas(optimal_combination_using_2_GAs)

# to do the plots I will add the time 

df_local_time = data.frame("time" = df_month_1[, "time"], 
                           "date" = day(df_month_1[, "time"]), 
                           "hour" = hour(df_month_1[, "time"]), 
                           "sunny" = (df_gen$gen_1 != 0))

# df_gen_sunny = df_gen[df_local_time$sunny,]
# 
# df_cons_selected_users = df_cons[, !is.na(best_combination$payback)]
# df_cons_selected_users_sunny = df_cons_selected_users[sunny_hours_index,]

df_gen_assigned = calculate_gen_assigned_betas(df_gen_day = df_gen[df_local_time$sunny,], matrix_coefficients = best_combination$optimum_coefficients)


# plot_solar_consumption_daily_mean_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[df_local_time$sunny, !is.na(best_combination$payback)], df_local_time)

plot_disaggregated_daily_mean_per_user_betas(df_gen_assigned = df_gen_assigned, df_cons_selected_users = df_cons[, !is.na(best_combination$payback)], df_local_time)

plot_disaggregated_daily_mean_community(df_gen_assigned = df_gen_assigned, df_cons_selected = df_cons_selected_users, df_local_time)
# the economic difference is negligible but this makes sense because the betas are not hourly yet (?)
# with hourly betas this should change
plot_economic_comparison(df_gen = df_gen, optimum_combination = best_combination$optimum_coefficients[best_combination$optimum_coefficients != 0], df_cons_selected = df_cons[best_combination$optimum_coefficients != 0])

