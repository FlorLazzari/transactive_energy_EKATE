# analysis

# TODO: work on plots to show that the hourly betas are working correctly

plot_optimization1_vs_optimization2(optimal_combination_using_2_GAs)

best_combination = select_best_combinations_betas(optimal_combination_using_2_GAs)

sunny_hours_index = which(df_gen != 0)
df_gen_day = df_gen[sunny_hours_index,]

df_cons_selected_users = df_cons[, !is.na(best_combination$payback)]
df_cons_selected_users_day = df_cons_selected_users[sunny_hours_index,]

df_gen_assigned_selected_users = calculate_gen_assigned_betas(df_gen_day = df_gen_day, matrix_coefficients = best_combination$optimum_coefficients)


plot_solar_consumption_daily_mean_betas(df_gen = df_gen, df_gen_assigned = df_gen_assigned_selected_users, df_cons_selected_users = df_cons_selected_users, time = df_month_1$time)
plot_disaggregated_daily_mean_per_user(df_gen_assigned = df_gen_assigned, df_cons_selected = df_cons_selected_users_day, time = df_month_1[, "time"])
plot_disaggregated_daily_mean_community(df_gen_assigned = df_gen_assigned_selected, df_cons_selected = df_cons_selected, time = df_month_1[, "time"])
# the economic difference is negligible but this makes sense because the betas are not hourly yet (?)
# with hourly betas this should change
plot_economic_comparison(df_gen = df_gen, optimum_combination = best_combination$optimum_coefficients[best_combination$optimum_coefficients != 0], df_cons_selected = df_cons[best_combination$optimum_coefficients != 0])

