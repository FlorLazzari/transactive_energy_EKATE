# TODO: should do 4 plots like this one 

plot_disaggregated_daily_mean_community_betas <- function(name, df_gen_assigned, df_cons_selected_users, df_local_time){
  
  df_cons_selected_users_sunny = df_cons_selected_users[df_local_time$sunny, ]
  colnames(df_gen_assigned) = colnames(df_cons_selected_users)
  
  
  # calculate solar consumption and surplus
  df_solar_consumption = calculate_solar_consumption(df_gen_assigned, df_cons_selected_users_sunny)
  
  solar_surplus <- df_gen_assigned - df_cons_selected_users_sunny
  solar_surplus[solar_surplus < 0] = 0
  
  grid = df_cons_selected_users_sunny - df_gen_assigned
  grid[grid < 0] = 0
  
  # add hour column
  df_solar_consumption$hour = df_local_time$hour[df_local_time$sunny]
  solar_surplus$hour = df_local_time$hour[df_local_time$sunny]
  grid$hour = df_local_time$hour[df_local_time$sunny]
  df_cons_selected_users$hour = df_local_time$hour
  
  grid = rbind(grid, df_cons_selected_users[!df_cons_selected_users$hour %in% grid$hour,])
  
  df_aux = df_cons_selected_users[!df_cons_selected_users$hour %in% df_solar_consumption$hour,] 
  df_aux[,-ncol(df_aux)] = 0
  
  df_solar_consumption = rbind(df_solar_consumption, df_aux)
  solar_surplus = rbind(solar_surplus, df_aux)
  
  # aggregate
  solar_consumption_mean = aggregate(x = df_solar_consumption, by = list(df_solar_consumption$hour), FUN = mean)
  solar_surplus_mean = aggregate(x = solar_surplus, by = list(solar_surplus$hour), FUN = mean) 
  grid_mean = aggregate(x = grid, by = list(grid$hour), FUN = mean) 
  
  # to calculate the self consumption and surplus 
  df_cons_selected_users$hour = df_local_time$hour
  df_cons_selected_mean = aggregate(x = df_cons_selected_users, by = list(df_cons_selected_users$hour), FUN = mean) 
  
  n_community = ncol(df_gen_assigned)
  
  df_gen_assigned$hour = df_local_time$hour[df_local_time$sunny]
  df_gen_assigned_mean = aggregate(x = df_gen_assigned, by = list(df_gen_assigned$hour), FUN = mean)
  
  ##
  
  solar_consumption_mean_community <- rowSums(solar_consumption_mean[, 2:(n_community+1)])
  grid_mean_community <- rowSums(grid_mean[, 2:(n_community+1)])  
  solar_surplus_mean_community <- rowSums(solar_surplus_mean[, 2:(n_community+1)])  
  
  df_plot <- data.frame("hour" =  df_local_time$hour,
                        "Solar_surplus" = solar_surplus_mean_community,
                        "Solar_consumption" = solar_consumption_mean_community,
                        "Grid_consumption" = grid_mean_community
  )
  
  df_plot = melt(df_plot, id.vars = "hour")
  
  # TODO: understand which of the 2 is the correct one
  # self_consumption_percentage_mean_1 = mean( df_plot[grep(x = df_plot$variable, pattern = "solar_consumption"), "value"] / df_cons_selected_mean[, user+1]) 
  # self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, user+1])
  
  # self_consumption_percentage_mean_1 = mean(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"] / rowSums(df_cons_selected_mean[, 2:(n_community+1)]))
  self_consumption_percentage_max = max(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"] / rowSums(df_cons_selected_mean[, 2:(n_community+1)]))
  self_consumption_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "Solar_consumption"), "value"]) / sum(df_cons_selected_mean[, 2:(n_community+1)])
  
  surplus_percentage_mean = sum(df_plot[grep(x = df_plot$variable, pattern = "surplus"), "value"]) / sum(df_gen_assigned_mean[, 2:(n_community+1)])  
  
  index_order = order(colSums(df_cons_selected_mean[, 2:(n_community+1)]))
  
  # how can I automatize this??
  if (n_community == 2) {
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 3){
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 4){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else if (n_community == 5){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)),fill = "")  
  }else if (n_community == 6){ 
    p <- ggplot() +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]])) +
      geom_line(aes(x = df_cons_selected_mean[, "hour"], y = df_cons_selected_mean[, 1+index_order[1]] + df_cons_selected_mean[,1+index_order[2]] + df_cons_selected_mean[,1+index_order[3]]  + df_cons_selected_mean[,1+index_order[4]] + df_cons_selected_mean[,1+index_order[5]] + df_cons_selected_mean[,1+index_order[6]])) +
      geom_area(aes(x = df_plot[, "hour"], y = df_plot[, "value"], fill = df_plot[,"variable"]), alpha = 0.5) +
      # grids() + 
      labs(x = "Time [h]", y = "Energy [kWh]", title = paste0("surplus = ", round(surplus_percentage_mean, digits = 2), ", max_self_cons = ", round(self_consumption_percentage_max, digits = 2)), fill = "")  
  }else{ 
    print("n_community != 2:6")
  }
  
  ggsave(filename = paste0("graphs/community_",name), plot = p, device = "pdf", width = 5, height = 3)
  
  return()
}




