############################# defining model 1: boxes #############################
# TODO:
n_community = 7
n_binary_rep = log(ncol(df_cons_sunny), base=2)

n_iter = 30
max_iter = 120
max_run = 40

# n_iter = 2
# max_iter = 2
# max_run = 2
    
# CONTRAST MODELS:
# # results: a convergence analysis 
# # problem, Im not solving the convergence problem! => but when I enlarge the sun generation the convergence problem is solved

############################# graph: convergence table #############################
# TODO: important! in import_data_genome_project the users are being ordered by mean hourly consumption!
# then this free model is not free at all => should introduce some randomness to make it a real free model 


# 0: free
df_cons_sunny_free = list()
for (i in 1:n_iter) {
  number_order = order(runif(n = ncol(df_cons)))
  df_cons_sunny_free[[i]] = df_cons_sunny[, number_order]
}
  
# 1: hourly_mean
hourly_mean = colMeans(df_cons_sunny, na.rm = T)
df_cons_sunny_ordered_1 = df_cons_sunny[, order(hourly_mean)]

# 2: dist_solar_cons
# monthly normalized
df_gen_norm = aggregate(x = df_gen$energy, by = list(paste0(df_local_time$month, df_local_time$date)), FUN = function(x){x/sum(x)})
df_gen_norm_mat = df_gen_norm$x

distance_per_user = c()
for (i in 1:ncol(df_cons)) {
  df_cons_norm = aggregate(x = df_cons[, i], by = list(paste0(df_local_time$month, df_local_time$date)), FUN = function(x){x/sum(x)})  
  df_cons_norm_mat = df_cons_norm$x
  # distance_tras <- pracma::distmat(X = t(df_cons_norm_mat[2, ]), Y = df_gen_norm_mat[2, ])
  distance_mat = pracma::distmat(X = df_cons_norm_mat, Y = df_gen_norm_mat)
  distance_sunny = c(t(distance_mat))[df_local_time$sunny]
  distance = mean(distance_sunny)
  distance_per_user[i] = distance
}

df_cons_sunny_ordered_2 = df_cons_sunny[, order(distance_per_user)]

# 3: self_consumption
self_consumption_per_user = c()
for (i in 1:ncol(df_cons)) {
  df_cons_sunny_selected = df_cons_sunny[, i]
  self_consumption_no_limits = df_gen_sunny/df_cons_sunny_selected
  self_consumption = ifelse(self_consumption_no_limits > 1, 1, self_consumption_no_limits)
  self_consumption = mean(self_consumption)
  self_consumption_per_user[i] = self_consumption
}

df_cons_sunny_ordered_3 = df_cons_sunny[, order(self_consumption_per_user)]


# 
list_cons_sunny_ordering = list("free" = df_cons_sunny_free, 
                                "hourly_mean" = df_cons_sunny_ordered_1, 
                                "dist_solar_cons" = df_cons_sunny_ordered_2, 
                                "self_consumption" = df_cons_sunny_ordered_3)

list_combination = list("free" = list(), "hourly_mean" = list(), "dist_solar_cons" = list(), "self_consumption" = list())

j = 1
for (ordering in names(list_cons_sunny_ordering)) {
  for (i in 1:n_iter) {
    if (ordering == "free") {
      df_cons_sunny_ordering = list_cons_sunny_ordering[[ordering]][[j]]
      j = j + 1
    } else{
      df_cons_sunny_ordering = list_cons_sunny_ordering[[ordering]]      
    }

    combination = optimization_1_to_analize(n_community = n_community, n_binary_rep = n_binary_rep, df_gen_to_optimize = df_gen_sunny, df_cons_to_optimize = df_cons_sunny_ordering, max_iter = max_iter)

    names_combination = which(colnames(df_cons_sunny) %in% colnames(df_cons_sunny_ordering)[which(combination == 1)])
    combination_aux = rep(0, times=ncol(df_cons_sunny))
    combination_aux[names_combination] = 1
    
    list_combination[[ordering]][[i]] = combination_aux  
  }
  
  df_list_combination_ordering = as.data.frame(list_combination[[ordering]])
  colnames(df_list_combination_ordering) = paste0("iter_",1:n_iter)
  surplus = colSums(apply(X = as.matrix(df_list_combination_ordering), MARGIN = 2, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny))
  surplus = as.character(round(surplus, digits = 2))
  table_surplus_ordering = table(surplus)
  
  plot_table_convergence(name = ordering, table_surplus_ordering)

}


############################# graph: convergence vs iteration #############################
# convergence graph
# TODO: working here to have a "mean" (for all the iterations) convergence graph for each of the orderings
# will not have a mean, will just draw all the lines one on top of the other

max_run = 40
n_iter = 20

list_best_combinations = list("free" = list(), "hourly_mean" = list(), "dist_solar_cons" = list(), "self_consumption" = list())
list_best_surplus = list("free" = list(), "hourly_mean" = list(), "dist_solar_cons" = list(), "self_consumption" = list())
list_time = list("free" = list(), "hourly_mean" = list(), "dist_solar_cons" = list(), "self_consumption" = list())

j = 1
for (ordering in names(list_cons_sunny_ordering)) {
  for (i in 1:n_iter) {
    if (ordering == "free") {
      df_cons_sunny_ordering = list_cons_sunny_ordering[[ordering]][[j]]
      j = j + 1
    } else{
      df_cons_sunny_ordering = list_cons_sunny_ordering[[ordering]]     
    }

    # PROBLEM: for ordering = "hourly_mean" the results have different length    
    results = optimization_1_to_analize_convergence(n_community = n_community, n_binary_rep = n_binary_rep, df_gen_to_optimize = df_gen_sunny, df_cons_to_optimize = df_cons_sunny_ordering, max_run = max_run)
  
    best_combinations = results[["best_combinations"]] 
    time = as.numeric(results[["time"]])
    
    names_combination =  apply(X = best_combinations, MARGIN = 1, FUN = function(x){
        which(colnames(df_cons_sunny) %in% colnames(df_cons_sunny_ordering)[which(x == 1)])
    })
    
    for (X in names(names_combination)) {
      if (length(names_combination[[X]]) < n_community) {
        names_combination[[X]][n_community] = 0
      }
    }

    names_combination = as.data.frame(names_combination)
    
    combination_aux = matrix(0, nrow = ncol(names_combination), ncol = ncol(df_cons_sunny))
    
    for (k in 1:ncol(names_combination)) {
      combination_aux[k, names_combination[, k]] = 1 
    }
  
    list_best_combinations[[ordering]][[i]] = combination_aux
  
    surplus = colSums(apply(X = as.matrix(best_combinations), MARGIN = 1, FUN = calculate_surplus_hourly_community, df_gen = df_gen_sunny, df_cons = df_cons_sunny_ordering))
    # list_best_surplus[[ordering]][[i]] = data.frame("iter" = 1:length(surplus),
    #                                                 "surplus" = surplus)
    
    list_best_surplus[[ordering]][[i]] = data.frame(t(surplus))
    colnames(list_best_surplus[[ordering]][[i]]) = 1:length(surplus)
        
  }
}  

plot_iterations_convergence_surplus(list_best_surplus)

############################# comparisons #############################

# esta va a ser mi idea:
# 1) voy a probar distintos modelos de planning que me van a dar distintos resultados (estaría bien que fueran mínimo 3 resultados) (el último puede ser definiendo las cajitas de forma distinta)
# 2) pruebo convergencia para los 3 modelos
# 3) de ahí aplico la optimización 2 y veo los resultados (para que los resultados sean consistentes entonces el payback tiene que estar bien calculado si o si (no solo tomar la mean!))




