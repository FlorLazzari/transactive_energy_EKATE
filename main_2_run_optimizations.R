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
# list_best_surplus_tunned = list("free" = list(), "hourly_mean" = list(), "dist_solar_cons" = list(), "self_consumption" = list())
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
    
    last_best_surplus = as.numeric(list_best_surplus[[ordering]][[i]][length(list_best_surplus[[ordering]][[i]])])

    # TODO: forced selection of the first row
    selected_combination = results[["selected_combination"]][1,] 
    surplus = sum(calculate_surplus_hourly_community(combination = selected_combination, df_gen = df_gen_sunny, df_cons = df_cons_sunny_ordering))

    # list_best_surplus_tunned[[ordering]][[i]] = list_best_surplus[[ordering]][[i]]

    if (list_best_surplus[[ordering]][[i]][length(list_best_surplus[[ordering]][[i]])] > 400){
      # if (as.numeric(list_best_surplus_tunned[[ordering]][[i]][diff(as.numeric(list_best_surplus[[ordering]][[i]])) > 0][1]) == surplus) {
        # index_trouble = which((diff(as.numeric(list_best_surplus[[ordering]][[i]])) > 0))[1]
        # list_best_surplus_tunned[[ordering]][[i]][index_trouble:length(list_best_surplus_tunned[[ordering]][[i]])] = surplus
        # print("solved")
      # } else{
        print("problem")
        # browser()
      # }
    }
  }
}  

plot_iterations_convergence_surplus(name = "trouble_solved", list_best_surplus)

# testing:

nrow(list_best_combinations[["free"]][[14]])
length(list_best_surplus[["free"]][[14]])

# where the problem starts:
# rowSums(list_best_combinations[["free"]][[14]][115:118, ])
# list_best_surplus[["free"]][[14]][115:118]
# list_best_combinations[["free"]][[14]][115:118, ]


which(list_best_combinations[["free"]][[14]][1, ]==1)
which(list_best_combinations[["free"]][[14]][80, ]==1)

which(list_best_combinations[["free"]][[14]][110, ]==1)

which(list_best_combinations[["free"]][[14]][114, ]==1)
which(list_best_combinations[["free"]][[14]][115, ]==1)

sum(list_best_combinations[["free"]][[14]][116, ])
list_best_surplus[["free"]][[14]][116]
which(list_best_combinations[["free"]][[14]][116, ]==1)

sum(list_best_combinations[["free"]][[14]][117, ])
list_best_surplus[["free"]][[14]][117]
which(list_best_combinations[["free"]][[14]][117, ]==1)

sum(list_best_combinations[["free"]][[14]][118, ])
list_best_surplus[["free"]][[14]][118]
which(list_best_combinations[["free"]][[14]][118, ]==1)

for (j in 1:nrow(list_best_combinations[[ordering]][[i]])) {
  print(j)
  print(which(list_best_combinations[[ordering]][[i]][j, ]==1))  
}

GA | iter = 1 | Mean = -2947.7107 | Best =  -341.6059
GA | iter = 2 | Mean = -2885.6255 | Best =  -191.6441
GA | iter = 3 | Mean = -2671.7911 | Best =  -191.6441
GA | iter = 4 | Mean = -2496.9451 | Best =  -191.6441
GA | iter = 5 | Mean = -2406.8094 | Best =  -191.6441
GA | iter = 6 | Mean = -2369.6773 | Best =  -143.1461
GA | iter = 7 | Mean = -2318.2675 | Best =  -143.1461
GA | iter = 8 | Mean = -2258.9479 | Best =  -143.1461
GA | iter = 9 | Mean = -2189.9621 | Best =  -143.1461
GA | iter = 10 | Mean = -2093.47628 | Best =   -81.00086
GA | iter = 11 | Mean = -2118.66338 | Best =   -81.00086
GA | iter = 12 | Mean = -2028.75382 | Best =   -81.00086
GA | iter = 13 | Mean = -1978.81827 | Best =   -81.00086
GA | iter = 14 | Mean = -1905.70146 | Best =   -81.00086
GA | iter = 15 | Mean = -1869.08377 | Best =   -57.09624
GA | iter = 16 | Mean = -1829.64328 | Best =   -57.09624
GA | iter = 17 | Mean = -1811.55682 | Best =   -57.09624
GA | iter = 18 | Mean = -1768.719 | Best =   -34.930
GA | iter = 19 | Mean = -1803.135 | Best =   -34.930
GA | iter = 20 | Mean = -1777.154 | Best =   -34.930
GA | iter = 21 | Mean = -1814.755 | Best =   -34.930
GA | iter = 22 | Mean = -1742.415 | Best =   -34.930
GA | iter = 23 | Mean = -1756.951 | Best =   -34.930
GA | iter = 24 | Mean = -1696.427 | Best =   -34.930
GA | iter = 25 | Mean = -1626.905 | Best =   -34.930
GA | iter = 26 | Mean = -1609.204 | Best =   -34.930
GA | iter = 27 | Mean = -1512.607 | Best =   -34.930
GA | iter = 28 | Mean = -1545.17670 | Best =   -16.93627
GA | iter = 29 | Mean = -1565.92075 | Best =   -16.93627
GA | iter = 30 | Mean = -1511.00530 | Best =   -16.93627
GA | iter = 31 | Mean = -1532.32525 | Best =   -16.93627
GA | iter = 32 | Mean = -1509.59486 | Best =   -16.93627
GA | iter = 33 | Mean = -1511.53543 | Best =   -16.93627
GA | iter = 34 | Mean = -1533.89109 | Best =   -16.93627
GA | iter = 35 | Mean = -1596.42072 | Best =   -16.93627
GA | iter = 36 | Mean = -1700.28552 | Best =   -16.93627
GA | iter = 37 | Mean = -1617.59598 | Best =   -16.93627
GA | iter = 38 | Mean = -1610.56804 | Best =   -16.93627
GA | iter = 39 | Mean = -1627.50401 | Best =   -16.93627
GA | iter = 40 | Mean = -1606.83251 | Best =   -16.93627
GA | iter = 41 | Mean = -1554.16093 | Best =   -16.93627
GA | iter = 42 | Mean = -1504.79723 | Best =   -16.93627
GA | iter = 43 | Mean = -1558.5217 | Best =   -13.5444
GA | iter = 44 | Mean = -1661.4590 | Best =   -13.5444
GA | iter = 45 | Mean = -1674.7729 | Best =   -13.5444
GA | iter = 46 | Mean = -1684.8860 | Best =   -13.5444
GA | iter = 47 | Mean = -1590.9054 | Best =   -13.5444
GA | iter = 48 | Mean = -1567.2051 | Best =   -13.5444
GA | iter = 49 | Mean = -1513.2162 | Best =   -13.5444
GA | iter = 50 | Mean = -1479.3770 | Best =   -13.5444
GA | iter = 51 | Mean = -1423.4942 | Best =   -13.5444
GA | iter = 52 | Mean = -1344.4081 | Best =   -13.5444
GA | iter = 53 | Mean = -1381.1079 | Best =   -13.5444
GA | iter = 54 | Mean = -1398.90801 | Best =   -13.40565
GA | iter = 55 | Mean = -1371.53400 | Best =   -13.40565
GA | iter = 56 | Mean = -1476.04517 | Best =   -13.40565
GA | iter = 57 | Mean = -1442.45555 | Best =   -13.40565
GA | iter = 58 | Mean = -1505.78190 | Best =   -13.40565
GA | iter = 59 | Mean = -1503.20709 | Best =   -13.40565
GA | iter = 60 | Mean = -1439.81585 | Best =   -13.40565
GA | iter = 61 | Mean = -1461.33345 | Best =   -13.40565
GA | iter = 62 | Mean = -1502.56353 | Best =   -13.40565
GA | iter = 63 | Mean = -1565.39890 | Best =   -13.40565
GA | iter = 64 | Mean = -1617.63101 | Best =   -13.40565
GA | iter = 65 | Mean = -1596.48137 | Best =   -13.40565
GA | iter = 66 | Mean = -1623.99650 | Best =   -13.40565
GA | iter = 67 | Mean = -1553.83893 | Best =   -13.40565
GA | iter = 68 | Mean = -1566.81471 | Best =   -13.40565
GA | iter = 69 | Mean = -1530.0225 | Best =   -11.2925
GA | iter = 70 | Mean = -1570.7954 | Best =   -11.2925
GA | iter = 71 | Mean = -1435.5094 | Best =   -11.2925
GA | iter = 72 | Mean = -1443.7968 | Best =   -11.2925
GA | iter = 73 | Mean = -1484.6374 | Best =   -11.2925
GA | iter = 74 | Mean = -1437.5562 | Best =   -10.0519
GA | iter = 75 | Mean = -1380.5475 | Best =   -10.0519
GA | iter = 76 | Mean = -1370.4976 | Best =   -10.0519
GA | iter = 77 | Mean = -1367.1730 | Best =   -10.0519
GA | iter = 78 | Mean = -1291.4760 | Best =   -10.0519
GA | iter = 79 | Mean = -1328.6910 | Best =   -10.0519
GA | iter = 80 | Mean = -1330.1555 | Best =   -10.0519
GA | iter = 81 | Mean = -1357.0905 | Best =   -10.0519
GA | iter = 82 | Mean = -1433.9072 | Best =    -7.8644
GA | iter = 83 | Mean = -1400.1159 | Best =    -7.8644
GA | iter = 84 | Mean = -1409.9071 | Best =    -7.8644
GA | iter = 85 | Mean = -1404.3197 | Best =    -7.8644
GA | iter = 86 | Mean = -1370.0239 | Best =    -7.8644
GA | iter = 87 | Mean = -1408.1651 | Best =    -7.8644
GA | iter = 88 | Mean = -1483.2241 | Best =    -7.8644
GA | iter = 89 | Mean = -1420.0918 | Best =    -7.8644
GA | iter = 90 | Mean = -1446.1793 | Best =    -7.8644
GA | iter = 91 | Mean = -1464.9941 | Best =    -7.8644
GA | iter = 92 | Mean = -1396.6453 | Best =    -7.8644
GA | iter = 93 | Mean = -1408.9111 | Best =    -7.8644
GA | iter = 94 | Mean = -1428.6397 | Best =    -7.8644
GA | iter = 95 | Mean = -1339.8219 | Best =    -7.8644
GA | iter = 96 | Mean = -1379.8014 | Best =    -7.8644
GA | iter = 97 | Mean = -1406.2289 | Best =    -7.8644
GA | iter = 98 | Mean = -1463.7506 | Best =    -7.8644
GA | iter = 99 | Mean = -1581.5398 | Best =    -7.8644
GA | iter = 100 | Mean = -1518.8770 | Best =    -7.8644
GA | iter = 101 | Mean = -1516.4420 | Best =    -7.8644
GA | iter = 102 | Mean = -1448.4352 | Best =    -7.8644
GA | iter = 103 | Mean = -1392.1715 | Best =    -7.8644
GA | iter = 104 | Mean = -1333.7214 | Best =    -7.8644
GA | iter = 105 | Mean = -1389.9332 | Best =    -7.8644
GA | iter = 106 | Mean = -1458.4530 | Best =    -7.8644
GA | iter = 107 | Mean = -1529.9621 | Best =    -7.8644
GA | iter = 108 | Mean = -1554.9390 | Best =    -7.8644
GA | iter = 109 | Mean = -1559.9889 | Best =    -7.8644
GA | iter = 110 | Mean = -1481.1432 | Best =    -7.8644
GA | iter = 111 | Mean = -1487.8408 | Best =    -7.8644
GA | iter = 112 | Mean = -1376.4416 | Best =    -7.8644
GA | iter = 113 | Mean = -1347.2453 | Best =    -7.8644
GA | iter = 114 | Mean = -1388.2839 | Best =    -7.8644
GA | iter = 115 | Mean = -1411.5802 | Best =    -7.8644
GA | iter = 116 | Mean = -1447.5343 | Best =    -7.8644
GA | iter = 117 | Mean = -1441.7950 | Best =    -7.8644
GA | iter = 118 | Mean = -1397.765523 | Best =    -5.562942
GA | iter = 119 | Mean = -1474.040320 | Best =    -5.562942
GA | iter = 120 | Mean = -1423.856425 | Best =    -5.562942
GA | iter = 121 | Mean = -1427.923111 | Best =    -5.562942
GA | iter = 122 | Mean = -1478.034469 | Best =    -5.562942
GA | iter = 123 | Mean = -1506.922566 | Best =    -5.562942
GA | iter = 124 | Mean = -1543.158363 | Best =    -5.562942
GA | iter = 125 | Mean = -1636.475618 | Best =    -5.562942
GA | iter = 126 | Mean = -1650.677843 | Best =    -5.562942
GA | iter = 127 | Mean = -1455.938095 | Best =    -5.562942
GA | iter = 128 | Mean = -1443.614374 | Best =    -5.562942
GA | iter = 129 | Mean = -1432.163268 | Best =    -5.562942
GA | iter = 130 | Mean = -1403.137650 | Best =    -5.562942
GA | iter = 131 | Mean = -1435.325985 | Best =    -4.887525
GA | iter = 132 | Mean = -1414.560967 | Best =    -4.887525
GA | iter = 133 | Mean = -1323.791160 | Best =    -4.887525
GA | iter = 134 | Mean = -1455.565506 | Best =    -4.887525
GA | iter = 135 | Mean = -1426.873059 | Best =    -4.887525
GA | iter = 136 | Mean = -1506.436435 | Best =    -4.887525
GA | iter = 137 | Mean = -1438.460940 | Best =    -4.887525
GA | iter = 138 | Mean = -1489.262037 | Best =    -4.887525
GA | iter = 139 | Mean = -1427.670466 | Best =    -4.887525
GA | iter = 140 | Mean = -1304.218162 | Best =    -4.887525
GA | iter = 141 | Mean = -1351.727529 | Best =    -4.887525
GA | iter = 142 | Mean = -1471.266466 | Best =    -4.887525
GA | iter = 143 | Mean = -1462.110713 | Best =    -4.887525
GA | iter = 144 | Mean = -1365.609070 | Best =    -4.887525
GA | iter = 145 | Mean = -1390.853798 | Best =    -4.887525
GA | iter = 146 | Mean = -1427.117007 | Best =    -4.887525
GA | iter = 147 | Mean = -1311.018824 | Best =    -4.887525
GA | iter = 148 | Mean = -1374.546259 | Best =    -4.887525
GA | iter = 149 | Mean = -1385.567328 | Best =    -4.887525
GA | iter = 150 | Mean = -1391.385531 | Best =    -4.887525
GA | iter = 151 | Mean = -1448.797915 | Best =    -4.887525
GA | iter = 152 | Mean = -1444.812073 | Best =    -4.887525
GA | iter = 153 | Mean = -1427.630777 | Best =    -4.887525
GA | iter = 154 | Mean = -1475.026281 | Best =    -4.887525
GA | iter = 155 | Mean = -1442.410323 | Best =    -4.887525
GA | iter = 156 | Mean = -1475.155726 | Best =    -3.755442
GA | iter = 157 | Mean = -1565.951190 | Best =    -3.755442
GA | iter = 158 | Mean = -1548.127588 | Best =    -3.755442
GA | iter = 159 | Mean = -1474.107025 | Best =    -3.755442
GA | iter = 160 | Mean = -1541.977692 | Best =    -3.755442
GA | iter = 161 | Mean = -1599.521344 | Best =    -3.755442
GA | iter = 162 | Mean = -1508.960499 | Best =    -3.755442
GA | iter = 163 | Mean = -1476.948681 | Best =    -3.755442
GA | iter = 164 | Mean = -1535.392025 | Best =    -3.755442
GA | iter = 165 | Mean = -1639.348339 | Best =    -3.755442
GA | iter = 166 | Mean = -1641.509630 | Best =    -3.755442
GA | iter = 167 | Mean = -1695.474546 | Best =    -3.755442
GA | iter = 168 | Mean = -1604.212295 | Best =    -3.755442
GA | iter = 169 | Mean = -1661.022954 | Best =    -3.755442
GA | iter = 170 | Mean = -1575.102322 | Best =    -3.755442
GA | iter = 171 | Mean = -1599.583421 | Best =    -3.755442
GA | iter = 172 | Mean = -1609.171019 | Best =    -3.755442
GA | iter = 173 | Mean = -1588.492733 | Best =    -3.755442
GA | iter = 174 | Mean = -1502.668872 | Best =    -3.755442
GA | iter = 175 | Mean = -1496.132525 | Best =    -3.755442
GA | iter = 176 | Mean = -1510.300797 | Best =    -3.755442
GA | iter = 177 | Mean = -1630.578726 | Best =    -3.755442
GA | iter = 178 | Mean = -1608.468860 | Best =    -3.755442
GA | iter = 179 | Mean = -1579.484041 | Best =    -3.755442
GA | iter = 180 | Mean = -1662.387801 | Best =    -3.755442
GA | iter = 181 | Mean = -1652.956367 | Best =    -3.755442
GA | iter = 182 | Mean = -1607.975934 | Best =    -3.755442
GA | iter = 183 | Mean = -1636.241187 | Best =    -3.755442
GA | iter = 184 | Mean = -1635.557937 | Best =    -3.755442
GA | iter = 185 | Mean = -1613.336804 | Best =    -3.755442
GA | iter = 186 | Mean = -1564.073606 | Best =    -3.755442
GA | iter = 187 | Mean = -1521.827120 | Best =    -3.755442
GA | iter = 188 | Mean = -1548.664794 | Best =    -3.755442
GA | iter = 189 | Mean = -1421.720369 | Best =    -3.755442
GA | iter = 190 | Mean = -1406.246127 | Best =    -3.755442
GA | iter = 191 | Mean = -1367.618778 | Best =    -3.755442
GA | iter = 192 | Mean = -1405.612833 | Best =    -3.755442
GA | iter = 193 | Mean = -1419.958933 | Best =    -3.755442
GA | iter = 194 | Mean = -1502.494517 | Best =    -3.755442
GA | iter = 195 | Mean = -1431.911304 | Best =    -3.755442



GA | iter = 1 | Mean = -2977.6645 | Best =  -338.4735
GA | iter = 2 | Mean = -2796.0570 | Best =  -260.0018
GA | iter = 3 | Mean = -2666.6818 | Best =  -231.2426
GA | iter = 4 | Mean = -2561.8996 | Best =  -231.2426
GA | iter = 5 | Mean = -2366.1293 | Best =  -221.8248
GA | iter = 6 | Mean = -2241.2774 | Best =  -201.4311
GA | iter = 7 | Mean = -2077.8420 | Best =  -122.5596
GA | iter = 8 | Mean = -1986.7486 | Best =  -122.5596
GA | iter = 9 | Mean = -1938.52217 | Best =   -76.68495
GA | iter = 10 | Mean = -1890.97429 | Best =   -67.07271
GA | iter = 11 | Mean = -1835.03685 | Best =   -67.07271
GA | iter = 12 | Mean = -1812.56148 | Best =   -67.07271
GA | iter = 13 | Mean = -1719.47407 | Best =   -67.07271
GA | iter = 14 | Mean = -1721.36889 | Best =   -39.07788
GA | iter = 15 | Mean = -1657.97923 | Best =   -39.07788
GA | iter = 16 | Mean = -1644.67705 | Best =   -39.07788
GA | iter = 17 | Mean = -1680.32654 | Best =   -39.07788
GA | iter = 18 | Mean = -1694.82435 | Best =   -39.07788
GA | iter = 19 | Mean = -1670.38394 | Best =   -39.07788
GA | iter = 20 | Mean = -1687.27874 | Best =   -39.07788
GA | iter = 21 | Mean = -1616.80307 | Best =   -39.07788
GA | iter = 22 | Mean = -1560.43441 | Best =   -36.75701
GA | iter = 23 | Mean = -1499.63118 | Best =   -36.75701
GA | iter = 24 | Mean = -1474.52424 | Best =   -36.75701
GA | iter = 25 | Mean = -1466.63761 | Best =   -27.30298
GA | iter = 26 | Mean = -1434.79660 | Best =   -27.30298
GA | iter = 27 | Mean = -1425.51003 | Best =   -27.30298
GA | iter = 28 | Mean = -1439.94509 | Best =   -27.30298
GA | iter = 29 | Mean = -1411.27732 | Best =   -27.30298
GA | iter = 30 | Mean = -1430.24603 | Best =   -27.30298
GA | iter = 31 | Mean = -1377.22444 | Best =   -27.30298
GA | iter = 32 | Mean = -1360.14562 | Best =   -27.30298
GA | iter = 33 | Mean = -1418.31763 | Best =   -27.30298
GA | iter = 34 | Mean = -1442.24946 | Best =   -27.30298
GA | iter = 35 | Mean = -1444.28278 | Best =   -25.83211
GA | iter = 36 | Mean = -1368.18743 | Best =   -25.83211
GA | iter = 37 | Mean = -1383.06849 | Best =   -25.83211
GA | iter = 38 | Mean = -1396.78027 | Best =   -25.83211
GA | iter = 39 | Mean = -1369.64160 | Best =   -25.83211
GA | iter = 40 | Mean = -1327.95105 | Best =   -25.83211
GA | iter = 41 | Mean = -1362.45449 | Best =   -25.83211
GA | iter = 42 | Mean = -1400.58301 | Best =   -25.83211
GA | iter = 43 | Mean = -1411.92942 | Best =   -21.17153
GA | iter = 44 | Mean = -1478.64261 | Best =   -21.17153
GA | iter = 45 | Mean = -1425.56558 | Best =   -20.14878
GA | iter = 46 | Mean = -1399.13690 | Best =   -20.14878
GA | iter = 47 | Mean = -1380.09221 | Best =   -13.83752
GA | iter = 48 | Mean = -1291.10194 | Best =   -13.83752
GA | iter = 49 | Mean = -1363.39660 | Best =   -12.29041
GA | iter = 50 | Mean = -1331.85707 | Best =   -12.29041
GA | iter = 51 | Mean = -1253.35613 | Best =   -12.29041
GA | iter = 52 | Mean = -1260.01849 | Best =   -12.29041
GA | iter = 53 | Mean = -1294.84159 | Best =   -12.29041
GA | iter = 54 | Mean = -1289.40999 | Best =   -12.29041
GA | iter = 55 | Mean = -1199.46624 | Best =   -12.29041
GA | iter = 56 | Mean = -1241.54261 | Best =   -12.29041
GA | iter = 57 | Mean = -1315.09586 | Best =   -12.29041
GA | iter = 58 | Mean = -1361.95736 | Best =   -12.29041
GA | iter = 59 | Mean = -1353.94998 | Best =   -12.29041
GA | iter = 60 | Mean = -1374.82135 | Best =   -12.29041
GA | iter = 61 | Mean = -1416.72942 | Best =   -12.29041
GA | iter = 62 | Mean = -1437.57514 | Best =   -12.29041
GA | iter = 63 | Mean = -1370.21710 | Best =   -12.29041
GA | iter = 64 | Mean = -1355.82746 | Best =   -12.29041
GA | iter = 65 | Mean = -1300.12282 | Best =   -12.29041
GA | iter = 66 | Mean = -1241.12347 | Best =   -12.29041
GA | iter = 67 | Mean = -1234.915600 | Best =    -8.491954
GA | iter = 68 | Mean = -1287.638191 | Best =    -8.491954
GA | iter = 69 | Mean = -1306.088648 | Best =    -8.491954
GA | iter = 70 | Mean = -1274.132598 | Best =    -8.491954
GA | iter = 71 | Mean = -1313.619225 | Best =    -8.491954
GA | iter = 72 | Mean = -1378.918936 | Best =    -8.491954
GA | iter = 73 | Mean = -1429.440785 | Best =    -8.491954
GA | iter = 74 | Mean = -1424.028508 | Best =    -8.491954
GA | iter = 75 | Mean = -1359.591318 | Best =    -5.562942
GA | iter = 76 | Mean = -1417.330556 | Best =    -5.562942
GA | iter = 77 | Mean = -1474.826101 | Best =    -5.562942
GA | iter = 78 | Mean = -1423.566802 | Best =    -5.562942
GA | iter = 79 | Mean = -1418.952381 | Best =    -5.562942
GA | iter = 80 | Mean = -1388.414606 | Best =    -5.562942
GA | iter = 81 | Mean = -1352.168409 | Best =    -5.562942
GA | iter = 82 | Mean = -1334.990143 | Best =    -5.562942
GA | iter = 83 | Mean = -1307.288093 | Best =    -5.562942
GA | iter = 84 | Mean = -1249.806160 | Best =    -3.755442
GA | iter = 85 | Mean = -1280.164508 | Best =    -3.755442
GA | iter = 86 | Mean = -1240.087277 | Best =    -3.755442
GA | iter = 87 | Mean = -1186.923073 | Best =    -3.755442
GA | iter = 88 | Mean = -1155.102325 | Best =    -3.755442
GA | iter = 89 | Mean = -1119.945903 | Best =    -3.755442
GA | iter = 90 | Mean = -1113.970924 | Best =    -3.755442
GA | iter = 91 | Mean = -1078.950237 | Best =    -3.755442
GA | iter = 92 | Mean = -1083.057155 | Best =    -3.755442
GA | iter = 93 | Mean = -1095.008838 | Best =    -3.755442
GA | iter = 94 | Mean = -1183.607599 | Best =    -3.755442
GA | iter = 95 | Mean = -1262.288786 | Best =    -3.755442
GA | iter = 96 | Mean = -1231.964243 | Best =    -3.755442
GA | iter = 97 | Mean = -1294.368550 | Best =    -3.755442
GA | iter = 98 | Mean = -1257.407909 | Best =    -3.755442
GA | iter = 99 | Mean = -1233.236878 | Best =    -3.755442
GA | iter = 100 | Mean = -1290.397038 | Best =    -3.755442
GA | iter = 101 | Mean = -1298.283946 | Best =    -3.755442
GA | iter = 102 | Mean = -1261.705792 | Best =    -3.755442
GA | iter = 103 | Mean = -1263.975738 | Best =    -3.755442
GA | iter = 104 | Mean = -1326.683362 | Best =    -3.755442
GA | iter = 105 | Mean = -1299.064121 | Best =    -3.755442
GA | iter = 106 | Mean = -1332.411272 | Best =    -3.755442
GA | iter = 107 | Mean = -1397.118232 | Best =    -3.755442
GA | iter = 108 | Mean = -1339.700552 | Best =    -3.755442
GA | iter = 109 | Mean = -1364.459680 | Best =    -3.755442
GA | iter = 110 | Mean = -1357.644094 | Best =    -3.755442
GA | iter = 111 | Mean = -1343.249751 | Best =    -3.755442
GA | iter = 112 | Mean = -1313.964391 | Best =    -3.755442
GA | iter = 113 | Mean = -1335.618326 | Best =    -3.755442
GA | iter = 114 | Mean = -1360.079255 | Best =    -3.755442
GA | iter = 115 | Mean = -1431.422162 | Best =    -3.755442
GA | iter = 116 | Mean = -1408.586107 | Best =    -3.755442
GA | iter = 117 | Mean = -1367.603635 | Best =    -3.755442
GA | iter = 118 | Mean = -1341.895234 | Best =    -3.755442
GA | iter = 119 | Mean = -1228.243645 | Best =    -3.755442
GA | iter = 120 | Mean = -1178.570466 | Best =    -3.755442
GA | iter = 121 | Mean = -1166.303280 | Best =    -3.755442
GA | iter = 122 | Mean = -1174.381588 | Best =    -3.755442
GA | iter = 123 | Mean = -1143.991302 | Best =    -3.755442


############################# comparisons #############################

# esta va a ser mi idea:
# 1) voy a probar distintos modelos de planning que me van a dar distintos resultados (estaría bien que fueran mínimo 3 resultados) (el último puede ser definiendo las cajitas de forma distinta)
# 2) pruebo convergencia para los 3 modelos
# 3) de ahí aplico la optimización 2 y veo los resultados (para que los resultados sean consistentes entonces el payback tiene que estar bien calculado si o si (no solo tomar la mean!))




