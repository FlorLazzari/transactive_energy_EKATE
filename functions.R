calculate_surplus <- function(df_generation, df_cons, combination){
  not_selected_cons = is.na(combination) 
  df_consumption = df_cons
  df_consumption[, not_selected_cons] = 0
  coeffs <- combination/100 
  coeffs[is.na(coeffs)] = 0
  df_generation_assigned <- as.data.frame(t(t(df_generation) *  coeffs))  
  total_surplus <- rowSums(df_generation_assigned - df_consumption)
  hourly_surplus <- ifelse(total_surplus >= 0, total_surplus, 0)
  return(hourly_surplus)
}


optimizer <- function(X, class_per_feature, nclasses_per_feature, names_per_feature, levels_per_feature, 
                      df_generation, df_cons){
  
  # X = sample(c(0,1),nBits,replace=T)
  
  combination <- decodeValueFromBin(X, class_per_feature, nclasses_per_feature, levels_per_feature = levels_per_feature)
  combination <- as.numeric(combination)
  combination <- reconstruct_combination(combination) 
  
  hourly_surplus <- calculate_surplus(df_generation, df_cons, combination)
  
  # Proposed penalty (penalty if the surplus bound in violated)
  # delta <- surplus *
  
  # if (any(delta!=0)) {
  #   # Exponential parameter lambda
  #   lambda <- 2
  #   # Maximum delta allowed for each hour 
  #   delta_limit <- 3
  #   # (Both parameters should be hardcoded outside, during trial session are going to stay here)
  #   # The soft restrictions has been implemented the following way:
  #   # Exponential growth of penalty with homographic function to include the asymptote in the delta_limit
  #     penalty_function <- function(x, delta_limit, lambda){
  #        y = -exp(x)/(x-delta_limit)
  #        return(y)
  #     }
  #
  #   delta_penalty <- penalty_function(delta, delta_limit = 1.2, lamda)
  
  
  #   delta_penalty[delta_penalty<0] <- 500000
  #   penalty <- sum(delta_penalty)
  # }
  
  # Cost function: sum over 24 hours
  # check units (in price is euro/MWh and consumption Wh)??
  penalty = 0
  price = 1
  
  score <- sum(price * hourly_surplus) + penalty
  return(-score)
}

toBin <- function(x){ as.integer(paste(rev( as.integer(intToBits(x))),collapse="")) }

decodeValueFromBin <- function(binary_representation, class_per_feature, nclasses_per_feature, 
                               levels_per_feature = NULL, min_per_feature = NULL, max_per_feature = NULL){
  
  bitOrders <- mapply(function(x) { nchar(toBin(x)) }, nclasses_per_feature)
  #binary_representation <- X
  binary_representation <- split(binary_representation, rep.int(seq.int(bitOrders), times = bitOrders))
  orders <- sapply(binary_representation, function(x) { binary2decimal(gray2binary(x)) })
  orders <- mapply(function(x){min(orders[x],nclasses_per_feature[x])},1:length(orders))
  orders <- mapply(
    function(x){
      switch(class_per_feature[x],
             "discrete"= levels_per_feature[[x]][orders[x]+1],
             "int"= floor(seq(min_per_feature[x],max_per_feature[x],
                              by=if(nclasses_per_feature[x]>0){
                                (max_per_feature[x]-min_per_feature[x])/(nclasses_per_feature[x])
                              }else{1}
             )[orders[x]+1]),
             "float"= seq(min_per_feature[x],max_per_feature[x],
                          by=if(nclasses_per_feature[x]>0){
                            (max_per_feature[x]-min_per_feature[x])/(nclasses_per_feature[x])
                          }else{1})[orders[x]+1]
      )
    }
    ,1:length(orders))
  return(unname(orders))
}

reconstruct_combination <- function(combination){
  combination[is.na(combination)] = 0
  if (sum(is.na(combination))) {
    combination = c(50, NA, 50, NA)
  } else{
    combination = c(50, NA, 50, NA)
  }
   
  return(combination)
}



