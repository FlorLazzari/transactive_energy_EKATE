genome = runif(900, 0, 1)
hist(genome)

genome_norm = normalize_genome(genome, 
                               n_community, 
                               n_sunny_hours = 9, 
                               dim = 90, 
                               popSize = 100)

hist(genome_norm)

genome_dirichlet = rdirichlet(n = popSize, a = rep(1/10, times = n_community))
hist(genome_dirichlet)
rowSums(genome_dirichlet)

a = rdirichlet(n = 2, a = rep(2, 3))

###############################
# trying crossover:
library(nsga2R)

fn <- function(x){
  p = x/sum(x)
  score_1 = p[1]*10 + p[4]*5 + p[2]*0 + p[3]*0  
  score_2 = p[1]*10 + p[4]*5 + p[2]*0 + p[3]*0  
  return(c(-score_1, -score_2))
}

# parent = c(0.8, 0, 0, 0.2) 
lowerBounds = c(0, 0, 0, 0)
upperBounds = c(1, 1, 1 , 1)
popSize = 10
varNo = 4
objDim = 2
tourSize = 2 #check this!
cprob = 0.8
XoverDistIdx = 5 #check this!
mprob = 0.2
MuDistIdx = 10

parent = t(sapply(1:popSize, function(u) array(runif(
                                          length(lowerBounds),
                                          lowerBounds, 
                                          upperBounds))))
parent[1, ] = c(0.8, 0, 0, 0.2) 
parent[2, ] = c(0.08, 0, 0, 0.02) 
parent[3, ] = c(0.9, 0, 0, 0.2) 
parent[4, ] = c(0.08, 0, 0, 0.02) 
parent[5, ] = c(0.9, 0, 0, 0.2) 
parent[6, ] = c(0.08, 0, 0, 0.02) 
parent[7, ] = c(0.8, 0, 0, 0.2) 
parent[8, ] = c(0.08, 0, 0, 0.02) 
parent[9, ] = c(0.9, 0, 0, 0.2) 
parent[10, ] = c(0.09, 0, 0, 0.02) 



parent <- cbind(parent, t(apply(parent, 1, fn)))


ranking <- fastNonDominatedSorting(parent[, (varNo + 1):
                                            (varNo + objDim)])
rnkIndex <- integer(popSize)

i <- 1
while (i <= length(ranking)) {
  rnkIndex[ranking[[i]]] <- i
  i <- i + 1
}
parent <- cbind(parent, rnkIndex)

objRange <- apply(parent[, (varNo + 1):(varNo + objDim)], 
                  2, max) - apply(parent[, (varNo + 1):(varNo + objDim)], 
                                  2, min)
cd <- crowdingDist4frnt(parent, ranking, objRange)
parent <- cbind(parent, apply(cd, 1, sum))

matingPool <- tournamentSelection(parent, popSize, tourSize)

childAfterX <- boundedSBXover(matingPool[, 1:varNo], 
                              lowerBounds, upperBounds, cprob, XoverDistIdx)

childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, 
                                   upperBounds, mprob, MuDistIdx)

childAfterM <- cbind(childAfterM, t(apply(childAfterM, 
                                          1, fn)))

parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)

ranking <- fastNonDominatedSorting(parentNext[, (varNo + 
                                                   1):(varNo + objDim)])
i <- 1
while (i <= length(ranking)) {
  rnkIndex[ranking[[i]]] <- i
  i <- i + 1
}
parentNext <- cbind(parentNext, rnkIndex)
objRange <- apply(parentNext[, (varNo + 1):(varNo + 
                                              objDim)], 2, max) - apply(parentNext[, (varNo + 
                                                                                        1):(varNo + objDim)], 2, min)
cd <- crowdingDist4frnt(parentNext, ranking, objRange)

parentNext <- cbind(parentNext, apply(cd, 1, sum))
parentNext.sort <- parentNext[order(parentNext[, varNo + 
                                                 objDim + 1], -parentNext[, varNo + objDim + 2]), 
                              ]
# cat("environmental selection")
# cat("\n")
parent <- parentNext.sort[1:popSize, ]


# mi conclusión de todo esto es que usando una 
# cprob (proba de crossover) baja entonces debería andar bien :)



####################################################################

crossover_function_1 <- function(parent_1, parent_2, r_1, r_2){
  print(parent_1/sum(parent_1))
  print(parent_2/sum(parent_2))
  
  offspring_1 = r_1 * parent_1 + (r_1 - 1) * parent_2
  offspring_2 = (r_2 - 1) * parent_1 + (r_2) * parent_2  
  
  # print(offspring_1)
  # print(offspring_2)

  offspring_1 = ifelse(offspring_1 > 1 , 1, offspring_1)
  offspring_1 = ifelse(offspring_1 < 0 , 0, offspring_1)
  
  offspring_2 = ifelse(offspring_2 > 1 , 1, offspring_2)
  offspring_2 = ifelse(offspring_2 < 0 , 0, offspring_2)

  print(offspring_1/sum(offspring_1))
  print(offspring_2/sum(offspring_2))
  return()
}

crossover_function_2 <- function(parent_1, parent_2, r_1, r_2){

  parent_1 = parent_1/sum(parent_1)
  parent_2 = parent_2/sum(parent_2)
  
  offspring_1 = r_1 * parent_1 + (r_1 - 1) * parent_2
  offspring_2 = (r_2 - 1) * parent_1 + (r_2) * parent_2  
  
  offspring_1 = ifelse(offspring_1 > 1 , 1, offspring_1)
  offspring_1 = ifelse(offspring_1 < 0 , 0, offspring_1)

  offspring_2 = ifelse(offspring_2 > 1 , 1, offspring_2)
  offspring_2 = ifelse(offspring_2 < 0 , 0, offspring_2)
  
  # print(offspring_1)
  # print(offspring_2)
  
  print(offspring_1/sum(offspring_1))
  print(offspring_2/sum(offspring_2))
  return()
}

r_1 = runif(1, 0, 1)
r_2 = runif(1, 0, 1)

crossover_function_1(parent_1 = c(0.1, 0.0001, 0.0001, 0.01), 
                     parent_2 = c(0.8, 0.001, 0.001, 0.1), 
                     r_1 = r_1, r_2 = r_2)

crossover_function_2(parent_1 = c(0.1, 0.0001, 0.0001, 0.01), 
                     parent_2 = c(0.8, 0.001, 0.001, 0.1), 
                     r_1 = r_1, r_2 = r_2)

parent_chromosome = c()
boundedSBXover(parent_chromosome, lowerBounds, upperBounds, cprob, mu) 


# esta idea de que parent_1 primero se multiplica por "r_1" y después por "r_2 - 1" 
# me parece muy ineresante.. pensar un poco en esto... 
# se podría usar esta forma de equilibrar para plantear el random init (?)

