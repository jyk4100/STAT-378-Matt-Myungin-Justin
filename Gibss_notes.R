##--- draw ------------------------------------------##
# Draw from the conditional distirbution using inverse transformation

draw <- function(lambda, B = 5){
  unif <- runif(1,0,1) # draw from unif(0,1) for transformations
  c <- 1/(1-exp(-lambda*B))
  # adjusting constant for making exponential 0 < x < B a density
  return((lambda^-1) * log(c/(c-unif))) # inverse transformations
}

# ##--- gibbs ------------------------------------------##
# # main sampling
# # chaing sampling from the x to y and y to x
# # since conditional distribution of x given y and y given x are equivalent
# # draw x_i from y_(i-1) and draw y_i from x_i
# # credit : Matthew Best
# 
# gibbs <- function(numIter = 500) {
#   m <- matrix(0, nrow = numIter, ncol = 2)
#   m[1,] <- runif(2)
#   for (iter in 2:numIter) {
#     m[iter,1] <- sample_conditional_distribution(m[iter-1, 2])
#     m[iter,2] <- sample_conditional_distribution(m[iter, 1])
#   }
#   return(m)
# }


## comparing inverse tranfom and mat's implementaion
set.seed(19920922); B <- 5; lambda <- 0.5
matt <- (qexp(runif(1000, 0, pexp(B, lambda)), lambda))

set.seed(19920922)
c <- 1/(1-exp(-lambda*B))
unif <- runif(1000,0,1)
justin <- (lambda^-1) * log(c/(c-unif))

sum(justin-matt)

par(mfrow=c(1,2))
hist(matt); hist(justin)
