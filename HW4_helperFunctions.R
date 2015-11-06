# Metropolis-Hastings algorithm ==============================================
proposal_function <- function(phi, c = 1) {
  # proposal_function(phi, c) will return a random draw from the proposal
  # distribution given curren observation, phi, and constant, c.
  prop <- 0
  reps <- 1
  maxReps <- 100000
  
  # The function `rbeta` can return either 0 or 1, but this shouldn't happen
  # so if either a 0 or 1 is generated, we will discard it and draw again.
  while ((prop == 0 | prop == 1) & reps < maxReps) {
    prop <- rbeta(1, c * phi, c * (1 - phi))
    reps <- reps + 1
  }
  if (reps == maxReps) {
    stop("Sampler is jammed.")
  }
  return(prop)
}

proposal_prob <- function(x, phi, c = 1) {
  # Returns the probability that a proposal, x, came from a beta distribution
  # with scale parameters c*phi, and c * (1-phi)
  return(dbeta(x, c * phi, c * (1 - phi)))
}

stationary_prob <- function(x, a = 6, b = 4) {
  # Returns the probability that an observation came from a beta distribution
  # with scale parameters a and b
  return(dbeta(x, a, b))
}

run_mh <- function(c = 1, startingPoint = runif(1), numIters = 10000, burnIn = 0, thinning = 0) {
  # run_mh(c, startingPoint, numIters, burnIn, thinning) will use the 
  # Metropolis-Hastings algorithm to sample from a beta distribution with
  # scale parameters a = 6 and b = 4.  The width parameter, c, adjusts the
  # width of the proposal distribution.  Optional arguments include the 
  # starting position of the Markov chain, startingPoint; the number of
  # iterations in the Markov chain, numIters; the number of samples to
  # discard for burn in, burnIn; and the number of samples that should be
  # discarded between consecutive draws of the chain, thinning.
  # The default value of thinning does not discard any samples.
  # run_mh returns the chain after discarding elements due to burn in and
  # thinning.
  m <- matrix( , nrow = numIters)
  xprev <- startingPoint
  
  for (iter in 1:numIters) {
    proposal <- proposal_function(xprev)
    
    a1 <- stationary_prob(proposal) / stationary_prob(xprev)
    a2 <- proposal_prob(xprev, proposal, c) / proposal_prob(proposal, xprev, c)
    
    a <- a1 * a2
    
    if (a > 1) {
      xprev <- proposal
    } else {
      if (runif(1) < a) {
        xprev <- proposal
      }
    }
    m[iter] <- xprev
  }
  m <- m[seq(burnIn+1, numIters, by=(thinning+1))]
  return(m)
}
# end of Metropolis-Hastings =================================================



# Gibbs sampling =============================================================
sample_conditional_distribution <- function(lambda, B = 5){
  # Draws a random sample from the conditional distribution of x given y or 
  # y given x
  return(qexp(runif(1, 0, pexp(B, lambda)), lambda))
}
###Report###
#We use the Inverse Transform method to draw a random sample value.
#Since x and y ranges over (0,B) for any given B>0, we need to cut off our domain for the quantile function.
#It turns out that our domain should be (0,F(B)) where F is the cdf of exponential(lamda), and clearly this interval is a subset of (0,1).
#We randomly pick any value from (0,F(B)) and if this random value is evaluated at exponential quantile, it will gives us some random value between 0 and B. Now, the function returns it.
############


run_gibbs <- function(numIter = 500) {
  # run_gibbs(numIter) draws numIter random samples using Gibbs sampling
  m <- matrix( , nrow = numIter, ncol = 2) #We first make a matrix which will contain our sample values.
  m[1,] <- runif(2) #first row will be filled with initial values, which is random in (0,1).
  for (iter in 2:numIter) {
    m[iter,1] <- sample_conditional_distribution(m[iter-1, 2])
    m[iter,2] <- sample_conditional_distribution(m[iter, 1])
  } #Iteratively, previously stored value will work as a lamda in the next rnadom sample.
  return(m)
}

# figures appears when sourcing not to be called 
# m1<-run_gibbs(500)
# m2<-run_gibbs(5000)
# m3<-run_gibbs(50000)
# #Store the result matrix for case T=500,5000, AND 50000 into m1,m2, and m3 respectively.
# hist(m1[,1])
# hist(m2[,1])
# hist(m3[,1])
# #Histogram of x values of each case
# #From the histogram, we see that the larger the sample values, the smoother the histogram is
# 
# #We will now calculate the approximate expected value:
# s1<-sum(m1[,1])
# s2<-sum(m2[,1])
# s3<-sum(m3[,1])
# #s1,s2, and s3 are sum of all the x-values for each case.
# mean1<-(1/500)*s1
# mean2<-(1/5000)*s2
# mean3<-(1/50000)*s3
# print(mean1)
# print(mean2)
# print(mean3)

#We can say the sample mean of these will converge in distribution to true expectation (near 1.27 in this case).
#Probability theory based on this approach is Strong Law of Large Numbers.
#We might argue that T=500,5000 and 50000 can be large enough (even we can say "almost infinite") to apply this theorem.
# end of Gibbs samping =======================================================



# K-Means ====================================================================
intersectionOverUnion <- function(groundTruthLabels, predictedLabels) {
  # intersectionOverUnion(groundTruthLabels, predictedLabels) computes the
  # intersection over union metric for multiclass classification problems.
  # This algorithm is commonly used in the field of computer vision when
  # characterizing the performance of image segmentation algorithms.
  # This metric lives in the interval (0,1], with higher values corresponding
  # to better classification.
  #
  # Briefly, for each class we compute the IOU as defined in section 5.4 of:
  # http://host.robots.ox.ac.uk:8080/pascal/VOC/voc2012/devkit_doc.pdf
  # The overall IOU is computed by taking a weighted average of each class'
  # IOU where the weight is equal to the proportion of data that truly belong
  # to a given class.
  
  numLevels <- nlevels(groundTruthLabels)
  IOU <- 0
  for (i in 1:numLevels) {
    IOU <- IOU + ((sum(groundTruthLabels == i & predictedLabels == i) /
                  sum(groundTruthLabels == i | predictedLabels == i)) * 
                  mean(groundTruthLabels == i))
  }
  return(IOU)
}
reorderClusts <- function(clust, newOrder) {
  # reorderClusts reassigns the values in cluster labels clust with the values
  # specified in the vector newOrder
  clust[clust == 1] = 4
  clust[clust == 2] = 5
  clust[clust == 3] = 6
  clust[clust == 4] = newOrder[1]
  clust[clust == 5] = newOrder[2]
  clust[clust == 6] = newOrder[3]
  return(clust)
}

findBestIOU <- function(groundTruthLabels, predictedLabels) {
  # findBestIOU(groundTruthLabels, predictedLabels) attempts to solve the 
  # cluster correspondence problem between ground truth and predicted
  # cluster labels. It tests each of the 6 possible orderings and
  # returns the best IOU of the 6 orderings
  curBest <- 0
  pl123 <- as.factor(reorderClusts(predictedLabels, c(1,2,3)))
  pl132 <- as.factor(reorderClusts(predictedLabels, c(1,3,2)))
  pl213 <- as.factor(reorderClusts(predictedLabels, c(2,1,3)))
  pl231 <- as.factor(reorderClusts(predictedLabels, c(2,3,1)))
  pl312 <- as.factor(reorderClusts(predictedLabels, c(3,1,2)))
  pl321 <- as.factor(reorderClusts(predictedLabels, c(3,2,1)))
  
  tmp <- intersectionOverUnion(groundTruthLabels, pl123)
  if (curBest < tmp) {
    curBest <- tmp
  }
  tmp <- intersectionOverUnion(groundTruthLabels, pl132)
  if (curBest < tmp) {
    curBest <- tmp
  }
  tmp <- intersectionOverUnion(groundTruthLabels, pl213)
  if (curBest < tmp) {
    curBest <- tmp
  }
  tmp <- intersectionOverUnion(groundTruthLabels, pl231)
  if (curBest < tmp) {
    curBest <- tmp
  }
  tmp <- intersectionOverUnion(groundTruthLabels, pl312)
  if (curBest < tmp) {
    curBest <- tmp
  }
  tmp <- intersectionOverUnion(groundTruthLabels, pl321)
  if (curBest < tmp) {
    curBest <- tmp
  }
  return(curBest)
}

ed <- function(x, y) {
  # returns the Euclidean distance between vectors x and y
  return(sqrt(sum((x-y)^2)))
}

pd2 <- function(x, y) {
  # Computes the pairwise euclidean distance between rows of 
  # the matrix x and rows of the matrix y.
  numX <- length(x[,1])
  numY <- length(y[,1])
  D <- matrix( , nrow = numX, ncol = numY)
  for (i in 1:numX) {
    for (j in 1:numY) {
      D[i,j] <- ed(x[i,], y[j,])
    }
  }
  return(D)
}

assignToCluster <- function(D) {
  # Find the index of the minimum entry in each row of the distance matrix D
  numObs <- length(D[,1])
  cluster <- matrix( , nrow = numObs)
  for (i in 1:numObs) {
    cluster[i] <- which.min(D[i,])
  }
  return(cluster)
}

updateClusterMeans <- function(data, cluster) {
  numC <- length(unique(cluster))
  clusterMeans <- matrix( , nrow = numC, ncol = length(data[1,]))
  for (c in unique(cluster)) {
    clusterMeans[c,] <- colMeans(data[cluster == c,])
  }
  return(clusterMeans)
}

km <- function(data, startingConditions, maxNumIters = 100) {
  # km(data, startingConditions) performs k-means clustering on data starting
  # from initial conditions in the variable startingConditions.  It returns a
  # label for each observation (row) in data.
  
  iter <- 1
  hasConverged <- F
  
  if (length(startingConditions == 1)) {
    oldClusterMeans <- data[sample(length(data), startingConditions),]
  } else {
    oldClusterMeans <- startingConditions
  }
  
  while (iter < maxNumIters & hasConverged == F) {
    cluster <- assignToCluster(pd2(data, oldClusterMeans))
    newClusterMeans <- updateClusterMeans(data, cluster)
    
    # Check for convergence; here the model is said to converge if the
    # cluster means do not move in subsequent iterations
    if (all(oldClusterMeans  == newClusterMeans)) {
      hasConverged = T
    } else {
      oldClusterMeans <- newClusterMeans
      iter <- iter + 1
    }
  }
  
  if (iter == maxNumIters & hasConverged == F) {
    warning("Algorithm failed to converge before iteration limit reached")
  } else {
    cat("Algorithm converged in ", iter, " iterations.\n")
  }
  return(cluster)
}
# end of K-Means =============================================================
