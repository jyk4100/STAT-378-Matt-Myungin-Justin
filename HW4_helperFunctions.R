# Metropolis-Hastings algorithm ==============================================
proposal_function <- function(phi, c = 1) {
  # proposal_function(phi, c) will return a random draw from the proposal
  # distribution given curren observation, phi, and constant, c.
  prop <- 0
  reps <- 1
  maxReps <- 10000
  
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

run_gibbs <- function(numIter = 500) {
  # run_gibbs()
  m <- matrix( , nrow = numIter, ncol = 2)
  m[1,] <- runif(2)
  for (iter in 2:numIter) {
    m[iter,1] <- sample_conditional_distribution(m[iter-1, 2])
    m[iter,2] <- sample_conditional_distribution(m[iter, 1])
  }
  return(m)
}
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
    IOU <- IOU + (sum(groundTruthLabels == i & predictedLabels == i) /
                  sum(groundTruthLabels == i | predictedLabels == i)) * 
                  mean(groundTruthLabels == i)
    return(IOU)
  }
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
