##---------- L2distance -----------------------------------##
L2distance <- function(point1, point2) {
  sqrt(sum(point1-point2)^2)
}


##---------- assign cluster labels -------------------------##
assign <- function(mat, centroids, k, adj = 1){
  dist <- matrix(0,nrow=1,ncol=k)
  r <- dim(mat)[1]; c<-dim(mat)[2]; #adj <- 0
  label <- numeric(r)
  for (i in c(1:r)) {
    for (j in c(1:k)) {
      dist[1,j] <- L2distance(mat[i,1:c-adj],centroids[j,])
    }
    label[i] <- which.min(dist)
  }
  return(label)
}



##---------- recalculate centroids -------------------------##
recenter <- function(mat,centernumber){
  r <- dim(mat)[1]; c <- dim(mat)[2]
  count = 0; newcenter <- numeric(c-1)
#   for (i in c(1:r)) {
#     if (mat[i,c] == centernumber) {
#       newcenter <- newcenter + as.matrix(mat[i,(1:c-1)])
#       dimnames(newcenter) <- NULL
#       count = count + 1
#     }
#   }
  index <- mat[,c] == centernumber
  return(apply(mat[index,(1:c-1)],2,sum) / sum(index) )
}



##---------- main kmeans method (simple iteration) ---------------------------##
kmeanloop <- function(mat,k) {
  r <- dim(mat)[1]; c <- dim(mat)[2];
  
  # initial centroid (uniform random)
  ## cent.index <- floor( (r/k) * c(1:3) ) #floor(runif(k,0,r))
  ## centroids <- mat[cent.index,1:c]
  ## centroids <- as.matrix(centroids) #non numeric ?.. have to step by step
  ## dimnames(centroids) <- NULL
  max <- apply(mat,2,max); min <- apply(mat,2,min)
  centroids <- mapply(runif, k, min = min,max = max)
  
  # distance matrix for updating cluster label
  dist <- matrix(0,nrow=1,ncol=k)
  
  # initial assignment of the clusters
  # (seperate b.c. restricting column numbers on L2 distance calculation later steps)
  mat[,c+1] <- assign(mat,centroids,k,adj=0)
  
  for (m in c(1:4)) {
    #update cluster label based on new centroids
    mat[,c+1] <- assign(mat,centroids,k,adj=1) #tjrtprtm! tprtm!
    
    temp <- centroids
    
    #update centroids
    for (i in c(1:k)) {
      centroids[i,] <- recenter(mat,i)
    }
    #     change <- sum((temp - centroids)^2)
    #     # print(centroids) #test
    #     if(change <= threshold) break
    print(mat[,c+1])
  }
  return(mat[,c+1])
}
##-------------------------------------------------------------------------##


#testcase
data(iris);paste(iris);data <- iris;rm(iris);data <- data[,1:4]
kmeanloop(data[,1:2],3)
# [1] 2 2 2 2 2 3 2 2 2 2 3 2 2 2 1 1 3 2 3 3 3 3 2 2 2 2 2 3 2 2 2 3 3 3 2
# [36] 2 3 2 2 2 2 2 2 2 3 2 3 2 3 2 1 3 1 2 3 2 3 2 3 2 2 3 2 3 2 1 2 2 2 2
# [71] 3 3 3 3 3 3 3 3 3 2 2 2 2 3 2 3 1 2 2 2 2 3 2 2 2 3 2 3 2 2 3 2 1 3 3
# [106] 1 2 1 3 1 3 3 1 2 2 3 3 1 1 2 1 2 1 3 1 1 3 3 3 1 1 1 3 3 3 1 3 3 3 1
# [141] 1 1 2 1 1 3 3 3 3 3
kmean(data[,1:2],3)


##---------- main kmeans method (dowhileloop) ----------------------------##
kmean <- function(mat,k) {
  r <- dim(mat)[1]; c <- dim(mat)[2]; adj <- 0
  minmax <- sum(apply(mat,2,max)[1:c] - apply(mat,2,min)[1:c])
  threshold <- ceiling(log(minmax, base=10))/100
  
  # initial centroid (uniform random)
  max <- apply(mat,2,max); min <- apply(mat,2,min)
  centroids <- mapply(runif, k, min = min,max = max)
  
  # distance matrix for updating cluster label
  dist <- matrix(0,nrow=1,ncol=k)
  
  # initial assignment of the clusters
  # (seperate b.c. restricting column numbers on L2 distance calculation later steps)
  mat[,c+1] <- assign(mat,centroids,k,adj=0)
  
  repeat {
    #update cluster label based on new centroids (1+i iterations)
    mat[,c+1] <- assign(mat,centroids,k,adj=1)
    
    temp <- centroids # for steady state check
    
    #update centroids
    for (i in c(1:k)) {
      centroids[i,] <- recenter(mat,i)
    }
    
    change <- sum((temp - centroids)^2)
    # print(centroids) #test
    if(change <= threshold) break
  }
  return(mat[,c+1])
}

# # initial centroid
# cent.index <- floor( (r/k) * c(1:3) ) #floor(runif(k,0,r))
# centroids <- mat[cent.index,1:c]
# centroids <- as.matrix(centroids) #non numeric .... have to stepwise
# dimnames(centroids) <- NULL
