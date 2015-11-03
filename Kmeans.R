data <- iris[,1:2]
kmean(data,3)
data[,3] <- as.factor(data[,3])
colnames(data)[3] <- "cluster"


require(ggplot2)
p <- ggplot(data, aes(Sepal.Length, Sepal.Width))
p + geom_point((aes(colour = cluster)))
#p + geom_point((aes(colour = factor(cluster))))





## ----- L2distance -----------------------------------##
L2distance <- function(point1, point2) {
  #( (point1[1,1] - point2[1,1])^2 + (point1[1,2] - point2[1,2])^2 )
  sum(point1-point2)^2
}


##----- assign cluster labels -------------------------##
assign <- function(mat, centroids, k){
  dist <- matrix(0,nrow=1,ncol=k)
  r <- dim(mat)[1]
  for (i in c(1:r)) {
    for (j in c(1:k)) {
      dist[1,j] <- L2distance(mat[i,1:2],centroids[j,])
    }
    mat[i,3] <- which.min(dist)
  }
  return(mat[,3])
}

##----- recalculate centroids -------------------------##
barrycenter <- function(matrix,centernumber){
  xcount = 0; ycount = 0; count = 0; r <- dim(matrix)[1]
  for (i in c(1:r)) {
    if (matrix[i,3] == centernumber) {
      xcount = xcount + matrix[i,1]
      ycount = ycount + matrix[i,2]
      count = count + 1
    }
  }
  return(c(xcount/count, ycount/count))
}


##----- main kmeans method ----------------------------##
kmean <- function(data,k) {
  r <- dim(data)[1]
  minmax <- range(data[,1])[2]-range(data[,1])[1] + range(data[,2])[2]-range(data[,2])[1]
  threshold <- ceiling(log(minmax, base=10))/100
  
  #iteration <- 0 test
  
  # initial centroid
  cent.index <- floor( (r/k) * c(1:3) ) #floor(runif(k,0,r))
  centroids <- as.matrix(data)[cent.index,1:2]
  dimnames(centroids) <- NULL
  
  # distance matrix for updating cluster label
  dist <- matrix(0,nrow=1,ncol=k)
  
  #do-while loop to do(repeat) while(change in centroids stead state)
  repeat {
    #update cluster label based on new centroids
    data[,3] <- assign(data,centroids,k)
    #iteration <- iteration + 1 test
    
    temp <- centroids
    #update centroids
    for (i in c(1:k)) {
      centroids[i,] <- barrycenter(data,i)
    }
    change <- sum((temp - centroids)^2)
    # print(centroids) test
    if(change <= threshold) break
  }
  return(data[,3])
  # print(iteration) test
}