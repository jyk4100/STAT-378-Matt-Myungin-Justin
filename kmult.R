##--- L2distance ------------------------------------------##
# find the L2 distance between two vectors
# vec1 : first vector
# vec2 : second vector
L2distance <- function(vec1, vec2) {
  sqrt(sum(vec1-vec2)^2)
}


##--- assign cluster labels --------------------------------##
# assign each data entry of the input matrix "mat" to one of the "k" clusters
# according to the centroid (mean)
# mat : input matrix
# centroids : centroids/mean for labeling
# k : number of clusters
# adj : paramter for column index matching
assign <- function(mat, centroids, k, adj = 1){
  dist <- matrix(0,nrow=1,ncol=k)  # distance matrix
  r <- dim(mat)[1]; c<-dim(mat)[2];   # dimensions
  label <- numeric(r)
  for (i in c(1:r)) { 
    for (j in c(1:k)) {
      dist[1,j] <- L2distance(mat[i,1:c-adj],centroids[j,])
      # compute the distance bewteen each r data entry and k means
    }
    label[i] <- which.min(dist)
    # assign to cluster of the mean with which
    # the distance between the entry and the mean is minimum
  }
  return(label)
}


##--- recalculate centroids --------------------------------##
# recalculate the mean given the updated cluster labeling for each cluster
# mat: input matrix
# centernumber : numbering of cluster (1 ~ k)
recenter <- function(mat,centernumber){
  r <- dim(mat)[1]; c <- dim(mat)[2] # dimensions
  # for loop version
  #  count = 0; newcenter <- numeric(c-1)
  #   for (i in c(1:r)) {
  #     if (mat[i,c] == centernumber) {
  #       newcenter <- newcenter + as.matrix(mat[i,(1:c-1)])
  #       dimnames(newcenter) <- NULL
  #       count = count + 1
  #     }
  #   }
  # using apply
  index <- mat[,c] == centernumber
  # subset the data entries that belong to each cluster
  return( apply(mat[index,(1:c-1)],2,sum) / sum(index) )
  # find the mean of each cluster
  # apply "sum" function over "mat[index,(1:c-1)]" (only featuers)
  # "2" (column) wise
}


##--- main kmeans method (simple iteration) ---------------------------##
# mat: input matrix
# k : number of cluters
kmeanloop <- function(mat,k) {
  r <- dim(mat)[1]; c <- dim(mat)[2] # dimensions
  
  ## non-random intial assignment for just tried
  ## cent.index <- floor( (r/k) * c(1:3) ) #floor(runif(k,0,r))
  ## centroids <- mat[cent.index,1:c]
  ## centroids <- as.matrix(centroids) #non numeric ?.. have to step by step
  ## dimnames(centroids) <- NULL

  # initial centroid (uniform random)
  max <- apply(mat,2,max); min <- apply(mat,2,min) 
  # max and min of the features
  centroids <- mapply(runif, k, min = min,max = max)
  # generate k random vectors of length = number of features
  # from uniform distribution of min and max of each features
  
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
    print(mat[,c+1]) #testing
  }
  return(mat[,c+1])
}

##---main kmeans method (dowhileloop) ----------------------------##
# mat: input matrix
# k : number of cluters
kmeansR <- function(mat,k) {
  r <- dim(mat)[1]; c <- dim(mat)[2] #dimensions of the matrix 
  adj <- 0 # for column number matching
  
  maxmin <- as.numeric(as.vector(apply(mat,2,max)[1:c])) - 
            as.numeric(as.vector(apply(mat,2,min)[1:c]))
  threshold <- 10^(4 - ceiling(log(sum(maxmin), base=10)))
  # my own threshold idk okay?
  # negative power of 10 based on the order of magnitude of min max difference and
    
  # initial centroid (uniform random)
  max <- apply(mat,2,max); min <- apply(mat,2,min)
  # max and min of the features
  centroids <- mapply(runif, k, min = min, max = max)
  # generate k random vectors of length = number of features
  # from uniform distribution of min and max of each features
  
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
    # find the change in mean of each clusters
    
    # print(centroids) # testing
    if(change <= threshold) break
  }
  return(mat[,c+1])
}

##-------------------------------------------------------------------------##


# #testcase
# data(iris);attach(iris);data <- iris;rm(iris);data <- data[,1:4]
# kmeanloop(data[,1:2],3)
# kmean(data[,1:2],3)



##--- graphics  -----------------------------------##

require(fpc); require(rattle)
data(wine); attach(wine)
wine <- wine[,2:14] # exclude categorical variable
wine[,14]<-kmeans(wine,3) 
colnames(wine)[14] <- "cluster"
plotcluster(wine, wine$cluster)



##--- heatmap  ----------------------------------##

data(iris);attach(iris);iris <- iris[,1:4]
scaled <- as.data.frame(scale(iris))
heatmap(as.matrix(scaled), Colv=NA, Rowv=NA,scale='none')

require(RColorBrewer)
scaled  <- as.matrix(scaled)
rc <- rainbow(nrow(scaled), start = 0, end = .3)
cc <- rainbow(ncol(scaled), start = 0, end = .3)
heatmap(scaled, Rowv=NA, Colv=NA, col=brewer.pal(9, "Blues")[1:9])



##--- its 2 am 0.0 ---------------------------------##
# cluster
iris[,5] <- kmeans(iris,3)
colnames(iris)[5] <- "cluster"
require(fpc); plotcluster(iris, iris$cluster)

clustered <- iris[order(iris$cluster),]
#clustered.scaled <- scale(clustered)
require(RColorBrewer)
#heatmap(clustered.scaled[,1:4], Rowv=NA, Colv=NA, col=brewer.pal(9, "Blues")[1:9])

require(pheatmap)
pheatmap(clustered[,1:4], cluster_rows=F,cluster_cols=F, col=brewer.pal(9, "Blues")[1:9], border_color=NA)
pheatmap(iris[,1:4], cluster_rows=F,cluster_cols=F, col=brewer.pal(9, "Blues")[1:9], border_color=NA)

#hmmmmm.......

##--- its 2 am 0.0 wine will be nice ---------------------------------##
require(rattle)
data(wine); attach(wine)
wine <- wine[,2:14] # exclude categorical variable
wine[,14]<-kmeansR(wine,3) 
wine[,15] <- kmeans(wine,3)$cluster
colnames(wine)[14] <- "cluster"

pheatmap(scale(wine[,1:13]), cluster_rows=F,cluster_cols=F, border_color=NA)
clustered.wine <- wine[order(wine$cluster),]
pheatmap(scale(clustered.wine[,1:13]), cluster_rows=F,cluster_cols=F, border_color=NA)


clustered.wine <- wine[order(wine$V15),]
pheatmap(scale(clustered.wine2[,1:13]), cluster_rows=F,cluster_cols=F, border_color=NA)

par(mfrow=c(1,2))
plotcluster(wine, wine$cluster)
plotcluster(wine, wine$V15)
