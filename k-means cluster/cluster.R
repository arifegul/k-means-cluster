

choseRandomCentroids <- function(points, numberOfCentroids) {
  randomX <- runif(numberOfCentroids, min(points[, 'x']), max(points[, 'x']))  
  randomY <- runif(numberOfCentroids, min(points[, 'y']), max(points[, 'y']))  
  
  matrix(c(randomX, randomY), ncol = 2)
}

# points: a set of (x,y) points
# return the central point of teh region limited by the points of the input.
computeCentralPoint <- function(points) {
  xCentral <- (min(points[, 'x']) + max(points[, 'x'])) /2
  yCentral <- (min(points[, 'y']) + max(points[, 'y'])) /2
  
  c(xCentral, yCentral)
}


# - clusters: is a data.frame with two columns: second column has the
#   index of a given point in 'points', and the first column has the
#   identifier for a cluster.
# - points : a set of (x, y) points
# - numberOfCentroids : number of centroids
# return - return the central point for each cluster 
chooseCentroids <- function(clusters, points, numberOfCentroids) {
  centroids <- data.frame()
  
  for (i in 1:numberOfCentroids) {
    filteredPointsIndexes <- which(clusters$centroidIndex == i)
    filteredDataSet <- points[filteredPointsIndexes,]
    
    newCentroid <- computeCentralPoint(filteredDataSet)
    centroids <- rbind(centroids, newCentroid)
  }
  
  centroids
}
computeDistanceBetweenPoints <- function (centroid, point) {
  sqrt((centroid[1]-point[1])^2 + (centroid[2]-point[2])^2)
}

computeDistanceForCentroids <- function(point, centroids) {
  distances <- c()
  
  for (i in 1:nrow(centroids)) {
    centroid <- centroids[i,]
    distance <- computeDistanceBetweenPoints(centroid, point)
    
    distances <- c(distances, distance)
  }
  
  distances
}


# this function receives an array like [1.2, 5.5, 0.4]
# this input means that the centroid-1 is within 1.2 for a point p,
# centroid-2 is within 5.5 for a point p, and centroid-3 is within 0.4 
# for a point p.
# then, this function will return 3, because 3 is the index (the number that identifies)
# the closest centroid
chooseIndexOfClosestCentroid <- function(centroidsDistances) {
  which(centroidsDistances %in% min(centroidsDistances))[1]
}



clustering <- function(data, centroids) {
  myCluster <- data.frame()
  
  for (i in 1:nrow(data)) {
    point <- data[i,]
    distances <- computeDistanceForCentroids(point, centroids)
    closestCentroidIndex <- chooseIndexOfClosestCentroid(distances)
    myCluster <- rbind(myCluster, c(closestCentroidIndex, i))
  }
  
  colnames(myCluster) <- c('centroidIndex', 'pointIndex')
  myCluster
}

# this function is awful. :(
areGroupsEqual <- function(group1, group2) {
  if (is.null(dim(group1))) {
    return(FALSE)
  }
  
  
  for (i in 1:nrow(group1)) {
    for (j in 1:ncol(group1[1,])) {
      if (group1[i,j] != group2[i,j]) {
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}
# plot the graphic on screen
doThePainting <- function(myCluster, randomPoints) {
  colorMap = c('red', 'green', 'blue', 'black', 'gray')
  
  for (i in 1:nrow(myCluster)) {
    index <- myCluster[i,2]
    points(randomPoints[index,1], randomPoints[index,2], col=colorMap[myCluster[i,1]], pch = 19)
  }
}


# this function executes k-means and plot each iteration on a graphic, 
# to visualize how it works.
#
# data: a set of (x,y) points
# numberOfCentroids : numberOfCentroids
# return the data clustered in 'numberOfCentroids' 
mykmeans <- function(data, numberOfCentroids) {
  groups <- NULL
  
  centroids <- choseRandomCentroids(data, numberOfCentroids)
  newGroups <- clustering(data, centroids)
  
  while (!areGroupsEqual(groups, newGroups)) {
    doThePainting(newGroups, data)
    Sys.sleep(1)
    groups <- newGroups
    centroids <- chooseCentroids(groups, data, numberOfCentroids)
    newGroups <- clustering(data, centroids)
  }
  
  groups
}

data <- matrix(runif(100, 0, 10), ncol = 2)
colnames(data) <- c('x', 'y')
plot(data)
centroidNumber <- 4
mykmeans(data, centroidNumber)

scaled <- scale(data)
w <- (nrow(scaled)-1)*sum(apply(scaled,2,var))

for (i in 2:15) w[i] <- sum(kmeans(scaled,centers=i)$withinss)
plot(1:15, w, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
