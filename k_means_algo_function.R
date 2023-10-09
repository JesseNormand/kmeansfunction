#Create data

x <- rnorm(200,2.5,3)

y <- rnorm(200,1,1)

dat <- cbind(x,y)

#Set k parameter and centroids

k <- 3
ridx <- sample(1:nrow(dat), k, replace = FALSE)
centroids <- dat[ridx, , drop = FALSE]

#Create matrix for distance calculation


dists <- matrix(0, nrow(dat), k)

for (ci in 1:k) {
  dists[, ci] <- rowSums((dat - centroids[ci, ])^2)
}

dists

#Group the data with minimum distance from centroid

groupidx <- apply(dists, 1, which.min)


#Find the mean of grouped data and set new centroids

for (ki in 1:k) {
  centroids[ki, 1] <- mean(dat[groupidx == ki, 1])
  centroids[ki, 2] <- mean(dat[groupidx == ki, 2])
}

#Plot the data

plot(dat)

plot(dat, col = groupidx, pch = 19, main = "Data Points and Centroids", xlab = "X-Axis", ylab = "Y-Axis")

# Add centroids to the plot

points(centroids, col = 1:k, pch = 24, lwd = 10, cex = 2)

#Plot connecting lines from data to centriods

for (ki in 1:k) {
  group_data <- dat[groupidx == ki, ]
  for (i in 1:nrow(group_data)) {
    segments(x0 = centroids[ki, 1], y0 = centroids[ki, 2], x1 = group_data[i, 1], y1 = group_data[i, 2], col = ki)
  }
}








