library(scatterplot3d)


make_blobs <- function(n_samples=100, n_features=2, centers=NULL, cluster_std=1.0,
                       center_box=c(-10, 10), shuffle=TRUE, random_state=NULL, return_centers=FALSE, plot=FALSE) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }
  
  if (is.null(centers)) {
    n_centers <- 3
  } else if (is.numeric(centers)) {
    n_centers <- centers
    centers <- matrix(runif(n_centers * n_features, min=center_box[1], max=center_box[2]), nrow=n_centers)
  } else {
    n_centers <- nrow(centers)
  }
  
  samples_per_center <- ceiling(n_samples / n_centers)
  cluster_labels <- rep(1:n_centers, each=samples_per_center)[1:n_samples]
  
  X <- matrix(nrow=n_samples, ncol=n_features)
  for (i in 1:n_centers) {
    center <- if (is.null(centers)) {
      runif(n_features, min=center_box[1], max=center_box[2])
    } else {
      centers[i, ]
    }
    indices <- which(cluster_labels == i)
    X[indices, ] <- mapply(rnorm, n=length(indices), mean=center, sd=cluster_std)
  }
  
  if (shuffle) {
    indices <- sample(n_samples)
    X <- X[indices, ]
    cluster_labels <- cluster_labels[indices]
  }
  
  if (plot) {
    if (n_features == 2) {
      colors <- rainbow(n_centers)
      plot(X, col=colors[cluster_labels], pch=19, xlab="Feature 1", ylab="Feature 2", main="Generated Blob Clusters")
      if (return_centers) {
        points(centers, pch=4, col=1:n_centers, lwd=2)
      }
    } else if (n_features == 3) {
      colors <- rainbow(n_centers)
      scatterplot3d(X[,1], X[,2], X[,3], color=colors[cluster_labels], pch=19, xlab="Feature 1", ylab="Feature 2", zlab="Feature 3", main="Generated Blob Clusters")
      if (return_centers) {
        scatterplot3d(centers[,1], centers[,2], centers[,3], color=1:n_centers, pch=4, lwd=2, add=TRUE)
      }
    } else {
      warning("Plotting is available only for 2D and 3D data.")
    }
  }
  
  if (return_centers) {
    return(list("X"=X, "y"=cluster_labels, "centers"=centers))
  } else {
    return(list("X"=X, "y"=cluster_labels))
  }
}

# Test Cases
# Test case 1: Default settings
make_blobs(plot=TRUE)

# Test case 2: Increased number of samples and 4 clusters
make_blobs(n_samples=200, centers=4, plot=TRUE)

# error
# Test case 3: Custom cluster centers and higher variability
custom_centers <- matrix(c(0, 0, 5, 5, -5, -5), byrow=TRUE, ncol=2)
make_blobs(n_samples=150, centers=custom_centers, cluster_std=2, plot=TRUE, random_state=24)

# Test case 4: 3D data (plotting disabled for 3D)
make_blobs(n_samples=100, n_features=3, centers=3, plot=TRUE)

# Note: Remember to set plot=FALSE for non-2D data as the plotting functionality is designed for 2D datasets only.
