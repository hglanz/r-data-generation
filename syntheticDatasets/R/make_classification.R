library(scatterplot3d)

okabe_ito_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                      "#0072B2", "#D55E00", "#CC79A7", "#999999")

#' Generate Synthetic Classification Dataset
#'
#' This function generates a random n-class classification problem. This dataset is useful for testing classification algorithms. It can simulate datasets with various levels of complexity and class separation.
#'
#' @param n_samples Total number of samples to generate.
#' @param n_features Total number of features. Features are divided into informative, redundant (linear combinations of the informative features), and noise features.
#' @param n_informative Number of informative features, i.e., features actually influencing the class labels.
#' @param n_redundant Number of redundant features.
#' @param n_repeated Number of duplicated informative features.
#' @param n_classes Number of classes (or labels) for classification.
#' @param n_clusters_per_class Number of clusters per class.
#' @param weights A vector of proportions of samples assigned to each class. If `NULL`, classes are balanced.
#' @param flip_y Proportion of samples whose class is randomly flipped.
#' @param class_sep Separation factor between the classes. A larger value spreads the classes apart.
#' @param scale Scaling factor for the features. Default is 1.0.
#' @param shuffle Logical, indicating whether to shuffle the samples.
#' @param random_state Integer, the seed used by the random number generator for reproducibility.
#' @param plot Logical, indicating whether to plot the dataset. Supported only for datasets with 2 or more features.
#' @return A list containing the feature matrix 'X' and the vector 'labels' of class labels for each sample.
#' @examples
#' # Generate and plot a simple classification dataset
#' data <- make_classification(n_samples=100, n_features=2, n_informative=2, n_classes=2, plot=TRUE)
#'
#' # Generate a more complex dataset with 3 classes and plot it
#' complex_data <- make_classification(n_samples=150, n_features=4, n_informative=3, n_classes=3, n_clusters_per_class=1, plot=TRUE)
#' @export
make_classification <- function(n_samples = 100, n_features = 20, n_informative = 2,
                                n_redundant = 2, n_repeated = 0, n_classes = 2,
                                n_clusters_per_class = 2, class_sep = 1.0, flip_y = 0.01,
                                weights = NULL, random_state = NULL, shuffle = TRUE, scale = 1.0, plot = FALSE) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }

  # Check total features
  total_features <- n_informative + n_redundant + n_repeated
  if (total_features > n_features) {
    stop("Total number of informative, redundant, and repeated features must be <= n_features")
  }

  # Calculate the number of samples per class
  if (is.null(weights)) {
    weights <- rep(1/n_classes, n_classes)
  }
  samples_per_class <- round(weights * n_samples)

  # Adjust the last class to ensure the total number of samples is correct
  samples_per_class[length(samples_per_class)] <- n_samples - sum(samples_per_class[-length(samples_per_class)])

  # Generate the feature matrix
  X <- matrix(0, nrow = n_samples, ncol = n_features)
  labels <- numeric(n_samples)

  start_idx <- 1

  for (class in 0:(n_classes - 1)) {
    end_idx <- start_idx + samples_per_class[class + 1] - 1

    for (cluster in 1:n_clusters_per_class) {
      cluster_center <- rnorm(n_informative, mean = class * class_sep, sd = 1.0)
      cluster_size <- round(samples_per_class[class + 1] / n_clusters_per_class)
      if (cluster == n_clusters_per_class) {
        cluster_size <- end_idx - start_idx + 1
      }

      cluster_samples <- matrix(rnorm(cluster_size * n_informative, mean = rep(cluster_center, each = cluster_size)), nrow = cluster_size, ncol = n_informative)
      X[start_idx:(start_idx + cluster_size - 1), 1:n_informative] <- cluster_samples
      labels[start_idx:(start_idx + cluster_size - 1)] <- class
      start_idx <- start_idx + cluster_size
    }
  }

  # Generate redundant features
  if (n_redundant > 0) {
    coeffs <- matrix(runif(n_informative * n_redundant), ncol = n_redundant)
    redundant_features <- X[, 1:n_informative] %*% coeffs
    X <- cbind(X, redundant_features)
  }

  # Add repeated features
  if (n_repeated > 0) {
    repeated_indices <- sample(n_informative + n_redundant, n_repeated, replace = TRUE)
    repeated_features <- X[, repeated_indices]
    X <- cbind(X, repeated_features)
  }

  # Ensure the number of features is exactly n_features
  X <- X[, 1:n_features]

  # Add noise features if needed to complete the number of features
  if (ncol(X) < n_features) {
    noise_features <- matrix(rnorm(n_samples * (n_features - ncol(X))), nrow = n_samples)
    X <- cbind(X, noise_features)
  }

  # Scale features
  if (!is.null(scale)) {
    X <- X * scale
  }

  # Randomly flip a proportion of the labels
  flip_indices <- sample(n_samples, size = round(flip_y * n_samples))
  labels[flip_indices] <- sample(0:(n_classes-1), length(flip_indices), replace = TRUE)

  # Shuffle if requested
  if (shuffle) {
    indices <- sample(n_samples)
    X <- X[indices, ]
    labels <- labels[indices]
  }

  # Plot if needed
  if (plot) {
    if (n_classes > length(okabe_ito_colors)) {
      stop("Number of classes exceeds available colors in Okabe-Ito palette.")
    }
    colors <- okabe_ito_colors[1:n_classes]
    if (n_features == 3) {
      scatterplot3d(X[, 1], X[, 2], X[, 3], color = colors[labels + 1], pch = 19,
                    xlab = "Feature 1", ylab = "Feature 2", zlab = "Feature 3",
                    main = "Generated 3D Classification Plot")
    } else if (n_features == 2) {
      plot(X[, 1], X[, 2], col = colors[labels + 1], pch = 19,
           xlab = "Feature 1", ylab = "Feature 2", main = "Generated 2D Classification Plot")
      legend("topright", legend = paste("Class", 0:(n_classes-1)), fill = colors)
    } else {
      warning("Plotting requires 2 or 3 features.")
    }
  }

  return(list("X" = X, "labels" = labels))
}
