okabe_ito_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                      "#0072B2", "#D55E00", "#CC79A7", "#999999")

#' Generate Four Square Dataset
#'
#' This function generates a synthetic dataset that consists of points distributed in four squares.
#' This dataset can be used for testing clustering algorithms, visualization, and other data science tasks.
#' The dataset is divided into two classes, with each class occupying two opposite squares.
#'
#' @param n_samples Total number of samples to generate. Default is 100.
#' @param noise Standard deviation of Gaussian noise added to the data. Default is 0.0.
#' @param shuffle Logical, indicating whether to shuffle the samples. Default is TRUE.
#' @param random_state Integer, the seed used by the random number generator for reproducibility. Can be NULL. Default is NULL.
#' @param plot Logical, indicating whether to plot the dataset. Default is FALSE.
#' @return A list containing two elements: 'features' is a matrix of coordinates (x, y) of the points, and 'target' is a vector indicating the class (1 or 2) of each point.
#' @examples
#' # Generate a default four square dataset and plot it
#' dataset <- make_four_square_dataset(n_samples=1000, plot=TRUE)
#'
#' # Generate a dataset with noise and plot it
#' noisy_dataset <- make_four_square_dataset(n_samples=1000, noise=0.1, plot=TRUE)
#' @export
make_four_square <- function(n_samples=100, noise=0.0, shuffle=TRUE, random_state=NULL, plot=FALSE) {

  if (!is.null(random_state)) {
    set.seed(random_state)
  }

  n_samples_per_class <- n_samples / 4

  # first class (bottom left and top right)
  x1_class1_bottom_left <- runif(n_samples_per_class, min=0, max=0.5)
  x2_class1_bottom_left <- runif(n_samples_per_class, min=0, max=0.5)
  x1_class1_top_right <- runif(n_samples_per_class, min=0.5, max=1)
  x2_class1_top_right <- runif(n_samples_per_class, min=0.5, max=1)

  # second class (top left and bottom right)
  x1_class2_top_left <- runif(n_samples_per_class, min=0, max=0.5)
  x2_class2_top_left <- runif(n_samples_per_class, min=0.5, max=1)
  x1_class2_bottom_right <- runif(n_samples_per_class, min=0.5, max=1)
  x2_class2_bottom_right <- runif(n_samples_per_class, min=0, max=0.5)

  features <- rbind(
    cbind(x1_class1_bottom_left, x2_class1_bottom_left),
    cbind(x1_class1_top_right, x2_class1_top_right),
    cbind(x1_class2_top_left, x2_class2_top_left),
    cbind(x1_class2_bottom_right, x2_class2_bottom_right)
  )
  features <- features + matrix(rnorm(n_samples * 2, mean = 0, sd = noise), ncol = 2)

  target <- c(rep(1, n_samples_per_class * 2), rep(2, n_samples_per_class * 2))

  # shuffle the dataset if requested
  if (shuffle) {
    indices <- sample(n_samples)
    features <- features[indices, ]
    target <- target[indices]
  }

  if (plot) {
    colors <- okabe_ito_colors[1:2]
    plot(features, col=colors[target], pch=19, xlab="Feature 1", ylab="Feature 2", main = "Generated 4-Square Dataset")
    legend("topright", legend=c("Class 1", "Class 2"), fill=colors)
  }

  list("features" = features, "target" = target)
}



