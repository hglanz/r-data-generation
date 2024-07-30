library(scatterplot3d)

#' Generate Regression Dataset
#'
#' This function generates a regression dataset suitable for testing linear regression models. It allows for customization of the number of samples, features, informative features, targets, noise, and other parameters to simulate various regression scenarios.
#'
#' @param n_samples Total number of samples to generate. Default is 100.
#' @param n_features Total number of features. Non-informative features are generated as noise. Default is 100.
#' @param n_informative Number of informative features, i.e., features actually used to build the linear model. Default is 10.
#' @param n_targets Number of targets (response variables) to generate. Default is 1.
#' @param bias Intercept term in the underlying linear model. Default is 0.0.
#' @param noise Standard deviation of Gaussian noise added to the output. Default is 0.0.
#' @param shuffle Logical, indicating whether to shuffle the samples. Default is TRUE.
#' @param coef Logical, indicates whether to return the coefficients of the underlying linear model along with the dataset. Default is FALSE.
#' @param random_state Integer, the seed used by the random number generator for reproducibility. Can be `NULL`. Default is `NULL`.
#' @param effective_rank The approximate number of singular vectors required to explain most of the input data by linear combinations. Relevant for simulating data with collinear features. Default is `NULL`.
#' @param tail_strength The relative importance of the fat noisy tail of the singular values profile if `effective_rank` is not `NULL`. Affects how quickly the importance of singular vectors falls off. Default is 0.5.
#' @param plot Logical, indicating whether to plot the dataset. Supported only for datasets with a single target. Default is FALSE.
#' @return A list containing the feature matrix 'X', the target vector 'Y', and optionally the true coefficients if `coef` is TRUE.
#' @examples
#' # Generate a simple regression dataset and plot it
#' simple_regression <- make_regression(plot = TRUE)
#'
#' # Generate a regression dataset with more informative features and plot it
#' informative_regression <- make_regression(n_features=200, n_informative=50, plot=TRUE)
#'
#' # Generate a multi-output regression dataset and plot it
#' multi_output_regression <- make_regression(n_targets=2, plot=TRUE)
#' @export
make_regression <- function(n_samples=100, n_features=100, n_informative=10, n_targets=1,
                            bias=0.0, noise=0.0, shuffle=TRUE, coef=FALSE, random_state=NULL,
                            effective_rank=NULL, tail_strength=0.5, plot = FALSE) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }

  # random data values
  # initial feature matrix (X) with more features than the effective rank
  X <- matrix(rnorm(n_samples * n_features), nrow=n_samples, ncol=n_features)

  # implementing effective rank and tail_strength
  if (!is.null(effective_rank) && effective_rank < n_features) {
    # decompose a matrix X into its singular vectors and singular values
    svd_result <- svd(X)
    U <- svd_result$u
    Sigma <- svd_result$d
    V <- svd_result$v

    # for matrix X to have an effective_rank of k, keep the top k singular values and set the rest to zero.
    # Reduces the rank of the matrix, simulating a situation where only a subset of features provides most information.
    k <- effective_rank
    Sigma_adjusted <- c(Sigma[1:k], rep(0, length(Sigma) - k))

    #  linearly reducing the magnitude of each successive singular value from the largest to the kth value
    # adjust each of the top k singular values by a factor that decreases linearly
    decay_factor <- seq(from = 1, to = tail_strength, length.out = k)
    Sigma_adjusted[1:k] <- Sigma_adjusted[1:k] * decay_factor

    # reconstructing the matrix
    X_adjusted <- U %*% diag(Sigma_adjusted) %*% t(V)
    X <- X_adjusted
  }

  # target data. Y = intercept + coeff * X + error
  if (n_targets == 1) {
    Y <- matrix(nrow=n_samples, ncol=1)
  } else {
    Y <- matrix(nrow=n_samples, ncol=n_targets)
  }

  coefficients <- matrix(rnorm(n_informative), ncol = 1)

  informative_X <- X[, seq(n_informative), drop = FALSE]

  for (target in 1:n_targets) {
    Y[, target] <- bias + informative_X %*% coefficients + rnorm(n_samples, sd=noise)
  }

  # shuffle if requested
  if (shuffle) {
    indices <- sample(n_samples)
    X <- X[indices, ]
    Y <- Y[indices, ]
  }

  result <- if (coef) {
    list("X"=X, "Y"=Y, "coefficients"=coefficients)
  } else {
    list("X"=X, "Y"=Y)
  }

  if (plot && n_features >= 2 && n_targets == 1) {
    if (n_features == 2) {
      scatterplot3d(X[,1], X[,2], Y, color="#0072B2", pch=19,
                    xlab="Feature 1", ylab="Feature 2", zlab="Target", main="Generated 3D Regression Data")
    } else {
      plot(X[,1], Y, col="#0072B2", xlab="Feature 1", ylab="Target", pch=19, main="Generated 2D Regression Data")
    }
  }

  return(result)
}

