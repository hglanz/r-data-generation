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

  # set the number of informative coefficients
  coefficients <- rnorm(n_informative)

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
  informative_X <- X[, seq(n_informative)]
  if (n_targets == 1) {
    Y <- matrix(nrow=n_samples, ncol=1)
  } else {
    Y <- matrix(nrow=n_samples, ncol=n_targets)
  }

  for (target in 1:n_targets) {
    Y[, target] <- bias + informative_X %*% coefficients + rnorm(n_samples, sd=noise)
  }

  # multi-output check
  if (n_targets > 1) {
    Y <- matrix(Y, ncol=n_targets)
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

  if (plot) {
    if (n_targets == 1) {
      plot(X[,1], Y, xlab="Feature 1", ylab="Target", pch = 19)
    } else {
      par(mfrow=c(1, n_targets))
      for (i in 1:n_targets) {
        plot(X[,1], Y[,i], xlab="Feature 1", ylab=paste("Target", i), pch = 19)
      }
    }
  }

  return(result)

}


# Testing

# Test Case 1: Default Parameters
make_regression(plot=TRUE)

# Test Case 2: Increased Sample Size
make_regression(n_samples=1000, plot=TRUE)

# Test Case 3: More Features
make_regression(n_features=200, n_informative=50, plot=TRUE)

# Test Case 4: Multi-Output Regression
make_regression(n_targets=2, plot=TRUE)

# Test Case 5: Adding Bias and
make_regression(bias=10, noise=5, plot=TRUE)

# Test Case 6: Effective rank and tail strength
make_regression(n_features=50, effective_rank=10, tail_strength=0.2, plot=TRUE)

# Test Case 7: No shuffling
make_regression(shuffle=FALSE, plot=TRUE)

# Test Case 8: Checking to see if it's returning coefficients
make_regression(coef=TRUE, plot=TRUE)

# Test Case 9: Specifying Random State
make_regression(random_state=42, plot=TRUE)

# Test Case 10: High Noise Level
make_regression(noise=20, plot=TRUE)


# Test Case 11: High effective rank with moderate tail strength
library(car)

data <- make_regression(n_samples=100, n_features=20, effective_rank=20, tail_strength=0.5)

df <- as.data.frame(data$X)
df$Y <- data$Y

model <- lm(Y ~ ., data=df)

vif_result <- vif(model)
print(vif_result)

# Expectation: low multicollinearity among features







#
#
# # Double check:
# # Test Case 12: Low effective rank with moderate tail strength
# data <- make_regression(n_samples=100, n_features=20, effective_rank=50, tail_strength=0.5)
# # Expectation: high multicollinearity
#
# df <- as.data.frame(data$X)
# df$Y <- data$Y
#
# model <- lm(Y ~ ., data=df)
#
# # getting alias errors




