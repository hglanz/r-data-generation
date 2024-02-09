
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
      plot(X[,1], Y, main="Feature 1 vs Target", xlab="Feature 1", ylab="Target")
    } else {
      par(mfrow=c(1, n_targets))
      for (i in 1:n_targets) {
        plot(X[,1], Y[,i], main=paste("Feature 1 vs Target", i), xlab="Feature 1", ylab=paste("Target", i))
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

library(car)

# Test Case 11: High effective rank with moderate tail strength
library(car)

data <- make_regression(n_samples=100, n_features=20, effective_rank=20, tail_strength=0.5)

df <- as.data.frame(data$X)
df$Y <- data$Y

model <- lm(Y ~ ., data=df)

vif_result <- vif(model)
print(vif_result)

# Expectation: low multicollinearity among features









# Double check:
# Test Case 12: Low effective rank with moderate tail strength
data <- make_regression(n_samples=100, n_features=20, effective_rank=50, tail_strength=0.5)
# Expectation: high multicollinearity

df <- as.data.frame(data$X)
df$Y <- data$Y

model <- lm(Y ~ ., data=df)

# getting alias errors




