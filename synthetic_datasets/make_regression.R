
make_regression <- function(n_samples=100, n_features=100, n_informative=10, n_targets=1, 
                            bias=0.0, noise=0.0, shuffle=TRUE, coef=FALSE, random_state=NULL) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }
  
  # set the number of informative coefficients
  coefficients <- rnorm(n_informative)
  
  # random data values
  X <- matrix(rnorm(n_samples * n_features), ncol=n_features)
  
  # target data. Y = intercept + coeff * X + error
  informative_X <- X[, seq(n_informative)]
  Y <- bias + informative_X %*% coefficients  + rnorm(n_samples, sd=noise)
  
  # multi-output check
  if (n_targets > 1) {
    Y <- matrix(Y, ncol=n_targets)
  }
  
  # shuffle if requested
  if (shuffle) {
    indices <- sample(n_samples)
    X <- X[indices,]
    Y <- Y[indices,]
  }
  
  if (coef) {
    return(list("X"=X, "Y"=Y, "coefficients"=coefficients))
  } else {
    return(list("X"=X, "Y"=Y))
  }
  
  # to do:
  # Implement effective_rank and tail_strength 
  # Optional functionality: plotting the points on a graph
  
}
