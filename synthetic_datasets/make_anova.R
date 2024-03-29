source("Desktop/Thesis/r-data-generation/synthetic_datasets/make_regression.R")


simulate_anova_data <- function(n_groups = 3, n_samples_per_group = 100, n_features = 100, n_informative = 10,
                                bias = 0.0, noise = 5.0, shuffle = TRUE, coef = FALSE, random_state = NULL,
                                effective_rank = NULL, tail_strength = 0.5, group_effect = 10, plot = FALSE) {
  if (!is.null(random_state)) {
    set.seed(random_state)
  }
  
  total_samples <- n_groups * n_samples_per_group
  
  # use make_regression for X data
  data <- make_regression(n_samples = total_samples, n_features = n_features, n_informative = n_informative,
                          bias = bias, noise = noise, shuffle = FALSE, coef = coef, random_state = random_state,
                          effective_rank = effective_rank, tail_strength = tail_strength, plot = FALSE)
  
  group_labels <- rep(1:n_groups, each = n_samples_per_group)
  group_effects <- rep(1:n_groups, each = n_samples_per_group) * group_effect
  Y_adjusted <- data$Y + group_effects
  
  if (shuffle) {
    indices <- sample(total_samples)
    data$X <- data$X[indices, ]
    Y_adjusted <- Y_adjusted[indices]
    group_labels <- group_labels[indices]
  }
  
  list("X" = data$X, "Y" = Y_adjusted, "GroupLabels" = group_labels)
}


simulate_anova_data()

simulate_anova_data(n_groups = 5, n_samples_per_group = 50)

simulate_anova_data(noise = 20, bias = 10, random_state = 123)

simulate_anova_data(effective_rank = 20, tail_strength = 0.2)


