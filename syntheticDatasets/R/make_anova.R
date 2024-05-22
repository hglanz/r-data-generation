library(here)
library(scatterplot3d)
source(here("R", "make_regression.R"))
okabe_ito_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                      "#0072B2", "#D55E00", "#CC79A7", "#999999")
#' Simulate Data for ANOVA Analysis
#'
#' This function generates synthetic data suitable for ANOVA (Analysis of Variance) testing. It creates a dataset with a specified number of groups, samples per group, and features, adding an optional group effect to simulate between-group differences. The function leverages `make_regression` to generate the feature data (`X`) and then adjusts the response (`Y`) to include group effects.
#'
#' @param n_groups Number of groups to generate data for. Default is 3.
#' @param n_samples_per_group Number of samples per group. Default is 100.
#' @param n_features Total number of features. Default is 100.
#' @param n_informative Number of informative features. Influences the regression target. Default is 10.
#' @param bias Bias term in the underlying linear model. Default is 0.0.
#' @param noise Standard deviation of the Gaussian noise applied to the output. Default is 5.0.
#' @param shuffle Logical, indicating whether to shuffle the samples. Default is TRUE.
#' @param coef If TRUE, the coefficients of the underlying linear model are returned. Default is FALSE.
#' @param random_state Integer, the seed used by the random number generator for reproducibility. Can be NULL. Default is NULL.
#' @param effective_rank The approximate number of singular vectors required to explain most of the input data by linear combinations. Relevant for 'make_regression'. Default is NULL.
#' @param tail_strength The relative importance of the fat noisy tail of the singular values profile if 'effective_rank' is not NULL. Default is 0.5.
#' @param group_efect The effect size to be added to each group to simulate between-group differences. Default is 10.
#' @param plot Logical, indicating whether to plot the dataset. Default is FALSE.
#' @return A list containing three elements: 'X' is a matrix of feature data, 'Y' is the adjusted response vector including group effects, and 'GroupLabels' is a vector indicating the group of each sample.
#' @examples
#' # Generate default ANOVA data
#' anova_data <- simulate_anova_data()
#'
#' # Generate ANOVA data with 5 groups, each with 50 samples
#' custom_anova_data <- simulate_anova_data(n_groups = 5, n_samples_per_group = 50)
#' @export
make_anova <- function(n_groups = 3, n_samples_per_group = 100, n_features = 100, n_informative = 10,
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

  if (is.list(data)) {
    data_X <- data$X
    data_Y <- data$Y
  } else {
    data_X <- data[, 1:n_features]
    data_Y <- data[, (n_features + 1)]
  }

  group_labels <- rep(1:n_groups, each = n_samples_per_group)
  group_effects <- rep(1:n_groups, each = n_samples_per_group) * group_effect
  Y_adjusted <- data_Y + group_effects

  if (shuffle) {
    indices <- sample(total_samples)
    data_X <- data_X[indices, ]
    Y_adjusted <- Y_adjusted[indices]
    group_labels <- group_labels[indices]
  }

  if (plot) {
    if (n_features == 1) {
      colors <- okabe_ito_colors[1:n_groups]
      plot(data_X, col=colors[group_labels], pch=19, xlab="Feature 1", ylab="Feature 2", main="Simulated ANOVA Data")
    } else if (n_features == 2) {
      colors <- rainbow(n_groups)
      scatterplot3d(data_X[,1], data_X[,2], Y_adjusted, color=colors[group_labels], pch=19, xlab="Feature 1", ylab="Feature 2", zlab="Feature 3", main="Generated ANOVA Data")
    }
  }

  list("X" = data_X, "Y" = Y_adjusted, "GroupLabels" = group_labels)
}



