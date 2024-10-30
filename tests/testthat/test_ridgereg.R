library(testthat)
library(MASS)

data("iris")

# Test invalid input
test_that("ridgereg rejects erroneous input", {
  expect_error(ridgereg$new(formula = Petal.Length ~ Species, data = irfsfdis, lambda = 0.1))
  expect_error(ridgereg$new(formula = Petal.Length ~ Speciees, data = iris, lambda = 0.1))
})

# Test model class
test_that("class is correct", {
  ridge_model <- ridgereg$new(Petal.Length ~ Species, data = iris, data_name = "iris", lambda = 0.1)
  expect_true(class(ridge_model) == "ridgereg")
})

# Test predict method
test_that("predict() method works with new data", {
  ridge_model <- ridgereg$new(Petal.Length ~ Species, data = iris, data_name = "iris", lambda = 0.1)

  # Create new data containing all variables
  new_data <- iris[, c("Species", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width")]
  new_data <- new_data[1:3, ]

  # Make predictions
  new_predictions <- ridge_model$predict(newdata = new_data)
  expect_equal(length(new_predictions), nrow(new_data))
})

# Test coef() method
test_that("coef() method works", {
  ridge_model <- ridgereg$new(Petal.Length ~ Species, data = iris, data_name = "iris", lambda = 0.1)
  coefficients <- ridge_model$coef()
  expect_true(length(coefficients) > 0)
  expect_type(coefficients, "double")
})

# Test similarity of coefficients with lm.ridge
test_that("ridge regression coefficients are similar to lm.ridge()", {
  lm_ridge <- MASS::lm.ridge(Petal.Length ~ Species, data = iris, lambda = 0.1)
  ridge_model <- ridgereg$new(Petal.Length ~ Species, data = iris, data_name = "iris", lambda = 0.1)

  # Get coefficients from ridge_model and add names
  ridge_coefs <- ridge_model$coef()
  names(ridge_coefs) <- names(coef(lm_ridge))  # Add names for comparison

  lm_ridge_coefs <- coef(lm_ridge)

  # Use expect_equal to compare, allowing for some error
  expect_equal(round(ridge_coefs, 2), round(lm_ridge_coefs, 2),
               tolerance = 0.01, # Set tolerance
               info = "Coefficients should be similar to those from lm.ridge")
})
