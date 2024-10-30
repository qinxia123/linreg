#' Ridge Regression Reference Class
#'
#' This reference class implements ridge regression, allowing users to fit a ridge regression model, compute coefficients, predict values, and print model summaries.
#'
#' Ridge regression is useful in cases where multicollinearity is present among predictors or when the number of predictors (p) exceeds the number of observations (n). This class calculates the coefficients using the formula:
#' \deqn{\beta_{ridge} = (X^TX + \lambda I)^{-1}X^Ty}
#' where \eqn{\lambda} is the regularization parameter.
#'
#' @field formula A formula object specifying the regression model.
#' @field data_name A character string with the name of the data used in the model.
#' @field data A data frame containing the dataset to be used for ridge regression.
#' @field lambda A numeric value for the ridge penalty parameter, \eqn{\lambda}.
#' @field coefficients A numeric vector storing the ridge regression coefficients.
#' @field fitted_values A numeric vector of the model's fitted values.
#'
#' @return An instance of the `ridgereg` class with fields and methods for ridge regression.
#' @export
ridgereg <- setRefClass(
  "ridgereg",

  fields = list(
    formula = "formula",
    data_name = "character",
    data = "data.frame",
    lambda = "numeric",
    coefficients = "numeric",
    fitted_values = "numeric"
  ),

  methods = list(

    #' Initialize the ridge regression model
    #' @param formula A formula specifying the model to be used.
    #' @param data A data frame containing the data for fitting the model.
    #' @param data_name A character string specifying the name of the dataset.
    #' @param lambda A numeric value for the ridge penalty parameter (default is 0).
    #' @export
    initialize = function(formula, data, data_name, lambda = 0) {
      formula <<- formula
      data_name <<- data_name
      data <<- data
      lambda <<- lambda

      # Create the model frame and design matrix
      mf <- model.frame(formula, data)
      X <- model.matrix(formula, mf)  # Don't exclude intercept column
      y <- model.response(mf)

      # Ridge coefficients calculation
      lambda_I <- lambda * diag(ncol(X))
      coefficients <<- as.numeric(solve(t(X) %*% X + lambda_I) %*% (t(X) %*% y))

      # Compute fitted values
      fitted_values <<- as.numeric(X %*% coefficients)

      message("Data name stored: ", data_name)
    },

    #' Print the ridge regression model details
    #' @export
    print = function() {
      cat("Call:\n")
      cat(sprintf("ridgereg(formula = %s, data = %s, lambda = %.1f)\n",
                  deparse(formula), data_name, lambda))
      cat("\nCoefficients:\n")

      # Print coefficients with variable names
      coef_names <- colnames(model.matrix(formula, data))  # Include intercept
      coef_values <- coefficients

      for (i in seq_along(coef_values)) {
        cat(sprintf("%20s", coef_names[i]))
      }
      cat("\n")

      for (j in seq_along(coef_values)) {
        cat(sprintf("%20.2f", coef_values[j]))
      }
      cat("\n")
    },

    #' Predict new values based on the ridge regression model
    #' @param newdata Optional data frame for predicting new values. If NULL, the method returns fitted values for the training data.
    #' @return A numeric vector of predicted values.
    #' @export
    predict = function(newdata = NULL) {
      if (is.null(newdata)) {
        return(fitted_values)
      } else {
        # Include intercept for new data
        newdata <- cbind(Intercept = 1, newdata)  # Add an intercept column
        X_new <- model.matrix(formula, newdata)
        return(as.numeric(X_new %*% coefficients))
      }
    },

    #' Get the ridge regression coefficients
    #' @return A numeric vector of ridge regression coefficients.
    #' @export
    coef = function() {
      return(as.numeric(coefficients))
    }
  )
)

#' Fit a Ridge Regression Model
#' @param formula A formula object specifying the model to be fitted.
#' @param data A data frame containing the dataset for fitting.
#' @param lambda A numeric value specifying the ridge penalty parameter.
#' @return An instance of the `ridgereg` class containing the fitted model.
#' @seealso \code{\link[MASS]{lm.ridge}} for a comparable ridge regression implementation in the MASS package.
#' @examples
#' data(iris)
#' mod <- ridgereg_fit(Petal.Length ~ Species, data = iris, lambda = 0.1)
#' mod$print()
#' mod$predict(newdata = iris)
#' mod$coef()
#' @export
ridgereg_fit <- function(formula, data, lambda) {
  data_name <- deparse(substitute(data))
  ridgereg$new(formula = formula, data = data, data_name = data_name, lambda = lambda)
}
