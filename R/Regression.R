#' @title Mean Square Error Loss
#'
#' @description
#' Compute the mean squared error regression loss.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return Mean Square Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' MSE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @export

MSE <- function(y_pred, y_true) {
  MSE <- mean((y_true - y_pred)^2)
  return(MSE)
}


#' @title Root Mean Square Error Loss
#'
#' @description
#' Compute the root mean squared error regression loss.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return Root Mean Square Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' RMSE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @export

RMSE <- function(y_pred, y_true) {
  RMSE <- sqrt(mean((y_true - y_pred)^2))
  return(RMSE)
}


#' @title Root Mean Squared Logarithmic Error Loss
#'
#' @description
#' Compute the root mean squared logarithmic error regression loss.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return Root Mean Squared Logarithmic Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' RMSLE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @export

RMSLE <- function(y_pred, y_true) {
  RMSLE <- sqrt(mean((log(1 + y_true) - log(1 + y_pred))^2))
  return(RMSLE)
}


#' @title Root Mean Square Percentage Error Loss
#'
#' @description
#' Compute the root mean squared percentage error regression loss.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return Root Mean Squared Percentage Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' RMSPE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @export

RMSPE <- function(y_pred, y_true) {
  RMSPE <- sqrt(mean(((y_true - y_pred) / y_true)^2))
  return(RMSPE)
}


#' @title Root Relative Squared Error Loss
#'
#' @description
#' Compute the root relative squared error regression loss.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return Root Relative Squared Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' RRSE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @export

RRSE <- function(y_pred, y_true) {
  RRSE <- sqrt(sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2))
  return(RRSE)
}


#' @title Mean Absolute Error Loss
#'
#' @description
#' Compute the mean absolute error regression loss.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return Mean Absolute Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' MAE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @export

MAE <- function(y_pred, y_true) {
  MAE <- mean(abs(y_true - y_pred))
  return(MAE)
}


#' @title Mean Absolute Percentage Error Loss
#'
#' @description
#' Compute the mean absolute percentage error regression loss.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return Mean Absolute Percentage Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' MAPE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @export

MAPE <- function(y_pred, y_true) {
  MAPE <- mean(abs((y_true - y_pred) / y_true))
  return(MAPE)
}


#' @title Median Absolute Error Loss
#'
#' @description
#' Compute the median absolute error regression loss.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return Median Absolute Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' MedianAE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @importFrom stats median
#' @export

MedianAE <- function(y_pred, y_true) {
  MedianAE <- median(abs(y_true - y_pred))
  return(MedianAE)
}


#' @title Median Absolute Percentage Error Loss
#'
#' @description
#' Compute the Median absolute percentage error regression loss.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return Median Absolute Percentage Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' MedianAPE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @importFrom stats median
#' @export

MedianAPE <- function(y_pred, y_true) {
  MedianAPE <- median(abs((y_true - y_pred) / y_true))
  return(MedianAPE)
}


#' @title Relative Absolute Error Loss
#'
#' @description
#' Compute the relative absolute error regression loss.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return Relative Absolute Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' RAE(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @export

RAE <- function(y_pred, y_true) {
  RAE <- sum(abs(y_true - y_pred)) / sum(abs(y_true - mean(y_true)))
  return(RAE)
}


#' @title R-Squared (Coefficient of Determination) Regression Score
#'
#' @description
#' Compute the R-Squared (Coefficient of Determination) Regression Score.
#'
#' @param y_pred Estimated target values vector
#' @param y_true Ground truth (correct) target values vector
#' @return R^2 Score
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' R2_Score(y_pred = exp(reg$fitted.values), y_true = cars$dist)
#' @export

R2_Score <- function(y_pred, y_true) {
  R2_Score <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
  return(R2_Score)
}
