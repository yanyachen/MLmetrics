#' @title Mean Square Error Loss
#'
#' @description
#' Compute the mean squared error regression loss.
#'
#' @param y_true Ground truth (correct) target values vector
#' @param y_pred Estimated target values vector
#' @return Mean Square Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist)~log(speed), data=cars)
#' MSE(y_true=log(cars$dist), y_pred=reg$fitted.values)
#' @export

MSE <- function(y_true, y_pred){
  MSE <- mean((y_true-y_pred)^2)
  return(MSE)
}


#' @title Root Mean Square Error Loss
#'
#' @description
#' Compute the root mean squared error regression loss.
#'
#' @param y_true Ground truth (correct) target values vector
#' @param y_pred Estimated target values vector
#' @return Root Mean Square Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist)~log(speed), data=cars)
#' RMSE(y_true=log(cars$dist), y_pred=reg$fitted.values)
#' @export

RMSE <- function(y_true, y_pred){
  RMSE <- sqrt(mean((y_true-y_pred)^2))
  return(RMSE)
}


#' @title Root Mean Squared Logarithmic Error Loss
#'
#' @description
#' Compute the root mean squared logarithmic error regression loss.
#'
#' @param y_true Ground truth (correct) target values vector
#' @param y_pred Estimated target values vector
#' @return Root Mean Squared Logarithmic Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist)~log(speed), data=cars)
#' RMSLE(y_true=log(cars$dist), y_pred=reg$fitted.values)
#' @export

RMSLE <- function(y_true, y_pred){
  RMSLE <- sqrt(mean((log(1+y_true)-log(1+y_pred))^2))
  return(RMSLE)
}


#' @title Root Relative Squared Error Loss
#'
#' @description
#' Compute the root relative squared error regression loss.
#'
#' @param y_true Ground truth (correct) target values vector
#' @param y_pred Estimated target values vector
#' @return Root Relative Squared Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist)~log(speed), data=cars)
#' RRSE(y_true=log(cars$dist), y_pred=reg$fitted.values)
#' @export

RRSE <- function(y_true, y_pred){
  RRSE <- sqrt(sum((y_true-y_pred)^2)/sum((y_true-mean(y_true))^2))
  return(RRSE)
}


#' @title Mean Absolute Error Loss
#'
#' @description
#' Compute the mean absolute error regression loss.
#'
#' @param y_true Ground truth (correct) target values vector
#' @param y_pred Estimated target values vector
#' @return Mean Absolute Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist)~log(speed), data=cars)
#' MAE(y_true=log(cars$dist), y_pred=reg$fitted.values)
#' @export

MAE <- function(y_true, y_pred){
  MAE <- mean(abs(y_true-y_pred))
  return(MAE)
}


#' @title Median Absolute Error Loss
#'
#' @description
#' Compute the median absolute error regression loss.
#'
#' @param y_true Ground truth (correct) target values vector
#' @param y_pred Estimated target values vector
#' @return Median Absolute Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist)~log(speed), data=cars)
#' MedianAE(y_true=log(cars$dist), y_pred=reg$fitted.values)
#' @export

MedianAE <- function(y_true, y_pred){
  MedianAE <- median(abs(y_true-y_pred))
  return(MedianAE)
}


#' @title Relative Absolute Error Loss
#'
#' @description
#' Compute the relative absolute error regression loss.
#'
#' @param y_true Ground truth (correct) target values vector
#' @param y_pred Estimated target values vector
#' @return Relative Absolute Error Loss
#' @examples
#' data(cars)
#' reg <- lm(log(dist)~log(speed), data=cars)
#' RAE(y_true=log(cars$dist), y_pred=reg$fitted.values)
#' @export

RAE <- function(y_true, y_pred){
  RAE <- sum(abs(y_true-y_pred))/sum(abs(y_true-mean(y_true)))
  return(RAE)
}


#' @title R-Squared (Coefficient of Determination) Regression Score
#'
#' @description
#' Compute the R-Squared (Coefficient of Determination) Regression Score.
#'
#' @param y_true Ground truth (correct) target values vector
#' @param y_pred Estimated target values vector
#' @return R^2 score
#' @examples
#' data(cars)
#' reg <- lm(log(dist)~log(speed), data=cars)
#' R2_score(y_true=log(cars$dist), y_pred=reg$fitted.values)
#' @export

R2_score <- function(y_true, y_pred){
  R2_score <- 1-sum((y_true-y_pred)^2)/sum((y_true-mean(y_true))^2)
  return(R2_score)
}