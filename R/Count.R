#' @title Poisson Log loss
#'
#' @description
#' Compute the log loss/cross-entropy loss.
#'
#' @param y_pred Predicted labels vector, as returned by a model
#' @param y_true Ground truth (correct) labels vector
#' @return Log loss/Cross-Entropy Loss
#' @examples
#' d_AD <- data.frame(treatment = gl(3,3), outcome = gl(3,1,9),
#'                    counts = c(18,17,15,20,10,20,25,13,12))
#' glm_poisson <- glm(counts ~ outcome + treatment,
#'                    family = poisson(link = "log"), data = d_AD)
#' Poisson_LogLoss(y_pred = glm_poisson$fitted.values, y_true = d_AD$counts)
#' @export

Poisson_LogLoss <- function(y_pred, y_true) {
  eps <- 1e-15
  y_pred <- pmax(y_pred, eps)
  Poisson_LogLoss <- mean(log(gamma(y_true + 1)) + y_pred - log(y_pred) * y_true)
  # Poisson_LogLoss <- mean(-dpois(y_true, y_pred, log = TRUE))
  return(Poisson_LogLoss)
}


#' @title Normalized Gini Coefficient
#'
#' @description
#' Compute the Normalized Gini Coefficient.
#'
#' @param y_pred Predicted labels vector, as returned by a model
#' @param y_true Ground truth (correct) labels vector
#' @return Normalized Gini Coefficient
#' @examples
#' d_AD <- data.frame(treatment = gl(3,3), outcome = gl(3,1,9),
#'                    counts = c(18,17,15,20,10,20,25,13,12))
#' glm_poisson <- glm(counts ~ outcome + treatment,
#'                    family = poisson(link = "log"), data = d_AD)
#' NormalizedGini(y_pred = glm_poisson$fitted.values, y_true = d_AD$counts)
#' @export

NormalizedGini <- function(y_pred, y_true) {
  SumGini <- function(y_pred, y_true) {
    y_true_sort <- y_true[order(y_pred, decreasing = TRUE)]
    y_random <- 1:length(y_pred) / length(y_pred)
    y_Lorentz <- cumsum(y_true_sort) / sum(y_true_sort)
    SumGini <- sum(y_Lorentz - y_random)
    return(SumGini)
  }
  NormalizedGini <- SumGini(y_pred, y_true) / SumGini(y_true, y_true)
  return(NormalizedGini)
}







