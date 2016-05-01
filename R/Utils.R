#' @title Calculate the Area Under the Curve
#'
#' @description
#' Calculate the area under the curve.
#'
#' @param x the x-points of the curve
#' @param y the y-points of the curve
#' @param method can be "trapezoid" (default), "step" or "spline"
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds
#' @return Area Under the Curve (AUC)
#' @examples
#' x <- seq(0, pi, length.out = 200)
#' plot(x = x, y = sin(x), type = "l")
#' Area_Under_Curve(x = x, y = sin(x), method = "trapezoid", na.rm = TRUE)
#' @importFrom stats integrate
#' @importFrom stats na.omit
#' @importFrom stats splinefun
#' @export

Area_Under_Curve <- function(x, y, method = c("trapezoid", "step", "spline"), na.rm = FALSE) {
  if (na.rm == TRUE) {
    xy_cbind <- na.omit(cbind(x, y))
    x <- xy_cbind[, 1]
    y <- xy_cbind[, 2]
  }
  if (length(x) != length(y)) {
    stop("length x must equal length y")
  }
  idx <- order(x)
  x <- x[idx]
  y <- y[idx]
  switch(match.arg(arg = method, choices = c("trapezoid", "step","spline")),
         trapezoid = {
           AUC <- sum((apply(cbind(y[-length(y)], y[-1]), 1, mean)) *(x[-1] - x[-length(x)]))},
         step = {
           AUC <- sum(y[-length(y)] * (x[-1] - x[-length(x)]))},
         spline = {
           AUC <- integrate(splinefun(x, y, method = "natural"), lower = min(x),upper = max(x))$value})
  return(AUC)
}











