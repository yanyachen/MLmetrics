#' @title Normalized Zero-One Loss (Classification Error Loss)
#'
#' @description
#' Compute the zero-one classification loss.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return Zero-One Loss
#' @export

ZeroOneLoss <- function(y_true, y_pred){
  CE <- mean(y_true!=y_pred)
  return(CE)
}


#' @title Accuracy
#'
#' @description
#' Compute the accuracy classification score.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return Accuracy
#' @export

Accuracy <- function(y_true, y_pred){
  Accuracy <- mean(y_true==y_pred)
  return(Accuracy)
}


#' @title Confusion Matrix
#'
#' @description
#' Compute confusion matrix to evaluate the accuracy of a classification.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return a table of Confusion Matrix
#' @export

ConfusionMatrix <- function(y_true, y_pred){
  Confusion_Mat <- table(y_true, y_pred)
  return(Confusion_Mat)
}


#' @title Confusion Matrix (Data Frame Format)
#'
#' @description
#' Compute data frame format confusion matrix for internal usage.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return a data.frame of Confusion Matrix
#' @keywords internal
#' @export

ConfusionDF <- function(y_true, y_pred){
  Confusion_DF <- transform(as.data.frame(ConfusionMatrix(y_true, y_pred)),
                            y_true=as.character(y_true),
                            y_pred=as.character(y_pred),
                            Freq=as.integer(Freq))
  return(Confusion_DF)
}


#' @title Precision
#'
#' @description
#' Compute the precision score.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return Precision
#' @export

Precision <- function(y_true, y_pred, positive=NULL){
  Confusion_DF <- ConfusionDF(y_true, y_pred)
  if(is.null(positive)) positive <- as.character(Confusion_DF[1,1])
  TP <- as.integer(subset(Confusion_DF, y_true==positive&y_pred==positive)["Freq"])
  FP <- as.integer(sum(subset(Confusion_DF, y_true==positive&y_pred!=positive)["Freq"]))
  Precision <- TP/(TP+FP)
  return(Precision)
}


#' @title Recall
#'
#' @description
#' Compute the recall score.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return Recall
#' @export

Recall <- function(y_true, y_pred, positive=NULL){
  Confusion_DF <- ConfusionDF(y_true, y_pred)
  if(is.null(positive)) positive <- as.character(Confusion_DF[1,1])
  TP <- as.integer(subset(Confusion_DF, y_true==positive&y_pred==positive)["Freq"])
  FN <- as.integer(sum(subset(Confusion_DF, y_true!=positive&y_pred==positive)["Freq"]))
  Recall <- TP/(TP+FN)
  return(Recall)
}


#' @title F1 Score
#'
#' @description
#' Compute the F1 Score.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @return F1 Score
#' @export

F1_Score <- function(y_true, y_pred, positive=NULL){
  if(is.null(positive)) positive <- as.character(Confusion_DF[1,1])
  Precision <- Precision(y_true, y_pred, positive)
  Recall <- Recall(y_true, y_pred, positive)
  F1_Score <- 2*(Precision*Recall)/(Precision+Recall)
  return(F1_Score)
}


#' @title F-Beta Score
#'
#' @description
#' Compute the F-Beta Score
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @param positive An optional character string for the factor level that
#'   corresponds to a "positive" result
#' @param beta Weight of precision in harmonic mean
#' @return F-Beta Score
#' @export

FBeta_Score <- function(y_true, y_pred, positive=NULL, beta=1){
  if(is.null(positive)) positive <- as.character(Confusion_DF[1,1])
  Precision <- Precision(y_true, y_pred, positive)
  Recall <- Recall(y_true, y_pred, positive)
  Fbeta_Score <- (1+beta^2)*(Precision*Recall)/(beta^2*Precision+Recall)
  return(FBeta_Score)
}


#' @title Log loss/Cross-Entropy Loss
#'
#' @description
#' Compute the log loss/cross-entropy loss.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return Log loss/Cross-Entropy Loss
#' @export

LogLoss <- function(y_true, y_pred){
  eps <- 1e-15
  y_pred <- pmax(pmin(y_pred,1-eps),eps)
  LogLoss <- -mean(y_true*log(y_pred)+(1-y_true)*log(1-y_pred))
  return(LogLoss)
}


#' @title Multi Class Log Loss
#'
#' @description
#' Compute the multi class log loss.
#'
#' @param y_true Ground truth (correct) labels vector or a matrix of 
#'   correct labels indicating by 0 and 1, same format as probabilities matrix
#' @param y_pred Predicted probabilities matrix, as returned by a classifier
#' @return Multi Class Log Loss
#' @export

MultiLogLoss <- function(y_true, y_pred){
  if(is.vector(y_true)){
    y_true_mat <- matrix(0, nrow=length(y_true), ncol=length(levels(y_true)))
    sample_levels <- as.integer(y_true)
    for(i in 1:length(y_true)) y_true_mat[i,sample_levels[i]] <- 1
    y_true <- y_true_mat
  }
  eps <- 1e-15
  N <- nrow(y_pred)
  y_pred <- pmax(pmin(y_pred,1-eps),eps)
  MultiLogLoss <- (-1/N)*sum(y_true*log(y_pred))
  return(MultiLogLoss)
}


#' @title Area Under the Curve (AUC)
#'
#' @description
#' Compute the Area Under the Curve (AUC) from prediction scores.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return Area Under the Curve (AUC)
#' @export

AUC <- function(y_true, y_pred){
  rank <- rank(y_pred)
  n_pos <- sum(y_true==1)
  n_neg <- sum(y_true==0)
  AUC <- (sum(rank[y_true==1]) - n_pos*(n_pos+1)/2)/(n_pos*n_neg)
  return(AUC)
}


#' @title Gini Coefficient
#'
#' @description
#' Compute the Gini Coefficient.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return Gini Coefficient
#' @export

Gini <- function(y_true, y_pred){
  AUC <- AUC(y_true, y_pred)
  Gini <- 2*AUC-1
  return(Gini)
}