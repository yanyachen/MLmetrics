#' @title Normalized Zero-One Loss (Classification Error Loss)
#'
#' @description
#' Compute the zero-one classification loss.
#'
#' @param y_true Ground truth (correct) labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return Zero-One Loss
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' pred <- ifelse(logreg$fitted.values<0.5, 0, 1)
#' ZeroOneLoss(y_true=mtcars$vs, y_pred=pred)
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
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' pred <- ifelse(logreg$fitted.values<0.5, 0, 1)
#' Accuracy(y_true=mtcars$vs, y_pred=pred)
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
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' pred <- ifelse(logreg$fitted.values<0.5, 0, 1)
#' ConfusionMatrix(y_true=mtcars$vs, y_pred=pred)
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
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' pred <- ifelse(logreg$fitted.values<0.5, 0, 1)
#' ConfusionDF(y_true=mtcars$vs, y_pred=pred)
#' @keywords internal
#' @export

ConfusionDF <- function(y_true, y_pred){
  Freq <- NULL
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
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' pred <- ifelse(logreg$fitted.values<0.5, 0, 1)
#' Precision(y_true=mtcars$vs, y_pred=pred, positive="0")
#' Precision(y_true=mtcars$vs, y_pred=pred, positive="1")
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
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' pred <- ifelse(logreg$fitted.values<0.5, 0, 1)
#' Recall(y_true=mtcars$vs, y_pred=pred, positive="0")
#' Recall(y_true=mtcars$vs, y_pred=pred, positive="1")
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
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' pred <- ifelse(logreg$fitted.values<0.5, 0, 1)
#' F1_Score(y_true=mtcars$vs, y_pred=pred, positive="0")
#' F1_Score(y_true=mtcars$vs, y_pred=pred, positive="1")
#' @export

F1_Score <- function(y_true, y_pred, positive=NULL){
  Confusion_DF <- ConfusionDF(y_true, y_pred)
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
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' pred <- ifelse(logreg$fitted.values<0.5, 0, 1)
#' FBeta_Score(y_true=mtcars$vs, y_pred=pred, positive="0", beta=2)
#' FBeta_Score(y_true=mtcars$vs, y_pred=pred, positive="1", beta=2)
#' @export

FBeta_Score <- function(y_true, y_pred, positive=NULL, beta=1){
  Confusion_DF <- ConfusionDF(y_true, y_pred)
  if(is.null(positive)) positive <- as.character(Confusion_DF[1,1])
  Precision <- Precision(y_true, y_pred, positive)
  Recall <- Recall(y_true, y_pred, positive)
  Fbeta_Score <- (1+beta^2)*(Precision*Recall)/(beta^2*Precision+Recall)
  return(Fbeta_Score)
}


#' @title Log loss/Cross-Entropy Loss
#'
#' @description
#' Compute the log loss/cross-entropy loss.
#'
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return Log loss/Cross-Entropy Loss
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' LogLoss(y_true=mtcars$vs, y_pred=logreg$fitted.values)
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
#'   correct labels indicating by 0-1, same format as probabilities matrix
#' @param y_pred Predicted probabilities matrix, as returned by a classifier
#' @return Multi Class Log Loss
#' @examples
#' data(iris)
#' svm.model <- e1071::svm(Species~., data=iris, probability=TRUE)
#' pred <- predict(svm.model, iris, probability=TRUE)
#' MultiLogLoss(y_true=iris$Species, y_pred=attr(pred, "probabilities"))
#' @export

MultiLogLoss <- function(y_true, y_pred){
  if(is.factor(y_true)){
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
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return Area Under the Curve (AUC)
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' AUC(y_true=mtcars$vs, y_pred=logreg$fitted.values)
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
#' @param y_true Ground truth (correct) 0-1 labels vector
#' @param y_pred Predicted labels vector, as returned by a classifier
#' @return Gini Coefficient
#' @examples
#' data(cars)
#' logreg <- glm(formula=vs~hp+wt, family=binomial(link = "logit"), mtcars)
#' Gini(y_true=mtcars$vs, y_pred=logreg$fitted.values)
#' @export

Gini <- function(y_true, y_pred){
  AUC <- AUC(y_true, y_pred)
  Gini <- 2*AUC-1
  return(Gini)
}