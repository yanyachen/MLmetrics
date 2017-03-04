# MLmetrics

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/MLmetrics)](http://cran.r-project.org/package=MLmetrics)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/MLmetrics)](http://cran.r-project.org/package=MLmetrics)
![CRAN Downloads Total](http://cranlogs.r-pkg.org/badges/grand-total/MLmetrics?color=brightgreen)
Linux/Mac: [![Build Status](https://travis-ci.org/yanyachen/MLmetrics.svg)](https://travis-ci.org/yanyachen/MLmetrics)
Windows: [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/yanyachen/MLmetrics?branch=master&svg=true)](https://ci.appveyor.com/project/yanyachen/MLmetrics)

Machine Learning Evaluation Metrics  

A collection of evaluation metrics, including loss, score and utility functions, that measure regression, classification and ranking performance.  
 
* Regression:  
Mean Squared Error  
Root Mean Squared Error  
Root Mean Squared Logarithmic Error  
Root Mean Square Percentage Error  
Root Relative Squared Error  
Mean Absolute Error  
Mean Absolute Percentage Error  
Median Absolute Error  
Median Absolute Percentage Error  
Relative Absolute Error  
R-Squared (Coefficient of Determination) Regression Score  
Poisson LogLoss  
Normalized Gini Coefficient
* Classification:  
Confusion Matrix  
Zero-One Loss  
Accuracy  
Precision
Precision (micro averaged)
Precision (macro averaged)  
Recall
Recall (micro averaged)
Recall (macro averaged)    
Sensitivity  
Specificity  
F1 Score  
F1 Score (micro averaged)
F1 Score (macro averaged) 
F-Beta Score  
Log loss / Cross-Entropy Loss  
Multi Class Log Loss  
AUC  
Gini  
PRAUC  
LiftAUC  
GainAUC  
Kolmogorov-Smirnov Statistic  

To install:  
* the stable version from [CRAN](http://cran.r-project.org/web/packages/MLmetrics/index.html):  
```r
install.packages("MLmetrics")
```

* the latest development version:  
```r
devtools::install_github("yanyachen/MLmetrics")
```
