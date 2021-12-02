# linearRegression

The 'linearRegression' package and 'linearReg' function fits a linear model and performs simple and multiple linear regression on a response
variable vector and a matrix of explanatory variables. The 'linearReg' function provides the explanatory variable's coefficient
estimates and the corresponding standard error, t-statistic, p-value, and 95% confidence interval. Additonally, the function will signify
if the p-values are signficant for alpha levels 0.05 and 0.01, which are specified by "*" and "**" respectively. Laslty, 'linearReg' function
will report the residual quantiles, R-squared, and Adjusted R-squared values for the model.

# Installation

To install and use the 'linearReg' you must first install and load the package library:

```{r}
install.packages("linearRegression")
library(linearRegression)
```
