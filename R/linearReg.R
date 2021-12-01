#' Linear Regression Model
#'
#' This function loads in a matrix of explanatory variables and a response
#' vector. The matrix of explanatory variables in converted into a design matrix
#' and the response vector is converted into a column vector.
#'
#' @param y - numeric response variable vector (dim = 1 x p)
#' @param X - matrix of explanatory variables included in the model (dim= n x p)
#' @return Linear regression model summary including residual quantiles,
#'         regression coefficients, standard error, t-statistics, p-values,
#'         95% confidence intervals, R-squared and Adjusted R-squared.
#'
#' @examples
#' linearReg(as.vector(mtcars["mpg"]),as.matrix(mtcars[c("cyl", "wt")]))
#'
#' @export
linearReg<-function(y, X){

  ##ADD A 1 FOR THE INTERCEPTS TO BE A DESIGN MATRIX
  X=cbind(rep(1, nrow(X)), X)
  #rename column of 1's intercept
  colnames(X)[1] <- "Intercept"

  ##MAKE Y A COLUMN VECTOR
  y= as.matrix(y)

  n=nrow(X)
  p=ncol(X)

  ##FIND BETA ESTIMATES
  beta_hats= solve(t(X)%*%X) %*% t(X)%*% y

  ##RESIDUALS: (I-H)*y
  H=X %*% solve(t(X)%*%X) %*% t(X)
  I= diag(nrow(X))
  res=(I-H)%*%y

  #quantiles of residuals
  resq=unname(stats::quantile(res))
  resq=t(as.matrix(resq))
  colnames(resq)<-c("minimun", "Q1-25%", "median", "Q3-75%", "maximum")
  rownames(resq)<-c("")


  ##STANDARD ERROR

  #var_hat of beta = sigma^2_hat(t(X)*X)^-1
  sigma2_hat= ( t(res) %*% res ) / (n-p)
  var_beta=c(diag(solve(t(X) %*% X)) %*% sigma2_hat) #diag of var-cov matrix* sigma2

  #se=sqrt(var)
  se= sqrt(var_beta)


  ##t-STATISTIC
  t_stats= beta_hats/se

  ##P-VALUES
  p_value= 2* (1-pt(q=abs(t_stats), df= (n-p)))

  #significance
  sig_fun<-function(x){
    if(x<=0.01){
      x= "**"
    }else if(x>0.01 & x<0.05){
      x= "*"
    }else{
      x=""
    }
  }
  signficance=sapply(p_value, sig_fun)

  ##95% CI
  ci_95R= beta_hats + 1.96*se
  ci_95L=beta_hats - 1.96*se

  #R-SQUARED: 1-SSE/SSY
  SSE= as.numeric(( t(res) %*% res ))
  one= matrix(rep(1, n^2),n,n)
  SSY= as.numeric( t(y)%*% (diag(n) - (one/n)) %*% y )
  r_squared= 1- SSE/SSY

  #ADJUSTED R SQUARED
  adj_rsquared= 1-( (SSE/(n-p)) / (SSY/(n-1)) )



  ##REGRESSION MODEL OUTPUT
  output=as.data.frame(cbind(beta_hats, se, t_stats, p_value, ci_95L, ci_95R))
  output=cbind(output, signficance)
  colnames(output)<- c("Beta Estimate", "Std Error", "t-Statistic", "p-value","Lower 95% CI", "Upper 95% CI", "Signficance")

  #significance levels
  Sig= 'Signficance Level: p value < 0.05*, p value <0.01**'
  Sig=as.matrix(Sig)
  rownames(Sig)<-c("")
  colnames(Sig)<-c("")

  #r squared output
  r=rbind(r_squared, adj_rsquared)
  rownames(r)<-c("R Squared", "Adjusted R Squared")
  colnames(r)<-c("")

  return(list("Residuals"=resq,"Output"=output, "Signficance Level"= Sig,"R Squared"=r))

}
