Get.IC.Table.Ridge <- function(train_x,train_y,index){
  # a function to calculate the AIC,AICc,BIC for different lambdas in Ridge
  # Args:
  #   train_x: a data.frame for training
  #   train_y: a data.frame of multi-labels for training samples
  #   index: label(voxel) index
  # Returns: 
  #   a data.frame contains AIC,BIC,AICc values for different lambdas in LASSO
  
  #Calculate AIC, AICc and BIC
  i = index
  Yi = train_y[,i]
  fit.lasso.i <- glmnet(train_x, Yi, alpha = 0, nlambda = 100)
  tLL <- fit.lasso.i$nulldev - deviance(fit.lasso.i)
  k <- fit.lasso.i$df
  n <- fit.lasso.i$nobs
  AIC <-  -tLL+2*k
  AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  BIC<-log(n)*k - tLL
  
  #return the table contains AIC, AICc and BIC criterion
  CVtable = data.frame(lambda = fit.lasso.i$lambda,AIC = AIC, AICc = AICc, BIC = BIC)
  return(CVtable)
}