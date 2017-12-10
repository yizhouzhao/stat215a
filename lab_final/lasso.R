library(glmnet)

ES <- function(train_x, Y, test_x,lambda){
  folds <- cut(seq(1,nrow(train_x)),breaks=10,labels=FALSE)
  cv_Y_pred = data.frame(1:nrow(test_x))
  for(i in 1:10){
    cvIndexes <- which(folds==i,arr.ind=TRUE)
    fit.lasso<- glmnet(train_x[-cvIndexes,], Y[-cvIndexes], alpha = 1, lambda = lambda)
    cv_Y_pred = data.frame(cv_Y_pred,predict(fit.lasso,test_x,s = lambda))
  }
  cv_Y_pred = cv_Y_pred[,-1]
  Y_bar = rowMeans(cv_Y_pred)
  var_Y = (sum(cv_Y_pred^2) - ncol(cv_Y_pred)*sum(Y_bar^2))/nrow(cv_Y_pred)
  return(var_Y/sum(Y_bar^2))
}

Get.IC.Table <- function(train_x,train_y,index){
  i = index
  Yi = train_y[,i]
  fit.lasso.i <- glmnet(train_x, Yi, alpha = 1, nlambda = 100)
  tLL <- fit.lasso.i$nulldev - deviance(fit.lasso.i)
  k <- fit.lasso.i$df
  n <- fit.lasso.i$nobs
  AIC <-  -tLL+2*k
  AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  BIC<-log(n)*k - tLL
  
  CVtable = data.frame(lambda = fit.lasso.i$lambda,AIC = AIC, AICc = AICc, BIC = BIC)
  return(CVtable)
}

Get.Corr.Table <- function(train_x,train_y,test_x,test_y){
  options(warn=-1)
  cor_pred = vector()
  for (i in 1:2) {
    
    feature_cor = vector()
    for(j in 1:ncol(train_x)){
      feature_cor[j] = abs(cor(train_x[,j],train_y[,i]))
    }
    
    names(feature_cor) = 1:ncol(train_x)
    feature_index = as.numeric(names(sort(feature_cor,decreasing = T))[1:1000])
    
    model = cv.glmnet(train_x[,feature_index],train_y[,i],
                      type.measure = "mse",nfolds = 10, nlambda = 50)
    yhati =  predict(model, s=model$lambda.1se, newx=test_x[,feature_index])
    cor_pred[i] = cor(yhati,test_y[,i])
  }
  return(cor_pred)
}

Get.Feature.Indexes <- function(train_x,train_y){
  folds <- cut(seq(1,nrow(train_x)),breaks=10,labels=FALSE)
  coef_list = list()
  for(j in 1:10){
    cvIndexes <- which(folds==j,arr.ind=TRUE)
    cv.lasso.m = cv.glmnet(train_x[cvIndexes,],train_y[cvIndexes,],type.measure = "mse",
                           family="mgaussian",nfolds = 10, nlambda = 100)
    co = coef(cv.lasso.m, s = "lambda.1se")
    coef_list[[j]] = co[[1]]@i
  }
  return(coef_list)
}
