library(glmnet)

ES <- function(train_x, Y, test_x,lambda){
  # a function to calculate the ESCV for a given lambda in LASSO
  # Args:
  #   train_x: a data.frame for training
  #   Y: a vector of labels for training samples
  #   lambda: hyper-parameter in LASSO
  #   test_x: a data.frame for testing
  # Returns: 
  #   a number as ES value.
  
  #Set up ten-fold CV 
  folds <- cut(seq(1,nrow(train_x)),breaks=10,labels=FALSE)
  
  cv_Y_pred = data.frame(1:nrow(test_x))
  for(i in 1:10){
    cvIndexes <- which(folds==i,arr.ind=TRUE)
    
    # Train LASSO 
    fit.lasso<- glmnet(train_x[-cvIndexes,], Y[-cvIndexes], alpha = 0, lambda = lambda)
    
    # Predict on Testing set
    cv_Y_pred = data.frame(cv_Y_pred,predict(fit.lasso,test_x,s = lambda))
  }
  cv_Y_pred = cv_Y_pred[,-1]
  
  # Calculate the mean and variance of ES(\lambda) 
  Y_bar = rowMeans(cv_Y_pred)
  var_Y = (sum(cv_Y_pred^2) - ncol(cv_Y_pred)*sum(Y_bar^2))/nrow(cv_Y_pred)
  
  # Get ES value
  return(var_Y/sum(Y_bar^2))
}

Get.IC.Table <- function(train_x,train_y,index){
  # a function to calculate the AIC,AICc,BIC for different lambdas in LASSO
  # Args:
  #   train_x: a data.frame for training
  #   train_y: a data.frame of multi-labels for training samples
  #   index: label(voxel) index
  # Returns: 
  #   a data.frame contains AIC,BIC,AICc values for different lambdas in LASSO
  
  i = index
  Yi = train_y[,i]
  
  #Fit LASSO
  fit.lasso.i <- glmnet(train_x, Yi, alpha = 1, nlambda = 100)
  tLL <- fit.lasso.i$nulldev - deviance(fit.lasso.i)
  k <- fit.lasso.i$df
  n <- fit.lasso.i$nobs
  
  #Calculate AIC
  AIC <-  -tLL+2*k
  AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  
  #Calculate BIC
  BIC<-log(n)*k - tLL
  
  #Get the table of those values from cross-validation
  CVtable = data.frame(lambda = fit.lasso.i$lambda,AIC = AIC, AICc = AICc, BIC = BIC)
  return(CVtable)
}

Get.Corr.Table <- function(train_x,train_y,test_x,test_y){
  # a function to calculate correlations of prediction and true values
  # Args:
  #   train_x: a data.frame for training
  #   train_y: a data.frame of multi-labels for training samples
  #   test_x: a data.frame for testing
  #   test_y: a data.frame of multi-labels for testing samples
  # Returns: 
  #   a vector contains correlation between predicted values and real results.
  #Suppress warning
  options(warn=-1)
  cor_pred = vector()
  
  #Traverse all the twenty voxels
  for (i in 1:20) {
    feature_cor = vector()
    for(j in 1:ncol(train_x)){
      feature_cor[j] = abs(cor(train_x[,j],train_y[,i]))
    }
    
    names(feature_cor) = 1:ncol(train_x)
    feature_index = as.numeric(names(sort(feature_cor,decreasing = T))[1:1000])
    
    #Model training 
    model = cv.glmnet(train_x[,feature_index],train_y[,i],
                      type.measure = "mse",nfolds = 10, nlambda = 50)
    
    #Get prediction and correlation
    yhati =  predict(model, s=model$lambda.1se, newx=test_x[,feature_index])
    cor_pred[i] = cor(yhati,test_y[,i])
  }
  return(cor_pred)
}

Get.Feature.Indexes <- function(train_x,train_y){
  # a function to calculate the index of the features extract
  # Args:
  #   train_x: a data.frame for training
  #   train_y: a data.frame of multi-labels for training samples
  # Returns: 
  #   a list contains selected features from 5-fold validation.
  
  #Set up 5 folds
  folds <- cut(seq(1,nrow(train_x)),breaks=5,labels=FALSE)
  coef_list = list()
  for(j in 1:5){
    print(j)
    cvIndexes <- which(folds==j,arr.ind=TRUE)
    cv.lasso.m = cv.glmnet(train_x[-cvIndexes,],train_y[-cvIndexes,],type.measure = "mse",
                           family="mgaussian",nfolds = 10, nlambda = 100)
    co = coef(cv.lasso.m, s = "lambda.1se")
    coef_list[[j]] = co[[1]]@i
  }
  return(coef_list)
}

Get.Lambda <- function(train_x,train_y){
  # a function to calculate the lambdas from random sampling
  # Args:
  #   train_x: a data.frame for training
  #   train_y: a data.frame of multi-labels for training samples
  # Returns: 
  #   a data.frame contains the mean, sd of the best lambdas chosen. 
  
  #random shuffle train data
  shuffle = sample(nrow(train_x))
  trainX = train_x[shuffle,]
  trainY = train_y[shuffle,]
  
  #Get two criterion of lambda: 1se and min
  lambda_min =list()
  lambda_1se = list()
  
  folds <- cut(seq(1,nrow(train_x)),breaks=10,labels=FALSE)
  for(i in 1:20){
    
    lambda_min[[i]] =vector()
    lambda_1se[[i]] = vector()
    
    for(j in 1:10){
      print(i)
      print(j)
      cvIndexes <- which(folds==j,arr.ind=TRUE)
      cv.lasso.j = cv.glmnet(alpha = 0, trainX[cvIndexes,],trainY[cvIndexes,i],type.measure = "mse",
                             nfolds = 10, nlambda = 100)
      lambda_min[[i]][j] = cv.lasso.j$lambda.min
      lambda_1se[[i]][j] = cv.lasso.j$lambda.1se
    }
    
  }
  return(data.frame(voxel = 1:20, mean_lambda_min = sapply(lambda_min,mean),
                    sd_lambda_min = sapply(lambda_min,sd), mean_lambda_1se = sapply(lambda_1se,mean),
                    sd_lambda_1se = sapply(lambda_1se,sd)))
  
}

Get.Feature.Frequency = function(train_x,train_y,index){
  # a function to calculate the frequcies of the feature selected from LASSO of random shuffling
  # Args:
  #   train_x: a data.frame for training
  #   train_y: a data.frame of multi-labels for training samples
  # Returns: 
  #   a hashmap contains the frequencies of each wavelet chosen.
  
  i = index
  shuffle = sample(nrow(train_x))
  trainX = train_x[shuffle,]
  trainY = train_y[shuffle,]
  
  feature_freq = rep(0,10921)
  
  folds <- cut(seq(1,nrow(train_x)),breaks=10,labels=FALSE)
  for(j in 1:10){
    print(j)
    cvIndexes <- which(folds==j,arr.ind=TRUE)
    cv.lasso.j = cv.glmnet(trainX[cvIndexes,],trainY[cvIndexes,i],type.measure = "mse",
                           nfolds = 10, nlambda = 100)
    
    co.j = coef(cv.lasso.j,s = "lambda.1se")
    
    #increase the frequency by 1 if selected
    for(k in co.j@i){
      feature_freq[k] = feature_freq[k] + 1
    }
  }
  
  
}
