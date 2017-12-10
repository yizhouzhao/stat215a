setwd("E:/courses/Stat215A/lab_final")
load("fMRIdata.RData")
getwd()

library(dplyr)

images = read.csv("fit_stim.csv",nrows = 10)
sample1 = matrix(unlist(images[1,]),128,128)
image(sample1)

wsample1 = matrix(unlist(fit_feat[1,1:10816]),104,104)
wsample1 = wsample1[1:104,]
par(mar=c(1,1,1,1))
image(wsample1,col = gray(1:128/129))

dim(fit_feat)


library(glmnet)

train_x = fit_feat[1:1400,]
train_y = resp_dat[1:1400,]

test_x = fit_feat[1401:1750,]
test_y = resp_dat[1401:1750,]


Y1 = train_y[,1]
fit.lasso.1 <- glmnet(train_x, Y1, alpha = 1, nlambda = 100)
tLL <- fit.lasso.1$nulldev - deviance(fit)
k <- fit.lasso.1$df
n <- fit.lasso.1$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
BIC<-log(n)*k - tLL

ES <- function(model,train_x, test_x,Y,lambda){
  folds <- cut(seq(1,1400),breaks=10,labels=FALSE)
  cv_Y_pred = data.frame(1:350)
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


cv.lasso.1 = cv.glmnet(train_x,train_y[,20],type.measure = "mse",nfolds = 10, nlambda = 100)
yhat1 <- predict(cv.lasso.1, s=cv.lasso.1$lambda.1se, newx=test_x)
cor(yhat1,test_y[,20])

cor_pred = vector()
for (i in 15:20) {
  print(i)
  model = cv.glmnet(train_x,train_y[,i],
                    type.measure = "mse",nfolds = 10, nlambda = 50)
  yhati =  predict(model, s=model$lambda.1se, newx=test_x)
  cor_pred[i] = cor(yhati,test_y[,i])
  print(cor_pred[i])
}


cv.lasso.m = cv.glmnet(train_x,train_y,type.measure = "mse",family="mgaussian",nfolds = 10, nlambda = 100)
yhatm <- predict(cv.lasso.m, s=cv.lasso.m$lambda.1se, newx=test_x)
yhatm = data.frame(yhatm)

multiple.lasso.cor = vector()
for(i in 1:20){
  multiple.lasso.cor[i] = cor(yhatm[,i],test_y[,i])
}

write.csv(file = "data/output/multile_lasso_cor.csv",data.frame(multiple = multiple.lasso.cor),row.names = F)

lasso.cor.2 = Get.Corr.Table(train_x,train_y,test_x,test_y)
lasso.cor.2[20] = 0

write.csv(file = "data/output/screen_lasso_cor.csv",data.frame(screened = lasso.cor.2),row.names = F)

co = coef(cv.lasso.m, s = "lambda.1se")
save(cv.lasso.m,file = "data/output/coefficients.Rdata")
load("data/output/coefficients.Rdata")

coef_table = data.frame(index = co[[1]]@i)
for(i in 1:20){
  coef_table = cbind(coef_table,co[[i]]@x)
}
colnames(coef_table)[2:21] = 1:20 

write.csv(coef_table, file = "data/output/coef.csv")


#----
real.wav = read_csv("data/real_wav.csv")
