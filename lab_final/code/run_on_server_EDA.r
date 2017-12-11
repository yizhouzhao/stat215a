
#this file is not runnable, it need the more raw data sets, and ipython notebook on SCF server to show the training results.

library(corrplot)
library(ggplot2)
library(reshape2)
library(rgl)
library(Matrix)
library(dplyr)
library(scatterplot3d) 
library(graphics)

images = read.csv("fit_stim.csv",nrows = 10)

load("fMRIdata.RData")

loc_dat = data.frame(loc_dat)
colnames(loc_dat) = c("x","y","z")

with(loc_dat, {
   s3d <- scatterplot3d(x, y, z,        # x y and z axis
                 color="blue", pch=19,        # filled blue circles
                 type="h",                    # vertical lines to the x-y plane
                 main="3-D Scatterplot Example 3",
                 xlab="Displacement (cu. in.)",
                 ylab="Weight (lb/1000)",
                 zlab="Miles/(US) Gallon")
    s3d.coords <- s3d$xyz.convert(x, y, z) # convert 3D coords to 2D projection
    text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
         labels=row.names(loc_dat),               # text to plot
         cex=.5, pos=4)           # shrink text 50% and place to right of points)
})

s3d <- scatterplot3d(loc_dat$x, loc_dat$y, loc_dat$z)

rownames(loc_dat) = 1:20
colnames(loc_dat) = c("x","y","z")
loc_dat = data.frame(loc_dat)

sample1 = matrix(images[1,],nrow = 128,ncol = 128)

sample1 = RotateImageVector(images[1,])

sample1 = matrix(unlist(images[1,]),128,128)
sample1 = t(sample1[128:1,])
image(sample1,col = gray(1:128/129))

wsample1 = matrix(unlist(fit_feat[1,1:10816]),104,104)
wsample1 = wsample1[1:104,]
image(wsample1,col = gray(128:1/129))

colnames(resp_dat) = paste0("V" ,as.character(1:20))

density_resp = melt(resp_dat,id = colnames(resp_dat))
colnames(density_resp) = c("id","voxel","response")

ggplot(density_resp, aes(response, ..density.., colour = voxel)) +
  geom_density() + ggtitle("Density plot for the response")

blank_theme <- theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 8))

library(glmnet)

train_x = fit_feat[1:1400,]
train_y = resp_dat[1:1400,]

test_x = fit_feat[1401:1750,]
test_y = resp_dat[1401:1750,]

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

table = Get.IC.Table(train_x,train_y,1)

Y1 = train_y[,1]
fit.lasso.1 <- glmnet(train_x, Y1, alpha = 1, nlambda = 100)
#tLL <- fit.lasso.1$nulldev - deviance(fit.lasso.1)
#k <- fit.lasso.1$df
#an <- fit.lasso.1$nobs
#AIC <-  -tLL+2*k
#AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
#BIC<-log(n)*k - tLL

cv.lasso.1 = cv.glmnet(train_x,Y1,type.measure = "mse",nfolds = 10,nlambda = 100)

length(cv.lasso.1$lambda)

CVtable = data.frame(lambda = fit.lasso.1$lambda,AIC = AIC, AICc = AICc, BIC = BIC)

CVplot = melt(table,id = "lambda")

ggplot(data = CVplot,aes(x = lambda,y = value,colour = variable)) + geom_line() + ylim(c(-500,500))

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

ESCV = vector()

for(i in 1:50){
    ESCV[i] = ES(train_x[1:1200,],train_y[,1][1:1200],train_x[1201:1400,],fit.lasso.1$lambda[i])
    print(ESCV[i])
}

write.csv(file = "ESCV1.csv",ESCV)


plot(ESCV)

Lasso.cor = c(0.4639530,0.5219477,0.5039433,0.4164676,0.4981058,0.5329392,0.5665793,
0.5483646,0.6338439,0.1435815,0.3358171,0.4303465,0.09696409,0.2029987,
0.5238992,0.09,0.3444867,0.5683686,0.3487718,0)

write.csv(file = "lasso_cor.csv",data.frame(individual = Lasso.cor),row.names = F)

Y1 = train_y[,1]

names(feature_cor) = 1:ncol(train_x)

feature_index = as.numeric(names(sort(feature_cor,decreasing = T))[1:1000])

Get.Corr.Table <- function(train_x,train_y,test_x,test_y){
    cor_pred = vector()
    for (i in 1:20) {
        print(i)
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
        print(cor_pred[i])
    }
    return(cor_pred)
}

lasso.cor.2 = Get.Corr.Table(train_x,train_y,test_x,test_y)

ESCV

real.wav = read.csv("real_wav.csv")

head(real.wav)

coef_table = read.csv("coef.csv")
select_index = coef_table$index[2:nrow(coef_table)]

select_wav = real.wav[,select_index]

head(select_wav)

save(select_wav,file = "select_wav.Rdata")

select_wav$sum = rowSums(select_wav)

area = matrix(unlist(select_wav$sum),128,128)
area = t(area[128:1,])
image(area,col = gray(1:128/129))

sample1 = matrix(unlist(images[10,]),128,128)
sample1 = t(sample1[128:1,])
image(sample1,col = gray(1:128/129))

sample2 = sample1 * as.matrix(blur(as.im(area),4))
image(sample2,col = gray(1:128/129))

density_points = data.frame(x = 0, y = 0)
for(i in 1:128){
    for(j in 1:128){
        if(sample1[i,j] == FALSE){
            density_points = rbind(density_points,c(i,j))
        }
    }
}

 ggplot(density_points, aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")

head(coef_table)

coef_table = coef_table[-1,]

library(spatstat)

select_fit_feat = fit_feat[,select_index]

dim(select_fit_feat )

head(select_fit_feat)

coef_density = coef_table[,3:ncol(coef_table)]

dim(area)

save(area,file = "feature_area.Rdata")

Get.Feature.Indexes <- function(train_x,train_y){
  folds <- cut(seq(1,nrow(train_x)),breaks=10,labels=FALSE)
  coef_list = list()
  for(j in 1:10){
      print(j)
    cvIndexes <- which(folds==j,arr.ind=TRUE)
    cv.lasso.m = cv.glmnet(train_x[cvIndexes,],train_y[cvIndexes,],type.measure = "mse",
                           family="mgaussian",nfolds = 10, nlambda = 100)
    co = coef(cv.lasso.m, s = "lambda.1se")
    coef_list[[j]] = co[[1]]@i
  }
  return(coef_list)
}

coef_list = Get.Feature.Indexes(train_x,train_y)

library(cluster)

head(train_x)

write.csv(images,file = "sample_images.csv",row.names = F)

save(images,file = "samples.Rdata")

library(corrplot)
M = cor(resp_dat)

corrplot(M, method="circle")

lasso_lambda = read.csv("lasso_lambda.csv")[,-1]
lasso_lambda$label = "MIN"
lasso_lambda$label.2 = "1SE"

lasso_lambda_plot = rbind(lasso_lambda[,c(1,2,3,6)],
                          cbind(data.frame(voxel = 1:20, label = "1SE"),lasso_lambda[,4:5]))

lasso_lambda_1 = lasso_lambda[,c(1,2,3,6)]

colnames(lasso_lambda_1) = c("voxel","mean","sd","label")

lasso_lambda_2 = lasso_lambda[,c(1,4,5,7)]
colnames(lasso_lambda_1) = c("voxel","mean","sd","label")

lasso_plot = rbind(lasso_lambda_1,lasso_lambda_2)

lasso_lambda_2
