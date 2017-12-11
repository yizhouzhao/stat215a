
load("fMRIdata.RData")

head(resp_dat)

k2 = kmeans(t(resp_dat),centers = 2)
k3 = kmeans(t(resp_dat),centers = 3)
k4 = kmeans(t(resp_dat),centers = 4)
k5 = kmeans(t(resp_dat),centers = 5)
k6 = kmeans(t(resp_dat),centers = 6)

within = c(sum(k2$withinss),sum(k3$withinss),sum(k4$withinss),sum(k5$withinss),sum(k6$withinss))

between =  c(sum(k2$betweenss),sum(k3$betweenss),sum(k4$betweenss),sum(k5$betweenss),sum(k6$betweenss))

dis_k = data.frame(centers = 2:6, within = within, between = between)

library(ggplot2)
library(reshape2)

dis_plot = melt(dis_k,id = "centers")

dis_plot

ggplot(data = dis_plot, aes(x = centers,y = value,color = variable)) + geom_point() + geom_line()

library(cluster)

gap <- clusGap(t(resp_dat), kmeans, K.max=6, B=50)

plot(gap,ylim = c(0.1,0.25),main = "Gap statistics for kmeans clustering")
 abline(v=k, lty=2, lwd=2, col="Blue")

k2$cluster

loc_dat = data.frame(loc_dat)
colnames(loc_dat) = c("x","y","z")
loc_dat$cluster = k2$cluster

library(scatterplot3d)

with(loc_dat, {
   s3d <- scatterplot3d(x, y, z,        # x y and z axis
                 color=cluster, pch=19,        # filled blue circles
                 #type="h",                    # vertical lines to the x-y plane
                 main="3-D Scatterplot of Voxels with two clusters",
                 xlab="X",
                 ylab="Y",
                 zlab="Z")
    s3d.coords <- s3d$xyz.convert(x, y, z) # convert 3D coords to 2D projection
    text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
         labels=row.names(loc_dat),               # text to plot
         cex=.5, pos=4)           # shrink text 50% and place to right of points)
    legend("topleft", inset=.05,      # location and inset
    bty="n", cex=.5,              # suppress legend box, shrink text 50%
    title="cluster",
    c("1", "2"), fill=c("red", "black"))
})

train_x = fit_feat[1:1400,]
train_y = resp_dat[1:1400,]

test_x = fit_feat[1401:1750,]
test_y = resp_dat[1401:1750,]

select_index = (1:20)[k2$cluster == 2] 

select_index

train_y_2 = train_y[,select_index]

train_data_1 = cbind(train_y_2[,1],train_x)

colnames(train_data_1) = c("Y",colnames(train_x))

head(train_data_1)

rf_model <- randomForest(train_x,train_y[,10], ntree = 500, maxnodes = )

library(randomForest)

Yhat1 = predict(rf_model, newdata = test_x)

library(e1071)

cor(Yhat1,test_y[,20])

cor(svm01$fitted,train_y_2[,1])

cor(svm01$fitted,train_y_2[,1])

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
    
    train_data = cbind(train_y[,i],train_x[,feature_index])
    colnames(train_data) = c("Y")
    model = cv.glmnet(train_x[,feature_index],train_y[,i],
                      type.measure = "mse",nfolds = 10, nlambda = 50)
    yhati =  predict(model, s=model$lambda.1se, newx=test_x[,feature_index])
    cor_pred[i] = cor(yhati,test_y[,i])
        print(cor_pred[i])
    }
    return(cor_pred)
}

library(caret)

cv.lasso.m = cv.glmnet(train_x,train_y[,select_index],type.measure = "mse",family="mgaussian",nfolds = 10, nlambda = 100)
yhatm <- predict(cv.lasso.m, s=cv.lasso.m$lambda.1se, newx=test_x)
yhatm = data.frame(yhatm)

library(glmnet)

cor(yhatm[,1],test_y[,16])

yhatm = data.frame(yhatm)

class(yhatm)

select_index

library(FNN)

model  = knn.reg(train = train_x, y = train_y[,10],k = 10,algorithm = "kd_tree")

str(model)

train_y[,10][1:10]

str(svm01)

str(rf_model)

rf_model_1 <- randomForest(train_x,train_y[,10], ntree = 1000, maxnodes = 100)
rf_model_2<- randomForest(train_x,train_y[,11], ntree = 1000, maxnodes = 100)
rf_model_3 <- randomForest(train_x,train_y[,13], ntree = 1000, maxnodes = 100)
rf_model_4<- randomForest(train_x,train_y[,16], ntree = 1000, maxnodes = 100)
rf_model_5<- randomForest(train_x,train_y[,20], ntree = 1000, maxnodes = 100)


str(rf_model_1)

cor(yhat1,test_y[,10])

yhat1 = predict(rf_model_1, newdata = test_x)

dim(test_x)

yhat10 = predict(rf_model_1,newdata = val_feat)
yhat11 = predict(rf_model_2,newdata = val_feat)
yhat13 = predict(rf_model_3,newdata = val_feat)
yhat14 = predict(rf_model_4,newdata = val_feat)
yhat15 = predict(rf_model_5,newdata = val_feat)




dim(test_x)

dim(val_feat)

yhat1 = predict(rf_model_1, newdata = val_feat)
yhat2 = predict(rf_model_2, newdata = val_feat)
yhat3 = predict(rf_model_3, newdata = val_feat)
yhat4 = predict(rf_model_4, newdata = val_feat)
yhat5 = predict(rf_model_5, newdata = val_feat)

my_prediction = data.frame(V10 = yhat1,V11 = yhat2, V13 = yhat3, V16 = yhat4, V20 = yhat5)

write.csv(my_prediction,"outer_part.csv",row.names = F)

cv.lasso.m = cv.glmnet(train_x,train_y,type.measure = "mse",family="mgaussian",nfolds = 10, nlambda = 100)


yhatm <- predict(cv.lasso.m, s=cv.lasso.m$lambda.1se, newx=test_x)

yhatm = data.frame(yhatm)

cor(yhatm[,1],test_y[,1])

yhatf <- predict(cv.lasso.m, s=cv.lasso.m$lambda.1se, newx=val_feat)

yhatf = data.frame(yhatf)

dim(yhatf)

write.table(yhatf[,1],"predv1_yizhou.txt",row.names = F,col.names = F)

cv.lasso.m$cvm

mse = vector()

yhat = predict(cv.lasso.m, s=cv.lasso.m$lambda.1se, newx=test_x)
yhat = data.frame(yhat)
for(i in 1:20){
    mse[i] = mean((yhat[,i]-train_y[,i])^2)
}

mse

plot(mse,type = "l")

write.csv(mse,file = "mse.csv",row.names = F)

cv.lasso.m$cvsd

yhat = predict(cv.lasso.m, s=cv.lasso.m$lambda.1se, newx=test_x)
yhat = data.frame(yhat)

cor(yhat[,20],test_y[,20])
