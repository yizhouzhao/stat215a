#except for KDE and NMF, this is almost the same as the code in lab2_blind.Rnw, comments can be found there


####Kernel density plots
d=density(redwood_complete$humid_temp,bw=0.1,kernel="gaussian")

df=data.frame(temperature=d$x,density=d$y)

ggplot(df)+
  geom_line(aes(x=temperature,y=density))

d=density(redwood_complete$humid_temp,bw=0.5,kernel="gaussian")

df=data.frame(temperature=d$x,density=d$y)

ggplot(df)+
  geom_line(aes(x=temperature,y=density))

d=density(redwood_complete$humid_temp,bw=0.1,kernel="epanechnikov")

df=data.frame(temperature=d$x,density=d$y)

ggplot(df)+
  geom_line(aes(x=temperature,y=density))

d=density(redwood_complete$humid_temp,bw=0.5,kernel="epanechnikov")

df=data.frame(temperature=d$x,density=d$y)

ggplot(df)+
  geom_line(aes(x=temperature,y=density))


#loess smoother
redwood_subset = filter(redwood_complete, Hour == 14)

l = loess(humidity~Temperature, redwood_subset, method = "loess")
humid_approx = predict(l)
redwood_subset <- redwood_subset %>%
  mutate(loess_humidity = humid_approx)

ggplot(redwood_subset)+
  geom_point(aes(x = Temperature, y = humidity),alpha=0.2)+
  geom_line(aes(x= Temperature, y = loess_humidity), col='red')


l = loess(humidity~Temperature, redwood_subset, method = "loess",degree = 1)
humid_approx = predict(l)
redwood_subset <- redwood_subset %>%
  mutate(loess_humidity = humid_approx)

ggplot(redwood_subset)+
  geom_point(aes(x = Temperature, y = humidity),alpha=0.2)+
  geom_line(aes(x= Temperature, y = loess_humidity), col='red')

l = loess(humidity~Temperature, redwood_subset, method = "loess", span = 0.4, degree = 1)
humid_approx = predict(l)
redwood_subset <- redwood_subset %>%
  mutate(loess_humidity = humid_approx)

ggplot(redwood_subset)+
  geom_point(aes(x = Temperature, y = humidity),alpha=0.2)+
  geom_line(aes(x= Temperature, y = loess_humidity), col='red')

l = loess(humidity~Temperature, redwood_subset, method = "loess", span = 0.4)
humid_approx = predict(l)
redwood_subset <- redwood_subset %>%
  mutate(loess_humidity = humid_approx)

ggplot(redwood_subset)+
  geom_point(aes(x = Temperature, y = humidity),alpha=0.2)+
  geom_line(aes(x= Temperature, y = loess_humidity), col='red')


#####lab2 begins

lingdata <- read.table("~/Desktop/lab2/data/lingData.txt", header=TRUE)
lingloc <- read.table("data/lingLocation.txt", header = TRUE)

questions = load("~/Desktop/lab2/data/question_data.Rdata")


NumofNA <- rep(0,ncol(lingdata))
for(i in 1:ncol(lingdata)){
  
   NumofNA[i] = length(which(is.na(lingdata[,i]) == TRUE))
  
}

answers=rep(0,67)
for(i in 5:71){
  print(i)
  answers[i-4] = max(lingdata[,i],na.rm=TRUE)
}


NumofNonres <- rep(0,ncol(lingdata))
for(i in 1:ncol(lingdata)){
  
  NumofNonres[i] = length(which(lingdata[,i] == 0))
  
}

NumofNonres <- rep(0,nrow(lingdata))
for(i in 1:nrow(lingdata)){
  
  NumofNonres[i] = length(which(lingdata[i,] == 0))
  
}

length(which(NumofNonres == 0))
#40061

length(which(NumofNonres > 0))
#7410

length(which(NumofNonres > 10))
#1473

lingdata <- lingdata[-which(NumofNonres > 10),]

lingdata[lingdata == 0] <- NA


kendall_Cor <- matrix(0,67,67)

for(i in 5:71){
  print(i)
  for(j in i:71){
    kendall_Cor[(i-4),(j-4)] <- cor(lingdata[1:1000,i],lingdata[1:1000,j],method = "kendall",use="pairwise.complete.obs")
  }
}

library(ggmap)
map <- get_map(location = 'United States', zoom = 4, color ='bw')
mapPoints <- ggmap(map) +
  geom_point(aes(x = long, y = lat, col = as.factor(Q073)), data = lingdata, size = .4, alpha = .3)
plot(mapPoints)



lingdata$Q073[which(lingdata$Q073 !=1 & lingdata$Q073 !=6)] <- "others"

lingdata$Q073[which(is.na(lingdata$Q073)==TRUE)] <- "others"

lingdata$Q073[which(lingdata$Q073 ==1)] <- "sneakers"

lingdata$Q073[which(lingdata$Q073 == 6)] <- "tennis shoes"


lingdata <- lingdata%>%
  mutate(rubber_soled_shoes = Q073)


mapPoints <- ggmap(map) +
  geom_point(aes(x = long, y = lat, col = rubber_soled_shoes), data = lingdata, size = .4, alpha = .3)
plot(mapPoints)


lingdata$Q105[which(lingdata$Q105>3)] <- "others"

lingdata$Q105[which(is.na(lingdata$Q105)==TRUE)] <- "others"

lingdata$Q105[which(lingdata$Q105 == 1)] <- "soda"

lingdata$Q105[which(lingdata$Q105 == 2)] <- "pop"

lingdata$Q105[which(lingdata$Q105 == 3)] <- "coke"


lingdata <- lingdata%>%
  mutate(sweetened_carbonated_beverage = Q105)


mapPoints <- ggmap(map) +
  geom_point(aes(x = long, y = lat, col = sweetened_carbonated_beverage), data = lingdata, size = .4, alpha = .3)
plot(mapPoints)

for(i in 5:71){
  
  lingdata[,i] <- as.factor(lingdata[,i])
  
}

ling_bin = model.matrix(~ . + 0, data=lingdata[,5:71], contrasts.arg = lapply(lingdata[,5:71], contrasts, contrasts=FALSE))

cov_mat = cov(ling_bin)
eigens <- eigen(cov_mat)
eigenvalue <- eigens$value

ling_bin_pca <- prcomp(ling_bin)

ling_by_state <- lingdata%>%
  group_by(STATE)%>%
  summarise(count=n())


ling_bin <- ling_bin%>%
  mutate(PC1 = (ling_bin_pca$x)[,1])

ling_bin <- ling_bin%>%
  mutate(PC2 = (ling_bin_pca$x)[,2])

id = as.numeric(row.names(ling_bin))
ling_bin <- ling_bin%>%
  mutate(ID = id)

ling_bin <- as.data.frame(ling_bin)

ling_bin <- merge(x = ling_bin, 
                  y = lingdata,
                  by.x = "ID",
                  by.y = "ID")

ling_bin_rep = model.matrix(~ . + 0, data=lingdata[,5:71], contrasts.arg = lapply(lingdata[,5:71], contrasts, contrasts=FALSE))

ling_bin%>%
  filter(STATE == 'IL'|STATE == 'NY')%>%
  ggplot()+
  geom_point(aes(x = PC1, y=PC2, col= STATE), size = 1, alpha= .2)



####Non-negative matrix factorization using package NMF
library(NMF)

subset = sample.int(nrow(ling_bin_rep),5000)

ling_bin_nmf <- nmf(t(ling_bin_rep[subset,which(colSums(ling_bin_rep[subset,])>0)]),10, seed = "nndsvd")

W= basis(ling_bin_nmf) 

H= coef(ling_bin_nmf)

ling_bin_subset <- ling_bin[subset,]

ling_bin_subset <- ling_bin_subset%>%
  mutate(NMF_1 = H[1,])

ling_bin_subset <- ling_bin_subset%>%
  mutate(NMF_2 = H[2,])

ling_bin_subset%>%
  filter(STATE == 'TX'|STATE == 'NY'|STATE == 'IL')%>%
  ggplot()+
  geom_point(aes(x = NMF_1, y=NMF_2, col= STATE), size = 1, alpha= .2)
########

ling_specc <- kmeans(ling_bin_pca$x[,1:50],centers = 3)

ling_bin <- ling_bin%>%
  mutate(cluster = as.factor(ling_specc_3$cluster))

map <- get_map(location = 'United States', zoom = 4, color ='bw')
mapPoints <- ggmap(map) +
  geom_point(aes(x = long, y = lat, col = cluster), data = ling_bin, size = .4, alpha = .3)
plot(mapPoints)

subset2 = sample.int(nrow(ling_bin_rep),10000)

ling_bin_nmf2 <- nmf(t(ling_bin_rep[subset2,which(colSums(ling_bin_rep[subset2,])>0)]),10, seed = "nndsvd")

ling_bin_kmeans <- kmeans(ling_bin_rep, centers = 3)
ling_bin <- ling_bin%>%
  mutate(cluster = as.factor(ling_bin_kmeans$cluster))

withinss <- rep(0,10)
for(k in 1:10){
  #print(k)
ling_specc <- kmeans(ling_bin_rep,centers = k)
withinss[k] <- ling_specc$tot.withinss
}

map <- get_map(location = 'Indiana', zoom = 5, color ='bw')
mapPoints <- ggmap(map) +
  geom_point(aes(x = long, y = lat, col = cluster), data = ling_bin, size = .4, alpha = .3)
plot(mapPoints)

ling_bin_cont <- ling_bin%>%
  filter (lat>=35 & lat<=45 &lon > -92&lon< -80 &cluster != 1)

ling_bin_cont_2 <- ling_bin_cont%>%
  filter(cluster == 2)

ling_bin_cont_3 <- ling_bin_cont%>%
  filter(cluster == 3)

m2 = colSums((ling_bin_cont_2[,1:440]))/nrow(ling_bin_cont_2)

m3 = colSums((ling_bin_cont_3[,1:440]))/nrow(ling_bin_cont_3)

train = sample.int(nrow(ling_bin_rep),0.75*nrow(ling_bin_rep))

kmeans_train = ling_bin_rep[train, ] 

kmeans_val = ling_bin_rep[-train, ]
closest.cluster <- function(x) {
  cluster.dist <- apply(ling_kmeans_3$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist))
}
cluster[train]<- apply(kmeans_train, 1, closest.cluster)
cluster[-train]<- apply(kmeans_val, 1, closest.cluster)

ling_bin <- ling_bin%>%
  mutate(cluster = as.factor(cluster))


ling_bin_kmeans <- kmeans(kmeans_train,centers = 3)
centers = ling_bin_kmeans$centers
center_var = colSums(centers^2)/3-(colSums(centers)/3)^2
topten = order(center_var, decreasing = TRUE)[1:10]
quest_name = c("Q073a","Q073c","Q103d","Q103c", "Q105a","Q105b","Q076a", "Q076d", "Q050i","Q080a")
quest_diff = data.frame(Question = rep(quest_name,3),
           Percentage = c(centers[1,topten], centers[2,topten], centers[3,topten]),
           Group = as.factor(c(rep(1,10),rep(2,10),rep(3,10))))


ggplot(quest_diff,aes(x=Question,y=Percentage,fill=Group))+
  geom_bar(stat="identity",position="dodge")

map <- get_map(location = 'Indiana', zoom = 6, color ='bw')
mapPoints <- ggmap(map) +
  geom_point(aes(x = long, y = lat, col = cluster), data = ling_bin, size = .4, alpha = .3)
plot(mapPoints)

library(ggmap)
map <- get_map(location = 'Indianapolis', zoom = 8, color ='bw')
mapPoints <- ggmap(map) +
  geom_point(aes(x = long, y = lat, col = cluster), data = ling_bin, size = 2, alpha = 1)
plot(mapPoints)


train = sample(nrow(ling_bin_rep),0.75*nrow(ling_bin_rep))

kmeans_train = ling_bin_rep[train, ] 
ling_kmeans_3 <- kmeans(kmeans_train,centers = 3)
center = ling_kmeans_3$centers[1,]
dist = rep(0,100)
for(i in 1:100){ #draw 100 samples, each has 75% of the whole dataset
  train = sample.int(nrow(ling_bin_rep),0.75*nrow(ling_bin_rep))
  #print(train[1:100])
  kmeans_train = ling_bin_rep[train, ] 
  ling_kmeans_3 <- kmeans(kmeans_train,centers = 3)
  
  dist[i] = min(sum((center-ling_kmeans_3$centers[1,])^2),sum((center-ling_kmeans_3$centers[2,])^2),sum((center-ling_kmeans_3$centers[3,])^2))
  
}
#print(dist)
#hist(dist)

qplot(sqrt(c(dist,dist2)),
      binwidth =0.02,
      xlim=c(0,0.2),
      xlab = "Distance to oracle cluster center",
      ylab = "Frequency")
