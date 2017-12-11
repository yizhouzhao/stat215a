library(cluster)

#Not necessary to try cluster of more than two groups.

# Hierachical clustering

#hc = hclust(dist(t(resp_dat)))

#Kmeans clustering

#k3 = kmeans(t(resp_dat),centers = 3)
#k4 = kmeans(t(resp_dat),centers = 4)
#k5 = kmeans(t(resp_dat),centers = 5)
#k6 = kmeans(t(resp_dat),centers = 6)

#within = c(sum(k2$withinss),sum(k3$withinss),sum(k4$withinss),sum(k5$withinss),sum(k6$withinss))
#between =  c(sum(k2$betweenss),sum(k3$betweenss),sum(k4$betweenss),sum(k5$betweenss),sum(k6$betweenss))
#dis_k = data.frame(centers = 2:6, within = within, between = between)

# Get the gap statistics for kmeans clustering
gap <- clusGap(t(resp_dat), kmeans, K.max=6, B=50)

