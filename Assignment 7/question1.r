bitmapDataFrame <- read.csv(file = "D:/Data Analytics/Assignment 7/optdigits.csv", header = TRUE, sep = ",")
#part (a)
#k-means clustering
print("Part (a)")
set.seed(10)
bitmapCluster <- kmeans(bitmapDataFrame, 10,nstart = 1, iter.max = 200)
print("Clustering Details :")
bitmapCluster #prints clustering details

#table command counts the occurence of each digit in the cluster
p <- table(bitmapCluster$cluster , bitmapDataFrame$digit)
print("Clustering without labels")
print(p)

#To find labels
r <- p
r <- cbind(r, as.numeric(colnames(p)[apply(p,1,which.max)]))
colnames(r)[length(colnames(r))] = "labels"
print("Clustering with labels")
print(r)

#part(b)
print("Part (b)")
#Cluster 1 seems to have an equal distribution of two digits in it
print("Cluster 1 seems to have an equal distribution of two digits in it. Digit 1 has value of 112 and digit 9 has value of 97. Hence, the difference is not that much.")
cluster1 <- bitmapDataFrame[bitmapCluster$cluster == 1,]
distance <- dist(cluster1, method = "euclidean")
hc <- hclust(distance, method = "complete")
print("Plotting the Dendrogram")
plot(hc, hang = 0.5, main = "Cluster Dendrogram", xlab = NULL, ylab = "Height")
#Cutting off the tree into two clusters
groups <- cutree(hc, k = 2)
rect.hclust(hc, k = 2, border = "blue")

w <- table(groups, cluster1$digit)
print(w)

#part (c)
print("Part (c)")
library(class)
bitmapTest <- read.csv(file = "D:/Data Analytics/Assignment 7/optdigits_test.csv", header = TRUE, sep = ",")
center <- bitmapCluster$centers
info <- matrix(,nrow= length(bitmapTest[,1]),ncol=3)
colnames(info) = c("image no.", "digit", "cluster")

print("Calculating distance")

for (i in 1:length(bitmapTest[,1]))
{
	for(j in 1:length(center[,1]))
	{
		distanceEach <- dist(rbind(bitmapTest[i,2:length(bitmapTest[1,])], center[j,1:length(center[1,])-1]), method = "euclidean", p = 2)
		if(j == 1)
		{
			min <- distanceEach
			c <- j
		}
		if(min >= distanceEach)
		{
			min <- distanceEach
			c <- j
		}
	}
	info[i,1] <- i
	info[i,2] <- r[c,11]
	info[i,3] <- c
}

print(info)

#Part d
print("Part (d)")
new <- w
new <- cbind(new,as.numeric(colnames(w)[apply(w,1,which.max)]))
colnames(new)[length(colnames(new))] = "label"

m <- matrix(,nrow = 2, ncol = 2)
colnames(m) = c("label", "Data Points")

n <- length(new[1,])
m[1,1] <- new[1,n]
m[1,2] <- sum(new[2,1:8])
m[2,1] <- new[2,n]
m[2,2] <- sum(new[2,1:8])

print(m)


