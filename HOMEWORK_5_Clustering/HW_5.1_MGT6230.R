#Task 1: K-means Clustering and Scaling

#1. Perform K-means clustering with 3 clusters on the original data without scaling the
#variables. Set the number of random initial starts to be 20. Show the clustering output

data <- read.csv("ShoppingVisits.csv")
str(data) 

k_means <- kmeans(data, 3, nstart = 20)
k_means

#2. Scale the variables, and then perform K-means clustering with 3 clusters on the scaled data
#(with 20 random initial starts). Show the clustering output.

data_scaled <- scale(data)
data_scaled

k_means_scaled <- kmeans(data_scaled, 3, nstart=20)
k_means_scaled

#3. Create a scatter plot of the original data points with points belonging to different clusters
#marked in different colors. Create such a plot for both the clustering outcomes with and
#without scaling, and display the two plots side by side for comparison.
#Note: Data scaling is only for the cluster analysis (i.e., to obtain the cluster assignments);
#when drawing the scatter plot, we should still draw the data points in their original scales.

par(mfrow=c(1,2))
plot(data, col=k_means$cluster, pch=20, main='K-means Without Scaling')
plot(data, col=k_means_scaled$cluster, pch=20, main='K-means Scaled')

#4. Discuss the effects of variable scaling on the outcomes of cluster analysis.

#Task 2: K-means with Different Number of Clusters

#5. Perform K-means clustering with 2 clusters, 4 clusters, and 5 clusters, respectively, on the
#scaled data (with 20 random initial starts). Save all the cluster outcomes.

k_means_2 <- kmeans(data_scaled, 2, nstart=20)
k_means_4 <- kmeans(data_scaled, 4, nstart=20)
k_means_5 <- kmeans(data_scaled, 5, nstart=20)

#6. Display in a 2x2 panel the four scatter plots for the clustering outcomes with 2, 3, 4, and 5
#clusters, respectively. Same as previously, mark the original data points belonging to
#different clusters in different colors. Label the plots properly.

par(mfrow=c(2,2))
plot <- plot(data, col=k_means_2$cluster, pch=20, main='K-means 2 Clusters')
plot <- plot(data, col=k_means_scaled$cluster, pch=20, main='K-means 3 Clusters')
plot <- plot(data, col=k_means_4$cluster, pch=20, main='K-means 4 Clusters')
plot <- plot(data, col=k_means_5$cluster, pch=20, main='K-means 5 Clusters')

#7. Based on these outputs, intuitively, which number of clusters do you think is the most
#appropriate for this data set?

#Task 3: Choosing k
#8. In order to choose the most reasonable value for k, let’s create the so-called “elbow chart,”
#that is, a line chart depicting the total within-cluster sum of squares for the clustering
#outcomes under different number of clusters (k). Plot the elbow chart for k=1,…,5

#ss = sum fs squares
ss1 <- k_means_2$totss
ss2 <- k_means_2$tot.withinss
ss3 <- k_means_scaled$tot.withinss
ss4 <- k_means_4$tot.withinss
ss5 <- k_means_5$tot.withinss

ss.vec <- c(ss1, ss2, ss3, ss4, ss5)
par(mfrow=c(1,1))
plot(ss.vec, type='b', xlab="Number of Clusters", ylab="Total Within Cluster SS")

#the elbow plot shows that 3 clusters is the most appropriate as ss is decreasing after 5 clusters

#Task 4: Hierarchical Clustering and Dendrogram
#10. Now perform the hierarchical clustering on the same scaled data. Use the usual Euclidean
#distance. Apply the average linkage first. Plot the dendrogram and properly label it.
hier_cluster_average <- hclust(dist(data_scaled), method ="average")
plot(hier_cluster_average, cex=0.5, main="Dendrogram (Average Linkage)", xlab='', ylab='', sub='')

#11. If we want to have three clusters, what is a proper height where we can cut the tree?
#Mark this height onto the dendrogram as a dashed horizontal line.
abline(h=1.8, col='red', lty=2)

#Task 5: K-means vs. Hierarchical
#12. Form three clusters from the hierarchical clustering output. Again, create a scatter plot of
#the original data points with their cluster membership marked in different colors. Display
#this plot alongside the scatter plot from K-means clustering with the same number of
#clusters (i.e., 3 clusters).

cutree(hier_cluster_average, k=3)
par(mfrow=c(1,2))
plot <- plot(data, col=k_means_scaled$cluster, pch=20, main='K-means 3 Clusters')

plot <- plot(data, col=cutree(hier_cluster_average, k=3), pch=20, main='Hierarchical 3 Clusters')
#Even though they draw the boundary line slightly differently in that bottom-right corner, the overall structure is highly robust. Both methods cleanly 
#isolate the top-left quadrant and separate the remaining data into distinct behavioral regions. 

#Task 6: Hierarchical Clustering with Different Linkages
#14. Now perform the same hierarchical clustering using different linkages. In addition to the
#average linkage that has been applied, now use complete, single, and centroid linkages,
#respectively. Create a scatter plot (with colored cluster membership) for each case, and
#display all four of them in a 2x2 panel. Label them properly.

par(mfrow=c(2,2))

plot <- plot(data, col=cutree(hier_cluster_average, k=3), pch=20, main='Hierarchical Average')

hier_cluster_complete <- hclust(dist(data_scaled), method ="complete")
plot <- plot(data, col=cutree(hier_cluster_complete, k=3), pch=20, main='Hierarchical Complete')

hier_cluster_single <- hclust(dist(data_scaled), method ="single")
plot <- plot(data, col=cutree(hier_cluster_single, k=3), pch=20, main='Hierarchical Single')

hier_cluster_centroid <- hclust(dist(data_scaled), method ="centroid")
plot <- plot(data, col=cutree(hier_cluster_centroid, k=3), pch=20, main='Hierarchical Centroid')

#Single linkage has the least reasonable result because one cluster has only two data points whereas another cluster contains too many data points. 
#This is the result of trailing clusters, a major limitation of Single linkage
