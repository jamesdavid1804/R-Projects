#Creating the data set
coffdata = read.table("coffee.txt")
#Perform principal component analysis
coffpca = prcomp(coffdata[,2:808])
#PCA score plots
boxplot(coffdata)                                         

#Performing principal component analysis on scaled data
coffpca <- prcomp(coffdata[,2:808],scale = TRUE)   
#PCA score plots on scaled data
biplot(coffpca)                                           

#Removing the outliers
coffdata <- coffdata[-c(13,52,53),]
#Performing principal component analysis on new data set
coffpca <- prcomp(coffdata[,2:808],scale = TRUE)  
#PCA score plots on new data
biplot(coffpca)                                           

#PCA plots for PC1 and PC2
plot(coffpca$x[,1],coffpca$x[,2], xlab="PC1", ylab="PC2", main="PCA  plot", pch=20, col="blue")
points(coffpca$x[1:39,1], coffpca$x[1:39,2], pch=20, col="red")
legend("topright", legend=c("Factory 1", " Factory 2"),col=c("red", "blue"), pch = 20,cex=0.8)

#PCA plots for PC2 and PC3
plot(coffpca$x[,2],coffpca$x[,3], xlab="PC2", ylab="PC3", main="PCA  plot", pch=20, col="blue")
points(coffpca$x[1:39,2], coffpca$x[1:39,3], pch=20, col="red")
legend("topright", legend=c("Factory 1", " Factory 2"),col=c("red", "blue"), pch = 20,cex=0.8)

#PCA plots for PC3 and PC4
plot(coffpca$x[,3],coffpca$x[,4], xlab="PC3", ylab="PC4", main="PCA  plot", pch=20, col="blue")
points(coffpca$x[1:39,3], coffpca$x[1:39,4], pch=20, col="red")
legend("topright", legend=c("Factory 1", " Factory 2"),col=c("red", "blue"), pch = 20,cex=0.8)

#PCA plots for PC1 and PC4
plot(coffpca$x[,1],coffpca$x[,4], xlab="PC1", ylab="PC4", main="PCA  plot", pch=20, col="blue")
points(coffpca$x[1:39,1], coffpca$x[1:39,4], pch=20, col="red")
legend("topright", legend=c("Factory 1", " Factory 2"),col=c("red", "blue"), pch = 20,cex=0.8)

#PCA plots for PC1 and PC3
plot(coffpca$x[,1],coffpca$x[,3], xlab="PC1", ylab="PC3", main="PCA  plot", pch=20, col="blue")
points(coffpca$x[1:39,1], coffpca$x[1:39,3], pch=20, col="red")
legend("topright", legend=c("Factory 1", " Factory 2"),col=c("red", "blue"), pch = 20,cex=0.8)

#PCA plots for P2 and PC4
plot(coffpca$x[,2],coffpca$x[,4], xlab="PC2", ylab="PC4", main="PCA  plot", pch=20, col="blue")
points(coffpca$x[1:39,2], coffpca$x[1:39,4], pch=20, col="red")
legend("topright", legend=c("Factory 1", " Factory 2"),col=c("red", "blue"), pch = 20,cex=0.8)

#View the facotry 1 data for PC2 and PC4
coffpca$x[1:39,2:4]     
#View the factory 2 data for PC2 and PC4
coffpca$x[40:62,2:4]                                               

#Sort loadings into descending order
sorted_pca <- sort(abs(coffpca$rotation[,4]), decreasing = TRUE)   
#Highest loadings
sorted_pca[1:6]                                                    
