tumourdata = read.csv("tumour.csv",row.names = 1)
tmatrix = cor(tumourdata[,2:31])
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(tmatrix, col=col, symm=TRUE)
heatmap(tmatrix)
tumourpca = prcomp(tumourdata[,2:31])
biplot(tumourpca)

tumourpca <- prcomp(tumourdata[,2:31],scale = TRUE)
biplot(tumourpca)
plot(tumourpca$x[,1], tumourpca$x[,2], col=tumourdata[,1], pch=16, xlab = "PC1", ylab = "PC2")
plot(tumourpca$x[,2], tumourpca$x[,3], col=tumourdata[,1], pch=16, xlab = "PC2", ylab = "PC3")
plot(tumourpca$x[,3], tumourpca$x[,4], col=tumourdata[,1], pch=16, xlab = "PC3", ylab = "PC4")
plot(tumourpca$x[,1], tumourpca$x[,3], col=tumourdata[,1], pch=16, xlab = "PC1", ylab = "PC3")

heatmap(cor(tumourpca$x), col=col)

plot(tumourpca, main = "Principal Components")
summary(tumourpca)

pcascores = tumourpca$x[,1:20]
library(MASS)
ldamodel = lda(pcascores,tumourdata[,1],CV=T)
table(tumourdata[,1],ldamodel$class)
class = table(tumourdata[,1],ldamodel$class)
diag(prop.table(class,1))
sum(diag(prop.table(class)))

ldamodel = lda(tumourdata[,2:31],tumourdata[,1],CV=T)
table(tumourdata[,1],ldamodel$class)
class = table(tumourdata[,1],ldamodel$class)
diag(prop.table(class,1))
sum(diag(prop.table(class)))

tumourpca$rotation[,1:4]