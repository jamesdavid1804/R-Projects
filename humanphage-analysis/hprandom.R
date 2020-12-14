hp.data <- read.table("human-phage.txt")

test.rows <- sample(1:nrow(hp.data),200)
test <- hp.data[test.rows,]
train <- hp.data[-test.rows,]

test.rows <- sample(1:ncol(hp.data[,2:101]),40)
status = hp.data[,1]
practice <- cbind(status,hp.data[,test.rows])

test.rows <- sample(1:nrow(hp.data),200)
test <- hp.data[test.rows,]
train <- hp.data[-test.rows,]

install.packages("tree")
library(tree)

train[,1]


tree = tree(V1~.,data=train)
plot(tree)
text(tree,cex=0.7,pretty=0)
tree.pred = predict(tree, train[,-1], type="class")
h = table(predicted = tree.pred, true = train[,1])
acc.train <- (h[1,1]+h[2,2])/(sum(h))
err.train <- 1 - acc.train
tree.pred = predict(tree, test[,-1], type="class")
h = table(predicted = tree.pred, true = test[,1])
acc.test <- (h[1,1]+h[2,2])/(sum(h))
err.test <- 1 - acc.test

cv.prune = cv.tree(tree, FUN = prune.misclass)
plot(cv.prune)
prune.stat = prune.misclass(tree, best = 9)
prune.stat
plot(prune.stat)
text(prune.stat, cex=0.7, pretty = 0)
tree.pred = predict(prune.stat, train[,-1], type="class")
h = table(predicted = tree.pred, true = train[,1])
acc.n.train <- (h[1,1]+h[2,2])/(sum(h))
err.n.train <- 1 - acc.n.train
tree.pred = predict(prune.stat, test[,-1], type="class")
h = table(predicted = tree.pred, true = test[,1])
acc.n.test <- (h[1,1]+h[2,2])/(sum(h))
err.n.test <- 1 - acc.n.test

hp.test = cbind(as.numeric(hp.data$V1),as.numeric(hp.data$V12),as.numeric(hp.data$V45),as.numeric(hp.data$V53),as.numeric(hp.data$V82),as.numeric(hp.data$V91),as.numeric(hp.data$V94),as.numeric(hp.data$V98))
colnames(hp.test) = c("V1","V12","V45","V53","V82","V91","V94","V98")

data_pca = prcomp(hp.test[,2:8])
biplot(data_pca)
plot(data_pca$x[,1],data_pca$x[,2], xlab="PC1", ylab="PC2",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,1],data_pca$x[,3], xlab="PC1", ylab="PC3",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,1],data_pca$x[,4], xlab="PC1", ylab="PC4",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,2],data_pca$x[,3], xlab="PC2", ylab="PC3",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,2],data_pca$x[,4], xlab="PC2", ylab="PC4",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,3],data_pca$x[,4], xlab="PC3", ylab="PC4",main="PCA  plot", pch=15, col=unique(hp.num$V1))

library(klaR)
model1 = stepclass(V1~.,data=hp.test,method="lda",direction="forward",criterion="AC",improvement=0.01)
model2 = stepclass(V1~.,data=hp.num,method="lda",direction="forward",criterion="AC",improvement=0.01)

hp.prime = cbind(hp.test, hp.num$G, hp.num$GG,hp.num$C,hp.num$CC,hp.num$AA)
colnames(hp.prime) = c("V1","V12","V45","V53","V82","V91","V94","V98","G","GG","C","CC","AA")
model3 = stepclass(V1~.,data=hp.prime,method="lda",direction="forward",criterion="AC",improvement=0.01)
data_pca = prcomp(hp.prime[,2:8])
biplot(data_pca)
plot(data_pca$x[,1],data_pca$x[,2], xlab="PC1", ylab="PC2",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,1],data_pca$x[,3], xlab="PC1", ylab="PC3",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,1],data_pca$x[,4], xlab="PC1", ylab="PC4",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,2],data_pca$x[,3], xlab="PC2", ylab="PC3",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,2],data_pca$x[,4], xlab="PC2", ylab="PC4",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,3],data_pca$x[,4], xlab="PC3", ylab="PC4",main="PCA  plot", pch=15, col=unique(hp.num$V1))


hp.num = hp.data[,1:2]
hp.num$A = 0
for(i in c(1:600)){
  hp.num$A[i] = length(which(hp.data[i,] == "A"))
}
hp.num$T = 0
for(i in c(1:600)){
  hp.num$T[i] = length(which(hp.data[i,] == "T"))
}
hp.num$G = 0
for(i in c(1:600)){
  hp.num$G[i] = length(which(hp.data[i,] == "G"))
}
hp.num$C = 0
for(i in c(1:600)){
  hp.num$C[i] = length(which(hp.data[i,] == "C"))
}

hp.num$AA = 0
for (i in 1:nrow(hp.data)){
  for (j in 1:(ncol(hp.data)-1)){
    hp.num$AA[i] = hp.num$AA[i] + ((hp.data[i,j] == "A") & (hp.data[i,j+1] == "A"))
  } 
}
hp.num$TT = 0
for (i in 1:nrow(hp.data)){
  for (j in 1:(ncol(hp.data)-1)){
    hp.num$TT[i] = hp.num$TT[i] + ((hp.data[i,j] == "T") & (hp.data[i,j+1] == "T"))
  } 
}
hp.num$GG = 0
for (i in 1:nrow(hp.data)){
  for (j in 1:(ncol(hp.data)-1)){
    hp.num$GG[i] = hp.num$GG[i] + ((hp.data[i,j] == "G") & (hp.data[i,j+1] == "G"))
  } 
}
hp.num$CC = 0
for (i in 1:nrow(hp.data)){
  for (j in 1:(ncol(hp.data)-1)){
    hp.num$CC[i] = hp.num$CC[i] + ((hp.data[i,j] == "C") & (hp.data[i,j+1] == "C"))
  } 
}
hp.num = hp.num[,-c(2)]


data_pca = prcomp(hp.num[,2:9])
biplot(data_pca)
plot(data_pca$x[,1],data_pca$x[,2], xlab="PC1", ylab="PC2",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,1],data_pca$x[,3], xlab="PC1", ylab="PC3",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,1],data_pca$x[,4], xlab="PC1", ylab="PC4",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,2],data_pca$x[,3], xlab="PC2", ylab="PC3",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,2],data_pca$x[,4], xlab="PC2", ylab="PC4",main="PCA  plot", pch=15, col=unique(hp.num$V1))
plot(data_pca$x[,3],data_pca$x[,4], xlab="PC3", ylab="PC4",main="PCA  plot", pch=15, col=unique(hp.num$V1))

library(klaR)
model1 = stepclass(V1~.,data=hp.num,method="lda",direction="forward",criterion="AC",improvement=0.01)
model2 = stepclass(V1~.,data=,method="lda",direction="forward",criterion="AC",improvement=0.01)

hp.num2 = hp.data
for (i in c(1:600)){
  hp.num2[i,1] = as.numeric(hp.num2[i,1])
}

test.pca = prcomp(hp.test[,2:8])
summary(test.pca)

num.pca = prcomp(hp.num[,2:5])
summary(num.pca)

install.packages("nnet")
library(nnet)
test.rows <- sample(1:nrow(hp.num),200)
test <- hp.num[test.rows,]
train <- hp.num[-test.rows,]
nnmodel = nnet(V1~., data = hp.num, size = 10)
nnpred = predict(nnmodel, test[,-1], type = "class")
library(caret)
confusionMatrix(as.factor(nnpred), test$V1)



test.rows <- sample(1:nrow(hp.test),200)
test <- hp.test[test.rows,]
train <- hp.test[-test.rows,]
nnmodel = nnet(V1~., data = hp.test, size = 10)
nnpred = predict(nnmodel, test[,-1])
library(caret)
confusionMatrix(as.factor(nnpred), test$V1)

