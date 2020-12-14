#DNA Data

#reading in and preparing data sets
dna.data <- read.table("human-phage.txt")
dna.num = dna.data[,1:2]
dna.num$A = 0
for(i in c(1:600)){
  dna.num$A[i] = length(which(dna.data[i,] == "A"))
}
dna.num$T = 0
for(i in c(1:600)){
  dna.num$T[i] = length(which(dna.data[i,] == "T"))
}
dna.num$G = 0
for(i in c(1:600)){
  dna.num$G[i] = length(which(dna.data[i,] == "G"))
}
dna.num$C = 0
for(i in c(1:600)){
  dna.num$C[i] = length(which(dna.data[i,] == "C"))
}
dna.num$AA = 0
for (i in 1:nrow(dna.data)){
  for (j in 1:(ncol(dna.data)-1)){
    dna.num$AA[i] = dna.num$AA[i] + ((dna.data[i,j] == "A") & (dna.data[i,j+1] == "A"))
  } 
}
dna.num$TT = 0
for (i in 1:nrow(dna.data)){
  for (j in 1:(ncol(dna.data)-1)){
    dna.num$TT[i] = dna.num$TT[i] + ((dna.data[i,j] == "T") & (dna.data[i,j+1] == "T"))
  } 
}
dna.num$GG = 0
for (i in 1:nrow(dna.data)){
  for (j in 1:(ncol(dna.data)-1)){
    dna.num$GG[i] = dna.num$GG[i] + ((dna.data[i,j] == "G") & (dna.data[i,j+1] == "G"))
  } 
}
dna.num$CC = 0
for (i in 1:nrow(dna.data)){
  for (j in 1:(ncol(dna.data)-1)){
    dna.num$CC[i] = dna.num$CC[i] + ((dna.data[i,j] == "C") & (dna.data[i,j+1] == "C"))
  } 
}
dna.num = dna.num[,-c(2)]


#Tree
install.packages("tree")
library(tree)
test.rows <- sample(1:nrow(dna.data),180)
test <- dna.data[test.rows,]
train <- dna.data[-test.rows,]
tree = tree(as.factor(V1)~.,data=train)
plot(tree)
text(tree,cex=0.7,pretty=0)
tree.pred = predict(tree, train[,-1], type="class")
h = table(predicted = tree.pred, true = train[,1])
tree.acc.train <- (h[1,1] + h[2,2])/(sum(h))
tree.pred = predict(tree, test[,-1], type="class")
h = table(predicted = tree.pred, true = test[,1])
tree.acc.test <- (h[1,1] + h[2,2])/(sum(h))
#Prune
cv = cv.tree(tree, FUN = prune.misclass)
plot(cv)
prune = prune.misclass(tree, best = 24)
prune.pred = predict(prune, dna.data[,-1], type="class")
h = table(predicted = prune.pred, true = dna.data[,1])
prune.acc <- (h[1,1] + h[2,2])/(sum(h))
plot(prune)
text(prune,cex=0.7,pretty=0)

#PCA
data_pca = prcomp(dna.num[,2:9])
par(mfrow=c(1,1))
biplot(data_pca)
dna.num <- dna.num[-c(3),]
data_pca = prcomp(dna.num[,2:9])
par(mfrow=c(3,1))
plot(data_pca$x[,1],data_pca$x[,2], xlab="PC1", ylab="PC2",main="PC1/PC2 plot", pch=15, col=c("red","blue")[dna.num$V1])
legend("topleft", legend = c("Pos", "Neg"), pch = 15, col = c("red", "blue"))
plot(data_pca$x[,1],data_pca$x[,3], xlab="PC1", ylab="PC3",main="PC1/PC3 plot", pch=15, col=c("red","blue")[dna.num$V1])
legend("topleft", legend = c("Pos", "Neg"), pch = 15, col = c("red", "blue"))
plot(data_pca$x[,1],data_pca$x[,4], xlab="PC1", ylab="PC4",main="PC1/PC4 plot", pch=15, col=c("red","blue")[dna.num$V1])
legend("topleft", legend = c("Pos", "Neg"), pch = 15, col = c("red", "blue"))
data_pca

#LDA sums
test.rows <- sample(1:nrow(dna.num),180)
test <- dna.num[test.rows,]
train <- dna.num[-test.rows,]
lda = lda(train[,2:7],as.factor(train[,1]))
lda
lda.pred = predict(lda, train[,-1])
h = table(predicted = lda.pred$class, true = train[,1])
lda.acc.train <- (h[1,1] + h[2,2])/(sum(h))
lda.pred = predict(lda, test[,-1], type="class")
h = table(predicted = lda.pred$class, true = test[,1])
lda.acc.test <- (h[1,1] + h[2,2])/(sum(h))

#Neural-Network Model for num
install.packages("caret")
library(caret)
test.rows <- sample(1:nrow(dna.num),180)
test <- dna.num[test.rows,]
train <- dna.num[-test.rows,]
numFolds = trainControl(method = 'cv', number = 10, savePredictions = T)
nnmodel = train(V1~ ., data = train, method = 'nnet', preProcess = c('center'), trControl = numFolds)
nnmodel$results
grid = expand.grid(size=3, decay = 0.1)
nnmodel2 = train(V1~.,data = train, method = 'nnet', preProcess = c('center'),trControl = numFolds, tuneGrid = grid)
nnmodel2$results
nnpred = predict(nnmodel2, train[,-1])
h = table(predicted = nnpred, true = train[,1])
nnpred.acc.train <- (h[1,1] + h[2,2])/(sum(h))
nnpred = predict(nnmodel2, test[,-1])
h = table(predicted = nnpred, true = test[,1])
nnpred.acc.test <- (h[1,1] + h[2,2])/(sum(h))

#random forest
install.packages("randomForest")
library(randomForest)
dna.data1 = dna.data
test.rows <- sample(1:nrow(dna.data),180)
test <- dna.data1[test.rows,]
train <- dna.data1[-test.rows,]
oob.err = double(10)
test.err = double(10)
for(mtry in 1:10){
  model = randomForest(train[,1]~., data = train[,-1], mtry=mtry, ntree = 400)
  oob.err[mtry] = model$err.rate[400,1]
  pred = predict(model, test[,-1])
  h = table(predicted = pred, true = test[,1])
  test.err[mtry] = 1 - ((h[1,1]+h[2,2])/sum(h))
}
par(mfrow=c(1,1))
matplot(1:mtry, cbind(oob.err, test.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))
rfmodel = randomForest(train[,1]~., data = train[,-1], mtry=6, ntree = 400)
pred = predict(rfmodel, train[,-1])
h = table(predicted = pred, true = train[,1])
rf.acc.train = (h[1,1]+h[2,2])/sum(h)
pred = predict(rfmodel, test[,-1])
h = table(predicted = pred, true = test[,1])
rf.acc.test = (h[1,1]+h[2,2])/sum(h)








