#Biomedical Data

#reading in and preparing data sets
car.data <- read.table("carriers.txt", header = TRUE)
norm.data <- read.table("normals.txt", header = TRUE)
car.data$status = 1
norm.data$status = 2
group.data = rbind(car.data,norm.data)
year = group.data$date%%1000
month = as.integer(group.data$date / 10000)
for (i in c(1:194)){
  x <- c("01/",month[i],"/",year[i])
  group.data$date[i] = as.Date(paste(x, collapse = ""), format = "%d/%m/%y")
}
group.scale = cbind(group.data[,1:2],scale(group.data[,3:6]))
group.scale$status = NA
group.scale$status[1:67] = 1
group.scale$status[68:194] = 2

#Principal component analysis
#scaled
data_pca = prcomp(group.scale[3:6])
par(mfrow=c(1,1))
biplot(data_pca)
columns
attach(mtcars)
par(mfrow=c(3,1))
plot(data_pca$x[,1],data_pca$x[,2], xlab="PC1", ylab="PC2",main="PC1/PC2 plot", pch=15, col=c("red","blue")[group.scale$status])
legend("topleft", legend = c("Carrier", "Non-Carrier"), pch = 15, col = c("red", "blue"))
plot(data_pca$x[,1],data_pca$x[,3], xlab="PC1", ylab="PC3",main="PC1/PC3 plot", pch=15, col=c("red","blue")[group.scale$status])
legend("topleft", legend = c("Carrier", "Non-Carrier"), pch = 15, col = c("red", "blue"))
plot(data_pca$x[,1],data_pca$x[,4], xlab="PC1", ylab="PC4",main="PC1/PC4 plot", pch=15, col=c("red","blue")[group.scale$status])
legend("topleft", legend = c("Carrier", "Non-Carrier"), pch = 15, col = c("red", "blue"))
data_pca

#LDA Model
library(MASS)
#Scaled
ldamodel.s = lda(status ~ m3, data = group.scale,CV=T)
pred = table(predicted = ldamodel.s$class,real = group.scale$status)
lda.acc.3 = (pred[1,1]+pred[2,2])/sum(pred)
ldamodel.s = lda(status ~ m3+m1, data = group.scale,CV=T)
pred = table(predicted = ldamodel.s$class,real = group.scale$status)
lda.acc.31 = (pred[1,1]+pred[2,2])/sum(pred)
ldamodel.s = lda(status ~ m3+m1+m4, data = group.scale,CV=T)
pred = table(predicted = ldamodel.s$class,real = group.scale$status)
lda.acc.314 = (pred[1,1]+pred[2,2])/sum(pred)
ldamodel.s = lda(status ~ m4, data = group.scale,CV=T)
pred = table(predicted = ldamodel.s$class,real = group.scale$status)
lda.acc.4 = (pred[1,1]+pred[2,2])/sum(pred)
ldamodel.s = lda(status ~ m4+m3+m2+m1, data = group.scale,CV=T)
pred = table(predicted = ldamodel.s$class,real = group.scale$status)
lda.acc.4321 = (pred[1,1]+pred[2,2])/sum(pred)
#Tree
install.packages("tree")
library(tree)
#scale
test.rows <- sample(1:nrow(group.scale),57)
test <- group.scale[test.rows,]
train <- group.scale[-test.rows,]
tree.s = tree(as.factor(status)~.,data=train)
plot(tree.s)
text(tree.s,cex=0.7,pretty=0)
tree.pred = predict(tree.s, train[,-7], type="class")
h = table(predicted = tree.pred, true = train[,7])
tree.acc.train.s <- (h[1,1] + h[2,2])/(sum(h))
tree.pred = predict(tree.s, test[,-7], type="class")
h = table(predicted = tree.pred, true = test[,7])
tree.acc.test.s <- (h[1,1] + h[2,2])/(sum(h))
#Prune
#scale
cv.scale = cv.tree(tree.s, FUN = prune.misclass)
plot(cv.scale)
prune.scale.3 = prune.misclass(tree.s, best = 3)
prune.pred = predict(prune.scale.3, group.scale[,-7], type="class")
h = table(predicted = prune.pred, true = group.scale[,7])
prune.acc.s.3 <- (h[1,1] + h[2,2])/(sum(h))
prune.scale.5 = prune.misclass(tree.s, best = 5)
prune.pred = predict(prune.scale.5, group.scale[,-7], type="class")
h = table(predicted = prune.pred, true = group.scale[,7])
prune.acc.s.5 <- (h[1,1] + h[2,2])/(sum(h))
par(mfrow=c(1,2))
plot(prune.scale.3)
text(prune.scale.3,cex=0.7,pretty=0)
plot(prune.scale.5)
text(prune.scale.5,cex=0.7,pretty=0)
prune.scale.6 = prune.misclass(tree.s, best = 6)
prune.pred = predict(prune.scale.6, group.scale[,-7], type="class")
h = table(predicted = prune.pred, true = group.scale[,7])
prune.acc.s.6 <- (h[1,1] + h[2,2])/(sum(h))
plot(prune.scale.6)
text(prune.scale.6,cex=0.7,pretty=0)
#randomforest
install.packages("randomForest")
library(randomForest)
#scaled
test.rows <- sample(1:nrow(group.scale),57)
test <- group.scale[test.rows,]
train <- group.scale[-test.rows,]
oob.err = double(10)
test.err = double(10)
for(mtry in 1:10){
  model = randomForest(as.factor(train[,7])~., data = train[,-7], mtry=mtry, ntree = 400)
  oob.err[mtry] = model$err.rate[400,1]
  pred = predict(model, test[,-7])
  h = table(predicted = pred, true = test[,7])
  test.err[mtry] = 1 - ((h[1,1]+h[2,2])/sum(h))
}
matplot(1:mtry, cbind(oob.err, test.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))

model = randomForest(as.factor(train[,7])~., data = train[,-7], mtry=6, ntree = 400)
pred = predict(model, group.scale[,-7])
h = table(predicted = pred, true = group.scale[,7])
randomForest.s = (h[1,1]+h[2,2])/sum(h)

