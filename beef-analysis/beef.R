#PCA
beef_data <- read.csv("dry_aged_beef.csv",header = FALSE)
beef_pca <- prcomp(beef_data[,2:1235])
biplot(beef_pca)#3 outliers are 43, 44 and 45
beef_data <- beef_data[-c(43,44,45),]
beef_pca <- prcomp(beef_data[,2:1235])
biplot(beef_pca)

#Constructing data sets
set.seed(1234)
test.rows <- sample(1:nrow(beef_data),70)
test.data <- beef_data[test.rows,]
train.data <- beef_data[-test.rows,]

#Full Decision Tree Classifier
install.packages("tree")
library(tree)
beef.tree = tree(V1~., data = beef_data)
plot(beef.tree)
text(beef.tree, cex = 1.0, pretty = NULL)
tree.pred = predict(beef.tree,train.data[,-1],type="class")
table(predicted=tree.pred,true=train.data[,1])
train.accuracy_tree <- (80+84)/dim(train.data)[1]
tree.pred = predict(beef.tree,test.data[,-1],type="class")
table(predicted=tree.pred,true=test.data[,1])
test.accuracy_tree = (35+35)/dim(test.data)[1]
test.err_tree = 1 - test.accuracy

#Pruned Decision Tree Classifier
cv_beef = cv.tree(beef.tree, FUN = prune.misclass)
plot(cv_beef)
prune.beef = prune.misclass(beef.tree,best = 4)
plot(prune.beef)
text(beef.tree, cex = 0.7, pretty = NULL)
tree.pred = predict(prune.beef,train.data[,-1],type="class")
table(predicted=tree.pred,true=train.data[,1])
train.accuracy_prune <- (76+84)/dim(train.data)[1]
tree.pred = predict(beef.tree,test.data[,-1],type="class")
table(predicted=tree.pred,true=test.data[,1])
test.accuracy_prune = (35+35)/dim(test.data)[1]
test.err_prune = 1 - test.accuracy

#Random Forest Classifier
install.packages("randomForest")
library(randomForest)
rf = randomForest(V1~.,data = train.data,ntree = 500,mtry=35,importance=TRUE)
rf
pred <- predict(rf, train.data[,-1],type="class")
table(predicted = pred,true = train.data[,1])
train.accuracy_rf = (80+85)/(80+85)
train.err_rf = 1-train.accuracy_rf
pred <- predict(rf, test.data[,-1],type="class")
table(predicted = pred,true = test.data[,1])
test.accuracy_rf = (27+34)/(27+34+8+1)
test.err_rf = 1-test.accuracy_rf

#Boosting Model
install.packages("adabag")
library(adabag)
adaboost <- boosting(V1~., data = train.data,control = rpart.control(maxdepth=1))
predboosting <- predict.boosting(adaboost, newdata = test.data[,-1])
table(predicted = predboosting$class,true=test.data[,1])
test.accuracy_boost = (27+34)/(27+34+8+1)
test.err_boost = 1-test.accuracy_boost




