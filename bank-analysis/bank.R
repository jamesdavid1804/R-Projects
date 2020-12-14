bankdata = read.table("bank.txt",header = TRUE)
bankpca = prcomp(bankdata[,2:7])
biplot(bankpca)
bankdata[131,6] = 10.5
bankpca = prcomp(bankdata[,2:7])
biplot(bankpca)

library(MASS)
test.rows <- sample(1:nrow(bankdata),60)
testdata <- bankdata[test.rows,]
traindata <- bankdata[-test.rows,]
group = traindata[,1]
ldamodel = lda(traindata[,2:7],group)
predtest = predict(ldamodel,testdata[,2:7])
table(testdata$type,predtest$class)