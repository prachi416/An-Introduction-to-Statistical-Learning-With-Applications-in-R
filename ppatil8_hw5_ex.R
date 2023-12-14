##Exercise 8.4
#Q.8.4.7
library(ISLR2)
library(randomForest)
library(tree)
attach(Wage)
attach(Carseats)
library(caTools)

set.seed(1)
Boston = Boston
sample.data = sample.split(Boston$medv, SplitRatio = 0.70)
train.set = subset(Boston, select =-c(medv), sample.data == TRUE)
test.set = subset(Boston, select =-c(medv), sample.data == FALSE)
train.Y = subset(Boston$medv, sample.data == TRUE)
test.Y = subset(Boston$medv, sample.data == FALSE)

p = 10
Tree1 = randomForest(train.set, train.Y, test.set, test.Y, mtry = p, ntree = 800)
Tree2 = randomForest(train.set, train.Y, test.set, test.Y, mtry = p/3, ntree = 800)
Tree3 = randomForest(train.set, train.Y, test.set, test.Y, mtry = p/5, ntree = 800)
Tree4 = randomForest(train.set, train.Y, test.set, test.Y, mtry = p/7, ntree = 800)
Tree5 = randomForest(train.set, train.Y, test.set, test.Y, mtry = p/9, ntree = 800)
Tree6 = randomForest(train.set, train.Y, test.set, test.Y, mtry = p/10, ntree = 800)

x.axis = seq(1, 800, 1)
plot(x.axis, Tree1$test$mse, xlab = "Number of Trees", ylab = "Test Error", ylim = c(5, 20),type ="b",lwd = 2)
lines(x.axis, Tree2$test$mse, col = "red", lwd = 2)
lines(x.axis, Tree3$test$mse, col = "green", lwd = 2)
lines(x.axis, Tree4$test$mse, col = "darkgrey", lwd = 2)
lines(x.axis, Tree5$test$mse, col = "blue", lwd = 2)
lines(x.axis, Tree6$test$mse, col = "violet", lwd = 2)
legend(600, 19, legend = c("m = p", "m = p/3", "m = p/5", "m = p/7", "m = p/9", "m = p/11"),
       col = c("black", "red", "green", "darkgrey", "blue", "violet"), lty = c(1,1,1, lwd = c(2,2,2)))


#The test error rate decreases initially from m = p to m = p/5 and then it increases from m = p/7 to p/10
#The test error rate decreases rapidly with the icrease in number of trees
#While the change in test error rate follows a similar pattern from m = p to m = p/3, remaining values of m follws a similar pattern where a slight spike is seen around number of trees = 50=150 and it becomes constant after that

#Q.8.4.8
##a
set.seed(5)
train_index <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
train <- Carseats[train_index, ]
test <- Carseats[-train_index, ]

##b
tree_model <- tree(Sales ~ ., train)
plot(tree)
summary(tree)
text(tree, pretty = 0, cex = 0.7)
test_pred <- predict(tree_model, test)
mean((test_pred - test$Sales)^2)
baseline<- mean(train$Sales)
mean((baseline- test$Sales)^2)
#Test MSE is 2.806366

##c
set.seed(2)
cv_tree_model <- cv.tree(tree_model, K = 10)
df <- data.frame(n_leaves = cv_tree_model$size,
                 CV_RSS = cv_tree_model$dev)
df$min_CV_RSS <- as.numeric(df$CV_RSS == min(df$CV_RSS))
plot(df$n_leaves, df$CV_RSS, type = "l", col = "grey55", xlab = "Terminal Nodes", ylab = "CV RSS")
points(df$n_leaves[df$min_CV_RSS == 1], df$CV_RSS[df$min_CV_RSS == 1], col = "green", pch = 16)
points(df$n_leaves[df$min_CV_RSS == 0], df$CV_RSS[df$min_CV_RSS == 0], col = "blue", pch = 16)
pruned_tree_model <- prune.tree(tree_model, best = 11)
test_pred <- predict(pruned_tree_model, test)
mean((test_pred - test$Sales)^2)

data.frame(size = cv_tree_model$size, 
           dev = cv_tree_model$dev, 
           k = cv_tree_model$k)

#CV Error is lowest for tree with 11 nodes. 
#The full tree is pruned to obtain 11 node tree
#Test MSE is 2.806366
#The test MSE remains same for pruned tree as well

##d
set.seed(7)
bagged = randomForest(y = train$Sales,x = train[ ,-1],mtry=ncol(train)-1,importance=TRUE)
test_pred <- predict(bagged, test)
importance(bagged)
mean((test_pred-test$Sales)^2)

#Test MSE is 2.073448
#Bagging has slightly improved Test MSE
#'Price' and 'ShelveLoc' are the most important variables

##e
set.seed(2)
rf1.carseats = randomForest(Sales~.,data=train,mtry=7/5,importance=T)
rf2.carseats = randomForest(Sales~.,data=train,mtry=7/3,importance=T)
rf3.carseats = randomForest(Sales~.,data=train,mtry=sqrt(7),importance=T)
rf4.carseats = randomForest(Sales~.,data=train,mtry=7/2,importance=T)
importance(rf1.carseats)
importance(rf2.carseats)
importance(rf3.carseats)
importance(rf4.carseats)
varImpPlot(rf1.carseats)
varImpPlot(rf2.carseats)
varImpPlot(rf3.carseats)
varImpPlot(rf4.carseats)
mse1 = mean((predict(rf1.carseats,newdata = test)-test$Sales)^2)
mse2 = mean((predict(rf2.carseats,newdata = test)-test$Sales)^2)
mse3 = mean((predict(rf3.carseats,newdata = test)-test$Sales)^2)
mse4 = mean((predict(rf4.carseats,newdata = test)-test$Sales)^2)
mse1
mse2
mse3
mse4

#For all four models, "Price" and "ShelveLoc" are the most important variables
#The MSE for some models in random forest is lower than the MSE in Bagging

##f
library(BART)
train <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
x <- Carseats[, 1:11]
y <- Carseats[, "Sales"]
xtrain <- x[train, ]
ytrain <- y[train]
xtest <- x[-train, ]
ytest <- y[-train]
bart_model <- gbart(xtrain, ytrain, x.test = xtest)
yhat.bart <- bart_model$yhat.test.mean
mse_bart <- mean((ytest - bart_model$yhat.test.mean)^2)
mse_bart 
ord <- order(bart_model$varcount.mean, decreasing = TRUE)
bart_model$varcount.mean[ord]

#MSE obtained is 0.07092029
#Higher value of BART for variable signifies importance of the predictor varaible
#"ShelveLoc" is highly influential

#Q.8.10

##a
Hitters <- Hitters[!is.na(Hitters$Salary), ]
Hitters$Salary <- log(Hitters$Salary)

##b
set.seed(10)
sample.data = sample.split(Hitters$Salary, SplitRatio = 200/263) 
train.set = subset(Hitters, sample.data==TRUE)
test.set = subset(Hitters, sample.data==FALSE)

##c
lambdas = seq(0.0001,0.5,0.01)
train.mse = rep(NA,length(lambdas))
test.mse = rep(NA,length(lambdas))

set.seed(4)
for (i in lambdas){
  boost.Hitters = gbm(Salary~., data=train.set,distribution = "gaussian", n.trees = 1000, 
                      interaction.depth = 4, shrinkage = i)
  yhat.train = predict(boost.Hitters,newdata = train.set, n.trees = 1000)
  train.mse[which(i==lambdas)] = mean((yhat.train-train.set$Salary)^2)
  
  yhat.test = predict(boost.Hitters,newdata = test.set, n.trees = 1000)
  test.mse[which(i==lambdas)] = mean((yhat.test-test.set$Salary)^2)
}
par(mfrow=c(1,2))
plot(lambdas,train.mse,type="b",xlab=expression(lambda), ylab="Train MSE")

#Train MSE decreases rapidly from lambda = 0 to 0.1. After that train MSE approaches zero from larger values of lambda

##d

plot(lambdas,test.mse,type="b",xlab=expression(lambda), ylab="Test MSE")
lambdas[which.min(test.mse)];min(test.mse)
lambdas[which.min(train.mse)];min(train.mse)
#Test MSE increases from lambda = 0 to 0.3 
#After lambda = 0.3, test MSE follows a zig-zag pattern

##e
#Multiple Linear Regression
lm.fit = lm(Salary~., data=train.set)
lm.preds = predict(lm.fit, newdata = test.set)
lm.mse = mean((test.set$Salary-lm.preds)^2)
lm.mse
#Lasso Model
library(glmnet)
train = model.matrix(Salary~.,train.set)
test = model.matrix(Salary~.,test.set)
y.train = train.set$Salary
lasso.mod = glmnet(train, y.train, alpha = 1)
set.seed(4)
cv.out=cv.glmnet(train, y.train, alpha=1)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx = test)
mean((test.set$Salary-lasso.pred)^2)

#Test MSE for Multiple Linear Regression is 0.3294574
#Test MSE for Lasso Model is 0.3183498
#Tesst MSE for Multiple Linear Regression and Lasso Model is much higher as compared to test MSE of boosting

##f
boost.best = gbm(Salary~., data=train.set, distribution = "gaussian", n.trees = 1000, 
                 interaction.depth = 4, shrinkage = 0.01)
summary(boost.best)

#"CHits" is the most influential variable followed by "CATBat", "CRBI" and "CWalks"

##g
bag.Hitters = randomForest(Salary~.,train.set,mtry=16,importance=TRUE)
bag.pred = predict(bag.Hitters,newdata = test.set)
mean((test.set$Salary-bag.pred)^2)

#Test MSE using bagging is 0.1207996
