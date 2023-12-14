#9.7 Exercise
##Q5
#5a
x1=runif(500)-0.5
x2=runif(500)-0.5
y=1*(x1^2-x2^2 > 0 )

#5b
plot(x1, x2, xlab = "X1", ylab = "X2", col = (4 - y), pch = (3 - y))

#5c
df <- data.frame(x1, x2, y = factor(y))
library(glmnet)
glm.fit <-  glm(y~ x1 + x2,family='binomial', data=data.frame(x1,x2,y))
glm.fit
df$pred <- ifelse(predict(glm.fit, type = "response") > 0.5, 1, 0)
summary(glm.fit)

#5d
table(df$pred)
glm.pred <- predict(glm.fit,data.frame(x1,x2))
plot(x1,x2,col = ifelse(glm.pred>0,'green','purple'),pch=ifelse(as.integer(glm.pred>0) == y,1,4))
sum(df$y == df$pred) / nrow(df)

#5e
glm.fit <- glm(y~ x1 + x2 + I(x1^2) + I(x2^2), family = "binomial", data=data.frame(x1,x2,y))
df$pred <- ifelse(predict(glm.fit, type = "response") > 0.5, 1, 0)
summary(glm.fit)

#5f
glm.pred=predict(glm.fit,data.frame(x1,x2))     
plot(x1,x2,col=ifelse(glm.pred>0,'red','darkgrey'),pch=ifelse(as.integer(glm.pred>0) == y,1,4))

#5g
library(e1071)
dat <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit, dat)

#5h
set.seed(1)
dat <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))
svmfit <- svm(y ~ x1 + x2, data = dat, kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat)
svmfit <- svm(y ~ x1 + x2, data = dat, kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat)

#5i
#The graphs highlight the effectiveness of support vector machines (SVMs) with non-linear kernels in delineating non-linear boundaries
#Logistic regression with non-interactions and SVMs with linear kernels fail to find a clear decision boundary
#Introducing interaction terms to logistic regression appears to empower it similarly to radial-basis kernels
#Though, selecting the appropriate interaction terms involves manual effort and tuning, and this becomes challenging with a substantial number of features
#Radial basis kernels necessitate tuning only parameters gamma and cost facilitating an easier tuning process through cross-validation

##Q8
#8a
library(ISLR)
library(e1071)
set.seed(42)
train=sample(1:1070,800)
test=(1:1070)[-train]
n_train <- 800
n_test = length(test)

#8b
svm.fit=svm(Purchase~.,data=OJ,subset=train,cost=0.01,kernel='linear', scale = TRUE)
summary(svm.fit)

#8c
svm.pred <- predict(svm.fit, newdata = OJ[train, ])
table(predicted = svm.pred, truth = OJ[train, ]$Purchase)
mean(OJ$Purchase[train] != svm.pred)
svm.pred <- predict(svm.fit, newdata = OJ[test, ])
table(predicted = svm.pred, truth = OJ[test, ]$Purchase)
mean(OJ$Purchase[test] != svm.pred)

#8d
svm.tune=tune(svm, Purchase~.,data=OJ[train,], ranges=data.frame(cost=seq(0.01,10,25)), kernel='linear')
summary(svm.tune)
svm.tune=tune(svm, Purchase~.,data=OJ[test,], ranges=data.frame(cost=seq(0.01,10,25)), kernel='linear')
summary(svm.tune)
#8e
svm.pred=predict(svm.tune$best.model,OJ[train, ])
table(predicted = svm.pred, truth = OJ[train, ]$Purchase)
train.error = 1 - sum(svm.pred == OJ[train, ]$Purchase)/n_train
train.error
svm.pred=predict(svm.tune$best.model,OJ[test, ])
table(predicted = svm.pred, truth = OJ[test, ]$Purchase)
test.error = 1 - sum(svm.pred == OJ[test, ]$Purchase)/n_test
test.error

#8f
svm.fit=svm(Purchase~.,data=OJ,subset=train,cost=0.01,kernel='radial')
summary(svm.fit)
svm.pred=predict(svm.fit,OJ[train,])
table(OJ[train,'Purchase'],svm.pred)
mean(OJ$Purchase[train] != svm.pred)
svm.pred=predict(svm.fit,OJ[test,])
table(OJ[test,'Purchase'],svm.pred)
mean(OJ$Purchase[test] != svm.pred)
svm.tune=tune(svm, Purchase~.,data=OJ[train,], ranges=data.frame(cost=seq(0.01,10,25)), kernel='radial')
summary(svm.tune)
svm.tune=tune(svm, Purchase~.,data=OJ[test,], ranges=data.frame(cost=seq(0.01,10,25)), kernel='radial')
summary(svm.tune)
svm.pred=predict(svm.tune$best.model,OJ[train,])
table(OJ[train,'Purchase'],svm.pred)
mean(OJ$Purchase[train] != svm.pred)
svm.pred=predict(svm.tune$best.model,OJ[test,])
table(OJ[test,'Purchase'],svm.pred)
mean(OJ$Purchase[test] != svm.pred)

#8g:
svm.fit=svm(Purchase~.,data=OJ,subset=train,cost=0.01,kernel='polynomial', degree = 2)
summary(svm.fit)
svm.pred=predict(svm.fit,OJ[train,])
table(OJ[train,'Purchase'],svm.pred)
mean(OJ$Purchase[train] != svm.pred)
svm.pred=predict(svm.fit,OJ[test,])
table(OJ[test,'Purchase'],svm.pred)
mean(OJ$Purchase[test] != svm.pred)
svm.tune=tune(svm, Purchase~.,data=OJ[train,], ranges=data.frame(cost=seq(0.01,10,25)), kernel='polynomial', degree = 2)
summary(svm.tune)
svm.pred=predict(svm.tune$best.model,OJ[train,])
table(OJ[train,'Purchase'],svm.pred)
mean(OJ$Purchase[train] != svm.pred)
svm.pred=predict(svm.tune$best.model,OJ[test,])
table(OJ[test,'Purchase'],svm.pred)
mean(OJ$Purchase[test] != svm.pred)


svm.fit=svm(Purchase~.,data=OJ,subset=train,cost=0.01,kernel='polynomial')
summary(svm.fit)
svm.pred=predict(svm.fit,OJ[train,])
table(OJ[train,'Purchase'],svm.pred)
mean(OJ$Purchase[train] != svm.pred)
svm.pred=predict(svm.fit,OJ[test,])
table(OJ[test,'Purchase'],svm.pred)
mean(OJ$Purchase[test] != svm.pred)
svm.tune=tune(svm, Purchase~.,data=OJ[train,], ranges=data.frame(cost=seq(0.01,10,25)), kernel='polynomial')
summary(svm.tune)
svm.pred=predict(svm.tune$best.model,OJ[train,])
table(OJ[train,'Purchase'],svm.pred)
mean(OJ$Purchase[train] != svm.pred)
svm.pred=predict(svm.tune$best.model,OJ[test,])
table(OJ[test,'Purchase'],svm.pred)
mean(OJ$Purchase[test] != svm.pred)

#8h
#Train error for Linear Kernel SVM is 0.165
#Test error for Linear Kernel SVM is 0.18518
#Error estimation of Linear Kernel SVM using 10-fold cross validation: 0.17375
#Train error for Linear Kernel SVM after tuning for optimal cost is 0.165
#Test error for Linear Kernel SVM after tuning for optimal cost is 0.20373
##
#Train error for Radial Kernel SVM is 0.3975
#Test error for Radial Kernel SVM is 0.3666667
#Error estimation of Radial Kernel SVM using 10-fold cross validation: 0.3975
#Train error for Radial Kernel SVM after tuning for optimal cost is 0.3975
#Test error for Radial Kernel SVM after tuning for optimal cost is 0.3666667
##
#Train error for Polynimal Kernel with degree 2 SVM is 0.3975
#Test error for Polynimal Kernel with degree 2 SVM is 0.3666667
#Error estimation of Polynimal Kernel with degree 2 SVM using 10-fold cross validation: 0.3975
#Train error for Polynimal Kernel with degree 2 SVM after tuning for optimal cost is 0.3975
#Test error for Polynimal Kernel with degree 2 SVM after tuning for optimal cost is 0.3666667
#Train error and Test error are same for Radial kernel and polynomial kernel with degree 2
#But when the degree for polynomial is not defined, we get different values for Train error and Test error
##
#Overall the linear SVM gives lowest error rates and hence performs the best