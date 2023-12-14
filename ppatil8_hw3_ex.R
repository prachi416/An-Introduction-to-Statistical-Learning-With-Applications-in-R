#6.6
#Q.8
#8a

set.seed(100)
X <- rnorm(100)
e <- rnorm(100)

#8b
#values of b0, b1, b2, b3 are assumed as:
#b0 = 20
#b1 = -10
#b2 = 5
#b3 = -1.5

Y <- 20 - 10 * X +5 * X^2 + (-1.5) * X^3 + e

#8c
library(leaps)
data.full <- data.frame(y = Y, x = X)
regfit.full <- regsubsets(y~x+I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10,really.big = T)
reg.summary <- summary(regfit.full)
reg.summary
reg.summary$cp
reg.summary$bic
reg.summary$adjr2
#cp decreases substancially from one variable model to two variable model. Again from two variable model to three variable model it decreases substancially and then it keeps incresing
#BIC value is almost constant after three variable model and it is lowest for three variable model
#Adjusted R^2 increases at three variable model with 0.995 and it stays constant throughout with very insignificant change

par(mfrow = c(1, 3))

plot(reg.summary$cp,xlab="Number of Variables",ylab="cp",type="l")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="blue",cex=2,pch=20)
which.min(reg.summary$cp)

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red",cex=2,pch=20)
which.min(reg.summary$bic)

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="AdjR2",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="green",cex=2,pch=20)
which.max(reg.summary$adjr2)

#8d
#Forward
data.full <- data.frame(y = Y, x = X)
regfit.f <- regsubsets(y~x+I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10,really.big = T, method = 'forward')
regf.summary <- summary(regfit.f)
regf.summary
regf.summary$cp
regf.summary$bic
regf.summary$adjr2

#The statistical matrices are very similar to that for best subset selection
#cp decreases substancially from one variable model to two variable model. Again from two variable model to three variable model it decreases substancially and then again it keeps incresing
#BIC value is almost constant after three variable model and it is lowest for three variable model
#Adjusted R^2 increases at three variable model with 0.995 and it stays constant throughout with very insignificant change

par(mfrow = c(1, 3))

plot(reg.summary$cp,xlab="Number of Variables",ylab="cp",type="l")
points(which.min(regf.summary$cp),regf.summary$cp[which.min(regf.summary$cp)],col="blue",cex=2,pch=20)
which.min(regf.summary$cp)

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(which.min(reg.summary$bic),regf.summary$bic[which.min(regf.summary$bic)],col="red",cex=2,pch=20)
which.min(regf.summary$bic)

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="AdjR2",type="l")
points(which.max(regf.summary$adjr2),regf.summary$adjr2[which.max(regf.summary$adjr2)],col="green",cex=2,pch=20)
which.max(regf.summary$adjr2)

coef(regfit.full, id =3)
coef(regfit.full, id =4)
coef(regfit.full, id =10)

#Backward
data.full <- data.frame(y = Y, x = X)
regfit.b <- regsubsets(y~x+I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10,really.big = T, method = 'backward')
regb.summary <- summary(regfit.b)
regb.summary
regb.summary$cp
regb.summary$bic
regb.summary$adjr2

#The statistical matrices are very similar to that for best subset selection
#cp decreases substancially from one variable model to two variable model. Again from two variable model to three variable model it decreases substancially and then again it keeps incresing
#BIC value is almost constant after three variable model and it is lowest for three variable model
#Adjusted R^2 increases at three variable model with 0.995 and it stays constant throughout with very insignificant change
par(mfrow = c(1, 3))

plot(regb.summary$cp,xlab="Number of Variables",ylab="cp",type="l")
points(which.min(regb.summary$cp),regb.summary$cp[which.min(regb.summary$cp)],col="blue",cex=2,pch=20)
which.min(regb.summary$cp)

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(which.min(reg.summary$bic),regb.summary$bic[which.min(regb.summary$bic)],col="red",cex=2,pch=20)
which.min(regb.summary$bic)

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="AdjR2",type="l")
points(which.max(regb.summary$adjr2),regb.summary$adjr2[which.max(regb.summary$adjr2)],col="green",cex=2,pch=20)
which.max(regb.summary$adjr2)

coef(regfit.full, id =3)
coef(regfit.full, id =4)
coef(regfit.full, id =10)

#Results for backward selection are very similar to best subset and forward selection.
#All metrics show that the three variable model with the squared and cubed term is the best

#8e
set.seed(1)
xmat <- model.matrix(Y ~ X + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full)[,-1]
set.seed(1)
cv.lasso <- cv.glmnet(xmat,Y,alpha=1)
plot(cv.lasso)
bestlam<-cv.lasso$lambda.min
bestlam
fit.lasso<-glmnet(xmat,Y,alpha=1)
predict(fit.lasso,s=bestlam,type="coefficients")[1:11,]

#Higher values of lambda result in an increase in the MSE
#Best value of lambda is 0.0688
#The lasso model creates a sparse model with four variables. The intercept and coefficients for X, X^2 and X^3 closely match the ones chosen in initially
#b0 = 19.99~ 20
#b1 = -10 ~ -10
#b3 = 5.018 ~ 5
#b4 = -1.5264 ~ -1.5
#the value for x^4 is zero. Therefore, this model provides an accurate estimation of the response Y.

#8f
b0 <- 20
b7 <- 10
Y <- b0 + b7 * X^7 + e
data.full <- data.frame(y = Y, x = X)
regfit.full <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10)
reg.summary <- summary(regfit.full)
reg.summary$cp
reg.summary$bic
reg.summary$adjr2
par(mfrow = c(1, 3))
plot(reg.summary$cp, xlab = "Number of variables", ylab = "cp", type = "l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col = "blue", cex = 2, pch = 20)
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "AdjR2", type = "l")
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col = "green", cex = 2, pch = 20)
coef(regfit.full,2)
coef(regfit.full,1)
coef(regfit.full,4)

#Cp is lowest for one variable model with the x^7 term.
#BIC value is lowest for the one variable model and then it increases slightly
#Adjusted R2 is 0.99999 for the one variable model and it remains constant

xmat <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full)[, -1]
set.seed(1)
cv.lasso <- cv.glmnet(xmat, Y, alpha = 1)
bestlam <- cv.lasso$lambda.min
bestlam

fit.lasso <- glmnet(xmat, Y, alpha = 1)
lasso.coef = predict(fit.lasso, s = bestlam, type = "coefficients")[1:11, ]
lasso.coef

#Lasso model using best value of lambda results in a sparse model with one variable
#It assigns a non-zero coefficient to the variable X^7 that explains the response Y
#Except for coefficient of X^7, it assigns zero to all the other coefficients
