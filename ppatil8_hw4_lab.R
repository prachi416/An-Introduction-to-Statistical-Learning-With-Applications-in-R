#5.3
#Cross-Validation and the Bootstrap
#The valid Set Approach
library(ISLR2)
set.seed(1)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg - predict(lm.fit , Auto))[-train]^2)
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
set.seed(2)
train <- sample(392, 192)
lm.fit <- lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit,Auto))[-train]^2)
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

#Leave-One-Out Cross-Validation
glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta
cv.error <- rep(0, 10)
for(i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto) 
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

#k-Fold Cross-Validation
set.seed(17)
cv.error.10 <- rep(0, 10)
for(i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

#The Bootstrap
alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2* cov(X, Y))
}
alpha.fn(Portfolio, 1:100)
set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T))
boot(Portfolio, alpha.fn, R = 1000)
boot.fn <- function(data, index)
coef(lm(mpg ~ horsepower, data = data, subset = index))  
boot.fn(Auto, 1:392)
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))
boot(Auto, boot.fn, 1000)
summary(lm(mpg ~ horsepower, data = Auto))$coef
boot.fn <- function(data, index)
coef(
  lm(mpg ~ horsepower + I(horsepower^2),
     data = data, subset = index)
)  
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(
  lm(mpg ~ horsepower + I(horsepower^2), data = Auto)
)$coef

#7.8.3
#GAMs
library(splines)
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
library(gam)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")
plot.Gam(gam1, se = TRUE, col = "red")
gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")
summary(gam.m3)
preds <- predict(gam.m2, newdata = Wage)
gam.lo <- gam(
  wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage
)
plot.Gam(gam.lo, se = TRUE, col = "green")
gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)
library(akima)
plot(gam.lo.i)
gam.lr <- gam(
  I(wage > 250) ~ year + s(age, df = 5) + education, 
  family = binomial, data = Wage
)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")
table(Wage$education, I(Wage$wage > 250))
gam.lr.s <- gam(
  I(wage > 250) ~ year + s(age, df = 5) + education, 
  family = binomial, data = Wage, 
  subset = (education != "i. <HS Grad")
)
plot(gam.lr.s, se = T, col = "green")

