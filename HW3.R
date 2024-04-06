library(glmnet)
library(caret)
library(splines)
library(gam)

# Question 2
#read file
data = read.csv('/Users/gege/Desktop/Spring 2020/Machine Learning/R Code/fat.csv')
#split data into train and test
train = data[1:200,]
test = data[201:252,]

#2a
# linear model
linear.m_2a = lm(brozek ~ ., data=train)
summary(lm_2a)
linear.m_testpred = predict(lm_2a,test)
linear.m_mse = mean((test$brozek - linear.m_testpred)^2)
linear.m_mse

#2b
# ridge regression
x.train = model.matrix(brozek~., train)[,-1]
y.train = train$brozek
x.test = model.matrix(brozek~., test)[,-1]
lambda = seq(10^-2, 10^10, length = 1000)
ridge.r2b = glmnet(x.train, y.train, alpha = 0, lambda = lambda)
cv.out = cv.glmnet(x.train, y.train, alpha = 0)
bestlambda = cv.out$lambda.min
bestlambda
ridge.testpred = predict(ridge.r2b, s = bestlambda, newx = x.test)
ridge_mse = mean((test$brozek - ridge.testpred)^2)
ridge_mse

#2c
# LASSO
lambda = seq(10^-2, 10^10, length = 1000)
lasso.2c = glmnet(x.train, y.train, alpha = 1, lambda = lambda)
cv.out = cv.glmnet(x.train, y.train, alpha = 1)
bestlambda = cv.out$lambda.min
bestlambda
lasso.testpred = predict(lasso.2c, s = bestlambda, newx = x.test)
lasso.mse = mean((test$brozek - lasso_testpred)^2)
lasso.mse

#2d
# PCR
pcr_2d <- train(brozek~., data = train, method = "pcr", scale = TRUE,
                trControl = trainControl(method="cv"), tuneLength = 17)
# M, number of principal components, selected by cross-validation
M = pcr_2d$bestTune
M
pcr.testpred = predict(pcr_2d, newdata = test, ncomp = M)
pcr.mse = mean((test$brozek - pcr.testpred)^2)
pcr.mse

#2e

cat(paste('MSE: \nlinear model ',linear.m_mse, '\nridge ', ridge_mse, '\nLASSO ', lasso_mse, '\nPCR ', pcr_mse, sep = ''))
# MSE: 
# linear model 0.0109255300016614
# ridge 0.0128028789356873
# LASSO 0.00694531632677785 <-- you da best
# PCR 0.099202014053121

# Question 3
#read file
data = read.csv('/Users/gege/Desktop/Spring 2020/Machine Learning/Auto.csv')

#3a
plot(data$horsepower, data$mpg, xlab="horsepower", ylab="mpg")
poly_3a = lm(mpg ~ poly(horsepower,5), data = data)
confint(poly_3a, level=0.95)
#plot confidence intervals
x = seq(min(data$horsepower), max(data$horsepower), by = 0.5)
predicted.intervals <- predict(poly_3a, data.frame(horsepower=x),
                               interval='confidence', level=0.95)

plot(x, predicted.intervals[,'fit'], type='l', 
     xlab = 'horsepower', ylab = 'mpg', main = 'Confidence interval (polynomial, order 5)')
polygon(c(x,rev(x)),
        c(predicted.intervals[,'lwr'],rev(predicted.intervals[,'upr'])),
        col = "grey75", border = FALSE)
lines(x, predicted.intervals[,'fit'], col ='black', lwd = 2)
lines(x, predicted.intervals[,'upr'], col="red", lty=2)
lines(x, predicted.intervals[,'lwr'], col="red", lty=2)

#3b
spline_2b = lm (mpg ~ ns(horsepower, df = 5), data = data)
confint(spline_2b, level=0.95)
x = seq(min(data$horsepower), max(data$horsepower), by = 0.5)
predicted.intervals <- predict(spline_2b, data.frame(horsepower=x),
                               interval='confidence', level=0.95)

plot(x, predicted.intervals[,'fit'], type='l', 
     xlab = 'horsepower', ylab = 'mpg', main = 'Confidence interval (natural spline, df=5)')
polygon(c(x,rev(x)),
        c(predicted.intervals[,'lwr'],rev(predicted.intervals[,'upr'])),
        col = "grey75", border = FALSE)
lines(x, predicted.intervals[,'fit'], col ='black', lwd = 2)
lines(x, predicted.intervals[,'upr'], col="red", lty=2)
lines(x, predicted.intervals[,'lwr'], col="red", lty=2)

#3c
train = data[1:350,]
test = data[351:392,]
lm.3c = lm(mpg ~ horsepower + acceleration + year, data=train)
lm.testpred = predict(lm.3c, newdata = test)
lm.mse = mean((lm.testpred - test$mpg)^2)
lm.mse

gam.3c = gam(mpg ~ ns(horsepower, df=5) + ns(acceleration, df=5) + year, data=train)
summary(gam.3c)
gam.testpred = predict(gam_3c, newdata = test)
gam.mse = mean((gam.testpred - test$mpg)^2)
gam.mse
