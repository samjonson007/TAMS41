library("readxl")
install.packages("MASS")
library("MASS")
#######export data
data = read_excel(file.choose())
#####plot and information about data

####still incompletelibrary("readxl")
x <- subset(data, select = -c(id, y))
summary(data)
y = data$y
summary(data$y)
hist(data$x10)
pairs(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=data,
      main="Simple Scatterplot Matrix")

####train/set 

## Standardise data and prepare train/test split

data.scale = data.frame(cbind(scale(x[,1:20]),data$y)) # standardise the data
names(data.scale)[21] = 'y'
index_train <- sample(c(TRUE, FALSE), nrow(data.scale), rep=TRUE, prob = c(0.8, 0.2))
index_test <- (!index_train)
data.train = data.scale[index_train,] # choose the trainging data
data.test = data.scale[index_test,] # choose the test data

y.test = data.test$y # response for test data 
n.train = nrow(data.train)


#####linear model + standart error

lm.model = lm(y ~ ., data = data.train)
# lm.model = lm(Apps ~ ., data = data_college, subset = train_index)
summary(lm.model)
residual = residuals(lm.model)
y.pred.lm = predict(lm.model, data.test)
plot(lm.model)
summary((y.pred.lm - y.test)^2) # MSE for test data
MSE = mean((y.pred.lm - y.test)^2)
MSE
summary(y.pred.lm)
plot(y.pred.lm, y.test)

layout(matrix(c(1),1,1))
plot(lm.model)
plot(model.pls)


####residual plotting 
layout(matrix(c(1),1,1))
plot(data.train$x20, residual, main="residual vs fitted values for least square methods", xlab = "x20", ylab="residuals")
abline(0,0)

rstrandart(lm.model)
#####stepwise 

library(readxl)
library(MASS) # Till Stepwise
library(pls) 
full_model = lm(y ~ ., data= data.train)
simple_lm = lm(y ~ 1, data= data.train) # no predictors

library("caret")
# direction = "both", "backward" or "forward"
step_fw = stepAIC(simple_lm, scope=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20, direction = "forward", trace = TRUE) # start with the model without any predictors 
summary(step_fw)
y.pred.step_fw = predict(step_fw, data.test)
MSE = mean((y.pred.step_fw - y.test)^2)
MSE

step_bw = stepAIC(full_model, direction = "backward", trace = TRUE) # start with the model with all predictors
summary(step_bw)
y.pred.step_bw = predict(step_bw, data.test)
MSE = mean((y.pred.step_bw - y.test)^2)
MSE

plot(step_bw)



###residual for the stepback
residual_pls = residuals(step_bw)
plot(data$x20, residual_pls, main="residual vs fitted values for least square methods", xlab = "x20", ylab="residuals")
layout(matrix(c(1),1,1))
qqnorm(residual, ylab="Standardized Residuals", xlab="x_20", ylab="residual")
qqline(residual)



## PLS
install.packages("pls")
library(pls)

## PLS
model.pls = plsr(y ~ ., data = data.train, validation="CV")
summary(model.pls)
coefficients(model.pls)
fitted_values = fitted.values(model.pls)
plot(model.pls$fitted.values, model.pls$residuals)
plot(model.pls)

par(mfrow = c(2, 2))
plotXResiduals(model.pls, show.label = TRUE)
plotYResiduals(model.pls, show.label = TRUE)
plotPredictions(model.pls)
plotPredictions(model.pls, ncomp = 18, xlab = 'C, reference', ylab = 'C, predictions')
par(mfrow = c(1, 1))

# select number of components (by CV)
ncomp = which.min(model.pls$validation$adj)
print(ncomp) # number of direction for the PLS

validationplot(model.pls, val.type = "R2")


# predict
y.pred.pls <- predict(model.pls, data.test, ncomp = ncomp)
plot(y.pred.pls, )
summary((y.pred.pls - y.test)^2)
MSE.pls = mean((y.pred.pls - y.test)^2)
MSE.pls

#####residual

rresidual <- rstandard(model.pls)
plot(data, rresidual, ylab="Standardized Residuals", xlab="Waiting Time", main="Old Faithful Eruptions") 
plot(model.pls)
help("rstandard")
summary(model.pls$residuals)
