#install.packages("readxl")
#install.packages("MASS")
#install.packages("pls")
library(readxl)
library(MASS) # Till Stepwise
library(pls) # Till PLS
setwd("~/R/Data")

# Presentation
# Tyngd på PLS
# Var forward eller backward bäst? Kolla på felet
# Behöver inte förklara koden

# Binära variablerna behöver inte behandlas olika

# g) kommentera residualerna. 
# Varför är det skillnad mellan de olika modellerna?
# Uppskatta accuracy med MSE, R2 etc

# Kanske intressant att plotta y mot olika x?

# Residuals
# Skiljer de sig mellan olika modellerna. En plot per modell. Pred.värde mot faktiska värde.
# Kan plotta res mot expl.var för den intressantaste modellen om vi vill. T.ex stepwise.

####################################
# Read data

data = read_excel(file.choose())

####################################
# Different plots of the data
##### korrelation, x mot varandra. Kanske är intressant för presentationen. Förklara hur vi undersökte datan. Men mest om modellerna (framförallt PLS)

pairs(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20, data=data, main="Simple Scatterplot Matrix")

x4 = data$x4
x5 = data$x5
x6 = data$x6
x7 = data$x7
x11 = data$x11
x13 = data$x13
x14 = data$x14
x17 = data$x17
x18 = data$x18

###### DET FINNS ETT SAMBAND MELLAN VISSA VARIABLER, framförallt x4 och x18, samt x7 och x17
layout(matrix(c(1,2,3,4),2,2))
plot(x18, x4, main="x4 vs x18", xlab="x18", ylab="x4") # Intressant!
cor(x18, x4, method = c("pearson", "kendall", "spearman")) # 0,971
plot(x17, x7, main="x7 vs x17", xlab="x17", ylab="x7") # Intressant! 
cor(x17, x7, method = c("pearson", "kendall", "spearman")) #0,987
plot(x14, x11, main="x11 vs x14", xlab="x14", ylab="x11")
cor(x14, x11, method = c("pearson", "kendall", "spearman")) #0,809

plot(x11, x4, main="x4 vs x11", xlab="x11", ylab="x4")
cor(x11,x4, method = c("pearson", "kendall", "spearman")) #0,691
plot(x11, x5, main="x5 vs x11", xlab="x11", ylab="x5")
plot(x13, x6, main="x6 vs x13", xlab="x13", ylab="x6")
plot(x13, x7, main="x7 vs x13", xlab="x13", ylab="x7")
plot(x18, x11, main="x11 vs x18", xlab="x18", ylab="x11")

####################################
# Split data set, 80% for training, 20% for testing

# set.seed() Slumpen kommer inte variera då
data=data[,-1] # ta bort första kolumnen
n = nrow(data) 

data.std = data.frame(cbind(data[,2:21],data$y)) #Standardise data
names(data.std)[21] ='y'

set.seed(1)
train_index= sample(1: n, n*0.8)  #träningsdata, talar om vilka index som väljs för de 80 procenten
data.train=data.std[train_index,]  # träningsdatan består av de rader
data.test=data.std[-train_index,] #tar bort den data som här till trÃ¤ningsdatan, test datan är alltså de återstående 20%.(matrix så ta bort raderna med träningsdatans index och behåll kolumnerna)
y.test=data.std$y[-train_index] #respons för testdatan. tar bort den data (de rader) som här till träningsdatan

#############################################
# Least squares model of training set

lm = lm(y ~ ., data = data.train)
summary(lm)

y.pred.lm = predict(lm, data.test) # Prediction for test data
summary((y.pred.lm - y.test)^2) # MSE for test data
MSE = mean((y.pred.lm - y.test)^2)
print(MSE)

#############################################
# Stepwise regression model
## Subset regression not suitable for large amounts of predictors

# Forward selection
init.model = lm(y ~ 1 , data = data.train)
fwd.model = stepAIC(init.model, scope=list(upper=~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20,lower=~1) , direction="forward")
# predict
y.pred.fwd = predict(fwd.model,data.test)
summary((y.pred.fwd - y.test)^2) #MSE

# Backward selection    ### VÄLJ DENNA
init.model = lm(y ~ . , data = data.train)
bwd.model = stepAIC(init.model, direction="backward")
# predict
y.pred.bwd <- predict(bwd.model,data.test)
summary((y.pred.bwd - y.test)^2) #MSE

#########################################
# PLS model
# M är antalet förklaringsvariabler = ncomp. Använder CV i modellen vilket räknar ut M

model.pls.cv = plsr(y ~ ., data=data.train, validation="CV")
coefficients(model.pls.cv)
summary(model.pls.cv)

# select number of components (by CV)
ncomp = which.min(model.pls.cv$validation$adj) #
print(ncomp) # number of direction for the PLS

# predict
y.pred.pls.cv = predict(model.pls.cv, data.test, ncomp=ncomp)
summary((y.pred.pls.cv - y.test)^2) #MSE

#validationplot(model.pls.cv, val.type="R2")

# From lecture, probably should not include
# one standars error rule gives ncomp = 2, see figure 
#model.pls.2 = plsr(y ~ .,data=data.train, ncomp=2)
#coefficients(model.pls.2)
#y.pred.pls.2 = predict(model.pls.2, data.test, ncomp=2)
#summary((y.pred.pls.2 - y.test)^2) #MSE
##### WHAT IS THIS?

#########################################
# Examine residuals
# y.pred mot y.test?
# Res v några var
# Res v pred.val

res.lm = residuals(lm)
res.step = residuals(bwd.model) # Annars Backward-varianten
res.pls = residuals(model.pls.1) 

#plot(res.lm, xlab = "y", ylab = "Residuals") # n på x-axeln?

#install.packages("ggplot2")
#library("ggplot2")
#head(fortify(lm))
#residPlot<-ggplot(aes(x=.fitted,y=.resid),data=lm)+geom_point()+geom_hline(yintercept=0)+labs(x="Fitted value",y="Residual")
#residPlot

# Least Squares
fitted.lm = fitted.values(lm)
#x1.lm = data.train$x1
#x2.lm = data.train$x2
#x5.lm = data.train$x5
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fitted.lm,res.lm, main="residuals vs fitted values",
     xlab="fitted value", ylab="residuals")
#plot(x1.lm,res.lm, main="residuals vs x_1",xlab="x_1", ylab="residuals")
#plot(x2.lm,res.lm, main="residuals vs x_2",xlab="x_2", ylab="residuals")
#plot(x5.lm,res.lm, main="residuals vs x_5",xlab="x_5", ylab="residuals")
#hist(res.lm)


plot(y.pred.lm,y.test, main="Predicted vs actual values for Linear model",
     xlab="actual", ylab="predicted")

# Stepwise

# PLS

