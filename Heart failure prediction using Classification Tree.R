rm(list=ls())
gc() 
setwd('C:/Users/fabia/Documents/MBA Cal State Fullerton/ISDS 574 - Data Mining/Group Project/Alternative')

load('./cleaned_Heart_failure_data.rda')
library(rpart)
library(rpart.plot)

## data partition ##
set.seed(1)
n.train = floor(nrow(dat)*.8)
id.train = sample(1:nrow(dat), n.train)
id.test = setdiff(1:nrow(dat), id.train)

# Classification Tree with rpart
K = 10 # number of cross-validations
fit = rpart(DEATH_EVENT ~ ., method="class", data=dat[id.train,], cp = 0.01, minsplit = 10, xval=K) # same as using all other variables as predictors

printcp(fit)
plotcp(fit)

# Minimum Error Tree
pfit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit.me, main = 'Min Error Tree')


## Prediction using 0.5 cutoff
yhat = predict(pfit.me, dat[id.test,], type = "class") 
err.me = 1 - mean(yhat == dat[id.test,'DEATH_EVENT'])
err.me

ytest = factor(dat[id.test,'DEATH_EVENT'])
table(ytest, yhat)


sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(ytest, yhat)
spe(ytest, yhat)

#classification probability
prob1 = predict(pfit.me, dat[id.test,], type = "prob")[,2]

# Prediction using 0.4 cutoff
yhat1 = as.numeric(prob1 > .4)
err.me.newCut = 1 - mean(yhat1 == ytest)
err.me.newCut

table(ytest, yhat1)
sen(ytest, yhat1)
spe(ytest, yhat1)

# Prediction using 0.3 cutoff
yhat2 = as.numeric(prob1 > .3)
err.me.newCut = 1 - mean(yhat2 == ytest)
err.me.newCut

table(ytest, yhat2)
sen(ytest, yhat2)
spe(ytest, yhat2)

# Prediction using 0.2 cutoff
yhat3 = as.numeric(prob1 > .2)
err.me.newCut = 1 - mean(yhat3 == ytest)
err.me.newCut

table(ytest, yhat3)
sen(ytest, yhat3)
spe(ytest, yhat3)

# Prediction using 0.1 cutoff
yhat4 = as.numeric(prob1 > .1)
err.me.newCut = 1 - mean(yhat3 == ytest)
err.me.newCut

table(ytest, yhat4)
sen(ytest, yhat4)
spe(ytest, yhat4)