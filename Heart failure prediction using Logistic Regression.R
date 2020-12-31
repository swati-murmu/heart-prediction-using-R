rm(list=ls())
gc() 
setwd('C:\\Users\\Azmery\\Documents\\CSUF\\ISDS 574\\Group Project\\NEW')

dat = read.csv('heart_failure_clinical_records_dataset.csv', head=T, stringsAsFactors=F, na.strings='')

#remove time variable
dat$time = NULL

#removing outlier
which(dat$creatinine_phosphokinase > 3000)
dat = dat[dat$creatinine_phosphokinase <= 3000,]

which(dat$ejection_fraction > 65)
dat = dat[dat$ejection_fraction <= 65,]

which(dat$platelets > 590000)
dat = dat[dat$platelets <= 590000,]

which(dat$serum_creatinine > 8)
dat = dat[dat$serum_creatinine <= 8,]

which(dat$serum_sodium < 125)
dat = dat[dat$serum_sodium >= 125,]

which(dat$age == 60.667)
id = which(dat$age %in% c(60.667,61))
dat$age[id] = 61

####Logistic Regression Model####

set.seed(1) 
id.train = sample(1:nrow(dat), nrow(dat)*.9) 
id.test = setdiff(1:nrow(dat), id.train) 
dat.train = dat[id.train,]
dat.test = dat[id.test,]

min.model = glm(DEATH_EVENT ~ 1, data = dat.train, family = 'binomial')
max.model = glm(DEATH_EVENT ~ ., data = dat.train, family = 'binomial')
max.formula = formula(max.model)

#forward selection

#### 90% training data, cutoff - 0.2

obj2 = step(min.model, direction='forward', scope=max.formula) 

get.or = function(sobj, alpha=.05) {
  b = sobj$coef[-1, 'Estimate']
  se.b = sobj$coef[-1, 'Std. Error']
  pval = sobj$coef[-1, 'Pr(>|z|)']
  or = exp(b); se.or = exp(b)*se.b
  lb = b - qnorm(alpha/2)*se.b; lb.or = exp(lb)
  ub = b + qnorm(1-alpha/2)*se.b; ub.or = exp(ub)
  out = cbind(or, se.or, lb, ub, pval)
  colnames(out) = c('OR', 'SE', paste0((1-alpha)*100, '% CI, lower'),
                    paste0((1-alpha)*100, '% CI, upper'), 'p value')
  return(out)
}
get.or(summary(obj2))

yhat2 = predict(obj2, newdata = dat.test, type='response')
hist(yhat2)

dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat2.class = dichotomize(yhat2, .2) 
err2 = mean(yhat2.class != dat.test$DEATH_EVENT) # misclassification error rate
err2

table(yhat2.class, dat.test$DEATH_EVENT)


sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(dat.test$DEATH_EVENT, yhat2.class) #percentage of customers that will identify
spe(dat.test$DEATH_EVENT, yhat2.class)

####90% Training data, cutoff = 0.1

obj = step(min.model, direction='forward', scope=max.formula) 
summary(obj) 

yhat = predict(obj, newdata = dat.test, type='response')
hist(yhat)

yhat.class = dichotomize(yhat, .1) 
err = mean(yhat.class != dat.test$DEATH_EVENT) # misclassification error rate
err

table(yhat.class, dat.test$DEATH_EVENT)

sen(dat.test$DEATH_EVENT, yhat.class) #percentage of customers that will identify
spe(dat.test$DEATH_EVENT, yhat.class)

####With 90% training data, cutoff - 0.5

obj5 = step(min.model, direction='forward', scope=max.formula) 
summary(obj5) 

yhat5 = predict(obj5, newdata = dat.test, type='response')
hist(yhat5)

yhat5.class = dichotomize(yhat5, .5) 
err5 = mean(yhat5.class != dat.test$DEATH_EVENT) # misclassification error rate
err5

table(yhat5.class, dat.test$DEATH_EVENT)

sen(dat.test$DEATH_EVENT, yhat5.class) #percentage of customers that will identify
spe(dat.test$DEATH_EVENT, yhat5.class)

####With 90% training data, cutoff - 0.4

obj4 = step(min.model, direction='forward', scope=max.formula) 
summary(obj4) 

yhat4 = predict(obj4, newdata = dat.test, type='response')
hist(yhat4)

yhat4.class = dichotomize(yhat4, .4) 
err4 = mean(yhat4.class != dat.test$DEATH_EVENT) # misclassification error rate
err4

table(yhat4.class, dat.test$DEATH_EVENT)

sen(dat.test$DEATH_EVENT, yhat4.class) #percentage of customers that will identify
spe(dat.test$DEATH_EVENT, yhat4.class)

#### 90% training data, cutoff - 0.3

obj3 = step(min.model, direction='forward', scope=max.formula) 
summary(obj3) 

yhat3 = predict(obj3, newdata = dat.test, type='response')
hist(yhat3)

yhat3.class = dichotomize(yhat3, .3) 
err3 = mean(yhat3.class != dat.test$DEATH_EVENT) # misclassification error rate
err3

table(yhat3.class, dat.test$DEATH_EVENT)

sen(dat.test$DEATH_EVENT, yhat3.class) #percentage of customers that will identify
spe(dat.test$DEATH_EVENT, yhat3.class)

# backward selection
## Cutoff = 0.1
obj6 = step(max.model, direction='backward', scope=max.formula)
summary(obj6)

get.or(summary(obj6))

yhat6 = predict(obj6, newdata = dat.test, type='response')
hist(yhat6)
yhat6.class = dichotomize(yhat6, .1)
err6 = mean(yhat6.class != dat.test$DEATH_EVENT) # misclassification error rate
err6

sen(dat.test$DEATH_EVENT, yhat6.class)
spe(dat.test$DEATH_EVENT, yhat6.class)

## Cutoff = 0.2
obj7 = step(max.model, direction='backward', scope=max.formula)
summary(obj7)

get.or(summary(obj7))

yhat7 = predict(obj7, newdata = dat.test, type='response')
hist(yhat7)
yhat7.class = dichotomize(yhat7, .2)
err7 = mean(yhat7.class != dat.test$DEATH_EVENT) # misclassification error rate
err7

sen(dat.test$DEATH_EVENT, yhat7.class)
spe(dat.test$DEATH_EVENT, yhat7.class)

## Cutoff = 0.3
obj8 = step(max.model, direction='backward', scope=max.formula)
summary(obj8)

get.or(summary(obj8))

yhat8 = predict(obj8, newdata = dat.test, type='response')
hist(yhat8)
yhat8.class = dichotomize(yhat8, .3)
err8 = mean(yhat8.class != dat.test$DEATH_EVENT) # misclassification error rate
err8

sen(dat.test$DEATH_EVENT, yhat8.class)
spe(dat.test$DEATH_EVENT, yhat8.class)

## Cutoff = 0.4
obj9 = step(max.model, direction='backward', scope=max.formula)
summary(obj9)

get.or(summary(obj9))

yhat9 = predict(obj9, newdata = dat.test, type='response')
hist(yhat9)
yhat9.class = dichotomize(yhat9, .4)
err9 = mean(yhat9.class != dat.test$DEATH_EVENT) # misclassification error rate
err9

sen(dat.test$DEATH_EVENT, yhat9.class)
spe(dat.test$DEATH_EVENT, yhat9.class)

## Cutoff = 0.5
obj10 = step(max.model, direction='backward', scope=max.formula)
summary(obj10)

get.or(summary(obj10))

yhat10 = predict(obj10, newdata = dat.test, type='response')
hist(yhat10)
yhat10.class = dichotomize(yhat10, .5)
err10 = mean(yhat10.class != dat.test$DEATH_EVENT) # misclassification error rate
err10

sen(dat.test$DEATH_EVENT, yhat10.class)
spe(dat.test$DEATH_EVENT, yhat10.class)

# stepwise selection
# Cutoff = 0.1
obj11 = step(min.model, direction='both', scope=max.formula) 
summary(obj11)

get.or(summary(obj11))

yhat11 = predict(obj11, newdata = dat.test, type='response')
hist(yhat11)
yhat11.class = dichotomize(yhat11, .1)
err11 = mean(yhat11.class != dat.test$DEATH_EVENT) # misclassification error rate
err11

sen(dat.test$DEATH_EVENT, yhat11.class)
spe(dat.test$DEATH_EVENT, yhat11.class)

# Cutoff = 0.2
obj12 = step(min.model, direction='both', scope=max.formula) 
summary(obj12)

get.or(summary(obj12))

yhat12 = predict(obj12, newdata = dat.test, type='response')
hist(yhat12)
yhat12.class = dichotomize(yhat12, .2)
err12 = mean(yhat12.class != dat.test$DEATH_EVENT) # misclassification error rate
err12

sen(dat.test$DEATH_EVENT, yhat12.class)
spe(dat.test$DEATH_EVENT, yhat12.class)

# Cutoff = 0.3
obj13 = step(min.model, direction='both', scope=max.formula) 
summary(obj13)

get.or(summary(obj13))

yhat13 = predict(obj13, newdata = dat.test, type='response')
hist(yhat13)
yhat13.class = dichotomize(yhat13, .3)
err13 = mean(yhat13.class != dat.test$DEATH_EVENT) # misclassification error rate
err13

sen(dat.test$DEATH_EVENT, yhat13.class)
spe(dat.test$DEATH_EVENT, yhat13.class)

# Cutoff = 0.4
obj14 = step(min.model, direction='both', scope=max.formula) 
summary(obj14)

get.or(summary(obj14))

yhat14 = predict(obj14, newdata = dat.test, type='response')
hist(yhat14)
yhat14.class = dichotomize(yhat14, .4)
err14 = mean(yhat14.class != dat.test$DEATH_EVENT) # misclassification error rate
err14

sen(dat.test$DEATH_EVENT, yhat14.class)
spe(dat.test$DEATH_EVENT, yhat14.class)

# Cutoff = 0.5
obj15 = step(min.model, direction='both', scope=max.formula) 
summary(obj15)

get.or(summary(obj15))

yhat15 = predict(obj15, newdata = dat.test, type='response')
hist(yhat15)
yhat15.class = dichotomize(yhat15, .5)
err15 = mean(yhat15.class != dat.test$DEATH_EVENT) # misclassification error rate
err15

sen(dat.test$DEATH_EVENT, yhat15.class)
spe(dat.test$DEATH_EVENT, yhat15.class)
