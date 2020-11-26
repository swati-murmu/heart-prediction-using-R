###Normalize####
dat[,1] = scale(dat[,1])
dat[,3] = scale(dat[,3])
dat[,5] = scale(dat[,5])
dat[,7:9] = scale(dat[,7:9])

set.seed(1)
n.train = floor( nrow(dat)*0.8)
ind.train = sample(1:nrow(dat), n.train)
ind.test = setdiff(1:nrow(dat), ind.train)

require(class)
Xtrain = dat[ind.train,1:11]
Xtest = dat[ind.test,1:11]
ytrain = dat[ind.train,12]
ytest = dat[ind.test,12]

get.prob = function(x) {
  prob = attr(x, 'prob')
  cl = as.numeric(x)
  ind = which(cl == 1)
  prob[ind] = 1 - prob[ind]
  return(prob)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:282, ct = .7) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct ) + 1
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj1 = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1,125, 2), .7)
obj1
## rerun with the best k
ypred = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)
table(ytest, ypred)                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
####Rerun the model with k=15
ypred = knn(Xtrain, Xtest, ytrain, k=1, prob=T)
table(ytest, ypred)
### Senestivity and specifity
sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(ytest, ypred)
spe(ytest, ypred)