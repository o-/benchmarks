# LinearRegression - R lm based solution
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR'

setup <- function(args=c('500000', '10', '50')) {
  set.seed(42)
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 500000L }
    
    nvar <-as.integer(args[2])
    if(is.na(nvar)){ nvar <- 10L }
    
    niter<-as.integer(args[3])
    if(is.na(niter)){ niter <- 50L }
    
    cat('[INFO][', app.name, '] n=', n, ', nvar=', nvar, ', niter=', niter, '\n', sep='')
    
    X<- matrix(runif(n*nvar, 0, 10), nrow=nvar, ncol=n) 
    Y<- colSums(X) + rnorm(n) + 1 # now the coefficient are all 1
    YX <- lapply(1:n, function(i){c(Y[i],X[,i])})
    list(YX=YX, nvar=nvar, niter=niter);
}

run <- function(dataset) {
    YX <- dataset$YX
    #grab YX
    vYX <- t(simplify2array(YX))
    
    res<-lm(vYX[,1] ~ vYX[,-1]);
    print(res)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
