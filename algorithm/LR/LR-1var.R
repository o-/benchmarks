# LinearRegression-1var - R lm based solution
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR-1var'
setup <- function(args=c('700000', '50')) {
  set.seed(42)
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 700000L }
    
    niter<-as.integer(args[2])
    if(is.na(niter)){ niter <- 50L }
    
    cat('[INFO][', app.name, '] n=', n, ', niter=', niter, '\n', sep='')
    
    X<- runif(n, 0, 10) 
    Y<- X + rnorm(n) + 1
    YX <- lapply(1:n, function(i){c(Y[i],X[i])})
    list(YX=YX, niter=niter)
}


run <- function(dataset) {
    YX <- dataset$YX
    #grab YX
    vYX <- t(simplify2array(YX))
    
    res <- lm(vYX[,1] ~ vYX[,2]);
    print(res)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
