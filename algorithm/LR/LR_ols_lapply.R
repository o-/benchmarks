# LinearRegression - OLS(Ordinary Least Squares) lapply based solution
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR_ols_lapply'

setup <- function(args=c('400000', '10', '50')) {
  set.seed(42)
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 400000L }
    
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
    
    #X includes "1" column, Y column vec    
    A.func <- function(yx) {
        x <- yx
        x[1] <- 1 #modify the 1st element set to 1
        tcrossprod(x)
    }
    
    b.func <- function(yx) {
        y <- yx[1]
        x <- yx
        x[1] <-1
        x * y
    }
    
    A <- Reduce('+', lapply(YX, A.func))
    b <- Reduce('+', lapply(YX, b.func))
    
    theta <- solve(A, b)
    print(theta)
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
