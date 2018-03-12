# LinearRegression-1var - OLS(Ordinary Least Squares) lapply based solution
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR-1var_ols_lapply'

setup <- function(args=c('400000', '50')) {
  set.seed(42)
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 400000L }
    
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
    
    #X includes "1" column, Y column vec    
    A.func <- function(yx) {
        x <- c(1, yx[2])
        tcrossprod(x)
    }
    
    b.func <- function(yx) {
        y <- yx[1]
        x <- c(1, yx[2])
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
