# LinearRegression-1var - LMS(least mean square) vector programming based solution
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR-1var_lms_vec'

setup <- function(args=c('800000', '50')) {
  set.seed(42)
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 800000L }
    
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
    niter <- dataset$niter
    vYX <- t(simplify2array(YX))
    X <- cbind(1, matrix(vYX[,2]))
    Y <- vYX[,1]
    
    #X includes "1" column, Y column vec
    grad.func <- function(X, y) {
        error <- (X %*% theta - y)
        #This is a simple normalization
        delta <- t(X) %*% error / length(y)
        return(delta)
    }
    
    cost <- function(X, y, theta) {
        # computes the cost of using theta as the parameter for linear regression
        # to fit the data points in X and y
        sum((X %*% theta - y)^2)/(2 * length(y))
    }
    

    theta <- double(ncol(X)) #initial guess
    alpha <- 0.05 # small step

    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        delta <- grad.func(X, Y)
        #cat('delta =', delta, '\n')
        theta <- theta - alpha * delta
        ctm <- proc.time()
        cat("[INFO]Iter", iter, "Time =", (ctm - ptm)[[3]], '\n')
        ptm <- ctm
        cat('theta =', theta, '\n')
        #print(cost(X,y, theta))
    }
    cat('Final theta =', theta, '\n')
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
