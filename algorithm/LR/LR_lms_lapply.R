# LinearRegression - LMS(least mean square) lapply based solution
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'LR_lms_lapply'

setup <- function(args=c('20000', '10', '50')) {
  set.seed(42)
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 20000L }
    
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
    niter <- dataset$niter
    
    #X includes "1" column, Y column vec
    grad.func <- function(yx) {
        y <- yx[1]
        x <- yx
        x[1] <- 1 #modify the 1st element
        error <- (sum(x *theta) - y)
        delta <- error * x
        return(delta)
    }
    
    cost <- function(X, y, theta) {
        # computes the cost of using theta as the parameter for linear regression
        # to fit the data points in X and y
        sum((X %*% theta - y)^2)/(2 * length(y))
    }
    

    theta <- double(length(YX[[1]])) #initial guess as 0
    alpha <- 0.05/ length(YX) / length(theta) # small step

    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        delta <- lapply(YX, grad.func)
        #cat('delta =', delta, '\n')
        theta <- theta - alpha * Reduce('+', delta)
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
