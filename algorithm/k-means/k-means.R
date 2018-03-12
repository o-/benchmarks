# k-means - R internal k-means based implementation
# 
# Author: Haichuan Wang
#
# k-means using R built-in k-means implementation
###############################################################################
app.name <- "k-means"

setup <- function(args=c('1000000', '3', '10', '15')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    ndim<-as.integer(args[2])
    if(is.na(ndim)){ ndim <- 3L }
    
    ncluster<-as.integer(args[3])
    if(is.na(ncluster)){ ncluster <- 10L }
    
    niter<-as.integer(args[4])
    if(is.na(niter)){ niter <- 15L }
    
    cat('[INFO][', app.name, '] n=', n, ', ndim=', ndim, ', ncluster=', ncluster, ', niter=', niter, '\n', sep='')
    
    #the data, each is
    mean_shift <- rep(0:(ncluster-1), length.out = ndim*n)
    Points <- matrix(rnorm(ndim*n, sd = 0.3) + mean_shift, ncol=ndim)
    #now change data into list structure
    Points <- lapply(1:n, function(i) Points[i,])
    
    list(Points=Points, ndim<- ndim, ncluster=ncluster, niter=niter)
}
run <- function(dataset) {
    ncluster <- dataset$ncluster
    niter <- dataset$niter
    Points <- dataset$Points
    vPoints <- t(simplify2array(Points))
    
    res<-kmeans(vPoints, ncluster, iter.max=niter);
    cat("Centers:\n")
    print(res$centers);
    cat("Sizes:\n")
    print(res$size);
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
