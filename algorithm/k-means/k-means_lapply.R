# k-means - lapply based implementation
# 
# Author: Haichuan Wang
#
# k-means using lapply based iterative algorithm
###############################################################################

app.name <- "k-means_lapply"

setup <- function(args=c('10000', '3', '10', '15')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 10000L }
    
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
    
    dist.func <- function(ptr){ 
        dist.inner.func <- function(center){
            sum((ptr-center)^2)
        }
        lapply(centers, dist.inner.func)
    }
    
    centers <- Points[1:ncluster] #pick 10 as default centers
    size <- integer(ncluster)
    ptm <- proc.time() #previous iteration's time
    for(iter in 1:niter) {
        #map each item into distance to 10 centers.
        dists <- lapply(Points, dist.func)
        ids <- lapply(dists, which.min)
        #calculate the new centers through mean
        for(j in 1:ncluster) {
            cur_cluster <- Points[ids==j]
            size[j] <- length(cur_cluster)
            centers[[j]] <- Reduce('+', cur_cluster) / size[j]
        }
        ctm <- proc.time()
        cat("[INFO]Iter", iter, "Time =", (ctm - ptm)[[3]], '\n')
        ptm <- ctm
    }
    #calculate the distance to the 10 centers
    
    cat("Centers:\n")
    print(centers);
    cat("Sizes:\n")
    print(size);
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
