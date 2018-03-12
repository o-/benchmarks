# k-means-1D - R internal k-means based implementation
# 
# Author: Haichuan Wang
#
# k-means-1D using R built-in k-means implementation
###############################################################################
app.name <- "k-means-1D"
setup <- function(args=c('1000000', '10', '15')) {
  set.seed(42)
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000L }
    
    ncluster<-as.integer(args[3])
    if(is.na(ncluster)){ ncluster <- 10L }
    
    niter<-as.integer(args[3])
    if(is.na(niter)){ niter <- 15L }
    
    cat('[INFO][', app.name, '] n=', n, ', ncluster=', ncluster, ', niter=', niter, '\n', sep='')
    
    #the data, each is
    mean_shift <- rep(0:(ncluster-1), length.out = n)
    Points <- rnorm(n, sd = 0.3) + mean_shift
    Points <- lapply(1:n, function(i){Points[i]})
    
    return(list(Points=Points, ncluster=ncluster, niter=niter))
}
run <- function(dataset) {
    ncluster <- dataset$ncluster
    niter <- dataset$niter
    Points <- dataset$Points
    vPoints <- simplify2array(Points)
    
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
