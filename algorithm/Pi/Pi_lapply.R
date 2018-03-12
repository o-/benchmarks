# Monte-Carlo Pi - lapply based solution
# 
# Author: Haichuan Wang
###############################################################################
app.name <- 'Pi_lapply'

setup <- function(args=c('2000000')) {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 2000000L }
    
    cat('[INFO][', app.name, '] n=', n, '\n', sep='')
    
    rdata <- runif(n*2) 
    S <- lapply(1:n, function(i){rdata[(2*i-1):(2*i)]})
    
    S
}
run <- function(S) {
    
    #X includes "1" column, Y column vec
    sample.func <- function(aSample) {
        if((aSample[1]^2 + aSample[2]^2) < 1) {
            1.0
        } else {
            0.0
        }
    }
    
    sampleOut <- lapply(S, sample.func)
    
    reduceCount <- Reduce('+', sampleOut)
    mcPi <- 4.0 * reduceCount / length(S)
    
    cat('Pi = ', mcPi, '\n');
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
