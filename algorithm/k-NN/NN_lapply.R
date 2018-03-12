# Nearest Neighbor - lapply based implementation
# 
# Author: Haichuan Wang
###############################################################################
app.name <- "NN_lapply"
setup <- function(args=c('4000', '1000', '10', '5')) {
  set.seed(42)
    train_n<-as.integer(args[1])
    if(is.na(train_n)){ train_n <- 4000L }
    
    test_n<-as.integer(args[2])
    if(is.na(test_n)){ test_n <- 1000L }   
    
    ncluster<-as.integer(args[3])
    if(is.na(ncluster)){ ncluster <- 10L }
    
    k<-as.integer(args[4])
    if(is.na(k)){ k <- 5L }    
    
    cat('[INFO][', app.name, '] train_n=', train_n, ', test_n=', test_n, ', ncluster=', ncluster, ', k=', k, '\n', sep='')
    
    #generate training
    mean_shift <- rep(0:(ncluster-1), length.out = 3*train_n)
    train_set <- matrix(rnorm(3*train_n, sd = ncluster/2) + mean_shift, ncol=3)
    list_train_set <- lapply(1:train_n, function(i) {
                label_str <-paste('C', as.character(mean_shift[i]), sep="")
                list(val=train_set[i,], label=label_str)
            })
    
    test_set <- matrix(runif(3*test_n, min=-ncluster, max=2*ncluster-1), ncol=3)
    list_test_set <- lapply(1:test_n, function(i) {
                list(val=test_set[i,])
            })
    
    list(train_set=list_train_set, 
         test_set=list_test_set,
         ncluster=ncluster,
         k=k)
}
run <- function(dataset) {
    
    list_train<-dataset$train_set
    train_n <- length(list_train)
    list_test<-dataset$test_set
    test_n <- length(list_test)
    clusters<- dataset$clusters
    
    #outer loop, map function for each test
    NN.fun <- function(test_item) {
        #calculate the distance to all 
        dists.fun <- function(train_item) {
            sum((train_item$val - test_item$val)^2)
        }
        
        dists <- lapply(list_train, dists.fun)
        #get the which min
        min.train <- which.min(dists)
        #get the category
        test_item$label <- (list_train[[min.train]])$label
        test_item
    }
    
    out_list_test <- lapply(list_test, NN.fun)
    
    #get the cl
    test_cl_vec <- sapply(out_list_test, function(test_item){test_item$label})
    test_cl <- factor(test_cl_vec)
    print(summary(test_cl))
}

if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
