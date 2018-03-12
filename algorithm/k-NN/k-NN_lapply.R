# k Nearest Neighbor - lapply based implementation
# 
# Author: Haichuan Wang
###############################################################################
app.name <- "k-NN_lapply"

setup <- function(args=c('2000', '2000', '10', '5')) {
  set.seed(42)
    train_n<-as.integer(args[1])
    if(is.na(train_n)){ train_n <- 2000L }
    
    test_n<-as.integer(args[2])
    if(is.na(test_n)){ test_n <- 2000L }   
    
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
    k <- dataset$k
    
    #outer loop, map function for each test
    kNN.fun <- function(test_item) {
        #calculate the distance to all 
        dists.fun <- function(train_item) {
            sum((train_item$val - test_item$val)^2)
        }
        
        dists_list <- lapply(list_train, dists.fun)
        #change to dists_vec, and do the sorting
        dists <- unlist(dists_list)
        
        mink.indices <-order(dists)
        #then should pick the first k items, find t
        train_items_indices <- mink.indices[1:k]

        train_items_category <- character(k)
        for(i in 1:k) {
          train_items_category[i] <- list_train[[train_items_indices[i]]]$label
        }
        
        #now get the their label and vote
        test_item$label <- names(which.max(table(train_items_category)))
        test_item
    }
    
    ptm <- proc.time() #previous iteration's time
    out_list_test <- lapply(list_test, kNN.fun)
    
    
    #get the cl
    test_cl <- lapply(out_list_test, function(test_item){test_item$label})
    test_cl <- factor(unlist(test_cl))
    cat("[INFO]Time =", (proc.time()-ptm)[[3]], '\n')
    print(summary(test_cl))
}


if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
