# k Nearest Neighbor - R built-in knn1 based implementation
# 
# Author: Haichuan Wang
###############################################################################
app.name <- "k-NN"
library(class) #use built-in knn
setup <- function(args=c('30000', '30000', '10', '5')) {
  set.seed(42)
    train_n<-as.integer(args[1])
    if(is.na(train_n)){ train_n <- 30000L }
    
    test_n<-as.integer(args[2])
    if(is.na(test_n)){ test_n <- 30000L }   
    
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
    
    #change list_train into matrix
    train <- t(sapply(list_train, function(item){item$val}))
    train_cl <- factor(sapply(list_train, function(item){item$label}))
    test <- t(sapply(list_test, function(item){item$val}))
    test_cl <- knn(train, test, train_cl, k)

    #the raw data
    test_labels <- attr(test_cl, "levels")
    #finally change the test data to attach the label
    out_list_test <- lapply(1:test_n, function(i){
                                 item<-list_test[[i]]
                                 item$label<- test_labels[test_cl[i]]
                                 item
                             })
    print(summary(test_cl))
}


if (!exists('harness_argc')) {
    data <- setup(commandArgs(TRUE))
    run(data)
}
