# Vector Add
# 
###############################################################################

setup = function(args='40000000') {
    n <- as.integer(args[1])
    if(is.na(n)){ n <- 40000000 }
    
    cat("Vector Add two",  n, "size vectors(10% NA), built-in +\n");
    
    set.seed(42)
    A <- rnorm(n)
    B <- rnorm(n)
    idx <- runif(n*0.1, 1, n) #10% are NA
    A[idx] <- NA
    idx <- runif(n*0.1, 1, n) #10% are NA
    B[idx] <- NA
    list(A, B, n)
}



run = function(data) {
    #a and b are matrix
    A <- data[[1]]
    B <- data[[2]]
    n <- data[[3]]
    C <- A + B
}
