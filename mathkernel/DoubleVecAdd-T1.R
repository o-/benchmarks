# Vector Add
# 
###############################################################################

setup = function(args='20000000') {
    n <- as.integer(args[1])
    if(is.na(n)){ n <- 20000000 }
    
    cat("Vector Add two",  n, "size vectors, iterative method\n");
    
    A <- rnorm(n)
    B <- rnorm(n)
    
    list(A, B, n)
}



run <- function(data) {
    #a and b are matrix
    A <- data[[1]]
    B <- data[[2]]
    n <- data[[3]]
    C <- vector('double', n)
    for(i in 1:n) {
        C[i] = A[i] + B[i]
    }
    C
}
