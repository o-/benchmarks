# Matrix-Matrix Multiply
# 
###############################################################################

setup = function(args='2000') {
    n <- as.integer(args[1])
    if(is.na(n)){ n <- 2000 }
    
    cat("Matrix-Matrix Multiply of two",  n, "x", n, "matrices, built-in %*%\n");
    
    set.seed(42)
    A <- matrix(rnorm(n*n), ncol=n, nrow=n)
    B <- matrix(rnorm(n*n), ncol=n, nrow=n)
    
    list(A,B, n)
}



run <- function(data) {
    #a and b are matrix
    A <- data[[1]]
    B <- data[[2]]
    n <- data[[3]]
    C <- A %*% B
}
