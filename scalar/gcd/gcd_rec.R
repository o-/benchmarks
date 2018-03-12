# TODO: Add comment
# 
# Author: Haichuan Wang(hwang154@illinois.edu)
###############################################################################


setup <- function(args='200000') {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 200000 }
    return(n)
}

run <- function(iter=200000, mn=c(123456789, 234736437)) {
  for(i in 1:iter) {
    m <- mn[[1]]
    n <- mn[[2]]
	gcd<-function(m,n) {
		if(n==0) { m; }
		else { gcd(n,m %% n); }		
	}
	r<-gcd(m,n);
	print(r);
  }
}


if (!exists('harness_argc')) {
    mn <- setup(commandArgs(TRUE))
    run(mn)
}
