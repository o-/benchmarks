# TODO: Add comment
# 
# Author: Haichuan Wang(hwang154@illinois.edu)
###############################################################################


setup <- function(args='1000000') {
    n<-as.integer(args[1])
    if(is.na(n)){ n <- 1000000 }
    return(n)
}

run <- function(l=1000000) {
	
	a <- as.integer(runif(l, 1, 1000000000));
	b <- as.integer(runif(l, 1, 1000000000));	
	for(i in 1:l) {
		m<-a[i];
		n<-b[i];
		while(n!=0) {
			t=m; 
			m=n;
			n=t %% n;
		}
	}
	print(l)
}

if (!exists('harness_argc')) {
    n <- setup(commandArgs(TRUE))
    run(n)
}
