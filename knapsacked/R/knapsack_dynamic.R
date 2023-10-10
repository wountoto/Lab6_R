knapsack_dynamic <-
function(x, W){
  # checking the input
  stopifnot(is.data.frame(x),c('w','v') %in% colnames(x), 
            length(W)==1, min(x$w)<=W, is.numeric(x$w), is.numeric(x$v),W>0)
  n <- nrow(x) # how many objects that are to be determined
  m <- matrix(0, nrow = n, ncol = W) # an empty matrix with the properties resulting from the weight and objects
  
  for(i in 2:n){ # row wise indexing
    for(j in 1:W){ # column wise indexing
      if(x$w[i] <= j){
        m[i, j] <- max(m[i-1, j], m[i-1,j-x$w[i]] + x$v[i])
      } else {
        m[i, j] <- m[i-1, j]
      }
    }
  }
  
  
  value <- round(m[n, W], digits = 0)

  
  final<-c()
  i <- n 
  j <- W
  while ( i > 0 && j > 0){
    if(isTRUE(m[i,j]!=m[i-1,j])){
      final<-c(final,i)
      j<-j-x$w[i]
    }
    i<-i-1
  }
  return(list("value"=value,"elements"=final))
}
