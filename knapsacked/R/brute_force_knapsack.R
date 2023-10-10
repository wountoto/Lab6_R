brute_force_knapsack <-
function(x,W, parallel=FALSE){
  # checking the input
  stopifnot(is.data.frame(x),c('w','v') %in% colnames(x), 
            length(W)==1, min(x$w)<=W)
  func <- function(i){
    
    # get the combinations
    idx  <- base::intToBits(i) ==1
    
    # Use the combinations to pick out the rows in x
    # if the weight for the combination is lower than W
    if(sum(x$w[idx]) <= W && sum(x$v[idx]) > value){
      value <- sum(x$v[idx])
      elements <- as.numeric(rownames(x[idx,]))
    }
    
    return(list('value'=round(value,0), 'elements'= elements))
  }
  # If parralell is false then do..
  
  if(parallel==FALSE){
    value <- 0
    elements <- c()
    # making a function which can be called for the parallelize
    
    res <- sapply((1:2^nrow(x)-1),func)
    comb <- res[,which.max(res['value',])]
    return(comb)
  }
  
  else{
    value <- 0
    elements <- c()
    # checking coores on the cumputer
    core <- parallel::detectCores()
    cl <- parallel::makeCluster(core-1, type = "PSOCK")
    doParallel::registerDoParallel(cl)
    parallel::clusterExport(cl,c('x','func','W','value','elements'),envir=environment())
    
    
    results <- parallel::parSapply(cl,(1:2^nrow(x)-1),func)
    comb <- results[,which.max(results['value',])]
    return(comb)
    parallel::stopCluster(cl)
    
  }
  
}
