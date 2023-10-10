greedy_knapsack <-
function(x, W){
  # checking the input
  stopifnot(is.data.frame(x),c('w','v') %in% colnames(x), 
            length(W)==1, min(x$w)<=W, is.numeric(x$w), is.numeric(x$v),W>0)
  
  # Reorder the rows with value/weight in decreasing order
  # to add as much value per weight as possible
  x <- x[order(x$v/x$w, decreasing = TRUE),]
  
  # creating objects for the loop
  value <- 0 # values for the print
  
  elements <- c() # element list for the print
  
  knapsack_capacity <- W # current capacity of the knapsack
  k<- 1
  
  # While loop until the knapsack is "full" S1
  while(knapsack_capacity >= x$w[k]){
    
    # Updating the current capacity and adding the elements
    
    value <- value +x$v[k]
    elements[k] <- as.numeric(rownames(x)[k]) 
    knapsack_capacity <- knapsack_capacity - x$w[k]
    k <- k+1
  }
  lis <- list('value'=round(value,0), 'elements'= elements)
  return(lis)
  
}
