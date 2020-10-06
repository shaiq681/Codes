greedy_knapsack <- function(x,W){
  x[,'Ratio'] <- x[,2]/x[,1]
  
  new_df <- x[order(-x$Ratio),]
  
  capacity =W
  weight = 0
  value = 0
  counter = 1
  elements = c()
  
  while( weight <= capacity){
    value = value + new_df[counter,2]
    row <- as.integer(rownames(new_df[counter,]))
    elements <-c(elements,row)
    counter = counter +1
    weight <- weight + new_df[counter, 1]
  }
  
  result = list(
    value = round(value),
    elements = elements
  )
  
  return(result)
}


greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

