
sum_weight <- function(column,W,df){
  if (sum(column)<= W & !is.null(sum(column))){
    index <- c(column)
    return(match(index, df[,1]))
  }
}


brute_force_knapsack <- function(x, W = NULL){
  # get no. of rows
  value <- NULL
  rows <- nrow(x) # Rows of Dataframe
  updated_index <- c()
  row_value <- c()
  for (i in 1:rows){
    comb_matrix <- combn(x[,1],m = i)
    true_weight <- apply(comb_matrix, MARGIN = 2,FUN = sum_weight,W = W,df=x)
    # Remove Nulls
    true_weight <- true_weight[lengths(true_weight)!=0]
    
    updated_index <- c(updated_index,true_weight)
   
  }
  
  # Get Value
  
  for (i in seq(updated_index)){
    
    updated_value <-sum(x[updated_index[[i]],2])
    
    value <- max(updated_value,value)
    
    if (value == updated_value){
      row_value <- updated_index[[i]]
    }
  }
  
  result <- list(
    value = round(value),
    elements = row_value
  )
  
  return(result)
  
}


brute_force_knapsack(x = knapsack_objects[1:8,], W=3500)

