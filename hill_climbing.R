hill_climbing <- function(n, starting_piont, function_number, initial_step_size, epsilon = 0.0001, max_iter = 100, initial_acceleration=1.2) { #) {
  current_point <- starting_piont
  step_size <- initial_step_size
  acceleration <- initial_acceleration
  candidate <- list()
  candidate[1] <- -acceleration
  candidate[2] <- -1 / acceleration
  candidate[3] <- 0
  candidate[4] <- 1 / acceleration
  candidate[5] <- acceleration
  index = 0
  before <- cec2013(function_number, current_point)
  while(index < max_iter){
    mat = matrix(current_point, 5*n, n, byrow = T)
    
    for (i in (1:(length(current_point)))){
      for(j in (1:5)) {
        mat[(i-1)*5+j] = mat[(i-1)+j] + step_size[i] * candidate[[j]]
      }
    }
    scores = cec2013(function_number, mat)
    
    max_idx = which.max(scores)
    current_point = mat[max_idx,]
    best = (max_idx %% 5) + 1
    step_size[best] = step_size[best] * candidate[[best]]
    
    condition <- scores[max_idx] - before
    cat(paste("Index:",index))
    cat("\n")
    cat(paste("Point:", current_point[1:length(current_point)]))
    cat("\n")
    cat(paste("Difference:" ,condition, "Score:", scores[max_idx]))
    cat("\n")
    if ((condition < epsilon) | ((scores[max_idx] == Inf) & (before == Inf))){
      return(current_point)
      #sink(file_name)
      cat(paste("Index:",index,"Point:", current_point, "Difference:" ,condition))
      cat("\n")
      #sink()
    }
    index <- index + 1
    before <- current_point
  }
  
  return(current_point)
}


multi_start_hill_climbing <- function(n, data, function_number, initial_step_size, epsilon = 0.0001, max_iter = 100, initial_acceleration = 1.2){
  
  a <- list()
  for(i in (1:(length(data)))) {
    #sink(file_name)
    cat(paste("Sample Index:",i))
    cat("\n")
    #sink()
    a[[i]] <- hill_climbing(n, data[[i]], function_number, initial_step_size, epsilon, max_iter, initial_acceleration)
  }
  return (a)
}