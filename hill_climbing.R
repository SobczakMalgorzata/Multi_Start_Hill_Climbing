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
  while(index < max_iter){
    before <- cec2013(function_number, current_point)
    for (i in (1:(length(current_point)))){
      best <- -1
      best_score <- -Inf
      for(j in (1:5)) {
        current_point[i] = current_point[i] + step_size[i] * candidate[[j]]
        temp = cec2013(function_number,current_point)
        current_point[i] = current_point[i] - step_size[i] * candidate[[j]]
        if(temp > best_score) {
          best_score = temp
          best = j
        }
      }
      if (candidate[[best]] != 0) {
        current_point[i] = current_point[i] + step_size[i] * candidate[[best]];
        step_size[i] = step_size[i] * candidate[[best]]
      }
    }
    condition <- (cec2013(function_number, current_point) - before)
    if ((condition < epsilon) | ((cec2013(function_number, current_point)==Inf) & (before == Inf))){
      return(current_point)
    }
    index <- index + 1
  }
  
  return(current_point)
}


multi_start_hill_climbing <- function(n, data, function_number, initial_step_size, epsilon = 0.0001, max_iter = 100, initial_acceleration = 1.2){
  
  a <- list()
  for(i in (1:(length(data)))) {
    a[[i]] <- hill_climbing(n, data[i,], function_number, initial_step_size, epsilon, max_iter, initial_acceleration)
  }
  return (a)
}

