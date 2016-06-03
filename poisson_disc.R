run <- function(sample_size, n, r, min, max) {
  poisson_disc(n, sample_size, r, min, max)
}

poisson_disc <- function(n, sample_size, r, min, max, k=30){
  x0 <- runif(n, min, max)
  active <- x0
  active_list <- list()
  active_list[[1]] <- active
  new_point_list <- list()
  new_point_list[[1]] <- x0
  while ((length(new_point_list)<(sample_size + 1)) & (length(active_list)>0)){
    active <- active_list[[length(active_list)]]
    k_points <- generate_for_sphere(k, active, r, n, min, max)
    active_list <- c(active_list,k_points)
    
    added <- 0
    for(j in (1:length(k_points))) {
      c <- k_points[[j]]
      valid <- TRUE
      for (i in (1:length(new_point_list))){
        x <- new_point_list[[i]]
        if (!(r^2 < sum((x-c)^2))){
          valid <-FALSE
        }
      }
      if (valid){
        new_point_list[[length(new_point_list)+1]] <- c
        added <- added+1
      }
      if (added == 0){
        temp_active_list <- list()
        index_correct = 0
        for (k in (1:length(active_list))){
          if (!(active_list[[k]] == active)){
            temp_active_list[[k+index_correct]] <- active_list[[k]]
          }
          else
            index_correct=1
        }
        active_list <- temp_active_list
      }
    }
  }
  return(new_point_list)
}

generate_for_sphere <- function(k,c,r,n, min, max) {
  new_min <- c(min,r)
  new_max <- c(max,(2*r))
  polar <- set_generation(k,(n + 1),new_min,new_max)
  new_points <- list()
  for (i in (1:k)) {
    polarx <- (polar[,i])
    random_dis <- polarx[n+1]
    length(polarx)<- n
    vect <- (c-polarx)
    direction <- (normalize.vector(vect))
    x <- (c + direction*random_dis)
    if (r^2 < sum((x-c)^2) & (2*r)^2 > sum((x-c)^2)) {
      new_points[[i]] <- x
    }
    else
      print("Error in k points genration")
  }
  return (new_points)
}

set_generation <- function(set_size, n, min, max){
  vector <- c()
  for (i in (1:n)) {
    row <- runif(set_size, min[i], max[i])
    vector <-  c(vector, row)
  }
  matrix <- matrix(vector,nrow = n, ncol = set_size, byrow = TRUE)
  return (matrix)
}
