run <- function(set_size, sample_size, n, r, min, max) {
  data <- set_generation(set_size, n, min, max)
  poisson_disc(n, sample_size, data, r, min, max)
}

poisson_disc <- function(n, sample_size, set, r, min, max, k=30){
  set_size <- length(set[1,])
  x0_index <- sample(1:set_size, 1)
  x0 <- set[,x0_index]
  active <- x0
  new_point_list <- x0
  k_points <- generate_for_sphere(k, x0, r, n, min, max)
  
  new_point_list <- c(new_point_list,k_points)
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
