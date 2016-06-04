
random_set_generation <- function(set_size, n, min, max){
  vector <- c()
  for (i in (1:n)) {
    row <- runif(set_size, min[i], max[i])
    vector <-  c(vector, row)
  }
  matrix <- matrix(vector,nrow = n, ncol = set_size, byrow = TRUE)
  return (matrix)
}