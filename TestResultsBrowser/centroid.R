centroid <- function(result_points){
  d <- length(result_points[,1])
  l <- length(result_points[1,])
  c <- vector(length = l)
  for (i in (1:l)) 
  {
    c[i] <- sum(result_points[,i]/d)
  }
  return (c)
}