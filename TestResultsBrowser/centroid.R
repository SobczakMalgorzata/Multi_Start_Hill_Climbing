centroid <- function(mat){
  d <- (length(mat[1,])-1)/2
  l <- length(mat[,1])
  c <- apply(mat[,((d+1):(2*d))], FUN = sum, MARGIN = 2)/l
  return (c)
}