standard_deviation_for_dimensions <- function(mat){
  d <- (length(mat[1,])-1)/2
  l <- length(mat[,1])
  c <- apply(mat[,((d+1):(2*d))], FUN = sd, MARGIN = 2) 
  return (c)
}