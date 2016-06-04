hypercube_generation <- function(set_size, n, start_point, cube_size, min=-1000, max=1000) {
  all_elems <- set_size*n
  mat <- matrix(nrow=set_size,ncol = n)
  for(i in (1:set_size)){
    mat[i,] <- start_point - (sample(max,n)* cube_size)
  }
  return(mat)
}

#exemplary call of function
#a= hypercube_generation(2,3,seq(0.0,0.0, length.out = 3),1)