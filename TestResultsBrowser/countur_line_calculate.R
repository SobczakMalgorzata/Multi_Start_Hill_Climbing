our_x <- seq(from = -100, to = 100, by = 1)#(-100:100:10)
our_y <- seq(from = -100, to = 100, by = 1)# c(-100:100:10)
our_z <- matrix(0,length(our_x), length(our_y))
for (i in (1:length(our_x)))
  for(j in (1:length(our_y)))
    our_z[i,j] <- cec2013(2,c(our_x[i],our_y[j]))



#filled.contour(our_x, our_y, our_z)
p <- contour(our_x, our_y, our_z)
