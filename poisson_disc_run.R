#min <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
n <- 2
min <- seq(0.0,0.0, length.out = n)
#max <- c(10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0)
max <- seq(10.0,10.0, length.out = n)
sample_size <- 300
r <- 0.5
a = run(sample_size,n,r,min,max)
