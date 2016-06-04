library("MASS")
library("ppls")
library("cec2013")

#min <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
n <- 2
min <- seq(0.0,0.0, length.out = n)
#max <- c(10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0)
max <- seq(10.0,10.0, length.out = n)
sample_size <- 20
r <- 0.5
function_number <- 1
initial_step_size <- seq(1.0,1.0, length.out = n)
initial_acceleration <- 1.2
max_iter <- 20
#epsilon - our possible error
epsilon <- 1


data <- run(sample_size,n,r,min,max)
#length(data) <- sample_size

a <- multi_start_hill_climbing(n, data, function_number, initial_step_size, epsilon, max_iter) 
  