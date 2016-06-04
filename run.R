
run <- function(sample_size, n, r, min, max, function_number, initial_step_size, epsilon, max_iter, file_number = 0) {
  library("MASS")
  library("ppls")
  library("cec2013")
  data <- poisson_disc(n, sample_size, r, min, max)
  write.csv2(data, file = paste("4_06/StartingData", file_number,"function",function_number, ".csv"))
  a <- multi_start_hill_climbing(n, data, function_number, initial_step_size, epsilon, max_iter)
  write.csv2(a, file = paste("4_06/EndingData", file_number,"function",function_number, ".csv")) 
  return (a)
}

#min <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
n <- 2
common_min <- 0.0
min <- seq(common_min,common_min, length.out = n)
#max <- c(10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0)
common_max <- 100.0
max <- seq(common_max,common_max, length.out = n)
sample_size <- 30
r <- 5
#function_number <- 1
initial_step_size <- seq(1.0,1.0, length.out = n)
initial_acceleration <- 1.2
max_iter <- 2000
#epsilon - our possible error
epsilon <- 0.1
#file_number <- 1

for (function_number in (1:28)) {
  for(file_number in (1:1))
  answer <- run(sample_size,n,r,min,max, function_number, initial_step_size, epsilon, max_iter, file_number)
}
  
#length(data) <- sample_size


  