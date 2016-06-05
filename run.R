
run <- function(sample_size, n, r, min, max, function_number, initial_step_size, epsilon, max_iter, file_number = 0) {
  library("MASS")
  library("ppls")
  library("cec2013")
  
  min_v <- seq(min,min, length.out = n)
  max_v <- seq(max,max, length.out = n)
  #sink(file_name)
  cat(paste("Dimensions:",n,"Sample size:", sample_size))
  cat("\n")
  cat(paste("Function number:",function_number,"Maximal number of iterations for point:", max_iter))
  cat("\n")
  cat(paste("Minimal starting point value:",min,"Maximal starting point value:", max))
  cat("\n")
  data <- poisson_disc(n, sample_size, r, min_v, max_v)
  write.csv2(data, file = paste("5_06/StartingData", file_number,"function",function_number, ".csv"))
  a <- multi_start_hill_climbing(n, data, function_number, initial_step_size, epsilon, max_iter)
  write.csv2(a, file = paste("5_06/EndingData", file_number,"function",function_number, ".csv"))
  #sink() 
  return (a)
}

#min <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
n <- 2
common_min <- -1000.0
#max <- c(10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0)
common_max <- 1000.0
sample_size <- 30
r <- 50
#function_number <- 1
initial_step_size <- seq(1.0,1.0, length.out = n)
initial_acceleration <- 1.2
max_iter <- 20000
#epsilon - our possible error
epsilon <- 0.1
#file_number <- 1
file_name <- "5_06/output_log.txt"
sink(file_name)
for (function_number in (1:28)) {
  for(file_number in (1:1)){
    answer <- run(sample_size,n,r,common_min,common_max, function_number, initial_step_size, epsilon, max_iter, file_number)
    }
}
sink()
#length(data) <- sample_size


  