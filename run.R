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

runCECtest.Single <- function(n_starting_points = 10, n = 5, 
                      min, max, 
                      CEC_function_index = 1, 
                      points_selection_method = "random", 
                      poisson.r = 10, 
                      hillclimb.initial_step_size, hillclimb.epsilon = 1e-04, hillclimb.max_iter = 5000, 
                      logFileName){
  if(points_selection_method == "random")
  {starting_points = random_set_generation(n_starting_points, n, min, max)}
  else if (points_selection_method == "hypergrid")
  {starting_points = hypercube_generation(n_starting_points, n, rep(0,n), 0.1, min, max)} 
  else if (points_selection_method == "poisson disk")
  {starting_points = poisson_disc(n, n_starting_points, poisson.r, min, max)} 
  
  multi_start_hill_climbing(n, starting_points, CEC_function_index,
                            hillclimb.initial_step_size, hillclimb.epsilon, hillclimb.max_iter)
}

runCECtest.All <- function(n_starting_points = 10, n = 5, 
                           min, max, 
                           points_selection_method = "random", 
                           poisson.r = 10, 
                           hillclimb.initial_step_size, hillclimb.epsilon = 1e-04, hillclimb.max_iter = 5000)
{
  
  for (CEC_function_index in (1:28)) {
    answer[CEC_function_index] <- runCECtest.Single(n_starting_points, n, 
                                  min, max, 
                                  CEC_function_index, 
                                  points_selection_method, 
                                  poisson.r, 
                                  hillclimb.initial_step_size, hillclimb.epsilon = 1e-04, hillclimb.max_iter = 5000)
  }
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


#length(data) <- sample_size


  