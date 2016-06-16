list.of.packages <- c("ppls", "cec2013")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("MASS")
library("ppls")
library("cec2013")
source('random_set_generation.R')
source('hypercube_generation.R')
source('poisson_disc.R')
source('hill_climbing.R')

runCECtest.Single <- function(n_starting_points = 10, n = 5, 
                      min, max, 
                      CEC_function_index = 1, 
                      points_selection_method = "random", 
                      poisson.r = 5, 
                      hillclimb.initial_step_size, hillclimb.epsilon = 1e-04, hillclimb.max_iter = 5000, 
                      logFileName){
  if(points_selection_method == "random")
  {starting_points = random_set_generation(n_starting_points, n, rep(min,n), rep(max,n))}
  else if (points_selection_method == "hypergrid")
  {starting_points = hypercube_generation(n_starting_points, n, rep(0,n), 0.1, min, max)} 
  else if (points_selection_method == "poisson disk")
  {starting_points = poisson_disc(n, n_starting_points, poisson.r, rep(min,n), rep(max,n))} 
  
  results <- t(simplify2array(multi_start_hill_climbing(n, starting_points, CEC_function_index,
                                      hillclimb.initial_step_size, hillclimb.epsilon, hillclimb.max_iter)))
  write.csv2(results, file = logFileName)
  return(results)
}

runCECtest.All <- function(starting_points = 10, dimensions = c(2,5,10), 
                           min = -100, max = 100, 
                           poisson.r = 5, 
                           hillclimb.initial_step_size = 1, 
                           hillclimb.epsilon = 1e-04, hillclimb.max_iter = 5000,
                           attempts = 10, results_path = "")
{
  
  answer <- list()
  for (n in dimensions) {
    for (attempt in (1:attempts)) {
      for (n_starting_points in starting_points) {
        for(CEC_function_index in (1:28)) {
          for(points_selection_method in c("random","poisson disk", "hypergrid")){
            logFileName <- sprintf("%sf%d_D%d_nStart%d_stepSize%f_epsilon%f_maxIter%d_%s_%d.csv", results_path,CEC_function_index,n, n_starting_points,hillclimb.initial_step_size,hillclimb.epsilon,hillclimb.max_iter,points_selection_method, attempt)
            runCECtest.Single(n_starting_points, n, 
                                          min, max, 
                                          CEC_function_index, 
                                          points_selection_method, 
                                          poisson.r, 
                                          hillclimb.initial_step_size, hillclimb.epsilon = 1e-04, hillclimb.max_iter = 5000,logFileName = logFileName)
    
          }
        }
      }
    }
  }
}

# Example call of runCECtest function:
# runCECtest.All(n_starting_points = 10, dimensions = c(2,5,10), min = -100, max = 100, hillclimb.initial_step_size=1, hillclimb.max_iter = 2000, attempts = 10, results_path="results/")


  