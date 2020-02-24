rm(list = ls())

library(parallel)
library(doParallel)
library(foreach)
cl <- makeCluster(4)

N = 500L

test_list <- vector(mode = 'list', length = N)

lambda_vec <- rpois(N, 10)

f <- function(x) {
  #browser()
  assign('x', x, envir = .GlobalEnv)
  rm(x)
  eval(expr = expression({test_list[[x]] <- runif(lambda_vec[x])}), envir = .GlobalEnv)
  NULL
  }

p_list <- foreach(i = 1L:N) %dopar% {f(x = i)}




test_func <-function(x){
  
  envr<- environment()
  
  y <- 28
  
  envr$z <- 12
  
  envr$y <- y
  
  envr$y
  
  browser()
  
  test2_func <- function(x, envr){
   
    browser() 
   rm(y, envir = envr)
    
    
  }
  
  test2_func(x = x, envr = envr)
  
  browser()
  
}

test_func(x)
