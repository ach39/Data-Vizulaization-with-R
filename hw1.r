#
# @author : achauhan39 (GTid: 903271003)
# @date : Jan-20-2017
# @description : File has following functions
#                log_gamma_loop()
#                log_gamma_recursive()
#                sum_log_gamma_loop()
#                sum_log_gamma_lrecursive()
#                sum_lgamma()
#                compare_time()


library(ggplot2)
options(expressions=500000)
DBG <- 0

# ************************************************
# Function    : log_gamma_loop
# Description : interative version of log gamma
# Args        : postive integer (n)
# Returns     : log((n-1)!)
# ************************************************
log_gamma_loop <- function(n) {
  if(n <= 0){
    if(DBG) { print (" log_gamma_loop : n out of range") }
    return(0)
  }
  result <- 0 ;n <- n-1
  while(n>1){
    result <- result + log(n)
    n <- n-1
  }
  return(result)
}


# ************************************************
# Function    : log_gamma_recursive
# Description : Recursive version of log gamma
# Args        : postive integer (n)
# Returns     : log((n-1)!)
# ************************************************
log_gamma_recursive <- function(n) {
  return(f_recurse(n-1))
}

f_recurse = function(n){
  if(n<=0){
    #print (" log_gamma_recursive : n out of range")
    return(0)
  }

  return(log(n) + f_recurse(n-1) )
}



# *************************************************************
# Function    : sum_log_gamma_loop
# Description : sum of log gamma using iterative implemetation
# Args        : postive integer (n)
# Returns     : sum of LogGamma results over range 1 to n
# *************************************************************
sum_log_gamma_loop <- function(n) {
  gamma_val<-c()
  
  while(n>0){
    gamma_val <- c(gamma_val,log_gamma_loop(n))
    n<- n-1
  }
  result<-sum(gamma_val)

  if(DBG) { cat("sum=", result, "\t gamma_val =", gamma_val , "\n") }
  
  return(result)
}


# *************************************************************
# Function    : sum_log_gamma_recursive
# Description : sum of log gamma using recursive implemetation
# Args        : postive integer (n)
# Returns     : sum of LogGamma results over range 1 to n
# *************************************************************
sum_log_gamma_recursive<- function(n){
  if(n<=0) {
    return(0)
  }
  return(log_gamma_recursive(n) + sum_log_gamma_recursive(n-1))
}

# *************************************************************
# Function    : sum_lgamma
# Description : sum of log gamma using built-in R function lgamma
# Args        : postive integer (n)
# Returns     : sum of LogGamma results over range 1 to n
# *************************************************************
sum_lgamma <- function(n){
  gamma_val<-c()
  
  while(n>0){
    gamma_val <- c(gamma_val,lgamma(n))
    n<- n-1
  }
  
  result<-sum(gamma_val)
  if(DBG) { cat("sum=", result, "\t gamma_val =", gamma_val , "\n") }
  
  return(result)
}

# *************************************************************
# Function    : compare_time
# Description : compares execution time of sum of log gamma function 
#               using 3 different implemenatations.
#                 1) Iterative (sum_log_gamma_loop)
#                 2) Recursive (sum_log_gamma_recursive)
#                 3) built-in R function (sum_lgamma)
#
# Args        : lo,hi - low and high range from which n is to be drawn
#               sample_cnt - number of samples from (lo,hi) range
#
# Returns     : genrates a dataFrame and comparison-plot of execution 
#               time of abovementioned three implementations.
#
# *************************************************************
compare_time <- function(lo=1,hi=4000, sample_cnt=10){
  options(expressions=500000)
  n_vals <- seq(lo,hi, length= sample_cnt)
  loop_time <- rep(NA,sample_cnt)
  recurse_time <- rep(NA,sample_cnt)
  r_time <- rep(NA,sample_cnt)
  i <- 1

  for(n in n_vals ) {
    loop_time[i]<- (system.time(sum_log_gamma_loop(n)))[1]
    if(n < 4000){ recurse_time[i] <- (system.time(sum_log_gamma_recursive(n)))[1] }
    r_time[i]<- (system.time(sum_lgamma(n)))[1]
    i <- i+1
  }
  
  tbl = data.frame('N'= n_vals, 'Loop'= loop_time,'Recurse' = recurse_time,'R'= r_time)
  print(tbl)

  time_df = stack(list(loop=loop_time,recursion=recurse_time, R= r_time))
  names(time_df) = c("sys_time" , "Impl")
  time_df$N = n_vals
  time_df = subset(time_df , sys_time>=0)
  
  ggplot(time_df, aes(x=N, y=sys_time)) + geom_line(aes(color=Impl),size=0.8) + 
    geom_point(aes(color =Impl)) +
    xlab("N") + ylab("Computation Time (sec)") +
    ggtitle("Comparsion of computation time for three implementations ")
  
  #return(tbl)
}

compare_time(1,3000,10)
















# log_gamma_recursive = function(n){
#   n = n-1
#   f_recurse = function(n){
#     if(n<=0){return(0)}
#     print(n)
#     return(log(n)+ f_recurse(n-1))
#   }
#   
#   #return(f_recurse)
# }
