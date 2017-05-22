#
# @author : achauhan39 (GTid:903271003)
# @date   : Mar-17-2017
# @description : Home work-3 
# @version
#   Mar22 - Add LogReg function (gradinet.descent)
#   Mar 27 - submit(v2)
#   Mar 28 - add new convergence criteria based on cost
#
#


library(ggplot2)
library(gridExtra)

#Load data
train_org <- read.csv("mnist_train.csv", header=F)
test_org <- read.csv("mnist_test.csv",header=F)

train = t(train_org)
test = t(test_org)

last_col = ncol(train)

#subset train data
train_0_1 = subset(train , train[,last_col]==0 | train[,last_col]==1)
train_3_5 = subset(train , train[,last_col]==3 | train[,last_col]==5)

#print number of training samples for each class
table(train[,last_col])                
table(train_0_1[,last_col])
table(train_3_5[,last_col])

#subset test data
test_0_1 = subset(test , test[,last_col]==0 | test[,last_col]==1)
test_3_5 = subset(test , test[,last_col]==3 | test[,last_col]==5)

#print number of test samples for each class
table(test[,last_col])                
table(test_0_1[,last_col])
table(test_3_5[,last_col])

#separate features and label in trainig dataset
train_01_x = train_0_1[,seq(1,last_col-1)]
train_01_y = as.matrix(train_0_1[,last_col])

train_35_x = train_3_5[,seq(1,last_col-1)]
train_35_y = as.matrix(train_3_5[,last_col]) 

#separate labels in test dataset
test_01_x = test_0_1[,seq(1,last_col-1)]
test_01_y = as.matrix(test_0_1[,last_col])

test_35_x = test_3_5[,seq(1,last_col-1)]
test_35_y = as.matrix(test_3_5[,last_col]) 


cat("set :" , nrow(train_01_x), ncol(train_01_x), is.matrix(train_01_x))
cat("set :" , nrow(train_01_y), ncol(train_01_y), is.matrix(train_01_y))

cat("set :" , nrow(train_35_x), ncol(train_35_x), is.matrix(train_35_x))    
cat("set :" , nrow(train_35_y), ncol(train_35_y), is.matrix(train_35_y)) 

cat("set :" , nrow(test_01_x), ncol(test_01_x), is.matrix(test_01_x))
cat("set :" , nrow(test_01_y), ncol(test_01_y), is.matrix(test_01_y))

cat("set :" , nrow(test_35_x), ncol(test_35_x), is.matrix(test_35_x))    
cat("set :" , nrow(test_35_y), ncol(test_35_y), is.matrix(test_35_y))  


###########################################################################
# Q0. Print images 
#
###########################################################################

d0 = subset(train_0_1, train_0_1[,last_col]==0)[1, seq(1,last_col-1)]
#image(t(matrix(d0,28,28)), col=gray.colors(256))
d0 <- apply(matrix(d0,28,28), 2, rev)
image(t(d0), col=gray.colors(256))

d1 = subset(train_0_1, train_0_1[,last_col]==1)[1, seq(1,last_col-1)]
#image(t(matrix(d1,28,28)), col=gray.colors(256))
d1 <- apply(matrix(d1,28,28), 2, rev)
image(t(d1), col=gray.colors(256))

d3 = subset(train_3_5, train_3_5[,last_col]==3)[1, seq(1,last_col-1)]
#image(t(matrix(d3,28,28)), col=gray.colors(256))
d3 <- apply(matrix(d3,28,28), 2, rev)
image(t(d3), col=gray.colors(256))

d5 = subset(train_3_5, train_3_5[,last_col]==5)[1, seq(1,last_col-1)]
#image((matrix(d5,28,28)), col=gray.colors(256))
d5 <- apply(matrix(d5,28,28), 2, rev)
image(t(d5), col=gray.colors(256))


#Add bias term
if (!all(train_01_x[,1] == 1)) { train_01_x = cbind(rep(1, nrow(train_01_x)) , train_01_x)}
if (!all(train_35_x[,1] == 1)) { train_35_x = cbind(rep(1, nrow(train_35_x)) , train_35_x)}

if (!all(test_01_x[,1] == 1)) { test_01_x = cbind(rep(1, nrow(test_01_x)) , test_01_x)}
if (!all(test_35_x[,1] == 1)) { test_35_x = cbind(rep(1, nrow(test_35_x)) , test_35_x)}


# covert label 3 and 5 to 0 & 1 in train_35_y and test_35_y
train_35_y = ifelse(train_35_y == 3,0,1)   
test_35_y  = ifelse(test_35_y  == 3,0,1) 

table(train_35_y)  
table(test_35_y)

###########################################################################
# This section contains all the helper functions needed to answer Q# 3 to 5
#
###########################################################################

#### Sigmoid function
sigmoid <- function(z) { return(1/(1+exp(-z))) }


#### cost function  -- Assumes theta is numeric
cost <- function(X,Y,theta){
  hx =  1/(1 + exp(-X %*% (theta)))
  j = (-1/nrow(X)) * sum(Y*log(hx) +  (1-Y)* log(1-hx))
  return(j)
}

#### Predict function
my_pred <- function(X,theta){ return(sigmoid(X %*% theta)) }
my_acc <-function(X,Y,theta) {
  my_fit <- my_pred(X, theta)
  my_fit <- ifelse(my_fit > 0.5,1,0)
  acc = mean(my_fit == Y)
  #print("Accuracy :", acc)
  return(acc)
}

####  Gradient descent function : implements derivative of Cost function
grad_derv <- function(x, y, theta) {
  gradient <- (1/nrow(x)) * (t(x) %*% (1/(1 + exp(-x %*% t(theta))) - y))
  return(t(gradient))
}

#########################################################
# LogReg_v1 : uses Max Iteration as convergence criteria
#
#########################################################
LogReg_v1 <- function(x, y,alpha=0.1,iterations=1000) {
  # Initialize theta vecrtor to zero 
  theta <- matrix(rep(0, ncol(x)), nrow=1)
  
  theta_hist <- theta
  for (i in 1:iterations) {
    theta <- theta - alpha * grad_derv(x, y, theta)
    if(all(is.na(theta))) break
    #update theta_hist
    theta_hist <- rbind(theta_hist, theta)
    #alpha = 0.9 * alpha
  }
  
  #cat("LogReg_v1 : i " ,i , "iterations=" , iterations , "\n")
  return(theta_hist[nrow(theta_hist),])
}

#########################################################
# LogReg_v1a : same as logReg_v1 but theta initialised to 
# random value
#
#########################################################
LogReg_v1a <-function(x,y,alpha=0.1,iterations=1000,min_theta=-0.001, max_theta=0.001) {
  
  # Initialize theta vecrtor to random value 
  theta <- matrix(runif(ncol(x),min_theta,max_theta), nrow=1)
  print ("Random theta init")
  #print (theta[1:20])

  theta_hist <- theta
  for (i in 1:iterations) {
    theta <- theta - alpha * grad_derv(x, y, theta)
    if(all(is.na(theta))) break
    #update theta_hist
    theta_hist <- rbind(theta_hist, theta)
    #alpha = 0.9 * alpha
  }
  
  #cat("LogReg_v1 : i " ,i , "iterations=" , iterations , "\n")
  return(theta_hist[nrow(theta_hist),])
}

#########################################################
# LogReg_v2 : uses 'difference in theta update' 
#             as convergence criteria
#
# A safety check was added to avoid this function running 
# forever. This is done by setting max_iterations to a very 
# high value (1e6).
#
#########################################################
LogReg_v2 <- function(x, y,alpha=0.1,iterations=1e6, threshold=0.001) {
  theta <- matrix(rep(0, ncol(x)), nrow=1)
  theta_hist <- theta
  for (i in 1:iterations) {
    
    theta <- theta - alpha * grad_derv(x, y, theta)
    if(all(is.na(theta))) break
    
    theta_hist <- rbind(theta_hist, theta)
    if(i > 2) 
    {
        if(all(abs(theta - theta_hist[i-1,]) < threshold)) {
          #print(" converged - delta threshold met ")
          break 
        }
    }
    #alpha = 0.9 * alpha
  }
  #cat("LogRegv2 : i " ,i , "threshold=" , threshold , "\n")
  return(theta_hist[nrow(theta_hist),])
}

#########################################################
# LogReg_v3 : combination of LogReg v1 and v2. 
# Uses max_iter and theta_update_threshold (which ever is 
# met first) as convergence criteria.
#
#########################################################
LogReg_v3 <- function(x, y,alpha=0.1,iterations=1e4, threshold=0.001) {
  # Initialize theta vecrtor to zero 
  theta <- matrix(rep(0, ncol(x)), nrow=1)
  
  theta_hist <- theta
  for (i in 1:iterations) {
    theta <- theta - alpha * grad_derv(x, y, theta)
    if(all(is.na(theta))) break
    #update theta_hist
    theta_hist <- rbind(theta_hist, theta)
    
    #check convergence
    if(i > 2) {
      if(all(abs(theta - theta_hist[i-1,]) < threshold)) break 
    }
    #alpha = 0.9 * alpha
  }
  #print("decay alpha")
  
  cat("LogReg Iterations : i " ,i , "iterations=" , iterations , "\n")
  return(theta_hist[nrow(theta_hist),])
}

#########################################################
# LogReg_v4 : use norm of theta update as convergence criteria 
#
#########################################################
LogReg_v4 <- function(x, y,alpha=0.1,iterations=1e6, threshold=0.001) {
  theta <- matrix(rep(0, ncol(x)), nrow=1)
  theta_hist <- theta
  prev_cost = 1000
    
  for (i in 1:iterations) {
    
    theta <- theta - alpha * grad_derv(x, y, theta)
    if(all(is.na(theta))) break
    
    theta_hist <- rbind(theta_hist, theta)
    curr_cost = cost(x,y,as.vector(theta))
    
    #cat("LogRegv4 : i " ,i , "prev,curr=" ,prev_cost,curr_cost , "diff=" , (prev_cost-curr_cost), "\n")
    
    if(i > 2) 
    {
      if(!is.na(curr_cost) & !is.na(prev_cost) & abs(prev_cost - curr_cost) < threshold)
      {
        cat("\n v4. COST converged. ")
        break 
      }
      
      #cat("LogRegv4 : i " ,i , "prev,curr=" ,prev_cost,curr_cost , "diff=" , (prev_cost-curr_cost), "\n")
      
      prev_cost = curr_cost
      
      # # check norm values
      # norm_prev = norm(as.matrix(theta_hist[i-1]))
      # norm_curr = norm(theta)
      # norm_d = norm_curr -norm_prev 
      # #cat("\n" ,norm_d, norm(theta), norm_prev)
      # if( (norm_curr/norm_prev) < 1.1)
      # {
      #   cat("\n v4.  converged  - NORM delta threshold met. ")
      #   break 
      # }
      
    }
    #alpha = 0.9 * alpha
  }
  #cat("LogRegv4 : i " ,i , "threshold=" , threshold , 
      #"norm_d =",  norm_d ,norm_curr, norm_prev, norm_curr/norm_prev, "\n")
  
  cat("LogRegv4 : i " ,i)

  
  return(theta_hist[nrow(theta_hist),])
}


#*************************************************************************
#            THIS SECTION CONTAINS FUNCTIONS FOR QUESTION 3 TO 5.        *  
#*************************************************************************


#################################################
# Q3a. Baseline - uses max_iter as convergence 
#                 criteria
# alpha =0.1
# theta initialised to zero
# Max iteration : 100 for set_01 and 250 for set_35
#################################################
Q3a <-function() {
  theta_01 <- LogReg_v1(x=train_01_x ,y=train_01_y,iterations=100)
  # accuracy for 0 & 1
  trainAcc  = my_acc(train_01_x,train_01_y,theta_01)
  testAcc = my_acc(test_01_x,test_01_y,theta_01)
  cost_01 = cost(train_01_x,train_01_y, theta_01)
  print("Q3a. Accuracy ")
  cat("Set_01 : test = " , testAcc, "\t train = ", trainAcc ,"\t cost =",cost_01, "\n")
  
  
  theta_35 <- LogReg_v1(x=train_35_x ,y=train_35_y,iterations=250)
  # accuracy for 3 & 5
  trainAcc  = my_acc(train_35_x,train_35_y,theta_35)
  testAcc = my_acc(test_35_x,test_35_y,theta_35)
  cost_35 = cost(train_35_x,train_35_y, theta_35)
  cat("Set_35 : test = " , testAcc, "\t train = ", trainAcc ,"\t cost =",cost_35, "\n")
  
}
Q3a()


#################################################
# Q3b. Similar to Q3a but provides average  
# accuracy over 10 runs.
# Each run randomly selects 80% of training set.
#################################################

Q3b <-function(train_x,train_y,test_x, test_y, grad_iterations=1000, alpha=0.1,threshold=0.001) {
  sz = nrow(train_x) * 0.8
  train_acc_iter=c()
  test_acc_iter=c()
  sample_cnt=c()
  cost_iter =c()

  for(n in 1:10){
    #Randomly select 80% samples from train-set 
    idx = sample(1:nrow(train_x), sz, replace=FALSE)
    train_ss_x =  train_x[idx ,]
    train_ss_y =  as.matrix(train_y[idx])
    
    theta = LogReg_v1(x=train_ss_x ,y=train_ss_y,alpha,iterations = grad_iterations)
    
    # Uncomment this to test Q4a_2 (different theta init values)
    # theta = LogReg_v1a(x=train_ss_x ,y=train_ss_y,alpha,iterations = grad_iterations,min_theta=-0.5, max_theta=0.5)
    
    
    # Uncomment this to test Q4b_3(cost as CC)
    #theta = LogReg_v4(x=train_ss_x ,y=train_ss_y,alpha,threshold=threshold)
    
    
    train_acc_iter = c(train_acc_iter, my_acc(train_ss_x,train_ss_y,theta))
    test_acc_iter  = c(test_acc_iter , my_acc(test_x, test_y, theta))
    cost_iter      = c(cost_iter, cost(train_ss_x,train_ss_y,theta))
    sample_cnt = c(sample_cnt, nrow(train_ss_x))
  }
  
  avg_train_acc = mean(train_acc_iter)
  avg_test_acc  = mean(test_acc_iter)
  avg_cost = mean(cost_iter)
  
  #cat("sample_cnt :", sample_cnt, "\n")
  cat("train_acc :", train_acc_iter, " | mean = ", avg_train_acc, "\n")
  cat("test_acc  :", test_acc_iter , " | mean = ", avg_test_acc,"\n")
  cat("cost      :", cost_iter , " | mean = ", avg_cost,"\n")
  cat(alpha, avg_train_acc, avg_test_acc, avg_cost)
  
  return(list(train_avg=avg_train_acc , test_avg=avg_test_acc , cost_avg =avg_cost))
}


Q3b(train_01_x,train_01_y,test_01_x, test_01_y,100)

Q3b(train_35_x,train_35_y,test_35_x, test_35_y,250)





#################################################
# Q4a_1
# vary.alpha : Accuracy as function of learning 
# rate alpha.
#################################################
vary.alpha <-function(){
  alpha = c(0.9, 0.1 , 0.01, 0.001 , 0.0001, 0.00001)
  train35_acc = c()
  test35_acc = c()
  cost_35 =c()
  
  for(a in alpha) {
    res = Q3b(train_35_x,train_35_y,test_35_x, test_35_y,250,a)
    train35_acc = c(train35_acc, res$train_avg)
    test35_acc = c(test35_acc, res$test_avg)
    cost_35 =c(cost_35, res$cost_avg)
  }
  df = data.frame(alpha = alpha, test =test35_acc, train = train35_acc,cost = cost_35)
  print(df)
  
}
vary.alpha()

#################################################
# Q4a_2 : Run test with different theta values.
#
# This excericse reuses Q3b(), which is modified 
# to use logReg_v1a that uses random initiaization 
# of theta.
# Testing was done by initialising theta
# to random values in following range
# a) 0,0 (baseline)
# b) 0 to 1
# c) -0.5 to +0.5
# d) -0.001 to +0.001
#################################################



#################################################
# Q4b_1
# vary.iter : Accuracy as function of gradient
# descent iterations. 
#################################################
vary.iter<-function(){
  iter = c(10,50,100,150,200,250,300,400,600,1000)
  train35_acc = c()
  test35_acc = c()
  cost_35 =c()
  
  for(i in iter) {
    res = Q3b(train_35_x,train_35_y,test_35_x, test_35_y,i)
    train35_acc = c(train35_acc, res$train_avg)
    test35_acc = c(test35_acc, res$test_avg)
    cost_35 =c(cost_35, res$cost_avg)
  }
  df = data.frame(iter = iter,test =test35_acc, train = train35_acc, cost = cost_35)
  print(df)
}
vary.iter()


#################################################
# Q4b_2
# vary.threshold : Accuracy as function of theta
# update threshold.
#################################################
Q4b_2 <-function(train_x,train_y,test_x, test_y, threshold=0.001) {
  sz = nrow(train_x) * 0.8
  train_acc_iter=c()
  test_acc_iter=c()
  sample_cnt=c()
  cost_iter =c()
  
  for(n in 1:10){
    #Randomly select 80% samples from train-set 
    idx = sample(1:nrow(train_x), sz, replace=FALSE)
    train_ss_x =  train_x[idx ,]
    train_ss_y =  as.matrix(train_y[idx])
    
    theta <-LogReg_v2(x=train_ss_x ,y=train_ss_y,threshold = threshold)
    
    train_acc_iter = c(train_acc_iter, my_acc(train_ss_x,train_ss_y,theta))
    test_acc_iter  = c(test_acc_iter , my_acc(test_x, test_y, theta))
    cost_iter      = c(cost_iter, cost(train_ss_x,train_ss_y,theta))
    sample_cnt = c(sample_cnt, nrow(train_ss_x))
  }
  
  avg_train_acc = mean(train_acc_iter)
  avg_test_acc  = mean(test_acc_iter)
  avg_cost = mean(cost_iter)
  
  cat("LogReg_v2 : sample_cnt - ", sample_cnt, "\n")
  cat("train_acc :", train_acc_iter, " | mean = ", avg_train_acc, "\n")
  cat("test_acc  :", test_acc_iter , " | mean = ", avg_test_acc,"\n")
  cat("cost      :", cost_iter , " | mean = ", avg_cost,"\n")
  
  return(list(train_avg=avg_train_acc , test_avg=avg_test_acc , cost_avg =avg_cost))
}

vary.threshold<-function(){
  threshold = c(0.1,0.01 ,0.005, 0.001,0.0005) 
  train35_acc = c()
  test35_acc = c()
  cost_35 =c()
  
  for(t in threshold) {
    res = Q4b_2(train_35_x,train_35_y,test_35_x, test_35_y,t)
    train35_acc = c(train35_acc, res$train_avg)
    test35_acc = c(test35_acc, res$test_avg)
    cost_35 =c(cost_35, res$cost_avg)
  }
  df = data.frame(thres = threshold,test =test35_acc, train = train35_acc, cost = cost_35)
  print(df)
  
}
vary.threshold()

#################################################
# Q4b_3
# testing with 3rd as convergence criteria.
#     update in cost < threshold
#
# Reuses Q3b() 
# Test Threshold = 0.01, 0.001,0.0001 for 
# different values of alpha = 0.1, 0.9, 2
#################################################

Q3b(train_35_x,train_35_y,test_35_x, test_35_y,alpha=2, threshold=0.0001)


#################################################
# Q5 - version-2 
#
#################################################
Q5_v2 <- function(train_x,train_y,test_x, test_y,iter, avg_n=10) {
  
  train_sz = seq(5,100,5)
  train_acc_sz = c()  
  test_acc_sz =c() 
  sample_cnt =c()
  ll_train_sz = c()
  ll_test_sz = c()
  
  print(" in test_train_size... ")
  for(i in train_sz){
  
    train_acc_iter=c()
    test_acc_iter=c()
    ll_train_iter=c()
    ll_test_iter=c()
    
    sz= nrow(train_x) * i/100
    for(n in 1:avg_n){
      # Randomly select samples
      idx = sample(1:nrow(train_x), sz, replace=FALSE)
      train_ss_x =  train_x[idx ,]
      train_ss_y =  as.matrix(train_y[idx])
      
      theta <-LogReg_v1(x=train_ss_x ,y=train_ss_y,iterations=iter)
      train_acc_iter = c(train_acc_iter, my_acc(train_ss_x,train_ss_y,theta))
      test_acc_iter  = c(test_acc_iter , my_acc(test_x, test_y, theta))
      ll_train_iter  = c(ll_train_iter , cost(train_ss_x, train_ss_y, theta))
      ll_test_iter   = c(ll_test_iter  , cost(test_x, test_y, theta))

    }
    
    sample_cnt = c(sample_cnt, nrow(train_ss_x))
    avg_train_acc = mean(train_acc_iter)
    avg_test_acc  = mean(test_acc_iter)
    avg_train_ll  = mean(ll_train_iter)
    avg_test_ll   = mean(ll_test_iter)
    
    print(i)
    cat("train_acc :", train_acc_iter, " | mean = ", avg_train_acc, "\n")
    cat("test_acc  :", test_acc_iter , " | mean = ", avg_test_acc,"\n")
    cat("train_cost:", ll_train_iter , " | mean = ", avg_train_ll ,"\n")
    cat("test_cost :", ll_test_iter , "  | mean = ", avg_test_ll ,"\n")
    
    train_acc_sz  = c(train_acc_sz ,avg_train_acc)
    test_acc_sz   = c(test_acc_sz  ,avg_test_acc)
    ll_train_sz   = c(ll_train_sz  ,avg_train_ll)
    ll_test_sz    = c(ll_test_sz   ,avg_test_ll)
    
    
  }
  
  #print(train_acc_sz)
  #print(test_acc_sz)
  
  #plot train_sz vs train_acc,test_acc
  df = data.frame(smpl_cnt = sample_cnt, N=train_sz,
                  train =train_acc_sz, test = test_acc_sz, ll_train = ll_train_sz , ll_test= ll_test_sz )
  print(df)
  p1 = ggplot(data=df,aes(x=N)) + 
    geom_line(aes(y=train,color='train_acc')) + geom_point(aes(y=train,color='train_acc')) +
    geom_line(aes(y=test,color='test_acc')) +  geom_point(aes(y=test,color='test_acc')) +
    xlab("training set size (%)") +
    ylab("Accuracy") +
    scale_x_continuous(breaks=seq(0,100,10)) +
    ggtitle("Accuracy for different training-set sizes")
  
  #plot train_sz vs Cost/Loss
 p2 = ggplot(data=df,aes(x=N)) + 
   geom_line(aes(y=ll_train,color='ll_train')) + geom_point(aes(y=ll_train,color='ll_train')) +
   geom_line(aes(y=ll_test,color='ll_test')) +  geom_point(aes(y=ll_test,color='ll_test')) +
    xlab("training set size (%)") +
    ylab("Cost/ Loss") +
    scale_x_continuous(breaks=seq(0,100,10)) +
    ggtitle("Cost/ Loss for different training-set sizes")
 
 grid.arrange (p1,p2, ncol=1)
 
 return(list(acc=p1,cost=p2))
}

res = Q5_v2(train_01_x,train_01_y,test_01_x, test_01_y,100)
ggsave(filename="set_01_Accuracy.png", plot=res$acc)
ggsave(filename="set_01_cost.png", plot=res$cost)

res= Q5_v2(train_35_x,train_35_y,test_35_x, test_35_y,250)
ggsave(filename="set_35_Accuracy.png", plot=res$acc)
ggsave(filename="set_35_cost.png", plot=res$cost)

