
library(bayesm)
library(dplyr)
data(margarine)
head(margarine)



###########
#Exercise 1
###########

average=apply(as.matrix(margarine$choicePrice[,3:12]), 2, mean)
print(average)

dispersion=apply(as.matrix(margarine$choicePrice[,3:12]), 2, sd)
print(dispersion)

n = length(margarine$choicePrice$choice)
num_choice = table(margarine$choicePrice[,2])
market = rbind(num_choice,  format(num_choice/n, digits = 1))
print(market)
mean(num_choice)


###########
#Exercise 2
###########

CP<-data.frame(margarine$choicePrice)
DM<-data.frame(margarine$demos)
CPDM<-left_join(CP,DM,by="hhid")

like_fun_2=function(param,CP) {
  ni=nrow(CP)
  nj=length(unique(CP$choice))
  ut = mat.or.vec(ni,nj)
  
  ut[,1] = param[nj]*CP[,3]
  for(j in c(2:nj)) {
    ut[,j] = param[j-1] + param[nj] * CP[,j+2]
  }
  
  
  prob= exp(ut)
  sprob=rowSums(prob)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  for (i in 1:ni) {
    probc[i] = prob[i,CP$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

  
library(nloptr)
npar=10
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar)
model1_result   = nloptr(start,eval_f=like_fun_2, lb=lower,ub=upper,
                          opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10,"maxeval"=10000),
                          CP=CP)

model1_result$solution



############
# Exercise 3
############

like_fun_3=function(param,CP){
  
  ni = nrow(CP)
  nj = length(unique(CP$choice))
  ut = mat.or.vec(ni,nj)
  
  
  intercept <- append(0,param[1:9])
  beta <- append(0,param[10:18])
  for (j in 1:nj){
    ut[,j] = CP$Income*beta[j]+intercept[j]
  }
  
  
  prob = exp(ut)
  sprob = rowSums(prob)
  prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  
  for (i in 1:ni){
    probc[i] = prob[i,CP$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  like = sum(log(probc))
  return(-like)
}

npar=18
lower  = rep(-5,npar)
upper  = rep(5,npar)
start  = runif(npar)

model2_result  = nloptr(start,eval_f=like_fun_3, lb=lower,ub=upper,
               opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
               CP=CPDM)

model2_result$solution


############
# Exercise 4
############


margeff_eval= function(param,data,model){   
  m = length(unique(data$choice))
  n = nrow(data)
  xbeta = rep(0,n)
  for (j in 1:m){
    xbeta = xbeta + data[, j+2] * param[j]
  }
  pr = ifelse(model=="computellh1",
              dnorm(mean(xbeta)),dlogis(mean(xbeta)))
  marg = param * pr
  return(marg)
}

heps = 0.00001
gprime = NULL
out = NULL
out_bench = margeff_eval(model1_result$solution,CP,"like_fun_2")


for (i in 2:length(model1_result$solution))
{
  parh = model1_result$solution
  parh[i] = model1_result$solution[i] + heps 
  res = margeff_eval(parh, CP,"like_fun_2")
}




############
# Exercise 5
############

#beta_f

like_fun_5=function(param,CP){
  
  ni = nrow(CP)
  nj = length(unique(CP$choice))
  ut = mat.or.vec(ni,nj)
  
  
  intercept <- append(0,param[1:9])
  beta <- append(0,param[10:18])
  alpha <- param[19]
  for (j in 1:nj){
    ut[,j] = CP$Income*beta[j]+intercept[j]+alpha * CP[,j+2]
  }
  
  
  prob = exp(ut)
  sprob = rowSums(prob)
  prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  
  for (i in 1:ni){
    probc[i] = prob[i,CP$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  like = sum(log(probc))
  return(-like)
}

npar=19
lower  = rep(-10,npar)
upper  = rep(5,npar)
start  = runif(npar)

model3_result  = nloptr(start,eval_f=like_fun_5, lb=lower,ub=upper,
                        opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
                        CP=CPDM)

model3_result$solution




#beta_r
#delete first choice

CPDM_temp <- subset(CPDM, choice != 1)

like_fun_6=function(param,CP){
  
  ni = nrow(CP)
  nj = length(unique(CP$choice))
  ut = mat.or.vec(ni,nj)
  
  
  intercept <- append(0,param[1:8])
  beta <- append(0,param[9:16])
  alpha <- param[17]
  for (j in 1:nj){
    ut[,j] = CP$Income*beta[j]+intercept[j]+alpha * CP[,j+3]
  }
  
  
  prob = exp(ut)
  sprob = rowSums(prob)
  prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  
  for (i in 1:ni){
    probc[i] = prob[i,CP$choice[i]-1]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  like = sum(log(probc))
  return(-like)
}

npar=17
lower  = rep(-10,npar)
upper  = rep(5,npar)
start  = runif(npar)

model4_result  = nloptr(start,eval_f=like_fun_6, lb=lower,ub=upper,
                        opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
                        CP=CPDM_temp)

model4_result$solution

#MTT


param <- model3_result$solution
param_f <- param[-c(1,10)]
param_r <- model4_result$solution

L_r = like_fun_6(param_f, CPDM_temp)

MTT = -2*(L_r-model4_result$objective)

MTT
