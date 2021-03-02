stu <- read.csv(file = "C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\datstu.csv")
sch <- read.csv(file = "C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\datsss.csv")
jss <- read.csv(file = "C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\datjss.csv")


############
# Exercise 1
############

# number of students
dim(stu)[1]

# number of schools
length(unique(na.omit(sch$schoolcode)))

# Number of programs
temp1 <- unique(stu$choicepgm1)
temp2 <- unique(stu$choicepgm2)
temp3 <- unique(stu$choicepgm3)
temp4 <- unique(stu$choicepgm4)
temp5 <- unique(stu$choicepgm5)
temp6 <- unique(stu$choicepgm6)
temp7 <- unique(c(temp1, temp2, temp3, temp4, temp5, temp6))
programs <- temp7[temp7 != ""]
length(programs)

# Number of choice
stu$choice1 <- paste(stu$choicepgm1, stu$schoolcode1, sep = "")
stu$choice2 <- paste(stu$choicepgm2, stu$schoolcode2, sep = "")
stu$choice3 <- paste(stu$choicepgm3, stu$schoolcode3, sep = "")
stu$choice4 <- paste(stu$choicepgm4, stu$schoolcode4, sep = "")
stu$choice5 <- paste(stu$choicepgm5, stu$schoolcode3, sep = "")
stu$choice6 <- paste(stu$choicepgm6, stu$schoolcode4, sep = "")

temp1 <- vector()
temp2 <- vector()
temp3 <- vector()
temp4 <- vector()
temp5 <- vector()
temp6 <- vector()
temp7 <- vector()

temp1 <- unique(stu$choice1)
temp2 <- unique(stu$choice2)
temp3 <- unique(stu$choice3)
temp4 <- unique(stu$choice4)
temp5 <- unique(stu$choice5)
temp6 <- unique(stu$choice6)
temp7 <- unique(c(temp1, temp2, temp3, temp4, temp5, temp6))
choices <- temp5[temp5 != ""]
length(choices)


# Number of empty score
sum(is.na(stu$score))

# Number of students applying to the same school
var_sc<-c("schoolcode1","schoolcode2","schoolcode3","schoolcode4","schoolcode5","schoolcode6")
sc <- stu[,var_sc]
sc$applynum <- apply(sc, 1, function(x) (length(unique(na.omit(x)))+sum(is.na(x))))
nrow(sc[sc$applynum != 6, ])

# Apply to less than 6 choices


nrow(sc)-nrow(sc_complete)



############
# Exercise 2
############

# the district where the school is located
# the latitude of the district
# the longitude of the district
rm(newss)
var_school <- c("schoolcode","sssdistrict","ssslong","ssslat")
newss <- unique(sch[,var_school])

# cutoff (the lowest score to be admitted)
# quality (the average score of the students admitted)
# size (number of students admitted)

# Step 1: Get the school and program that a student was placed

stu$rankplace <- as.numeric(stu$rankplace)
stu_temp <- stu[which(stu$rankplace <= 6 & stu$rankplace >=1), ]

for (i in 1: nrow(stu_temp)) {
  stu_temp$schoolplaced[i] <- unlist(stu_temp[i,as.numeric(stu_temp$rankplace[i])+4])
  stu_temp$programplaced[i] <- unlist(stu_temp[i,as.numeric(stu_temp$rankplace[i])+10])
}

new_temp <- stu_temp[, c("schoolplaced","programplaced")]
new_temp1 <- unique(new_temp[,1:2])

# Step 2: Write the results

for (i in 1: nrow(new_temp1)) {
  school_temp = new_temp1$schoolplaced[i]
  program_temp = new_temp1$programplaced[i]
  stu_sch <- stu_temp[which(stu_temp$schoolplaced == school_temp & stu_temp$programplaced == program_temp),]
  new_temp1$cutoff[i] = min(stu_sch[,"score"])
  new_temp1$quality[i] = mean(stu_sch[,"score"])
  new_temp1$size[i] = nrow(stu_sch)
}

# Step 3: Merge two dataset
Output2 <- merge(newss,new_temp1,by.x=c("schoolcode"),by.y=c("schoolplaced"),all.x=TRUE)
Output2_final <- Output2[!is.na(Output2$size),]

write.csv(Output2_final,'C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\Exercise2.csv')

############
# Exercise 3
############

# define function for distance calculation

dist_calculator <- function(jss_x, jss_y, sss_x, sss_y){
  return(sqrt((69.172*(sss_x - jss_x)*cos(jss_y/57.3))^2 + (69.172*(sss_y-jss_y))^2 ))
}

distsss_temp <- unique(sch[,var_school])
distsss <- distsss_temp[complete.cases(distsss_temp), ]

rm(distance)
distance <- read.csv(file = "C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\distance.csv")


for (i in 1: nrow(jss)) {
  for (j in 1: nrow(distsss)){
    jssdistrict_temp <- jss$jssdistrict[i]
    jssdistrict_temp
    sssdistrict_temp <- distsss$sssdistrict[j]
    ssschoolcode_temp <- distsss$schoolcode[j]
    jss_x = jss$point_x[i]
    jss_y = jss$point_y[i]
    sss_x = distsss$ssslong[j]
    sss_y = distsss$ssslat[j]
    schooldistance_temp <- dist_calculator(jss_x, jss_y, sss_x, sss_y)
    schooldistance_temp
    distance[nrow(distance)+1,] = c(jssdistrict_temp,sssdistrict_temp,ssschoolcode_temp,schooldistance_temp)
  }
}
k = 1
for (i in 1: nrow(jss)) {
  for (j in 1: nrow(distsss)){
    sssdistrict_temp <- distsss$sssdistrict[j]
    distance[k,"sssdistrict"] = sssdistrict_temp
    k  = k+1
  }
}
write.csv(distance,'C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\Exercise3.csv')

############
# Exercise 4
############

for (i in 1:6) {
  rank_i<-stu[which(stu$rankplace==i),]
  cutoff<-min(rank_i[,"score"])
  quality<-mean(rank_i[,"score"])
  sd_quality<-sd(rank_i[,"score"])
  cat(i,cutoff,quality,sd_quality,"\n")
}

############
# Exercise 5
############
set.seed(100)

x1 <- runif(10000,1,3)
x2 <- rgamma(10000,shape = 3,scale = 2)
x3 <- rbinom(10000,1,0.3)
epsilon <- rnorm(10000,2,1)
y <- 0.5+1.2*x1-0.9*x2+0.1*x3+epsilon
meany <- mean(y)
ydum <- matrix(0,1,10000)
ydum[y>meany] <- 1

############
# Exercise 6
############
corr <- cor(y,x1)
corr
x <- cbind(1,x1,x2,x3)
ix <- t(x)
#beta
beta <- solve(t(x)%*%x)%*%t(x)%*%y
error <- y-x%*%beta
var_e <- t(error)%*%error/(1000-3-1)
var_e <- as.numeric(var_e)
k <- solve(t(x)%*%x)
var_b <- k*var_e
param <- cbind(beta,diag(sqrt(var_b)))
colnames(param) <- c("beta", "std")
write.csv(param,'C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\Exercise6.csv')

############
# Exercise 7
############

# Probit
## Solution 1 (Only use optimization package)
### Likelihood function
like_func_probit = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}
### Run Probit
set.seed(100)
start = runif(4)
prob = optim(start,fn=like_func_probit,method="BFGS",control=list(trace=6,REPORT=1,maxit=2000)
             ,x1=x1,x2=x2,x3=x3,yvar=ydum,hessian=TRUE)
fisher= solve(prob$hessian)
sigma  = sqrt(diag(fisher))
prob
param_probit <- cbind(prob$par,sigma)
colnames(param_probit) <- c("beta", "std")
write.csv(param_probit,'C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\Exercise7_probit.csv')

## Solution 2
### Use glm package directly
ydum<-as.numeric(ydum)
probit_result<-glm(ydum~x1+x2+x3,family=binomial(link=probit))
probit_result

# Logit
like_func_logit = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = exp(xbeta)/(1+exp(xbeta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}

logit = optim(start,fn=like_func_logit,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000)
                    ,x1=x1,x2=x2,x3=x3,yvar=ydum,hessian=TRUE)
fisher2 = solve(logit$hessian)
sigma2  = sqrt(diag(fisher2))
param_logit <- cbind(logit$par,sigma)
colnames(param_logit) <- c("beta", "std")
write.csv(param_logit,'C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\Exercise7_logit.csv')


logit_check<-glm(ydum~x1+x2+x3,family=binomial(link=logit),x=TRUE)
logit_check

# Linear Probability
linear<-lm(ydum~x)
write.csv(linear$coefficients,'C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\Exercise7_linear.csv')


############
# Exercise 8
############

# Define Functions

probit_func <- function(x1,x2,x3,ydum){
  start = runif(4)
  probit = optim(start,fn=like_func_probit,method="BFGS",control=list(trace=6,maxit=2000)
                 ,x1=x1,x2=x2,x3=x3,yvar=ydum,hessian=TRUE)
  pdf <- dnorm(A1_X %*% probit$par)
  marginal.effects <- pdf%*%probit$par
  me_mean<-colMeans(marginal.effects)
  return(me_mean)}

logit_func <- function(x1,x2,x3,ydum){
  start = runif(4)
  logit = optim(start,fn=like_func_logit,method="BFGS",control=list(trace=6,maxit=2000)
                ,x1=x1,x2=x2,x3=x3,yvar=ydum,hessian=TRUE)
  pdf <- dlogis(A1_X %*% logit$par)
  marginal.effects <- pdf%*%logit$par
  me_mean<-colMeans(marginal.effects)
  return(me_mean)}

# Bootstrapping
N = length(x1)
pb_me_bs <- data.frame(matrix(ncol = 4, nrow = 100))
lt_me_bs <- data.frame(matrix(ncol = 4, nrow = 100))
#X <- data.frame(matrix(ncol = 4, nrow = 10000))
#colnames(X)<-c("constant","X1","X2","X3")
#X$X1 <- runif(10000,1,3)
#X$X2 <- rgamma(10000,shape=3,scale=2)
#X$X3 <- rbinom(10000,1,0.3)
for(i in 1:100){
  samp <- sample(1:N,N,rep=TRUE)
  X2 <- x[A1_samp,]
  ydum_temp <- ydum[samp]
  x1=X2[,2]
  x2=X2[,3]
  x3=X2[,4]
  pb_me_bs[i,]<-probit_func(x1,x2,x3,ydum_temp)
  lt_me_bs[i,]<-logit_func(x1,x2,x3,ydum_temp)
}
pb_me_sd<-sapply(pb_me_bs,function(x) c( "Stand dev" = sd(x,na.rm=TRUE), 
                                               "Mean"= mean(x,na.rm=TRUE)))
lt_me_sd<-sapply(lt_me_bs,function(x) c( "Stand dev" = sd(x,na.rm=TRUE), 
                                               "Mean"= mean(x,na.rm=TRUE)))
colnames(pb_me_sd)<-c("X0","X1","X2","X3")
colnames(lt_me_sd)<-c("X0","X1","X2","X3")
write.csv(pb_me_sd,'C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\Exercise8_probit.csv')
write.csv(lt_me_sd,'C:\\Users\\kaike\\OneDrive\\Desktop\\Applied Econometrics\\A1\\dat\\Exercise8_logit.csv')
