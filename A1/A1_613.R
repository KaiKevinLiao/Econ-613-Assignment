### econ613_A1
###load data
datjss <- read_csv("Downloads/Econ613-master/Assignments/A1/dat/datjss.csv")
datsss <- read_csv("Downloads/Econ613-master/Assignments/A1/dat/datsss.csv")
datstu <- read_csv("Downloads/Econ613-master/Assignments/A1/dat/datstu.csv", col_types = cols(score = col_double(), rankplace = col_double()))

#### Exercise 1  Missing data
###number of students
num_stu<-length(datstu$score)
###number of schools
A1_coef_sc<-c("schoolcode1","schoolcode2","schoolcode3","schoolcode4","schoolcode5","schoolcode6")
A1_sc<-as.matrix(datstu[,A1_coef_sc])
A1_sc_vec<-as.vector(A1_sc)
length(A1_sc_vec) ####2044938
A1_sc_vec<-na.omit(A1_sc_vec)
length(A1_sc_vec) ####2009844
A1_sc_unique<-unique(A1_sc_vec)
length(A1_sc_unique) ###640
####number of programs
A1_coef_pgm<-c("choicepgm1","choicepgm2","choicepgm3","choicepgm4","choicepgm5","choicepgm6")
A1_pgm<-as.matrix(datstu[,A1_coef_pgm])
A1_pgm_vec<-as.vector(A1_pgm)
length(A1_pgm_vec) ####2044938
A1_pgm_vec<-na.omit(A1_pgm_vec)
length(A1_pgm_vec) ####2006484
A1_pgm_unique<-unique(A1_pgm_vec)
length(A1_pgm_unique) ###32
####number of choices
A1_cho1<-datstu[,c("schoolcode1","choicepgm1")]
colnames(A1_cho1)<-c("schoolcode","choicepgm")
A1_cho2<-datstu[,c("schoolcode2","choicepgm2")]
colnames(A1_cho2)<-c("schoolcode","choicepgm")
A1_cho3<-datstu[,c("schoolcode3","choicepgm3")]
colnames(A1_cho3)<-c("schoolcode","choicepgm")
A1_cho4<-datstu[,c("schoolcode4","choicepgm4")]
colnames(A1_cho4)<-c("schoolcode","choicepgm")
A1_cho5<-datstu[,c("schoolcode5","choicepgm5")]
colnames(A1_cho5)<-c("schoolcode","choicepgm")
A1_cho6<-datstu[,c("schoolcode6","choicepgm6")]
colnames(A1_cho6)<-c("schoolcode","choicepgm")
A1_cho<-rbind(A1_cho1,A1_cho2,A1_cho3,A1_cho4,A1_cho5,A1_cho6)
A1_cho_com<-A1_cho[complete.cases(A1_cho),] 
length(A1_cho_com$schoolcode) ####2006470
A1_cho_unique<-unique(A1_cho_com[,c("schoolcode","choicepgm")])
length(A1_cho_unique$schoolcode) ####2773
###the number of students without test scores
length(as.matrix(datstu[is.na(datstu$score),"score"])) #### 179887
length(as.matrix(datstu[!is.na(datstu$score),"score"])) ####160936
####the number of students applying to the same school
A1_sc<-datstu[,A1_coef_sc]
A1_sc$Count <- apply(A1_sc, 1, function(x) length(unique(x)))
length(as.matrix(A1_sc[A1_sc$Count<6,"Count"])) ####133668
#####the number of students applying to less than 6 choices
length(as.matrix(datstu[complete.cases(A1_pgm),"score"])) ####319835
length(as.matrix(datstu[!complete.cases(A1_pgm),"score"])) ####20988
summary(complete.cases(A1_pgm))
###Mode   FALSE    TRUE 
###logical   20988  319835 



####Exercise 2
###get geo-location information
###get a school level dataset including location information
A1_datsss_unique<-unique(datsss[,c("schoolcode","sssdistrict","ssslong","ssslat")])
A1_datsss_u<-A1_datsss_unique[complete.cases(A1_datsss_unique),]
check_duplicated<-A1_datsss_u[duplicated(A1_datsss_u$schoolcode),] ###no duplicates. each row corresponds to one school code
A1_school<-merge(A1_cho_unique,A1_datsss_u)
###get score information
###get placements
datstu$rankplace<-as.numeric(datstu$rankplace)
datstu[which(datstu$rankplace<=6&is.na(datstu$rankplace)==0),"rank_y"]<-1
datstu[which(is.na(datstu$rank_y)),"rank_y"]<-0
####drop rows with missing rankplace or rankplace bigger than 6
datstu_sub<-datstu[which(datstu$rankplace<=6&is.na(datstu$rankplace)==0),]
###get placements
for (a in 1:length(as.matrix(datstu_sub$rankplace))) {
datstu_sub$seniorsc[a]<-unlist(datstu_sub[a,as.numeric(datstu_sub$rankplace[a])+4])
datstu_sub$seniorpgm[a]<-unlist(datstu_sub[a,as.numeric(datstu_sub$rankplace[a])+10])}
####cutoff, quality and size
library(dplyr)
rm(A1_sc_e2)
A1_sc_e2<-ddply(datstu_sub,c("seniorsc","seniorpgm"),summarize, cutoff= min(score),quality=mean(score),size=length(score))
####merge
A1_school<-merge(A1_school,A1_sc_e2,by.x=c("schoolcode","choicepgm"),by.y=c("seniorsc","seniorpgm"),all.x=TRUE) ###it seems we could not get cutoff for some schools



####Excercise 3
A1_distance<-function(M)
{
  x=M[1]
  y=M[2]
  w=M[3]
  z=M[4]
  dist=sqrt((69.172*(x-w)*cos(z/57.3))^2+(69.172*(w-z))^2)
  return(dist)
}
A1_school1<-A1_school
colnames(A1_school1)<-c("schoolcode1","choicepgm1","sssdistrict1","ssslong1","ssslat1","cutoff1","quality1","size1")
A1_school2<-A1_school
colnames(A1_school2)<-c("schoolcode2","choicepgm2","sssdistrict2","ssslong2","ssslat2","cutoff2","quality2","size2")
A1_school3<-A1_school
colnames(A1_school3)<-c("schoolcode3","choicepgm3","sssdistrict3","ssslong3","ssslat3","cutoff3","quality3","size3")
A1_school4<-A1_school
colnames(A1_school4)<-c("schoolcode4","choicepgm4","sssdistrict4","ssslong4","ssslat4","cutoff4","quality4","size4")
A1_school5<-A1_school
colnames(A1_school5)<-c("schoolcode5","choicepgm5","sssdistrict5","ssslong5","ssslat5","cutoff5","quality5","size5")
A1_school6<-A1_school
colnames(A1_school6)<-c("schoolcode6","choicepgm6","sssdistrict6","ssslong6","ssslat6","cutoff6","quality6","size6")
datstu<-merge(datstu,A1_school1,all.x=TRUE)
datstu<-merge(datstu,A1_school2,all.x=TRUE)
datstu<-merge(datstu,A1_school3,all.x=TRUE)
datstu<-merge(datstu,A1_school4,all.x=TRUE)
datstu<-merge(datstu,A1_school5,all.x=TRUE)
datstu<-merge(datstu,A1_school6,all.x=TRUE)
datstu<-merge(datstu,datjss[,2:4],by="jssdistrict",all.x=TRUE)
###compute the distance between the junior high school and each applied senior high school for each student
####drop students with missing junior high school
A1_datstu<-datstu[which(complete.cases(datstu[,c("point_x","point_y")])),]
datstu$dist1<-apply(datstu[,c("ssslong1","ssslat1","point_x","point_y")],1,A1_distance)
datstu$dist2<-apply(datstu[,c("ssslong2","ssslat2","point_x","point_y")],1,A1_distance)
datstu$dist3<-apply(datstu[,c("ssslong3","ssslat3","point_x","point_y")],1,A1_distance)
datstu$dist4<-apply(datstu[,c("ssslong4","ssslat4","point_x","point_y")],1,A1_distance)
datstu$dist5<-apply(datstu[,c("ssslong5","ssslat5","point_x","point_y")],1,A1_distance)
datstu$dist6<-apply(datstu[,c("ssslong6","ssslat6","point_x","point_y")],1,A1_distance)

###Exercise 4
###first ranked choice
A1_e4<-sapply(datstu[,c("cutoff1","quality1","dist1","cutoff2","quality2","dist2","cutoff3","quality3","dist3","cutoff4","quality4","dist4","cutoff5","quality5","dist5","cutoff6","quality6","dist6")], function(x) c( "Stand dev" = sd(x,na.rm=TRUE), 
                         "Mean"= mean(x,na.rm=TRUE)))
library(gtools)
datstu$quantcut<-quantcut(datstu$score, q=4, na.rm=TRUE)                                                                                                                                                                                                                                                                                                                                                                                                                                                            
library(dplyr)
A1_e4_q<-datstu%>%
  group_by(quantcut) %>%
  summarise_at(.vars = c("cutoff1","quality1","dist1","cutoff2","quality2","dist2","cutoff3","quality3","dist3","cutoff4","quality4","dist4","cutoff5","quality5","dist5","cutoff6","quality6","dist6"),.fun=list(mean=mean,sd=sd),na.rm=TRUE) 



####Exercise 5
set.seed(2333)
A1_e5<-data.frame(matrix(ncol = 6, nrow = 10000))
colnames(A1_e5)<-c("X1","X2","X3","error","Y","ydum")
A1_e5$X1<-runif(10000,1,3)
A1_e5$X2 = rgamma(10000,shape=3,scale=2)
A1_e5$X3 = rbinom(10000,1,0.3)
A1_e5$error = rnorm(10000,2,1)
A1_e5$Y = 0.5 + 1.2*A1_e5$X1 - 0.9*A1_e5$X2 +0.1*A1_e5$X3 + A1_e5$error
A1_e5[which(A1_e5$Y>mean(A1_e5$Y)),"ydum"]<-1
A1_e5[which(is.na(A1_e5$ydum)),"ydum"]<-0

###Exercise 6 OLS
cor(A1_e5$Y,A1_e5$X1) #0.1985085
A1_e5$X0<-1
A1_X<-as.matrix(A1_e5[,c(7,1:3)])
A1_Y<-as.matrix(A1_e5[,6])
A1_e5_betas <- solve(t(A1_X) %*% A1_X) %*% t(A1_X) %*% A1_Y
A1_residuals <- A1_Y - A1_X %*% A1_e5_betas
A1_n<-nrow(A1_X)
A1_p <- ncol(A1_X) - 1
A1_residual_var <- t(A1_residuals) %*% A1_residuals / (A1_n - A1_p - 1)
A1_residual_var <- as.numeric(A1_residual_var)
A1_beta_covar <- A1_residual_var * solve(t(A1_X) %*% A1_X)
A1_beta_SE <- data.frame(sqrt(diag(A1_beta_covar)))
# X1           X2           X3           X0 
#0.0057361337 0.0009591193 0.0072519788 0.0134366632 
colnames(A1_beta_SE)<-"SE"
A1_e5_result<-cbind(A1_e5_betas,A1_beta_SE)
colnames(A1_e5_result)[1]<-"betas"
###check
summary(lm(A1_Y~A1_X))

####Exercise 7 Discrete choice
A1_ydum<-A1_e5$ydum
# Probit
flike = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}
set.seed(6666)
start = runif(4)
A1_e5_prob = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=2000),x1=A1_X[,2],x2=A1_X[,3],x3=A1_X[,4],yvar=A1_ydum,hessian=TRUE)
A1_fisher_info = solve(A1_e5_prob$hessian)
A1_prop_sigma  = sqrt(diag(A1_fisher_info))
A1_prop_sigma
A1_glm_p<-glm(A1_ydum~A1_X[,2:4],family = binomial(link = "probit"))
A1_prob_est = cbind(summary(A1_glm_p)$coefficients[,1],summary(A1_glm_p)$coefficients[,2], A1_e5_prob$par,A1_prop_sigma)
colnames(A1_prob_est) = c("glm : est","glm :se","own : est","own :se")
A1_prob_est
# Logit
flike2 = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = exp(xbeta)/(1+exp(xbeta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}
start = runif(4)
A1_e5_logit = optim(start,fn=flike2,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=A1_X[,2],x2=A1_X[,3],x3=A1_X[,4],yvar=A1_ydum,hessian=TRUE)
A1_fisher_info2 = solve(A1_e5_logit$hessian)
A1_logit_sigma  = sqrt(diag(A1_fisher_info2))
A1_logit_sigma
A1_glm_l<-glm(A1_ydum~A1_X[,2:4],family = binomial(link = "logit"))
A1_logit_est = cbind(summary(A1_glm_l)$coefficients[,1],summary(A1_glm_l)$coefficients[,2], A1_e5_logit$par,A1_logit_sigma)
colnames(A1_logit_est) = c("glm : est","glm :se","own : est","own :se")
A1_logit_est
# linear probability
A1_lp_betas <- solve(t(A1_X) %*% A1_X) %*% t(A1_X) %*% A1_ydum
A1_lp_residuals <- A1_ydum - A1_X %*% A1_lp_betas
A1_n<-nrow(A1_X)
A1_p <- ncol(A1_X) - 1
A1_lp_residual_var <- t(A1_lp_residuals) %*% A1_lp_residuals / (A1_n - A1_p - 1)
A1_lp_residual_var <- as.numeric(A1_lp_residual_var)
A1_lp_beta_covar <- A1_lp_residual_var * solve(t(A1_X) %*% A1_X)
A1_lp_beta_SE <- data.frame(sqrt(diag(A1_lp_beta_covar)))
colnames(A1_lp_beta_SE)<-"SE"
A1_lp_result<-cbind(A1_lp_betas,A1_lp_beta_SE)
A1_lp_result
colnames(A1_lp_result)[1]<-"betas"
###check
A1_lp<-lm(A1_ydum~A1_X[,2:4])
A1_lp_est = cbind(summary(A1_lp)$coefficients[,1],summary(A1_lp)$coefficients[,2], A1_lp_result)
colnames(A1_lp_est) = c("lm : est","lm :se","own : est","own :se")
A1_lp_est


#####Exercise 8 Marginal Effects
x1=A1_X[,2]
x2=A1_X[,3]
x3=A1_X[,4]
yvar=A1_ydum
###probit
mefun <- function(x1,x2,x3,yvar){
  start = runif(4)
  probit = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=2000),x1=x1,x2=x2,x3=x3,yvar=yvar,hessian=TRUE)
  # get marginal effects
  pdf <- dnorm(A1_X %*% probit$par)
  marginal.effects <- pdf%*%probit$par
  me_mean<-colMeans(marginal.effects)
  return(me_mean)}
A1_probit_me<-mefun(x1,x2,x3,yvar)
###logit
mefun2 <- function(x1,x2,x3,yvar){
  start = runif(4)
  logit = optim(start,fn=flike2,method="BFGS",control=list(trace=6,maxit=2000),x1=x1,x2=x2,x3=x3,yvar=yvar,hessian=TRUE)
  # get marginal effects
  pdf <- dlogis(A1_X %*% logit$par)
  marginal.effects <- pdf%*%logit$par
  me_mean<-colMeans(marginal.effects)
  return(me_mean)}
A1_logit_me<-mefun2(x1,x2,x3,yvar)
# start bootstrap
A1_nind = nrow(A1_X);  
A1_pb_me_bs<-data.frame(matrix(ncol = 4, nrow = 100))
A1_lt_me_bs<-data.frame(matrix(ncol = 4, nrow = 100))
  for(i in 1:100){
    A1_samp= sample(1:A1_nind,A1_nind,rep=TRUE)
    A1_X2<-A1_X[A1_samp,]
    A1_ydum2<-A1_ydum[A1_samp]
    x1=A1_X2[,2]
    x2=A1_X2[,3]
    x3=A1_X2[,4]
    yvar=A1_ydum2
    A1_pb_me_bs[i,]<-mefun(x1,x2,x3,yvar)
    A1_lt_me_bs[i,]<-mefun2(x1,x2,x3,yvar)
  }
A1_pb_me_sd<-sapply(A1_pb_me_bs,function(x) c( "Stand dev" = sd(x,na.rm=TRUE), 
                                              "Mean"= mean(x,na.rm=TRUE)))
A1_lt_me_sd<-sapply(A1_lt_me_bs,function(x) c( "Stand dev" = sd(x,na.rm=TRUE), 
                                               "Mean"= mean(x,na.rm=TRUE)))
colnames(A1_pb_me_sd)<-c("X0","X1","X2","X3")
colnames(A1_lt_me_sd)<-c("X0","X1","X2","X3")









#####other code
for (a in 1:length(as.matrix(datstu$rankplace))) {
  if (is.na(datstu[a,"rankplace"])==0 & as.numeric(datstu[a,"rankplace"])<=6) {
    datstu$seniorsc[a]<-datstu[a,as.numeric(datstu$rankplace[a])+4]
    datstu$seniorpgm[a]<-datstu[a,as.numeric(datstu$rankplace[a])+10]
  }
  else{
    datstu$seniorsc[a]<-NA
    datstu$seniorpgm[a]<-NA
  }
}
datstu<-datstu[,1:19]
function_place <- function(X)
{
  a=X[18]
  result1=X[a+4]
  return(result1)
}
datstu_sub$seniorsc<- apply(as.matrix(datstu_sub), 1, function_place)
datstu_sub<-datstu_sub[,1:19]


