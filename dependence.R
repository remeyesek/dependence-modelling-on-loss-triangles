library(copula)
library(fitdistrplus)
library(ChainLadder)
library(actuar)


# Loss Triangle for LoB-1: LoB1losses
# Loss Triangle for LoB-2: LoB2losses

# Premiums for LoB-1: LoB1premium
# Premiums for LoB-2: LoB2premium

kstest_1=ks.test(LoB1losses, "plnorm", meanlog, sdlog)
kstest_2=ks.test(LoB2losses, "plnorm", meanlog, sdlog)
#Kolmogorov-Smirnov test 


LoB1fit=glm(formula = log(LoB1losses) ~ dsgn[, 2] + dsgn[, 3] +
           dsgn[, 4] + dsgn[, 5] + dsgn[, 6] +
           dsgn[, 7] + dsgn[, 8] + dsgn[, 9] +
           dsgn[, 10] + dsgn[, 11] + dsgn[, 12] + dsgn[, 13],
         family=gaussian(link=identity))
LoB2fit=glm(formula = log(LoB2losses) ~ dsgn[, 2] + dsgn[, 3] +
              dsgn[, 4] + dsgn[, 5] + dsgn[, 6] +
              dsgn[, 7] + dsgn[, 8] + dsgn[, 9] +
              dsgn[, 10] + dsgn[, 11] + dsgn[, 12] + dsgn[, 13],
            family=gaussian(link=log))
#parameter estimation with GLM 

copulamodel=frankCopula(dim = 2)
lossdata=pobs(cbind(LoB1losses, LoB2losses))
fitcop=fitCopula(copulamodel, lossdata, method = "ml")
#copula regression model for frank copula


cumLoB1=incr2cum(LoB1losses)
n=10
factor=sapply(1:(n-1), function(i){
              sum(cumLoB1[c(1:(n-i)),i+1])/sum(cumLoB1[c(1:(n-i)),i])})
predicted_triangle=loss_triangle
for (j in 1:(ncol(LoB1losses) - 1)) {
  for (i in (nrow(LoB1losses) - j + 1):nrow(LoB1losses)) {
    predicted_cumLoB1[i, j + 1]=cumLoB1[i, j] * factor[j]}}
#chain ladder method


sd=standard deviation
LoB1unobserved=matrix(nrow=21,ncol=1)
for(i in 1:21){
  Lob1unobserved[i,]=qlnorm(u1[i,],systLoB1[i,],sd)}

newsystcomp=systLoB1[i,]+LoB1unobserved[i,]
#systematic component for additive model

newsystcomp=systLoB2[i,]*LoB2unobserved[i,]

#systematic component for multiplicative model

cellerror_LoB1=sqrt( (sum((LoB1losses - LoB1losses_predicted)^2)) / (sum(LoB1losses_predicted^2)))
#cell error calculatiob for LoB1



#############################################################
######################################

copparam=copulaparameter
mycop=normalCopula(copparam,dim=2,dispstr="ex")
loglikCOPULA=function(thetarm,py,px,cy,cx,copula){
  copulaparam=thetarm[1]
  beta1=thetarm[2:20]
  beta2=thetarm[21:39]
  loglik1=loglikNORM(beta1,disp=2.434273,py=persyvariate,px=persxvariate)
  loglik2=loglikGAMMA(beta2,disp=0.7664748,cy=lnsiz,cx=comsxvariate)
  uu=cbind(loglik1[[2]],loglik2[[2]])
  personalloglik=loglik1[[1]]
  commercialloglik=loglik2[[1]]
  copula@parameters=copulaparam
  Loglik=personalloglik+commercialloglik+sum(log(dCopula(uu,copula=mycop)))
  return(-Loglik)
}
#############################################################
######################################



set.seed(123)
simulated_data <- rCopula(1000, copula_fit@copula)

cat("LoB1 Ortalama Rezerv Tahmini: ", mean_reserve_LoB1, "\n")
cat("LoB2 Ortalama Rezerv Tahmini: ", mean_reserve_LoB2, "\n")

LoB1CL <- MackChainLadder(LoB1_loss_triangle)
LoB2CL <- MackChainLadder(LoB2_loss_triangle)
#CL estimation

LoB1fixed=paramt[1]
kayiki=kpar[2]
kayuc=kpar[3]
kaydort=kpar[4]
kaybes=kpar[5]
kayalti=kpar[6]
kayyedi=kpar[7]
kdyiki=kpar[8]
kdyuc=kpar[9]
kdydort=kpar[10]
kdybes=kpar[11]
kdyalti=kpar[12]
kdyyedi=kpar[13]

systcomp1= fixed value+ accident year+ development year
#systematic components for LoB1
