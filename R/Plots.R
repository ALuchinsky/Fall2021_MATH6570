#Simulations
#install.packages('tseries')
#install.packages("mvnTest")
library(mvtnorm)
library(DescTools)
library(truncgof)#KS test
library(tseries)#JB
library(Ecume)
library(dgof)#Cramer-von Mises

library(ggplot2)
library(dplyr)

mix.norm<-function(n,p,mu2,sigma2){
  N = n              
  
  
  U =runif(n)
  
  
  rand.samples = rep(NA,n)
  class=rep(NA,n)
  #Sampling from the mixture
  for(i in 1:n){
    if(U[i]<p){
      rand.samples[i] = rnorm(1,0,1)
      class[i]="1"
    }else{
      rand.samples[i] = rnorm(1,mu2,sigma2)
      class[i]="2"
    }
  }
  mlist=list("data"=rand.samples,"class"=class)
  return(mlist)
}






N=100000
data=replicate(n = N, 
               expr = mix.norm(100,0.1,0,1)$data )


#JB test
sigma2=c(1,2,3,4,6)
p=0.5
n=100

mu2=0


powerJB=c()
powerKS=c()
powerSW=c()
powerKSW1=c()

for (i in 1:length(sigma2)){
  data=replicate(n = N, 
                 expr = mix.norm(n,p,mu2,sigma2[i])$data )
  
  JB=apply(data, 2, jarque.bera.test)
  JB=data.frame(matrix(unlist(JB), nrow=length(JB), byrow=TRUE))
  pvalJB=JB$X3
  
  powerjb <- mean(pvalJB <= 0.5)
  
  powerJB[i]=powerjb
  
  class=data.frame(matrix(as.factor(replicate(n = N, 
                                              expr = mix.norm(n,p,0,sigma2[i])$class )),nrow=n,ncol=N))
  
  
  pvalKS=c()
  for(j in 1:N){
    DF1=cbind(data[,j],class[j])
    colnames(DF1)=c("d","c")
    x1=DF1[DF1$c==1,1]
    y1=DF1[DF1$c==2,1]
    if(length(x1)==0) x1=rnorm(100,mu2,sigma2[i])

    KS=dgof::ks.test(x1,y1,"pnorm",exact=TRUE)
    pvalKS[j]=KS$p.value
    
  }
  
  
  
  powerks <- mean(pvalKS <= 0.5)
  
  powerKS[i]=powerks
  
  SW=apply(data, 2, shapiro.test)
  SW=data.frame(matrix(unlist(SW), nrow=length(SW), byrow=TRUE))
  pvalSW=SW$X2
  
  powersw <- mean(pvalSW <= 0.5)
  
  powerSW[i]=powersw
  
  
  
  pvalKSW=c()
  for(j in 1:N){
    DF=cbind(data[,j],class[j])
    colnames(DF)=c("d","c")
    x=DF[DF$c==1,1]
    y=DF[DF$c==2,1]
    if(length(x)==0) x=rnorm(100,mu2,sigma2[i])
    
    KSW=ks_test(x,y)
    pvalKSW[j]=KSW$p.value
    
  }
  
  powerKSW <- mean(pvalKSW <= 0.5)
  print(powerKSW)
  
  powerKSW1[i]=powerKSW
  
  
  
  
}



dgf=data.frame(c(powerJB,powerKS,powerSW,powerKSW1))
dgf$Test=rep(c("JB","KS","SW","KSW"),each=5)
dgf$sigma=rep(sigma2,4)  
colnames(dgf)[1]="Power"

ggplot(dgf, aes(x=sigma, y=Power, group=Test)) +
  geom_line(aes(linetype=Test))+
  geom_point(aes(shape=Test))







#############################################################################3

