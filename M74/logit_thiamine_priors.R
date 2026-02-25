
log_thiam<-seq(from=-3, to=3, by=0.1)
N<-length(log_thiam)
a<-0
b<-2
sd<-0.01
surv_ysfm<-0.9
t1<--2  
mu<-muQ<-Q<-q<-c()
for(i in 1:N){
  muQ[i]<-a+b*log_thiam[i]
  Q[i]<-rnorm(1,muQ[i],sd)
  q[i]<-(exp(Q[i])/(1+exp(Q[i])))

  mu[i]<-
    if(log_thiam[i]<t1){
      0.001
    }else{
      q[i]*surv_ysfm
    }
  }
par(mfrow=c(1,2))
plot(log_thiam,q, main=c("a=",a,"b=",b),ylim=c(0,1))
plot(log_thiam,mu, main=c("a=",a,"b=",b),ylim=c(0,1))
q


M_priors<-"
model{
 
 for(i in 1:N){
 x[i]~dbin(p[i],Eggs[i])
 # p: survival probability
 p[i]~dbeta(ap[i], bp[i])T(0.01,0.99)

ap[i]<-mu[i]*eta
bp[i]<-(1-mu[i])*eta

# t1:n alapuolella ne joiden kuolleisuus on 100%
 mu[i]<-(1-step(thiam_obs[i]-t1))*0.001  + 
 
 # t1:n yläpuolella ne joiden kuolleisuus on<100%
 # surv_ysfm skaalaa logit-normaalin tavallisen ysfm:n tasolle silloin kun selviytyminen
 # olisi muuten 1. Muuta merkitystä tällä ei pitäisi olla, koska a ja b estimoituvat suhteessa
 # ko parametriin
 step(thiam_obs[i]-t1) *q[i]*surv_ysfm
 
logit(q[i])<-Q[i]
Q[i]~dnorm(muQ[i],tauQ)
muQ[i]<-aQ+bQ*thiam_obs[i]

} 

t1~dunif(-5,3)
t1X~dunif(-5,3)

aQ~dnorm(0,1)
bQ~dlnorm(log(2)-0.5*(sd_bQ*sd_bQ),1/(sd_bQ*sd_bQ))
sd_bQ<-1
sdQ<-0.01#~dlnorm(1,0.1)
tauQ<-1/pow(sdQ,2)

eta~dunif(0.01,1000)
surv_ysfm~dbeta(2,2)T(0.001,0.9999)


}"


#thiam<-seq(from=0.1, to=3, by=0.1)
log_thiam<-seq(from=-3, to=3, by=0.1)

data<-list(thiam_obs=log_thiam, N=length(log_thiam), Eggs=rep(100, length(log_thiam)))



var_names=c(
  "x","q","mu",
  "aQ", "bQ",
  "surv_ysfm",
  "t1", "t1X",
  "eta"
)


run0<- run.jags(M_priors,
                  monitor= var_names,data=data, #inits = inits,
                  n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                  modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                  progress.bar=TRUE)

chains<-as.mcmc(run0)

aQ<-chains[,"aQ"]
Nsample<-length(aQ)

mu<-x<-array(NA, dim=c(Nsample, N))
for(i in 1:N){
    x[,i]<-chains[,paste0("x[",i,"]")]
    mu[,i]<-chains[,paste0("mu[",i,"]")]
}
#x<-mean(chains[,"x[1]"])
plot(log_thiam, mu[1,])
for(i in 2:N){
points(log_thiam, mu[i,])
}
