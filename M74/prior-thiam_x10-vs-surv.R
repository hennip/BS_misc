
boxplot.df<-function(mcmc.chains, name, X){ # chain object, variable name, values to x-axis
  # note: length of x and dim variable need to match
  Q5<-c();Q25<-c();Q50<-c();Q75<-c();Q95<-c()
  n<-length(X)
  
  for(i in 1:n){
    y <- as.mcmc(mcmc.chains[,paste(sep="",name,"[",i,"]")][[1]])
    tmp<-summary(y,quantiles=c(0.05,0.25,0.5,0.75,0.95))
    Q5[i] = tmp$quantiles[1]
    Q25[i] = tmp$quantiles[2]
    Q50[i] = tmp$quantiles[3]
    Q75[i] = tmp$quantiles[4]
    Q95[i] = tmp$quantiles[5]
  }
  
  df<-data.frame(
    x<-X,
    q5=Q5,
    q25=Q25,
    q50=Q50,
    q75=Q75,
    q95=Q95
  )
  colnames(df)<-c("x","q5","q25","q50","q75","q95")
  return(df)
}


tiam<-seq(0,30, by=.1)
n<-length(tiam)

a<--5
b<-1
mu<-c();P<-c();p<-c();sd<-c()
sd<-1
for(i in 1:n){
  mu[i]<-a+b*tiam[i]
  
  P[i]<-rnorm(1,mu[i],sd)
  p[i]<-(exp(P[i])/(1+exp(P[i])))
}

df<-as_tibble(cbind(tiam,p))

ggplot(df) +
  geom_point(aes(tiam, p))+
  coord_cartesian(ylim=c(0,1))+
  labs(title=paste(sep="","a=",a," b=",b))+
  geom_vline(xintercept=0)

# Sovitetaan P:t edellisestä ja estimoidaan a, b ja sd


M2<-"
model{
for(i in 1:n){

P[i]~dnorm(mu[i],tau)
mu[i]<-a+b*tiam[i]
}
tau<-1/pow(sd,2)

a~dunif(-10,10)#dnorm(1,0.01)
b~dunif(0,5)#dlnorm(-3.4,0.43)
sd~dunif(0.001,5)#dlnorm(1,0.1)

aX~dunif(0,10)#dnorm(1,0.01)
bX~dunif(0,10)#dlnorm(-3.4,0.43)
sdX~dunif(0.001,5)#dlnorm(1,0.1)
}"

cat(M2,file="prior-tiam.txt")

data<-list(tiam=tiam,P=P, n=n)

system.time(jm<-jags.model('prior-tiam.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    #"p",
                                    "a","b", "sd",
                                    "aX","bX", "sdX"
                                  ),
                                  n.iter=5000,
                                  thin=1))

summary(chains1)
chainsM<-chains1


# Sitten katsotaan millaista matskua saadut priorit tuottaisivat

# odotusarvot ja hajonnat edellisesta ajosta
mua<-summary(chainsM[,"a"])$statistics[1]
sda<-summary(chainsM[,"a"])$statistics[2]
taua<-1/(sda*sda)
mua;taua

mub<-summary(chainsM[,"b"])$statistics[1]
sdb<-summary(chainsM[,"b"])$statistics[2]
cvb<-sdb/mub
taub<-1/log(cvb*cvb+1)
Mb<-log(mub)-0.5/taub
Mb;taub

musd<-summary(chainsM[,"sd"])$statistics[1]
sdsd<-summary(chainsM[,"sd"])$statistics[2]
cvsd<-sdsd/musd
tausd<-1/log(cvsd*cvsd+1)
Msd<-log(musd)-0.5/tausd
Msd;tausd


M2<-"
model{
for(i in 1:n){
logit(p[i])<-P[i]
P[i]~dnorm(mu[i],tau)
mu[i]<-a+b*tiam[i]
}
tau<-1/pow(sd,2)
# # 
#    a~dnorm(mu.a,t.a)
#    b~dlnorm(M.b,T.b)
#    sd~dlnorm(M.sd,T.sd)
   
# a~dnorm(-5,10)
# b~dlnorm(0.001,100)
# sd~dlnorm(-0.04,10)
 a~dnorm(-5,0.1)
 b~dlnorm(0.001,5)
 sd~dlnorm(-0.04,10)


}"

#cat(M2,file="prior-obs.txt")

data<-list( 
 # mu.a=mua,t.a=taua, 
 # M.b=Mb, T.b=taub, 
 # M.sd=Msd, T.sd=tausd,
  tiam=tiam, n=n
)

 run.jags(M2,
          monitor= c("p", "sd", "a", "b"),data=data, #inits = inits,
          n.chains = 2, method = 'parallel', thin=10, burnin =1000,
          modules = "mix",keep.jags.files=F,sample =5000, adapt = 1000,
          progress.bar=TRUE)


chains1<-run.jags(M2,monitor=c("p", "sd", "a", "b"),
                  n.iter=5000,thin=1,
                         adapt=1000,data=data,n.chains=2)



system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "p", "sd", "a", "b"
                                  ),
                                  n.iter=5000,
                                  thin=1))

summary(chains1[,"a"])
summary(chains1[,"b"])
summary(chains1[,"sd"])



df<-boxplot.df(chains1,"p",tiam)
df<-as_tibble(df)
df<-filter(df, x>0)

ggplot(df, aes(x, group=x))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  coord_cartesian(ylim=c(0,1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  labs(title=str_c("M74 survival, b_t=",b))+
  xlab("Thiamine nmol/g")+
  geom_vline(xintercept=1)

filter(df, x==0.2 | x==0.4 |x==0.6 |x==0.8|x==1|x==1.6|x==2)



