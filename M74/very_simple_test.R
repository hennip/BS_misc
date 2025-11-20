dfFI
df2<-dfFI |> mutate(YSFM=YSFM/100)

ggplot(df2, aes(x=thiam, y=1-YSFM, col=YEAR))+
  geom_point()+
  geom_smooth()

ggplot(df2, aes(x=log(thiam), y=1-YSFM, col=YEAR))+
  geom_point()+
  geom_smooth(method =
                "loess"
  )    
 # geom_smooth()
# geom_smooth(method =
#               "gam"
#             , formula = y ~ s(x, bs =
#                                 "cs"
#             ))    

ggplot(df2, aes(x=log(thiam), y=1-YSFM, col=YEAR))+
  geom_point()

ggplot(df2, aes(x=thiam, y=1-YSFM, col=YEAR))+
  geom_point()


df3<-df2 |>  
  filter(YSFM>0 #& YSFM<1
         ) |> 
filter(thiam<1 & thiam >0.2)# & YSFM<1)

ggplot(df3, aes(x=log(thiam), y=1-YSFM, col=YEAR))+
  geom_point()





M3<-"
model{
for(i in 1:n){
  x[i]~dbin(p[i],Eggs[i])
  logit(p[i])<-P[i]
  P[i]~dnorm(mu[i],tau)
  mu[i]<-a+b*thiam_obs[i]
}
tau<-1/pow(sd,2)

a~dunif(-10,10)#dnorm(1,0.01)
b~dunif(0,20)#dlnorm(-3.4,0.43)
sd~dunif(0.001,5)#dlnorm(1,0.1)

aX~dunif(-10,10)#dnorm(1,0.01)
bX~dunif(0,10)#dlnorm(-3.4,0.43)
sdX~dunif(0.001,5)#dlnorm(1,0.1)
}"

cat(M3,file="prior-tiam.txt")

df4<-df3 |> filter(!is.na(thiam))
data<-list(thiam_obs=log(df4$thiam), x=df4$surv_eggs, Eggs=df4$eggs, n=length(df4$eggs))


var_names=c(
  "a","b", "sd",
  "aX","bX", "sdX"
)
run10 <- run.jags(M3,
                 monitor= var_names,data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)


summary(run10)

chains<-as.mcmc.list(run10)

a<-chains[,"a"][[1]]
b<-chains[,"b"][[1]]
sd<-chains[,"sd"][[1]]
sim<-length(chains[,"a"][[1]])

lt_seq<-seq(-3,3, by=0.1)

P<-surv2<-surv<-mu<-array(NA, dim=c(sim, length(lt_seq)))
for( i in 1: sim){
  for(j in 1:length(lt_seq)){
  mu[i,j]<-a[i]+b[i]*lt_seq[j]
  P[i,j]<-rnorm(1,mu[i,j],sd[i])
  }
  surv[i,]<-exp(mu[i,])/(1+exp(mu[i,]))
  surv2[i,]<-exp(P[i,])/(1+exp(P[i,]))
}


par(mfrow=c(1,2))
plot( log(df4$thiam), 1-df4$YSFM)
for(i in 1:sim){
lines(lt_seq, surv[i,], col=rgb(1,0,0,0.03))
}
plot( log(df4$thiam), 1-df4$YSFM)
for(i in 1:sim){
  lines(lt_seq, surv2[i,], col=rgb(1,0,0,0.03))
}

summary(run10)


par(mfrow=c(2,2))

plot(density(a))
lines(density(chains[,"aX"][[1]]))
plot(density(b))
lines(density(chains[,"bX"][[1]]))
plot(density(sd))
lines(density(chains[,"sdX"][[1]]))

