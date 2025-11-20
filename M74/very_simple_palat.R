dfFI
df2<-dfFI |> mutate(YSFM=YSFM/100)

ggplot(df2, aes(x=thiam, y=1-YSFM, col=YEAR))+
  geom_point()+
  geom_smooth()

ggplot(df2, aes(x=log(thiam), y=1-YSFM, col=YEAR))+
  geom_point()+
  # geom_smooth(method =
  #               "loess"
  # )    
  geom_smooth()
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

ggplot(df2, aes(x=log(thiam), y=1-YSFM, col=YEAR))+
  geom_point()





M3<-"
model{
for(i in 1:n){
 #x[i]~dlnorm(M[i], T)
 #M[i]<-log(mu[i])-0.5/T
 
 x[i]~dbin(p[i],Eggs[i])
 x_rep[i]~dbin(p[i],Eggs[i])
 logit(p[i])<-P[i]
  P[i]~dnorm(mu[i],tau)
 
 mu[i]<-(1-step(thiam_obs[i]-t1))*0  + 
 step(thiam_obs[i]-t1)* (1-step(thiam_obs[i]-t2)) * (a+b*thiam_obs[i]) + 
 step(thiam_obs[i]-t2)* ysfm 

#t1: tiamiini, jonka alapuolella selviytyminen on 0
#t2: tiamiini, jonka yläpuolella selviytyminen on tavallinen ysfm
# näiden kahden välissä selviytyminen tulee yksinkertaisesta lineaarisesta mallista
} 
t1~dnorm(-2,0.1)
t2~dnorm(0,0.1)I(,1)
t1X~dnorm(-2,1)
t2X~dnorm(0,1)
#t1X~dnorm(-2,1/(0.5*0.5))
#t2X~dnorm(0,1/(0.5*0.5))

b<-(ysfm)/(t2-t1)
a<--t1*b

ysfm~dbeta(2,2)I(0.01,0.99)
ysfmX~dbeta(2,2)I(0.01,0.99)

tau<-1/pow(sd,2)
sd~dunif(0.001,5)#dlnorm(1,0.1)
sdX~dunif(0.001,5)#dlnorm(1,0.1)


}"

cat(M3,file="prior-tiam.txt")

df4<-df3 |> filter(!is.na(thiam))
df4<-df2 |> filter(!is.na(thiam))
data<-list(thiam_obs=log(df4$thiam), x=df4$surv_eggs, Eggs=df4$eggs, n=length(df4$eggs))


var_names=c(
 # "x_rep",
  "t1", "t2", "a", "b", "ysfm","sd",
"t1X", "t2X","aX", "bX", "ysfmX","sdX")


run10 <- run.jags(M3,
                 monitor= var_names,data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)

run11<-extend.jags(run10, sample=3000, thin=10)#, add.monitor = c("aX"), drop.monitor = "x_rep")

run<-run10
summary(run)

chains<-as.mcmc.list(run)

par(mfrow=c(2,3))
plot(density(chains[,"t1X"][[1]]), lty=2)
lines(density(chains[,"t1"][[1]]))
plot(density(chains[,"t2X"][[1]]), lty=2)
lines(density(chains[,"t2"][[1]]))
#plot(density(chains[,"aX"][[1]]), lty=2)
#lines(density(chains[,"a"][[1]]))
#plot(density(chains[,"bX"][[1]]), lty=2)
#lines(density(chains[,"b"][[1]]))
plot(density(chains[,"ysfmX"][[1]]), lty=2)
lines(density(chains[,"ysfm"][[1]]))
plot(density(chains[,"sdX"][[1]]), lty=2)
lines(density(chains[,"sd"][[1]]))


summary(run, var="x")

chains<-window(chains, thin=100)

t1<-chains[,"t1"][[1]]
t2<-chains[,"t2"][[1]]
a<-chains[,"a"][[1]]
b<-chains[,"b"][[1]]
ysfm<-chains[,"ysfm"][[1]]

log_thiam=seq(-3,3, by=0.1)
surv<-array(NA, dim=c(length(a),length(log_thiam)))

for(i in 1:length(a)){
  for(j in 1:length(log_thiam)){
  surv[i,j]<-ifelse(log_thiam[j]<t1[i], 0,
                  ifelse(log_thiam[j]<t2,a[i]+b[i]*log_thiam[j],
                         ysfm[i]))
}}


plot(log_thiam, surv[i,], type="l")


chains[,"x_rep[1]"]
windows()
par(mfrow=c(5,5))
for(i in 1:length(data$x)){
plot(density(chains[,str_c("x_rep[",i,"]")][[1]]))
abline(v=data$x[i])
}


par(mfrow=c(1,1))
plot(chains[,"x_rep[1]"],data$thiam[1], alpha=0.15)
points(data$thiam,data$x/data$Eggs, col="blue")





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

