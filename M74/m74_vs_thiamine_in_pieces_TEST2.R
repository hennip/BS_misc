dfFI
df2<-dfFI |> mutate(YSFM=YSFM/100)

ggplot(df2, aes(x=log(thiam), y=1-YSFM))+
  geom_point(alpha=0.2)

df3<-df2 |> filter(!is.na(thiam))
#df_ysfm<-df3 |> filter(thiam>=2) 
#df_m74<-df3 |> filter(thiam<2) 
df_ysfm<-df3 |> filter(thiam>=1) 
df_m74<-df3 |> filter(thiam<1) 

# KOKEILE VIELÄ MITEN TOIMISI JOS RAJA ON 1!
#df_ysfm<-df3 |> filter(thiam>=1) 
#df_m74<-df3 |> filter(thiam<1) 

#View(df_ysfm)
#View(df_m74)

#data<-list(thiam_obs=log(df4$thiam), x=df4$surv_eggs, Eggs=df4$eggs, n=length(df4$eggs))

###############################
# YSFM only
# 
# data<-list( x1=df_ysfm$surv_eggs, 
#             Eggs1=df_ysfm$eggs, 
#             N1=length(df_ysfm$eggs))
# 
# M_ysfm<-"
# model{
# 
# for(i in 1:N1){ # ysfm: survival from normal ysfm
#  x1[i]~dbin(ysfm[i],Eggs1[i])
#   ysfm[i]~dbeta(a_ysfm, b_ysfm)
# }
# a_ysfm<-mu_ysfm*eta_ysfm
# b_ysfm<-(1-mu_ysfm)*eta_ysfm
# mu_ysfm~dbeta(2,2)T(0.001,0.999)
# eta_ysfm~dunif(0.01,100)
# 
# pred_ysfm~dbeta(a_ysfm, b_ysfm)
# }"
# 
# var_names=c(
#   "pred_ysfm",
#   "mu_ysfm", "eta_ysfm")
# 
# run_ysfm <- run.jags(M_ysfm,
#                   monitor= var_names,data=data, #inits = inits,
#                   n.chains = 2, method = 'parallel', thin=10, burnin =1000,
#                   modules = "mix",keep.jags.files=F,sample =2000, adapt = 1000,
#                   progress.bar=TRUE)
# 
# run<-run_ysfm
# summary(run)

#             Lower95     Median   Upper95       Mean          SD Mode        MCerr MC%ofSD SSeff     AC.100      psrf
# pred_ysfm  0.876999  0.9704255  0.999993  0.9581824 0.041117962   NA 6.501321e-04     1.6  4000 0.01785897 1.0021529
# mu_ysfm    0.954129  0.9573255  0.961049  0.9572940 0.001738945   NA 2.749513e-05     1.6  4000 0.00562646 0.9998381
# eta_ysfm  18.925000 21.8915500 25.079600 21.9168705 1.599935520   NA 2.572906e-02     1.6  3867 0.02223250 1.0002549


###################################
# M74 and YSFM (and thiamine)

# data<-list(
#   thiam_obs=log(df_m74$thiam), x2=df_m74$surv_eggs, Eggs2=df_m74$eggs, N2=length(df_m74$eggs),
#   x1=df_ysfm$surv_eggs, Eggs1=df_ysfm$eggs, N1=length(df_ysfm$eggs))

data<-list(
  thiam_obs=log(df3$thiam), x=df3$surv_eggs, Eggs=df3$eggs, N=length(df3$eggs))


M4<-"
model{

# Estimoidaan ysfm osasta dataa (tiamiini >=2)
# ja käytetään tätä estimoidessa m74+ysfm kuolleisuutta lopulle datalle (tiamiini<2)
# päätös siitä mihin raja vedetään on subjektiivinen, pitäisi olla ekspertin tekemä rajaus
# 
# for(i in 1:N1){ # ysfm: survival from normal ysfm
#  x1[i]~dbin(ysfm[i],Eggs1[i])
#   ysfm[i]~dbeta(a_ysfm, b_ysfm)
# }
# a_ysfm<-mu_ysfm*eta_ysfm
# b_ysfm<-(1-mu_ysfm)*eta_ysfm
# mu_ysfm~dbeta(2,2)T(0.001,0.999)
# eta_ysfm~dunif(0.01,100)
# pred_ysfm~dbeta(a_ysfm, b_ysfm)



for(i in 1:N){
# i: female index
# x: surv_eggs
# Eggs: total number of eggs
 x[i]~dbin(p[i],Eggs[i])
 # p: survival probability
 p[i]~dbeta(ap[i], bp[i])T(0.01,0.99)

# Antti: logit-normaali korvaisi beta-jakauman näin:
# p[i] = ilogit(logit_p[i])
# logit_p[i] ~ dnorm(logit(mu[i]), sd_p^-2)
#  sd_p~
#########

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
bQ~dlnorm(log(2)-0.5*log(cv_bQ*cv_bQ),1/log(cv_bQ*cv_bQ+1))
cv_bQ<-0.2
sdQ~dlnorm(log(0.01)-0.5*log(cv_sdQ*cv_sdQ+1),1/log(cv_sdQ*cv_sdQ+1))
cv_sdQ<-0.2
tauQ<-1/pow(sdQ,2)

eta~dunif(0.01,1000)
surv_ysfm~dbeta(2,2)T(0.001,0.9999)

aQX~dnorm(0,1)
bQX~dlnorm(log(2)-0.5*log(cv_bQ*cv_bQ),1/log(cv_bQ*cv_bQ+1))
sdQX~dlnorm(log(0.01)-0.5*log(cv_sdQ*cv_sdQ+1),1/log(cv_sdQ*cv_sdQ+1))

}"



var_names=c(
  "sdQ", "sdQX ",
  "mu",
  "aQ", "bQ",
  "aQX", "bQX",
  "surv_ysfm",
  "t1", "t1X",
  "eta"
  )


run11 <- run.jags(M4,
                 monitor= var_names,data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                 modules = "mix",keep.jags.files=F,sample =10000, adapt = 1000,
                 progress.bar=TRUE)

run<-run11

summary(run)
plot(run)

plot(run, var="aQ")
plot(run, var="bQ")
plot(run, var="surv_ysfm")
plot(run, var="sdQ")
#
chains<-as.mcmc.list(run)

par(mfrow=c(2,3))
plot(density(chains[,"t1"][[1]]), lty=1)
lines(density(chains[,"t1X"][[1]]), lty=2)
plot(density(exp(chains[,"t1"][[1]])))

plot(density(chains[,"a"][[1]]))
plot(density(chains[,"b"][[1]]))
plot(density(chains[,"b2"][[1]]))

plot(density(chains[,"ysfm"][[1]]))
plot(density(chains[,"mu_ysfm"][[1]]))
plot(density(chains[,"eta_ysfm"][[1]]))


plot(run, var="t1")
plot(run, var="t2")
plot(run, var="ysfm")

summary(run, var="ysfm")
summary(run, var="b")
summary(run, var="t")



chains<-window(chains, thin=100)

t1<-chains[,"t1"][[1]]
t2<-chains[,"t2"][[1]]
a<-chains[,"a"][[1]]
b<-chains[,"b"][[1]]
ysfm<-chains[,"ysfm"][[1]]

t2_star<-(ysfm_pred-a)/b

log_thiam=seq(-2,1, by=0.01)
surv<-array(NA, dim=c(length(a),length(log_thiam)))

for(i in 1:length(a)){
  for(j in 1:length(log_thiam)){
  surv[i,j]<-ifelse(log_thiam[j]<t1[i], 0,
                  ifelse(log_thiam[j]<t2[i],a[i]+b[i]*log_thiam[j],
                         ysfm[i]))
  
  }}

par(mfrow=c(1,1))
plot(log_thiam, surv[1,], type="l", col=rgb(0,0,0,0.1), ylim=c(0,1), xlim=c(-2,1), ylab="survival",
     xlab="log(thiamine (nmol/g))")
#for(i in 1:length(t1)){
  for(i in 1:100){
    lines(log_thiam, surv[i,], type="l",  col=rgb(0,0,0,0.1))
}
points(log(df2$thiam), 1-df2$YSFM,  col=rgb(0,0,1,0.2))

# par(mfrow=c(1,1))
# plot(log_thiam, surv_star[1,], type="l", col=rgb(0,0,0,0.1), ylim=c(0,1), xlim=c(-2,1))
# #for(i in 1:length(t1)){
# for(i in 1:100){
#   lines(log_thiam, surv_star[i,], type="l",  col=rgb(0,0,0,0.1))
# }
# points(log(df2$thiam), 1-df2$YSFM,  col=rgb(0,0,1,0.2))
# 

summary((t2))

summary(exp(t1))
summary(exp(t2))
summary(ysfm)

summary(exp(t2_star))
summary(ysfm)


