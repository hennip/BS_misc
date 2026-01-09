dfFI
df2<-dfFI |> mutate(YSFM=YSFM/100)

ggplot(df2, aes(x=log(thiam), y=1-YSFM))+
  geom_point(alpha=0.2)

df3<-df2 |> filter(!is.na(thiam))
#df_ysfm<-df3 |> filter(thiam>=2) 
#df_m74<-df3 |> filter(thiam<2) 
df_ysfm<-df3 |> filter(thiam>=1) 
df_m74<-df3 |> filter(thiam<1) 

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

data<-list(
  thiam_obs=log(df_m74$thiam), x2=df_m74$surv_eggs, Eggs2=df_m74$eggs, N2=length(df_m74$eggs),
  x1=df_ysfm$surv_eggs, Eggs1=df_ysfm$eggs, N1=length(df_ysfm$eggs))


M4<-"
model{

# Estimoidaan ysfm osasta dataa (tiamiini >=2)
# ja käytetään tätä estimoidessa m74+ysfm kuolleisuutta lopulle datalle (tiamiini<2)
# päätös siitä mihin raja vedetään on subjektiivinen, pitäisi olla ekspertin tekemä rajaus

for(i in 1:N1){ # ysfm: survival from normal ysfm
 x1[i]~dbin(ysfm[i],Eggs1[i])
  ysfm[i]~dbeta(a_ysfm, b_ysfm)
}
a_ysfm<-mu_ysfm*eta_ysfm
b_ysfm<-(1-mu_ysfm)*eta_ysfm
mu_ysfm~dbeta(2,2)T(0.001,0.999)
eta_ysfm~dunif(0.01,100)
pred_ysfm~dbeta(a_ysfm, b_ysfm)

for(i in 1:N2){
 #x_rep[i]~dbin(p[i],Eggs[i])

# i: female index
# x: surv_eggs
# Eggs: total number of eggs
 x2[i]~dbin(p[i],Eggs2[i])
 # p: survival probability
 p[i]~dbeta(ap[i], bp[i])T(0.01,0.99)

# Antti: logit-normaali korvaisi beta-jakauman näin:
# p[i] = ilogit(logit_p[i])
# logit_p[i] ~ dnorm(logit(mu[i]), sd_p^-2)
#  sd_p~
#########

ap[i]<-mu[i]*eta
bp[i]<-(1-mu[i])*eta

 mu[i]<-(1-step(thiam_obs[i]-t1))*0.001  + 
 
 # Alternative 1:
 #step(thiam_obs[i]-t1) * (1-step(thiam_obs[i]-t2)) * (a+b*thiam_obs[i]) + 
 #step(thiam_obs[i]-t2) * mu_ysfm 
 
 # Alternative 2:
 # Using pred_ysfm instead of mu_ysfm adds one stochastic variable -> this enables ysfm==1 and all variation is explained with M74
 step(thiam_obs[i]-t1) * (1-step(thiam_obs[i]-t2)) * (a2+b2*thiam_obs[i]) + 
 step(thiam_obs[i]-t2) * pred_ysfm 
 
#t1: tiamiini, jonka alapuolella selviytyminen on 0
#t2: tiamiini, jonka yläpuolella selviytyminen on tavallinen ysfm
# näiden kahden välissä selviytyminen tulee yksinkertaisesta lineaarisesta mallista
} 

t1~dunif(-5,3)
t2~dunif(-3,3)
t1X~dunif(-5,3)
t2X~dunif(-3,3)

b<-(mu_ysfm)/(t2-t1)
a<--t1*b

b2<-(pred_ysfm)/(t2-t1)
a2<--t1*b2

eta~dunif(0.01,1000)

}"



var_names=c(
"a2", "b2",
    "pred_ysfm",
  "mu_ysfm", "eta_ysfm",
  "t1", "t2", "a", "b",
"t1X", "t2X")


run11 <- run.jags(M4,
                 monitor= var_names,data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)

run<-run10 # mu_ysfm as limit
#run<-run11 # pred_ysfm as limit

#summary(run)

chains<-as.mcmc.list(run)

par(mfrow=c(2,3))
plot(density(chains[,"t1"][[1]]), lty=1)
lines(density(chains[,"t1X"][[1]]), lty=2)
plot(density(exp(chains[,"t1"][[1]])))
plot(density(chains[,"t2"][[1]]), lty=1)
lines(density(chains[,"t2X"][[1]]), lty=2)
plot(density(exp(chains[,"t2"][[1]])))

plot(density(chains[,"a"][[1]]))
plot(density(chains[,"b"][[1]]))
plot(density(chains[,"b2"][[1]]))

plot(density(chains[,"pred_ysfm"][[1]]))
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
ysfm<-chains[,"mu_ysfm"][[1]]
# a<-chains[,"a2"][[1]]
# b<-chains[,"b2"][[1]]
# ysfm<-chains[,"pred_ysfm"][[1]]
ysfm_pred<-chains[,"pred_ysfm"][[1]]

t2_star<-(ysfm_pred-a)/b

log_thiam=seq(-2,1, by=0.01)
surv_star<-surv<-array(NA, dim=c(length(a),length(log_thiam)))

for(i in 1:length(a)){
  for(j in 1:length(log_thiam)){
  surv[i,j]<-ifelse(log_thiam[j]<t1[i], 0,
                  ifelse(log_thiam[j]<t2[i],a[i]+b[i]*log_thiam[j],
                         ysfm[i]))
  
  surv_star[i,j]<-ifelse(log_thiam[j]<t1[i], 0,
                    ifelse(log_thiam[j]<t2_star[i],a[i]+b[i]*log_thiam[j],
                           ysfm_pred[i]))
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
summary((t2_star))


summary(exp(t1))
summary(exp(t2))
summary(ysfm_pred)

summary(exp(t2_star))
summary(ysfm)


