
df2<-dfFI |> mutate(YSFM=YSFM/100) #|> 
  #  filter(YSFM>0)# & YSFM<1)
#  filter(thiam<1.5)# & YSFM<1)

plot( log(df2$thiam), df2$YSFM)

df2<-dfFI%>% filter(is.na(thiam)==F)
ggplot(df2, aes(x=log(thiam), y=1-YSFM, col=YEAR))+
  geom_point()


ggplot(df2, aes(x=thiam, y=1-YSFM, col=YEAR))+
  geom_point()


spline(x=log(thiam), y=1-YSFM, ties)
m1<-lm(df2$YSFM~bs(log(df2$thiam), knots=c(-1, 0)))
plot(log(df2$thiam), df2$YSFM)
thiam.grid <- seq(min(log(df2$thiam)), max(log(df2$thiam)), length.out = 100)
pred <- predict(m1, newdata = list("log(df$thiam)"=thiam.grid), se = T)

lines( pred$fit)
range(log(df2$thiam))


df3<-df2 |> filter(!is.na(thiam))
data<-list(thiam_obs=log(df3$thiam), x=df3$surv_eggs, Eggs=df3$eggs, n=length(df3$eggs))




M2<-"model{
  
  
  # Haudonta-aineisto, FI
  for (i in 1:N_FI){ #number of females in the Finnish data
    x[i]~dbin(p[i, j[i]], Eggs[i]) # likelihood function: x = number of surviving eggs, p= probability of survival, Egg = total number of eggs
    j[i]~dcat(q[1:2]) # dcat arpoo yksilön j-indeksin luokkiin M74 / ei M74 
    x_rep[i]~dbin(p[i, j[i]], Eggs[i]) 
    
    
    p[i,1] <- S_YSFM[i] #survival from normal YSFM
    p[i,2] <- S_YSFM[i]*S_M74[i] #survival from both YSFM and M74

    #p[i,2] <- S_YSFM[i]*S_M74[i,k[i]] #survival from both YSFM and M74
    #k[i]~dcat(qq[1:2]) # index if the female has 0% survival or not

    logit(S_M74[i])<-P[i] # M74 survival when not 0%
    P[i]~dnorm(a_t+b_t*thiam_obs[i],1/pow(sd,2))
    
    M_YSFM[i]~dbeta(a_YSFM,b_YSFM) # normal yolk-sac-fry mortality
    S_YSFM[i]<-1-M_YSFM[i]
  
  }
  YSFM_pred~dbeta(a_YSFM,b_YSFM)
  for(i in 1:n_lt_seq){
    P_pred[i]~dnorm(a_t+b_t*lt_seq[i], 1/pow(sd,2))
    logit(S_M74_pred[i])<-P_pred[i]
  }
  #S_M74[2] <- 0 # Survival 0 when 100% M74 mortality

  # parameters for normal yolk-sac-fry mortality
  a_YSFM<- mu_YSFM * eta_YSFM
  b_YSFM<- (1- mu_YSFM) * eta_YSFM
  mu_YSFM~dbeta(2,2)I(0.01,0.99)
  eta_YSFM~dunif(2,1000)
  
q[1]<-1-q[2] # Proportion that does not have M74
q[2]~dbeta(2,2)T(0.01,0.99) #Proportion that has M74
  

a_t~dunif(-100,100)
b_t~dunif(0,100)
sd~dunif(0.001,100)

  

}"



var_names=c(
  "YSFM_pred", "S_M74_pred",
  "S_M74",
  "x_rep",
  "a_t", "b_t", "sd",
  "mu_YSFM","eta_YSFM",
  "q"
  )

lt_seq<-seq(-3,3, by=0.1)

data=list(
  lt_seq=lt_seq, n_lt_seq=length(lt_seq),
  N_FI=length(df3$eggs),Eggs=df3$eggs,
  x=df3$surv_eggs, j=df3$isM74,
  #k=df3$isM74_100,
  thiam_obs=log(df3$thiam)
  
)

run0 <- run.jags(M2,
                 monitor= var_names,data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)
run<-run0

run1<-extend.jags(run0,combine=F,sample=1000, thin=10)
run<-run1

summary(run, var="_t")
summary(run, var="sd")
summary(run, var="q")
summary(run, var="YSFM")
summary(run, var="pred")



# Plot resultin logit curve + data
#####################################
chains<-as.mcmc.list(run)

a<-chains[,"a_t"][[1]]
b<-chains[,"b_t"][[1]]
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

par(mfrow=c(1,1))
plot( log(df2$thiam), 1-df2$YSFM)
for(i in 1:sim){
  lines(lt_seq, surv[i,], col=rgb(1,0,0,0.03))
  #lines(lt_seq, surv2[i,], col=rgb(1,0,0,0.03))
}

par(mfrow=c(5,5))
for(i in 1:500){
x<-chains[,paste0("x_rep[",i,"]")][[1]]

plot(density(x), main=i)
abline(v=df3$surv_eggs[i])
}

df3$surv_eggs[368]
df3$YSFM[368]



par(mfrow=c(5,5))
for(i in 1:500){
  x<-chains[,paste0("S_M74[",i,"]")][[1]]
  plot(density(x), main=i)
}




