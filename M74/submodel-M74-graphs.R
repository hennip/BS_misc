
#load("M74_run_SE2.RData")
load("M74_run_sd_t.RData")
load("M74_run_sd_t_prior.RData")

load("M74_run_sd_t_sy.RData")
load("M74_run_cv_t.RData")

plot_densities<-function(chains, chainsP, varname){
plot(density(chains[,varname][[1]]), main=varname)
  lines(density(chains[,varname][[2]]))
  lines(density(chainsP[,varname][[1]]), lty=2)
}

boxplot.df<-function(param, X){ # chain object, variable name, values to x-axis
  # note: length of x and dim variable need to match
  Q5<-c();Q25<-c();Q50<-c();Q75<-c();Q95<-c()
  n<-length(X)
  
  for(i in 1:n){
    y <- as.mcmc(param[,i])
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

# Densities
#########################################

chains<-as.mcmc.list(run)
chainsP<-as.mcmc.list(runP)
#chainsP<-as.mcmc.list(run)

summary(run, var="q[30,1,2]")
summary(run, var="mu_surv_M74[30,1]")
summary(run, var="E_thiam[30,1]")
summary(run, var="sd_t[30,1]")
summary(run, var="cv_E_sd_t")
summary(run, var="mu_E_sd_t")
summary(run, var="cv_t")


summary(run, var="P[300]")


data


summary(run, var="muq")
summary(run, var="etaq")

#summary(run, var="mort_M74")
summary(run, var="sd_t")

plot(run, var="muq")
plot(run, var="etaq")
plot(run, var="mumu")
plot(run, var="cvmu")
plot(run, var="musd")
plot(run, var="cvsd")
plot(run, var="a_t")
plot(run, var="b_t")
plot(run, var="sd_t")
plot(run, var="mu_sd_t")
plot(run, var="cv_sd_t")
#plot(run, var="mort_M74") #Näitä on 518kpl

par(mfrow=c(3,3),mar=c(2.5,4,4,1))
plot_densities(chains, chainsP, "a_t")
plot_densities(chains, chainsP, "b_t")
#plot_densities(chains, chainsP, "sd_t")
plot_densities(chains, chainsP, "mu_YSFM")
plot_densities(chains, chainsP, "eta_YSFM")
plot_densities(chains, chainsP, "mumu")
plot_densities(chains, chainsP, "cvmu")
plot_densities(chains, chainsP, "musd")
plot_densities(chains, chainsP, "cvsd")
plot_densities(chains, chainsP, "mu_sd_t")
plot_densities(chains, chainsP, "cv_sd_t")
#plot_densities(chains, chainsP, "mu_thiam")
#plot_densities(chains, chainsP, "sd_thiam")
#plot_densities(chains, chainsP, "etaq")

plot_densities(chains, chainsP, "[1,1,2]")
plot_densities(chains, chainsP, "q[2,1,2]")
plot_densities(chains, chainsP, "q[3,1,2]")

par(mfrow=c(3,3),mar=c(2.5,4,4,1))
for(y in 1:37){
  varname<-paste0("q[",y,",1,2]")
  plot(density(chains[,varname][[1]]), main=varname)
 # lines(density(chains[,varname][[2]]))
  #plot(hist(chains[,varname][[1]]), main=varname)
}


par(mfrow=c(3,3),mar=c(2.5,4,4,1))
for(y in 1:37){
  varname<-paste0("mort_M74[",y,",1]")
  plot(density(chains[,varname][[1]]), main=varname)
  lines(density(chains[,varname][[2]]))
  #plot(hist(chains[,varname][[1]]), main=varname)
}

par(mfrow=c(3,3),mar=c(2.5,4,4,1))
for(y in 1:37){
  varname<-paste0("mu_surv_M74[",y,",1]")
  plot(density(chains[,varname][[1]]), main=varname)
  lines(density(chains[,varname][[2]]))
  #plot(hist(chains[,varname][[1]]), main=varname)
}
for(y in 1:37){
  varname<-paste0("mu_surv_M74[",y,",1]")
  traceplot(chains[,varname], main=varname)
}

par(mfrow=c(3,3),mar=c(2.5,4,4,1))
for(y in 10:18){
  varname<-paste0("E_thiam[",y,",1]")
  plot(density(chains[,varname][[1]]), main=varname, xlim=c(0,10))
  lines(density(chains[,varname][[2]]))
  #plot(hist(chains[,varname][[1]]), main=varname)
}

# plot(density(chains[,"mu_YSFM"][[1]]), main="mu_YSFM", xlim=c(0,1))
# lines(density(chains[,"mu_YSFM"][[2]]))
# lines(density(chainsP[,"mu_YSFM"][[1]]), lty=2)

par(mfrow=c(2,2))
plot(density(chains[,"etaq"][[1]]), main="etaq", xlim=c(0,30))
lines(density(chains[,"etaq"][[2]]))
lines(density(chainsP[,"etaq"][[1]]), lty=2)

plot(density(chains[,"a_t"][[1]]), main="a_t", xlim=c(-20,0))
lines(density(chains[,"a_t"][[2]]))
lines(density(chainsP[,"a_t"][[1]]), lty=2)

plot(density(chains[,"b_t"][[1]]), main="b_t", xlim=c(0,5))
lines(density(chains[,"b_t"][[2]]))
lines(density(chainsP[,"b_t"][[1]]), lty=2)

plot(density(chains[,"sd_t"][[1]]), main="sd_t", xlim=c(0,8), ylim=c(0,1.5))
lines(density(chains[,"sd_t"][[2]]))
lines(density(chainsP[,"sd_t"][[1]]), lty=2)


# survival vs. thiamine
#########################################
thiam<-seq(0.1,30, by=1)
n<-length(thiam)
pick<-2 
#pick<-1 # sd not included

# pick chains for comparison
c1<-window(chains[[1]])#, thin=10)
c2<-window(chainsP[[1]], thin=10)


# Posterior
n_samp<-length(c1[,"a_t"])
at_samp<-c1[,"a_t"]
bt_samp<-c1[,"b_t"]
sdt_samp<-c1[,"sd_t"]

P_samp<-muP_samp<-array(NA, dim=c(n,n_samp))
p_samp<-array(NA, dim=c(n_samp,n))
for(j in 1:n_samp){
  for(i in 1:n){
    muP_samp[i,j]<-at_samp[j]+bt_samp[j]*thiam[i]
    P_samp[i,j]<-rnorm(1,muP_samp[i,j],sdt_samp[j])
    if(pick==2){p_samp[j,i]<-exp(P_samp[i,j])/(1+exp(P_samp[i,j]))}
    if(pick==1){p_samp[j,i]<-exp(muP_samp[i,j])/(1+exp(muP_samp[i,j]))}
  }
}

# Prior
n_samp<-length(c2[,"a_t"])
at_samp<-c2[,"a_t"]
bt_samp<-c2[,"b_t"]
sdt_samp<-c2[,"sd_t"]

P_samp<-muP_samp<-array(NA, dim=c(n,n_samp))
p_sampP<-array(NA, dim=c(n_samp,n))
for(j in 1:n_samp){
  for(i in 1:n){
    muP_samp[i,j]<-at_samp[j]+bt_samp[j]*thiam[i]
    P_samp[i,j]<-rnorm(1,muP_samp[i,j],sdt_samp[j])
    if(pick==2){p_sampP[j,i]<-exp(P_samp[i,j])/(1+exp(P_samp[i,j]))}
    if(pick==1){p_sampP[j,i]<-exp(muP_samp[i,j])/(1+exp(muP_samp[i,j]))}
  }
}



df<-boxplot.df(p_samp, thiam)
df.prior<-boxplot.df(p_sampP, thiam)


# In black and white
ggplot(df, aes(x, group=x))+
  theme_bw()+
  geom_boxplot(
    data=df.prior,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.01))+
  labs(x="Thiamin nmol/10g", y="Probability", 
       title="M74 Survival")+
  geom_line(aes(x,q50))+
  geom_line(data=df.prior, aes(x,q50),col="grey")#+
#theme(title = element_text(size=15), axis.text = element_text(size=12), strip.text = element_text(size=15))



