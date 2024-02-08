# library(runjags)
# library(rjags)
# library(writexl)
# 
# source("00-functions/path-main.R")
#source("01-submodels/M74/data-M74.R")
source("M74/data-M74_new.R")
dfFI
dfSE


M1<-"model{
  
  
  # Haudonta-aineisto, FI
  for (i in 1:N_FI){ #number of females in the Finnish data
    x[i]~dbin(p[i, j[i]], Eggs[i]) # likelihood function: x = number of surviving eggs, p= probability of survival, Egg = total number of eggs
    j[i]~dcat(q[year[i],stockFI[i],1:2])
    x_rep[i]~dbin(p[i, j[i]], Eggs[i]) 
    
    p[i,1] <- S_YSFM[i] #survival from normal YSFM
    p[i,2] <- S_YSFM[i]*S_M74[i] #survival from both YSFM and M74

    logit(S_M74[i])<-P[i] # M74 mortality
    P[i]~dnorm(a_t+b_t*thiam_obs[i],1/pow(sd_t,2))
    
    thiam_obs[i]~dlnorm(M_thiam_obs[i], T_thiam_obs[i])
    
    M_thiam_obs[i]<-E_thiam[year[i],stockFI[i]]-0.5/T_thiam_obs[i]
    T_thiam_obs[i]<-1/log( pow(cv_thiam_obs[year[i],stockFI[i]],2) +1 )
 
    M_YSFM[i]~dbeta(a_YSFM,b_YSFM) # normal yolk-sac-fry mortality
    S_YSFM[i]<-1-M_YSFM[i]
  
  }
  
  for (i in 1:N_SE){ # number of rows in the Swedish data
    xx[i]~dbin(q[yy[i], stockSE[i], 2], Females[i]) # likelihood function: xx = number of females affected by M74, p= probability of a female having M74, females = total number of females
  }

  
  for (y in 1:Nyears){ # Years , last spawner year class in the data is 37=2021 (add +1 each year)
        
    for (s in 1:Nstocks){
      cv_thiam_obs[y,s]~dunif(0.1,2) # Huom, havaintojen cv:t riippumattomia toisistaan.

      # Distribution of thiamin in year y at stock s
      E_thiam[y,s]~dlnorm(M_mean_thiam[y], 1/log(pow(sd_thiam[y]/mu_thiam[y],2)+1))
      
      logit_surv[y,s]~dnorm(a_t+b_t*E_thiam[y,s],1/pow(sd_t,2))
      logit(mu_surv_M74[y,s])<-logit_surv[y,s] # thiamin based annual & stock specific survival

      q[y,s,1]<-1-q[y,s,2] # Proportion that does not have M74
      q[y,s,2]~dbeta(aq[y],bq[y])T(0.01,0.99) #Proportion that has M74
  
      # proportion of all offspring that dies because of M74
      mort_M74[y,s] <- 1-( (q[y,s,1]*1)+(q[y,s,2]*mu_surv_M74[y,s]) )
      
    }
    M_mean_thiam[y]<-log(mu_thiam[y])-0.5*log(pow(sd_thiam[y]/mu_thiam[y],2)+1)

    mu_thiam[y]~dlnorm(log(mumu)-0.5/Tmu, Tmu)# keskimääräinen tiamiinitaso vuonna y, hierarkkiset parametrit samat yli vuosien
    sd_thiam[y]~dlnorm(log(musd)-0.5/Tsd, Tsd) # Kantojen välinen vaihtelu  
      
    aq[y]<-muq[y]*etaq
    bq[y]<-(1-muq[y])*etaq
    muq[y]~dbeta(2,2)T(0.01,0.99)
  }

  # parameters for normal yolk-sac-fry mortality
  a_YSFM<- mu_YSFM * eta_YSFM
  b_YSFM<- (1- mu_YSFM) * eta_YSFM
  mu_YSFM~dbeta(2,2)I(0.01,0.99)
  eta_YSFM~dunif(2,1000)

  # See prior-thiam-vs-surv.r
  a_t~dnorm(-5,0.1)
  b_t~dlnorm(0.001,5)
  sd_t~dlnorm(-0.04,10)

  mumu~dunif(0.01,10) 
  cvmu~dunif(0.1,2)
  Tmu<-1/log(cvmu*cvmu+1)

  musd~dunif(0.01,10) 
  cvsd~dunif(0.1,2)
  Tsd<-1/log(cvsd*cvsd+1)

  etaq~dunif(2,1000)

}"


data=list(
  N_SE=length(dfSE$yy),yy=dfSE$yy, stockSE=dfSE$stock, Females=dfSE$Females, xx=dfSE$xx,
  N_FI=length(dfFI$eggs),Eggs=dfFI$eggs, year=dfFI$year, stockFI=dfFI$stock,
  x=dfFI$surv_eggs, j=dfFI$isM74,
  #Nstocks=max(dfFI$stock), 
  Nstocks=max(dfSE$stock), 
  Nyears=max(dfFI$year),
  thiam_obs=dfFI$thiam2
  #thiam_obs=dfFI$thiam
) 


var_names=c("mort_M74", "muq", "etaq","x_rep",
            "a_t", "b_t", "sd_t", 
"mu_YSFM", "eta_YSFM",#"cv_thiam_obs", 
"mumu", "cvmu", 
"musd", "cvsd", 
"mu_mean_thiam", "cv_mean_thiam" )
#inits=list(p=array(0.01,dim=c(1754,2)))

run0 <- run.jags(M1,
                 monitor= var_names,data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)
run<-run0
save(run, file="/home/henni/WGBAST/out/M74_run_X.RData")

run1<-extend.jags(run0,combine=T,sample=1000, thin=10)
run<-run1
save(run, file="/home/henni/WGBAST/out/M74_run_SE2.RData")

run2<-extend.jags(run1,combine=F,sample=1000, thin=100)
run<-run2
save(run, file="/home/henni/WGBAST/out/M74_run_SE2.RData")

run3<-extend.jags(run2,combine=T,sample=10000, thin=100)
run<-run3
save(run, file="/home/henni/WGBAST/out/M74_run_SE2.RData")



run3<-extend.jags(run1,combine=F,sample=10000, thin=100)
run<-run3
save(run, file="/home/henni/WGBAST/out/M74_run_SE2tmp.RData")

load("M74_run_X.RData")


summary(run)
summary(run, var="YSFM")

plot(run)
plot(run, var="YSFM")
plot(run, var="a_t")
plot(run, var="b_t")
plot(run, var="sd_t")
plot(run, var="cv")
plot(run, var="mumu")
plot(run, var="musd")
plot(run, var="cvsd")
plot(run, var="etaq")

save(dfFI, file="dfFI.RData")
load("M74_run.RData")
load("M74_run_prior2.RData")

load("M74_run_SE2.RData")


sum_run<-summary(run)
rnames<-rownames(sum_run)
df<-as.data.frame(sum_run)
df$rnames<-rnames
write_xlsx(df,path = "H:/Projects/WGBAST/SubE_M74/2021/prg/output/M74_stats.xlsx")


# summary(run0)
# plot(run0)

chains<-as.mcmc.list(run)
par(mfrow=c(3,3))
#traceplot(chains[,"mean_M74[34,4]"])
traceplot(chains)


# tmp<-failed.jags(c('model','data','inits'))
# length(tmp)
# length(tmp[1])
# tmp[3] # inits

#y<-1;s<-1
beta<-alpha<-sd<-mu<-array(NA, dim=c(35,14))
for(y in 1:35){
for(s in 1:14){
  mu[y,s]<-summary(chains[,paste0("mean_M74[",y,",",s,"]")])$statistics[1]
  sd[y,s]<-summary(chains[,paste0("mean_M74[",y,",",s,"]")])$statistics[2]
  alpha[y,s]<-((mu[y,s]^2)-(mu[y,s]^3)-mu[y,s]*(sd[y,s]^2))/(sd[y,s]^2)
  beta[y,s]<-(mu[y,s]-(sd[y,s]^2)-2*(mu[y,s]^2)+(mu[y,s]^3)+mu[y,s]*(sd[y,s]^2))/(sd[y,s]^2)
}
}

cbind(alpha[,2],beta[,2])
cbind(alpha[,14],beta[,14])


