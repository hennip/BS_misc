# library(runjags)
# library(rjags)
# library(writexl)
# 
# source("00-functions/path-main.R")
source("run-this-first.R")
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
    
    # Ennustetaan puuttuvat tiamiinit
    thiam_obs[i]~dlnorm(M_thiam[i], T_thiam)
    
    M_thiam[i]<-mean_thiam[year[i],stockFI[i]]-0.5/T_thiam
   
    M_YSFM[i]~dbeta(a_YSFM,b_YSFM) # normal yolk-sac-fry mortality
    S_YSFM[i]<-1-M_YSFM[i]
  }
  # transformation from mean and eta into beta-parameters
  a_YSFM<- mu_YSFM * eta_YSFM
  b_YSFM<- (1- mu_YSFM) * eta_YSFM
  mu_YSFM~dbeta(2,2)I(0.01,0.99) # mean normal yolk-sac-fry survival
  eta_YSFM~dunif(2,1000) # indicator of variance in normal yolk-sac-fry survival

  # See prior-thiam-vs-surv.r
  # a_t~dnorm(-3,4)
  # b_t~dlnorm(0.68,5)
  # sd_t~dlnorm(0.82,5)
 #   # Priors when thiam x 10
#  a_t~dnorm(-5,10)
#  b_t~dlnorm(0.001,100)
#  sd_t~dlnorm(-0.04,10)
  a_t~dnorm(-5,0.1)
  b_t~dlnorm(0.001,5)
  sd_t~dlnorm(-0.04,10)

  
  T_thiam<-1/log(cv_thiam*cv_thiam+1)
  cv_thiam~dunif(0.1,2) # cv saman vuoden ja kannan mittausten yli
  
  #sd_mean_thiam~dunif(0.01,10)
  Mpsi<-log(mupsi)-0.5/Tpsi
  Tpsi<-1/log(cvpsi*cvpsi+1)
  mupsi~dunif(0.01,10) 
  cvpsi~dunif(0.1,2)
  
  T_mean_thiam<-1/log(cv_mean_thiam*cv_mean_thiam+1)
  cv_mean_thiam~dunif(0.1,2) # cv saman vuoden mittausten yli
  
  for (y in 1:Nyears){ # Years , last spawner year class in the data is 37=2021 (add +1 each year)
    mu_mean_thiam[y]~dlnorm(Mpsi, Tpsi)#dunif(0.01,10) # keskimääräinen tiamiinitaso vuonna y
    M_mean_thiam[y]<-log(mu_mean_thiam[y])-0.5/T_mean_thiam
        
    for (s in 1:Nstocks){
    # Mietitään pitäisikö olla mu_mean_thiam[y] ja näille hyperparametrit, silloin kai
    # jos kannasta ei ole tietoa, sen tiamiinitason pitäisi olla samanlaisempi muiden kantojen saman vuoden
    # mittauksien kanssa kuin aivan muiden vuosien kanssa
      #mean_thiam[y,s]~dnorm(mu_mean_thiam, 1/(pow(sd_mean_thiam,2)))T(0.01,)
      mean_thiam[y,s]~dlnorm(M_mean_thiam[y], T_mean_thiam)
      
      Pmean[y,s]~dnorm(a_t+b_t*mean_thiam[y,s],1/pow(sd_t,2))
      logit(mu_surv_M74[y,s])<-Pmean[y,s] # mean thiamin based annual stock specific survival

      q[y,s,1]<-1-q[y,s,2]
      # PITÄISIKÖ NÄIDEN PRIOREIDEN OLLA KANTAKOHTAISIA???
      q[y,s,2]~dbeta(aq[y],bq[y])T(0.01,0.99) #Proportion that has M74
  
      mort_M74[y,s] <- 1-( (q[y,s,1]*1)+(q[y,s,2]*mu_surv_M74[y,s]) )# proportion of all offspring that dies because of M74
      
    }
    aq[y]<-muq[y]*etaq
    bq[y]<-(1-muq[y])*etaq
    muq[y]~dbeta(2,2)T(0.01,0.99)
  }
  etaq~dunif(2,1000)
  

  for (i in 1:N_SE){ # number of rows in the Swedish data
    xx[i] ~dbin(q[yy[i], stockSE[i], 2], Females[i]) # likelihood function: xx = number of females affected by M74, p= probability of a female having M74, females = total number of females
  }
}"

# HUOMIOT: mort_M74:n epävarmuudesta tulee nyt hurjan suurta. Kumuloituuko tämä
# nyt q-parametrista, vai Pmean-parametrista?


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

var_names=c("mort_M74", "muq", "etaq",
            "a_t", "b_t", "sd_t", 
"mu_YSFM", "eta_YSFM","cv_thiam", "mupsi", "cvpsi", "mu_mean_thiam", "cv_mean_thiam" )
#inits=list(p=array(0.01,dim=c(1754,2)))

run0 <- run.jags(M1,
                 monitor= var_names,data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =10000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)

run1<-extend.jags(run0,combine=T,sample=1000, thin=10)
#run2<-extend.jags(run1,combine=F,sample=1000, thin=100)

run<-run1
summary(run)
summary(run, var="q[30,1,2]")
summary(run, var="mu_surv_M74[30,1]")
summary(run, var="E_thiam[30]")


summary(run, var="mu")
summary(run, var="YSFM")
summary(run, var="a_t")
summary(run, var="b_t")
summary(run, var="sd_t")

plot(run)
plot(run, var="YSFM")
plot(run, var="a_t")
plot(run, var="b_t")
plot(run, var="sd_t")

save(dfFI, file="dfFI.RData")
save(run, file="M74_run.RData")

load("M74_run.RData")

load("M74_run_SE.RData") # Ruotsin data mukana

load("M74_run_SE2.RData") # Testiajo, isM74==2 silloin kun thiam<0.5

load("M74_run_test.RData")
load("M74_run_prior2.RData")

load("M74_run_sd_t.RData")



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


