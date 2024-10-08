# library(runjags)
# library(rjags)
# library(writexl)
# 
# source("00-functions/path-main.R")
#source("01-submodels/M74/data-M74.R")
source("M74/data-M74_new.R")
dfFI
dfSE


M2<-"model{
  
  
  # Haudonta-aineisto, FI
  for (i in 1:N_FI){ #number of females in the Finnish data
    x[i]~dbin(p[i, j[i]], Eggs[i]) # likelihood function: x = number of surviving eggs, p= probability of survival, Egg = total number of eggs
    j[i]~dcat(q[year[i],stockFI[i],1:2]) # dcat arpoo yksilön j-indeksin luokkiin M74 / ei M74 
    x_rep[i]~dbin(p[i, j[i]], Eggs[i]) 
    
    p[i,1] <- S_YSFM[i] #survival from normal YSFM
    p[i,2] <- S_YSFM[i]*S_M74[i] #survival from both YSFM and M74

    logit(S_M74[i])<-P[i] # M74 mortality
    #P[i]~dnorm(a_t+b_t*thiam_obs[i],1/pow(sd_t,2))
    #P[i]~dnorm(a_t+b_t*thiam_obs[i],1/pow(sd_t[year[i]],2))
    P[i]~dnorm(a_t+b_t*thiam_obs[i],1/pow(sd_t[year[i], stockFI[i]],2))
    #! sd_t olisi tässä yleinen keskihajonta sille kuinka paljon
    # kuolleisuus voi poiketa pelkän tiamiinin mukaan määräytyvästä
    # Pitäisikö tämän hajonnan linkittyä johonkin muista hajontaparametreista,
    # vai onko selkeästi oma juttunsa?
    # Vaihtoehto olisi sd_t[i]~D(E_sd_t[year[i], cv), jolloin estimoitaisiin 
    # vuosittaiset poikkeamat tiamiinin antamasta ennusteesta.
    
    
    # Tiamiinien jakauma, tässä opitaan siitä mikä tiamiinipitoisuus olisi 
    # tiettynä vuonna tietyssä kannassa. Ennusteet vuosille joista ei ole mittauksia
    thiam_obs[i]~dlnorm(M_thiam_obs[i], T_thiam_obs[i])
    thiam_rep[i]~dlnorm(M_thiam_obs[i], T_thiam_obs[i])
    
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
      # Aiheuttaako tämä ongelmia survivalin estimoinnille? Jos tiamiinimittausta ei ole, cv tulee priorista

      # E_thiam: Tiamiinin jakauma vuonna y kannassa s sovitetaan logN-jakaumaan jotta
      # opitaan vuosikohtaisesta odotusarvosta ja hajonnasta
      # -> näiden avulla voidaan ennustaa tiamiinit esim. ruotsin kannoille
      E_thiam[y,s]~dlnorm(M_mean_thiam[y], 1/log(pow(sd_thiam[y]/mu_thiam[y],2)+1))
      logit_surv[y,s]~dnorm(a_t+b_t*E_thiam[y,s],1/pow(sd_t[y,s],2))
      logit(mu_surv_M74[y,s])<-logit_surv[y,s] # thiamin based annual & stock specific survival

      q[y,s,1]<-1-q[y,s,2] # Proportion that does not have M74
      q[y,s,2]~dbeta(aq[y],bq[y])T(0.01,0.99) #Proportion that has M74
  
      # proportion of all offspring that dies because of M74
      mort_M74[y,s] <- 1-( (q[y,s,1]*1)+(q[y,s,2]*mu_surv_M74[y,s]) )
      
      sd_t[y,s]~dlnorm(M_sd_t[y,s], cv_sd_t)
      M_sd_t[y,s]<-log(E_sd_t[y])-0.5*log(pow(cv_sd_t,2)+1)
    }
    M_mean_thiam[y]<-log(mu_thiam[y])-0.5*log(pow(sd_thiam[y]/mu_thiam[y],2)+1)
#    sd_t[y]~dlnorm(M_sd_t,T_sd_t)
    E_sd_t[y]~dlnorm(M_E_sd_t,T_E_sd_t)
    
    mu_thiam[y]~dlnorm(log(mumu)-0.5/Tmu, Tmu)# keskimääräinen tiamiinitaso vuonna y, hierarkkiset parametrit yli vuosien
    sd_thiam[y]~dlnorm(log(musd)-0.5/Tsd, Tsd) # Kantojen välinen vaihtelu  
      
    aq[y]<-muq[y]*etaq
    bq[y]<-(1-muq[y])*etaq
    muq[y]~dbeta(2,2)T(0.01,0.99)
  }
cv_sd_t~dunif(0.01,5)

  # parameters for normal yolk-sac-fry mortality
  a_YSFM<- mu_YSFM * eta_YSFM
  b_YSFM<- (1- mu_YSFM) * eta_YSFM
  mu_YSFM~dbeta(2,2)I(0.01,0.99)
  eta_YSFM~dunif(2,1000)

  # See prior-thiam-vs-surv.r
  a_t~dnorm(-5,0.1)
  b_t~dlnorm(0.001,5)
  #sd_t~dlnorm(-0.04,10)

  # M_sd_t<-mu_sd_t-0.5/T_sd_t
  # T_sd_t<-1/log( pow(cv_sd_t,2) +1 )
  # cv_sd_t~dunif(0.001,2)
  # mu_sd_t~dlnorm(-0.04,10)
  M_E_sd_t<-mu_E_sd_t-0.5/T_E_sd_t
  T_E_sd_t<-1/log( pow(cv_E_sd_t,2) +1 )
  cv_E_sd_t~dunif(0.001,2)
  mu_E_sd_t~dlnorm(-0.04,10)
  


  mumu~dunif(0.01,10) 
  cvmu~dunif(0.1,2)
  Tmu<-1/log(cvmu*cvmu+1)

  musd~dunif(0.01,10) 
  cvsd~dunif(0.1,2)
  Tsd<-1/log(cvsd*cvsd+1)

  etaq~dunif(2,1000)

}"

prior<-F

if(prior==F){
  data=list(
    N_SE=length(dfSE$yy),yy=dfSE$yy, stockSE=dfSE$stock, Females=dfSE$Females, xx=dfSE$xx,
    N_FI=length(dfFI$eggs),Eggs=dfFI$eggs, year=dfFI$year, stockFI=dfFI$stock,
    x=dfFI$surv_eggs, j=dfFI$isM74,
    #Nstocks=max(dfFI$stock), 
    Nstocks=max(dfSE$stock), 
    Nyears=max(dfFI$year),
    thiam_obs=dfFI$thiam2
    #thiam_obs=dfFI$thiam
  ) }else{
    
    data=list(
      N_SE=1,yy=1, stockSE=2, Females=100, 
      N_FI=1,Eggs=100, year=1, stockFI=2,
      #Nstocks=max(dfFI$stock), 
      Nstocks=2, 
      Nyears=1
    ) 
    
  }
data


var_names=c(
"thiam_obs",  
"thiam_rep",  
#  "P",
#  "E_sd_t",
  "sd_t",
  "mort_M74", 
            "mu_surv_M74", "E_thiam",
            "muq", "etaq","aq","bq","q",
            "x_rep",
            "a_t", "b_t", "sd_t", 
"mu_YSFM", "eta_YSFM",#"cv_thiam_obs", 
"mumu", "cvmu", 
"musd", "cvsd",
"mu_E_sd_t", "cv_E_sd_t")

#inits=list(p=array(0.01,dim=c(1754,2)))

modelName<-"M74_run_sd_t_sy"


if(prior==T){
  runP <- run.jags(M2,
                   monitor= var_names,data=data, #inits = inits,
                   n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                   modules = "mix",keep.jags.files=F,sample =100000, adapt = 1000,
                   progress.bar=TRUE)
  
  save(runP, file=paste0("/home/henni/WGBAST/out/",modelName,"_prior.RData"))
  
  
  }

run0 <- run.jags(M2,
                 monitor= var_names,data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)
run<-run0
save(run, file=paste0("/home/henni/WGBAST/out/",modelName,".RData"))

run1<-extend.jags(run0,combine=T,sample=1000, thin=10)
run<-run1
save(run, file=paste0("/home/henni/WGBAST/out/",modelName,".RData"))

run2<-extend.jags(run1,combine=F,sample=1000, thin=100)
run<-run2
save(run, file=paste0("/home/henni/WGBAST/out/",modelName,".RData"))

run3<-extend.jags(run2,combine=T,sample=10000, thin=100)
run<-run3
save(run, file=paste0("/home/henni/WGBAST/out/",modelName,".RData"))



run4<-extend.jags(run3,combine=F,sample=10000, thin=100)
run<-run4
save(run, file=paste0("/home/henni/WGBAST/out/",modelName,".RData"))



load("M74_run_X.RData")


summary(run)
summary(run, var="YSFM")
summary(run, var="q[30,1,2]")
summary(run, var="mu_surv_M74[30,1]")
summary(run, var="E_thiam[30,1]")
summary(run, var="sd_t[30,1]")
summary(run, var="cv_sd_t")

plot(run)
plot(run, var="YSFM")
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


