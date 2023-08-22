library(runjags)
library(rjags)
library(writexl)

source("00-functions/path-main.R")
#source("01-submodels/M74/data-M74.R")
dfFI
dfSE


M1_P<-"model{
  
  
    #x~dbin(p, Eggs) # likelihood function: x = number of surviving eggs, p= probability of survival, Egg = total number of eggs
    
    p <- S_YSFM*S_M74 #survival from both
   
    logit(S_M74)<-P # M74 mortality
    P~dnorm(a_t+b_t*thiam_obs,1/pow(sd_t,2))
    
    thiam_obs~dlnorm(M_thiam, T_thiam)
    
    M_thiam<-mean_thiam-0.5/T_thiam
   
    S_YSFM~dbeta(a_YSFM,b_YSFM) # survival from normal yolk-sac-fry mortality
  
  # transformation from mean and eta into beta-parameters
  a_YSFM<- mu_YSFM * eta_YSFM
  b_YSFM<- (1- mu_YSFM) * eta_YSFM
  mu_YSFM~dbeta(2,2)I(0.01,0.99) # mean normal yolk-sac-fry survival
  eta_YSFM~dunif(1,1000) # indicator of variance in normal yolk-sac-fry survival

  # See prior-thiam-vs-surv.r
  #a_t~dnorm(-3,4)
  #b_t~dlnorm(0.68,5)
  #sd_t~dlnorm(0.82,5)
a_t~dnorm(-1.5,10)
b_t~dlnorm(2,100)
sd_t~dlnorm(0.01,1)
 #sd_t~dnorm(0.1, 10)T(0.01,)

  
  T_thiam<-1/log(cv_thiam*cv_thiam+1)
  cv_thiam~dunif(0.1,2) # cv saman vuoden ja kannan mittausten yli
  
  Mpsi<-log(mupsi)-0.5/Tpsi
  Tpsi<-1/log(cvpsi*cvpsi+1)
  mupsi~dunif(0.01,10) 
  cvpsi~dunif(0.1,2)
  
  T_mean_thiam<-1/log(cv_mean_thiam*cv_mean_thiam+1)
  cv_mean_thiam~dunif(0.1,2) # cv saman vuoden mittausten yli
  
    mu_mean_thiam~dlnorm(Mpsi, Tpsi)#dunif(0.01,10) # keskimääräinen tiamiinitaso vuonna y
    M_mean_thiam<-log(mu_mean_thiam)-0.5/T_mean_thiam
        
      mean_thiam~dlnorm(M_mean_thiam, T_mean_thiam)
      
      Pmean~dnorm(a_t+b_t*mean_thiam,1/pow(sd_t,2))
      logit(surv_M74)<-Pmean # annual M74 survival
      mean_M74 <- 1 - surv_M74 #probability of offspring dying because of M74 mortality
  
  
  

  # for (i in 1:344){ # number of rows in the Swedish data 
  #   xx[i] ~dbin(q[yy[i], stock[i], 2], Females[i]) # likelihood function: xx = number of females affected by M74, p= probability of a female having M74, females = total number of females
  # }
}"


# data=list(N_FI=length(dfFI$eggs),Eggs=dfFI$eggs, year=dfFI$year, stock=dfFI$stock,
#           x=dfFI$surv_eggs,
#           Nstocks=max(dfFI$stock), Nyears=max(dfFI$year)) 
#           #yy=dfSE$yy, stock=dfSE$stock, Females=dfSE$Females, xx=dfSE$xx)

var_names=c(#"mean_M74", 
            "a_t", "b_t", "sd_t", 
"mu_YSFM", "eta_YSFM","cv_thiam", "mupsi", "cvpsi", "mu_mean_thiam" )
#inits=list(p=array(0.01,dim=c(1754,2)))

run0 <- run.jags(M1_P,
                 monitor= var_names,#data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =10000,
                 modules = "mix",keep.jags.files=F,sample =10000, adapt = 1000,
                 progress.bar=TRUE)


runP<-run0
summary(runP)

