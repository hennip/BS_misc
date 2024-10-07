library(runjags)
library(rjags)
library(writexl)

source("00-functions/path-main.R")
source("01-submodels/M74/data-M74.R")
dfFI
dfSE

#filter(dfFI, surv_eggs>0 & M74==2 & mortality100==2)
filter(dfFI, surv_eggs>0 &  mortality100==2)
dfFI<-dfFI%>%mutate(k=if_else(surv_eggs>0 & mortality100==2, 1, mortality100))
filter(dfFI, surv_eggs>0 &  mortality100==2)
#View(dfFI)

M1<-"model{
  
  for (i in 1:N_FI){ #number of females in the Finnish data
    x[i] ~dbin(p[i,j[i]], Eggs[i]) # likelihood function: x = number of surviving eggs, p= probability of survival, Egg = total number of eggs
    
    j[i]~dcat(q[year[i],ss[i], ]) # index if the female has M74 or not
    p[i,1]<- (1-M_YSFM[i])  # survival from normal yolk-sac-fry mortality
    p[i,2] <- (1-M_YSFM[i])*(1-M74[year[i],i,k[i]]) # survival from normal and M74 mortality
    
    k[i]~dcat(qq[year[i],ss[i],]) # index if the female has 100% M74 mortality or not
    M74[year[i],i,1] ~dbeta(a_M74[year[i]],b_M74[year[i]]) # M74 mortality
    M74[year[i],i,2] <- 1 # 100% M74 mortality
    M_YSFM[i]~dbeta(a_M_YSFM,b_M_YSFM) # normal yolk-sac-fry mortality
    
  }
  # transformation from mean and eta into beta-parameters
  a_M_YSFM<- mu_M_YSFM * eta_M_YSFM
  b_M_YSFM<- (1- mu_M_YSFM) * eta_M_YSFM
  mu_M_YSFM~dbeta(2,2)I(0.01,0.99) # mean normal yolk-sac-fry mortality
  eta_M_YSFM~dunif(2,1000) # indicator of variance in normal yolk-sac-fry mortality
  
  for (y in 1:35){ # Years , last spawner year class in the data is 35=2019 (add +1 each year)
    a_M74[y] <- mu_M74[y] * eta_M74[y] # conversion of mean and variance into alpha parameter of beta distribution
    b_M74[y] <- (1- mu_M74[y]) * eta_M74[y] # conversion of mean and variance into beta parameter of beta distribution
    mu_M74[y] ~dbeta(2,2)I(0.01,0.99) # mean M74 value for each year
    eta_M74[y]~dunif(2,1000) # indicator of variance between stocks
    
    for (s in 1:14){  # Note! Iijoki added
      
      surv2_M74[y,s] <-  (qq[y,s,1] * (1-mu_M74[y])) + (qq[y,s,2]) *0 # probability of offspring surviving M74 mortality if female has M74
      mean2_M74[y,s] <- 1 - surv2_M74[y,s] #probability of offspring dying because of M74 mortality if female has M74
      
      surv_M74[y,s] <- (q[y,s,1] *1)+ (q[y,s,2] * qq[y,s,1] * (1-mu_M74[y])) + (q[y,s,2] * qq[y,s,2] *0) # probability of offspring surviving M74 mortality
      mean_M74[y,s] <- 1 - surv_M74[y,s] #probability of offspring dying because of M74 mortality
      
      q[y,s,1] ~dbeta(a_q[y],b_q[y]) # probability of a female not having M74 syndromes
      q[y,s,2] <- 1- q[y,s,1] # probability of a female having M74 syndromes
      
      qq[y,s,1] ~dbeta(a_qq[y],b_qq[y]) # probability of a female not having 100 % M74 mortality
      qq[y,s,2] <- 1- qq[y,s,1] # probability of a female having 100 % M74 mortality
      
    }
    
    a_q[y] <- mu_q[y] * eta_q+1
    b_q[y] <- (1- mu_q[y]) * eta_q+ 1
    #mu_q[y]~dbeta(2,2)I(0.01,0.99)# mean probability of a female not having M74 syndromes
    mu_q[y]~dbeta(2,2)#I(0.02,0.98)# mean probability of a female not having M74 syndromes
    
    a_qq[y] <- mu_qq[y] * eta_qq
    b_qq[y] <- (1- mu_qq[y]) * eta_qq
    mu_qq[y]~dbeta(2,2)I(0.05,0.95)# mean probability of a female not having 100 % M74 mortality
    
  }
  eta_q~dunif(2, 1000)# indicator of variance in q
  eta_qq~dunif(10, 1000)# indicator of variance in qq
  
  
  for (i in 1:344){ # number of rows in the Swedish data 
    xx[i] ~dbin(q[yy[i], stock[i], 2], Females[i]) # likelihood function: xx = number of females affected by M74, p= probability of a female having M74, females = total number of females
  }}"


data=list(N_FI=length(dfFI$eggs),Eggs=dfFI$eggs, year=dfFI$year, ss=dfFI$stock,x=dfFI$surv_eggs, j=dfFI$M74, k=dfFI$k,
          yy=dfSE$yy, stock=dfSE$stock, Females=dfSE$Females, xx=dfSE$xx)
#data=list(Eggs=dfFI$eggs[120:1754], year=dfFI$year[120:1754], ss=dfFI$stock[120:1754],x=dfFI$surv_eggs[120:1754], 
#          j=dfFI$M74[120:1754], k=dfFI$mortality100[120:1754], N_FI=length(dfFI$eggs[120:1754]),
#          yy=dfSE$yy, stock=dfSE$stock, Females=dfSE$Females, xx=dfSE$xx)

var_names=c("mean_M74", "mean2_M74")
#inits=list(p=array(0.01,dim=c(1754,2)))

run0 <- run.jags(M1,
                 monitor= var_names,data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =10000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)

run1<-extend.jags(run0,combine=T,sample=1000, thin=10)
run2<-extend.jags(run1,combine=F,sample=1000, thin=100)

run<-run2

save(run, file="M74_run.RData")

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


