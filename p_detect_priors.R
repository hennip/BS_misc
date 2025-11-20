



M3<-"
model{

mu_sp_alpha[1]<-324    #Torne
mu_sp_beta[1]<-48

mu_sp_alpha[2]<-950   #Simo        #CHECK/CHANGE
mu_sp_beta[2]<-50

mu_sp_alpha[3]<-16   #Kalix
mu_sp_beta[3]<-13

mu_sp_alpha[4]<-20.4   #Mörrum
mu_sp_beta[4]<-82.7


CV_sp_alpha[1]<-3.75    #Torne
CV_sp_beta[1]<-71.25

CV_sp_alpha[2]<-10   #Simo   
CV_sp_beta[2]<-990

CV_sp_alpha[3]<-10   #Kalix
CV_sp_beta[3]<-30

CV_sp_alpha[4]<-10   #Mörrum
CV_sp_beta[4]<-30


for(s in 1:stocks){
    a_spawn[s]<-mu_spawn[s]*eta_spawn[s]+1
    b_spawn[s]<-(1-mu_spawn[s])*eta_spawn[s]+1
    
    mu_spawn[s]~dbeta(mu_sp_alpha[s],mu_sp_beta[s])      
    CV_spawn[s]~dbeta(CV_sp_alpha[s],CV_sp_beta[s])
    eta_spawn[s]<-(1-mu_spawn[s])/(mu_spawn[s]*CV_spawn[s]*CV_spawn[s])         #-1
    
    p.detect[s]~dbeta(a_spawn[s],b_spawn[s])
}
    
#sp_count[i,2]~dlnorm(muDS[i], tauDS) # Simojoki Didson count
#sp_count[i,1]~dbin(p.detect[i,1],NrWtot[i,1]) # Torne Didson count 

#muDS[i]<-log(NrWtot[i,2]/coefDS)-0.5*(1/tauDS)
tauDS<-1/(log(cvDS*cvDS+1))
cvDS~dlnorm(-2.37,8)
coefDS<-1.05 # assume that Simojoki Didson count is underestimate 


}"

cat(M3,file="p_detect.txt")

data<-list(
  stocks=4
)


var_names=c(
  "p.detect"#,"cvDS"
)
run10 <- run.jags(M3,
                  monitor= var_names,data=data, #inits = inits,
                  n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                  modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                  progress.bar=TRUE)


summary(run10)

plot(run10)

chains<-as.mcmc.list(run10)

Kalix p.detect prior
median 0.53, 95%PI 0.26-0.85


