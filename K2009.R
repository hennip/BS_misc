library(runjags)
library(rjags)
library(writexl)

source("C:/tmp/path-main.r")

# Testi

M1<-"
model{

for(i in 1:n){
K[i]<-1/beta[i]
beta[i]~dlnorm(M[i], T[i])
M[i]<-log(mu[i])-0.5/T[i]
T[i]<-1/log(cv[i]*cv[i]+1)
cv[i]<-sd[i]/mu[i]
}

}
"

cat(M1,file="K.txt")


mu_beta<-c(0.0006135,0.01202,0.001005,0.01656,0.03845,0.0635,0.006603,0.2297,0.1313,0.0054,0.0513,0.04328,0.4356,0.00809,0.03344)
sd_beta<-c(0.0001487,0.004226,0.0003783,0.008762,0.009986,0.02677,0.002314,0.3368,0.07344,0.001969,0.03669,0.02688,0.273,0.002482,0.01251)

data<-list(mu=mu_beta, sd=sd_beta, n=length(mu_beta))

system.time(jm<-jags.model('K.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "K"
                                  ),
                                  n.iter=5000,
                                  thin=1))

summary(chains1)

