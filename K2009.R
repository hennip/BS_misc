library(runjags)
library(rjags)
library(writexl)

source("C:/tmp/path-main.r")

M1<-"
model{

  for(i in 1:n){
    K09[i]<-1/beta09[i]
    beta09[i]~dlnorm(M09[i], T09[i])
    M09[i]<-log(mu09[i])-0.5/T09[i]
    T09[i]<-1/log(cv09[i]*cv09[i]+1)
    cv09[i]<-sd09[i]/mu09[i]

    K23[i]<-1/beta23[i]
    beta23[i]~dlnorm(M23[i], T23[i])
    M23[i]<-log(mu23[i])-0.5/T23[i]
    T23[i]<-1/log(cv23[i]*cv23[i]+1)
    cv23[i]<-sd23[i]/mu23[i]

  }

}
"

cat(M1,file="K.txt")

# See beta_2009.xlsx
mu_beta09<-c(0.0006135,0.01202,0.001005,0.01656,0.03845,0.0635,0.006603,0.2297,0.1313,0.0054,0.0513,0.04328,0.4356,0.00809,0.03344)
sd_beta09<-c(0.0001487,0.004226,0.0003783,0.008762,0.009986,0.02677,0.002314,0.3368,0.07344,0.001969,0.03669,0.02688,0.273,0.002482,0.01251)
mu_beta23<-c(0.000501262,	0.01544166,	0.001490141,	0.014867289,	0.037137476,	0.105648701,	0.007239971,	0.0945463,	0.075748119,	0.003584743,	0.042016499,	0.029546674,	0.434978826,	0.025209605,	0.050281628)
sd_beta23<-c(3.61241E-05,	0.002987873,	0.000192839,	0.003995154,	0.003142791,	0.042987245,	0.00154906, 0.033058192,	0.030043173,	0.000381865,	0.025302078,	0.018515058,	0.498277121,	0.004070209,	0.021380496)

data<-list(
  mu09=mu_beta09, sd09=sd_beta09, n=length(mu_beta09),
  mu23=mu_beta23, sd23=sd_beta23)

system.time(jm<-jags.model('K.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains<-coda.samples(jm,
                                  variable.names=c(
                                    "K09", "K23"
                                  ),
                                  n.iter=5000,
                                  thin=1))

summary(chains)


#stock codes
#1 "Tornionjoki"
#2 "Simojoki"
#3 "Kalixalven"
#4 "Ranealven"
#5 "Pitealven"
#6 "Abyalven"
#7 "Byskealven"
#8 "Ricklean"
#9 "Savaran"
#10 "Vindelalven"
#11 "Orealven"
#12 "Logdealven"
#13 "Ljungan"
#14 "Morrumsan"
#15 "Eman"
#16 "Kagealven"
#17 "Testeboan"

par(mfrow=c(5,3),mar=c(2.5,4,4,1))
plot(density(chains[,"K09[1]"][[1]]), ylim=c(0,0.003), xlim=c(0,3000), main="Torne")
lines(density(chains[,"K23[1]"][[1]]), lwd=2)
plot(density(chains[,"K09[2]"][[1]]), ylim=c(0,0.04), xlim=c(0,200), main="Simo")
lines(density(chains[,"K23[2]"][[1]]), lwd=2)
plot(density(chains[,"K09[3]"][[1]]), ylim=c(0,0.006), xlim=c(0,2500), main="Kalix")
lines(density(chains[,"K23[3]"][[1]]), lwd=2)
plot(density(chains[,"K09[4]"][[1]]), ylim=c(0,0.03), xlim=c(0,200), main="Råne")
lines(density(chains[,"K23[4]"][[1]]), lwd=2)
plot(density(chains[,"K09[5]"][[1]]), ylim=c(0,0.2), xlim=c(0,50), main="Pite")
lines(density(chains[,"K23[5]"][[1]]), lwd=2)
plot(density(chains[,"K09[6]"][[1]]), ylim=c(0,0.12), xlim=c(0,50), main="Åby")
lines(density(chains[,"K23[6]"][[1]]), lwd=2)
plot(density(chains[,"K09[7]"][[1]]), ylim=c(0,0.02), xlim=c(0,400), main="Byske")
lines(density(chains[,"K23[7]"][[1]]), lwd=2)
plot(density(chains[,"K09[8]"][[1]]), ylim=c(0,0.12), xlim=c(0,50), main="Rickle")
lines(density(chains[,"K23[8]"][[1]]), lwd=2)
plot(density(chains[,"K09[9]"][[1]]), ylim=c(0,0.1), xlim=c(0,50), main="Sävarån")
lines(density(chains[,"K23[9]"][[1]]), lwd=2)
plot(density(chains[,"K09[10]"][[1]]), ylim=c(0,0.02), xlim=c(0,500), main="Vindel")
lines(density(chains[,"K23[10]"][[1]]), lwd=2)
plot(density(chains[,"K09[11]"][[1]]), ylim=c(0,0.04), xlim=c(0,150), main="Öre")
lines(density(chains[,"K23[11]"][[1]]), lwd=2)
plot(density(chains[,"K09[12]"][[1]]), ylim=c(0,0.04), xlim=c(0,150), main="Lögde")
lines(density(chains[,"K23[12]"][[1]]), lwd=2)
plot(density(chains[,"K09[13]"][[1]]), ylim=c(0,0.3), xlim=c(0,20), main="Ljungan")
lines(density(chains[,"K23[13]"][[1]]), lwd=2)
plot(density(chains[,"K09[14]"][[1]]), ylim=c(0,0.08), xlim=c(0,250), main="Mörrum")
lines(density(chains[,"K23[14]"][[1]]), lwd=2)
plot(density(chains[,"K09[15]"][[1]]), ylim=c(0,0.07), xlim=c(0,80), main="Emån")
lines(density(chains[,"K23[15]"][[1]]), lwd=2)

