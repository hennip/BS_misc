ysfm<-0.9 # Survival
t1<-0.3
t2<-1.2

thiam_obs<-seq(0,3, by=0.05)

b<-(ysfm)/(t2-t1)
a<--t1*b
# t1/a=b
# <=> a= t1* 1/((t2-t1)*(1-ysfm))

surv<-c()
for(i in 1:length(thiam_obs)){
surv[i]<-
  ifelse(thiam_obs[i]<t1,0,
         ifelse(thiam_obs[i]>=t1 &thiam_obs[i]<t2, a +b * thiam_obs[i], ysfm))
}


plot(thiam_obs, surv)



# mu[i]<-(1-step(thiam_obs[i]-t1))*0  + 
#   step(thiam_obs[i]-t1)* (1-step(thiam_obs[i]-t2)) * (a+b*thiam_obs[i]) + 
#   step(thiam_obs[i]-t2)* ysfm 


# Sama toisin päin, eli y-akselilla kuolleisuus:

ysfm<-0.1 # mortality
t1<-0.3
t2<-1.2

thiam_obs<-seq(0,3, by=0.05)

b<--(1-ysfm)/(t2-t1)
a<-ysfm-b*t2
# t1/a=b
# <=> a= t1* 1/((t2-t1)*(1-ysfm))

mort<-c()
for(i in 1:length(thiam_obs)){
  mort[i]<-
    ifelse(thiam_obs[i]<t1,1,
           ifelse(thiam_obs[i]>=t1 &thiam_obs[i]<t2, a +b * thiam_obs[i], ysfm))
}


plot(thiam_obs, mort)
