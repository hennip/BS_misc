dat<-read.table(
"C:/Biom/FullLifeHistoryModel/2013/data/der/tags/TagsTornioSimo.txt",
header=T)
summary(dat)
dim(dat)

TW<-subset(dat, river=="Tornionjoki" & w_r=="W")
TR<-subset(dat, river=="Tornionjoki" & w_r=="R")
SW<-subset(dat, river=="Tornionjoki" & w_r=="W")
SR<-subset(dat, river=="Tornionjoki" & w_r=="R")
dim(TW)
summary(TW)


summary(as.factor(TW$YEAR))

years<-1998:2008

Recap<-array(NA, dim=c(length(years),4))
for(y in 1998:2008){
tmp1<-0
tmp2<-0
tmp3<-0
tmp4<-0

  for( i in 1:dim(TW)[1]){
    if(TW$YEAR[i]==y){tmp1<-tmp1+1}
  }
Recap[y-1997,1]<-tmp1

  for( i in 1:dim(TR)[1]){
    if(TR$YEAR[i]==y){tmp2<-tmp2+1}
  }
Recap[y-1997,2]<-tmp2

  for( i in 1:dim(SW)[1]){
    if(SW$YEAR[i]==y){tmp3<-tmp3+1}
  }
Recap[y-1997,3]<-tmp3

  for( i in 1:dim(SR)[1]){
    if(SR$YEAR[i]==y){tmp4<-tmp4+1}
  }
Recap[y-1997,4]<-tmp4
}


rel<-read.table(
"C:/Biom/FullLifeHistoryModel/2013/data/der/tags/TaggedReleasesTornioSimo.txt",
header=T)
summary(rel)
dim(rel)

rel<-subset(rel, YEAR>1997 & YEAR<2009)
summary(rel)
summary(as.factor(rel$YEAR))

relTW<-subset(rel, river=="Tornionjoki" & w_r=="W")
relTR<-subset(rel, river=="Tornionjoki" & w_r=="R")
relSW<-subset(rel, river=="Simojoki" & w_r=="W")
relSR<-subset(rel, river=="Simojoki" & w_r=="R")

tagged<-array(NA, dim=c(length(years),4))
for(y in 1998:2008){
#y<-1998
tmp1<-0
tmp2<-0
tmp3<-0
tmp4<-0

  for(i in 1:dim(relTW)[1]){
    if(relTW$YEAR[i]==y){tmp1<-tmp1+relTW$n_tag[i]}
  }
tagged[y-1997,1]<-tmp1

  for(i in 1:dim(relTR)[1]){
    if(relTR$YEAR[i]==y){tmp2<-tmp2+relTR$n_tag[i]}
  }
tagged[y-1997,2]<-tmp2

  for(i in 1:dim(relSW)[1]){
    if(relSW$YEAR[i]==y){tmp3<-tmp3+relSW$n_tag[i]}
  }
tagged[y-1997,3]<-tmp3

  for(i in 1:dim(relSR)[1]){
    if(relSR$YEAR[i]==y){tmp4<-tmp4+relSR$n_tag[i]}
  }
tagged[y-1997,4]<-tmp4

}

tagged

rate<-round(Recap/tagged,3)
colnames(rate)<-c("TW","TR","SW","SR")
rate

plot(years,rate[,1], type="l", ylim=c(0,0.4), xlab="Vuosi", ylab="Palautus %")
points(years,rate[,2], type="l",  col=2)
points(years,rate[,3], type="l",  col=3)
points(years,rate[,4], type="l",  col=4)

legend("topright", c("Tornionjoki villit", "Tornionjoki viljellyt",
"Simojoki villit","Simojoki viljellyt"), col=c(1:4), lty=c(1,1,1,1))


round(cbind(rate[,2]/rate[,1],rate[,4]/rate[,3]),2)


