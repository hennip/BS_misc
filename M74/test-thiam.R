

mean(df$YSFM)

a<--14
b<-2.9
sd<-5.1

filter(dfFI, rivername=="Tornio", is.na(thiam)==F)%>%
#  select(thiam2)%>%
  mutate(P=a+b*(thiam2))%>%
  mutate(surv=exp(P)/(1+exp(P)))%>%
  group_by(year)%>%
  summarise(
    n=n(),
    mean_ysfm_ka=mean(YSFM)/100,
    mean_ysfm_pred=mean(1-surv),
    erotus=mean_ysfm_pred-mean_ysfm_ka
  )




mean(a+b*(thiam))
