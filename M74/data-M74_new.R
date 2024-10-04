
# Modify input data for M74 model (OpenBUGS)

# Stocks:
# Simojoki="1", Tornionjoki="2", Kemijoki="3", Iijoki="4",
# Luleälven="5",Skellefteälven="6",Umeälven="7",Ångermanälven="8",
# Indalsälven="9",Ljungan="10",Ljusnan="11",Dalälven="12",
# Morrumsån="13",`Unsampled stock`="14"))


# 
# library(rjags)
# library(runjags)
# library(tidyverse)
# library(readxl)
# #library(xlsx)
# library(forcats)
# library(lubridate)
# library(stringr)
# library(gridExtra)
# library(coda)

#source("00-basics/tidy-functions.r")
source("run-this-first.R")

pathM74old<-paste0(pathMain,"WGBAST/dat/SubE_M74/2021/")
pathM74<-paste0(pathMain,"WGBAST/dat/SubE_M74/2023/")

# FI data
dat<-read_xlsx(path=str_c(pathM74,"dat/orig/Finnish_M74_data-2022_paivitetty_TPa_21_01_2023.xlsx"), 
              col_names = T, guess_max = 10000, sheet="Data", na=c("", "NA"))   
#View(dat)

df<-dat%>%
  mutate(river=fct_recode(RIVER,
                                    "1"="Simo",
                                    "2"="Tornio",
                                    "3"="Kemi",
                                    "4"="Iijoki"))%>%
  # this keeps stock numbers in a correct order!
  mutate(stock=parse_double(as.character(river)))%>%
  mutate(rivername=RIVER)%>%
  mutate(YEAR=FEMALE_YEAR)%>%
  mutate(year=FEMALE_YEAR-1984)%>%
  mutate(Eggs=ifelse(is.na(eggs)==F,eggs,ifelse(year<10,100,115)))%>%
  mutate(eggs=Eggs)%>%
  mutate(surv_eggs=round(eggs*(1-(YSFM/100)),0))%>%
  mutate(thiam=`TIAM_nmol/g`)%>%
  mutate(thiam2=thiam*10)


dfFI<-df%>% 
  select(YEAR, year, rivername,stock, eggs, surv_eggs,YSFM, thiam, thiam2)%>%
  mutate(isM74_old=ifelse(thiam<0.5, 2, ifelse(is.na(thiam)==T, NA, 1)))%>%
  mutate(isM74=ifelse(thiam<0.98, 2, ifelse(is.na(thiam)==T, NA, 1)))

#View(filter(dfFI, is.na(isM74)==F, isM74!=isM74_old))

# REMOVE THE LAST YEAR'S DATA (NOT HATCHED YET)
#View(filter(dfFI, YEAR==2022 ))
dfFI<-filter(dfFI, YEAR<2022)

#View(dfFI)

# Swedish M74 data until 2016 (in final format)
df1<-read_tsv(str_c(pathM74old,"dat/der/M74dataSE16.txt"), col_names = T)
              
# New Swedish M74 data
df2<-read_xlsx(str_c(pathM74,"dat/der/Swedish_M74_data_17-22.xlsx"), na="NA")

df2<-df2%>%
  #mutate(river=as_factor(river))%>%
  mutate(stock=ifelse(river=="Dal",11, 
                      ifelse(river=="Ljusnan", 10,
                             ifelse(river=="Indals", 8,
                                    ifelse(river=="Angerman",7,
                                           ifelse(river=="Ume",6,
                                                  ifelse(river=="Skellefte",5,
                                                         ifelse(river=="Lule",4,"X"))))))))%>%
  mutate(yy=year-1985)%>%
  mutate(Females=Kramade)%>%
  mutate(xx=`Antal M74`)%>%
  select(stock,river, yy, Females, xx)
#View(df2)

# add missing data for swedish stocks with no recent M74 data, plus for unsampled stock 
df3<-tibble(
  yy=35, #2020, add +1 each year (year of spawning)
  stock=as_factor(c(9,12,13)), # Ljungan, Morrum, unsampled stock
  Females=100,
  xx=as.numeric(NA)
)
df4<-df3%>%mutate(yy=yy+1) # assessment year-1 (year of hatching)
  
dfSE<-df2%>%
  full_join(df3)%>%
  full_join(df4)%>%
  mutate(stock=as.numeric(stock))%>%
  mutate(stock=stock+1)%>% # +1 is needed to start SE stocks from index 5 (Iijoki included as stock 4)
  select(yy,stock,Females,xx)%>%
  full_join(df1) # df1 has correct stock numbers

# Check that stock >= 5 for all swedish stocks
#dfSE%>%filter(stock<5)
#View(dfSE)



# wrangle for figures

ysfm<-dfFI%>%
  group_by(stock, YEAR)%>%
  summarise(ysfm=round(mean(YSFM/100),2),
            N_fem=n())

n_M74notNA<-dfFI%>%
  filter(is.na(isM74)==F)%>%
  group_by(stock, YEAR)%>%
  summarise(N_M74=n())

cases_M74<-dfFI%>%
  group_by(stock, YEAR)%>%
  count(isM74)%>%
  filter(isM74==2)%>%
  mutate(N_M74fem=n)

# cases_mort100<-dfFI%>%
#   group_by(stock, YEAR)%>%
#   count(mortality100)%>%
#   filter(mortality100==2)%>%
#   mutate(N_mort100=n)

dfFI.2<-full_join(ysfm, n_M74notNA)%>%
  full_join(cases_M74)%>%
  #full_join(cases_mort100, by=c("stock", "YEAR"))%>%
  select(stock, YEAR, ysfm, N_fem, N_M74, N_M74fem#,N_mort100
         )%>%
  mutate(N_M74fem=ifelse(is.na(N_M74fem)==T, ifelse(is.na(N_M74)==T, NA, 0),N_M74fem))%>%
  #mutate(N_mort100=ifelse(is.na(N_mort100)==T, ifelse(is.na(N_M74)==T, NA, 0),N_mort100))%>%
  mutate(propM74=round(N_M74fem/N_fem,2))#%>%
  #mutate(prop_mort100=round(N_mort100/N_M74,2))#%>%

#View(dfFI.2)

dfSE.2<-dfSE%>%
  mutate(propM74=round(xx/Females,2))%>%
  mutate(river=as.factor(stock))%>%
  mutate(YEAR=yy+1984)%>%
  mutate(N_fem=Females)%>%
  mutate(N_M74fem=xx)%>%
  select(YEAR, stock, N_fem, N_M74fem, propM74)#%>%

#View(dfSE.2)

# Joining must be made by stock (numeric). With river (factor/char) the numbers will mess up
dfM74<-full_join(dfFI.2,dfSE.2)%>%
  select(-N_M74)%>%
  ungroup()

#View(dfM74%>%filter(YEAR>2016)%>%arrange(stock))

