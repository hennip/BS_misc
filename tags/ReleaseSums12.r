## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		Baltic salmon stock assessment (WGBAST)
#
# Contents:		Calculate sums of tagged smolt releases -> T 2.10.1
#
# R-file:		  ReleaseSums.R

# input: 		  releases10.txt

# R ver:	  	2.9.2

# programmed:	2010 Henni Pulkkinen
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
source("00-basics/packages.R")


dat<-read_xlsx("tags/dat/Releases12.xlsx", guess_max=5000,col_names=T) |> 
  select(YEAR, country, w_r,n_tag, Reproductive, ManageGroup, river) |> 
  filter(river=="Kemijoki") |> 
  select(-river)
dat


released<-dat |> group_by(YEAR, ManageGroup, w_r) |> 
  summarise(rel=sum(n_tag))

View(released)

released |> ungroup() |>  summarise(N=sum(rel))

write_xlsx(released, "../../WGBAST_shared/data/carlin_TorneKemi/tagged_Kemijoki.xlsx")



dat<-read_xlsx("tags/dat/Releases12.xlsx", guess_max=5000,col_names=T) |> 
  select(YEAR, country, w_r,n_tag, Reproductive, ManageGroup, river) |> 
  filter(river=="Tornionjoki", w_r=="R") |> 
  select(-river)
dat

released<-dat |> group_by(YEAR, ManageGroup, w_r) |> 
  summarise(rel=sum(n_tag))

View(released)

released |> ungroup() |>  summarise(N=sum(rel))

write_xlsx(released, "../../WGBAST_shared/data/carlin_TorneKemi/tagged_Torne_Rsp.xlsx")


dat<-read_xlsx("tags/dat/Releases12.xlsx", guess_max=5000,col_names=T) |> 
  select(YEAR, country, w_r,n_tag, Reproductive, ManageGroup, river) |> 
  filter(river %in% c("Tornionjoki","Simojoki"), w_r=="R") |> 
  select(-river)
dat

released<-dat |> group_by(YEAR, ManageGroup, w_r) |> 
  summarise(rel=sum(n_tag))

View(released)

released |> ungroup() |>  summarise(N=sum(rel))

write_xlsx(released, "../../WGBAST_shared/data/carlin_TorneKemi/tagged_TorneSimo_Rsp.xlsx")








#########################################
# If we want to leave Swedish tag releases out from the study since 2004:
#datSE<-subset(dat, country=="SE" & YEAR<2004)

#datSE<-subset(dat, country=="SE")
#datFI<-subset(dat, country=="FI")

#dat2<-rbind(datSE, datFI)
#summary(dat2)
#dat<-dat2
#########################################

RelSums<-function(dat){
rel<-vector()
years<- c(min(dat$YEAR):max(dat$YEAR))

for(i in 1:length(years)){
temp<-0

   for(j in 1:length(dat$n_tag)){
      if(dat$YEAR[j]==(i+(min(dat$YEAR)-1))){
	   temp<-temp+dat$n_tag[j]
	}
   }

rel[i]<-temp
}

return(cbind(rel, c(min(dat$YEAR):max(dat$YEAR))))
}

# Reared salmon stocked in rivers without natural reproduction
G1RT<-subset(dat, ManageGroup==1 & w_r=="R" & Reproductive==0)
RelSums(G1RT)
G2RT<-subset(dat, ManageGroup==2 & w_r=="R" & Reproductive==0)
RelSums(G2RT)
G3RT<-subset(dat, ManageGroup==3 & w_r=="R" & Reproductive==0)
RelSums(G3RT)

# Reared salmon stocked in rivers with natural reproduction
G1RR<-subset(dat, ManageGroup==1 & w_r=="R" & Reproductive==1)
RelSums(G1RR)
G2RR<-subset(dat, ManageGroup==2 & w_r=="R" & Reproductive==1)
RelSums(G2RR)
G3RR<-subset(dat, ManageGroup==3 & w_r=="R" & Reproductive==1)
RelSums(G3RR)

# Wild salmon, AU 1
G1W<-subset(dat, ManageGroup==1 & w_r=="W" & Reproductive==1)
RelSums(G1W)




