## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		Baltic salmon stock assessment (WGBAST)

# Contents:		Script for calculating and saving several .csv-files from tagging
#             data for different fisheries. 

# R-file:		  TaggingDataWithLags.R

# input: 		  datR_TagRecaps12.txt
# output:  		Several .csv-files

# R ver:	  	2.13.2

# programmed:	2012 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

dat<-read_xlsx("tags/dat/datR_TagRecaps13_incl_river.xlsx", na="NA", col_names = T)

summary(dat)
dim(dat)

summary(dat$rel_year)
summary(as.factor(dat$rec_year))
summary(as.factor(dat$rec_day))

dat<-subset(dat, river=="Tornionjoki")



# Correct rec_year-variable
#===============================
#RecYear<-dat$rec_year

apu<-vector()
for(i in 1:length(dat$rec_year)){
  if(is.na(dat$rec_year[i])==F && dat$rec_day[i]!=99){
    if(dat$rec_year[i]>=0 && dat$rec_year[i]<30){ apu[i]<-dat$rec_year[i]+2000 }
    if(dat$rec_year[i]>=80 && dat$rec_year[i]<=99){ apu[i]<-dat$rec_year[i]+1900 }
    if(dat$rec_year[i]>1000){ apu[i]<-dat$rec_year[i] }
  }
  if(is.na(dat$rec_year[i])==T || dat$rec_day[i]==99){
    apu[i]<-NA
  }
}

summary(as.factor(apu))

dat$rec_year<-apu
summary(dat)

# =================================
# Define RecOK variable which gives 0 for those that rec_year is NA 
# and those that have been recaptured outside six years from release.
# Others are ok, RecOK==1

RecOK<-rep(1,length(dat$rec_year))
for(i in 1:length(dat$rec_year)){
   if(is.na(dat$rec_year[i])==T ||   # rec_year[i]'s that are NA
    (dat$rec_year[i]<dat$rel_year[i] || dat$rec_year[i]>dat$rel_year[i]+5)){  # Those that have not been recaptured within six years from release
      RecOK[i]<-0
   } 
} 

dat$RecOK<-RecOK
summary(dat)
sum(RecOK)


#! ================================
#! Run script TaggingFunctions.R!!!
#! ================================

summary(dat$gear)

levels(as.factor(dat$rec_year)) # check what's the latest recapture year!

# Inform the path where the .csv-files will be saved
path<-"tags/Files_csv"

dat1tot<-subset(dat, RecOK==1)
dat2tot<-subset(dat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

AllTags<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(AllTags,0), file=paste(sep="", path, "/AllTags.csv"), sep=",")

                             
# ==============================================================================
# Next, calculate number of tags for different fisheries and gears for 
# each recapture-half-year i.e. 2006/ 1&2 and 2007/ 1&2
# wild and reared separately
# then river, ou, oll, odn, cu, cgn1, cgn2, cgn3, ctn1, ctn2, ctn3, cll, cdn, 
# Save resulting tables into excel-files.
# ==============================================================================
# Total catch reared
# ############################ 
Rdat<-subset(dat, w_r=="R")
dim(Rdat)
#62966

# Reared coastal
# ===================
RCdat<-subset(dat, w_r=="R" & fishery=="C")
dim(RCdat)
# 18439

dat1tot<-subset(RCdat, RecOK==1)
dat2tot<-subset(RCdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_C<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_C), file=paste(sep="", path, "/Reared_C.csv"), sep=",")


# Reared offshore
# ===================
ROdat<-subset(dat, w_r=="R" & fishery=="O")
#summary(ROdat)
dim(ROdat)
# 37501
dat1tot<-subset(ROdat, RecOK==1)
dat2tot<-subset(ROdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_O<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_O), file=paste(sep="", path, "/Reared_O.csv"), sep=",")

# Reared river
# ===================
RRdat<-subset(dat, w_r=="R" & fishery=="R")
dim(RRdat)
# 5349
dat1tot<-subset(RRdat, RecOK==1)
dat2tot<-subset(RRdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_R<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_R), file=paste(sep="", path, "/Reared_R.csv"), sep=",")

# Reared unknown
# ===================
RUdat<-subset(dat, w_r=="R" & fishery=="U")
dim(RUdat)
# 1677
dat1tot<-subset(RUdat, RecOK==1)
dat2tot<-subset(RUdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_U<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_U), file=paste(sep="", path, "/Reared_U.csv"), sep=",")


# Total coastal catch reared
# ############################ 
summary(dat$gear)

# Coastal driftnet
# ===================
RCdat<-subset(dat, w_r=="R" & fishery=="C")
RCDNdat<-subset(RCdat, gear=="drifnet" | gear=="driftnet" | gear=="DRIFTNET" | gear=="Driftnet")
dim(RCDNdat)
# 686
dat1tot<-subset(RCDNdat, RecOK==1)
dat2tot<-subset(RCDNdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_CDN<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_CDN), file=paste(sep="", path, "/Reared_C_DN.csv"), sep=",")

# Coastal longline
# ===================
RCdat<-subset(dat, w_r=="R" & fishery=="C")
RCLLdat<-subset(RCdat, gear=="longline" | gear=="Longline" | gear=="LONGLINE")
dim(RCLLdat)
# 116
dat1tot<-subset(RCLLdat, RecOK==1)
dat2tot<-subset(RCLLdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_CLL<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_CLL), file=paste(sep="", path, "/Reared_C_LL.csv"), sep=",")

# Coastal trapnet AU1
# ===================
summary(dat$unit)
RCdat<-subset(dat, w_r=="R" & fishery=="C")
RCTNdat<-subset(RCdat, gear=="trapnet"| gear=="Trapnet" | gear=="TRAPNET")
RCTN1dat<-subset(RCTNdat, unit==1)
dim(RCTN1dat)
# 2606
dat1tot<-subset(RCTN1dat, RecOK==1)
dat2tot<-subset(RCTN1dat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_CTN1<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_CTN1), file=paste(sep="", path, "/Reared_C_TN_G1.csv"), sep=",")

# Coastal trapnet AU2
# ===================
RCdat<-subset(dat, w_r=="R" & fishery=="C")
RCTNdat<-subset(RCdat, gear=="trapnet" | gear=="Trapnet" | gear=="TRAPNET")
RCTN2dat<-subset(RCTNdat, unit==2)
dim(RCTN2dat)
# 2484
dat1tot<-subset(RCTN2dat, RecOK==1)
dat2tot<-subset(RCTN2dat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_CTN2<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_CTN2), file=paste(sep="", path, "/Reared_C_TN_G2.csv"), sep=",")

# Coastal trapnet AU3
# ===================
RCdat<-subset(dat, w_r=="R" & fishery=="C")
RCTNdat<-subset(RCdat, gear=="trapnet" | gear=="Trapnet" | gear=="TRAPNET")
RCTN3dat<-subset(RCTNdat, unit==3)
dim(RCTN3dat)
# 4371
dat1tot<-subset(RCTN3dat, RecOK==1)
dat2tot<-subset(RCTN3dat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_CTN3<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_CTN3), file=paste(sep="", path, "/Reared_C_TN_G3.csv"), sep=",")

# Coastal gillnet AU1
# ===================
RCdat<-subset(dat, w_r=="R" & fishery=="C")
RCGNdat<-subset(RCdat, gear=="gillnet" | gear=="Gillnet" | gear=="GILLNET" | 
gear=="other" | gear=="Other" | gear=="OTHER"| gear=="ANGLING")
RCGN1dat<-subset(RCGNdat, unit==1)
dim(RCGN1dat)
# 1426
dat1tot<-subset(RCGN1dat, RecOK==1)
dat2tot<-subset(RCGN1dat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_CGN1<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_CGN1), file=paste(sep="", path, "/Reared_C_GN_G1.csv"), sep=",")

# Coastal gillnet AU2
# ===================
RCdat<-subset(dat, w_r=="R" & fishery=="C")
RCGNdat<-subset(RCdat, gear=="gillnet" | gear=="Gillnet" | gear=="GILLNET" | 
gear=="other" | gear=="Other" | gear=="OTHER"| gear=="ANGLING")
RCGN2dat<-subset(RCGNdat, unit==2)
dim(RCGN2dat)
# 733
dat1tot<-subset(RCGN2dat, RecOK==1)
dat2tot<-subset(RCGN2dat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_CGN2<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_CGN2), file=paste(sep="", path, "/Reared_C_GN_G2.csv"), sep=",")

# Coastal gillnet AU3
# ===================
RCdat<-subset(dat, w_r=="R" & fishery=="C")
RCGNdat<-subset(RCdat, gear=="gillnet" | gear=="Gillnet" | gear=="GILLNET" | 
gear=="other" | gear=="Other" | gear=="OTHER"| gear=="ANGLING")
RCGN3dat<-subset(RCGNdat, unit==3)
dim(RCGN3dat)
# 3347
dat1tot<-subset(RCGN3dat, RecOK==1)
dat2tot<-subset(RCGN3dat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_CGN3<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_CGN3), file=paste(sep="", path, "/Reared_C_GN_G3.csv"), sep=",")

# Coastal unknown gear
# =====================
RCdat<-subset(dat, w_r=="R" & fishery=="C")
RCUdat<-subset(RCdat, gear=="UNKNOWN")
dim(RCUdat)
# 2667
dat1tot<-subset(RCUdat, RecOK==1)
dat2tot<-subset(RCUdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_CU<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_CU), file=paste(sep="", path, "/Reared_C_U.csv"), sep=",")

# Total offshore catch reared
# ############################ 
summary(ROdat$gear)

# Offshore driftnet
# ===================
ROdat<-subset(dat, w_r=="R" & fishery=="O")
RODNdat<-subset(ROdat, gear=="drifnet" | gear=="driftnet"| gear=="Driftnet" | gear=="DRIFTNET"| 
(gear=="gillnet" & rec_year<=2008)| (gear=="GILLNET" & rec_year<=2008)| (gear=="Gillnet" & rec_year<=2008))
dim(RODNdat)
# 16627 
dat1tot<-subset(RODNdat, RecOK==1)
dat2tot<-subset(RODNdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_ODN<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_ODN), file=paste(sep="", path, "/Reared_O_DN.csv"), sep=",")

# Offshore longline
# ===================
ROdat<-subset(dat, w_r=="R" & fishery=="O")
ROLLdat<-subset(ROdat, gear=="longline" | gear=="Longline" | gear=="LONGLINE"| gear=="ANGLING"|
(gear=="gillnet" & rec_year>2008)| (gear=="GILLNET" & rec_year>2008)| (gear=="Gillnet" & rec_year>2008))
dim(ROLLdat)
# 3273
dat1tot<-subset(ROLLdat, RecOK==1)
dat2tot<-subset(ROLLdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_OLL<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_OLL), file=paste(sep="", path, "/Reared_O_LL.csv"), sep=",")

# Offshore unknown gear
# =====================
ROdat<-subset(dat, w_r=="R" & fishery=="O")
ROUdat<-subset(ROdat, gear=="UNKNOWN" | gear=="other" | gear=="Other"| gear=="OTHER")
dim(ROUdat)
# 17615
dat1tot<-subset(ROUdat, RecOK==1)
dat2tot<-subset(ROUdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_OU<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_OU), file=paste(sep="", path, "/Reared_O_U.csv"), sep=",")


# Total river catch reared
# ############################ 
summary(dat$fishery)

# Terminal river fishery
# ===================
RRdat<-subset(dat, w_r=="R" & fishery=="R")
RRterdat<-subset(RRdat, Terminal==1)
dim(RRterdat)
# 4566
dat1tot<-subset(RRterdat, RecOK==1)
dat2tot<-subset(RRterdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_Rter<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_Rter), file=paste(sep="", path, "/Reared_R_ter.csv"), sep=",")

# Reproductive river fishery
# ===================
RRdat<-subset(dat, w_r=="R" & fishery=="R")
RRspdat<-subset(RRdat, Reproductive==1)
dim(RRspdat)
# 783
dat1tot<-subset(RRspdat, RecOK==1)
dat2tot<-subset(RRspdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

R_Rsp<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(R_Rsp), file=paste(sep="", path, "/Reared_R_sp.csv"), sep=",")

# Total catch wild
# ############################ 
WCdat<-subset(dat, w_r=="W")
dim(WCdat)
#2250

# Wild coastal
# ===================
WCdat<-subset(dat, w_r=="W" & fishery=="C")
dim(WCdat)
# 556
dat1tot<-subset(WCdat, RecOK==1)
dat2tot<-subset(WCdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

W_C<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_C), file=paste(sep="", path, "/Wild_C.csv"), sep=",")

# Wild offshore
# ===================
WOdat<-subset(dat, w_r=="W" & fishery=="O")
dim(WOdat)
# 1600
dat1tot<-subset(WOdat, RecOK==1)
dat2tot<-subset(WOdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

W_O<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_O), file=paste(sep="", path, "/Wild_O.csv"), sep=",")

# Wild river
# ===================
WRdat<-subset(dat, w_r=="W" & fishery=="R")
dim(WRdat)
# 83
dat1tot<-subset(WRdat, RecOK==1)
dat2tot<-subset(WRdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

W_R<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_R), file=paste(sep="", path, "/Wild_R.csv"), sep=",")

# Wild unknown
# ===================
WUdat<-subset(dat, w_r=="W" & fishery=="U")
dim(WUdat)
# 11
dat1tot<-subset(WUdat, RecOK==1)
dat2tot<-subset(WUdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

W_U<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_U), file=paste(sep="", path, "/Wild_U.csv"), sep=",")

# T?t? ei kannata p?ivitt??, koska osa merkeist? katoaa v?lille 
#(rec_day=99:ien jako ei toimi koska tunnettuja palautuskuukausia ei ole
# olemassa joka kohortille)

# Total coastal catch wild
# ############################ 
summary(WCdat$gear)

# Coastal driftnet
# ===================
WCdat<-subset(dat, w_r=="W" & fishery=="C")
WCDNdat<-subset(WCdat, gear=="drifnet" | gear=="driftnet" | gear=="Driftnet"| gear=="DRIFTNET")
dim(WCDNdat)
# 22 
dat1tot<-subset(WCDNdat, RecOK==1)
dat2tot<-subset(WCDNdat, RecOK==0)
N1<-RelYearSum(dat1tot)
#N2<-RelYearSum(dat2tot)
#N2<-rep(0, length(N1))

W_CDN<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_CDN), file=paste(sep="", path, "/Wild_C_DN.csv"), sep=",")

# Coastal longline    Skip this. One tag from '89
# ===================
#WCdat<-subset(dat, w_r=="W" & fishery=="C")
#WCLLdat<-subset(WCdat, gear=="longline" | gear=="Longline"| gear=="LONGLINE")
#dim(WCLLdat)
# 1
#dat1tot<-subset(WCLLdat, RecOK==1)
#dat2tot<-subset(WCLLdat, RecOK==0)
#N1<-RelYearSum(dat1tot)
#N2<-RelYearSum(dat2tot)

#summary(WCLLdat)
#W_CLL<-TAGS.TOT(dat1tot, N1, N2)
#write.table(round(W_CLL), file=paste(sep="", path, "/Wild_C_LL.csv"), sep=",")

# Coastal trapnet 
# ===================
# Supposedly this contains all assessment units, although there are no tags in other than AU1
WCdat<-subset(dat, w_r=="W" & fishery=="C")
WCTNdat<-subset(WCdat, gear=="trapnet" | gear=="Trapnet" | gear=="TRAPNET")
# WCTN1dat<-subset(WCTNdat, unit==1)
dim(WCTNdat)
# 291
dat1tot<-subset(WCTNdat, RecOK==1)
dat2tot<-subset(WCTNdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

W_CTN<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_CTN), file=paste(sep="", path, "/Wild_C_TN.csv"), sep=",")

# Coastal gillnet AU1
# ===================
WCdat<-subset(dat, w_r=="W" & fishery=="C")
WCGNdat<-subset(WCdat, gear=="gillnet" | gear=="Gillnet" | gear=="GILLNET" | 
gear=="other" | gear=="Other" | gear=="OTHER")
WCGN1dat<-subset(WCGNdat, unit==1)
dim(WCGN1dat)
dim(WCGNdat)
# 158
dat1tot<-subset(WCGN1dat, RecOK==1)
dat2tot<-subset(WCGN1dat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

W_CGN1<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_CGN1), file=paste(sep="", path, "/Wild_C_GN_G1.csv"), sep=",")

# Coastal unknown gear
# =====================
WCdat<-subset(dat, w_r=="W" & fishery=="C")
WCUdat<-subset(WCdat, gear=="UNKNOWN")
dim(WCUdat)
# 84
dat1tot<-subset(WCUdat, RecOK==1)
dat2tot<-subset(WCUdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

W_CU<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_CU), file=paste(sep="", path, "/Wild_C_U.csv"), sep=",")

# Total offshore catch wild
# ############################ 
summary(WOdat$gear)

# Offshore driftnet
# ===================
WOdat<-subset(dat, w_r=="W" & fishery=="O")
WODNdat<-subset(WOdat, gear=="drifnet" | gear=="driftnet" | gear=="Driftnet" | gear=="DRIFTNET")
dim(WODNdat)
# 582
dat1tot<-subset(WODNdat, RecOK==1)
dat2tot<-subset(WODNdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

W_ODN<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_ODN), file=paste(sep="", path, "/Wild_O_DN.csv"
), sep=",")

# Offshore longline
# ===================
WOdat<-subset(dat, w_r=="W" & fishery=="O")
WOLLdat<-subset(WOdat, gear=="longline" | gear=="Longline" | gear=="LONGLINE")
dim(WOLLdat)
dat1tot<-subset(WOLLdat, RecOK==1)
dat2tot<-subset(WOLLdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

# 253
W_OLL<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_OLL), file=paste(sep="", path, "/Wild_O_LL.csv"), sep=",")

# Offshore unknown gear
# =====================
WOdat<-subset(dat, w_r=="W" & fishery=="O")
WOUdat<-subset(WOdat, gear=="UNKNOWN"| gear=="trapnet"| gear=="Trapnet"| gear=="TRAPNET"| 
gear=="other" | gear=="Other"| gear=="OTHER"| gear=="gillnet" | gear=="Gillnet"| gear=="GILLNET")
dim(WOUdat)
# 765
dat1tot<-subset(WOUdat, RecOK==1)
dat2tot<-subset(WOUdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

W_OU<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_OU), file=paste(sep="", path, "/Wild_O_U.csv"), sep=",")

# Total river catch wild
# ############################ 

WRdat<-subset(dat, w_r=="W" & fishery=="R")
dim(WRdat)
# 83
dat1tot<-subset(WRdat, RecOK==1)
dat2tot<-subset(WRdat, RecOK==0)
N1<-RelYearSum(dat1tot)
N2<-RelYearSum(dat2tot)

W_R<-TAGS.TOT(dat1tot, N1, N2)
write.table(round(W_R), file=paste(sep="", path, "/Wild_R.csv"), sep=",")

