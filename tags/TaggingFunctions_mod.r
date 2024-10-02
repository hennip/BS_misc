## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		Baltic salmon stock assessment (WGBAST)

# Contents:		Functions needed in TaggingDataWithLags.r
# R-file:		  TaggingFunctions.R

# input: 		  none
# output:  		none

# R ver:	  	2.13.2

# programmed:	2012 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
minYear<-1987
maxYear<-2012
years<-c(minYear:maxYear)

# pick the last two from which data is available 
#(at spring 2013 we pick 25=2011 and 26=2012)
# Lags are calculated for these (release)years
Y2<-25
Y1<-26

# Calculate release year specific sums from the dataset 
RelYearSum<-function(dat){
  N<-vector()
  for(y in 1:length(minYear:maxYear)){
    apu<-0
    for(i in 1:length(dat$YEAR)){
      if(dat$YEAR[i]==(y+1986)){apu<-apu+1}
    }
  N[y]<-apu  
  }
  return(N)
}


# Function to make table about the amount of tags
TAGS.TOT<-function(dat1,sum1,sum2){

  Table<- array(NA,dim= c((2*length(years)), length(years)))

  # These produce names for columns and rows
  HYR<-rep(c(1,2), length(years))
  YEAR<-vector()
  for(y in minYear:maxYear){
    ifelse( y==minYear, YEAR<-c(rep(y,2)),
      YEAR<-c(YEAR,rep(y,2)))
  }
  YEAR<-c(0,YEAR)
  HYR<-c(0,HYR)

  for(i in 1:length(years)){ # release year
    for(j in 1:length(years)){  # recapture year
      for(k in 1:2){    # half year
        temp<-0
        temp2<-0

        for(r in 1:(dim(dat1)[1])){
          if(dat1$YEAR[r]==(i+1986)){
            if(dat1$RECyear[r]==(j+1986)){
              if(dat1$HYR[r]==k){
                temp<-temp+1
              }
            }
          }
        }
        
        # The proportion of those tags that have real recapture date
        # and that are caught in year j, half year k
        weight<-temp/sum1[i]

        # Add lagged tags for last two years 
        if(j==Y2){temp2<-temp*0.19}
        if(j==Y1){temp2<-temp*0.07}

        Table[((j*2)-(2-k)),i]<-temp+weight*sum2[i]+temp2
                                                                 
      }
    }
  }

  Table<-rbind(years, Table)
  Table<-cbind(YEAR, HYR, Table)

  return(Table)
}





