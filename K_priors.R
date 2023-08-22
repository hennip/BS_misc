library(rjags)
library(runjags)
source("C:/tmp/path-main.r")

M1<-"model{
# original Laura's priors
  expert~dcat(p[]) # Prior for the experts... 

for(i in 1:5){
  p[i]<-1/5 # ...is uniform 
}

for (r in 1:13){
  R[r]<- CC[r]/1000
  CC[r]<-exp(LCC[r]) # From log-scale to the original scale 
  LCC[r]~dnorm(MM[r],tau[r]) # Normal distribution on log-scale 
  MM[r]<-param[1,r,expert] # param[1,,]: Mean on the log scale 
  tau[r]<-1/pow(param[2,r,expert],2) # param[2,,]: standard deviation on log scale 
}

R[14]~dlnorm(4.53, 35.1) # Emån
R[15]~dlnorm(2.73, 39.56) # Mörrumsån


#current priors (2023)
for(s in 1:17){
  K[s]~dlnorm(M_R0[s],tau_R0[s])     #M_R0 and tau_R0 come from the data file K_prior
}

}"


#K prior
carrying_capacity<-as.matrix(read.table(paste0(pathMain,"WGBAST_shared/data/data_2023/","K_prior.txt"),header=T))
M_R0<-carrying_capacity[,1]
tau_R0<-carrying_capacity[,2]

param<-array(NA,dim=c(2,13,5))
param[,1,1]<-c(13.02937668,0.405438463)
param[,2,1]<-c(10.82946914,0.577329167)
param[,3,1]<-c(12.42272175,0.392144772)
param[,4,1]<-c(10.21396871,0.603853193)
param[,5,1]<-c(10.51845666,0.493061009)
param[,6,1]<-c(9.429819457,0.567223364)
param[,7,1]<-c(11.36477683,0.581614058)
param[,8,1]<-c(8.300464644,0.497022839)
param[,9,1]<-c(8.025178409,0.655649354)
param[,10,1]<-c(11.87215219,0.608565891)
param[,11,1]<-c(8.886427511,0.411856937)
param[,12,1]<-c(9.576551693,0.557688179)
param[,13,1]<-c(8.118883415,0.629789638)
param[,1,2]<-c(15.13694468,0.477250937)
param[,2,2]<-c(12.28598492,0.370940044)
param[,3,2]<-c(14.00055739,0.090909504)
param[,4,2]<-c(11.55797549,0.447450569)
param[,5,2]<-c(11.2311541,0.546013881)
param[,6,2]<-c(11.3163421,0.319350094)
param[,7,2]<-c(13.07922425,0.450788934)
param[,8,2]<-c(10.04408273,0.175160165)
param[,9,2]<-c(10.10929845,0.122165899)
param[,10,2]<-c(13.59143354,0.379224354)
param[,11,2]<-c(11.63357403,0.31756251)
param[,12,2]<-c(11.99745803,0.456641863)
param[,13,2]<-c(9.866985347,0.18403447)
param[,1,3]<-c(14.15497837,0.530198602)
param[,2,3]<-c(11.22324493,0.509423103)
param[,3,3]<-c(13.34210356,0.412903743)
param[,4,3]<-c(11.07569483,0.589667346)
param[,5,3]<-c(10.66159723,0.657707518)
param[,6,3]<-c(10.07125319,0.213135445)
param[,7,3]<-c(12.38157527,0.409551124)
param[,8,3]<-c(9.545944541,0.532558424)
param[,9,3]<-c(8.100075717,0.614209415)
param[,10,3]<-c(12.4367747,0.656231643)
param[,11,3]<-c(10.03034201,0.212289202)
param[,12,3]<-c(10.9881643,0.43618946)
param[,13,3]<-c(8.843360347,0.747166632)
param[,1,4]<-c(13.79957401,0.883552727)
param[,2,4]<-c(10.94243968,0.657223702)
param[,3,4]<-c(13.00752056,0.707041133)
param[,4,4]<-c(10.77420858,0.779672296)
param[,5,4]<-c(10.89108515,0.967388401)
param[,6,4]<-c(9.07038116,0.768280836)
param[,7,4]<-c(11.49923995,0.761022901)
param[,8,4]<-c(8.617841126,0.933280738)
param[,9,4]<-c(9.464023604,0.223146798)
param[,10,4]<-c(11.86613047,0.973703557)
param[,11,4]<-c(9.977474278,0.342133959)
param[,12,4]<-c(10.02788899,0.342125262)
param[,13,4]<-c(7.889100784,0.97622951)
param[,1,5]<-c(14.14599604,0.315846366)
param[,2,5]<-c(11.06333179,1.042517948)
param[,3,5]<-c(14.18005474,0.548381067)
param[,4,5]<-c(11.19852836,1.049923169)
param[,5,5]<-c(10.89725766,1.508341073)
param[,6,5]<-c(9.961269148,0.617660065)
param[,7,5]<-c(12.04681777,0.849026924)
param[,8,5]<-c(9.646569809,0.241444276)
param[,9,5]<-c(8.221816621,1.441811044)
param[,10,5]<-c(12.1539576,1.269085226)
param[,11,5]<-c(9.928685088,0.841062046)
param[,12,5]<-c(10.39438439,0.668182163)
param[,13,5]<-c(9.049351591,0.780976787)

data<-list(param=param, M_R0=M_R0, tau_R0=tau_R0)

var_names<-c("R", "K")


run1 <- run.jags(M1,
                 monitor= var_names,data=data,#inits = inits,
                 n.chains = 2, method = 'parallel', thin=1, burnin =10000,
                 modules = "mix",keep.jags.files=F,sample =10000, adapt = 1000,
                 progress.bar=TRUE)

summary(run1)
chains<-as.mcmc(run1)
summary(chains, quantiles = c(0.05,0.25,0.5,0.75,0.95))

par(mfrow=c(5,3),mar=c(2.5,4,4,1))
plot(density(chains[,"R[1]"]), ylim=c(0,0.001), xlim=c(0,3000), main="Torne")
lines(density(chains[,"K[1]"]), lwd=2)
plot(density(chains[,"R[2]"]), ylim=c(0,0.02), xlim=c(0,200), main="Simo")
lines(density(chains[,"K[2]"]), lwd=2)
plot(density(chains[,"R[3]"]), ylim=c(0,0.002), xlim=c(0,2500), main="Kalix")
lines(density(chains[,"K[3]"]), lwd=2)
plot(density(chains[,"R[4]"]), ylim=c(0,0.02), xlim=c(0,200), main="Råne")
lines(density(chains[,"K[4]"]), lwd=2)
plot(density(chains[,"R[5]"]), ylim=c(0,0.02), xlim=c(0,500), main="Pite")
lines(density(chains[,"K[5]"]), lwd=2)
plot(density(chains[,"R[6]"]), ylim=c(0,0.06), xlim=c(0,50), main="Åby")
lines(density(chains[,"K[6]"]), lwd=2)
plot(density(chains[,"R[7]"]), ylim=c(0,0.01), xlim=c(0,400), main="Byske")
lines(density(chains[,"K[7]"]), lwd=2)
plot(density(chains[,"R[8]"]), ylim=c(0,0.12), xlim=c(0,50), main="Rickle")
lines(density(chains[,"K[8]"]), lwd=2)
plot(density(chains[,"R[9]"]), ylim=c(0,0.1), xlim=c(0,50), main="Sävarån")
lines(density(chains[,"K[9]"]), lwd=2)
plot(density(chains[,"R[10]"]), ylim=c(0,0.005), xlim=c(0,1000), main="Vindel")
lines(density(chains[,"K[10]"]), lwd=2)
plot(density(chains[,"R[11]"]), ylim=c(0,0.04), xlim=c(0,150), main="Öre")
lines(density(chains[,"K[11]"]), lwd=2)
plot(density(chains[,"R[12]"]), ylim=c(0,0.03), xlim=c(0,150), main="Lögde")
lines(density(chains[,"K[12]"]), lwd=2)
plot(density(chains[,"R[13]"]), ylim=c(0,0.2), xlim=c(0,20), main="Ljungan")
lines(density(chains[,"K[13]"]), lwd=2)
plot(density(chains[,"R[14]"]), ylim=c(0,0.04), xlim=c(0,250), main="Mörrum")
lines(density(chains[,"K[14]"]), lwd=2)
plot(density(chains[,"R[15]"]), ylim=c(0,0.2), xlim=c(0,80), main="Emån")
lines(density(chains[,"K[15]"]), lwd=2)




