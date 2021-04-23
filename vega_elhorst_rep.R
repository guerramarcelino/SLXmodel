setwd("C:/Users/User/Desktop/SLXmodel")

##################### You will need those packages
libs<-c("xlsx", "plm", "fixest", "pracma")
lapply(libs, require, character.only=TRUE)

##################### Original dataset
#####################
original<-read.xlsx("cigarette+2var.xls", sheetName = "Sheet1", as.data.frame = T, header = T)
coords<-read.xlsx("cigar_states.xls", sheetName = "Sheet1", as.data.frame = T, header = T)
W1<-read.xlsx("Spat-Sym-US.xls", sheetName = "Sheet1", as.data.frame = T, header = F)

## rnormw function
W1<-as.matrix(W1)
W1.sums<-apply(W1,1,sum)
W<-W1/W1.sums

########## SLX - Table 4 Column 1
ciga<-pdata.frame(original, c("state","year"))
ciga<-ciga[order(ciga$year),]
WP<-slag(ciga$logp,W)
WI<-slag(ciga$logy,W)
wx<-cbind(WP, WI)

data<-cbind(ciga, wx)

SLX<-plm(logc~logp+logy+WP+WI, model="within", effect="twoways", data=ciga)
summary(SLX)

########## SLX - Table 4 Column 2

## W matrix based on inverse distance

B<-as.matrix(coords[,2:3])

W<-matrix(NA, 46,46)
for(i in 1:46){
  for(j in 1:46){
    if(i==j) {W[i,j]=0}
    else{W[i,j]=1/sqrt((B[i,1]-B[j,1])^2+(B[i,2]-B[j,2])^2)} 
  }
}

## Normalizing the distance matrix 

lambda<-eigen(W)$values
W=W/max(lambda)

## Subsetting the original data and renaming columns to match the notation in the f_gravity_alpha_fe.R

data2<-original%>%dplyr::select(year, state, logc,logp,logy)
names<-c("time", "id", "y", "x1", "x2")
colnames(data2)<-names

############
#T=30
#N=46

#### You need two packages:

source("f_gravity_alpha_fe.R")

## Authors choice to search lambda from 1 to 20
param<-optim(0.1,param_W, data=data2, x=data2[,4:5],W=W, method="L-BFGS-B", N=46, T=30,lower=1,upper=20)$par
param

## Param is the estimated gamma -> the one that minimizes the SSR of the TWFE
## Having the gamma, create the Wgamma, the respective WXs and run the TWFE again

Wgamma=W^param
lambda<-eigen(Wgamma)$values
Wgamma<-Wgamma/max(lambda)
wx<-matrix(NA,nrow(x),ncol(x))
for(t in 1:T){
  t1=(t-1)*N+1
  t2=t*N
  wx[t1:t2,]=Wgamma%*%as.matrix(data2[,4:5])[t1:t2,]
}

wx<-data.frame(wx)
names<-c("wx1", "wx2")
colnames(wx)<-names
newdata<-cbind(data2, wx)

reg<-feols(y~x1+x2+wx1+wx2| id+time, data=newdata)
summary(reg)
