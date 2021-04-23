param_W<-function(alpha, W, data,x,T,N){
  Walpha=W^alpha
  #  lambda<-eigen(Walpha)$values
  #  Walpha<-Walpha/max(lambda)
  wx<-matrix(NA,nrow(x),ncol(x))
  for(t in 1:T){
    t1=(t-1)*N+1
    t2=t*N
    wx[t1:t2,]=Walpha%*%as.matrix(x)[t1:t2,]
  }
  wx<-data.frame(wx)
  names<-c("wx1", "wx2")
  colnames(wx)<-names
  newdata<-cbind(data, wx)
  reg<-feols(y~x1+x2+wx1+wx2| id+time, data=newdata)
  vec<-c(reg$residuals%*%reg$residuals)
  return(vec)
}
