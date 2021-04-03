param_W<-function(alpha, W, y,x,T,N){
  Walpha=W^alpha
#  lambda<-eigen(Walpha)$values
#  Walpha<-Walpha/max(lambda)
  wx<-matrix(NA,nrow(x),ncol(x)-2)
  for(t in 1:T){
    t1=(t-1)*N+1
    t2=t*N
    wx[t1:t2,]=Walpha%*%as.matrix(x[t1:t2,3:ncol(x)])
  }
  newx<-cbind(x, wx)
  x.twfe<- newx %>%
    group_by(id) %>%
    mutate_at(vars(-id, -time),.funs = funs('u' = . - mean(.)))%>% 
    ungroup()%>%
    group_by(time)%>%
    mutate_at(vars(-id, -time),.funs = funs('t' = . - mean(.)))%>%
    ungroup()%>%
    dplyr::select(ends_with("u_t"))
  y.twfe<-y %>%
    group_by(id) %>%
    mutate_at(vars(-id, -time),.funs = funs('u' = . - mean(.)))%>% 
    ungroup()%>%
    group_by(time)%>%
    mutate_at(vars(-id, -time),.funs = funs('t' = . - mean(.)))%>%
    ungroup()%>%
    dplyr::select(ends_with("u_t"))
  ols<-lm(as.matrix(y.twfe)~-1+as.matrix(x.twfe))
  return(t(resid(ols))%*%resid(ols))
}
