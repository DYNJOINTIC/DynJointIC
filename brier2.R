
brier3<-function(data.id3,survt,survp,s,th_v) {

  nn=nrow(data.id3)
  nr=length(survt)
  mt=length(th_v)

  TL1=data.id3$TL; TR1=data.id3$TR

  delta=ifelse(TR1==Inf,0,1)

  
  surv0=surv1=rep(0,nn)
  for(i in 1:nn) {
    if(s<survt[1]) surv0[i]=1
    for(j in 2:nr) {
      if(s<survt[j]) {surv0[i]=survp[i,j-1]; break}
    }
    for(j in 2:nr) {
      if(TL1[i]<survt[j]) {surv1[i]=survp[i,j-1]; break}
    }
  }
  
  
  mark2=mark22=matrix(0,nn,4) 
  for(k in 1:4){
    th=th_v[k]+s
     for(j in 2:length(survt)){
      if(survt[j-1]<th& th<survt[j]) { 
        for(i in 1:nn){
          mark2[i,k]=survp[i,j-1]/surv0[i]
        } 
      }
      if(th<survt[1]) mark2[1:nn,k]=survp[1:nn,1]
    }
  }
  
  
  bs<-rep(0,4)
  for(k in 1:4){
    th=th_v[k]+s
    tot=0; tot1=tot2=tot3=tot4=tot5=tot6=0; ind1=ind2=ind3=ind4=ind5=ind6=0
    for(i in 1:nn){
       if(s<TR1[i]){
      if(s<=TL1[i])                            {
        tot=tot+1
        if(th<=TL1[i])                                 {tot1=tot1+(1-mark2[i,k])^2; ind1=ind1+1}
        else if(TL1[i]<th&th<=TR1[i]&delta[i]==1)      {tot2=tot2+(1-mark2[i,k])*(0-mark2[i,k])^2+mark2[i,k]*(1-mark2[i,k])^2; ind2=ind2+1}
        else if(TR1[i]<th)                             {tot3=tot3+(0-mark2[i,k])^2 ;ind3=ind3+1}
        else if(TL1[i]<th&delta[i]==0)                 {tot4=tot4+mark2[i,k]*(1-mark2[i,k])^2+(1-mark2[i,k])*(0-mark2[i,k])^2 ; ind4=ind4+1}
      }
      if(TL1[i]<s&TR1[i]<th)                           {tot5=tot5+surv1[i]*(0-mark2[i,k])^2;    ind5=ind5+surv1[i]}
      if(TL1[i]<s&th<TR1[i])                           {tot6=tot6+surv1[i]*((1-mark2[i,k])^2*mark2[i,k]+mark2[i,k]^2*(1-mark2[i,k]));    ind6=ind6+surv1[i]}    
     
    }}
    bs[k]=(tot1+tot2+tot3+tot4+tot5+tot6)/(ind1+ind2+ind3+ind4+ind5+ind6)
        }

  return(list("bs"=bs))

}
