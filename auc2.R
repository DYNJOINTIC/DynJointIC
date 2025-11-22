
AUC_ic3<-function(s,th_v,data3.id,survp,survt) {

  nn=nrow(data3.id);
  nr=ncol(survp)
  nr0=nr-1
  surv0=mark2=rep(0,nn)
  TL1=data3.id$TL
  TR1=data3.id$TR

  delta=ifelse(TR1==Inf,0,1)


  survp0=rep(0,nn)

  for(i in 1:nn) {
    if(s<survt[1]) surv0[i]=1
    for(j in 2:nr) {
      if(survt[1]<s&s<=survt[j]) {survp0[i]=survp[i,j]; break}
    }
  }

  mark2=matrix(0,nn,4)
  for(k in 1:4){
    th=th_v[k]+s
    for(i in 1:nn){
      for(j in 2:nr){
        if(survt[j]<th)  mark2[i,k]=survp[i,j]/survp0[i]
      }
    }
    if(th<survt[2])  mark2[1:nn,k]=survp[1:nn,1]/survp0[i]
  }

  auc1<-rep(0,4)
  n1=nn-1
  for(k in 1:4){
    tot1=tot11=tot2=tot22=tot3=tot33=tot4=tot44=tot5=tot6=tot55=tot66=0
    tot7=tot8=tot9=tot10=tot111=tot12=tot77=tot88=tot99=tot1010=tot11111=tot1212=0
    ind1=ind2=ind3=ind4=ind5=ind6=ind7=ind8=ind9=ind10=ind11=ind12=ind=0
    th=th_v[k]+s
    for(i in 1:n1){
      i2=i+1
       if(TR1[i]>s){
      for(j in i2:nn){
         if(TR1[j]>s){
        ind=ind+1
        
        if(s<=TL1[i]&TR1[i]<=th){
          if(th<=TL1[j])                         {tot1=tot1+1;   ind1=ind1+1 ;       if(mark2[i,k]<mark2[j,k]) tot11=tot11+1 }
          if(s<=TL1[j]&TL1[j]<=th&th<TR1[j])     {tot2=tot2+mark2[j]; ind2=ind2+1 ;  if(mark2[i,k]<mark2[j,k]) tot22=tot22+mark2[j,k]}
          if(TL1[j]<s&th<TR1[j])                 {tot3=tot3+mark2[j]; ind3=ind3+1;   if(mark2[i,k]<mark2[j,k]) tot33=tot33+mark2[j,k]} 
          
        }
        else if(s<TL1[i]&TL1[i]<th&th<TR1[i]) {
          if(th<=TL1[j])                         {tot4=tot4+(1-mark2[i,k]); ind4=ind4+1;            if(mark2[i,k]<mark2[j,k]) tot44=tot44+(1-mark2[i,k])}
          if(s<=TL1[j]&TL1[j]<=th&th<TR1[j])     {tot5=tot5+(1-mark2[i,k])*mark2[j,k]; ind5=ind5+1; if(mark2[i,k]<mark2[j,k]) tot55=tot55+(1-mark2[i,k])*mark2[j,k]}
          if(TL1[j]<s&th<TR1[j])                 {tot6=tot6+(1-mark2[i,k])*mark2[j,k]; ind6=ind6+1; if(mark2[i,k]<mark2[j,k]) tot66=tot66+(1-mark2[i,k])*mark2[j,k]}
        }
        
        else if(TL1[i]<s&TR1[i]<th){
          if(th<=TL1[j])                        {tot7=tot7+surv0[i];   ind7=ind7+1;           if(mark2[i,k]<mark2[j,k]) tot77=tot77+surv0[i]}
          if(s<=TL1[j]&TL1[j]<=th&th<TR1[j])    {tot8=tot8+surv0[i]*mark2[j,k] ; ind8=ind8+1; if(mark2[i,k]<mark2[j,k]) tot88=tot88+surv0[i]*mark2[j,k]}
          if(TL1[j]<s&th<TR1[j])                {tot9=tot9+surv0[i]*mark2[j,k] ; ind9=ind9+1; if(mark2[i,k]<mark2[j,k]) tot99=tot99+surv0[i]*mark2[j,k]}
        }
        else if(TL1[i]<s&th<TR1[i]){
          if(th<=TL1[j])                        {tot10=tot10+surv0[i];   ind10=ind10+1;           if(mark2[i,k]<mark2[j,k]) tot1010=tot1010+surv0[i]}
          if(s<=TL1[j]&TL1[j]<=th&th<TR1[j])    {tot111=tot111+surv0[i]*mark2[j,k] ; ind11=ind11+1; if(mark2[i,k]<mark2[j,k]) tot11111=tot11111+surv0[i]*mark2[j,k]}
          if(TL1[j]<s&th<TR1[j])                {tot12=tot12+surv0[i]*mark2[j,k] ; ind12=ind12+1; if(mark2[i,k]<mark2[j,k]) tot1212=tot1212+surv0[i]*mark2[j,k]}
        }
         }
      } 
      }
    }
  
    auc_num=(tot11+tot22+tot33+tot44+tot55+tot66+tot77+tot88+tot99+tot1010+tot1111+tot1212)
    auc_den=(tot1+tot2+tot3+tot4+tot5+tot6+tot7+tot8+tot9+tot10+tot11+tot12)
    auc1[k]=auc_num/auc_den
  }
print (auc1)
  return(list("auc1"=auc1))
}
############
