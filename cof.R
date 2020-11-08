


cof<-function(x){ #your data
  
  
  starts<-1
  ends<-length(x)
  
  CI<-CI(x,ci=0.95)
  upper<-CI['upper']
  lower<-CI['lower']

  SD<-sd(x,na.rm = TRUE)
  print(CI)
  cat("SD Value of Given Data: ",SD,"\n" )
  
  result<-list()
  result[['values that > upper bound']]<-numeric()
  result[['values that < lower bound']]<-numeric()
  
  
  while(TRUE){
    
    if(x[[starts]]>upper){
      d=x[starts]
      result[['values that > upper bound']]<-append(result[['values that > upper bound']],d) 
      
    }else if(x[[starts]]<lower){
      d=x[starts]
      result[['values that < lower bound']]<-append(result[['values that < lower bound']],d)
    }
    
    starts<-starts+1
    
    if(starts==ends){
      break
    }
    
  }
  
  
  return(result)
  
  
}

#cofresults<-cof(x) x is your data
