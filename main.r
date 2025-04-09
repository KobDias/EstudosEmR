mdc<-function(x,y){
  if((x>y)==TRUE){
    menor<-y
  }else{
    menor<-x
  }
  for(i in 1:menor){
    if((x%%i==0)&(y%%i==0)){
      mdc<-i
    }
  }
  return(mdc)
}

mmc<-function(y,x){
  if((y>x)==TRUE){
    maior<-y
  }else{
    maior<-x
  }
  while(TRUE){
    if((maior%%x==0) & (maior%%y==0)){
      mmc=maior
      break
    }
    maior=maior+1
  }
  return(mmc)
}

{a=as.numeric(readline("digite o primeiro numero:"))
b=as.numeric(readline("digite o segundo numero:"))
paste("O MMC é", mmc(a,b), "O MDC é", mdc(a,b))}
