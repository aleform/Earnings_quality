sink(paste("Downloads/DefondandP",".txt",sep=""),append = TRUE)
for (z in 1:12){
  nam<-paste('df',z,sep="")
  aida=get(nam)
  for (i in 20:13){
    nam<-paste('WC',i,sep="")
    CA=(aida[paste("ATTCIR",i,sep="")]+aida[paste("RATERIS",i,sep="")])
    Cash=aida[paste("CASH",i,sep="")]
    Equiv=aida[paste("TOTATTFIN",i,sep="")]
    STD=aida[paste("Bank",i,sep="")]+aida[paste("Otherfin",i,sep="")]
    CL=STD+aida[paste("PAY",i,sep="")]+aida[paste("TAXFUN",i,sep="")]+aida[paste("RATERISPASS",i,sep="")]+aida[paste("SSF",i,sep="")]+aida[paste("DOWNPAY",i,sep="")]
    Totalaccrt=CA-Cash-Equiv-(CL-STD)
    assign(nam,Totalaccrt)
  }
  
  WC=data.frame(WC20,WC19,WC18,WC17,WC16,WC15,WC14,WC13)
  Rev=data.frame(aida$REV20,aida$REV19,aida$REV18,aida$REV17,aida$REV16,aida$REV15,aida$REV14,aida$REV13)
  AWCA=data.frame('20'=double(dim(WC)[1]),'19'=double(dim(WC)[1]),'18'=double(dim(WC)[1]),'17'=double(dim(WC)[1]),'16'=double(dim(WC)[1]),'15'=double(dim(WC)[1]),'14'=double(dim(WC)[1]))
  for (i in 1:7){
  AWCA[,i]=WC[,i]-((WC[,i+1]/Rev[,i+1])*Rev[,i])
  }
  TotAsset=data.frame(aida$TA19,aida$TA18,aida$TA17,aida$TA16,aida$TA15,aida$TA14,aida$TA13)
  AWCAR=AWCA/TotAsset
  for (i in 1:7){
  naind=which(abs(AWCAR[,i])==(Inf))
  AWCAR[,i][naind]=NA
  }
  for (i in 1:7)
  {
    lower_bound <- quantile( AWCAR[,i], 0.05,na.rm = TRUE)
    upper_bound <- quantile(AWCAR[,i], 0.95,na.rm = TRUE)
    outlier_ind <- which(AWCAR[,i] < lower_bound | AWCAR[,i] > upper_bound)
    AWCAR=AWCAR[-outlier_ind,]
  }
  print(paste("Settore",z))
  print(mean(colMeans(abs(AWCAR), na.rm = TRUE)))
  rm(list=ls()[-grep("df",ls())])
}
sink()
  