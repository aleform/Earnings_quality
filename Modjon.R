library(readxl)
sink(paste("Downloads/MJ_TA_out",".txt",sep=""),append = TRUE)
pdf("Downloads/resiudalplot.pdf") 
for (z in 1:12){
  nam<-paste('df',z,sep="")
aida=get(nam)
#aida=na.omit(aida)
RevminRec=c()
for (i in 0:6){
  temp=(aida[4+i]-aida[5+i])-(((aida[52+i]+aida[60+i])-(aida[53+i]+aida[61+i])))  
  RevminRec=c(RevminRec,temp)
}
for (i in 20:14){
  DCA=(aida[paste("ATTCIR",i,sep="")]+aida[paste("RATERIS",i,sep="")])-(aida[paste("ATTCIR",i-1,sep="")]+aida[paste("RATERIS",i-1,sep="")])
  Dcash=aida[paste("CASH",i,sep="")]-aida[paste("CASH",i-1,sep="")]
  Dequiv=aida[paste("TOTATTFIN",i,sep="")]-aida[paste("TOTATTFIN",i-1,sep="")]
  #DCL=aida[paste("DEBBREV",i,sep="")]-aida[paste("DEBBREV",i-1,sep="")]
  DSTD=aida[paste("Bank",i,sep="")]-aida[paste("Bank",i-1,sep="")]+aida[paste("Otherfin",i,sep="")]-aida[paste("Otherfin",i-1,sep="")]
  DCL=DSTD+aida[paste("PAY",i,sep="")]-aida[paste("PAY",i-1,sep="")]+aida[paste("TAXFUN",i,sep="")]-aida[paste("TAXFUN",i-1,sep="")]+aida[paste("RATERISPASS",i,sep="")]-aida[paste("RATERISPASS",i-1,sep="")]+aida[paste("SSF",i,sep="")]-aida[paste("SSF",i-1,sep="")]+aida[paste("DOWNPAY",i,sep="")]-aida[paste("DOWNPAY",i-1,sep="")]
  Depr=aida[paste("Dep",i,sep="")]
  Totalaccr=DCA-Dcash-Dequiv-(DCL-DSTD)-Depr
  nam<-paste('Totalaccr',i,sep="")
assign(nam, Totalaccr)
}

Totalaccr=data.frame(Totalaccr20,Totalaccr19,Totalaccr18,Totalaccr17,Totalaccr16,Totalaccr15,Totalaccr14)

Rev=data.frame(aida$REV20,aida$REV19,aida$REV18,aida$REV17,aida$REV16,aida$REV15,aida$REV14)
PPE=data.frame(aida$PPE20,aida$PPE19,aida$PPE18,aida$PPE17,aida$PPE16,aida$PPE15,aida$PPE14)
TotAsset=data.frame(aida$TA19,aida$TA18,aida$TA17,aida$TA16,aida$TA15,aida$TA14,aida$TA13)
RevminRec=data.frame(RevminRec)
Rev=data.frame(Rev)
TotalaccrR=Totalaccr/TotAsset
RevminRecR=RevminRec/TotAsset
RevR=Rev/TotAsset
PPER=PPE/TotAsset
colnames(TotalaccrR)=c('20','19','18','17','16','15','14')
beta=matrix(nrow = 7,ncol = 10)
NDAccR=data.frame('20'=double(nrow(aida)),'19'=double(nrow(aida)),'18'=double(nrow(aida)),'17'=double(nrow(aida)),'16'=double(nrow(aida)),'15'=double(nrow(aida)),'14'=double(nrow(aida)))
Dacc=data.frame('20'=double(nrow(aida)),'19'=double(nrow(aida)),'18'=double(nrow(aida)),'17'=double(nrow(aida)),'16'=double(nrow(aida)),'15'=double(nrow(aida)),'14'=double(nrow(aida)))

for (i in 1:7)
{
  lower_bound <- quantile(TotalaccrR[,i], 0.01,na.rm = TRUE)
  upper_bound <- quantile(TotalaccrR[,i], 0.99,na.rm = TRUE)
  outlier_ind <- which(TotalaccrR[,i] < lower_bound | TotalaccrR[,i] > upper_bound)
  Dacc=Dacc[-outlier_ind,]
  NDAccR=NDAccR[-outlier_ind,]
  TotalaccrR=TotalaccrR[-outlier_ind,]
  RevminRecR=RevminRecR[-outlier_ind,]
  PPER=PPER[-outlier_ind,]
  RevR=RevR[-outlier_ind,]
}

print(paste('Settore',z))
for (i in 1:7){
df <- data.frame(TotalaccrR[i],RevminRecR[i],PPER[i])
df=na.omit(df)
colnames(df)<-c('Y','X1','X2')
temp=lm((Y)~(X1)+(X2),data=df)
temp.res=resid(temp)
corrmatr=cor(df$Y,temp$residuals)
colnames(beta)<-c('Intercept','X1','X2','R2','DAccPlusDivN','NPlus','DAccMinDivN','NMin','DAccABSmean','Nobs')
rownames(beta)<-c('20','19','18','17','16','15','14')
NDAccR[,i]=temp$coefficients[1]+temp$coefficients[2]*RevminRecR[,i]+temp$coefficients[3]*PPER[,i]
Dacc[,i]=TotalaccrR[,i]-NDAccR[,i]
colnames(Dacc)=c('20','19','18','17','16','15','14')
beta[i,]=c(temp$coefficients[1],temp$coefficients[2],temp$coefficients[3],summary(temp)$r.squared,mean(Dacc[,i][which(Dacc[,i]>0)],na.rm=TRUE),length(Dacc[,i][which(Dacc[,i]>0)]),mean(Dacc[,i][which(Dacc[,i]<0)],na.rm=TRUE),length(Dacc[,i][which(Dacc[,i]<0)]),mean(abs(Dacc[,i]),na.rm = TRUE),summary(temp)$df[2]+3)
taxcorr=cor(Dacc[,1][which(Dacc[,i]<0)],aida$INCOMETAX20[which(Dacc[,i]<0)],use = "complete.obs")
print(paste("Corr Disc and Tax:", taxcorr))
}

boxplot(TotalaccrR)
print(beta)
rm(list=ls()[-grep("df",ls())])
}
dev.off() 
sink()

