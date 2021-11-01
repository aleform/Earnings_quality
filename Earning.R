library(readxl)
sink(paste("Downloads/earnings_graph",".txt",sep=""),append = TRUE)
pdf("Downloads/earnings_graph.pdf") 
for (z in 1:12){
  nam<-paste('df',z,sep="")
  aida=get(nam)
  beta=matrix(nrow = 7,ncol = 6)
  colnames(beta)<-c('Intercept','PvInt','X1','PvX1','R2','Nobs')
  rownames(beta)<-c('20','19','18','17','16','15','14')
  NI=data.frame(aida$NI20,aida$NI19,aida$NI18,aida$NI17,aida$NI16,aida$NI15,aida$NI14,aida$NI13)
  TotAsset=data.frame(aida$TA19,aida$TA18,aida$TA17,aida$TA16,aida$TA15,aida$TA14,aida$TA13)
  NITAR=NI[,1:7]/TotAsset
  NITAt1R=NI[2:8]/TotAsset
  for (i in 1:7){
  lower_bound <- quantile(NITAR[,i], 0.05,na.rm = TRUE)
  upper_bound <- quantile(NITAR[,i], 0.95,na.rm = TRUE)
  outlier_ind <- which(NITAR[,i] < lower_bound | NITAR[,i] > upper_bound)
  NITAR=NITAR[-outlier_ind,]
  NITAt1R=NITAt1R[-outlier_ind,]
  }
  for (i in 2:7){
  df <- data.frame(NITAR[i],NITAt1R[i])
  colnames(df)<-c('Y','X1')
  form=Y~X1
  temp=lm(form,data = df)
  beta[i,]=c(temp$coefficients[1],summary(temp)$coefficients[,4][1],temp$coefficients[2],summary(temp)$coefficients[,4][2],summary(temp)$r.squared,summary(temp)$df[2]+2)
  }
  x=as.numeric(unlist(NITAR[1:7]))
  hist(x,breaks = 200)
  print(paste('Settore',z))
  print(beta)
  print(mean(beta[,3], na.rm=TRUE))
  rm(list=ls()[-grep("df",ls())])
}
sink()
dev.off()

#NITOT=aida[155:161]/aida[92:98]
#for (i in 1:7){
#  lower_bound <- quantile(NITOT[,i], 0.01,na.rm = TRUE)
#  upper_bound <- quantile(NITOT[,i], 0.99,na.rm = TRUE)
#  outlier_ind <- which(NITOT[,i] < lower_bound | NITOT[,i] > upper_bound)
#  NITOT=NITOT[-outlier_ind,]
#}
#for (i in 1:7){
#  b=c("2019","2018","2017","2016","2015","2014","2013")
#  x=as.numeric(unlist(NITOT[i]))
#  hist(x,breaks = 2000,main = paste("Net income divided by total asset",b[i]))
#}

  