library(readxl)
aida <- read_excel("Downloads/Aida_Export_9.xlsx", sheet = "Risultati")
colnames(aida)[3] <- 'ff12'

int = c()
for (i in 1:length(aida$ff12)) {
  x <- as.integer(aida$ff12[i])
  int = c(int,x)
}

aida$ff12 = int


f1 <- c(100:999, 2000:2399,2700:2749,2770:2799,3100:3199,3940:3989)
f2 <- c(2500:2519,2590:2599,3630:3659,3710:3711,3714:3714,3716:3716,3750:3751,3792:3792,3900:3939,3990:3999) 
f3 <- c(2520:2589,2600:2699,2750:2769,3000:3099,3200:3569,3580:3629,3700:3709,3712:3713,3715:3715,3717:3749,3752:3791,3793:3799,3830:3839,3860:3899)
f4 <- c(1200:1399,2900:2999)
f5 <- c(2800:2829,2840:2899)
f6 <- c(3570:3579,3660:3692,3694:3699,3810:3829,7370:7379)
f7 <- c(4800:4899)
f8 <- c(4900:4949)
f9 <- c(5000:5999,7200:7299,7600:7699)
f10 <- c(2830:2839,3693:3693,3840:3859,8000:8099)
f11 <- c(6000:6999)

df1 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df1)=colnames(aida)
df2 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df2)=colnames(aida)
df3 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df3)=colnames(aida)
df4 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df4)=colnames(aida)
df5 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df5)=colnames(aida)
df6 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df6)=colnames(aida)
df7 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df7)=colnames(aida)
df8 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df8)=colnames(aida)
df9 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df9)=colnames(aida)
df10 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df10)=colnames(aida)
df11 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df11)=colnames(aida)
df12 <- data.frame(matrix(0, ncol = ncol(aida), ))
colnames(df12)=colnames(aida)




for (i in 1:length(aida$ff12)) {
  if(aida$ff12[i] %in% f1){
    aida$ff12[i] <- 1
    df1=rbind(df1,aida[i,])
  }
  else if(aida$ff12[i] %in% f2){
    aida$ff12[i] <- 2
    df2=rbind(df2,aida[i,])
  }
  else if(aida$ff12[i] %in% f3){
    aida$ff12[i] <- 3
    df3=rbind(df3,aida[i,])
  }
  else if(aida$ff12[i] %in% f4){
    aida$ff12[i] <- 4
    df4=rbind(df4,aida[i,])
  }
  else if(aida$ff12[i] %in% f5){
    aida$ff12[i] <- 5
    df5=rbind(df5,aida[i,])
  }
  else if(aida$ff12[i] %in% f6){
    aida$ff12[i] <- 6
    df6=rbind(df6,aida[i,])
  }
  else if(aida$ff12[i] %in% f7){
    aida$ff12[i] <- 7
    df7=rbind(df7,aida[i,])
  }
  else if(aida$ff12[i] %in% f8){
    aida$ff12[i] <- 8
    df8=rbind(df8,aida[i,])
  }
  else if(aida$ff12[i] %in% f9){
    aida$ff12[i] <- 9
    df9=rbind(df9,aida[i,])
  }
  else if(aida$ff12[i] %in% f10){
    aida$ff12[i] <- 10
    df10=rbind(df10,aida[i,])
  }
  else if(aida$ff12[i] %in% f11){
    aida$ff12[i] <- 11
    df11=rbind(df11,aida[i,])
  }
  else{
    aida$ff12[i] <- 12
    df12=rbind(df12,aida[i,])
  }
}

df1=df1[-1,]
df2=df2[-1,]
df3=df3[-1,]
df4=df4[-1,]
df5=df5[-1,]
df6=df6[-1,]
df7=df7[-1,]
df8=df8[-1,]
df9=df9[-1,]
df10=df10[-1,]
df11=df11[-1,]
df12=df12[-1,]
