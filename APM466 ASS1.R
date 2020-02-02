# data 
install.packages("jrvFinance")
library("jrvFinance")
data<-read.csv("/Users/mingyangqin/Desktop/ZSS/APM466/ASS1/466 data modified.csv")
data$maturity.date<-as.Date(data$maturity.date,"%m/%d/%Y")
data$Issue.date<-as.Date(data$Issue.date,"%m/%d/%Y")
data$coupon<-as.numeric(sub("%", "",data$coupon,fixed=TRUE))/100
data<-data[order(data$maturity.date,decreasing = FALSE), ]

for (i in 1:10) {
  data$Jan.2[i]<-(data$Jan.2[i]+4/12*100*data$coupon[i])
  data$Jan.3[i]<-(data$Jan.3[i]+4/12*100*data$coupon[i])
  data$Jan.6[i]<-(data$Jan.6[i]+4/12*100*data$coupon[i])
  data$Jan.7[i]<-(data$Jan.7[i]+4/12*100*data$coupon[i])
  data$Jan.8[i]<-(data$Jan.8[i]+4/12*100*data$coupon[i])
  data$Jan.9[i]<-(data$Jan.9[i]+4/12*100*data$coupon[i])
  data$Jan.10[i]<-(data$Jan.10[i]+4/12*100*data$coupon[i])
  data$Jan.13[i]<-(data$Jan.13[i]+4/12*100*data$coupon[i])
  data$Jan.14[i]<-(data$Jan.14[i]+4/12*100*data$coupon[i])
  data$Jan.15[i]<-(data$Jan.15[i]+4/12*100*data$coupon[i])
}

t<-seq(0.5,5,by=0.5)


#4 a ytm curve

#ytm_2
ytm_2<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.2[1])
ytm_2[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-x*2/12))-
     data$Jan.2[2])
ytm_2[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-x*2/12))
   +(0.5*100*data$coupon[3]*exp(-x*8/12))
   -data$Jan.2[3])
uniroot(f, lower=0, upper=1)$root
ytm_2[3]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-x*2/12))+
     (0.5*100*data$coupon[4]*exp(-x*8/12))+
     (0.5*100*data$coupon[4]*exp(-x*14/12))
   -data$Jan.2[4])
uniroot(f, lower=0, upper=1)$root
ytm_2[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-x*2/12))+
     (0.5*100*data$coupon[5]*exp(-x*8/12))+
     (0.5*100*data$coupon[5]*exp(-x*14/12))+
     (0.5*100*data$coupon[5]*exp(-x*20/12))
   -data$Jan.2[5])
uniroot(f, lower=0, upper=1)$root
ytm_2[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-x*2/12))+
     (0.5*100*data$coupon[6]*exp(-x*8/12))+
     (0.5*100*data$coupon[6]*exp(-x*14/12))+
     (0.5*100*data$coupon[6]*exp(-x*20/12))+
     (0.5*100*data$coupon[6]*exp(-x*26/12))+
     (0.5*100*data$coupon[6]*exp(-x*32/12))
   -data$Jan.2[6])
uniroot(f, lower=0, upper=1)$root
ytm_2[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-x*2/12))+
     (0.5*100*data$coupon[7]*exp(-x*8/12))+
     (0.5*100*data$coupon[7]*exp(-x*14/12))+
     (0.5*100*data$coupon[7]*exp(-x*20/12))+
     (0.5*100*data$coupon[7]*exp(-x*26/12))+
     (0.5*100*data$coupon[7]*exp(-x*32/12))
   -data$Jan.2[7])
uniroot(f, lower=0, upper=1)$root
ytm_2[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-x*2/12))+
     (0.5*100*data$coupon[8]*exp(-x*8/12))+
     (0.5*100*data$coupon[8]*exp(-x*14/12))+
     (0.5*100*data$coupon[8]*exp(-x*20/12))+
     (0.5*100*data$coupon[8]*exp(-x*26/12))+
     (0.5*100*data$coupon[8]*exp(-x*32/12))+
     (0.5*100*data$coupon[8]*exp(-x*38/12))+
     (0.5*100*data$coupon[8]*exp(-x*44/12))
   -data$Jan.2[8])
uniroot(f, lower=0, upper=1)$root
ytm_2[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-x*2/12))+
     (0.5*100*data$coupon[9]*exp(-x*8/12))+
     (0.5*100*data$coupon[9]*exp(-x*14/12))+
     (0.5*100*data$coupon[9]*exp(-x*20/12))+
     (0.5*100*data$coupon[9]*exp(-x*26/12))+
     (0.5*100*data$coupon[9]*exp(-x*32/12))+
     (0.5*100*data$coupon[9]*exp(-x*38/12))+
     (0.5*100*data$coupon[9]*exp(-x*44/12))+
     (0.5*100*data$coupon[9]*exp(-x*50/12))
   -data$Jan.2[9])
uniroot(f, lower=0, upper=1)$root
ytm_2[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-x*2/12))+
     (0.5*100*data$coupon[10]*exp(-x*8/12))+
     (0.5*100*data$coupon[10]*exp(-x*14/12))+
     (0.5*100*data$coupon[10]*exp(-x*20/12))+
     (0.5*100*data$coupon[10]*exp(-x*26/12))+
     (0.5*100*data$coupon[10]*exp(-x*32/12))+
     (0.5*100*data$coupon[10]*exp(-x*38/12))+
     (0.5*100*data$coupon[10]*exp(-x*44/12))+
     (0.5*100*data$coupon[10]*exp(-x*50/12))+
     (0.5*100*data$coupon[10]*exp(-x*56/12))+
     
   -data$Jan.2[10])
uniroot(f, lower=0, upper=1)$root
ytm_2[10]<-uniroot(f, lower=0, upper=1)$root


#ytm_3
ytm_3<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.3[1])
ytm_3[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-x*2/12))-
     data$Jan.3[2])
ytm_3[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-x*2/12))
   +(0.5*100*data$coupon[3]*exp(-x*8/12))
   -data$Jan.3[3])
uniroot(f, lower=0, upper=1)$root
ytm_3[3]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-x*2/12))+
     (0.5*100*data$coupon[4]*exp(-x*8/12))+
     (0.5*100*data$coupon[4]*exp(-x*14/12))
   -data$Jan.3[4])
uniroot(f, lower=0, upper=1)$root
ytm_3[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-x*2/12))+
     (0.5*100*data$coupon[5]*exp(-x*8/12))+
     (0.5*100*data$coupon[5]*exp(-x*14/12))+
     (0.5*100*data$coupon[5]*exp(-x*20/12))
   -data$Jan.3[5])
uniroot(f, lower=0, upper=1)$root
ytm_3[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-x*2/12))+
     (0.5*100*data$coupon[6]*exp(-x*8/12))+
     (0.5*100*data$coupon[6]*exp(-x*14/12))+
     (0.5*100*data$coupon[6]*exp(-x*20/12))+
     (0.5*100*data$coupon[6]*exp(-x*26/12))+
     (0.5*100*data$coupon[6]*exp(-x*32/12))
   -data$Jan.3[6])
uniroot(f, lower=0, upper=1)$root
ytm_3[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-x*2/12))+
     (0.5*100*data$coupon[7]*exp(-x*8/12))+
     (0.5*100*data$coupon[7]*exp(-x*14/12))+
     (0.5*100*data$coupon[7]*exp(-x*20/12))+
     (0.5*100*data$coupon[7]*exp(-x*26/12))+
     (0.5*100*data$coupon[7]*exp(-x*32/12))
   -data$Jan.3[7])
uniroot(f, lower=0, upper=1)$root
ytm_3[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-x*2/12))+
     (0.5*100*data$coupon[8]*exp(-x*8/12))+
     (0.5*100*data$coupon[8]*exp(-x*14/12))+
     (0.5*100*data$coupon[8]*exp(-x*20/12))+
     (0.5*100*data$coupon[8]*exp(-x*26/12))+
     (0.5*100*data$coupon[8]*exp(-x*32/12))+
     (0.5*100*data$coupon[8]*exp(-x*38/12))+
     (0.5*100*data$coupon[8]*exp(-x*44/12))
   -data$Jan.3[8])
uniroot(f, lower=0, upper=1)$root
ytm_3[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-x*2/12))+
     (0.5*100*data$coupon[9]*exp(-x*8/12))+
     (0.5*100*data$coupon[9]*exp(-x*14/12))+
     (0.5*100*data$coupon[9]*exp(-x*20/12))+
     (0.5*100*data$coupon[9]*exp(-x*26/12))+
     (0.5*100*data$coupon[9]*exp(-x*32/12))+
     (0.5*100*data$coupon[9]*exp(-x*38/12))+
     (0.5*100*data$coupon[9]*exp(-x*44/12))+
     (0.5*100*data$coupon[9]*exp(-x*50/12))
   -data$Jan.3[9])
uniroot(f, lower=0, upper=1)$root
ytm_3[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-x*2/12))+
     (0.5*100*data$coupon[10]*exp(-x*8/12))+
     (0.5*100*data$coupon[10]*exp(-x*14/12))+
     (0.5*100*data$coupon[10]*exp(-x*20/12))+
     (0.5*100*data$coupon[10]*exp(-x*26/12))+
     (0.5*100*data$coupon[10]*exp(-x*32/12))+
     (0.5*100*data$coupon[10]*exp(-x*38/12))+
     (0.5*100*data$coupon[10]*exp(-x*44/12))+
     (0.5*100*data$coupon[10]*exp(-x*50/12))+
     (0.5*100*data$coupon[10]*exp(-x*56/12))+
     
     -data$Jan.3[10])
uniroot(f, lower=0, upper=1)$root
ytm_3[10]<-uniroot(f, lower=0, upper=1)$root


#ytm_6
ytm_6<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.6[1])

ytm_6[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-x*2/12))-
     data$Jan.6[2])
ytm_6[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-x*2/12))
   +(0.5*100*data$coupon[3]*exp(-x*8/12))
   -data$Jan.6[3])
uniroot(f, lower=0, upper=1)$root
ytm_6[3]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-x*2/12))+
     (0.5*100*data$coupon[4]*exp(-x*8/12))+
     (0.5*100*data$coupon[4]*exp(-x*14/12))
   -data$Jan.6[4])
uniroot(f, lower=0, upper=1)$root
ytm_6[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-x*2/12))+
     (0.5*100*data$coupon[5]*exp(-x*8/12))+
     (0.5*100*data$coupon[5]*exp(-x*14/12))+
     (0.5*100*data$coupon[5]*exp(-x*20/12))
   -data$Jan.6[5])
uniroot(f, lower=0, upper=1)$root
ytm_6[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-x*2/12))+
     (0.5*100*data$coupon[6]*exp(-x*8/12))+
     (0.5*100*data$coupon[6]*exp(-x*14/12))+
     (0.5*100*data$coupon[6]*exp(-x*20/12))+
     (0.5*100*data$coupon[6]*exp(-x*26/12))+
     (0.5*100*data$coupon[6]*exp(-x*32/12))
   -data$Jan.6[6])
uniroot(f, lower=0, upper=1)$root
ytm_6[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-x*2/12))+
     (0.5*100*data$coupon[7]*exp(-x*8/12))+
     (0.5*100*data$coupon[7]*exp(-x*14/12))+
     (0.5*100*data$coupon[7]*exp(-x*20/12))+
     (0.5*100*data$coupon[7]*exp(-x*26/12))+
     (0.5*100*data$coupon[7]*exp(-x*32/12))
   -data$Jan.6[7])
uniroot(f, lower=0, upper=1)$root
ytm_6[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-x*2/12))+
     (0.5*100*data$coupon[8]*exp(-x*8/12))+
     (0.5*100*data$coupon[8]*exp(-x*14/12))+
     (0.5*100*data$coupon[8]*exp(-x*20/12))+
     (0.5*100*data$coupon[8]*exp(-x*26/12))+
     (0.5*100*data$coupon[8]*exp(-x*32/12))+
     (0.5*100*data$coupon[8]*exp(-x*38/12))+
     (0.5*100*data$coupon[8]*exp(-x*44/12))
   -data$Jan.6[8])
uniroot(f, lower=0, upper=1)$root
ytm_6[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-x*2/12))+
     (0.5*100*data$coupon[9]*exp(-x*8/12))+
     (0.5*100*data$coupon[9]*exp(-x*14/12))+
     (0.5*100*data$coupon[9]*exp(-x*20/12))+
     (0.5*100*data$coupon[9]*exp(-x*26/12))+
     (0.5*100*data$coupon[9]*exp(-x*32/12))+
     (0.5*100*data$coupon[9]*exp(-x*38/12))+
     (0.5*100*data$coupon[9]*exp(-x*44/12))+
     (0.5*100*data$coupon[9]*exp(-x*50/12))
   -data$Jan.6[9])
uniroot(f, lower=0, upper=1)$root
ytm_6[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-x*2/12))+
     (0.5*100*data$coupon[10]*exp(-x*8/12))+
     (0.5*100*data$coupon[10]*exp(-x*14/12))+
     (0.5*100*data$coupon[10]*exp(-x*20/12))+
     (0.5*100*data$coupon[10]*exp(-x*26/12))+
     (0.5*100*data$coupon[10]*exp(-x*32/12))+
     (0.5*100*data$coupon[10]*exp(-x*38/12))+
     (0.5*100*data$coupon[10]*exp(-x*44/12))+
     (0.5*100*data$coupon[10]*exp(-x*50/12))+
     (0.5*100*data$coupon[10]*exp(-x*56/12))+
     -data$Jan.6[10])
uniroot(f, lower=0, upper=1)$root
ytm_6[10]<-uniroot(f, lower=0, upper=1)$root




#ytm_7
ytm_7<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.7[1])

ytm_7[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-x*2/12))-
     data$Jan.7[2])
ytm_7[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-x*2/12))
   +(0.5*100*data$coupon[3]*exp(-x*8/12))
   -data$Jan.7[3])
uniroot(f, lower=0, upper=1)$root
ytm_7[3]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-x*2/12))+
     (0.5*100*data$coupon[4]*exp(-x*8/12))+
     (0.5*100*data$coupon[4]*exp(-x*14/12))
   -data$Jan.7[4])
uniroot(f, lower=0, upper=1)$root
ytm_7[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-x*2/12))+
     (0.5*100*data$coupon[5]*exp(-x*8/12))+
     (0.5*100*data$coupon[5]*exp(-x*14/12))+
     (0.5*100*data$coupon[5]*exp(-x*20/12))
   -data$Jan.7[5])
uniroot(f, lower=0, upper=1)$root
ytm_7[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-x*2/12))+
     (0.5*100*data$coupon[6]*exp(-x*8/12))+
     (0.5*100*data$coupon[6]*exp(-x*14/12))+
     (0.5*100*data$coupon[6]*exp(-x*20/12))+
     (0.5*100*data$coupon[6]*exp(-x*26/12))+
     (0.5*100*data$coupon[6]*exp(-x*32/12))
   -data$Jan.7[6])
uniroot(f, lower=0, upper=1)$root
ytm_7[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-x*2/12))+
     (0.5*100*data$coupon[7]*exp(-x*8/12))+
     (0.5*100*data$coupon[7]*exp(-x*14/12))+
     (0.5*100*data$coupon[7]*exp(-x*20/12))+
     (0.5*100*data$coupon[7]*exp(-x*26/12))+
     (0.5*100*data$coupon[7]*exp(-x*32/12))
   -data$Jan.7[7])
uniroot(f, lower=0, upper=1)$root
ytm_7[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-x*2/12))+
     (0.5*100*data$coupon[8]*exp(-x*8/12))+
     (0.5*100*data$coupon[8]*exp(-x*14/12))+
     (0.5*100*data$coupon[8]*exp(-x*20/12))+
     (0.5*100*data$coupon[8]*exp(-x*26/12))+
     (0.5*100*data$coupon[8]*exp(-x*32/12))+
     (0.5*100*data$coupon[8]*exp(-x*38/12))+
     (0.5*100*data$coupon[8]*exp(-x*44/12))
   -data$Jan.7[8])
uniroot(f, lower=0, upper=1)$root
ytm_7[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-x*2/12))+
     (0.5*100*data$coupon[9]*exp(-x*8/12))+
     (0.5*100*data$coupon[9]*exp(-x*14/12))+
     (0.5*100*data$coupon[9]*exp(-x*20/12))+
     (0.5*100*data$coupon[9]*exp(-x*26/12))+
     (0.5*100*data$coupon[9]*exp(-x*32/12))+
     (0.5*100*data$coupon[9]*exp(-x*38/12))+
     (0.5*100*data$coupon[9]*exp(-x*44/12))+
     (0.5*100*data$coupon[9]*exp(-x*50/12))
   -data$Jan.7[9])
uniroot(f, lower=0, upper=1)$root
ytm_7[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-x*2/12))+
     (0.5*100*data$coupon[10]*exp(-x*8/12))+
     (0.5*100*data$coupon[10]*exp(-x*14/12))+
     (0.5*100*data$coupon[10]*exp(-x*20/12))+
     (0.5*100*data$coupon[10]*exp(-x*26/12))+
     (0.5*100*data$coupon[10]*exp(-x*32/12))+
     (0.5*100*data$coupon[10]*exp(-x*38/12))+
     (0.5*100*data$coupon[10]*exp(-x*44/12))+
     (0.5*100*data$coupon[10]*exp(-x*50/12))+
     (0.5*100*data$coupon[10]*exp(-x*56/12))+
     -data$Jan.7[10])
uniroot(f, lower=0, upper=1)$root
ytm_7[10]<-uniroot(f, lower=0, upper=1)$root



#ytm_8
ytm_8<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.8[1])

ytm_8[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-x*2/12))-
     data$Jan.8[2])
ytm_8[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-x*2/12))
   +(0.5*100*data$coupon[3]*exp(-x*8/12))
   -data$Jan.8[3])
uniroot(f, lower=0, upper=1)$root
ytm_8[3]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-x*2/12))+
     (0.5*100*data$coupon[4]*exp(-x*8/12))+
     (0.5*100*data$coupon[4]*exp(-x*14/12))
   -data$Jan.8[4])
uniroot(f, lower=0, upper=1)$root
ytm_8[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-x*2/12))+
     (0.5*100*data$coupon[5]*exp(-x*8/12))+
     (0.5*100*data$coupon[5]*exp(-x*14/12))+
     (0.5*100*data$coupon[5]*exp(-x*20/12))
   -data$Jan.8[5])
uniroot(f, lower=0, upper=1)$root
ytm_8[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-x*2/12))+
     (0.5*100*data$coupon[6]*exp(-x*8/12))+
     (0.5*100*data$coupon[6]*exp(-x*14/12))+
     (0.5*100*data$coupon[6]*exp(-x*20/12))+
     (0.5*100*data$coupon[6]*exp(-x*26/12))+
     (0.5*100*data$coupon[6]*exp(-x*32/12))
   -data$Jan.8[6])
uniroot(f, lower=0, upper=1)$root
ytm_8[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-x*2/12))+
     (0.5*100*data$coupon[7]*exp(-x*8/12))+
     (0.5*100*data$coupon[7]*exp(-x*14/12))+
     (0.5*100*data$coupon[7]*exp(-x*20/12))+
     (0.5*100*data$coupon[7]*exp(-x*26/12))+
     (0.5*100*data$coupon[7]*exp(-x*32/12))
   -data$Jan.8[7])
uniroot(f, lower=0, upper=1)$root
ytm_8[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-x*2/12))+
     (0.5*100*data$coupon[8]*exp(-x*8/12))+
     (0.5*100*data$coupon[8]*exp(-x*14/12))+
     (0.5*100*data$coupon[8]*exp(-x*20/12))+
     (0.5*100*data$coupon[8]*exp(-x*26/12))+
     (0.5*100*data$coupon[8]*exp(-x*32/12))+
     (0.5*100*data$coupon[8]*exp(-x*38/12))+
     (0.5*100*data$coupon[8]*exp(-x*44/12))
   -data$Jan.8[8])
uniroot(f, lower=0, upper=1)$root
ytm_8[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-x*2/12))+
     (0.5*100*data$coupon[9]*exp(-x*8/12))+
     (0.5*100*data$coupon[9]*exp(-x*14/12))+
     (0.5*100*data$coupon[9]*exp(-x*20/12))+
     (0.5*100*data$coupon[9]*exp(-x*26/12))+
     (0.5*100*data$coupon[9]*exp(-x*32/12))+
     (0.5*100*data$coupon[9]*exp(-x*38/12))+
     (0.5*100*data$coupon[9]*exp(-x*44/12))+
     (0.5*100*data$coupon[9]*exp(-x*50/12))
   -data$Jan.8[9])
uniroot(f, lower=0, upper=1)$root
ytm_8[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-x*2/12))+
     (0.5*100*data$coupon[10]*exp(-x*8/12))+
     (0.5*100*data$coupon[10]*exp(-x*14/12))+
     (0.5*100*data$coupon[10]*exp(-x*20/12))+
     (0.5*100*data$coupon[10]*exp(-x*26/12))+
     (0.5*100*data$coupon[10]*exp(-x*32/12))+
     (0.5*100*data$coupon[10]*exp(-x*38/12))+
     (0.5*100*data$coupon[10]*exp(-x*44/12))+
     (0.5*100*data$coupon[10]*exp(-x*50/12))+
     (0.5*100*data$coupon[10]*exp(-x*56/12))+
     -data$Jan.8[10])
uniroot(f, lower=0, upper=1)$root
ytm_8[10]<-uniroot(f, lower=0, upper=1)$root


#ytm_9
ytm_9<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.9[1])

ytm_9[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-x*2/12))-
     data$Jan.9[2])
ytm_9[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-x*2/12))
   +(0.5*100*data$coupon[3]*exp(-x*8/12))
   -data$Jan.9[3])
uniroot(f, lower=0, upper=1)$root
ytm_9[3]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-x*2/12))+
     (0.5*100*data$coupon[4]*exp(-x*8/12))+
     (0.5*100*data$coupon[4]*exp(-x*14/12))
   -data$Jan.9[4])
uniroot(f, lower=0, upper=1)$root
ytm_9[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-x*2/12))+
     (0.5*100*data$coupon[5]*exp(-x*8/12))+
     (0.5*100*data$coupon[5]*exp(-x*14/12))+
     (0.5*100*data$coupon[5]*exp(-x*20/12))
   -data$Jan.9[5])
uniroot(f, lower=0, upper=1)$root
ytm_9[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-x*2/12))+
     (0.5*100*data$coupon[6]*exp(-x*8/12))+
     (0.5*100*data$coupon[6]*exp(-x*14/12))+
     (0.5*100*data$coupon[6]*exp(-x*20/12))+
     (0.5*100*data$coupon[6]*exp(-x*26/12))+
     (0.5*100*data$coupon[6]*exp(-x*32/12))
   -data$Jan.9[6])
uniroot(f, lower=0, upper=1)$root
ytm_9[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-x*2/12))+
     (0.5*100*data$coupon[7]*exp(-x*8/12))+
     (0.5*100*data$coupon[7]*exp(-x*14/12))+
     (0.5*100*data$coupon[7]*exp(-x*20/12))+
     (0.5*100*data$coupon[7]*exp(-x*26/12))+
     (0.5*100*data$coupon[7]*exp(-x*32/12))
   -data$Jan.9[7])
uniroot(f, lower=0, upper=1)$root
ytm_9[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-x*2/12))+
     (0.5*100*data$coupon[8]*exp(-x*8/12))+
     (0.5*100*data$coupon[8]*exp(-x*14/12))+
     (0.5*100*data$coupon[8]*exp(-x*20/12))+
     (0.5*100*data$coupon[8]*exp(-x*26/12))+
     (0.5*100*data$coupon[8]*exp(-x*32/12))+
     (0.5*100*data$coupon[8]*exp(-x*38/12))+
     (0.5*100*data$coupon[8]*exp(-x*44/12))
   -data$Jan.9[8])
uniroot(f, lower=0, upper=1)$root
ytm_9[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-x*2/12))+
     (0.5*100*data$coupon[9]*exp(-x*8/12))+
     (0.5*100*data$coupon[9]*exp(-x*14/12))+
     (0.5*100*data$coupon[9]*exp(-x*20/12))+
     (0.5*100*data$coupon[9]*exp(-x*26/12))+
     (0.5*100*data$coupon[9]*exp(-x*32/12))+
     (0.5*100*data$coupon[9]*exp(-x*38/12))+
     (0.5*100*data$coupon[9]*exp(-x*44/12))+
     (0.5*100*data$coupon[9]*exp(-x*50/12))
   -data$Jan.9[9])
uniroot(f, lower=0, upper=1)$root
ytm_9[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-x*2/12))+
     (0.5*100*data$coupon[10]*exp(-x*8/12))+
     (0.5*100*data$coupon[10]*exp(-x*14/12))+
     (0.5*100*data$coupon[10]*exp(-x*20/12))+
     (0.5*100*data$coupon[10]*exp(-x*26/12))+
     (0.5*100*data$coupon[10]*exp(-x*32/12))+
     (0.5*100*data$coupon[10]*exp(-x*38/12))+
     (0.5*100*data$coupon[10]*exp(-x*44/12))+
     (0.5*100*data$coupon[10]*exp(-x*50/12))+
     (0.5*100*data$coupon[10]*exp(-x*56/12))+
     -data$Jan.9[10])
uniroot(f, lower=0, upper=1)$root
ytm_9[10]<-uniroot(f, lower=0, upper=1)$root


#ytm_10
ytm_10<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.10[1])

ytm_10[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-x*2/12))-
     data$Jan.10[2])
ytm_10[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-x*2/12))
   +(0.5*100*data$coupon[3]*exp(-x*8/12))
   -data$Jan.10[3])
uniroot(f, lower=0, upper=1)$root
ytm_10[3]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-x*2/12))+
     (0.5*100*data$coupon[4]*exp(-x*8/12))+
     (0.5*100*data$coupon[4]*exp(-x*14/12))
   -data$Jan.10[4])
uniroot(f, lower=0, upper=1)$root
ytm_10[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-x*2/12))+
     (0.5*100*data$coupon[5]*exp(-x*8/12))+
     (0.5*100*data$coupon[5]*exp(-x*14/12))+
     (0.5*100*data$coupon[5]*exp(-x*20/12))
   -data$Jan.10[5])
uniroot(f, lower=0, upper=1)$root
ytm_10[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-x*2/12))+
     (0.5*100*data$coupon[6]*exp(-x*8/12))+
     (0.5*100*data$coupon[6]*exp(-x*14/12))+
     (0.5*100*data$coupon[6]*exp(-x*20/12))+
     (0.5*100*data$coupon[6]*exp(-x*26/12))+
     (0.5*100*data$coupon[6]*exp(-x*32/12))
   -data$Jan.10[6])
uniroot(f, lower=0, upper=1)$root
ytm_10[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-x*2/12))+
     (0.5*100*data$coupon[7]*exp(-x*8/12))+
     (0.5*100*data$coupon[7]*exp(-x*14/12))+
     (0.5*100*data$coupon[7]*exp(-x*20/12))+
     (0.5*100*data$coupon[7]*exp(-x*26/12))+
     (0.5*100*data$coupon[7]*exp(-x*32/12))
   -data$Jan.10[7])
uniroot(f, lower=0, upper=1)$root
ytm_10[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-x*2/12))+
     (0.5*100*data$coupon[8]*exp(-x*8/12))+
     (0.5*100*data$coupon[8]*exp(-x*14/12))+
     (0.5*100*data$coupon[8]*exp(-x*20/12))+
     (0.5*100*data$coupon[8]*exp(-x*26/12))+
     (0.5*100*data$coupon[8]*exp(-x*32/12))+
     (0.5*100*data$coupon[8]*exp(-x*38/12))+
     (0.5*100*data$coupon[8]*exp(-x*44/12))
   -data$Jan.10[8])
uniroot(f, lower=0, upper=1)$root
ytm_10[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-x*2/12))+
     (0.5*100*data$coupon[9]*exp(-x*8/12))+
     (0.5*100*data$coupon[9]*exp(-x*14/12))+
     (0.5*100*data$coupon[9]*exp(-x*20/12))+
     (0.5*100*data$coupon[9]*exp(-x*26/12))+
     (0.5*100*data$coupon[9]*exp(-x*32/12))+
     (0.5*100*data$coupon[9]*exp(-x*38/12))+
     (0.5*100*data$coupon[9]*exp(-x*44/12))+
     (0.5*100*data$coupon[9]*exp(-x*50/12))
   -data$Jan.10[9])
uniroot(f, lower=0, upper=1)$root
ytm_10[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-x*2/12))+
     (0.5*100*data$coupon[10]*exp(-x*8/12))+
     (0.5*100*data$coupon[10]*exp(-x*14/12))+
     (0.5*100*data$coupon[10]*exp(-x*20/12))+
     (0.5*100*data$coupon[10]*exp(-x*26/12))+
     (0.5*100*data$coupon[10]*exp(-x*32/12))+
     (0.5*100*data$coupon[10]*exp(-x*38/12))+
     (0.5*100*data$coupon[10]*exp(-x*44/12))+
     (0.5*100*data$coupon[10]*exp(-x*50/12))+
     (0.5*100*data$coupon[10]*exp(-x*56/12))+
     -data$Jan.10[10])
uniroot(f, lower=0, upper=1)$root
ytm_10[10]<-uniroot(f, lower=0, upper=1)$root



#ytm_13
ytm_13<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.13[1])

ytm_13[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-x*2/12))-
     data$Jan.13[2])
ytm_13[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-x*2/12))
   +(0.5*100*data$coupon[3]*exp(-x*8/12))
   -data$Jan.13[3])
uniroot(f, lower=0, upper=1)$root
ytm_13[3]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-x*2/12))+
     (0.5*100*data$coupon[4]*exp(-x*8/12))+
     (0.5*100*data$coupon[4]*exp(-x*14/12))
   -data$Jan.13[4])
uniroot(f, lower=0, upper=1)$root
ytm_13[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-x*2/12))+
     (0.5*100*data$coupon[5]*exp(-x*8/12))+
     (0.5*100*data$coupon[5]*exp(-x*14/12))+
     (0.5*100*data$coupon[5]*exp(-x*20/12))
   -data$Jan.13[5])
uniroot(f, lower=0, upper=1)$root
ytm_13[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-x*2/12))+
     (0.5*100*data$coupon[6]*exp(-x*8/12))+
     (0.5*100*data$coupon[6]*exp(-x*14/12))+
     (0.5*100*data$coupon[6]*exp(-x*20/12))+
     (0.5*100*data$coupon[6]*exp(-x*26/12))+
     (0.5*100*data$coupon[6]*exp(-x*32/12))
   -data$Jan.13[6])
uniroot(f, lower=0, upper=1)$root
ytm_13[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-x*2/12))+
     (0.5*100*data$coupon[7]*exp(-x*8/12))+
     (0.5*100*data$coupon[7]*exp(-x*14/12))+
     (0.5*100*data$coupon[7]*exp(-x*20/12))+
     (0.5*100*data$coupon[7]*exp(-x*26/12))+
     (0.5*100*data$coupon[7]*exp(-x*32/12))
   -data$Jan.13[7])
uniroot(f, lower=0, upper=1)$root
ytm_13[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-x*2/12))+
     (0.5*100*data$coupon[8]*exp(-x*8/12))+
     (0.5*100*data$coupon[8]*exp(-x*14/12))+
     (0.5*100*data$coupon[8]*exp(-x*20/12))+
     (0.5*100*data$coupon[8]*exp(-x*26/12))+
     (0.5*100*data$coupon[8]*exp(-x*32/12))+
     (0.5*100*data$coupon[8]*exp(-x*38/12))+
     (0.5*100*data$coupon[8]*exp(-x*44/12))
   -data$Jan.13[8])
uniroot(f, lower=0, upper=1)$root
ytm_13[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-x*2/12))+
     (0.5*100*data$coupon[9]*exp(-x*8/12))+
     (0.5*100*data$coupon[9]*exp(-x*14/12))+
     (0.5*100*data$coupon[9]*exp(-x*20/12))+
     (0.5*100*data$coupon[9]*exp(-x*26/12))+
     (0.5*100*data$coupon[9]*exp(-x*32/12))+
     (0.5*100*data$coupon[9]*exp(-x*38/12))+
     (0.5*100*data$coupon[9]*exp(-x*44/12))+
     (0.5*100*data$coupon[9]*exp(-x*50/12))
   -data$Jan.13[9])
uniroot(f, lower=0, upper=1)$root
ytm_13[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-x*2/12))+
     (0.5*100*data$coupon[10]*exp(-x*8/12))+
     (0.5*100*data$coupon[10]*exp(-x*14/12))+
     (0.5*100*data$coupon[10]*exp(-x*20/12))+
     (0.5*100*data$coupon[10]*exp(-x*26/12))+
     (0.5*100*data$coupon[10]*exp(-x*32/12))+
     (0.5*100*data$coupon[10]*exp(-x*38/12))+
     (0.5*100*data$coupon[10]*exp(-x*44/12))+
     (0.5*100*data$coupon[10]*exp(-x*50/12))+
     (0.5*100*data$coupon[10]*exp(-x*56/12))+
     -data$Jan.13[10])
uniroot(f, lower=0, upper=1)$root
ytm_13[10]<-uniroot(f, lower=0, upper=1)$root




#ytm_14
ytm_14<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.14[1])

ytm_14[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-x*2/12))-
     data$Jan.14[2])
ytm_14[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-x*2/12))
   +(0.5*100*data$coupon[3]*exp(-x*8/12))
   -data$Jan.14[3])
uniroot(f, lower=0, upper=1)$root
ytm_14[3]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-x*2/12))+
     (0.5*100*data$coupon[4]*exp(-x*8/12))+
     (0.5*100*data$coupon[4]*exp(-x*14/12))
   -data$Jan.14[4])
uniroot(f, lower=0, upper=1)$root
ytm_14[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-x*2/12))+
     (0.5*100*data$coupon[5]*exp(-x*8/12))+
     (0.5*100*data$coupon[5]*exp(-x*14/12))+
     (0.5*100*data$coupon[5]*exp(-x*20/12))
   -data$Jan.14[5])
uniroot(f, lower=0, upper=1)$root
ytm_14[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-x*2/12))+
     (0.5*100*data$coupon[6]*exp(-x*8/12))+
     (0.5*100*data$coupon[6]*exp(-x*14/12))+
     (0.5*100*data$coupon[6]*exp(-x*20/12))+
     (0.5*100*data$coupon[6]*exp(-x*26/12))+
     (0.5*100*data$coupon[6]*exp(-x*32/12))
   -data$Jan.14[6])
uniroot(f, lower=0, upper=1)$root
ytm_14[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-x*2/12))+
     (0.5*100*data$coupon[7]*exp(-x*8/12))+
     (0.5*100*data$coupon[7]*exp(-x*14/12))+
     (0.5*100*data$coupon[7]*exp(-x*20/12))+
     (0.5*100*data$coupon[7]*exp(-x*26/12))+
     (0.5*100*data$coupon[7]*exp(-x*32/12))
   -data$Jan.14[7])
uniroot(f, lower=0, upper=1)$root
ytm_14[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-x*2/12))+
     (0.5*100*data$coupon[8]*exp(-x*8/12))+
     (0.5*100*data$coupon[8]*exp(-x*14/12))+
     (0.5*100*data$coupon[8]*exp(-x*20/12))+
     (0.5*100*data$coupon[8]*exp(-x*26/12))+
     (0.5*100*data$coupon[8]*exp(-x*32/12))+
     (0.5*100*data$coupon[8]*exp(-x*38/12))+
     (0.5*100*data$coupon[8]*exp(-x*44/12))
   -data$Jan.14[8])
uniroot(f, lower=0, upper=1)$root
ytm_14[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-x*2/12))+
     (0.5*100*data$coupon[9]*exp(-x*8/12))+
     (0.5*100*data$coupon[9]*exp(-x*14/12))+
     (0.5*100*data$coupon[9]*exp(-x*20/12))+
     (0.5*100*data$coupon[9]*exp(-x*26/12))+
     (0.5*100*data$coupon[9]*exp(-x*32/12))+
     (0.5*100*data$coupon[9]*exp(-x*38/12))+
     (0.5*100*data$coupon[9]*exp(-x*44/12))+
     (0.5*100*data$coupon[9]*exp(-x*50/12))
   -data$Jan.14[9])
uniroot(f, lower=0, upper=1)$root
ytm_14[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-x*2/12))+
     (0.5*100*data$coupon[10]*exp(-x*8/12))+
     (0.5*100*data$coupon[10]*exp(-x*14/12))+
     (0.5*100*data$coupon[10]*exp(-x*20/12))+
     (0.5*100*data$coupon[10]*exp(-x*26/12))+
     (0.5*100*data$coupon[10]*exp(-x*32/12))+
     (0.5*100*data$coupon[10]*exp(-x*38/12))+
     (0.5*100*data$coupon[10]*exp(-x*44/12))+
     (0.5*100*data$coupon[10]*exp(-x*50/12))+
     (0.5*100*data$coupon[10]*exp(-x*56/12))+
     -data$Jan.14[10])
uniroot(f, lower=0, upper=1)$root
ytm_14[10]<-uniroot(f, lower=0, upper=1)$root



#ytm_15
ytm_15<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.15[1])

ytm_15[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-x*2/12))-
     data$Jan.15[2])
ytm_15[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-x*2/12))
   +(0.5*100*data$coupon[3]*exp(-x*8/12))
   -data$Jan.15[3])
uniroot(f, lower=0, upper=1)$root
ytm_15[3]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-x*2/12))+
     (0.5*100*data$coupon[4]*exp(-x*8/12))+
     (0.5*100*data$coupon[4]*exp(-x*14/12))
   -data$Jan.15[4])
uniroot(f, lower=0, upper=1)$root
ytm_15[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-x*2/12))+
     (0.5*100*data$coupon[5]*exp(-x*8/12))+
     (0.5*100*data$coupon[5]*exp(-x*14/12))+
     (0.5*100*data$coupon[5]*exp(-x*20/12))
   -data$Jan.15[5])
uniroot(f, lower=0, upper=1)$root
ytm_15[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-x*2/12))+
     (0.5*100*data$coupon[6]*exp(-x*8/12))+
     (0.5*100*data$coupon[6]*exp(-x*14/12))+
     (0.5*100*data$coupon[6]*exp(-x*20/12))+
     (0.5*100*data$coupon[6]*exp(-x*26/12))+
     (0.5*100*data$coupon[6]*exp(-x*32/12))
   -data$Jan.15[6])
uniroot(f, lower=0, upper=1)$root
ytm_15[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-x*2/12))+
     (0.5*100*data$coupon[7]*exp(-x*8/12))+
     (0.5*100*data$coupon[7]*exp(-x*14/12))+
     (0.5*100*data$coupon[7]*exp(-x*20/12))+
     (0.5*100*data$coupon[7]*exp(-x*26/12))+
     (0.5*100*data$coupon[7]*exp(-x*32/12))
   -data$Jan.15[7])
uniroot(f, lower=0, upper=1)$root
ytm_15[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-x*2/12))+
     (0.5*100*data$coupon[8]*exp(-x*8/12))+
     (0.5*100*data$coupon[8]*exp(-x*14/12))+
     (0.5*100*data$coupon[8]*exp(-x*20/12))+
     (0.5*100*data$coupon[8]*exp(-x*26/12))+
     (0.5*100*data$coupon[8]*exp(-x*32/12))+
     (0.5*100*data$coupon[8]*exp(-x*38/12))+
     (0.5*100*data$coupon[8]*exp(-x*44/12))
   -data$Jan.15[8])
uniroot(f, lower=0, upper=1)$root
ytm_15[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-x*2/12))+
     (0.5*100*data$coupon[9]*exp(-x*8/12))+
     (0.5*100*data$coupon[9]*exp(-x*14/12))+
     (0.5*100*data$coupon[9]*exp(-x*20/12))+
     (0.5*100*data$coupon[9]*exp(-x*26/12))+
     (0.5*100*data$coupon[9]*exp(-x*32/12))+
     (0.5*100*data$coupon[9]*exp(-x*38/12))+
     (0.5*100*data$coupon[9]*exp(-x*44/12))+
     (0.5*100*data$coupon[9]*exp(-x*50/12))
   -data$Jan.15[9])
uniroot(f, lower=0, upper=1)$root
ytm_15[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-x*2/12))+
     (0.5*100*data$coupon[10]*exp(-x*8/12))+
     (0.5*100*data$coupon[10]*exp(-x*14/12))+
     (0.5*100*data$coupon[10]*exp(-x*20/12))+
     (0.5*100*data$coupon[10]*exp(-x*26/12))+
     (0.5*100*data$coupon[10]*exp(-x*32/12))+
     (0.5*100*data$coupon[10]*exp(-x*38/12))+
     (0.5*100*data$coupon[10]*exp(-x*44/12))+
     (0.5*100*data$coupon[10]*exp(-x*50/12))+
     (0.5*100*data$coupon[10]*exp(-x*56/12))+
     -data$Jan.15[10])
uniroot(f, lower=0, upper=1)$root
ytm_15[10]<-uniroot(f, lower=0, upper=1)$root







plot(t,ytm_2,type = "b", ylim = c(0.014,0.025), main = "5-year ytm curve",ylab = "ytm")
points(t,ytm_3,col=2);lines(t,ytm_3,col=2)
points(t,ytm_6,col=3);lines(t,ytm_6,col=3)
points(t,ytm_7,col=4);lines(t,ytm_7,col=4)
points(t,ytm_8,col=5);lines(t,ytm_8,col=5)
points(t,ytm_9,col=6);lines(t,ytm_9,col=6)
points(t,ytm_10,col=7);lines(t,ytm_10,col=7)
points(t,ytm_13,col=8);lines(t,ytm_13,col=8)
points(t,ytm_14,col=9);lines(t,ytm_14,col=9)
points(t,ytm_15,col=10);lines(t,ytm_15,col=10)

legend("topright", legend = c("Jan 2","Jan 3","Jan 6","Jan 7","Jan 8", "Jan 9", "Jan 10","Jan 13","Jan 14","Jan 15"),
       col=1:10, lty =1, cex = 0.7)













#4(b) spot rate 

#spr_2
spr_2<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.2[1])

spr_2[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-spr_2[1]*2/12))-
     data$Jan.2[2])
spr_2[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-spr_2[1]*2/12))
   +(0.5*100*data$coupon[3]*exp(-spr_2[2]*8/12))
   -data$Jan.2[3])
spr_2[3]<-uniroot(f, lower=0, upper=1)$root



f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-spr_2[1]*2/12))+
     (0.5*100*data$coupon[4]*exp(-spr_2[2]*8/12))+
     (0.5*100*data$coupon[4]*exp(-spr_2[3]*14/12))
   -data$Jan.2[4])
uniroot(f, lower=0, upper=1)$root
spr_2[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-spr_2[1]*2/12))+
     (0.5*100*data$coupon[5]*exp(-spr_2[2]*8/12))+
     (0.5*100*data$coupon[5]*exp(-spr_2[3]*14/12))+
     (0.5*100*data$coupon[5]*exp(-spr_2[4]*20/12))
   -data$Jan.2[5])
uniroot(f, lower=0, upper=1)$root
spr_2[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-spr_2[1]*2/12))+
     (0.5*100*data$coupon[6]*exp(-spr_2[2]*8/12))+
     (0.5*100*data$coupon[6]*exp(-spr_2[3]*14/12))+
     (0.5*100*data$coupon[6]*exp(-spr_2[4]*20/12))+
     (0.5*100*data$coupon[6]*exp(-spr_2[5]*26/12))+
     (0.5*100*data$coupon[6]*exp(-spr_2[5]*32/12))
   -data$Jan.2[6])
uniroot(f, lower=0, upper=1)$root
spr_2[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-spr_2[1]*2/12))+
     (0.5*100*data$coupon[7]*exp(-spr_2[2]*8/12))+
     (0.5*100*data$coupon[7]*exp(-spr_2[3]*14/12))+
     (0.5*100*data$coupon[7]*exp(-spr_2[4]*20/12))+
     (0.5*100*data$coupon[7]*exp(-spr_2[5]*26/12))+
     (0.5*100*data$coupon[7]*exp(-spr_2[5]*32/12))
   -data$Jan.2[7])
uniroot(f, lower=0, upper=1)$root
spr_2[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-spr_2[1]*2/12))+
     (0.5*100*data$coupon[8]*exp(-spr_2[2]*8/12))+
     (0.5*100*data$coupon[8]*exp(-spr_2[3]*14/12))+
     (0.5*100*data$coupon[8]*exp(-spr_2[4]*20/12))+
     (0.5*100*data$coupon[8]*exp(-spr_2[5]*26/12))+
     (0.5*100*data$coupon[8]*exp(-spr_2[5]*32/12))+
     (0.5*100*data$coupon[8]*exp(-spr_2[6]*38/12))+
     (0.5*100*data$coupon[8]*exp(-spr_2[7]*44/12))
   -data$Jan.2[8])
uniroot(f, lower=0, upper=1)$root
spr_2[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-spr_2[1]*2/12))+
     (0.5*100*data$coupon[9]*exp(-spr_2[2]*8/12))+
     (0.5*100*data$coupon[9]*exp(-spr_2[3]*14/12))+
     (0.5*100*data$coupon[9]*exp(-spr_2[4]*20/12))+
     (0.5*100*data$coupon[9]*exp(-spr_2[5]*26/12))+
     (0.5*100*data$coupon[9]*exp(-spr_2[5]*32/12))+
     (0.5*100*data$coupon[9]*exp(-spr_2[6]*38/12))+
     (0.5*100*data$coupon[9]*exp(-spr_2[7]*44/12))+
     (0.5*100*data$coupon[9]*exp(-spr_2[8]*50/12))
   -data$Jan.2[9])
uniroot(f, lower=0, upper=1)$root
spr_2[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-spr_2[1]*2/12))+
     (0.5*100*data$coupon[10]*exp(-spr_2[2]*8/12))+
     (0.5*100*data$coupon[10]*exp(-spr_2[3]*14/12))+
     (0.5*100*data$coupon[10]*exp(-spr_2[4]*20/12))+
     (0.5*100*data$coupon[10]*exp(-spr_2[5]*26/12))+
     (0.5*100*data$coupon[10]*exp(-spr_2[5]*32/12))+
     (0.5*100*data$coupon[10]*exp(-spr_2[6]*38/12))+
     (0.5*100*data$coupon[10]*exp(-spr_2[7]*44/12))+
     (0.5*100*data$coupon[10]*exp(-spr_2[8]*50/12))+
     (0.5*100*data$coupon[10]*exp(-spr_2[9]*56/12))+
     -data$Jan.2[10])
uniroot(f, lower=0, upper=1)$root
spr_2[10]<-uniroot(f, lower=0, upper=1)$root




#spr_3

spr_3<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.3[1])

spr_3[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-spr_3[1]*2/12))-
     data$Jan.3[2])
spr_3[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-spr_3[1]*2/12))
   +(0.5*100*data$coupon[3]*exp(-spr_3[2]*8/12))
   -data$Jan.3[3])
spr_3[3]<-uniroot(f, lower=0, upper=1)$root



f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-spr_3[1]*2/12))+
     (0.5*100*data$coupon[4]*exp(-spr_3[2]*8/12))+
     (0.5*100*data$coupon[4]*exp(-spr_3[3]*14/12))
   -data$Jan.3[4])
uniroot(f, lower=0, upper=1)$root
spr_3[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-spr_3[1]*2/12))+
     (0.5*100*data$coupon[5]*exp(-spr_3[2]*8/12))+
     (0.5*100*data$coupon[5]*exp(-spr_3[3]*14/12))+
     (0.5*100*data$coupon[5]*exp(-spr_3[4]*20/12))
   -data$Jan.3[5])
uniroot(f, lower=0, upper=1)$root
spr_3[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-spr_3[1]*2/12))+
     (0.5*100*data$coupon[6]*exp(-spr_3[2]*8/12))+
     (0.5*100*data$coupon[6]*exp(-spr_3[3]*14/12))+
     (0.5*100*data$coupon[6]*exp(-spr_3[4]*20/12))+
     (0.5*100*data$coupon[6]*exp(-spr_3[5]*26/12))+
     (0.5*100*data$coupon[6]*exp(-spr_3[5]*32/12))
   -data$Jan.3[6])
uniroot(f, lower=0, upper=1)$root
spr_3[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-spr_3[1]*2/12))+
     (0.5*100*data$coupon[7]*exp(-spr_3[2]*8/12))+
     (0.5*100*data$coupon[7]*exp(-spr_3[3]*14/12))+
     (0.5*100*data$coupon[7]*exp(-spr_3[4]*20/12))+
     (0.5*100*data$coupon[7]*exp(-spr_3[5]*26/12))+
     (0.5*100*data$coupon[7]*exp(-spr_3[5]*32/12))
   -data$Jan.3[7])
uniroot(f, lower=0, upper=1)$root
spr_3[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-spr_3[1]*2/12))+
     (0.5*100*data$coupon[8]*exp(-spr_3[2]*8/12))+
     (0.5*100*data$coupon[8]*exp(-spr_3[3]*14/12))+
     (0.5*100*data$coupon[8]*exp(-spr_3[4]*20/12))+
     (0.5*100*data$coupon[8]*exp(-spr_3[5]*26/12))+
     (0.5*100*data$coupon[8]*exp(-spr_3[5]*32/12))+
     (0.5*100*data$coupon[8]*exp(-spr_3[6]*38/12))+
     (0.5*100*data$coupon[8]*exp(-spr_3[7]*44/12))
   -data$Jan.3[8])
uniroot(f, lower=0, upper=1)$root
spr_3[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-spr_3[1]*2/12))+
     (0.5*100*data$coupon[9]*exp(-spr_3[2]*8/12))+
     (0.5*100*data$coupon[9]*exp(-spr_3[3]*14/12))+
     (0.5*100*data$coupon[9]*exp(-spr_3[4]*20/12))+
     (0.5*100*data$coupon[9]*exp(-spr_3[5]*26/12))+
     (0.5*100*data$coupon[9]*exp(-spr_3[5]*32/12))+
     (0.5*100*data$coupon[9]*exp(-spr_3[6]*38/12))+
     (0.5*100*data$coupon[9]*exp(-spr_3[7]*44/12))+
     (0.5*100*data$coupon[9]*exp(-spr_3[8]*50/12))
   -data$Jan.3[9])
uniroot(f, lower=0, upper=1)$root
spr_3[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-spr_3[1]*2/12))+
     (0.5*100*data$coupon[10]*exp(-spr_3[2]*8/12))+
     (0.5*100*data$coupon[10]*exp(-spr_3[3]*14/12))+
     (0.5*100*data$coupon[10]*exp(-spr_3[4]*20/12))+
     (0.5*100*data$coupon[10]*exp(-spr_3[5]*26/12))+
     (0.5*100*data$coupon[10]*exp(-spr_3[5]*32/12))+
     (0.5*100*data$coupon[10]*exp(-spr_3[6]*38/12))+
     (0.5*100*data$coupon[10]*exp(-spr_3[7]*44/12))+
     (0.5*100*data$coupon[10]*exp(-spr_3[8]*50/12))+
     (0.5*100*data$coupon[10]*exp(-spr_3[9]*56/12))+
     -data$Jan.3[10])
uniroot(f, lower=0, upper=1)$root
spr_3[10]<-uniroot(f, lower=0, upper=1)$root



#spr_6

spr_6<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.6[1])

spr_6[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-spr_6[1]*2/12))-
     data$Jan.6[2])
spr_6[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-spr_6[1]*2/12))
   +(0.5*100*data$coupon[3]*exp(-spr_6[2]*8/12))
   -data$Jan.6[3])
spr_6[3]<-uniroot(f, lower=0, upper=1)$root



f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-spr_6[1]*2/12))+
     (0.5*100*data$coupon[4]*exp(-spr_6[2]*8/12))+
     (0.5*100*data$coupon[4]*exp(-spr_6[3]*14/12))
   -data$Jan.6[4])
uniroot(f, lower=0, upper=1)$root
spr_6[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-spr_6[1]*2/12))+
     (0.5*100*data$coupon[5]*exp(-spr_6[2]*8/12))+
     (0.5*100*data$coupon[5]*exp(-spr_6[3]*14/12))+
     (0.5*100*data$coupon[5]*exp(-spr_6[4]*20/12))
   -data$Jan.6[5])
uniroot(f, lower=0, upper=1)$root
spr_6[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-spr_6[1]*2/12))+
     (0.5*100*data$coupon[6]*exp(-spr_6[2]*8/12))+
     (0.5*100*data$coupon[6]*exp(-spr_6[3]*14/12))+
     (0.5*100*data$coupon[6]*exp(-spr_6[4]*20/12))+
     (0.5*100*data$coupon[6]*exp(-spr_6[5]*26/12))+
     (0.5*100*data$coupon[6]*exp(-spr_6[5]*32/12))
   -data$Jan.6[6])
uniroot(f, lower=0, upper=1)$root
spr_6[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-spr_6[1]*2/12))+
     (0.5*100*data$coupon[7]*exp(-spr_6[2]*8/12))+
     (0.5*100*data$coupon[7]*exp(-spr_6[3]*14/12))+
     (0.5*100*data$coupon[7]*exp(-spr_6[4]*20/12))+
     (0.5*100*data$coupon[7]*exp(-spr_6[5]*26/12))+
     (0.5*100*data$coupon[7]*exp(-spr_6[5]*32/12))
   -data$Jan.6[7])
uniroot(f, lower=0, upper=1)$root
spr_6[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-spr_6[1]*2/12))+
     (0.5*100*data$coupon[8]*exp(-spr_6[2]*8/12))+
     (0.5*100*data$coupon[8]*exp(-spr_6[3]*14/12))+
     (0.5*100*data$coupon[8]*exp(-spr_6[4]*20/12))+
     (0.5*100*data$coupon[8]*exp(-spr_6[5]*26/12))+
     (0.5*100*data$coupon[8]*exp(-spr_6[5]*32/12))+
     (0.5*100*data$coupon[8]*exp(-spr_6[6]*38/12))+
     (0.5*100*data$coupon[8]*exp(-spr_6[7]*44/12))
   -data$Jan.6[8])
uniroot(f, lower=0, upper=1)$root
spr_6[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-spr_6[1]*2/12))+
     (0.5*100*data$coupon[9]*exp(-spr_6[2]*8/12))+
     (0.5*100*data$coupon[9]*exp(-spr_6[3]*14/12))+
     (0.5*100*data$coupon[9]*exp(-spr_6[4]*20/12))+
     (0.5*100*data$coupon[9]*exp(-spr_6[5]*26/12))+
     (0.5*100*data$coupon[9]*exp(-spr_6[5]*32/12))+
     (0.5*100*data$coupon[9]*exp(-spr_6[6]*38/12))+
     (0.5*100*data$coupon[9]*exp(-spr_6[7]*44/12))+
     (0.5*100*data$coupon[9]*exp(-spr_6[8]*50/12))
   -data$Jan.6[9])
uniroot(f, lower=0, upper=1)$root
spr_6[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-spr_6[1]*2/12))+
     (0.5*100*data$coupon[10]*exp(-spr_6[2]*8/12))+
     (0.5*100*data$coupon[10]*exp(-spr_6[3]*14/12))+
     (0.5*100*data$coupon[10]*exp(-spr_6[4]*20/12))+
     (0.5*100*data$coupon[10]*exp(-spr_6[5]*26/12))+
     (0.5*100*data$coupon[10]*exp(-spr_6[5]*32/12))+
     (0.5*100*data$coupon[10]*exp(-spr_6[6]*38/12))+
     (0.5*100*data$coupon[10]*exp(-spr_6[7]*44/12))+
     (0.5*100*data$coupon[10]*exp(-spr_6[8]*50/12))+
     (0.5*100*data$coupon[10]*exp(-spr_6[9]*56/12))+
     -data$Jan.6[10])
uniroot(f, lower=0, upper=1)$root
spr_6[10]<-uniroot(f, lower=0, upper=1)$root




#spr_7

spr_7<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.7[1])

spr_7[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-spr_7[1]*2/12))-
     data$Jan.7[2])
spr_7[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-spr_7[1]*2/12))
   +(0.5*100*data$coupon[3]*exp(-spr_7[2]*8/12))
   -data$Jan.7[3])
spr_7[3]<-uniroot(f, lower=0, upper=1)$root



f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-spr_7[1]*2/12))+
     (0.5*100*data$coupon[4]*exp(-spr_7[2]*8/12))+
     (0.5*100*data$coupon[4]*exp(-spr_7[3]*14/12))
   -data$Jan.7[4])
uniroot(f, lower=0, upper=1)$root
spr_7[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-spr_7[1]*2/12))+
     (0.5*100*data$coupon[5]*exp(-spr_7[2]*8/12))+
     (0.5*100*data$coupon[5]*exp(-spr_7[3]*14/12))+
     (0.5*100*data$coupon[5]*exp(-spr_7[4]*20/12))
   -data$Jan.7[5])
uniroot(f, lower=0, upper=1)$root
spr_7[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-spr_7[1]*2/12))+
     (0.5*100*data$coupon[6]*exp(-spr_7[2]*8/12))+
     (0.5*100*data$coupon[6]*exp(-spr_7[3]*14/12))+
     (0.5*100*data$coupon[6]*exp(-spr_7[4]*20/12))+
     (0.5*100*data$coupon[6]*exp(-spr_7[5]*26/12))+
     (0.5*100*data$coupon[6]*exp(-spr_7[5]*32/12))
   -data$Jan.7[6])
uniroot(f, lower=0, upper=1)$root
spr_7[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-spr_7[1]*2/12))+
     (0.5*100*data$coupon[7]*exp(-spr_7[2]*8/12))+
     (0.5*100*data$coupon[7]*exp(-spr_7[3]*14/12))+
     (0.5*100*data$coupon[7]*exp(-spr_7[4]*20/12))+
     (0.5*100*data$coupon[7]*exp(-spr_7[5]*26/12))+
     (0.5*100*data$coupon[7]*exp(-spr_7[5]*32/12))
   -data$Jan.7[7])
uniroot(f, lower=0, upper=1)$root
spr_7[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-spr_7[1]*2/12))+
     (0.5*100*data$coupon[8]*exp(-spr_7[2]*8/12))+
     (0.5*100*data$coupon[8]*exp(-spr_7[3]*14/12))+
     (0.5*100*data$coupon[8]*exp(-spr_7[4]*20/12))+
     (0.5*100*data$coupon[8]*exp(-spr_7[5]*26/12))+
     (0.5*100*data$coupon[8]*exp(-spr_7[5]*32/12))+
     (0.5*100*data$coupon[8]*exp(-spr_7[6]*38/12))+
     (0.5*100*data$coupon[8]*exp(-spr_7[7]*44/12))
   -data$Jan.7[8])
uniroot(f, lower=0, upper=1)$root
spr_7[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-spr_7[1]*2/12))+
     (0.5*100*data$coupon[9]*exp(-spr_7[2]*8/12))+
     (0.5*100*data$coupon[9]*exp(-spr_7[3]*14/12))+
     (0.5*100*data$coupon[9]*exp(-spr_7[4]*20/12))+
     (0.5*100*data$coupon[9]*exp(-spr_7[5]*26/12))+
     (0.5*100*data$coupon[9]*exp(-spr_7[5]*32/12))+
     (0.5*100*data$coupon[9]*exp(-spr_7[6]*38/12))+
     (0.5*100*data$coupon[9]*exp(-spr_7[7]*44/12))+
     (0.5*100*data$coupon[9]*exp(-spr_7[8]*50/12))
   -data$Jan.7[9])
uniroot(f, lower=0, upper=1)$root
spr_7[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-spr_7[1]*2/12))+
     (0.5*100*data$coupon[10]*exp(-spr_7[2]*8/12))+
     (0.5*100*data$coupon[10]*exp(-spr_7[3]*14/12))+
     (0.5*100*data$coupon[10]*exp(-spr_7[4]*20/12))+
     (0.5*100*data$coupon[10]*exp(-spr_7[5]*26/12))+
     (0.5*100*data$coupon[10]*exp(-spr_7[5]*32/12))+
     (0.5*100*data$coupon[10]*exp(-spr_7[6]*38/12))+
     (0.5*100*data$coupon[10]*exp(-spr_7[7]*44/12))+
     (0.5*100*data$coupon[10]*exp(-spr_7[8]*50/12))+
     (0.5*100*data$coupon[10]*exp(-spr_7[9]*56/12))+
     -data$Jan.7[10])
uniroot(f, lower=0, upper=1)$root
spr_7[10]<-uniroot(f, lower=0, upper=1)$root


#spr_8

spr_8<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.8[1])

spr_8[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-spr_8[1]*2/12))-
     data$Jan.8[2])
spr_8[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-spr_8[1]*2/12))
   +(0.5*100*data$coupon[3]*exp(-spr_8[2]*8/12))
   -data$Jan.8[3])
spr_8[3]<-uniroot(f, lower=0, upper=1)$root



f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-spr_8[1]*2/12))+
     (0.5*100*data$coupon[4]*exp(-spr_8[2]*8/12))+
     (0.5*100*data$coupon[4]*exp(-spr_8[3]*14/12))
   -data$Jan.8[4])
uniroot(f, lower=0, upper=1)$root
spr_8[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-spr_8[1]*2/12))+
     (0.5*100*data$coupon[5]*exp(-spr_8[2]*8/12))+
     (0.5*100*data$coupon[5]*exp(-spr_8[3]*14/12))+
     (0.5*100*data$coupon[5]*exp(-spr_8[4]*20/12))
   -data$Jan.8[5])
uniroot(f, lower=0, upper=1)$root
spr_8[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-spr_8[1]*2/12))+
     (0.5*100*data$coupon[6]*exp(-spr_8[2]*8/12))+
     (0.5*100*data$coupon[6]*exp(-spr_8[3]*14/12))+
     (0.5*100*data$coupon[6]*exp(-spr_8[4]*20/12))+
     (0.5*100*data$coupon[6]*exp(-spr_8[5]*26/12))+
     (0.5*100*data$coupon[6]*exp(-spr_8[5]*32/12))
   -data$Jan.8[6])
uniroot(f, lower=0, upper=1)$root
spr_8[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-spr_8[1]*2/12))+
     (0.5*100*data$coupon[7]*exp(-spr_8[2]*8/12))+
     (0.5*100*data$coupon[7]*exp(-spr_8[3]*14/12))+
     (0.5*100*data$coupon[7]*exp(-spr_8[4]*20/12))+
     (0.5*100*data$coupon[7]*exp(-spr_8[5]*26/12))+
     (0.5*100*data$coupon[7]*exp(-spr_8[5]*32/12))
   -data$Jan.8[7])
uniroot(f, lower=0, upper=1)$root
spr_8[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-spr_8[1]*2/12))+
     (0.5*100*data$coupon[8]*exp(-spr_8[2]*8/12))+
     (0.5*100*data$coupon[8]*exp(-spr_8[3]*14/12))+
     (0.5*100*data$coupon[8]*exp(-spr_8[4]*20/12))+
     (0.5*100*data$coupon[8]*exp(-spr_8[5]*26/12))+
     (0.5*100*data$coupon[8]*exp(-spr_8[5]*32/12))+
     (0.5*100*data$coupon[8]*exp(-spr_8[6]*38/12))+
     (0.5*100*data$coupon[8]*exp(-spr_8[7]*44/12))
   -data$Jan.8[8])
uniroot(f, lower=0, upper=1)$root
spr_8[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-spr_8[1]*2/12))+
     (0.5*100*data$coupon[9]*exp(-spr_8[2]*8/12))+
     (0.5*100*data$coupon[9]*exp(-spr_8[3]*14/12))+
     (0.5*100*data$coupon[9]*exp(-spr_8[4]*20/12))+
     (0.5*100*data$coupon[9]*exp(-spr_8[5]*26/12))+
     (0.5*100*data$coupon[9]*exp(-spr_8[5]*32/12))+
     (0.5*100*data$coupon[9]*exp(-spr_8[6]*38/12))+
     (0.5*100*data$coupon[9]*exp(-spr_8[7]*44/12))+
     (0.5*100*data$coupon[9]*exp(-spr_8[8]*50/12))
   -data$Jan.8[9])
uniroot(f, lower=0, upper=1)$root
spr_8[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-spr_8[1]*2/12))+
     (0.5*100*data$coupon[10]*exp(-spr_8[2]*8/12))+
     (0.5*100*data$coupon[10]*exp(-spr_8[3]*14/12))+
     (0.5*100*data$coupon[10]*exp(-spr_8[4]*20/12))+
     (0.5*100*data$coupon[10]*exp(-spr_8[5]*26/12))+
     (0.5*100*data$coupon[10]*exp(-spr_8[5]*32/12))+
     (0.5*100*data$coupon[10]*exp(-spr_8[6]*38/12))+
     (0.5*100*data$coupon[10]*exp(-spr_8[7]*44/12))+
     (0.5*100*data$coupon[10]*exp(-spr_8[8]*50/12))+
     (0.5*100*data$coupon[10]*exp(-spr_8[9]*56/12))+
     -data$Jan.8[10])
uniroot(f, lower=0, upper=1)$root
spr_8[10]<-uniroot(f, lower=0, upper=1)$root




#spr_9

spr_9<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.9[1])

spr_9[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-spr_9[1]*2/12))-
     data$Jan.9[2])
spr_9[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-spr_9[1]*2/12))
   +(0.5*100*data$coupon[3]*exp(-spr_9[2]*8/12))
   -data$Jan.9[3])
spr_9[3]<-uniroot(f, lower=0, upper=1)$root



f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-spr_9[1]*2/12))+
     (0.5*100*data$coupon[4]*exp(-spr_9[2]*8/12))+
     (0.5*100*data$coupon[4]*exp(-spr_9[3]*14/12))
   -data$Jan.9[4])
uniroot(f, lower=0, upper=1)$root
spr_9[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-spr_9[1]*2/12))+
     (0.5*100*data$coupon[5]*exp(-spr_9[2]*8/12))+
     (0.5*100*data$coupon[5]*exp(-spr_9[3]*14/12))+
     (0.5*100*data$coupon[5]*exp(-spr_9[4]*20/12))
   -data$Jan.9[5])
uniroot(f, lower=0, upper=1)$root
spr_9[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-spr_9[1]*2/12))+
     (0.5*100*data$coupon[6]*exp(-spr_9[2]*8/12))+
     (0.5*100*data$coupon[6]*exp(-spr_9[3]*14/12))+
     (0.5*100*data$coupon[6]*exp(-spr_9[4]*20/12))+
     (0.5*100*data$coupon[6]*exp(-spr_9[5]*26/12))+
     (0.5*100*data$coupon[6]*exp(-spr_9[5]*32/12))
   -data$Jan.9[6])
uniroot(f, lower=0, upper=1)$root
spr_9[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-spr_9[1]*2/12))+
     (0.5*100*data$coupon[7]*exp(-spr_9[2]*8/12))+
     (0.5*100*data$coupon[7]*exp(-spr_9[3]*14/12))+
     (0.5*100*data$coupon[7]*exp(-spr_9[4]*20/12))+
     (0.5*100*data$coupon[7]*exp(-spr_9[5]*26/12))+
     (0.5*100*data$coupon[7]*exp(-spr_9[5]*32/12))
   -data$Jan.9[7])
uniroot(f, lower=0, upper=1)$root
spr_9[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-spr_9[1]*2/12))+
     (0.5*100*data$coupon[8]*exp(-spr_9[2]*8/12))+
     (0.5*100*data$coupon[8]*exp(-spr_9[3]*14/12))+
     (0.5*100*data$coupon[8]*exp(-spr_9[4]*20/12))+
     (0.5*100*data$coupon[8]*exp(-spr_9[5]*26/12))+
     (0.5*100*data$coupon[8]*exp(-spr_9[5]*32/12))+
     (0.5*100*data$coupon[8]*exp(-spr_9[6]*38/12))+
     (0.5*100*data$coupon[8]*exp(-spr_9[7]*44/12))
   -data$Jan.9[8])
uniroot(f, lower=0, upper=1)$root
spr_9[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-spr_9[1]*2/12))+
     (0.5*100*data$coupon[9]*exp(-spr_9[2]*8/12))+
     (0.5*100*data$coupon[9]*exp(-spr_9[3]*14/12))+
     (0.5*100*data$coupon[9]*exp(-spr_9[4]*20/12))+
     (0.5*100*data$coupon[9]*exp(-spr_9[5]*26/12))+
     (0.5*100*data$coupon[9]*exp(-spr_9[5]*32/12))+
     (0.5*100*data$coupon[9]*exp(-spr_9[6]*38/12))+
     (0.5*100*data$coupon[9]*exp(-spr_9[7]*44/12))+
     (0.5*100*data$coupon[9]*exp(-spr_9[8]*50/12))
   -data$Jan.9[9])
uniroot(f, lower=0, upper=1)$root
spr_9[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-spr_9[1]*2/12))+
     (0.5*100*data$coupon[10]*exp(-spr_9[2]*8/12))+
     (0.5*100*data$coupon[10]*exp(-spr_9[3]*14/12))+
     (0.5*100*data$coupon[10]*exp(-spr_9[4]*20/12))+
     (0.5*100*data$coupon[10]*exp(-spr_9[5]*26/12))+
     (0.5*100*data$coupon[10]*exp(-spr_9[5]*32/12))+
     (0.5*100*data$coupon[10]*exp(-spr_9[6]*38/12))+
     (0.5*100*data$coupon[10]*exp(-spr_9[7]*44/12))+
     (0.5*100*data$coupon[10]*exp(-spr_9[8]*50/12))+
     (0.5*100*data$coupon[10]*exp(-spr_9[9]*56/12))+
     -data$Jan.9[10])
uniroot(f, lower=0, upper=1)$root
spr_9[10]<-uniroot(f, lower=0, upper=1)$root



#spr_10

spr_10<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.10[1])

spr_10[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-spr_10[1]*2/12))-
     data$Jan.10[2])
spr_10[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-spr_10[1]*2/12))
   +(0.5*100*data$coupon[3]*exp(-spr_10[2]*8/12))
   -data$Jan.10[3])
spr_10[3]<-uniroot(f, lower=0, upper=1)$root



f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-spr_10[1]*2/12))+
     (0.5*100*data$coupon[4]*exp(-spr_10[2]*8/12))+
     (0.5*100*data$coupon[4]*exp(-spr_10[3]*14/12))
   -data$Jan.10[4])
uniroot(f, lower=0, upper=1)$root
spr_10[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-spr_10[1]*2/12))+
     (0.5*100*data$coupon[5]*exp(-spr_10[2]*8/12))+
     (0.5*100*data$coupon[5]*exp(-spr_10[3]*14/12))+
     (0.5*100*data$coupon[5]*exp(-spr_10[4]*20/12))
   -data$Jan.10[5])
uniroot(f, lower=0, upper=1)$root
spr_10[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-spr_10[1]*2/12))+
     (0.5*100*data$coupon[6]*exp(-spr_10[2]*8/12))+
     (0.5*100*data$coupon[6]*exp(-spr_10[3]*14/12))+
     (0.5*100*data$coupon[6]*exp(-spr_10[4]*20/12))+
     (0.5*100*data$coupon[6]*exp(-spr_10[5]*26/12))+
     (0.5*100*data$coupon[6]*exp(-spr_10[5]*32/12))
   -data$Jan.10[6])
uniroot(f, lower=0, upper=1)$root
spr_10[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-spr_10[1]*2/12))+
     (0.5*100*data$coupon[7]*exp(-spr_10[2]*8/12))+
     (0.5*100*data$coupon[7]*exp(-spr_10[3]*14/12))+
     (0.5*100*data$coupon[7]*exp(-spr_10[4]*20/12))+
     (0.5*100*data$coupon[7]*exp(-spr_10[5]*26/12))+
     (0.5*100*data$coupon[7]*exp(-spr_10[5]*32/12))
   -data$Jan.10[7])
uniroot(f, lower=0, upper=1)$root
spr_10[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-spr_10[1]*2/12))+
     (0.5*100*data$coupon[8]*exp(-spr_10[2]*8/12))+
     (0.5*100*data$coupon[8]*exp(-spr_10[3]*14/12))+
     (0.5*100*data$coupon[8]*exp(-spr_10[4]*20/12))+
     (0.5*100*data$coupon[8]*exp(-spr_10[5]*26/12))+
     (0.5*100*data$coupon[8]*exp(-spr_10[5]*32/12))+
     (0.5*100*data$coupon[8]*exp(-spr_10[6]*38/12))+
     (0.5*100*data$coupon[8]*exp(-spr_10[7]*44/12))
   -data$Jan.10[8])
uniroot(f, lower=0, upper=1)$root
spr_10[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-spr_10[1]*2/12))+
     (0.5*100*data$coupon[9]*exp(-spr_10[2]*8/12))+
     (0.5*100*data$coupon[9]*exp(-spr_10[3]*14/12))+
     (0.5*100*data$coupon[9]*exp(-spr_10[4]*20/12))+
     (0.5*100*data$coupon[9]*exp(-spr_10[5]*26/12))+
     (0.5*100*data$coupon[9]*exp(-spr_10[5]*32/12))+
     (0.5*100*data$coupon[9]*exp(-spr_10[6]*38/12))+
     (0.5*100*data$coupon[9]*exp(-spr_10[7]*44/12))+
     (0.5*100*data$coupon[9]*exp(-spr_10[8]*50/12))
   -data$Jan.10[9])
uniroot(f, lower=0, upper=1)$root
spr_10[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-spr_10[1]*2/12))+
     (0.5*100*data$coupon[10]*exp(-spr_10[2]*8/12))+
     (0.5*100*data$coupon[10]*exp(-spr_10[3]*14/12))+
     (0.5*100*data$coupon[10]*exp(-spr_10[4]*20/12))+
     (0.5*100*data$coupon[10]*exp(-spr_10[5]*26/12))+
     (0.5*100*data$coupon[10]*exp(-spr_10[5]*32/12))+
     (0.5*100*data$coupon[10]*exp(-spr_10[6]*38/12))+
     (0.5*100*data$coupon[10]*exp(-spr_10[7]*44/12))+
     (0.5*100*data$coupon[10]*exp(-spr_10[8]*50/12))+
     (0.5*100*data$coupon[10]*exp(-spr_10[9]*56/12))+
     -data$Jan.10[10])
uniroot(f, lower=0, upper=1)$root
spr_10[10]<-uniroot(f, lower=0, upper=1)$root


#spr_13

spr_13<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.13[1])

spr_13[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-spr_13[1]*2/12))-
     data$Jan.13[2])
spr_13[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-spr_13[1]*2/12))
   +(0.5*100*data$coupon[3]*exp(-spr_13[2]*8/12))
   -data$Jan.13[3])
spr_13[3]<-uniroot(f, lower=0, upper=1)$root



f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-spr_13[1]*2/12))+
     (0.5*100*data$coupon[4]*exp(-spr_13[2]*8/12))+
     (0.5*100*data$coupon[4]*exp(-spr_13[3]*14/12))
   -data$Jan.13[4])
uniroot(f, lower=0, upper=1)$root
spr_13[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-spr_13[1]*2/12))+
     (0.5*100*data$coupon[5]*exp(-spr_13[2]*8/12))+
     (0.5*100*data$coupon[5]*exp(-spr_13[3]*14/12))+
     (0.5*100*data$coupon[5]*exp(-spr_13[4]*20/12))
   -data$Jan.13[5])
uniroot(f, lower=0, upper=1)$root
spr_13[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-spr_13[1]*2/12))+
     (0.5*100*data$coupon[6]*exp(-spr_13[2]*8/12))+
     (0.5*100*data$coupon[6]*exp(-spr_13[3]*14/12))+
     (0.5*100*data$coupon[6]*exp(-spr_13[4]*20/12))+
     (0.5*100*data$coupon[6]*exp(-spr_13[5]*26/12))+
     (0.5*100*data$coupon[6]*exp(-spr_13[5]*32/12))
   -data$Jan.13[6])
uniroot(f, lower=0, upper=1)$root
spr_13[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-spr_13[1]*2/12))+
     (0.5*100*data$coupon[7]*exp(-spr_13[2]*8/12))+
     (0.5*100*data$coupon[7]*exp(-spr_13[3]*14/12))+
     (0.5*100*data$coupon[7]*exp(-spr_13[4]*20/12))+
     (0.5*100*data$coupon[7]*exp(-spr_13[5]*26/12))+
     (0.5*100*data$coupon[7]*exp(-spr_13[5]*32/12))
   -data$Jan.13[7])
uniroot(f, lower=0, upper=1)$root
spr_13[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-spr_13[1]*2/12))+
     (0.5*100*data$coupon[8]*exp(-spr_13[2]*8/12))+
     (0.5*100*data$coupon[8]*exp(-spr_13[3]*14/12))+
     (0.5*100*data$coupon[8]*exp(-spr_13[4]*20/12))+
     (0.5*100*data$coupon[8]*exp(-spr_13[5]*26/12))+
     (0.5*100*data$coupon[8]*exp(-spr_13[5]*32/12))+
     (0.5*100*data$coupon[8]*exp(-spr_13[6]*38/12))+
     (0.5*100*data$coupon[8]*exp(-spr_13[7]*44/12))
   -data$Jan.13[8])
uniroot(f, lower=0, upper=1)$root
spr_13[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-spr_13[1]*2/12))+
     (0.5*100*data$coupon[9]*exp(-spr_13[2]*8/12))+
     (0.5*100*data$coupon[9]*exp(-spr_13[3]*14/12))+
     (0.5*100*data$coupon[9]*exp(-spr_13[4]*20/12))+
     (0.5*100*data$coupon[9]*exp(-spr_13[5]*26/12))+
     (0.5*100*data$coupon[9]*exp(-spr_13[5]*32/12))+
     (0.5*100*data$coupon[9]*exp(-spr_13[6]*38/12))+
     (0.5*100*data$coupon[9]*exp(-spr_13[7]*44/12))+
     (0.5*100*data$coupon[9]*exp(-spr_13[8]*50/12))
   -data$Jan.13[9])
uniroot(f, lower=0, upper=1)$root
spr_13[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-spr_13[1]*2/12))+
     (0.5*100*data$coupon[10]*exp(-spr_13[2]*8/12))+
     (0.5*100*data$coupon[10]*exp(-spr_13[3]*14/12))+
     (0.5*100*data$coupon[10]*exp(-spr_13[4]*20/12))+
     (0.5*100*data$coupon[10]*exp(-spr_13[5]*26/12))+
     (0.5*100*data$coupon[10]*exp(-spr_13[5]*32/12))+
     (0.5*100*data$coupon[10]*exp(-spr_13[6]*38/12))+
     (0.5*100*data$coupon[10]*exp(-spr_13[7]*44/12))+
     (0.5*100*data$coupon[10]*exp(-spr_13[8]*50/12))+
     (0.5*100*data$coupon[10]*exp(-spr_13[9]*56/12))+
     -data$Jan.13[10])
uniroot(f, lower=0, upper=1)$root
spr_13[10]<-uniroot(f, lower=0, upper=1)$root




#spr_14

spr_14<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.14[1])

spr_14[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-spr_14[1]*2/12))-
     data$Jan.14[2])
spr_14[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-spr_14[1]*2/12))
   +(0.5*100*data$coupon[3]*exp(-spr_14[2]*8/12))
   -data$Jan.14[3])
spr_14[3]<-uniroot(f, lower=0, upper=1)$root



f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-spr_14[1]*2/12))+
     (0.5*100*data$coupon[4]*exp(-spr_14[2]*8/12))+
     (0.5*100*data$coupon[4]*exp(-spr_14[3]*14/12))
   -data$Jan.14[4])
uniroot(f, lower=0, upper=1)$root
spr_14[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-spr_14[1]*2/12))+
     (0.5*100*data$coupon[5]*exp(-spr_14[2]*8/12))+
     (0.5*100*data$coupon[5]*exp(-spr_14[3]*14/12))+
     (0.5*100*data$coupon[5]*exp(-spr_14[4]*20/12))
   -data$Jan.14[5])
uniroot(f, lower=0, upper=1)$root
spr_14[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-spr_14[1]*2/12))+
     (0.5*100*data$coupon[6]*exp(-spr_14[2]*8/12))+
     (0.5*100*data$coupon[6]*exp(-spr_14[3]*14/12))+
     (0.5*100*data$coupon[6]*exp(-spr_14[4]*20/12))+
     (0.5*100*data$coupon[6]*exp(-spr_14[5]*26/12))+
     (0.5*100*data$coupon[6]*exp(-spr_14[5]*32/12))
   -data$Jan.14[6])
uniroot(f, lower=0, upper=1)$root
spr_14[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-spr_14[1]*2/12))+
     (0.5*100*data$coupon[7]*exp(-spr_14[2]*8/12))+
     (0.5*100*data$coupon[7]*exp(-spr_14[3]*14/12))+
     (0.5*100*data$coupon[7]*exp(-spr_14[4]*20/12))+
     (0.5*100*data$coupon[7]*exp(-spr_14[5]*26/12))+
     (0.5*100*data$coupon[7]*exp(-spr_14[5]*32/12))
   -data$Jan.14[7])
uniroot(f, lower=0, upper=1)$root
spr_14[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-spr_14[1]*2/12))+
     (0.5*100*data$coupon[8]*exp(-spr_14[2]*8/12))+
     (0.5*100*data$coupon[8]*exp(-spr_14[3]*14/12))+
     (0.5*100*data$coupon[8]*exp(-spr_14[4]*20/12))+
     (0.5*100*data$coupon[8]*exp(-spr_14[5]*26/12))+
     (0.5*100*data$coupon[8]*exp(-spr_14[5]*32/12))+
     (0.5*100*data$coupon[8]*exp(-spr_14[6]*38/12))+
     (0.5*100*data$coupon[8]*exp(-spr_14[7]*44/12))
   -data$Jan.14[8])
uniroot(f, lower=0, upper=1)$root
spr_14[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-spr_14[1]*2/12))+
     (0.5*100*data$coupon[9]*exp(-spr_14[2]*8/12))+
     (0.5*100*data$coupon[9]*exp(-spr_14[3]*14/12))+
     (0.5*100*data$coupon[9]*exp(-spr_14[4]*20/12))+
     (0.5*100*data$coupon[9]*exp(-spr_14[5]*26/12))+
     (0.5*100*data$coupon[9]*exp(-spr_14[5]*32/12))+
     (0.5*100*data$coupon[9]*exp(-spr_14[6]*38/12))+
     (0.5*100*data$coupon[9]*exp(-spr_14[7]*44/12))+
     (0.5*100*data$coupon[9]*exp(-spr_14[8]*50/12))
   -data$Jan.14[9])
uniroot(f, lower=0, upper=1)$root
spr_14[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-spr_14[1]*2/12))+
     (0.5*100*data$coupon[10]*exp(-spr_14[2]*8/12))+
     (0.5*100*data$coupon[10]*exp(-spr_14[3]*14/12))+
     (0.5*100*data$coupon[10]*exp(-spr_14[4]*20/12))+
     (0.5*100*data$coupon[10]*exp(-spr_14[5]*26/12))+
     (0.5*100*data$coupon[10]*exp(-spr_14[5]*32/12))+
     (0.5*100*data$coupon[10]*exp(-spr_14[6]*38/12))+
     (0.5*100*data$coupon[10]*exp(-spr_14[7]*44/12))+
     (0.5*100*data$coupon[10]*exp(-spr_14[8]*50/12))+
     (0.5*100*data$coupon[10]*exp(-spr_14[9]*56/12))+
     -data$Jan.14[10])
uniroot(f, lower=0, upper=1)$root
spr_14[10]<-uniroot(f, lower=0, upper=1)$root


#spr_15

spr_15<-rep(0,10)

f <- function(x)  
  ((100+0.5*100*data$coupon[1])*exp(-x*2/12)
   -data$Jan.15[1])

spr_15[1]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[2])*exp(-x*8/12)+
     (0.5*100*data$coupon[2]*exp(-spr_15[1]*2/12))-
     data$Jan.15[2])
spr_15[2]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[3])*exp(-x*14/12)
   +(0.5*100*data$coupon[3]*exp(-spr_15[1]*2/12))
   +(0.5*100*data$coupon[3]*exp(-spr_15[2]*8/12))
   -data$Jan.15[3])
spr_15[3]<-uniroot(f, lower=0, upper=1)$root



f <- function(x)  
  ((100+0.5*100*data$coupon[4])*exp(-x*20/12)+
     (0.5*100*data$coupon[4]*exp(-spr_15[1]*2/12))+
     (0.5*100*data$coupon[4]*exp(-spr_15[2]*8/12))+
     (0.5*100*data$coupon[4]*exp(-spr_15[3]*14/12))
   -data$Jan.15[4])
uniroot(f, lower=0, upper=1)$root
spr_15[4]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[5])*exp(-x*26/12)+
     (0.5*100*data$coupon[5]*exp(-spr_15[1]*2/12))+
     (0.5*100*data$coupon[5]*exp(-spr_15[2]*8/12))+
     (0.5*100*data$coupon[5]*exp(-spr_15[3]*14/12))+
     (0.5*100*data$coupon[5]*exp(-spr_15[4]*20/12))
   -data$Jan.15[5])
uniroot(f, lower=0, upper=1)$root
spr_15[5]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[6])*exp(-x*38/12)+
     (0.5*100*data$coupon[6]*exp(-spr_15[1]*2/12))+
     (0.5*100*data$coupon[6]*exp(-spr_15[2]*8/12))+
     (0.5*100*data$coupon[6]*exp(-spr_15[3]*14/12))+
     (0.5*100*data$coupon[6]*exp(-spr_15[4]*20/12))+
     (0.5*100*data$coupon[6]*exp(-spr_15[5]*26/12))+
     (0.5*100*data$coupon[6]*exp(-spr_15[5]*32/12))
   -data$Jan.15[6])
uniroot(f, lower=0, upper=1)$root
spr_15[6]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[7])*exp(-x*41/12)+
     (0.5*100*data$coupon[7]*exp(-spr_15[1]*2/12))+
     (0.5*100*data$coupon[7]*exp(-spr_15[2]*8/12))+
     (0.5*100*data$coupon[7]*exp(-spr_15[3]*14/12))+
     (0.5*100*data$coupon[7]*exp(-spr_15[4]*20/12))+
     (0.5*100*data$coupon[7]*exp(-spr_15[5]*26/12))+
     (0.5*100*data$coupon[7]*exp(-spr_15[5]*32/12))
   -data$Jan.15[7])
uniroot(f, lower=0, upper=1)$root
spr_15[7]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[8])*exp(-x*50/12)+
     (0.5*100*data$coupon[8]*exp(-spr_15[1]*2/12))+
     (0.5*100*data$coupon[8]*exp(-spr_15[2]*8/12))+
     (0.5*100*data$coupon[8]*exp(-spr_15[3]*14/12))+
     (0.5*100*data$coupon[8]*exp(-spr_15[4]*20/12))+
     (0.5*100*data$coupon[8]*exp(-spr_15[5]*26/12))+
     (0.5*100*data$coupon[8]*exp(-spr_15[5]*32/12))+
     (0.5*100*data$coupon[8]*exp(-spr_15[6]*38/12))+
     (0.5*100*data$coupon[8]*exp(-spr_15[7]*44/12))
   -data$Jan.15[8])
uniroot(f, lower=0, upper=1)$root
spr_15[8]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[9])*exp(-x*56/12)+
     (0.5*100*data$coupon[9]*exp(-spr_15[1]*2/12))+
     (0.5*100*data$coupon[9]*exp(-spr_15[2]*8/12))+
     (0.5*100*data$coupon[9]*exp(-spr_15[3]*14/12))+
     (0.5*100*data$coupon[9]*exp(-spr_15[4]*20/12))+
     (0.5*100*data$coupon[9]*exp(-spr_15[5]*26/12))+
     (0.5*100*data$coupon[9]*exp(-spr_15[5]*32/12))+
     (0.5*100*data$coupon[9]*exp(-spr_15[6]*38/12))+
     (0.5*100*data$coupon[9]*exp(-spr_15[7]*44/12))+
     (0.5*100*data$coupon[9]*exp(-spr_15[8]*50/12))
   -data$Jan.15[9])
uniroot(f, lower=0, upper=1)$root
spr_15[9]<-uniroot(f, lower=0, upper=1)$root

f <- function(x)  
  ((100+0.5*100*data$coupon[10])*exp(-x*62/12)+
     (0.5*100*data$coupon[10]*exp(-spr_15[1]*2/12))+
     (0.5*100*data$coupon[10]*exp(-spr_15[2]*8/12))+
     (0.5*100*data$coupon[10]*exp(-spr_15[3]*14/12))+
     (0.5*100*data$coupon[10]*exp(-spr_15[4]*20/12))+
     (0.5*100*data$coupon[10]*exp(-spr_15[5]*26/12))+
     (0.5*100*data$coupon[10]*exp(-spr_15[5]*32/12))+
     (0.5*100*data$coupon[10]*exp(-spr_15[6]*38/12))+
     (0.5*100*data$coupon[10]*exp(-spr_15[7]*44/12))+
     (0.5*100*data$coupon[10]*exp(-spr_15[8]*50/12))+
     (0.5*100*data$coupon[10]*exp(-spr_15[9]*56/12))+
     -data$Jan.15[10])
uniroot(f, lower=0, upper=1)$root
spr_15[10]<-uniroot(f, lower=0, upper=1)$root





plot(t,spr_2,type = "b", ylim = c(0.014,0.025), main = "5-year spot rate curve",ylab = "spot rate")
points(t,spr_3,col=2);lines(t,spr_3,col=2)
points(t,spr_6,col=3);lines(t,spr_6,col=3)
points(t,spr_7,col=4);lines(t,spr_7,col=4)
points(t,spr_8,col=5);lines(t,spr_8,col=5)
points(t,spr_9,col=6);lines(t,spr_9,col=6)
points(t,spr_10,col=7);lines(t,spr_10,col=7)
points(t,spr_13,col=8);lines(t,spr_13,col=8)
points(t,spr_14,col=9);lines(t,spr_14,col=9)
points(t,spr_15,col=10);lines(t,spr_15,col=10)

legend("topright", legend = c("Jan 2","Jan 3","Jan 6","Jan 7","Jan 8", "Jan 9", "Jan 10","Jan 13","Jan 14","Jan 15"),
       col=1:10, lty =1, cex = 0.7)







#4(c) forward

s1<-0.01860411+(1-8/12)/(14/12-8/12)*(0.01706732-0.01860411)
s2<-0.01713799+(2-20/12)/(26/12-20/12)*(0.01639587-0.01713799)
s3<-0.01639587+(3-26/12)/(38/12-26/12)*(0.01643282-0.01639587)
s4<-0.01542468+(4-41/12)/(50/12-41/12)*(0.01613476-0.01542468)
s5<-0.01783411+(5-56/12)/(62/12-56/12)*(0.01597756-0.01783411)


#f2

s2_1<-spr_2[2]+(1-8/12)/(14/12-8/12)*(spr_2[3]-spr_2[2])
s2_2<-spr_2[4]+(2-20/12)/(26/12-20/12)*(spr_2[5]-spr_2[4])
s2_3<-spr_2[5]+(3-26/12)/(38/12-26/12)*(spr_2[6]-spr_2[5])
s2_4<-spr_2[7]+(4-41/12)/(50/12-41/12)*(spr_2[8]-spr_2[7])
s2_5<-spr_2[9]+(5-56/12)/(62/12-56/12)*(spr_2[10]-spr_2[9])


f2<-rep(0,4)
f2[1]<-(s2_2*2-s2_1)/(2-1)
f2[2]<-(s2_3*3-s2_1)/(3-1)
f2[3]<-(s2_4*4-s2_1)/(4-1)
f2[4]<-(s2_5*5-s2_1)/(5-1)


#f3

s3_1<-spr_3[2]+(1-8/12)/(14/12-8/12)*(spr_3[3]-spr_3[2])
s3_2<-spr_3[4]+(2-20/12)/(26/12-20/12)*(spr_3[5]-spr_3[4])
s3_3<-spr_3[5]+(3-26/12)/(38/12-26/12)*(spr_3[6]-spr_3[5])
s3_4<-spr_3[7]+(4-41/12)/(50/12-41/12)*(spr_3[8]-spr_3[7])
s3_5<-spr_3[9]+(5-56/12)/(62/12-56/12)*(spr_3[10]-spr_3[9])


f3<-rep(0,4)
f3[1]<-(s3_2*2-s3_1)/(2-1)
f3[2]<-(s3_3*3-s3_1)/(3-1)
f3[3]<-(s3_4*4-s3_1)/(4-1)
f3[4]<-(s3_5*5-s3_1)/(5-1)


#f6

s6_1<-spr_2[2]+(1-8/12)/(14/12-8/12)*(spr_6[3]-spr_6[2])
s6_2<-spr_2[4]+(2-20/12)/(26/12-20/12)*(spr_6[5]-spr_6[4])
s6_3<-spr_2[5]+(3-26/12)/(38/12-26/12)*(spr_6[6]-spr_6[5])
s6_4<-spr_2[7]+(4-41/12)/(50/12-41/12)*(spr_6[8]-spr_6[7])
s6_5<-spr_2[9]+(5-56/12)/(62/12-56/12)*(spr_6[10]-spr_6[9])


f6<-rep(0,4)
f6[1]<-(s6_2*2-s6_1)/(2-1)
f6[2]<-(s6_3*3-s6_1)/(3-1)
f6[3]<-(s6_4*4-s6_1)/(4-1)
f6[4]<-(s6_5*5-s6_1)/(5-1)



#f7
s7_1<-spr_7[2]+(1-8/12)/(14/12-8/12)*(spr_7[3]-spr_7[2])
s7_2<-spr_7[4]+(2-20/12)/(26/12-20/12)*(spr_7[5]-spr_7[4])
s7_3<-spr_7[5]+(3-26/12)/(38/12-26/12)*(spr_7[6]-spr_7[5])
s7_4<-spr_7[7]+(4-41/12)/(50/12-41/12)*(spr_7[8]-spr_7[7])
s7_5<-spr_7[9]+(5-56/12)/(62/12-56/12)*(spr_7[10]-spr_7[9])


f7<-rep(0,4)
f7[1]<-(s7_2*2-s7_1)/(2-1)
f7[2]<-(s7_3*3-s7_1)/(3-1)
f7[3]<-(s7_4*4-s7_1)/(4-1)
f7[4]<-(s7_5*5-s7_1)/(5-1)



#f8
s8_1<-spr_8[2]+(1-8/12)/(14/12-8/12)*(spr_8[3]-spr_8[2])
s8_2<-spr_8[4]+(2-20/12)/(26/12-20/12)*(spr_8[5]-spr_8[4])
s8_3<-spr_8[5]+(3-26/12)/(38/12-26/12)*(spr_8[6]-spr_8[5])
s8_4<-spr_8[7]+(4-41/12)/(50/12-41/12)*(spr_8[8]-spr_8[7])
s8_5<-spr_8[9]+(5-56/12)/(62/12-56/12)*(spr_8[10]-spr_8[9])


f8<-rep(0,4)
f8[1]<-(s8_2*2-s8_1)/(2-1)
f8[2]<-(s8_3*3-s8_1)/(3-1)
f8[3]<-(s8_4*4-s8_1)/(4-1)
f8[4]<-(s8_5*5-s8_1)/(5-1)





#f9
s9_1<-spr_9[2]+(1-8/12)/(14/12-8/12)*(spr_9[3]-spr_9[2])
s9_2<-spr_9[4]+(2-20/12)/(26/12-20/12)*(spr_9[5]-spr_9[4])
s9_3<-spr_9[5]+(3-26/12)/(38/12-26/12)*(spr_9[6]-spr_9[5])
s9_4<-spr_9[7]+(4-41/12)/(50/12-41/12)*(spr_9[8]-spr_9[7])
s9_5<-spr_9[9]+(5-56/12)/(62/12-56/12)*(spr_9[10]-spr_9[9])


f9<-rep(0,4)
f9[1]<-(s9_2*2-s9_1)/(2-1)
f9[2]<-(s9_3*3-s9_1)/(3-1)
f9[3]<-(s9_4*4-s9_1)/(4-1)
f9[4]<-(s9_5*5-s9_1)/(5-1)



#f10
s10_1<-spr_10[2]+(1-8/12)/(14/12-8/12)*(spr_10[3]-spr_10[2])
s10_2<-spr_10[4]+(2-20/12)/(26/12-20/12)*(spr_10[5]-spr_10[4])
s10_3<-spr_10[5]+(3-26/12)/(38/12-26/12)*(spr_10[6]-spr_10[5])
s10_4<-spr_10[7]+(4-41/12)/(50/12-41/12)*(spr_10[8]-spr_10[7])
s10_5<-spr_10[9]+(5-56/12)/(62/12-56/12)*(spr_10[10]-spr_10[9])


f10<-rep(0,4)
f10[1]<-(s10_2*2-s10_1)/(2-1)
f10[2]<-(s10_3*3-s10_1)/(3-1)
f10[3]<-(s10_4*4-s10_1)/(4-1)
f10[4]<-(s10_5*5-s10_1)/(5-1)





#f13
s13_1<-spr_13[2]+(1-8/12)/(14/12-8/12)*(spr_13[3]-spr_13[2])
s13_2<-spr_13[4]+(2-20/12)/(26/12-20/12)*(spr_13[5]-spr_13[4])
s13_3<-spr_13[5]+(3-26/12)/(38/12-26/12)*(spr_13[6]-spr_13[5])
s13_4<-spr_13[7]+(4-41/12)/(50/12-41/12)*(spr_13[8]-spr_13[7])
s13_5<-spr_13[9]+(5-56/12)/(62/12-56/12)*(spr_13[10]-spr_13[9])


f13<-rep(0,4)
f13[1]<-(s13_2*2-s13_1)/(2-1)
f13[2]<-(s13_3*3-s13_1)/(3-1)
f13[3]<-(s13_4*4-s13_1)/(4-1)
f13[4]<-(s13_5*5-s13_1)/(5-1)



#f14
s14_1<-spr_14[2]+(1-8/12)/(14/12-8/12)*(spr_14[3]-spr_14[2])
s14_2<-spr_14[4]+(2-20/12)/(26/12-20/12)*(spr_14[5]-spr_14[4])
s14_3<-spr_14[5]+(3-26/12)/(38/12-26/12)*(spr_14[6]-spr_14[5])
s14_4<-spr_14[7]+(4-41/12)/(50/12-41/12)*(spr_14[8]-spr_14[7])
s14_5<-spr_14[9]+(5-56/12)/(62/12-56/12)*(spr_14[10]-spr_14[9])


f14<-rep(0,4)
f14[1]<-(s14_2*2-s14_1)/(2-1)
f14[2]<-(s14_3*3-s14_1)/(3-1)
f14[3]<-(s14_4*4-s14_1)/(4-1)
f14[4]<-(s14_5*5-s14_1)/(5-1)


#f15
s15_1<-spr_15[2]+(1-8/12)/(14/12-8/12)*(spr_15[3]-spr_15[2])
s15_2<-spr_15[4]+(2-20/12)/(26/12-20/12)*(spr_15[5]-spr_15[4])
s15_3<-spr_15[5]+(3-26/12)/(38/12-26/12)*(spr_15[6]-spr_15[5])
s15_4<-spr_15[7]+(4-41/12)/(50/12-41/12)*(spr_15[8]-spr_15[7])
s15_5<-spr_15[9]+(5-56/12)/(62/12-56/12)*(spr_15[10]-spr_15[9])


f15<-rep(0,4)
f15[1]<-(s15_2*2-s15_1)/(2-1)
f15[2]<-(s15_3*3-s15_1)/(3-1)
f15[3]<-(s15_4*4-s15_1)/(4-1)
f15[4]<-(s15_5*5-s15_1)/(5-1)






plot(c(1:4),f2,type = "b", ylim = c(0.015,0.018), main = "5-year forward rate curve",ylab = "forward rate")
points(c(1:4),f3,col=2);lines(c(1:4),f3,col=2)
points(c(1:4),f6,col=3);lines(c(1:4),f6,col=3)
points(c(1:4),f7,col=4);lines(c(1:4),f7,col=4)
points(c(1:4),f8,col=5);lines(c(1:4),f8,col=5)
points(c(1:4),f9,col=6);lines(c(1:4),f9,col=6)
points(c(1:4),f10,col=7);lines(c(1:4),f10,col=7)
points(c(1:4),f13,col=8);lines(c(1:4),f13,col=8)
points(c(1:4),f14,col=9);lines(c(1:4),f14,col=9)
points(c(1:4),f15,col=10);lines(c(1:4),f15,col=10)

legend("topright", legend = c("Jan 2","Jan 3","Jan 6","Jan 7","Jan 8", "Jan 9", "Jan 10","Jan 13","Jan 14","Jan 15"),
       col=1:10, lty =1, cex = 0.4)


















#5 
ytm<-cbind(ytm_2,ytm_3,ytm_6,ytm_7,ytm_8,ytm_9,ytm_10,ytm_13,ytm_14,ytm_15)
lny<-cbind(ytm_3,ytm_6,ytm_7,ytm_8,ytm_9,ytm_10,ytm_13,ytm_14,ytm_15)

for (i in 1:9) {
     lny[i,]<-diff(log(ytm[i,]))
   }

lny<-lny[-c(1,2,4,7,9),]
row.names(lny)<-c("y1","y2","y3","y4","y5")
colnames(lny)<-c("1","2","3","4","5","6","7","8","9")





f<-cbind(f2,f3,f6,f7,f8,f9,f10,f13,f14,f15)
lnf<-cbind(f3,f6,f7,f8,f9,f10,f13,f14,f15)

for (i in 1:4) {
  lnf[i,]<-diff(log(f[i,]))
}

row.names(lnf)<-c("y1_1","y1_2","y1_3","y1_4")
colnames(lnf)<-c("1","2","3","4","5","6","7","8","9")



cov_lny<-cov(t(lny))
cov_lnf<-cov(t(lnf))


#6

e_lny<-eigen(cov_lny)
e_lnf<-eigen(cov_lnf)
e_lny$values[1]/sum(e_lny$values)
e_lnf$values[1]/sum(e_lnf$values)


