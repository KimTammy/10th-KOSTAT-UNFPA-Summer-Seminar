
###
### Life Years lost abridged
###

## select a country
Name<-c("USA")

## Select a year Y1
Y1<-2015

#### Data Preparation ####

raw_data <- read.csv("C:/Users/Tammy/Documents/During KOSTAT summer seminar/LYLextra/USA_d_short_idr.csv")

raw_data <- raw_data[raw_data$sex!=3,]

raw_data <- raw_data[,c(1:3,6,7:25,27,29,30)]

Data <- c()

for(x in unique(raw_data$year)){
  for(y in unique(raw_data$sex)){
    for(z in unique(raw_data$cause)){
      
      data_temp <- 
        raw_data[raw_data$year==x&
                   raw_data$sex==y&
                   raw_data$cause==z,c(5:26)]
      
      df_temp <- 
        data.frame(
          Year = x,
          Sex = ifelse(y==1,"Male","Female"),
          Cause = z,
          Age = c(0,1,seq(5,95,5)),
          Count = c(as.matrix(data_temp))[-1]
        )
      
      Data <- rbind(Data,df_temp)
      # data_temp <- NULL
      # df_temp <- NULL
    }
  }
}

Data2 <- reshape2::dcast(Data,Year+Cause+Age~Sex,
                         value.var = "Count",
                         fun.aggregate = sum)

Data2 <- Data2[Data2$Cause!=0,]

AA1<-read.table("C:/Users/Tammy/Documents/During KOSTAT summer seminar/LYLextra/mltper_1x1.txt",
                header=TRUE,fill=TRUE,skip=1)
A1<-AA1[(AA1$Year==Y1),]

BM<-Data2[Data2$Year==Y1,]

#### Function ####

LostYears<-function(A1,BM,Y1,Name){
  
  
  ## each of the life table functions will need to be 
  ## age-grouped because the cause of death data comes 
  ## in groups as shown in Ages
  
  Ages<-c(paste(seq(0,80,by=5),"-",seq(4,84,by=5),sep=""),"85+")
  
  
  q1<-as.numeric(A1$qx) 
  l1<-as.numeric(A1$lx)/100000
  d1<-as.numeric(A1$dx)/100000
  L1<-as.numeric(A1$Lx)/100000
  T1<-as.numeric(A1$Tx)/100000
  e1<-as.numeric(A1$ex)
  a1<-as.numeric(A1$ax)
  mx<-as.numeric(A1$mx)
  
  dx<-c(sum(d1[1:5]),sum(d1[6:10]),sum(d1[11:15]),sum(d1[16:20]),
        sum(d1[21:25]),sum(d1[26:30]),sum(d1[31:35]),sum(d1[36:40]),sum(d1[41:45]),
        sum(d1[46:50]),sum(d1[51:55]),sum(d1[56:60]),sum(d1[61:65]),sum(d1[66:70]),
        sum(d1[71:75]),sum(d1[76:80]),sum(d1[81:85]),l1[86])
  
  Ndx<-length(dx)
  Last<-1-(sum(dx[-Ndx]))
  dx<-c(dx[-Ndx],Last)
  
  Lx<-c(sum(L1[1:5]),sum(L1[6:10]),sum(L1[11:15]),sum(L1[16:20]),
        sum(L1[21:25]),sum(L1[26:30]),sum(L1[31:35]),sum(L1[36:40]),sum(L1[41:45]),
        sum(L1[46:50]),sum(L1[51:55]),sum(L1[56:60]),sum(L1[61:65]),sum(L1[66:70]),
        sum(L1[71:75]),sum(L1[76:80]),sum(L1[81:85]),T1[86])
  
  Tx<-c(T1[1],T1[6],T1[11],T1[16],T1[21],T1[26],T1[31],T1[36],T1[41],
        T1[46],T1[51],T1[56],T1[61],T1[66],T1[71],T1[76],T1[81],T1[86])
  
  lx<-c(l1[1],l1[6],l1[11],l1[16],l1[21],l1[26],l1[31],l1[36],l1[41],
        l1[46],l1[51],l1[56],l1[61],l1[66],l1[71],l1[76],l1[81],Last)
  
  ## we need a second lx to make the ploting 
  lxPlot<-c(l1[1],l1[6],l1[11],l1[16],l1[21],l1[26],l1[31],l1[36],l1[41],
            l1[46],l1[51],l1[56],l1[61],l1[66],l1[71],l1[76],l1[81],Last,0)
  
  ax<-c(sum(a1[1:5]),sum(a1[6:10]),sum(a1[11:15]),sum(a1[16:20]),
        sum(a1[21:25]),sum(a1[26:30]),sum(a1[31:35]),sum(a1[36:40]),sum(a1[41:45]),
        sum(a1[46:50]),sum(a1[51:55]),sum(a1[56:60]),sum(a1[61:65]),sum(a1[66:70]),
        sum(a1[71:75]),sum(a1[76:80]),sum(a1[81:85]),1/mx[Ndx])
  
  ## now we can construct our desire death rates and probabilites by age-groups
  
  mx<-dx/Lx
  qx<-dx/lx 
  px<-1-qx
  ex<-Tx/lx
  
  ## we also need to group some of the cause of death data
  NUM<-as.numeric(names(table(BM$Cause)))
  BM<-matrix(BM$Male,21)
  BM<-rbind(colSums(BM[1:2,]),BM[3:18,],colSums(BM[19:21,]))
  
  ### Causes of Death that we have are the following
  NC<-
    c("Infectious","Neoplasms",
      "Diseases of the blood and blood-forming organs",
      "Endocrine, nutritional and metabolic diseases",
      "Mental and behavioral disorders",
      "Diseases of the nervous system and the sense organs",
      "Heart diseases","Cerebrovascular diseases",
      "Other and unspecified disorders of the circulatory system",
      "Acute respiratory diseases",
      "Other respiratory diseases",
      "Diseases of the digestive system",
      "Diseases of the skin and tissue",
      "Diseases related to childbirth",
      "Certain conditions originating in the perinatal period",
      "External causes")
  
  ## since we also have the total deaths at each age we can also calcualte the 
  ## remaining causes 
  NamesCauses<-c(NC)
  NumC<-length(NamesCauses)  
  NamesCauses<-NamesCauses[NUM]
  
  ### First create a matrix of percentage of deaths by cause (including "others")
  ### for the total population
  Ec<-c()
  
  ET<-matrix(rep(rowSums(BM),dim(BM)[2]),dim(BM)[1])
  Ec<-BM/ET
  Rx1<-Ec
  
  ### now  we calculate the deaths at each age for each cause
  lx1<-matrix(rep(lx,dim(BM)[2]),dim(BM)[1])
  qx1<-matrix(rep(qx,dim(BM)[2]),dim(BM)[1])
  
  dxi<-Rx1*qx1*lx1
  
  ### We are grouping the causes 
  
  Cancer<-dxi[,2]
  Heart<-rowSums(dxi[,7:8])
  External<-dxi[,16]
  Others<-rowSums(dxi[,c(1,3:6,9:15)])
  
  
  ## we canculate the accumulated deaths for each cause
  CancerA<-c(0,cumsum(Cancer))
  HeartA<-c(0,cumsum(Heart))
  ExternalA<-c(0,cumsum(External))
  OthersA<-c(0,cumsum(Others))
  ALL<-CancerA+HeartA+ExternalA+OthersA

  ## now we make the plot of the survival function and the causes of death
  ## contributing to the lost years
  Age<-seq(0,85,by=5)
  lxPlot<-lxPlot[-19]
  CancerA<-CancerA[-19]
  HeartA<-HeartA[-19]
  ExternalA<-ExternalA[-19]
  OthersA<-OthersA[-19]
  
  ## first just the lines
  plot(c(0,85),c(0,1),col=0,xlab="Ages",ylab="Survivors")
  lines(Age,lxPlot,col=1)
  lines(Age,lxPlot+CancerA,col=2)
  lines(Age,lxPlot+CancerA+HeartA,col=3)
  lines(Age,lxPlot+CancerA+HeartA+OthersA,col=4)
  lines(Age,lxPlot+CancerA+HeartA+ExternalA+OthersA,col=5)
  
  legend(20,.8,c("External","Others","CVD","Cancer"),col=c(7,6,1,2),pch=15,box.lty=0)
  
  title(paste("Life table distribution of survivors and deaths by cause of death, 
for males from ",Name," in year ",Y1))
  mtext("Source: HMD & HCD.",1,3,adj=-.15,cex=.8)
  
  Nage<-length(Age)
  
  ## and now with the areas in between
  xx <- c(Age, rev(Age))
  yyC <-c(lxPlot,rev(lxPlot+CancerA))
  polygon(xx, yyC,col=2,border=2)
  
  yyH <-c(lxPlot+CancerA,rev(HeartA+CancerA+lxPlot))
  polygon(xx, yyH,col=1,border=1)
  
  yyI <-c(HeartA+CancerA+lxPlot,rev(CancerA+HeartA+OthersA+lxPlot))
  polygon(xx, yyI,col=6,border=6)
  
  yyO <-c(lxPlot+HeartA+CancerA+OthersA,rev(lxPlot+CancerA+HeartA+ExternalA+OthersA))
  polygon(xx, yyO,col=7,border=7)
  
  
  ## and the age threshold that we selected
  abline(v=55,col="black",lty=3,lwd=2)
  text(53,0,55)
  
  abline(v=70,col="black",lty=3,lwd=2)
  text(68,0,70)
  
  abline(v=85,col="black",lty=3,lwd=2)
  text(83,0,85)
  
}

#### Analysis ####

M1<-LostYears(A1,BM,Y1,Name)

