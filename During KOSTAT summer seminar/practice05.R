
library(ggplot2)
library(knitr)
library(colorRamps)
library(RColorBrewer)
library(tidyverse)
library(data.table)
library(reshape2)
options(scipen = 100000,digits=4)
library(readxl)


RxiMatrix<-function(PP,Cum){
  #PP<-B
  NumC<-dim(PP)[2]
  NumR<-dim(PP)[1]
  
  G<-PP
  FD<-colSums(PP)
  
  if (Cum==1){
    G<-t(apply(G,1,cumsum))
    FD<-cumsum(FD)
  } 
  
  FRx3<-t(matrix(rep(FD,(NumR)),NumC))
  FRx<-G/FRx3
  FRx[is.infinite(FRx)]<-0
  return(FRx)
}

# A set of life table functions

Calculate_a0 <- function(m0,sex) {
  #Andreev-Kingkade formulas for computing a0 given m0
  # HMD full protocol Table 1 pg37
  #Males
  if(sex=="m"){
    if (m0<0.02300) {a0<-0.14929-(1.99545*m0)}
    if ((0.0230<= m0)&(m0<0.08307)) {a0<-0.02832+(3.26021*m0)}
    if (0.08307<= m0) {a0<-0.29915}
  }
  if (sex=="f"){
    #Females
    if (m0<0.01724) {a0<-0.14903-(2.05527*m0)}
    if ((0.01724 <= m0)&(m0< 0.06891)) {a0<-0.04667+(3.88089*m0)}
    if (0.06891<= m0) {a0<-0.31411}
  }
  return(a0) }

lifetable.mx<-function(mx,sex){
  
  N<-length(mx)
  AgeI<-rep(1,N)
  a0<-0.5
  if(AGEF==0){a0<-Calculate_a0(mx[1],sex)}                                       
  ax<-c(a0,rep(0.5,(N-1)))
  if(mx[N]>0){ax[N]<-1/mx[N]}                                       
    qx<-mx/(1+(1-ax)*mx)
  qx[N]<-1 
  
  px<-1-qx
  
  lx<-100000
  
  for(y in 1:(N-1)){          
    lx[y+1]<-lx[y]*px[y]
  }
  
  dx<-lx*qx
  dx[N]<-lx[N]
  
  Lx<- lx+(ax-AgeI)*dx                                       
    Lx[N]<-lx[N]*ax[N]                 
  
  Tx<-c()
  for(y in 1:N){
    Tx[y]<-sum(Lx[y:N])
  }
  
  ex<-Tx/lx
  Age<-AGEF:(AGEF+N-1) 
  AgeI<-rep(1,N)
  ALL<-cbind(Age,AgeI,ax,mx,qx,lx,dx,Lx,Tx,ex)
  return(ALL)
}

# Calculating life years lost based on the 
# life table we have just calculated

LostYears2<-function(FLT,B){
  
  N<-dim(FLT)[1]
  AgeI<-rep(1,N)
  lx<-as.numeric(FLT[,6])/100000
  dx<-c(lx[-N]-lx[-1],lx[N])
  ax<-as.numeric(FLT[,5])
  
  Lx<-AgeI*lx+(ax-AgeI)*dx                                        
  Lx[N]<-lx[N]*ax[N]                                        
    
    
    Rxi<-RxiMatrix(B,0)
  fxi<-RxiMatrix(B,1)
  
  Nrow<-dim(Rxi)[1]
  Ncol<-dim(Rxi)[2]
  
  ## we use the life table functions to separate the 
  ## person years and person lost
  
  LYL<-AgeI-Lx
  
  # age-specific LYLs
  LYLi2<-c()
  LYLi3<-matrix(0,Nrow,Ncol)
  for (y in 1:(Ncol-1)){
    L<-matrix(0,Nrow,Ncol)
    L[,y]<-(AgeI[y]-ax[y])*dx[y]*Rxi[,y]                                       
      L[,(y+1):Ncol]<-                                
      matrix(rep(AgeI[(y+1):Ncol], each=Nrow) ,Nrow)*
      matrix(rep(dx[y]*Rxi[,y], length((y+1):Ncol)), Nrow)
      LYLi2<- cbind(LYLi2,rowSums(L))
    LYLi3<-(LYLi3+L)                                       
  }
  y<-Ncol
  LYLi2<- cbind(LYLi2, (AgeI[y]-ax[y])*dx[y]*Rxi[,y])                                        
    LYLi3[,y]<-LYLi3[,y]+LYLi2[,y]
  
  return(LYLi2)
  
}

# This function returns the life years lost 
# visualization in the terms of the complement of 
# the survival curve in a life table.

NicePlot2<-function(FLT,B,S){
  
  causesN0<-Causes
  causesN<-causesN0
  NcausesT<-length(Causes)
  Ncauses<-length(S)
  Dif<-length(Causes)-Ncauses                                        ## fix this
    
    
    #Selected causes
    if(Dif>0){causesN <- c(causesN0[S], "Other causes")}                                        ## fix this
      if(Dif>0){Ncauses<-(length(S)+1)}
  
  # COL<-matlab.like2(NcausesT)
  COL<-primary.colors(NcausesT)
  
  N<-dim(FLT)[1]
  AgeI<-rep(1,N)
  lx<-as.numeric(FLT[,6])/100000
  dx<-c(lx[-N]-lx[-1],lx[N])
  
  FRx<-RxiMatrix(B,0)
  Fdxi<-FRx*matrix(rep(dx,each=NcausesT),NcausesT)                                        ## fix this
    dxi0<-apply(Fdxi,1,cumsum)
  
  ## now we make the plot of the survival 
  ## function and the causes of death
  ## contributing to the lost years
  
  ## first just the lines
  dxi <- dxi0
  if(Dif>0){ dxi <- cbind(dxi0[,S],rowSums(dxi0[,-S]))}
  dxi.1 <- rep(0,dim(dxi)[2])
  dxi.f <- rbind(dxi.1, dxi[-N,])
  
  test.p0 <- cbind(lx, dxi.f)
  test.p1 <- cbind(test.p0[1:length(Age),], Age )
  test.p1[is.na(test.p1)]<-0
  test.p2 <- as.data.frame(test.p1)
  names(test.p2)
  
  test.p <- reshape2::melt(test.p2, id.vars = c("lx", "Age"))
  
  # Alt colors
  ggplot(test.p, aes(x= Age, y = value,
                     group = variable, fill = variable)) + 
    geom_area(size = 0.3, color = "black", 
              position = position_stack()) + 
    scale_y_reverse(name = "Probability of surviving and life years lost",
                    breaks = c(0, .2, .4, .6, .8, 1), 
                    labels = c(1, .8, .6, .4, .2, 0), 
                    limits = c(1,0)) + 
    scale_x_continuous(name = "Age",
                       limits = c(AGEF, AGEL)) + 
    scale_fill_manual(name="", values = COL,
                      labels = causesN) +
    theme_bw(base_size = 10) + 
    theme(legend.position = "bottom") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 
}




#install.packages("colorRamps")
#install.packages("reshape2")
library(colorRamps)
library(reshape2)

##### Here you can change the sex under study 1 = Male and 2 = Female
SEX<-c("Male","Female")
Sex<-2                                        ## fix this
  
  
  #####  here you can change the ages from 0 to 95
  #####  AGEF = Age First     AGEL = Age Last
AGEF<-0                                        ## fix this
AGEL<-90                                        ## fix this
  
  
  ##### population, we will use France
Names<-c("FRATNP")
Names2<-c("France")

##### here you can change the year from 1950 to 2014

Year<-1950                                        ## fix this
  
  
  #country codes
CO<-c(4080)

Causes<-c("Infectious","Neoplasms","CVD",
          "Not classified","Mental","Nervous",
          "Diabetes","Digestive",
          "Genitourinary","Congenital","Respiratory",
          "External")
## causes of death + 1 (since the first place is for the total deaths)
## it is done one by one

CONTIf<-c()
CONTIm<-c()

Af<-read.table("C:/Users/Tammy/Documents/During KOSTAT summer seminar/Data/LYL/fltper_1x1.txt", header=TRUE, fill=TRUE, skip=1)                                        ## fix this
Am<-read.table("C:/Users/Tammy/Documents/During KOSTAT summer seminar/Data/LYL/mltper_1x1.txt", header=TRUE, fill=TRUE, skip=1)
  
Af<-Af[Af$Year==Year,]
Am<-Am[Am$Year==Year,]

for (CC in 2:13){
  B1C<-t(read.csv(paste("C:/Users/Tammy/Documents/During KOSTAT summer seminar/Data/LYL/","Country", CO[1],"Cause", CC, "f",".txt",sep=""),
                  header=TRUE, fill=TRUE, skip=0)[,-1])                                        ## fix this
    
    B1Cm<-t(read.csv(paste("C:/Users/Tammy/Documents/During KOSTAT summer seminar/Data/LYL/","Country",CO[1],"Cause",
                           CC,"m",".txt",sep=""),
                     header=TRUE, fill=TRUE, skip=0)[,-1])                                       ## fix this
    
    
    CONTIm<-rbind(CONTIm,B1Cm[B1Cm[,1]==Year,])
  CONTIf<-rbind(CONTIf,B1C[B1C[,1]==Year,])
  
}

CD <- c()
A1 <- c()

if (Sex==1){(A1<-Am)}
if (Sex==1){(sex<-c("m"))}
if (Sex==1){(CD<-CONTIm)}
if (Sex==2){(A1<-Af)}
if (Sex==2){(sex<-c("f"))}
if (Sex==2){(CD<-CONTIf)}

LT<-A1[c(AGEF:AGEL)+1,]

WW<-CD[,c(AGEF:AGEL)+2]

Age   <- c(AGEF:AGEL)




#### Here you can change the number of causes of death from 
####  3 to 11 
S<-c(1:5)                                        ## fix this
NicePlot2(LT,WW,S)


















########## Variable-r
Cnty <- "SWE"

Year <- 2020


Age <- 100

library(data.table)

decomp_data <- function(Cnty,Year,Age){
  
  country <- Cnty
  year <- Year
  age <- Age
  
  # First step is read data. We will need birth, mortality,
  # and Population data from HMD. We will be using lexis deaths
  # since we want to get a more accurate cohort mortality estimate.
  
  # Population
  Pop <- fread(paste0("C:/Users/Tammy/Documents/During KOSTAT summer seminar/",country,".Population.txt"),header=T,skip=1)
  colnames(Pop) <- c("Year","Age","Female","Male","Total")
  Pop$Age[Pop$Age=="110+"]<-110
  Pop$Age <- as.numeric(Pop$Age)
  Pop$Year = gsub("[-]","", Pop$Year)
  Pop$Year <- as.numeric(Pop$Year)
  
  # Birth
  Birth <- fread(paste0("C:/Users/Tammy/Documents/During KOSTAT summer seminar/",country,".birth.txt"),header=T,skip=1)                                         ## fix this
  Pop$Year <- as.numeric(Pop$Year)
  
  # Death
  Death <-fread(paste0("C:/Users/Tammy/Documents/During KOSTAT summer seminar/",country,".Deaths_lexis.txt"), header=T, skip=1)                                         ## fix this
    
  Death$Cohort[Death$Cohort=="."]<-0
  Death$Cohort <- as.numeric(Death$Cohort)
  Death$Age[Death$Age=="110+"]<-110
  Death$Age <- as.numeric(Death$Age)
  
  # specify the vectors needed.
  
  sex_ratio <- rep(0,age+1)                                        ## fix this
    
    
    # For female and male population along
  P_male <- rep(0,age+1)
  P_female <- rep(0,age+1)
  B_male <- rep(0,age+1)
  B_female <- rep(0,age+1)
  S_male <- rep(0,age+1)
  S_female <- rep(0,age+1)
  M_male <- rep(0,age+1)
  M_female <- rep(0,age+1)
  
  # for the ratio
  rB <- rep(0,age+1)                                        ## fix this
  rS <- rep(0,age+1)                                        ## fix this
  rM <- rep(0,age+1)                                        ## fix this
    
    
    # We are calculating the cohort survival per
    # HMD protocol which is the  parallelogram 
    # but first age 0 there is only a triangle.
    
  P_male[1] <- Pop[Year==paste0(year)&Age==0,Male]
  P_female[1] <- Pop[Year==paste0(year)&Age==0,Female]
  
  sex_ratio[1] <- P_male[1]/P_female[1]                                        ## fix this
    
    
    #### Birth: here
    
  B_male[1] <- Birth[Year==year-1,Male]
  B_female[1] <- Birth[Year==year-1,Female]
  
  rB[1] <- B_male[1]/B_female[1]                                         ## fix this
    
    
    #### Survival
    
    # Survival is calculated as the probability
    # of dying for people exposed.
    
    ### male survival
    qx_male <- Death[Cohort==year-1&
                       Age==0&
                       Year==year-1,][,c(1:3,5)]                                         ## fix this
    
    qx_male <- qx_male[,sum(Male),by=.(Age,Cohort)]
  # This is essentially the group_by and summarize function
  # combined
  names(qx_male)[3] <- "D"
  
  qx_male$P <- 
    Pop[Year==year&Age==0,Male]+qx_male$D
  
  qx_male$Mx <- qx_male$D/qx_male$P                                        ## fix this
    
    
    S_male[1] <- (1-qx_male$Mx)                                         ## fix this
    
    
    ### female survival
    qx_female <- Death[Cohort==year-1&
                         Age==0&
                         Year==year-1,][,c(1:3,4)]                                        ## fix this
    
    qx_female <- qx_female[,sum(Female),by=.(Age,Cohort)]
  names(qx_female)[3] <- "D"
  
  qx_female$P <- 
    Pop[Year==year&Age==0,Female]+qx_female$D
  
  qx_female$Mx <- qx_female$D/qx_female$P                                         ## fix this
    
    
    S_female[1] <- (1-qx_female$Mx)                                         ## fix this
    
    
    ### Survival ratio
    rS[1] <- S_male[1]/S_female[1]                                        ## fix this
    
    
    #### Net-migration
    M_male[1] <- P_male[1]/(B_male[1]*S_male[1])                                        ## fix this
    M_female[1] <- P_female[1]/(B_female[1]*S_female[1])                                        ## fix this
    
    
    rM[1] <- M_male[1]/M_female[1]                                         ## fix this
    
    
    for (a in 1:age) {
      
      #### Sex ratio
      
      P_male[a+1] <- Pop[Year==paste0(year)&Age==a,Male]
      P_female[a+1] <- Pop[Year==paste0(year)&Age==a,Female]
      
      sex_ratio[a+1] <- P_male[a+1]/P_female[a+1]                              ## fix this
        
        
        #### Birth
        
      B_male[a+1] <- Birth[Year==year-a-1,Male]
      B_female[a+1] <- Birth[Year==year-a-1,Female]
      
      rB[a+1] <- B_male[a+1]/B_female[a+1]                                        ## fix this
        
        
        #### Survival
        
        ### male survival
      qx_male <- Death[Cohort==year-a-1&Age<=a,][,c(1:3,5)]
      qx_male <- qx_male[,sum(Male),by=.(Age,Cohort)]
      names(qx_male)[3] <- "D"
      
      for (c in (a-1):0) {
        qx_male$P[which(qx_male$Age==a-c-1)] <- 
          Pop[Year==year-c-1&Age==a-c-1,Male]+
          Death[Year==year-c-2&Age==a-c-1,Male][1]
      }
      
      # The last age group is also a triangle so 
      # we calculate them separately. 
      
      last_qx_male <- 
        Death[Cohort==year-a-1&
                Age==a&
                Year==year-1,][,c(1:3,5)]
      
      names(last_qx_male)[4] <- "D"
      
      last_qx_male$P <- 
        Pop[Year==year&Age==a,Male]+last_qx_male$D
      
      last_qx_male$Mx <- last_qx_male$D/last_qx_male$P                                   ## fix this
        
        
        qx_male$Mx <- qx_male$D/(qx_male$P)                                         ## fix this
        
        
        S_male[a+1] <- 
        tail(cumprod(1-qx_male$Mx),1)*(1-last_qx_male$Mx)
      
      ### female survival
      qx_female <- Death[Cohort==year-a-1&Age<a,][,c(1:3,4)]
      qx_female <- qx_female[,sum(Female),by=.(Age,Cohort)]
      names(qx_female)[3] <- "D"
      
      for (c in (a-1):0) {
        qx_female$P[which(qx_female$Age==a-c-1)] <- 
          Pop[Year==year-c-1&Age==a-c-1,Female]+
          Death[Year==year-c-2&Age==a-c-1,Female][1]
      }
      
      last_qx_female <- 
        Death[Cohort==year-a-1&
                Age==a&
                Year==year-1,][,c(1:3,4)]
      
      names(last_qx_female)[4] <- "D"
      
      last_qx_female$P <- 
        Pop[Year==year&Age==a,Female]+last_qx_female$D
      
      last_qx_female$Mx <- last_qx_female$D/last_qx_female$P
      
      qx_female$Mx <- qx_female$D/(qx_female$P)                                         ## fix this
        
        
        S_female[a+1] <- 
          tail(cumprod(1-qx_female$Mx),1)*(1-last_qx_female$Mx)## fix this
        
        
        ### Survival ratio
        rS[a+1] <- S_male[a+1]/S_female[a+1]
      
      #### Net-migration
      M_male[a+1] <- P_male[a+1]/ 
        (B_male[a+1]*S_male[a+1])
      
      M_female[a+1] <- P_female[a+1]/ 
        (B_female[a+1]*S_female[a+1])
      
      # rM[a+1] <- sex_ratio[a+1]/
      #   (rS[a+1]*rB[a+1])
      
      rM[a+1] <- M_male[a+1]/
        M_female[a+1]
      
    }
  
  table <- data.table(
    Age = 0:age,
    sex_ratio = sex_ratio,
    P_male = P_male, P_female = P_female,
    rB = rB, rS = rS, rM = rM,
    B_male = B_male, B_female = B_female,
    S_male = S_male, S_female = S_female,
    M_male = M_male, M_female = M_female
  )
  
  table <- table[,`:=`(Cx_f = P_female/sum(P_female),
                       Cx_m = P_male/sum(P_male))]
  
  return(table)
  
}

Data <- decomp_data(Cnty,Year,Age)

ggplot(Data,mapping=aes(x=Age))+
  geom_line(mapping=aes(y=sex_ratio,color="Sex Ratios"), linewidth=1, alpha=0.8)+
  geom_line(mapping=aes(y=rM,color="Net-Migration Ratios"),
            linewidth=1, alpha=0.8)+
  geom_line(mapping=aes(y=rS,color="Survival Ratios"),
            linewidth=1,alpha=0.8)+
  geom_line(mapping=aes(y=rB, color="Birth Ratios"),
            linewidth=1, alpha=0.8)+
  geom_hline(yintercept = 1,linetype=2)+
  scale_color_manual(values = c("skyblue","forestgreen",
                                "gold","maroon"))+
  scale_x_continuous(n.breaks=5,limits = c(0,100))+
  scale_y_continuous(n.breaks=10,limits = c(0,1.5))+
  guides(color=guide_legend(title="",
                            nrow = 2,byrow = T))+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(title = "Sex ratio decompostion, Sweden, 2020",
       x="Age",y="Ratios")



