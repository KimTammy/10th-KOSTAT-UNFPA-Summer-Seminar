
library(ggplot2)
library(knitr)
library(colorRamps)
library(RColorBrewer)
library(tidyverse)
library(data.table)
library(reshape2)
options(scipen = 100000,digits=4)
library(readxl)

### Life expectancy decomposition

# This is a very simple life table function
# The input values are
# 1. a vector of single age-specific death rates
#    with a length of 111 (age 0 to age 110+).
# 2. an indicator for the sex of either Male ("m") or        
#    Female ("f") you are computing. 

life.table<-function(mx,sex){
  
  N<-length(mx)
  
  ax<-rep(0.5,N) 
  # We assume the people who died during the age
  # interval lived half of the year. A very common
  # assumption. 
  
  if(sex=="m"){
    ax[1]<-ifelse(mx[1]<0.107,0.045+mx[1]*2.684,0.330)}
  if(sex=="f"){
    ax[1]<-ifelse(mx[1]<0.107,0.053+2.800*mx[1],0.350)
  }
  # a "ifelse" function to compute the ax for the infant
  # age since the distribution of death for infants are
  # very different from adult years 
  
  qx<- mx/(1+(1-ax)*mx)                                        ## fix this
  
  # Chiang's conversion from age-specific mortality
  # to age-specific probability of death
  
  qx[N] <- 1                                        ## fix this
  
  
  # Everyone dies in a life table eventually
  # with the last value of qx = 1
  
  px<-1-qx
  
  lx<-100000                                       ## fix this
  for(y in 1:(N-1)){
    lx[y+1]<-lx[y]*px[y]
  }
  lx <- ifelse(lx<0,0,lx)
  
  dx<-lx*qx                                       ## fix this
  
  # Calculating the death distribution of the 
  # life table
  
  Lx<-lx[-1]+ax[-N]*dx[-N] 
  Lx[N]<-ifelse(mx[N]>0,lx[N]/mx[N],0)                  
  # Person-year lived within each age interval.
  
  Tx<-c() 
  for(y in 1:N){
    Tx[y]<-sum(Lx[y:N]) 
  }
  
  ex<-Tx/lx                                       ## fix this
  
  # Calculate life expectancy at each age
  
  Age<-0:106              
  ALL<-data.frame(Age,mx,lx,dx,Lx,Tx,ex)
  return(ALL)
}

# The Arriaga decomposition function 
# The input consists of two sets of vector containing
# age-specific mortality rates. 
# At the same time, an indicator of sex of 
# either Male ("m") or Female ("f")
# needs to be specified. 
# A separate indicator of whether the results comes in
# contributions by age (breakdown = F) 
# or contributions by age and direct & indirect+interaction effect 
# (breakdown = T).

arriaga <- function(nmx1,nmx2,sex,breakdown=F){
  
  LT1 <- life.table(nmx1,sex)
  LT2 <- life.table(nmx2,sex)
  
  # Creating the two life tables
  
  lx1 <- LT1$lx
  lx2 <- LT2$lx
  
  Lx1 <- LT1$Lx
  Lx2 <- LT2$Lx
  
  Tx1 <- LT1$Tx
  Tx2 <- LT2$Tx
  
  # Specifying the life table statistics we 
  # need to perform the Arriaga decomposition.
  
  if(breakdown==FALSE){
    delta <- rep(0,107)
    for (i in 1:106){
      delta[i]    <- 
        (lx1[i]/lx1[1])*(Lx2[i]/lx2[i]-Lx1[i]/lx1[i])+
        (Tx2[i+1]/lx1[1])*(lx1[i]/lx2[i]-lx1[i+1]/lx2[i+1])
      delta[111]  <-
        (lx1[111]/lx1[1])*(Tx2[111]/lx2[111]-Tx1[111]/lx1[111])
    }
  }
  
  if(breakdown==T){
    direct <- rep(0,107)
    indirect <- rep(0,107)
    for (i in 1:106){
      direct[i] <- 
        (lx1[i]/lx1[1])*(Lx2[i]/lx2[i]-Lx1[i]/lx1[i])
      direct[111] <-
        (lx1[111]/lx1[1])*(Tx2[111]/lx2[111]-Tx1[111]/lx1[111])
      indirect[i] <-
        (Tx2[i+1]/lx1[1])*(lx1[i]/lx2[i]-lx1[i+1]/lx2[i+1])
    }
    delta <- data.frame(
      age=0:106,
      direct = direct,
      indirect = indirect
    )
  }
  
  # depends on whether you want to separate the direct and indirect
  # effect, the "delta" results can either be a vector of contributions
  # or a data.frame containing the direct and indirect components of 
  # the decomposition.
  return(delta)
}

USA_Mx <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_usa.txt", header = T, skip = 2 ) 
CAN_Mx <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_canada.txt", header = T, skip = 2 ) 

CAN_Mx[CAN_Mx=="."] <- "0"

str(CAN_Mx)
str(USA_Mx)

USA_Mx$Female <- as.numeric(USA_Mx$Female)
USA_Mx$Male <- as.numeric(USA_Mx$Male)
USA_Mx$Total <- as.numeric(USA_Mx$Total)
USA_Mx$Age <- as.numeric(USA_Mx$Age)

str(USA_Mx)

CAN_Mx$Female <- as.numeric(CAN_Mx$Female)
CAN_Mx$Male <- as.numeric(CAN_Mx$Male)
CAN_Mx$Total <- as.numeric(CAN_Mx$Total)
CAN_Mx$Age <- as.numeric(CAN_Mx$Age)

str(CAN_Mx)

ex_diff <- 
  life.table(CAN_Mx[CAN_Mx$Year==2020, "Male"],"m")$ex[1]-
  life.table(USA_Mx[USA_Mx$Year==2020, "Male"],"m")$ex[1]


ex_decomp <- 
  arriaga(as.numeric(USA_Mx[USA_Mx$Year==2020,4]),
          as.numeric(KOR_Mx[KOR_Mx$Year==2020,4]),
          sex = "m",
          breakdown = F)

table <-
  matrix(round(
    c(life.table(CAN_Mx[CAN_Mx$Year==2020,4], "m")$ex[1],
      life.table(USA_Mx[USA_Mx$Year==2020,4], "m")$ex[1],
      ex_diff,
      sum(ex_decomp)),
    1),ncol=1)## fix this


row.names(table) <- 
  c("Life expectancy at birth for Canada",
    "Life expectancy at birth for USA",
    "Life expectancy difference betwen Canada and USA",
    "Estimated difference from decompositon")

colnames(table) <- "Arriaga"

kable(table,caption = "male life expectancy gap, Canada - USA, 2020 ")





ggplot()+
  geom_col(aes(x=0:110,y=ex_decomp), fill="blue"                                       ## fix this 
  )+
  scale_x_continuous(breaks = seq(0,110,10))+
  theme_minimal()+
  labs(title = "Age-decomposition of the difference in female life expectancy \n between canada and United States, 2020",
       x="Age",y="Contributions")







