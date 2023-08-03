
library(ggplot2)
library(knitr)
library(colorRamps)
library(RColorBrewer)
library(tidyverse)
library(data.table)
library(reshape2)
options(scipen = 100000,digits=4)


#### CDR Kitagawa 

### Korea 

Mx_KOR <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_korea.txt", header = T, skip = 2 ) 
  
  Mx_KOR <- Mx_KOR[Mx_KOR$Year==2020,]

Mx_KOR$Age[Mx_KOR$Age=="110+"] <- "110"
Mx_KOR$Age <- as.numeric(Mx_KOR$Age)

Mx_KOR$Total <- as.numeric(Mx_KOR$Total)

Mx_KOR$Male <- Mx_KOR$Female <- NULL

Pop_KOR <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header = T, skip = 2 )
  
  Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Year1 <- 2020
Year2 <- 2021

Pop_KOR1 <- Pop_KOR[Pop_KOR$Year==Year1,] #population at 2020
Pop_KOR2 <- Pop_KOR[Pop_KOR$Year==Year2,] #population at 2021

Pop_KOR1$Total <-  Pop_KOR1$Total/sum(Pop_KOR1$Total) 

Pop_KOR2$Total <-  Pop_KOR2$Total/sum(Pop_KOR2$Total)

Exposure_KOR <- (Pop_KOR1$Total+Pop_KOR2$Total)/2

### Japan

Mx_JPN <-  read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_japan.txt", header = T, skip = 2 ) 
  
  Mx_JPN <- Mx_JPN[Mx_JPN$Year==2020,]

Mx_JPN$Age[Mx_JPN$Age=="110+"] <- "110"
Mx_JPN$Age <- as.numeric(Mx_JPN$Age)

Mx_JPN$Total <- as.numeric(Mx_JPN$Total)

Mx_JPN$Male <- Mx_JPN$Female <- NULL

Pop_JPN <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_japan.txt", header = T, skip = 2 )
  
  Pop_JPN$Age[Pop_JPN$Age=="110+"] <- "110"
Pop_JPN$Age <- as.numeric(Pop_JPN$Age)

Year1 <- 2020
Year2 <- 2021

Pop_JPN1 <- Pop_JPN[Pop_JPN$Year==Year1,] #japan 2020 population
Pop_JPN2 <- Pop_JPN[Pop_JPN$Year==Year2,] #japan 2021 population

Pop_JPN1$Total <-   Pop_JPN1$Total/sum(Pop_JPN1$Total) 

Pop_JPN2$Total <-   Pop_JPN2$Total/sum(Pop_JPN2$Total) 

Exposure_JPN <- (Pop_JPN1$Total+Pop_JPN2$Total)/2

CDR_JPN <- sum(Mx_JPN$Total*Exposure_JPN )*1000

CDR_KOR <- sum(Mx_KOR$Total*Exposure_KOR )*1000

CDR_diff <- CDR_JPN-CDR_KOR

CDR_direct <- sum((Mx_JPN$Total-Mx_KOR$Total) *  (Exposure_JPN+Exposure_KOR)/2)*1000

CDR_comp <- sum((Exposure_JPN  - Exposure_KOR) * (Mx_JPN$Total+Mx_KOR$Total  )/2)*1000

CDR_decomp <- CDR_direct+CDR_comp




table <-
  matrix(round(c(CDR_JPN,CDR_KOR,
                 CDR_diff,CDR_direct,
                 CDR_comp,CDR_decomp),2),
         ncol=1)

colnames(table) <- ""

row.names(table) <- c(paste0("CDR in Japan"),
                      paste0("CDR in Korea"),
                      "Difference",
                      "Direct component",
                      "Indirect component",
                      "Total estimated difference")

kable(table,caption = "CDR decomposition (Kitagawa)")






## kitagawa difference in growth rates

Year1 = 2010

Year2 = 2020

table_t1 <-  read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_japan.txt", header = T, skip = 2 )
  
  table_t1 <- table_t1[table_t1$Year==Year1,c("Age","Female","Male")]

table_t1$Cx_f <- table_t1$Female/sum(table_t1$Female)

table_t1$Cx_m <- table_t1$Male/sum(table_t1$Male)

table_t2 <-  read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_japan.txt", header = T, skip = 2 )
  
  table_t2 <- table_t2[table_t2$Year==Year2,c("Age","Female","Male")]

table_t2$Cx_f <- table_t2$Female/sum(table_t2$Female)

table_t2$Cx_m <- table_t2$Male/sum(table_t2$Male)

r_Pm <- log(table_t2$Male/table_t1$Male)/10

r_Pm <- ifelse(is.infinite(r_Pm),0,r_Pm)

r_Pf <- log(table_t2$Female/table_t1$Female)/10

r_Pf <- ifelse(is.infinite(r_Pf),0,r_Pf)

Cx_m <- (table_t1$Cx_m+table_t2$Cx_m)/2

Cx_f <- (table_t1$Cx_f+table_t2$Cx_f)/2

rm <- sum(ifelse(is.infinite(r_Pm),0,r_Pm)*Cx_m,na.rm = T)
rf <- sum(ifelse(is.infinite(r_Pf),0,r_Pf)*Cx_f,na.rm = T)

r_diff <- rm-rf

r_direct <- sum((r_Pm-r_Pf)*(Cx_m+Cx_f)/2)

r_comp <- sum((Cx_m-Cx_f)*(r_Pm+r_Pf)/2)

r_decomp <-
  sum((r_Pm-r_Pf)*(Cx_m+Cx_f)/2+
        (Cx_m-Cx_f)*(r_Pm+r_Pf)/2,
      na.rm = T)




table <- 
  matrix(round(c(rm*100,rf*100,r_diff*100,
                 r_direct*100,r_comp*100,r_decomp*100),2),
         ncol=1)

row.names(table) <- c(paste0("Growth of Male Population ",
                             Year2,"-",Year1),
                      paste0("Growth of Female Population ",
                             Year2,"-",Year1),
                      "Observed difference",
                      "Direct Component",
                      "Indirect Component",
                      "Estimated Difference")

colnames(table) <- "Japan"

kable(table, caption = "Growth Difference between Male and Female, Japan")




## age-decomposition

table <- 
  data.frame(
    age = rep(0:110,2),
    variable = 
      rep(c("growth component",
            "composition component"),
          each = 111),
    value = 
      c((r_Pm-r_Pf)*(Cx_m+Cx_f)/2*100,
        (Cx_m-Cx_f)*(r_Pm+r_Pf)/2*100)
  )

ggplot(data = table,aes(x=age,y=value,fill=variable))+
  geom_col()+
  scale_fill_manual(values = c("blue","red"))+
  labs(title = "Male-Female growth difference contributions, Japan",
       x="Age",y="Contribution",fill="Component",
       caption = "Note: results are multiplied by 100")+
  theme_classic()




# We have 0 on the denominator

log(1/0)

# or we have NA on the denominator

log(1/NA)





log_func <- function(v2,v1,time){
  v1 <- ifelse(v1==0|is.na(v1),1,v1)
  div <- ifelse(v2/v1==0|is.na(v2/v1),1,v2/v1)
  result <- log(div)/time
  return(result)
}





log_func(1,0,1)





Year1 <- 2010

Year2 <- 2020

KOR_pop <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header = T, skip = 2 )

KOR_pop1 <- 
  (KOR_pop[KOR_pop$Year==Year1,"Female"]*
     KOR_pop[KOR_pop$Year==Year1+1,"Female"])^0.5

KOR_pop2 <- 
  (KOR_pop[KOR_pop$Year==Year2,"Female"]*
     KOR_pop[KOR_pop$Year==Year2+1,"Female"])^0.5

KOR_Mx <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_korea.txt", header = T, skip = 2 )

KOR_Mx1 <- KOR_Mx[KOR_Mx$Year==Year1,"Female"]

KOR_Mx2 <- KOR_Mx[KOR_Mx$Year==Year2,"Female"]

KOR_comp1 <- (KOR_pop1)/sum(KOR_pop1)

KOR_comp2 <- (KOR_pop2)/sum(KOR_pop2)

KOR_comp <- 
  (KOR_pop1*KOR_pop2)^0.5/
  sum((KOR_pop1*KOR_pop2)^0.5)

CDR_Y1 <- sum(KOR_Mx1*KOR_comp1)*1000

CDR_Y2 <- sum(KOR_Mx2*KOR_comp2)*1000

CDR_diff <- 
  log(CDR_Y2/CDR_Y1)/(Year2-Year1)*
  (CDR_Y1*CDR_Y2)^0.5

CDR_direct <- 
  sum(log_func(KOR_Mx2,KOR_Mx1,Year2-Year1)*
        (KOR_Mx2*KOR_Mx1)^0.5*KOR_comp,na.rm = T)*1000

CDR_comp <- 
  (sum((KOR_Mx2*KOR_Mx1)^0.5*
         log_func(KOR_pop2,KOR_pop1,
                  Year2-Year1)*KOR_comp)-
     sum((KOR_Mx2*KOR_Mx1)^0.5*KOR_comp)*
     sum(log_func(KOR_pop2,KOR_pop1,
                  Year2-Year1)*KOR_comp))*1000

CDR_decomp <- (CDR_direct+CDR_comp)





table <-
  matrix(round(c(CDR_Y1,CDR_Y2,
                 CDR_diff,CDR_direct,
                 CDR_comp,CDR_decomp),2),
         ncol=1)

colnames(table) <- "Korea"

row.names(table) <- c(paste0("CDR in ",Year1),
                      paste0("CDR in ",Year2),
                      "CDR annualized difference",
                      "Direct component",
                      "Compositional component",
                      "Total estimated difference")

kable(table,caption = "CDR decomposition (VCR)")




ggplot()+
  geom_line(aes(x=0:110,
                y=log10((KOR_Mx2*KOR_Mx1)^0.5),
                color = "Mortality"))+
  geom_line(aes(x=0:110,
                y=log_func(KOR_pop2,KOR_pop1,Year2-Year1)*20,
                color = "Population Growth"))+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(-5,3),
                     sec.axis = sec_axis(~./20,
                                         name = "Growth Rate"))+
  scale_color_manual(values = c("blue","red"))+
  labs(x="Age",y="Logged Mortality Rate",color="Component",
       caption = "Mortality is not in log scale")+
  theme_classic()

# Please notice the second axis of the figure, 
# in this case we are tricking the ggplot 
# system to fit two lines from different scale 
# into one plot region. We will need to change 
# the scale of one line by multiplying a factor 
# or adding or subtracting some numbers.



##general fertility rate

Year1 <- 2010
Year2 <- 2020

ASFR_KOR <-read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/asfr_korea.txt", header = T, skip = 2 )

ASFR_KOR$Age[ASFR_KOR$Age=="12-"] <- "12"
ASFR_KOR$Age[ASFR_KOR$Age=="55+"] <- "55"
ASFR_KOR$Age <- as.numeric(ASFR_KOR$Age)

ASFR_KOR1 <- ASFR_KOR[ASFR_KOR$Year==Year1,"ASFR"]
ASFR_KOR2 <- ASFR_KOR[ASFR_KOR$Year==Year2,"ASFR"]

Pop_KOR <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header = T, skip = 2 )

Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Pop_KOR <- Pop_KOR[Pop_KOR$Age%in%c(12:55),]

Pop_KOR1 <- 
  (Pop_KOR[Pop_KOR$Year==Year1,"Female"]*
     Pop_KOR[Pop_KOR$Year==Year1+1,"Female"])^0.5

Pop_KOR2 <- 
  (Pop_KOR[Pop_KOR$Year==Year2,"Female"]*
     Pop_KOR[Pop_KOR$Year==Year2+1,"Female"])^0.5

Comp_KOR1 <- Pop_KOR1/sum(Pop_KOR1)

Comp_KOR2 <- Pop_KOR2/sum(Pop_KOR2)

Comp_avg <- 
  (Pop_KOR1*Pop_KOR2)^0.5/
  sum((Pop_KOR1*Pop_KOR2)^0.5)

GFR_Y1 <- sum(ASFR_KOR1*Comp_KOR1)*1000

GFR_Y2 <- sum(ASFR_KOR2*Comp_KOR2)*1000

GFR_diff <- 
  log(GFR_Y2/GFR_Y1)/(Year2-Year1)*
  (GFR_Y1*GFR_Y2)^0.5

GFR_direct <- 
  sum(log_func(ASFR_KOR2,ASFR_KOR1,Year2-Year1)*
        (ASFR_KOR2*ASFR_KOR1)^0.5*
        Comp_avg,na.rm = T)*1000

GFR_comp <- 
  (sum((ASFR_KOR2*ASFR_KOR1)^0.5*
         log_func(Pop_KOR2,Pop_KOR1,Year2-Year1)*Comp_avg)-
     sum((ASFR_KOR2*ASFR_KOR1)^0.5*Comp_avg)*
     sum(log_func(Pop_KOR2,Pop_KOR1,Year2-Year1)*Comp_avg))*1000

GFR_decomp <- (GFR_direct+GFR_comp)



table <-
  matrix(round(c(GFR_Y1,GFR_Y2,
                 GFR_diff,GFR_direct,
                 GFR_comp,GFR_decomp),2),
         ncol=1)

colnames(table) <- "Korea"

row.names(table) <- c(paste0("GFR in ",Year1),
                      paste0("GFR in ",Year2),
                      "GFR annualized difference",
                      "Direct component",
                      "Compositional component",
                      "Total estimated difference")

kable(table,caption = "GFR decomposition (VCR)")




ggplot()+
  geom_line(aes(x=12:55,
                y=(ASFR_KOR1*ASFR_KOR2)^0.5,
                color = "Fertility"))+
  geom_line(aes(x=12:55,
                y=log_func(Pop_KOR2,Pop_KOR1,Year2-Year1),
                color = "Population Growth"))+
  scale_x_continuous(limits = c(12,55))+
  scale_y_continuous(limits = c(-0.1,0.2))+
  scale_color_manual(values = c("blue","red"))+
  labs(x="Age",y="Rate",color="Component")+
  theme_classic()



##Mean Age of the population

Year <- 2020

KOR_pop <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header = T, skip = 2 )

KOR_pop <- KOR_pop[KOR_pop$Year==Year,"Female"]

JPN_pop <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_japan.txt", header = T, skip = 2 )

JPN_pop <- JPN_pop[JPN_pop$Year==Year,"Female"]

MAP_KOR <- sum(0:110*KOR_pop)/sum(KOR_pop)

MAP_JPN <- sum(0:110*JPN_pop)/sum(JPN_pop)

MAP_diff <- log(MAP_JPN/MAP_KOR)*(MAP_JPN*MAP_KOR)^0.5

comp_pop <- (JPN_pop*KOR_pop)^0.5/sum((JPN_pop*KOR_pop)^0.5)

MAP_cov <- 
  sum(0:110*log_func(JPN_pop,KOR_pop,1)*comp_pop)-
  sum(0:110*comp_pop)*
  sum(log_func(JPN_pop,KOR_pop,1)*comp_pop)





table <- matrix(round(c(MAP_KOR,MAP_JPN,
                        MAP_diff,
                        MAP_cov),2),ncol = 1)

row.names(table) <- c("MAP for Korea",
                      "MAP for Japan",
                      "Difference in MAP",
                      "Estimated Difference")

colnames(table) <- "Korea"

kable(table,caption = "MAP decomposition (VCR)")




ggplot()+
  geom_line(aes(x=0:110,
                y=log_func(JPN_pop,KOR_pop,1),
                color = "Population Growth"))+
  geom_line(aes(x=0:110,
                y=(0:110)/25,
                color = "Age"))+
  scale_y_continuous(sec.axis = 
                       sec_axis(trans = ~.*25,
                                breaks=seq(0,110,10),
                                name = "Age"))+
  scale_color_manual(values = c("blue","red"))+
  labs(x= "Age", y = "Population Growth Factor")+
  theme_bw()


## Mean age at childbearing (Korea)


Year1 <- 2010

Year2 <- 2020

KOR_fx <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/asfr_korea.txt", header = T, skip = 2 )
KOR_fx1 <- KOR_fx[KOR_fx$Year==Year1,"ASFR"]
KOR_fx2 <- KOR_fx[KOR_fx$Year==Year2,"ASFR"]

MAB_Y1 <- sum(12.5:55.5*KOR_fx1)/sum(KOR_fx1)

MAB_Y2 <- sum(12.5:55.5*KOR_fx2)/sum(KOR_fx2)

MAB_diff <- 
  (log(MAB_Y2/MAB_Y1)/(Year2-Year1))*
  (MAB_Y2*MAB_Y1)^0.5

comp <- (KOR_fx2*KOR_fx1)^0.5/
  sum((KOR_fx2*KOR_fx1)^0.5)

MAB_decomp <-
  sum(12.5:55.5*log_func(KOR_fx2,KOR_fx1,
                         Year2-Year1)*comp)-
  sum(12.5:55.5*comp)*
  sum(log_func(KOR_fx2,KOR_fx1,
               Year2-Year1)*comp)




table <- matrix(round(c(MAB_Y1,MAB_Y2,
                        MAB_diff,
                        MAB_decomp),2),ncol = 1)

row.names(table) <- c(paste0("MAB for Korea ",Year1),
                      paste0("MAB for Korea ",Year2),
                      "Difference in MAB",
                      "Estimated Difference")

colnames(table) <- "Korea"

kable(table,caption = "MAB decomposition (VCR)")




ggplot()+
  geom_line(aes(x=12:55,
                y=log_func(KOR_fx2,KOR_fx1,
                           Year2-Year1),
                color = "Relative Change in Age-specific Fertility"))+
  geom_line(aes(x=12:55,
                y=(12:55)/100-0.4,
                color = "Age"))+
  scale_y_continuous(sec.axis = 
                       sec_axis(trans = ~(.+0.4)*100,
                                breaks=seq(0,110,10),
                                name = "Age"))+
  scale_color_manual(values = c("blue","red"))+
  labs(x= "Age", y = "Rate of Change")+
  theme_bw()





Year1 <- 2010

Year2 <- 2020

KOR_pop <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header = T, skip = 2 )
KOR_fpop1 <- 
  (KOR_pop[KOR_pop$Year==Year1,"Female"]*
     KOR_pop[KOR_pop$Year==Year1+1,"Female"])^0.5

KOR_fpop2 <- 
  (KOR_pop[KOR_pop$Year==Year2,"Female"]*
     KOR_pop[KOR_pop$Year==Year2+1,"Female"])^0.5

KOR_totpop2 <- 
  (KOR_pop[KOR_pop$Year==Year2,"Total"]*
     KOR_pop[KOR_pop$Year==Year2+1,"Total"])^0.5

KOR_totpop1 <- 
  (KOR_pop[KOR_pop$Year==Year1,"Total"]*
     KOR_pop[KOR_pop$Year==Year1+1,"Total"])^0.5

KOR_fx <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/asfr_korea.txt", header = T, skip = 2 )

KOR_fx1 <- c(rep(0,12),
             KOR_fx[KOR_fx$Year==Year1,"ASFR"],
             rep(0,55))

KOR_fx2 <- c(rep(0,12),
             KOR_fx[KOR_fx$Year==Year2,"ASFR"],
             rep(0,55))

KOR_comp1 <- (KOR_fpop1)/sum(KOR_totpop1)

KOR_comp2 <- (KOR_fpop2)/sum(KOR_totpop2)

KOR_comp <- 
  (KOR_fpop1*KOR_fpop2)^0.5/
  sum((KOR_totpop1*KOR_totpop2)^0.5)

CBR_Y1 <- sum(KOR_fx1*KOR_comp1)*1000

CBR_Y2 <- sum(KOR_fx2*KOR_comp2)*1000

CBR_diff <- 
  log(CBR_Y2/CBR_Y1)/(Year2-Year1)*
  (CBR_Y1*CBR_Y2)^0.5

CBR_direct <- 
  sum(log_func(KOR_fx2,KOR_fx1,Year2-Year1)*
        (KOR_fx2*KOR_fx1)^0.5*KOR_comp,na.rm = T)*1000

CBR_comp <- 
  (sum((KOR_fx2*KOR_fx1)^0.5*
         log_func(KOR_totpop2,KOR_totpop1,
                  Year2-Year1)*KOR_comp)-
     sum((KOR_fx2*KOR_fx1)^0.5*KOR_comp)*
     sum(log_func(KOR_totpop2,KOR_totpop1,
                  Year2-Year1)*KOR_comp))*1000

CBR_decomp <- (CBR_direct+CBR_comp)





table <-
  matrix(round(c(CBR_Y1,CBR_Y2,
                 CBR_diff,CBR_direct,
                 CBR_comp,CBR_decomp),2),
         ncol=1)

colnames(table) <- "Korea"

row.names(table) <- c(paste0("CBR in ",Year1),
                      paste0("CBR in ",Year2),
                      "CBR annualized difference",
                      "Direct component",
                      "Compositional component",
                      "Total estimated difference")

kable(table,caption = "CBR decomposition (VCR)")




ggplot()+
  geom_line(aes(x=0:110,
                y=(KOR_fx2*KOR_fx1)^0.5,
                color = "Fertility"))+
  geom_line(aes(x=0:110,
                y=log_func(KOR_fpop2,KOR_fpop1,Year2-Year1),
                color = "Population Growth"))+
  scale_x_continuous(limits = c(12,55))+
  scale_y_continuous(limits = c(-0.1,0.2))+
  scale_color_manual(values = c("blue","red"))+
  labs(x="Age",y="Rate",color="Component")+
  theme_classic()





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
  
  qx<-mx/(1+(1-ax)*mx)
  # Chiang's conversion from age-specific mortality
  # to age-specific probability of death
  
  qx[N] <- 1 
  # Everyone dies in a life table eventually
  # with the last value of qx = 1
  
  px<-1-qx
  lx<-100000
  for(y in 1:(N-1)){
    lx[y+1]<-lx[y]*px[y]
  }
  lx <- ifelse(lx<0,0,lx)
  
  dx<-lx*qx
  # Calculating the death distribution of the 
  # life table
  
  Lx<-lx[-1]+ax[-N]*dx[-N] 
  Lx[N]<-ifelse(mx[N]>0,lx[N]/mx[N],0)                  
  # Person-year lived within each age interval.
  
  Tx<-c() 
  for(y in 1:N){
    Tx[y]<-sum(Lx[y:N]) 
  }
  
  ex<-Tx/lx 
  # Calculate life expectancy at each age
  
  Age<-0:110              
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
    delta <- rep(0,111)
    for (i in 1:110){
      delta[i]    <- 
        (lx1[i]/lx1[1])*(Lx2[i]/lx2[i]-Lx1[i]/lx1[i])+
        (Tx2[i+1]/lx1[1])*(lx1[i]/lx2[i]-lx1[i+1]/lx2[i+1])
      delta[111]  <-
        (lx1[111]/lx1[1])*(Tx2[111]/lx2[111]-Tx1[111]/lx1[111])
    }
  }
  
  if(breakdown==T){
    direct <- rep(0,111)
    indirect <- rep(0,111)
    for (i in 1:110){
      direct[i] <- 
        (lx1[i]/lx1[1])*(Lx2[i]/lx2[i]-Lx1[i]/lx1[i])
      direct[111] <-
        (lx1[111]/lx1[1])*(Tx2[111]/lx2[111]-Tx1[111]/lx1[111])
      indirect[i] <-
        (Tx2[i+1]/lx1[1])*(lx1[i]/lx2[i]-lx1[i+1]/lx2[i+1])
    }
    delta <- data.frame(
      age=0:110,
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



### Kitagawa + crude birth rate + korea #######

ASFR_KOR <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/asfr_korea.txt", header = T, skip = 2 )

ASFR_KOR$Age[ASFR_KOR$Age=="12-"]<- "12"
ASFR_KOR$Age[ASFR_KOR$Age=="55-"]<-"55"
ASFR_KOR$Age <- as.numeric(ASFR_KOR$Age)

Pop_KOR <-  read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header = T, skip = 2 )
Pop_KOR$Age[Pop_KOR$Age=="12-"]<-"12"
Pop_KOR$Age[Pop_KOR$Age=="55+"]<-"55"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Year1 <- 2010
Year2<- 2020

Pop_KOR1 <- Pop_KOR[Pop_KOR$Year==Year1,] #2010년
Pop_KOR2 <- Pop_KOR[Pop_KOR$Year==Year2,] #2020년

Pop_KOR1$Total <- Pop_KOR1$Total/sum(Pop_KOR1$Total)
Pop_KOR2$Total <- Pop_KOR2$Total/sum(Pop_KOR2$Total)

Exposure_KOr <- (Pop_KOR1$Total+Pop_KOR2$Total)/2

CBR_2010 <- sum()

##+++++++ day 03++++++++++++++++++++++++##


USA_Mx <-  read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_usa.txt", header = T, skip = 2 )

KOR_Mx <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_korea.txt", header = T, skip = 2 )

ex_diff <- 
  life.table(KOR_Mx[KOR_Mx$Year==2019,3],"f")$ex[1]-
  life.table(USA_Mx[USA_Mx$Year==2019,3],"f")$ex[1]

ex_decomp <- 
  arriaga(as.numeric(USA_Mx[USA_Mx$Year==2019,3]),
          as.numeric(KOR_Mx[KOR_Mx$Year==2019,3]),
          sex = "f",
          breakdown = F)

table <-
  matrix(round(
    c(life.table(KOR_Mx[KOR_Mx$Year==2019,3],"f")$ex[1],
      life.table(USA_Mx[USA_Mx$Year==2019,3],"f")$ex[1],
      ex_diff,
      sum(ex_decomp)),
    1),ncol = 1)

row.names(table) <- 
  c("Life expectancy at birth for Korea",
    "Life expectancy at birth for USA",
    "Life expectancy difference betwen Korea and USA",
    "Estimated difference from decompositon")

colnames(table) <- "Arriaga"

kable(table,caption = "Female life expectancy gap, Korea - USA, 2019 ")





ggplot()+
  geom_col(aes(x=0:110,y=ex_decomp),fill="blue")+
  scale_x_continuous(breaks = seq(0,110,10))+
  theme_minimal()+
  labs(title = "Age-decomposition of the difference in female life expectancy \n between Korea and United States, 2019",
       x="Age",y="Contributions")





USA_Mx <-read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_usa.txt", header = T, skip = 2 )

KOR_Mx <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_korea.txt", header = T, skip = 2 )

ex_diff <- 
  life.table(KOR_Mx[KOR_Mx$Year==2019,3],"f")$ex[1]-
  life.table(USA_Mx[USA_Mx$Year==2019,3],"f")$ex[1]

ex_decomp <- 
  arriaga(as.numeric(USA_Mx[USA_Mx$Year==2019,3]),
          as.numeric(KOR_Mx[KOR_Mx$Year==2019,3]),
          sex = "f",
          breakdown = T)

table <-
  matrix(round(
    c(life.table(KOR_Mx[KOR_Mx$Year==2019,3],"f")$ex[1],
      life.table(USA_Mx[USA_Mx$Year==2019,3],"f")$ex[1],
      ex_diff,
      sum(ex_decomp$direct),
      sum(ex_decomp$indirect),
      sum(ex_decomp$direct)+sum(ex_decomp$indirect)),
    1),ncol = 1)

row.names(table) <- 
  c("Life expectancy at birth for Korea",
    "Life expectancy at birth for USA",
    "Life expectancy difference betwen Korea and USA",
    "Direct component",
    "Indirect and interaction component",
    "Estimated difference from decompositon")

colnames(table) <- "Arriaga"

kable(table,caption = "Female life expectancy gap, Korea - USA, and its components 2019")





# in order to visualize the data with both direct and indirect
# components, we are going to turn this data.frame from wide to
# long format. We are using the functions from the package
# data.table.

ex_decomp <- melt.data.table(setDT(ex_decomp),
                             id.vars = "age",
                             measure.vars = c("direct","indirect"))

ggplot(ex_decomp,aes(x=age,y=value,fill=variable))+
  geom_col()+
  scale_x_continuous(breaks = seq(0,110,10))+
  theme_minimal()+
  labs(title = "Age-decomposition of the difference in female life expectancy \n between Korea and United States, 2019",
       x="Age",y="Contributions")





USA_Mx <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_usa.txt", header = T, skip = 2 )

Mx1 <- USA_Mx[USA_Mx$Year==2010,3]

Mx2 <- USA_Mx[USA_Mx$Year==2018,3]

ex_diff <- 
  life.table(Mx2,"f")$ex[1]-
  life.table(Mx1,"f")$ex[1]

ex_decomp <- 
  arriaga(as.numeric(Mx1),
          as.numeric(Mx2),
          sex = "f",
          breakdown = F)

cause_count <- read.csv("data/MortDecomp/USA_Cause16_counts.csv")

cause_count1 <- cause_count[cause_count$Year==2010&
                              cause_count$Sex==2,]

cause_count1 <- matrix(c(cause_count1$Count),
                       ncol = length(unique(cause_count1$Cause)),
                       nrow = length(unique(cause_count1$Age)))

row.names(cause_count1) <- 0:110

colnames(cause_count1) <- 1:16

cause_total1 <- rowSums(cause_count1)

cause_prop1 <- apply(cause_count1, 2, function(x){x/cause_total1})

cause_count2 <- cause_count[cause_count$Year==2018&
                              cause_count$Sex==2,]

cause_count2 <- matrix(c(cause_count2$Count),
                       ncol = length(unique(cause_count2$Cause)),
                       nrow = length(unique(cause_count2$Age)))
# Each row is an age and each column is a cause.

row.names(cause_count2) <- 0:110

colnames(cause_count2) <- 1:16

cause_total2 <- rowSums(cause_count2)

cause_prop2 <- apply(cause_count2, 2, function(x){x/cause_total2})

## Now we construct the cause-specific factor.

cause_fac1 <- 
  (cause_prop1*Mx1)/
  ifelse((Mx2-Mx1)==0,1,Mx2-Mx1)*ex_decomp

cause_fac2 <- 
  (cause_prop2*Mx2)/
  ifelse((Mx2-Mx1)==0,1,Mx2-Mx1)*ex_decomp

# sum(cause_fac2-cause_fac1) 
# This get you the same results as the Arriaga method by age

cause_mat <- cause_fac2 - cause_fac1

table <- matrix(round(
  c(life.table(Mx1,"f")$ex[1],
    life.table(Mx2,"f")$ex[1],
    sum(ex_decomp),
    sum(cause_mat[,6:7]),
    sum(cause_mat[,2]),
    sum(cause_mat[,c(1,3:5,8:15)]),
    sum(cause_mat[,16]),
    sum(cause_mat)),2))

# The column number is coded by the number of causes it
# represents in the short list provided in the link above.
# You can crate your own list by changing the column number
# included and create your own list of cause contributions. 

row.names(table) <- c(
  "Life expectancy at birth for USA, 2010",
  "Life expectancy at birth for USA, 2018",
  "Life expectancy difference betwen 2010 and 2018",
  "Cardiovascular disease component",
  "Neoplasms component",
  "Other caueses component",
  "External causes component",
  "Estimated total difference from decompositon")

colnames(table) <- "USA"

kable(table, caption = "Arriaga Decomposition by age and cause")




total_dat <- data.frame(
  Age = 0:110,
  Value = ex_decomp
)

cause_dat <- data.frame(
  Age = rep(0:110,4),
  Cause = rep(c("Cardiovascular",
                "Neoplasms",
                "Other causes",
                "External causes"),each=111),
  Value = c(rowSums(cause_mat[,6:7]),cause_mat[,2],
            rowSums(cause_mat[,c(1,3:5,8:15)]),cause_mat[,16])
)

ggplot()+
  geom_col(data=cause_dat,
           mapping = aes(x=Age,y=Value,fill=Cause))+
  geom_line(data=total_dat,
            mapping = aes(x=0:110,y=Value))+
  scale_fill_brewer(type = "qual",palette = 7)+
  labs(title = "Age- and cause-decomposition of the change in the American \n female life expectancy between 2010 and 2018.",
       x="Age",y="Contributions",fill="Causes")+
  theme_bw()





LT1 <- life.table(
  ifelse(is.na(as.numeric(USA_Mx[USA_Mx$Year==2014,4])),
         0,as.numeric(USA_Mx[USA_Mx$Year==2014,4])),sex = "m")
LT2 <- life.table(
  ifelse(is.na(as.numeric(USA_Mx[USA_Mx$Year==2019,4])),
         0, as.numeric(USA_Mx[USA_Mx$Year==2019,4])), sex = "m")

e0_diff <- (log(LT2$ex[1]/LT1$ex[1])/5)*((LT2$ex[1]*LT1$ex[1])^0.5)
e0_diff2 <- (LT2$ex[1]-LT1$ex[1])/5

# Not much of a difference between the two
e0_diff-e0_diff2

ex_diff <- 
  (-log(LT2$mx/LT1$mx)/5)*
  ((LT1$dx/100000*LT2$dx/100000)^0.5)*
  (LT1$ex+LT1$ex)/2

table <- matrix(c(e0_diff,sum(ex_diff)),
                ncol=1)
row.names(table) <- c("observed gap","estimated gap")
kable(table)



ggplot()+
  geom_col(aes(x=0:110,y=ex_diff),fill="blue")+
  scale_x_continuous(breaks = seq(0,110,10))+
  theme_bw()+
  labs(title = "Age-decomposition of the change in American male  \n  life expectancy between 2014 and 2019",
       x="Age",y="Contributions")




Y1 <- 1950
Y2 <- 1955

# The period of time, this is to calculate annualized change 
tp <- Y2-Y1

# For life table, I think they used the both sex one from Sweden so this is the one (Sweden) from HMD(2023), the results might be different due to different treatments at different ages to mortality statistics across 20 years by the HMD team.

LT1 <- read.table('data/MortDecomp/SWE.bltper_1x1.txt',
                  header=T,skip=2)
LT1 <- LT1[LT1$Year==Y1,]

LT2 <- read.table('data/MortDecomp/SWE.bltper_1x1.txt',
                  header=T,skip=2)
LT2 <- LT2[LT2$Year==Y2,]

gap <- 
  (life.table(LT2$mx,sex="m")$ex[1]-
     life.table(LT1$mx,sex="m")$ex[1])+
  (life.table(LT2$mx,sex="f")$ex[1]-
     life.table(LT1$mx,sex="f")$ex[1])

# Since it's life table by both sexes,
# in order to minimize the error that
# could arise from the infant age group
# due to differential ax values, we therefore
# do an average of both the female and male 
# life table procedure. 

gap <- gap/(tp*2)





ex <- (LT1$ex+LT2$ex)/2

# from Alyson van Raalte to calculate e-dagger
ineq_edag <- function(age, dx, lx, ex, ax){
  age_length_equal <- all.equal(length(age),length(dx),
                                length(lx),length(ex),
                                length(ax))
  
  stopifnot(age_length_equal)
  
  # length of the age interval
  n <- c(diff(age),1)
  explusone <- c(ex[-1],ex[length(age)])
  # the average remaining life expectancy in each age interval 
  # (as opposed to the beginning of the interval)
  # ends up being roughly half of the ex between ages
  ex_average <- ex + ax / n * (explusone - ex)
  
  rev(cumsum(rev(dx * ex_average))) / lx 
}

edag <- (ineq_edag(0:110,LT1$dx,LT1$lx,LT1$ex,LT1$ax)*
           ineq_edag(0:110,LT2$dx,LT2$lx,LT2$ex,LT2$ax))^0.5

rho <- -log(LT2$mx/LT1$mx)/tp

fx <- (LT1$dx/100000*LT2$dx/100000)^0.5

main <- sum(rho*fx)*edag[1]
# direct component, as equation shows is the product
# between average improvements in mortality and
# life disparity or e-dagger at birth.

error <- sum((rho-sum(rho*fx))*(ex-edag[1])*fx)

gap2 <- main+error

table <- 
  matrix(round(c(LT1$ex[1],LT2$ex[1],gap,
                 sum(rho*fx),edag[1],
                 main,error,gap2),3),ncol=1)

row.names(table) <- c(paste0("Life expectancy at ",Y1),
                      paste0("Life expectancy at ",Y2),
                      paste0("Annualized change between ",
                             Y1,"-",Y2),
                      "Average improvements in mortality",
                      "Life disparity at birth",
                      "Direct component",
                      "Covariance component",
                      "Total estimated difference")

colnames(table) <- "SWE"

kable(table,caption = "Decomposition of life expectancy (VCR)")





ggplot()+
  geom_line(mapping = aes(x=0:110,y=rho,
                          color="Mortality Improvements"))+
  geom_smooth(mapping = aes(x=0:110,y=rho,
                            color="Mortality Improvements"),se=F,linetype=2,show.legend = F)+
  geom_line(mapping = aes(x=0:110,y=ex/1000,
                          color="Life expectancy"))+
  scale_y_continuous(sec.axis = 
                       sec_axis(trans=~.*1000,
                                name = "Life expectancy"))+
  scale_colour_manual(values = c("blue","red"))+
  theme_bw()+
  labs(x="Age",y="Mortality Improvements", color = "Components",
       caption = "Note: red dotted line is the smoothed curve of
       mortality improvements to help see the trend more
       clearly.")+
  theme(legend.position = "bottom")





years <- c(1907:2018)
age <- c(0:110)

plot(x = c(1905,2025), y=c(0,110),col = 0,
     xlab = 'Year', ylab = 'Age',
     axes = F)

axis(1,at=seq(1905,2025,5),labels = seq(1905,2025,5))

axis(2,at=seq(0,110,5),labels = seq(0,110,5))

polygon(x = c(1907,2018,2018), y=c(0,110,0),
        border = 'grey50')

polygon(x = c(1907,1907,2018,2018),y=c(0,0,110,0),
        border = 'grey50',col = 'pink')

segments(x0=1907,x1=2018,y0=0,y1=110,
         col='navy',lty=2,lwd=4)

text(1940,45,expression("Cohort (1907)"),
     col='navy',cex=0.9,srt = 30)

segments(x0=2018,x1=2018,y0=0,y1=110,
         col='forestgreen',lty=2,lwd=4)

text(2023,50,expression("Period (2018)"),
     col='forestgreen',cex=0.9,srt=90)

segments(x0=2008,x1=2018,y0=0,y1=10,
         col='black',lty=2,lwd=2)

text(2008,10,expression(l [c](10,2008)),
     col = "black",cex=0.9,
     srt=45)

segments(x0=1988,x1=2018,y0=0,y1=30,col='black',
         lty=2,lwd=2)

text(1998,20,expression(l[c](30,1988)),
     col = "black",cex=0.9,srt=45)

segments(x0=1968,x1=2018,y0=0,y1=50,col='black',
         lty=2,lwd=2)

text(1988,30,expression(l[c](50,1968)),
     col = "black",cex=0.9,srt=45)

segments(x0=1948,x1=2018,y0=0,y1=70,col='black',
         lty=2,lwd=2)

text(1978,40,expression(l[c](70,1948)),
     col = "black",cex=0.9,srt=45)

segments(x0=1928,x1=2018,y0=0,y1=90,col='black',
         lty=2,lwd=2)

text(1968,50,expression(l[c](90,1928)),
     col = "black",cex=0.9,srt=45)





# The functions require two input matrix of
# age-specific probability of survival of more 
# than 111 years. And the name of the two populations
# we are comparing.

CALDecompFunction<-function(Mx1,Mx2,Y,Name1,Name2){
  
  CALlx<-c()
  CALlx1<-c()
  CALlx2<-c()
  PxCh<-c()
  
  YM<-Y-Y1
  
  for (x in 1:111){
    if (x <(YM+1)){
      px1<-c()
      px2<-c()
      for (z in 1:x){
        px1<-c(px1,Mx1[z,YM-x+z])
        px2<-c(px2,Mx2[z,YM-x+z])
      }
      
      pxCH<-c(log(px2/px1),rep(0,111-x)) 
      
      lx1<-prod(px1)
      lx2<-prod(px2)
    }
    if (x >(YM)){
      px1<-c()
      px2<-c()
      for (z in (x-YM+1):x){
        px1<-c(px1,Mx1[z,YM-x+z])
        px2<-c(px2,Mx2[z,YM-x+z])}
      
      px1<-c(rep(1,(x-YM)),px1)
      px2<-c(rep(1,(x-YM)),px2)
      pxCH<-c(log(px2/px1),rep(0,111-x))	 	
      
      lx1<-prod(px1)
      lx2<-prod(px2)
    }
    CALlx1<-c(CALlx1,lx1)
    CALlx2<-c(CALlx2,lx2)
    
    PxCh<-cbind(PxCh,pxCH)
    
  }
  CALlx<- t(matrix(rep((CALlx1+ CALlx2)/2,111),111))
  
  PxCh[is.na(PxCh)]<-0
  
  ## as Guillot calculates this plus a one for l(0)
  A1<-sum(c(1,CALlx1))+.5
  A2<-sum(c(1,CALlx2))+.5
  A3<-sum(CALlx2)-sum(CALlx1)
  A4<-sum(PxCh*CALlx)
  
  #print()
  return(
    list(overall = rbind(c(paste("CAL-",Name2),
                           paste("CAL-",Name1),
                           "Diff","est-Diff"),
                         round(c(A2,A1,A3,A4),2)),
         detailed = PxCh*CALlx))
}

# The results returns a list of both age-cohort specific
# information and the aggregated information. 





LT1 <- read.table("data/MortDecomp/ITA.mltper_1x1.txt",
                  header = T,skip=2)

Mx1 <- matrix(c(1-LT1$qx),nrow = 111)

row.names(Mx1) <- 0:110
colnames(Mx1) <- unique(LT1$Year)

LT2 <- read.table("data/MortDecomp/FRATNP.mltper_1x1.txt",
                  header = T,skip=2)

Mx2 <- matrix(c(1-LT2$qx),nrow = 111)

row.names(Mx2) <- 0:110
colnames(Mx2) <- unique(LT2$Year)

Y <- 2020

Y1 <- 1909
# This is the year where we start doing the
# decomposition minus one. 

result <- CALDecompFunction(Mx1,Mx2,2020,"Italy","France")
# If this return "You need longer series of data, you
# input data series is too small.
# Then, you need to have a good look on the data you
# prepared and try it again. 

kable(result$overall,
      caption = "CAL decomposition between France & Italy")





library(RColorBrewer)
mypalette<-rev(brewer.pal(8,"YlGnBu"))
mypalette2<-rev(brewer.pal(8,"YlOrRd"))
WildColors<-c(mypalette[1:4],"white","white",mypalette2[c(6,4,2,1)])
WildColors<-c(WildColors[1:4],"white","white",WildColors[7:10])
levels<-c(-1,-0.1,-0.01,-0.001,-0.0001,0,.0001,.001,.01,.1,1)

options(scipen=10)

customAxis <- function() { 
  n <- length(levels) 
  y <- seq(min(levels), max(levels), length.out=n) 
  rect(0, y[1:(n-1)], 1, y[2:n], col=WildColors) 
  axis(4, at=y, labels=levels) 
} 

CALlxDecomp <- result$detailed

# The correct assignment of contributions and the cummulative changes
CALlxD<-matrix(0,111,111)
CALlxDS<-CALlxD

Age<-c(0:110)

YEARS<-c((Y-110):Y)

# The correct assignment of contributions and the cumulative changes

for (y in 1:111){
  for (x in 1:y){
    CALlxD[x,(111-y+x)]<-CALlxDecomp[x,y]			
    CALlxDS[x,(111-y+x)]<-sum(CALlxDecomp[(1:x),y])
  }
}

par(cex.axis=1.1)
filled.contour(YEARS,Age,t(CALlxD),
               levels=levels,col=WildColors,
               key.axes=customAxis(),ylab="Age-contribution",
               xlab="Year",cex.lab=1.2)
title("CAL-difference between \n Italy and France,2020",
      adj=0.3)
mtext("Italy",1,0.5,adj=.9,cex=1.1)
mtext("France",3,0.5,adj=.9,cex=1.1)

par(cex.axis=1.1)
filled.contour(YEARS,Age,t(CALlxDS),
               levels=levels,col=WildColors,
               key.axes=customAxis(),ylab="Age-contribution",
               xlab="Year",cex.lab=1.2)
title("CAL-difference between Italy and \n France, cumulative changes, 2020",adj=0.1)
mtext("Italy",1,0.5,adj=.9,cex=1.1)
mtext("France",3,0.5,adj=.9,cex=1.1)




data <- read.csv("data/FertDecomp/data_base_prop.csv")

data$edu <- factor(data$edu,
                   levels = c("High","Medium","Low"))

# We can first visualize the education composition
# change across time.

ggplot(data,aes(x=cohort,y=Prop_edu,fill=edu))+
  geom_col(width = 1)+
  scale_y_continuous(labels = function(x){paste0(x*100,"%")})+
  theme_bw()+
  labs(title ="Education-composition for cohorts of South Korean women 1930 to 1970.",x="Cohort",y="Proportion",fill="Education level")






data <- read.csv("data/FertDecomp/data_base_fert.csv")

ggplot(data,aes(x=cohort,y=CCF,
                linetype=edu,color=edu))+
  geom_line(linewidth=1)+
  theme_bw()+
  labs(title = "Korean Complete Cohort Fertility, 1930-1970",
       x="Cohort",y="Complete Cohort Fertility",
       color = "Education level and National level",
       linetype = "Education level and National level")+
  theme(legend.position = "bottom")





Year1 <- 1940

Year2 <- 1950

data <- read.csv("data/FertDecomp/data_base_decomp.csv")

data <- data[,c("cohort","edu", "B", 
                "We", "W", "E", "Fe")]

data1 <- data[data$cohort==Year1,]

data2 <- data[data$cohort==Year2,]

mat1 <- as.matrix(data1[,3:7])
row.names(mat1) <- unique(data1$edu)

mat2 <- as.matrix(data2[,3:7])
row.names(mat2) <- unique(data2$edu)

mid_E <- (mat1[,4] * mat2[,4]) ^0.5

mid_F <- (mat1[,5] * mat2[,5]) ^ 0.5

deriv_E <- (log(mat2[,4] / mat1[,4]) / (Year2-Year1)) * mid_E

deriv_F <- (log(mat2[,5] / mat1[,5]) / (Year2-Year1)) * mid_F

Results_E <- deriv_E*mid_F

Results_F <- deriv_F*mid_E

Results <-  
  data.frame(Edu = rep(row.names(mat1),3),
             variable = c(rep(c("Fertility",
                                "Education-composition"),
                              each=3),
                          rep("Total",3)),
             value = c(Results_F*100,Results_E*100,
                       Results_E*100+Results_F*100))

table <- matrix(c(round(sum(mat1[,4]*mat1[,5]),2),
                  round(sum(mat2[,4]*mat2[,5]),2),
                  round(sum(c(Results_F+Results_E)),3),
                  round(sum(Results_F),3),
                  round(sum(Results_E),3)),
                ncol = 1)

row.names(table) <-
  c(paste0("CCF in ",Year1),
    paste0("CCF in ",Year2),
    "Total annualized change",
    "Fertility component",
    "Education-composition component")

colnames(table) <- "Korea"

kable(table,caption = "CCF decomposition")




Results$Edu <- factor(Results$Edu,
                      levels = c("Low","Medium","High"))

ggplot(Results,aes(x=variable,y=value))+
  geom_col(aes(fill=Edu),width = 0.8)+
  stat_summary(geom = "point",fun = sum,show.legend = F)+
  labs(x = "Component",y = "Contribution to Change in CCF",
       fill = "Education Level")+
  theme_bw()





# calculate CALC and decomposition
CALCDecompFunction  <- function(px1, px2, lxLx, 
                                Name1, Name2){
  CALClx  <- c()
  CALClx1 <- c()
  CALClx2 <- c()
  PxCh   <- c()
  
  PxCh <- log(px2 / px1)
  PxCh <- ifelse(is.na(PxCh), 0, PxCh)
  colnames(PxCh) <- rownames(PxCh) <- NULL
  
  # change the order: 1st column (the youngest cohort) -> 
  # the last column (the oldest cohort)
  
  PxCh <- PxCh[, ncol(PxCh):1]
  
  px2CALlx <- function(px){
    # px matrix to lx
    lx <- apply(px, 2, cumprod)
    lx <- rbind(rep(1, ncol(px)), lx)
    
    # lx to CAL lx
    CALlx <- c()
    for(i in 1:ncol(px)){
      order <- 38:1
      CALlx[i] <- lx[order[i], i]
    }
    CALlx <- rev(CALlx)
    
    return(CALlx)
  }
  
  CALClx1 <- px2CALlx(px1)
  CALClx2 <- px2CALlx(px2)
  
  CALClx_mid <- t(matrix(rep((CALClx1 + CALClx2)/2, 38), length(CALClx1)))
  
  # calculate CALC
  CALC <- function(lx, type){
    
    # CALC using lx
    CALC_lx <- sum(lx[-1]) + 0.5
    
    # CALC using Lx
    Lx <- (lx[1:37] + lx[2:38]) / 2
    Lx <- c(Lx, lx[38])
    CALC_Lx <- sum(Lx)
    
    out <- ifelse(type == "lx", CALC_lx, CALC_Lx)
    
    return(out)
  }
  
  # final output
  A1 <- CALC(lx = CALClx1, type = lxLx)
  A2 <- CALC(lx = CALClx2, type = lxLx)
  A3 <- sum(CALClx2) - sum(CALClx1)
  A4 <- sum(PxCh * CALClx_mid)
  
  
  print(rbind(c(paste("CALC-", Name1), 
                paste("CALC-", Name2), 
                "Diff", "est-Diff"), 
              round(c(A1, A2, A3, A4), 2)))
  return(PxCh * CALClx_mid)
}





# We are comparing the two populations of Spain and Sweden.

Names <- c("SWE","ESP")

Names2 <- c("Sweden","Spain")

# The original code is written with tidyverse.

library(tidyverse)

# We are not considering the ages above 50.
over50 <- c("50", "51", "52", "53", "54", "55+")

# Here we are creating our own function that instead of
# matching contents (the overlap in the Venn diagram) 
# we are finding the inverse. (the non-overlapping part)
`%out%` = Negate(`%in%`)

# We are using our own color palette
library(RColorBrewer)

mypalette  <- rev(brewer.pal(8, "YlGnBu"))
mypalette2 <- rev(brewer.pal(8, "YlOrRd"))

WildColors <- c(mypalette[1:4], "white", 
                "white", mypalette2[c(6, 4, 2, 1)])

Mycolor <- c(mypalette[1:4], "white", "white", "#FED976",
             "#FFEDA0", "#D4B9DA", "#FD8D3C")

# We are specifying the levels, here we are 
# creating a logged scale manually. This illustrates the
# differences more vividly.

levels <- c(-1, -0.1, -0.01, -0.001, -0.0001, 0, 
            .0001, .001, .01, .1, 1)

# This function is to accompany the previous
# logged scale and to make it show in the base r plot.
customAxis <- function() { 
  n <- length(levels) 
  y <- seq(min(levels), max(levels), length.out = n) 
  rect(0, y[1:(n-1)], 1, y[2:n], col = WildColors) 
  axis(4, at = y, labels = levels) 
} 

# A function to prepare the data, since for the fertility
# data we are combining completed cohort fertility and period
# fertility information from cohorts not yet reaching their
# fertility window. 

lxpx <- function(Names, lxpx){
  
  # target life table function: lx or px
  target <- lxpx
  
  ## Data from country 1
  A1 <- read.table(paste("data/FertDecomp/", Names, 
                         "cft.txt", sep = ""), 
                   header = TRUE, fill = TRUE, skip = 2)
  
  ## Function to create a matrix of lx or px
  widelxpx <- function(data){
    px <- data %>% 
      as.data.frame() %>% 
      #filter(Cohort >= Y1) %>% 
      # select year from the same year
      filter(x %out% over50) %>%              
      # select age (12- to 49)
      mutate(q1x = as.numeric(as.character(q1x)),
             px = 1 - q1x) %>% 
      select(Cohort, x, px)
    
    # create a matrix of px
    px_wide <- px %>% 
      mutate(px = ifelse(x %in% 
                           c("12-", "13", "14", "15") & 
                           is.na(px), 1, px)) %>% 
      spread(key = Cohort, value = px) %>%
      select(-x) %>% 
      as.matrix()
    
    lx_wide <- matrix(NA, 
                      ncol = ncol(px_wide), 
                      nrow = nrow(px_wide))
    
    lx_wide[1, ] <- 1
    for(i in 1:(nrow(lx_wide)-1)){
      lx_wide[i+1, ] <- lx_wide[i, ] * px_wide[i, ]
    }
    
    colnames(lx_wide) <- colnames(px_wide)
    
    if(target == "lx"){
      outcome <- lx_wide
    } else {
      outcome <- px_wide
    }
    
    return(outcome)
  }
  
  
  ## For country 1
  lf_bc <- widelxpx(data = A1)
  
  if(any(colnames(lf_bc) == "1966")){
    lf_1966 <- lf_bc[, "1966"]
  } else {
    lf_1966 <- NA
  }
  
  ## the position of the maximum completed birth cohort
  if(length(lf_1966[!is.na(lf_1966)]) == 38){
    min1 <- which(colnames(lf_bc) == "1966")
    
    lf_bc <- lf_bc[, min1:ncol(lf_bc)]
    
    # extract data in a triangle format
    lf_triangle <- c()
    for(k in 1:ncol(lf_bc)){
      lf_triangle <- cbind(lf_triangle, 
                           c(lf_bc[1:(38 - k + 1), k],
                             rep(NA, k - 1)))
    }
    
    colnames(lf_triangle) <- colnames(lf_bc)
    
  } else {
    min1 <- lf_bc[nrow(lf_bc), ]
    min1 <- length(min1[!is.na(min1)])
    
    ## select data from the maximum completed birth cohort
    lf_triangle <- lf_bc[, min1:ncol(lf_bc)]
  }
  
  
  ### Prepare period data to create hypothetical data
  
  # create Age:Year matrix contains lx or px
  data_select <- function(Pdata, lf_bc){
    
    startY <- 
      as.numeric(as.character(colnames(lf_bc)))[ncol(lf_bc)] + 12
    lastBC <- lf_bc[, ncol(lf_bc)]
    endY   <- startY + length(lastBC[!is.na(lastBC)]) - 1
    
    px_wide <- Pdata %>%
      as.data.frame() %>% 
      filter(x %out% over50) %>%
      mutate(q1x = as.numeric(as.character(q1x)),
             px = 1 - q1x) %>%
      select(Year, x, px) %>%
      filter(Year >= startY & Year <= endY) %>%
      spread(key = Year, value = px) %>%
      select(-x) %>%
      as.matrix()
    
    lx_wide <- matrix(NA, 
                      ncol = ncol(px_wide), 
                      nrow = nrow(px_wide))
    lx_wide[1, ] <- 1
    for(i in 1:(nrow(lx_wide)-1)){
      lx_wide[i+1, ] <- lx_wide[i, ] * px_wide[i, ]
    }
    
    colnames(lx_wide) <- colnames(px_wide)
    
    if(target == "lx"){
      outcome <- lx_wide
    } else {
      outcome <- px_wide
    }
    
    return(outcome)
  }
  
  ## Data from country A using period fertility table
  A1_per <- read.table(paste("data/FertDecomp/", Names, 
                             "pft.txt", sep = ""), 
                       header = TRUE, fill = TRUE, skip = 2)
  A1_per <- data_select(Pdata = A1_per, 
                        lf_bc = lf_triangle)
  
  # make new data strage: hypthetical cohort
  period2cohort <- function(data){
    
    n <- ncol(data)
    
    bc <- c()
    for(i in 2:n){
      row <- c()
      row <- c(data[i, - c(1:(i - 1))], rep(NA, i-1))
      bc  <- rbind(bc, row)
    }
    bc <- rbind(data[1,], bc)
    
    years <- as.numeric(as.character(colnames(bc)))
    colnames(bc) <- years - 12
    rownames(bc) <- NULL
    
    return(bc)
  }
  
  A1_hypbc <- period2cohort(data = A1_per)
  
  ## combine cohort data and hypothetical data
  A1_hypbc <- rbind(A1_hypbc, 
                    matrix(NA, 
                           dim(lf_triangle)[1] - 
                             dim(A1_hypbc)[1],
                           ncol(A1_hypbc)))
  lf_A1_bc <- cbind(lf_triangle, A1_hypbc[, -1])
  
  
  return(lf_A1_bc)
}

px1 <- lxpx(Names = Names[1], lxpx = "px")
px2 <- lxpx(Names = Names[2], lxpx = "px")

# Calculate CALC and decomposition

CALlxDecompBC <- CALCDecompFunction(px1, px2, 
                                    "Lx", Names[1], Names[2])

# The correct assignment of contributions and the cummulative changes
CALlxD  <- matrix(0, 38, 38)
CALlxDS <- CALlxD

Age <- c(12:49)

BC <- 1966:2003

for (y in 1:38){
  for (x in 1:y){
    # Age-cohort difference
    CALlxD[x, (38 - y + x)]  <- CALlxDecompBC[x, y]			
    # cumulative age-cohort difference
    CALlxDS[x, (38 - y + x)] <- sum(CALlxDecompBC[(1:x), y])
  }
}

options(scipen = 10)

par(cex.axis = 1)
par(oma = c(1, 0, 0, 0))
filled.contour(BC, Age, t(CALlxD), 
               levels = levels, 
               col = WildColors, 
               key.axes = customAxis(), 
               ylab = "Age- & cohort-contribution", 
               xlab = "", 
               cex.lab = 1.1,
               plot.axes = {
                 axis(1, 
                      at = c(1966, seq(1970, 2000, by = 5),2003),
                      labels = c("", "1970\n(1982)", 
                                 "1975\n(1987)","1980\n(1992)",
                                 "1985\n(1997)","1990\n(2002)",
                                 "1995\n(2007)", "2000\n(2012)",
                                 "2003\n(2015)"), 
                      hadj = 0.6, padj = 0.5, 
                      cex.axis = 0.9)
                 axis(2, at = seq(15, 50, by = 5), 
                      labels = seq(15, 50, by = 5))})

mtext("Birth cohort\n(Year)", 1, line = 4.5, adj = 0.4)

mtext(Names2[2], 3, 0.5, adj = 0.9, cex = 0.9)

mtext(Names2[1], 1, 0.5, adj = 0.9, cex = 0.9)





options(scipen = 10)

par(cex.axis = 1)
par(oma = c(1, 0, 0, 0)) #bottom, right, top, left
filled.contour(BC, Age, t(CALlxDS), 
               levels = levels, 
               col = WildColors, 
               key.axes = customAxis(), 
               ylab = "Cumulative age- & cohort-contribution",
               xlab = "", cex.lab = 1.1,
               plot.axes = {
                 axis(1, at = c(1966, 
                                seq(1970, 2000, by = 5),
                                2003), 
                      labels = c("", "1970\n(1982)",
                                 "1975\n(1987)", "1980\n(1992)", 
                                 "1985\n(1997)", "1990\n(2002)",
                                 "1995\n(2007)", "2000\n(2012)",
                                 "2003\n(2015)"), 
                      hadj = 0.6, padj = 0.5, cex.axis = 0.9)
                 axis(2, at = seq(15, 50, by = 5), 
                      labels = seq(15, 50, by = 5))})

mtext("Birth cohort\n(Year)", 1, line = 4.5, adj = 0.4) 

mtext(Names2[2], 3, 0.5, adj = 0.9, cex = 0.9)

mtext(Names2[1], 1, 0.5, adj = 0.9, cex = 0.9)





library(tidyverse)

# data cleaning ----

Birth <- read.table("data/FertDecomp/FRATNPbirthsRR.txt", 
                    header = TRUE, fill = TRUE, skip = 2)

Birth$Age[Birth$Age=="12-"] <- "12"
Birth$Age[Birth$Age=="55+"] <- "55"

Birth$Age <- as.numeric(Birth$Age)

Pop <- read.table("data/FertDecomp/FRATNPexposRR.txt", 
                  header = TRUE, fill = TRUE, skip = 2)

Birth <- left_join(Birth,Pop)

Tot.birth = read.table("data/FertDecomp/Births.txt", 
                       header = TRUE, fill = TRUE, skip = 1)

Tot.birth = Tot.birth %>% mutate(F.per = Female/Total)

lx = read.table("data/FertDecomp/fltper_1x1.txt",
                header = TRUE, fill = TRUE, skip = 1)[,c(1,2,6)]
lx$Age = gsub("[+]", "", lx$Age)
lx$Age = as.numeric(lx$Age)

Birth.C = Birth

Birth.C = inner_join(Birth.C,Tot.birth[,c(1,5)])
Birth.C = Birth.C %>% mutate(Female = Total * F.per)

Birth.C = Birth.C %>% mutate(ASFR = Total/Exposure)
Birth.C = inner_join(Birth.C,lx)
Birth.C = Birth.C %>% mutate(lx = lx/100000) %>% 
  mutate(ASNRR = ASFR * lx * F.per)

NRR <- Birth.C %>%group_by(Year) %>% summarise(NRR=sum(ASNRR))

ggplot(NRR,aes(x=Year,y=NRR))+
  geom_line()+
  scale_x_continuous(n.breaks = 10)+
  geom_vline(xintercept = c(1970,1975),linetype=3)+
  theme_bw()+
  labs(x="Year",y="NRR",title = "Time trend in the Net-Reproductive Rate, France 1946-2020")




# main decomposition ----

Year1 <- 1970

Year2 <- 1975

r_nrr <- 
  log(NRR$NRR[which(NRR$Year==Year2)]/
        NRR$NRR[which(NRR$Year==Year1)])/(Year2-Year1)

d_nrr <- 
  sqrt(NRR$NRR[which(NRR$Year==Year2)]*
         NRR$NRR[which(NRR$Year==Year1)])*r_nrr

r_tfr <- 
  log(Birth.C$ASFR[which(Birth.C$Year==Year2)]/
        Birth.C$ASFR[which(Birth.C$Year==Year1)])/(Year2-Year1)

r_tfr[is.na(r_tfr)] <- 0
r_tfr[is.nan(r_tfr)] <- 0
r_tfr[is.infinite(r_tfr)] <- 0

r_lx <- 
  log(Birth.C$lx[which(Birth.C$Year==Year2)]/
        Birth.C$lx[which(Birth.C$Year==Year1)])/(Year2-Year1)

r_per <- 
  log(Birth.C$F.per[which(Birth.C$Year==Year2)]/
        Birth.C$F.per[which(Birth.C$Year==Year1)])/(Year2-Year1)

m_tfr <- 
  sqrt(Birth.C$ASFR[which(Birth.C$Year==Year1)]*
         Birth.C$ASFR[which(Birth.C$Year==Year2)])

m_lx <- sqrt(Birth.C$lx[which(Birth.C$Year==Year1)]*
               Birth.C$lx[which(Birth.C$Year==Year2)])

m_per <- sqrt(Birth.C$F.per[which(Birth.C$Year==Year1)]*
                Birth.C$F.per[which(Birth.C$Year==Year2)])

d_tfr <- sum(m_tfr*m_lx*m_per*r_tfr)
d_lx <- sum(m_tfr*m_lx*m_per*r_lx)
d_per <- d_nrr-d_tfr-d_lx

table <- 
  matrix(round(
    c(NRR[NRR$Year==Year1,]$NRR,
      NRR[NRR$Year==Year2,]$NRR,
      d_nrr,d_lx,d_tfr+d_per),3),ncol = 1)

row.names(table) <- 
  c(paste0("NRR in ",Year1),
    paste0("NRR in ",Year2),
    "Annualized change of NRR",
    "Mortality Component",
    "Fertility Component")

colnames(table) <- "France"

kable(table,caption = "NRR decomposition")





# This function is to help calculating the
# cause specific factor R_i with counts of 
# death by each cause

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
  
  Lx<-lx+(ax-AgeI)*dx
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
      matrix(rep(AgeI[(y+1):Ncol],each=Nrow),Nrow)*
      matrix(rep(dx[y]*Rxi[,y],length((y+1):Ncol)),Nrow)
    LYLi2<- cbind(LYLi2,rowSums(L))
    LYLi3<- (LYLi3+L)
  }
  y<-Ncol
  LYLi2<- cbind(LYLi2,(AgeI[y]-ax[y])*dx[y]*Rxi[,y])
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
  Dif<-length(Causes)-Ncauses
  
  #Selected causes
  if(Dif>0){causesN<- c(causesN0[S],"Other causes")}
  if(Dif>0){Ncauses<-(length(S)+1)}
  
  # COL<-matlab.like2(NcausesT)
  COL<-primary.colors(NcausesT)
  
  N<-dim(FLT)[1]
  AgeI<-rep(1,N)
  lx<-as.numeric(FLT[,6])/100000
  dx<-c(lx[-N]-lx[-1],lx[N])
  
  FRx<-RxiMatrix(B,0)
  Fdxi<-FRx*matrix(rep(dx,each=NcausesT),NcausesT)
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
  ggplot(test.p, aes(x = Age, y = value, 
                     group = variable, fill = variable)) + 
    geom_area(size = 0.3, color = "black", 
              position = position_stack()) + 
    scale_y_reverse(name = "Probability of surviving and life years lost",
                    breaks = c(0, .2, .4, .6, .8, 1), 
                    labels = c(1, .8, .6, .4, .2, 0), 
                    limits = c(1,0)) + 
    scale_x_continuous(name = "Age",
                       limits = c(AGEF,AGEL)) + 
    scale_fill_manual(name = "", values = COL,
                      labels = causesN) +
    theme_bw(base_size = 10) + 
    theme(legend.position = "bottom") + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 
}





library(colorRamps)
library(reshape2)

#sex
SEX<-c("Male","Female")
Sex<-2

#Ages
AGEF<-0
AGEL<-90 #Age Limit

# population, we will use France
Names<-c("FRATNP")
Names2<-c("France")

#year
Year<-1950

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

Af<-read.table("data/LYL/fltper_1x1.txt",
               header=TRUE,fill=TRUE,skip=1)
Am<-read.table("data/LYL/mltper_1x1.txt",
               header=TRUE,fill=TRUE,skip=1)
Af<-Af[Af$Year==Year,]
Am<-Am[Am$Year==Year,]

for (CC in 2:13){
  
  B1C<-t(read.csv(paste("data/LYL/","Country",CO[1],"Cause",
                        CC,"f",".txt",sep=""),
                  header=TRUE,fill=TRUE,skip=0)[,-1])
  B1Cm<-t(read.csv(paste("data/LYL/","Country",CO[1],"Cause",
                         CC,"m",".txt",sep=""),
                   header=TRUE,fill=TRUE,skip=0)[,-1])
  
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

S<-c(1:5)





NicePlot2(LT,WW,S)






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
  Pop <- fread(paste0("data/Variabler/",
                      country,".Population.txt"),
               header = T,skip=1)
  colnames(Pop) <- c("Year","Age","Female","Male","Total")
  Pop$Age[Pop$Age=="110+"]<-110
  Pop$Age <- as.numeric(Pop$Age)
  Pop$Year = gsub("[-]", "", Pop$Year)
  Pop$Year <- as.numeric(Pop$Year)
  
  # Birth
  Birth <- fread(paste0("data/Variabler/",
                        country,".Births.txt"),
                 header = T,skip=1)
  Pop$Year <- as.numeric(Pop$Year)
  
  # Death
  Death <- fread(paste0("data/Variabler/",
                        country,".Deaths_lexis.txt"),
                 header = T,skip=1)
  Death$Cohort[Death$Cohort=="."]<-0
  Death$Cohort <- as.numeric(Death$Cohort)
  Death$Age[Death$Age=="110+"]<-110
  Death$Age <- as.numeric(Death$Age)
  
  # specify the vectors needed.
  
  sex_ratio <- rep(0,age+1)
  
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
  rB <- rep(0,age+1) 
  rS <- rep(0,age+1)
  rM <- rep(0,age+1)
  
  # We are calculating the cohort survival per
  # HMD protocol which is the  parallelogram 
  # but first age 0 there is only a triangle.
  
  P_male[1] <- Pop[Year==paste0(year)&Age==0,Male]
  P_female[1] <- Pop[Year==paste0(year)&Age==0,Female]
  
  sex_ratio[1] <- P_male[1]/P_female[1]
  
  #### Birth
  
  B_male[1] <- Birth[Year==year-1,Male]
  B_female[1] <- Birth[Year==year-1,Female]
  
  rB[1] <- B_male[1]/B_female[1]
  
  #### Survival
  
  # Survival is calculated as the probability
  # of dying for people exposed.
  
  ### male survival
  qx_male <- Death[Cohort==year-1&
                     Age==0&
                     Year==year-1,][,c(1:3,5)]
  qx_male <- qx_male[,sum(Male),by=.(Age,Cohort)]
  # This is essentially the group_by and summarise function
  # combined
  names(qx_male)[3] <- "D"
  
  qx_male$P <- 
    Pop[Year==year&Age==0,Male]+qx_male$D
  
  qx_male$Mx <- qx_male$D/qx_male$P
  
  S_male[1] <- (1-qx_male$Mx)
  
  ### female survival
  qx_female <- Death[Cohort==year-1&
                       Age==0&
                       Year==year-1,][,c(1:3,4)]
  qx_female <- qx_female[,sum(Female),by=.(Age,Cohort)]
  names(qx_female)[3] <- "D"
  
  qx_female$P <- 
    Pop[Year==year&Age==0,Female]+qx_female$D
  
  qx_female$Mx <- qx_female$D/qx_female$P
  
  S_female[1] <- (1-qx_female$Mx)
  
  ### Survival ratio
  rS[1] <- S_male[1]/S_female[1]
  
  #### Net-migration
  M_male[1] <- P_male[1]/(B_male[1]*S_male[1])
  
  M_female[1] <- P_female[1]/(B_female[1]*S_female[1])
  
  rM[1] <- M_male[1]/M_female[1]
  
  for (a in 1:age) {
    
    #### Sex ratio
    
    P_male[a+1] <- Pop[Year==paste0(year)&Age==a,Male]
    P_female[a+1] <- Pop[Year==paste0(year)&Age==a,Female]
    
    sex_ratio[a+1] <- P_male[a+1]/P_female[a+1]
    
    #### Birth
    
    B_male[a+1] <- Birth[Year==year-a-1,Male]
    B_female[a+1] <- Birth[Year==year-a-1,Female]
    
    rB[a+1] <- B_male[a+1]/B_female[a+1]
    
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
    
    last_qx_male$Mx <- last_qx_male$D/last_qx_male$P
    
    qx_male$Mx <- qx_male$D/(qx_male$P)
    
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
    
    qx_female$Mx <- qx_female$D/(qx_female$P)
    
    S_female[a+1] <- 
      tail(cumprod(1-qx_female$Mx),1)*(1-last_qx_female$Mx)
    
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
  geom_line(mapping=aes(y=sex_ratio,color="Sex Ratios"),
            linewidth=1,alpha=0.8)+
  geom_line(mapping=aes(y=rM,color="Net-Migration Ratios"),
            linewidth=1,alpha=0.8)+
  geom_line(mapping=aes(y=rS,color="Survival Ratios"),
            linewidth=1,alpha=0.8)+
  geom_line(mapping=aes(y=rB,color="Birth Ratios"),
            linewidth=1,alpha=0.8)+
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





Year1 <- 2010

Year2 <- 2020

yeard <- 10

JPN_pop <- read.table("data/standard/JPN.Population.txt",
                      header = T,skip = 2)

JPN_gth1 <- log_func(JPN_pop[JPN_pop$Year==Year1,"Female"],
                     JPN_pop[JPN_pop$Year==(Year1-yeard),"Female"],
                     yeard)

JPN_gth2 <- log_func(JPN_pop[JPN_pop$Year==Year2,"Female"],
                     JPN_pop[JPN_pop$Year==(Year2-yeard),"Female"],
                     yeard)

JPN_pop1 <- (JPN_pop[JPN_pop$Year==Year1,"Female"]*
               JPN_pop[JPN_pop$Year==(Year1-yeard),"Female"])^0.5

JPN_pop2 <- (JPN_pop[JPN_pop$Year==Year2,"Female"]*
               JPN_pop[JPN_pop$Year==(Year2-yeard),"Female"])^0.5

Comp1 <- JPN_pop1/sum(JPN_pop1)

Comp2 <- JPN_pop2/sum(JPN_pop2)

Comp_avg <- 
  (JPN_pop2*JPN_pop1)^0.5/
  sum((JPN_pop2*JPN_pop1)^0.5)

r_bar1 <- sum(JPN_gth1*Comp1)*100

r_bar2 <- sum(JPN_gth2*Comp2)*100

r_bar_diff <- 
  (r_bar2-r_bar1)/(Year2-Year1)

r_bar_direct <-
  sum((JPN_gth2-JPN_gth1)/(Year2-Year1)*Comp_avg)*100

gth_avg <- log_func(JPN_pop2,JPN_pop1,Year2-Year1)

r_bar_comp <- 
  (sum(gth_avg^2*Comp_avg)-
     sum((gth_avg*Comp_avg)^2))*100

r_bar_decomp <- r_bar_direct+r_bar_comp

table <- matrix(round(c(r_bar1,r_bar2,r_bar_diff,
                        r_bar_direct,r_bar_comp,
                        r_bar_decomp),2))

row.names(table) <- c("growth rate 2000-2010",
                      "growth rate 2010-2020",
                      "difference in growth rates",
                      "direct component",
                      "composition component",
                      "total estimated difference")

colnames(table) <- "Japan"

kable(table,caption = "Growth rate decompostion (VCR)")




USA_Mx <- read.table("data/MortDecomp/USA.Mx_1x1.txt",
                     header = T,skip = 2,fill = T)

Mx1 <- USA_Mx[USA_Mx$Year==2000,3]

Mx2 <- USA_Mx[USA_Mx$Year==2010,3]

Mx3 <- USA_Mx[USA_Mx$Year==2018,3]

ex_diff1 <- 
  life.table(Mx2,"f")$ex[1]-
  life.table(Mx1,"f")$ex[1]

ex_diff2 <- 
  life.table(Mx3,"f")$ex[1]-
  life.table(Mx2,"f")$ex[1]

ex_decomp1 <- 
  arriaga(as.numeric(Mx1),
          as.numeric(Mx2),
          sex = "f",
          breakdown = F)

ex_decomp2 <- 
  arriaga(as.numeric(Mx2),
          as.numeric(Mx3),
          sex = "f",
          breakdown = F)

cause_count <- read.csv("data/MortDecomp/USA_Cause16_counts.csv")

cause_count1 <- cause_count[cause_count$Year==2000&
                              cause_count$Sex==2,]

cause_count1 <- matrix(c(cause_count1$Count),
                       ncol = length(unique(cause_count1$Cause)),
                       nrow = length(unique(cause_count1$Age)))

row.names(cause_count1) <- 0:110

colnames(cause_count1) <- 1:16

cause_total1 <- rowSums(cause_count1)

cause_prop1 <- apply(cause_count1, 2, function(x){x/cause_total1})

cause_count2 <- cause_count[cause_count$Year==2010&
                              cause_count$Sex==2,]

cause_count2 <- matrix(c(cause_count2$Count),
                       ncol = length(unique(cause_count2$Cause)),
                       nrow = length(unique(cause_count2$Age)))

row.names(cause_count2) <- 0:110

colnames(cause_count2) <- 1:16

cause_total2 <- rowSums(cause_count2)

cause_prop2 <- apply(cause_count2, 2, function(x){x/cause_total2})


cause_count3 <- cause_count[cause_count$Year==2010&
                              cause_count$Sex==2,]

cause_count3 <- matrix(c(cause_count3$Count),
                       ncol = length(unique(cause_count3$Cause)),
                       nrow = length(unique(cause_count3$Age)))

row.names(cause_count3) <- 0:110

colnames(cause_count3) <- 1:16

cause_total3 <- rowSums(cause_count3)

cause_prop3 <- apply(cause_count3, 2, function(x){x/cause_total3})

cause_fac1_1 <- 
  (cause_prop1*Mx1)/
  ifelse((Mx2-Mx1)==0,1,Mx2-Mx1)*ex_decomp1

cause_fac2_1 <- 
  (cause_prop2*Mx2)/
  ifelse((Mx2-Mx1)==0,1,Mx2-Mx1)*ex_decomp1

cause_fac2_2 <- 
  (cause_prop2*Mx2)/
  ifelse((Mx3-Mx2)==0,1,Mx3-Mx2)*ex_decomp2

cause_fac3_2 <- 
  (cause_prop3*Mx3)/
  ifelse((Mx3-Mx2)==0,1,Mx3-Mx2)*ex_decomp2

cause_mat1 <- cause_fac2_1 - cause_fac1_1

cause_mat2 <- cause_fac3_2 - cause_fac2_2




table <- matrix(c(
  round(c(life.table(Mx1,"f")$ex[1],
          life.table(Mx2,"f")$ex[1]),2),
  round(c(
    sum(ex_decomp1)/10,
    sum(cause_mat1[,6:7])/10,
    sum(cause_mat1[,2])/10,
    sum(cause_mat1[,c(1,3:5,8:15)])/10,
    sum(cause_mat1[,16])/10,
    sum(cause_mat1)/10),3)))

row.names(table) <- c(
  "Life expectancy at birth for USA, 2000",
  "Life expectancy at birth for USA, 2010",
  "Life expectancy annualized difference betwen 2000 and 2010",
  "Cardiovascular disease component",
  "Neoplasms component",
  "Other caueses component",
  "External causes component",
  "Estimated total difference from decompositon")

colnames(table) <- "USA"

kable(table, caption = "Arriaga Decomposition by age and cause")



table <- matrix(c(
  round(c(life.table(Mx2,"f")$ex[1],
          life.table(Mx3,"f")$ex[1]),2),
  round(c(
    sum(ex_decomp2)/8,
    sum(cause_mat2[,6:7])/8,
    sum(cause_mat2[,2])/8,
    sum(cause_mat2[,c(1,3:5,8:15)])/8,
    sum(cause_mat2[,16])/8,
    sum(cause_mat2)/8),3)))

row.names(table) <- c(
  "Life expectancy at birth for USA, 2010",
  "Life expectancy at birth for USA, 2018",
  "Life expectancy annualized difference betwen 2010 and 2018",
  "Cardiovascular disease component",
  "Neoplasms component",
  "Other caueses component",
  "External causes component",
  "Estimated total difference from decompositon")

colnames(table) <- "USA"

kable(table, caption = "Arriaga Decomposition by age and cause")




# Remember to load the two essential functions, 
# the life table function and the function to 
# calculate e-dagger or life disparity.

Y1 <- 2009
Y2 <- 2019

tp <- Y2-Y1

LT1 <- read.table('data/MortDecomp/SWE.bltper_1x1.txt',
                  header=T,skip=2)
LT1 <- LT1[LT1$Year==Y1,]

LT2 <- read.table('data/MortDecomp/SWE.bltper_1x1.txt',
                  header=T,skip=2)
LT2 <- LT2[LT2$Year==Y2,]

gap <- 
  (life.table(LT2$mx,sex="m")$ex[1]-
     life.table(LT1$mx,sex="m")$ex[1])+
  (life.table(LT2$mx,sex="f")$ex[1]-
     life.table(LT1$mx,sex="f")$ex[1])

gap <- gap/(tp*2)

ex <- (LT1$ex+LT2$ex)/2

edag <- (ineq_edag(0:110,LT1$dx,LT1$lx,LT1$ex,LT1$ax)*
           ineq_edag(0:110,LT2$dx,LT2$lx,LT2$ex,LT2$ax))^0.5

rho <- -log(LT2$mx/LT1$mx)/tp

fx <- (LT1$dx/100000*LT2$dx/100000)^0.5

main <- sum(rho*fx)*edag[1]

error <- sum((rho-sum(rho*fx))*(ex-edag[1])*fx)

gap2 <- main+error

table <- 
  matrix(round(c(LT1$ex[1],LT2$ex[1],gap,
                 sum(rho*fx),edag[1],
                 main,error,gap2),3),ncol=1)

row.names(table) <- c(paste0("Life expectancy at ",Y1),
                      paste0("Life expectancy at ",Y2),
                      paste0("Annualized change between ",
                             Y1,"-",Y2),
                      "Average improvements in mortality",
                      "Life disparity at birth",
                      "Direct component",
                      "Covarinace component",
                      "Total estimated difference")

colnames(table) <- "SWE"

kable(table,caption = "Decomposition of life expectancy (VCR)")





library(tidyverse)
library(readxl)

c="Nigeria"

p = c("1980-1985","1985-1990","1990-1995",
      "1995-2000","2000-2005","2005-2010",
      "2010-2015","2015-2020")

yeard = 5

# data cleaning ----
SRB <- 
  read_excel("data/WPP/WPP2019_FERT_F02_SEX_RATIO_AT_BIRTH.xlsx",
             sheet = "ESTIMATES", range = "C17:U272") %>% 
  filter(Type == "Country/Area") %>% select(c(1,6:19)) %>%
  pivot_longer(2:15,names_to = "Period")

Fertility <- 
  read_excel("data/WPP/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx", 
             sheet = "ESTIMATES", range = "C17:O3587") %>% 
  filter(Type == "Country/Area") %>% select(c(1,6:13)) %>%
  pivot_longer(3:9,names_to = "Age")

Fertility$Age <- substr(Fertility$Age,1,2)

Fertility <- left_join(Fertility,SRB,
                       by = c("Region, subregion, country or area *",
                              "Period"))

Mortility <- 
  read_excel("data/WPP/WPP2019_MORT_F15_3_LIFE_TABLE_SURVIVORS_FEMALE.xlsx",
             sheet = "ESTIMATES", range = "C17:AD3587") %>% 
  filter(Type == "Country/Area") %>% select(c(1,6:28)) %>%
  pivot_longer(3:24,names_to = "Age")

Mortility$value = as.numeric(Mortility$value)/100000

# NRR
B <- left_join(Fertility,Mortility,
               by = c("Region, subregion, country or area *",
                      "Period", "Age"))

B = B %>% filter(`Region, subregion, country or area *`==c,Period %in% p)

colnames(B) <- c("Country","Year","Age","ASFR","SRB","lx")

B = B %>% mutate(ASFR= as.numeric(ASFR)*5/1000) %>% mutate(F.per=(1-as.numeric(SRB)/2.05)) %>% mutate(ASNRR = ASFR*lx*F.per)

B$Year <- substr(B$Year,1,4)

B$Year <- as.numeric(B$Year)

Sums = B %>% group_by(Year) %>% summarise(NRR = sum(ASNRR))

Year1 <- 2000

Year2 <- 2010

r_nrr <- 
  log(Sums$NRR[which(Sums$Year==Year2)]/
        Sums$NRR[which(Sums$Year==Year1)])/(Year2-Year1)

d_nrr <- 
  sqrt(Sums$NRR[which(Sums$Year==Year2)]*
         Sums$NRR[which(Sums$Year==Year1)])*r_nrr

r_tfr <- 
  log(B$ASFR[which(B$Year==Year2)]/
        B$ASFR[which(B$Year==Year1)])/
  (Year2-Year1)

r_tfr[is.na(r_tfr)] <- 0
r_tfr[is.nan(r_tfr)] <- 0
r_tfr[is.infinite(r_tfr)] <- 0

r_lx <- 
  log(B$lx[which(B$Year==Year2)]/
        B$lx[which(B$Year==Year1)])/(Year2-Year1)

r_per <- 
  log(B$F.per[which(B$Year==Year2)]/
        B$F.per[which(B$Year==Year1)])/(Year2-Year1)

m_tfr <- 
  sqrt(B$ASFR[which(B$Year==Year1)]*
         B$ASFR[which(B$Year==Year2)])

m_lx <- sqrt(B$lx[which(B$Year==Year1)]*
               B$lx[which(B$Year==Year2)])

m_per <- sqrt(B$F.per[which(B$Year==Year1)]*
                B$F.per[which(B$Year==Year2)])

d_tfr <- sum(m_tfr*m_lx*m_per*r_tfr)
d_lx <- sum(m_tfr*m_lx*m_per*r_lx)
d_per <- d_nrr-d_tfr-d_lx

table <- 
  matrix(round(
    c(Sums$NRR[Sums$Year==Year1],
      Sums$NRR[Sums$Year==Year2],
      d_nrr,d_lx,d_tfr+d_per),3),ncol = 1)

row.names(table) <- 
  c(paste0("NRR in ",Year1),
    paste0("NRR in ",Year2),
    "Annualized change of NRR",
    "Mortality Component",
    "Fertility Component")

colnames(table) <- c

kable(table,caption = "NRR decomposition")

