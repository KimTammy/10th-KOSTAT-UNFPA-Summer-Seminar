#mortality: https://mortality.org/
#fertility: https://www.humanfertility.org/

library(ggplot2)
library(knitr)
library(colorRamps)
library(RColorBrewer)
library(tidyverse)
library(data.table)
library(reshape2)
options(scipen = 100000,digits=4)


### Table Crude death rate
#DX=death
#EX=popuation

JPN_DX <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/death_japan.txt", header=T, skip=2 )
JPN_DX <- JPN_DX[JPN_DX$Year==2020,c(1:2,5)]

JPN_EX <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_japan.txt", header=T, skip=2 )
JPN_EX1 <- JPN_EX[JPN_EX$Year==2020,c(1:2,5)]
JPN_EX2 <- JPN_EX[JPN_EX$Year==2021,c(1:2,5)]

KOR_DX <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/death_korea.txt", header=T, skip=2 )
KOR_EX <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header=T, skip=2 )

KOR_DX <- KOR_DX[KOR_DX$Year==2020,c(1:2,5)]
KOR_EX1 <- KOR_EX[KOR_EX$Year==2020,c(1:2,5)]
KOR_EX2 <- KOR_EX[KOR_EX$Year==2021,c(1:2,5)]

JPN_CDR <- sum(JPN_DX$Total) / sum((JPN_EX1$Total+JPN_EX2$Total)/2)*1000
KOR_CDR <- sum(KOR_DX$Total) / sum((KOR_EX1$Total+KOR_EX2$Total)/2)*1000

CDR <- matrix(c(round(KOR_CDR,1), round(JPN_CDR,1)), ncol=2)
colnames(CDR) <- c("Korea","Japan")
row.names(CDR) <- "CDR"

kable(CDR,caption="CDR Comparison")


### Age-specific death rates
#MX=death rate

### Korea

Mx_KOR <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/mortality_korea.txt", header=T, skip=2 )

Mx_KOR <- Mx_KOR[Mx_KOR$Year==2020,]

Mx_KOR$Age[Mx_KOR$Age=="110+"] <- "110"
Mx_KOR$Age <- as.numeric(Mx_KOR$Age)

Mx_KOR$Total <- as.numeric(Mx_KOR$Total)

Mx_KOR$Male <- Mx_KOR$Female <- NULL

Mx_KOR$pop <- "Korea"

### Japan

Mx_JPN <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/mortality_japan.txt", header=T, skip=2 )

Mx_JPN <- Mx_JPN[Mx_JPN$Year==2020,]

Mx_JPN$Age[Mx_JPN$Age=="110+"] <- "110"
Mx_JPN$Age <- as.numeric(Mx_JPN$Age)

Mx_JPN$Total <- as.numeric(Mx_JPN$Total)

Mx_JPN$Male <- Mx_JPN$Female <- NULL

Mx_JPN$pop <- "Japan"

Mx <- rbind(Mx_KOR, Mx_JPN)

ggplot(Mx, aes(x=Age, y=Total, color=pop))+
  geom_line()+
  scale_y_continuous(n.breaks = 10,trans = "log10")+
  theme_bw()+
  labs(x="Age",y="Age-specific Death Rate",
       color="Country")


### Population Pyramid

### Korea

Pop_KOR <-  read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header=T, skip=2 )

Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Year1 <- 2020  
  
  Pop_KOR1 <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header=T, skip=2 )


Pop_KOR1$pctf <- Pop_KOR1$Female/sum(Pop_KOR1$Total)*100
Pop_KOR1$pctm <- Pop_KOR1$Male/sum(Pop_KOR1$Total)*100*-1

Pop_KORF <- Pop_KOR1[,c(1,2,6)]
Pop_KORF$Sex <- "Female"
names(Pop_KORF)[3] <- "percentage"

Pop_KORM <- Pop_KOR1[,c(1,2,7)]
Pop_KORM$Sex <- "Male"
names(Pop_KORM)[3] <- "percentage"

Pop_KOR1 <- rbind(Pop_KORF, Pop_KORM)

Pop_KOR1$pop <- "Korea"

### Japan

Pop_JPN <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_japan.txt", header=T, skip=2 )

Pop_JPN$Age[Pop_JPN$Age=="110+"] <- "110"
Pop_JPN$Age <- as.numeric(Pop_JPN$Age)

Year1 <- 2020  
  
  Pop_JPN1 <- Pop_JPN[Pop_JPN$Year==2020,]

  Pop_JPN1$pctf <- Pop_JPN1$Female/sum(Pop_JPN1$Total)*100
  Pop_JPN1$pctm <- Pop_JPN1$Male/sum(Pop_JPN1$Total)*100*-1

Pop_JPNF <- Pop_JPN1[,c(1,2,6)]
Pop_JPNF$Sex <- "Female"
names(Pop_JPNF)[3] <- "percentage"

Pop_JPNM <- Pop_JPN1[,c(1,2,7)]
Pop_JPNM$Sex <- "Male"
names(Pop_JPNM)[3] <- "percentage"

Pop_JPN1 <- rbind(Pop_JPNF,Pop_JPNM)

Pop_JPN1$pop <- "Japan"

Pop <- rbind(Pop_KOR1,Pop_JPN1 )

ggplot()+ 
  geom_col(data = Pop[Pop$pop=="Korea",],
           mapping = aes(x=Age, y=percentage,
                         fill = "Korea"),
           alpha = 0.5)+
  geom_col(data = Pop[Pop$pop=="Japan",],
           mapping = aes(x=Age  ,y=percentage   ,
                         fill = "Japan"),
           alpha = 0.5)+
  scale_y_continuous(labels = function(x){
    paste0(abs(x),"%")}, 
    limits = max(Pop$percentage) * c(-1.1,1.1))+
  coord_flip()+
  theme_minimal()



std <-  read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/std_pop.txt")

### Korea

Mx_KOR <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate1_korea.txt", header=T, skip=2 )

Mx_KOR <- Mx_KOR[Mx_KOR$Year==2020,]

Mx_KOR$Age[Mx_KOR$Age=="110+"] <- "110"
Mx_KOR$Age <- as.numeric(Mx_KOR$Age)

Mx_KOR$Total <- as.numeric(Mx_KOR$Total)

Mx_KOR$Male <- Mx_KOR$Female <- NULL

Mx_KOR$pop <- "Korea"

### Japan

Mx_JPN <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_japan.txt", header=T, skip=2 )
#head(Mx_JPN)
#tail(Mx_JPN)

Mx_JPN <- Mx_JPN[Mx_JPN$Year==2020,]

Mx_JPN$Age[Mx_JPN$Age=="110+"] <- "110"
Mx_JPN$Age <- as.numeric(Mx_JPN$Age)

Mx_JPN$Total <- as.numeric(Mx_JPN$Total)

Mx_JPN$Male <- Mx_JPN$Female <- NULL

Mx_JPN$pop <- "Japan"

KOR_CDR_DS <- sum(Mx_KOR$Total*std$pop)*1000

JPN_CDR_DS <- sum(Mx_JPN$Total*std$pop)*1000

CDR_DS <- 
  matrix(c(round(KOR_CDR,1),round(KOR_CDR_DS,1),
           round(JPN_CDR,1),round(JPN_CDR_DS,1)),
         ncol = 2)

colnames(CDR_DS) <- c("Korea","Japan")
row.names(CDR_DS) <- c("CDR","Standardized CDR")

kable(CDR_DS,caption = "CDR comparison with WHO standard")






### second  direct standardization
### Korea

Mx_KOR <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate1_korea.txt", header=T, skip=2 )

Mx_KOR <- Mx_KOR[Mx_KOR$Year==2020,]

Mx_KOR$Age[Mx_KOR$Age=="110+"] <- "110"
Mx_KOR$Age <- as.numeric(Mx_KOR$Age)

Mx_KOR$Total <- as.numeric(Mx_KOR$Total)

Mx_KOR$Male <- Mx_KOR$Female <- NULL

Pop_KOR <-read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header=T, skip=2 )  

Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Year1 <- 2020  
Year2 <- 2021

Pop_KOR1 <- Pop_KOR[Pop_KOR$Year==Year1,]
Pop_KOR2 <- Pop_KOR[Pop_KOR$Year==Year2,]

Pop_KOR1$Total <- Pop_KOR1$Total/sum(Pop_KOR1$Total)

Pop_KOR2$Total <- Pop_KOR2$Total/sum(Pop_KOR2$Total)

### Japan

Mx_JPN <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_japan.txt", header=T, skip=2 )

Mx_JPN <- Mx_JPN[Mx_JPN$Year==2020,]

Mx_JPN$Age[Mx_JPN$Age=="110+"] <- "110"
Mx_JPN$Age <- as.numeric(Mx_JPN$Age)

Mx_JPN$Total <- as.numeric(Mx_JPN$Total)

Mx_JPN$Male <- Mx_JPN$Female <- NULL

Pop_JPN <-read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_japan.txt", header=T, skip=2 ) 

Pop_JPN$Age[Pop_JPN$Age=="110+"] <- "110"
Pop_JPN$Age <- as.numeric(Pop_JPN$Age)

Year1 <- 2020  
Year2 <- 2021  

Pop_JPN1 <- Pop_JPN[Pop_JPN$Year==Year1,]
Pop_JPN2 <- Pop_JPN[Pop_JPN$Year==Year2,]

Pop_JPN1$Total <-   Pop_JPN1$Total/sum(Pop_JPN1$Total)

Pop_JPN2$Total <-   Pop_JPN2$Total/sum(Pop_JPN2$Total)


KOR_CDR_DS_JPN <- sum(Mx_KOR$Total * (Pop_JPN1$Total  +   Pop_JPN2$Total )/2)*1000

JPN_CDR <- sum(Mx_JPN$Total * (Pop_JPN1$Total + Pop_JPN2$Total )/2)*1000

JPN_CDR_DS_KOR <- sum(Mx_JPN$Total   * (Pop_KOR1$Total + Pop_KOR2$Total )/2)*1000

KOR_CDR <- sum(Mx_KOR$Total * (Pop_KOR1$Total + Pop_KOR2$Total )/2)*1000



CDR_table1 <- 
  matrix(c(round(KOR_CDR_DS_JPN,1),
           round(JPN_CDR,1),
           round(KOR_CDR,1),
           round(JPN_CDR_DS_KOR,1)),
         ncol = 2)

colnames(CDR_table1) <- c("JPN as Standard",
                          "KOR as Standard")
row.names(CDR_table1) <- c("Korea","Japan")

kable(CDR_table1,caption = "Comparison of Standardized CDR")



## Expected Number of Births
ASFR_KOR <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/asfr_korea.txt", header = T, skip = 2 ) 


ASFR_KOR$Age[ASFR_KOR$Age=="12-"] <- "12"
ASFR_KOR$Age[ASFR_KOR$Age=="55+"] <- "55"
ASFR_KOR$Age <- as.numeric(ASFR_KOR$Age)

ASFR_KOR <- ASFR_KOR[ASFR_KOR$Year==2019,"ASFR"]

ASFR_USA <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/asfr_usa.txt", header = T, skip = 2 ) 

ASFR_USA$Age[ASFR_USA$Age=="12-"] <- "12"
ASFR_USA$Age[ASFR_USA$Age=="55+"] <- "55"
ASFR_USA$Age <- as.numeric(ASFR_USA$Age)

ASFR_USA <- ASFR_USA[ASFR_USA$Year==2019,"ASFR"]

Pop_KOR<-read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header=T, skip=2 )  

Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Exp_KOR <- 
  (Pop_KOR[Pop_KOR$Year==2019,"Female"]+
     Pop_KOR[Pop_KOR$Year==2020,"Female"])/2

Pop_USA <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_usa.txt", header=T, skip=2 )  


Pop_USA$Age[Pop_USA$Age=="110+"] <- "110"
Pop_USA$Age <- as.numeric(Pop_USA$Age)

Exp_USA <- (Pop_USA[Pop_USA$Year==2019,"Female"]+
              Pop_USA[Pop_USA$Year==2020,"Female"])/2

ENB_KOR_NS <- sum(c(rep(0,12),ASFR_KOR,rep(0,55))*Exp_KOR )/100000

ENB_KOR_DS <- sum(c(rep(0,12),ASFR_KOR,rep(0,55))*Exp_USA )/100000

ENB_USA_NS <- sum(c(rep(0,12),ASFR_USA,rep(0,55))*Exp_KOR )/100000

ENB_USA_DS <- sum(c(rep(0,12),ASFR_USA,rep(0,55))*Exp_USA )/100000

table <- matrix(c(round(ENB_KOR_NS,1),
                  round(ENB_USA_DS,1),
                  round(ENB_KOR_DS,1),
                  round(ENB_USA_NS,1)
),
ncol = 2)

row.names(table) <- c("Korea", 
                      "USA")

colnames(table) <- c("KOR standard", "USA standard")


kable(table,caption = "ENB Comparison, Korea & USA, * 100,000")



#write.table(table, file = "C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-summer-seminar/result.csv", sep = ",", col.names = TRUE, row.names = FALSE)


#++++++++here(2023-07-31)+++++++++++++++#




### General Fertility Rate

ASFR_KOR <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/asfr_korea.txt", header = T, skip = 2 ) 


ASFR_KOR$Age[ASFR_KOR$Age=="12-"] <- "12"
ASFR_KOR$Age[ASFR_KOR$Age=="55+"] <- "55"
ASFR_KOR$Age <- as.numeric(ASFR_KOR$Age)

ASFR_KOR1 <- ASFR_KOR[ASFR_KOR$Year==2000,]
ASFR_KOR2 <- ASFR_KOR[ASFR_KOR$Year==2020,]

Ex_KOR <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_korea.txt", header = T, skip = 2 ) 

Ex_KOR$Age[Ex_KOR$Age=="12-"] <- "12"
Ex_KOR$Age[Ex_KOR$Age=="55+"] <- "55"
Ex_KOR$Age <- as.numeric(Ex_KOR$Age)

Ex_KOR1 <- Ex_KOR[Ex_KOR$Year==2000,]
Ex_KOR1$Exposure <- Ex_KOR1$Exposure/sum(Ex_KOR1$Exposure)

Ex_KOR2 <- Ex_KOR[Ex_KOR$Year==2020,]
Ex_KOR2$Exposure <- Ex_KOR2$Exposure/sum(Ex_KOR2$Exposure)

Exposure_avg <- 
  (Ex_KOR1$Exposure+Ex_KOR2$Exposure)/
  sum(Ex_KOR1$Exposure+Ex_KOR2$Exposure)

GFR1_NS <- weighted.mean(ASFR_KOR1$ASFR,Ex_KOR1$Exposure)*1000
##sum(ASFR_KOR1$ASFR*Ex_KOR1$Exposure)/sum(Ex_KOR1$Exposure)*1000

GFR1_DS <- weighted.mean(ASFR_KOR1$ASFR,Exposure_avg)*1000

GFR2_NS <- weighted.mean(ASFR_KOR2$ASFR,Ex_KOR2$Exposure)*1000

GFR2_DS <- weighted.mean(ASFR_KOR2$ASFR,Exposure_avg)*1000

table <- matrix(c(round(GFR1_NS,1),
                  round(GFR1_DS,1),
                  round(GFR2_NS,1),
                  round(GFR2_DS,1)),
                nrow = 2)

row.names(table) <- c("GFR", 
                      "Standardized GFR")

colnames(table) <- c("2000", "2020")

kable(table,caption = "GFR comparison, Korea")




###homework###


### CDR comparison, Canada vs USA
#CDR = Crude death rate
#MX: death rate

### USA

Mx_USA <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_usa.txt", header = T, skip = 2 ) 
  
  Mx_USA <- Mx_USA[Mx_USA$Year==2020,]

Mx_USA$Age[Mx_USA$Age=="110+"] <- "110"
Mx_USA$Age <- as.numeric(Mx_USA$Age)

Mx_USA$Total <- as.numeric(Mx_USA$Total)

Mx_USA$Male <- Mx_USA$Female <- NULL

Pop_USA <-
  read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_usa.txt", header = T, skip = 2 ) 

Pop_USA$Age[Pop_USA$Age=="110+"] <- "110"
Pop_USA$Age <- as.numeric(Pop_USA$Age)

Year1 <- 2020
Year2 <- 2021

Pop_USA1 <- Pop_USA[Pop_USA$Year==Year1,]
Pop_USA2 <- Pop_USA[Pop_USA$Year==Year2,]

Pop_USA1$Total <- 
  Pop_USA1$Total/sum(Pop_USA1$Total)

Pop_USA2$Total <- 
  Pop_USA2$Total/sum(Pop_USA2$Total)

Exposure1 <- (Pop_USA1$Total+Pop_USA2$Total)/2

### Canada

Mx_CAN <- read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/deathrate_canada.txt", header = T, skip = 2 ) 
 
  Mx_CAN <- Mx_CAN[Mx_CAN$Year==2020,]

Mx_CAN$Age[Mx_CAN$Age=="110+"] <- "110"
Mx_CAN$Age <- as.numeric(Mx_CAN$Age)

Mx_CAN$Total <- as.numeric(Mx_CAN$Total)

Mx_CAN$Male <- Mx_CAN$Female <- NULL

Pop_CAN <-
  read.table("C:/Users/Tammy/Documents/10th-KOSTAT-UNFPA-Seminar/Day01/population_canada.txt", header = T, skip = 2 ) 


Pop_CAN$Age[Pop_CAN$Age=="110+"] <- "110"
Pop_CAN$Age <- as.numeric(Pop_CAN$Age)

Year1 <- 2020
Year2 <- 2021

Pop_CAN1 <- Pop_CAN[Pop_CAN$Year==Year1,]
Pop_CAN2 <- Pop_CAN[Pop_CAN$Year==Year2,]

Pop_CAN1$Total <- 
  Pop_CAN1$Total/sum(Pop_CAN1$Total)

Pop_CAN2$Total <- 
  Pop_CAN2$Total/sum(Pop_CAN2$Total)

Exposure2 <- (Pop_CAN1$Total+Pop_CAN2$Total)/2

### 

USA_CDR_IS <- sum((Mx_USA$Total+Mx_CAN$Total)/2*Exposure1)*1000

USA_CDR <- sum(Mx_USA$Total*Exposure1)*1000

CAN_CDR_IS <- sum((Mx_USA$Total+Mx_CAN$Total)/2*Exposure2)*1000

CAN_CDR <- sum(Mx_CAN$Total*Exposure2)*1000

table <- matrix(c(round(USA_CDR,1),
                  round(USA_CDR_IS,1),
                  round(CAN_CDR,1),
                  round(CAN_CDR_IS,1)),
                ncol = 2)

row.names(table) <- c("CDR", 
                      "Indirect Standardized CDR")

colnames(table) <- c("USA", "Canada")

kable(table, caption = "CDR comparison, USA vs Canada")

###here

### END Comparison with indirect standard
Mx_KOR <-  
  
  Mx_KOR$Age[Mx_KOR$Age=="110+"] <- "110"
Mx_KOR$Age <- as.numeric(Mx_KOR$Age)

Mx_KOR <- Mx_KOR[Mx_KOR$Year==2020,"Total"]

Mx_USA <- 
  
  Mx_USA$Age[Mx_USA$Age=="110+"] <- "110"
Mx_USA$Age <- as.numeric(Mx_USA$Age)

Mx_USA <- Mx_USA[Mx_USA$Year==2020,"Total"]

Pop_KOR <-  
  
  Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Exp_KOR <- 
  (Pop_KOR[Pop_KOR$Year==2020,"Total"]+
     Pop_KOR[Pop_KOR$Year==2021,"Total"])/2

Pop_USA <- 
  
  Pop_USA$Age[Pop_USA$Age=="110+"] <- "110"
Pop_USA$Age <- as.numeric(Pop_USA$Age)

Exp_USA <- 
  (Pop_USA[Pop_USA$Year==2020,"Total"]+
     Pop_USA[Pop_USA$Year==2021,"Total"])/2

ENB_KOR_NS <- sum(   *    )/100000

ENB_KOR_IS <- sum(   *    )/100000

ENB_USA_NS <- sum(   *    )/100000

ENB_USA_IS <- sum(   *    )/100000

table <- matrix(c(round(ENB_KOR_NS,0),
                  round(ENB_USA_IS,0),
                  round(ENB_KOR_IS,0),
                  round(ENB_USA_NS,0)
),
ncol = 2)

row.names(table) <- c("Korea", 
                      "USA")

colnames(table) <- c("KOR standard", "USA standard")


kable(table,caption = "END Comparison with indirect standard, Korea & USA, * 100,000")





#  GFR comparison
ASFR_KOR <-
  read.table("data/Standard/KORasfrRR.txt",
             header=TRUE,fill=TRUE, skip=2) 

ASFR_KOR$Age[ASFR_KOR$Age=="12-"] <- "12"
ASFR_KOR$Age[ASFR_KOR$Age=="55+"] <- "55"
ASFR_KOR$Age <- as.numeric(ASFR_KOR$Age)

ASFR_KOR1 <- ASFR_KOR[ASFR_KOR$Year==2000,]
ASFR_KOR2 <- ASFR_KOR[ASFR_KOR$Year==2020,]

Ex_KOR <-
  read.table("data/Standard/KORexposRR.txt",
             header=TRUE,fill=TRUE, skip=2) 

Ex_KOR$Age[Ex_KOR$Age=="12-"] <- "12"
Ex_KOR$Age[Ex_KOR$Age=="55+"] <- "55"
Ex_KOR$Age <- as.numeric(Ex_KOR$Age)

Ex_KOR1 <- Ex_KOR[Ex_KOR$Year==2000,]
Ex_KOR1$Exposure <- Ex_KOR1$Exposure/sum(Ex_KOR1$Exposure)

Ex_KOR2 <- Ex_KOR[Ex_KOR$Year==2020,]
Ex_KOR2$Exposure <- Ex_KOR2$Exposure/sum(Ex_KOR2$Exposure)

CBR1_NS <- 
  
  CBR1_IS <- 
  
  CBR2_NS <- 
  
  CBR2_IS <- 
  
  table <- matrix(c(round(CBR1_NS,1),
                    round(CBR1_IS,1),
                    round(CBR2_NS,1),
                    round(CBR2_IS,1)),
                  nrow = 2)

row.names(table) <- c("GFR", 
                      "Indirect Standardized GFR")

colnames(table) <- c("2000", "2020")

kable(table, caption = "GFR comparison, Korea 2000 and 2020")






### CBR Comparison

ASFR_KOR <-  
  
  ASFR_KOR$Age[ASFR_KOR$Age=="12-"] <- "12"
ASFR_KOR$Age[ASFR_KOR$Age=="55+"] <- "55"
ASFR_KOR$Age <- as.numeric(ASFR_KOR$Age)

ASFR_KOR1 <- ASFR_KOR[ASFR_KOR$Year==2003,]
ASFR_KOR2 <- ASFR_KOR[ASFR_KOR$Year==2019,]

Pop_KOR <- 
  
  Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

totpop_KOR1 <-  
  (Pop_KOR[Pop_KOR$Year==2003,"Total"]+
     Pop_KOR[Pop_KOR$Year==2004,"Total"])/2

totpop_KOR2 <- 
  (Pop_KOR[Pop_KOR$Year==2019,"Total"]+
     Pop_KOR[Pop_KOR$Year==2020,"Total"])/2

fpop_KOR1 <-  
  (Pop_KOR[Pop_KOR$Year==2003,"Female"]+
     Pop_KOR[Pop_KOR$Year==2004,"Female"])/2

fpop_KOR2 <- 
  (Pop_KOR[Pop_KOR$Year==2019,"Female"]+
     Pop_KOR[Pop_KOR$Year==2020,"Female"])/2

Exposure1 <- fpop_KOR1/sum(totpop_KOR1)

Exposure2 <- fpop_KOR2/sum(totpop_KOR2)

Exposure_avg <- 
  (fpop_KOR1+fpop_KOR2)*0.5/
  sum((totpop_KOR1+totpop_KOR2)*0.5)

CBR1_NS <-  
  
  CBR1_DS <-  
  
  CBR2_NS <-  
  
  CBR2_DS <-  
  
  table <- matrix(c(round(CBR1_NS,1),
                    round(CBR1_DS,1),
                    round(CBR2_NS,1),
                    round(CBR2_DS,1)),
                  ncol = 2)

row.names(table) <- c("CBR", 
                      "Standard CBR")

colnames(table) <- c("2003", "2019")


kable(table,caption = "CBR Comparison, Korea")





#####  CBR Indirect S

ASFR_KOR <- 
  
  ASFR_KOR$Age[ASFR_KOR$Age=="12-"] <- "12"
ASFR_KOR$Age[ASFR_KOR$Age=="55+"] <- "55"
ASFR_KOR$Age <- as.numeric(ASFR_KOR$Age)

ASFR_KOR1 <- ASFR_KOR[ASFR_KOR$Year==2003,]
ASFR_KOR2 <- ASFR_KOR[ASFR_KOR$Year==2019,]

Pop_KOR <- 
  
  Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

totpop_KOR1 <-  
  (Pop_KOR[Pop_KOR$Year==2003,"Total"]+
     Pop_KOR[Pop_KOR$Year==2004,"Total"])/2

totpop_KOR2 <- 
  (Pop_KOR[Pop_KOR$Year==2019,"Total"]+
     Pop_KOR[Pop_KOR$Year==2020,"Total"])/2

fpop_KOR1 <-  
  (Pop_KOR[Pop_KOR$Year==2003,"Female"]+
     Pop_KOR[Pop_KOR$Year==2004,"Female"])/2

fpop_KOR2 <- 
  (Pop_KOR[Pop_KOR$Year==2019,"Female"]+
     Pop_KOR[Pop_KOR$Year==2020,"Female"])/2

Exposure1 <- fpop_KOR1/sum(totpop_KOR1)

Exposure2 <- fpop_KOR2/sum(totpop_KOR2)

CBR1_NS <-  
  
  CBR1_IS <-  
  
  CBR2_NS <-  
  
  CBR2_IS <-   
  
  table <- matrix(c(round(CBR1_NS,1),
                    round(CBR1_IS,1),
                    round(CBR2_NS,1),
                    round(CBR2_IS,1)),
                  ncol = 2)

row.names(table) <- c("No Standardization", 
                      "Indirect Standardization")

colnames(table) <- c("Year 2003", "Year 2019")

kable(table)







