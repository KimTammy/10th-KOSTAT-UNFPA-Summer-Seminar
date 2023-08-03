

# We will be using extensively these packages
# So, please install them on your computer beforehand

# Making beautiful figures
library(ggplot2)

# Making tables
library(knitr)
# the package "knitr" does not have the option
# to add notes under the table. In this document
# the notes are added with LaTex
# Alternatively, you can use the add_footnote() 
# function from the "kableExtra" package to add the notes.

# Make the color palette pretty
library(colorRamps)
library(RColorBrewer)

# data manipulation package
# contains many small useful 
# packages such as "magrittr" 
# and "dplyr".
library(tidyverse)

# reshaping the data.frame and data manipulation
# e.g. from wide to long and from long to wide
library(data.table)

# Similar package to reshape data.frame
library(reshape2)

# This is to regulate the results regarding
# the rounding and the digits we will be working
# with. 
options(scipen = 100000,digits=4)
 

### Table CDR

JPN_Dx <-   read.table("  ",header = ,skip = )

JPN_Dx <- JPN_Dx[JPN_Dx$Year==    ,  ]

JPN_Ex <- read.table("  ",header = ,skip = )

JPN_Ex1 <- JPN_Ex[JPN_Ex$Year==   ,   ]
JPN_Ex2 <- JPN_Ex[JPN_Ex$Year==   ,   ]

KOR_Dx <- read.table("  ",header = ,skip = )

KOR_Dx <- KOR_Dx[KOR_Dx$Year==   ,   ]

KOR_Ex <- read.table("  ",header = ,skip = )

KOR_Ex1 <- KOR_Ex[KOR_Ex$Year==   ,   ]
KOR_Ex2 <- KOR_Ex[KOR_Ex$Year==   ,   ]

JPN_CDR <- sum(   ) /  sum((  +   )/2)*1000

KOR_CDR <- sum(   ) /  sum((  +   )/2)*1000

CDR <- matrix(c(round(KOR_CDR,1),round(JPN_CDR,1)),ncol = 2)

colnames(CDR) <- c("Korea","Japan")
row.names(CDR) <- "CDR"

kable(CDR,caption="CDR Comparison")




### Age-specific death rates

### Korea

Mx_KOR <- read.table("  ",header = ,skip = )

Mx_KOR <- Mx_KOR[Mx_KOR$Year==,]

Mx_KOR$Age[Mx_KOR$Age=="110+"] <- ""
Mx_KOR$Age <- as.numeric(   )

Mx_KOR$Total <- as.numeric(Mx_KOR$Total)

Mx_KOR$Male <- Mx_KOR$Female <- NULL

Mx_KOR$pop <- "Korea"

### Japan

Mx_JPN <- read.table("  ",header = ,skip = )

Mx_JPN <- Mx_JPN[Mx_JPN$Year==  ,]

Mx_JPN$Age[Mx_JPN$Age=="110+"] <- " "
Mx_JPN$Age <- as.numeric(Mx_JPN$Age)

Mx_JPN$Total <- as.numeric(Mx_JPN$Total)

Mx_JPN$Male <- Mx_JPN$Female <- NULL

Mx_JPN$pop <- " "

Mx <- rbind(  ,  )

ggplot(   ,aes(x=   ,y=   ,color=   ))+
  geom_line()+
  scale_y_continuous(n.breaks = 10,trans = "log10")+
  theme_bw()+
  labs(x="Age",y="Age-specific Death Rate",
       color="Country")




### Population Pyramid

### Korea

Pop_KOR <-  read.table("  ",header = ,skip = )

Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "  "
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Year1 <-  

Pop_KOR1 <- Pop_KOR[Pop_KOR$Year==  ,]

Pop_KOR1$pctf <- Pop_KOR1$  /sum(Pop_KOR1$  )*100
Pop_KOR1$pctm <- Pop_KOR1$  /sum(Pop_KOR1$  )*100*-1

Pop_KORF <- Pop_KOR1[,c(1,2,   )]
Pop_KORF$Sex <- "   "
names(Pop_KORF)[3] <- "percentage"

Pop_KORM <- Pop_KOR1[,c(1,2,   )]
Pop_KORM$Sex <- "Male"
names(Pop_KORM)[3] <- "percentage"

Pop_KOR1 <- rbind( , )

Pop_KOR1$pop <- "  "

### Japan

Pop_JPN <- read.table("  ",header = ,skip = ) 

Pop_JPN$Age[Pop_JPN$Age=="110+"] <- "  "
Pop_JPN$Age <- as.numeric(Pop_JPN$Age)

Year1 <-  

Pop_JPN1 <- Pop_JPN[Pop_JPN$Year==  ,]

Pop_JPN1$pctf <- Pop_JPN1$  /sum(Pop_JPN1$   )*100
Pop_JPN1$pctm <- Pop_JPN1$  /sum(Pop_JPN1$   )*100*-1

Pop_JPNF <- Pop_JPN1[,c(1,2, )]
Pop_JPNF$Sex <- "  "
names(Pop_JPNF)[3] <- "percentage"

Pop_JPNM <- Pop_JPN1[,c(1,2,)]
Pop_JPNM$Sex <- "  "
names(Pop_JPNM)[3] <- "percentage"

Pop_JPN1 <- rbind( , )

Pop_JPN1$pop <- "  "

Pop <- rbind(   ,   )

ggplot()+ 
  geom_col(data = Pop[Pop$pop=="  ",],
           mapping = aes(x=  ,y=   ,
                         fill = "   "),
           alpha = 0.5)+
  geom_col(data = Pop[Pop$pop=="  ",],
           mapping = aes(x=  ,y=   ,
                         fill = "  "),
           alpha = 0.5)+
  scale_y_continuous(labels = function(x){
    paste0(abs(x),"%")}, 
    limits = max(Pop$percentage) * c(-1.1,1.1))+
  coord_flip()+
  theme_minimal()



### first direct standardization

std <- read.table("std_pop.txt")

### Korea

Mx_KOR <- read.table("  ",header = ,skip = ) 

Mx_KOR <- Mx_KOR[Mx_KOR$Year==   ,]

Mx_KOR$Age[Mx_KOR$Age=="110+"] <- "   "
Mx_KOR$Age <- as.numeric(Mx_KOR$Age)

Mx_KOR$Total <- as.numeric(Mx_KOR$Total)

Mx_KOR$Male <- Mx_KOR$Female <- NULL

Mx_KOR$pop <- " "

### Japan

Mx_JPN <- read.table("  ",header = ,skip = ) 

Mx_JPN <- Mx_JPN[Mx_JPN$Year==  ,]

Mx_JPN$Age[Mx_JPN$Age=="110+"] <- "  "
Mx_JPN$Age <- as.numeric(Mx_JPN$Age)

Mx_JPN$Total <- as.numeric(Mx_JPN$Total)

Mx_JPN$Male <- Mx_JPN$Female <- NULL

Mx_JPN$pop <- "Japan"

KOR_CDR_DS <- sum(Mx_KOR$  *  std$ )*1000

JPN_CDR_DS <- sum(Mx_JPN$  *  std$ )*1000

CDR_DS <- 
  matrix(c(round(KOR_CDR,1),round(KOR_CDR_DS,1),
           round(JPN_CDR,1),round(JPN_CDR_DS,1)),
         ncol = 2)

colnames(CDR_DS) <- c("Korea","Japan")
row.names(CDR_DS) <- c("CDR","Standardized CDR")

kable(CDR_DS,caption = "CDR comparison with WHO standard")






### second  direct standardization
### Korea

Mx_KOR <- read.table("  ",header = ,skip = ) 

Mx_KOR <- Mx_KOR[Mx_KOR$Year==  ,]

Mx_KOR$Age[Mx_KOR$Age=="110+"] <- "  "
Mx_KOR$Age <- as.numeric(Mx_KOR$  )

Mx_KOR$Total <- as.numeric(Mx_KOR$Total)

Mx_KOR$Male <- Mx_KOR$Female <- NULL

Pop_KOR <-read.table("  ",header = ,skip = )   

Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Year1 <-  
Year2 <- 

Pop_KOR1 <- Pop_KOR[Pop_KOR$Year==Year1,]
Pop_KOR2 <- Pop_KOR[Pop_KOR$Year==Year2,]

Pop_KOR1$Total <- Pop_KOR1$Total/sum(Pop_KOR1$Total)

Pop_KOR2$Total <- Pop_KOR2$Total/sum(Pop_KOR2$Total)

### Japan

Mx_JPN <- read.table("  ",header = ,skip = ) 

Mx_JPN <- Mx_JPN[Mx_JPN$Year==  ,]

Mx_JPN$Age[Mx_JPN$Age=="110+"] <- "  "
Mx_JPN$Age <- as.numeric(Mx_JPN$Age)

Mx_JPN$Total <- as.numeric(Mx_JPN$Total)

Mx_JPN$Male <- Mx_JPN$Female <- NULL

Pop_JPN <- read.table("  ",header = ,skip = ) 

Pop_JPN$Age[Pop_JPN$Age=="110+"] <- "  "
Pop_JPN$Age <- as.numeric(Pop_JPN$Age)

Year1 <-  
Year2 <-  

Pop_JPN1 <- Pop_JPN[Pop_JPN$Year==Year1,]
Pop_JPN2 <- Pop_JPN[Pop_JPN$Year==Year2,]

Pop_JPN1$Total <-   Pop_JPN1$Total/sum(Pop_JPN1$Total)

Pop_JPN2$Total <-   Pop_JPN2$Total/sum(Pop_JPN2$Total)

### 

KOR_CDR_DS_JPN <- sum(Mx_KOR$   * (Pop_JPN1$    +   Pop_JPN2$   )/2)*1000

JPN_CDR <- sum(Mx_JPN$ * (Pop_JPN1$ + Pop_JPN2$ )/2)*1000

JPN_CDR_DS_KOR <- sum(Mx_JPN$   * (Pop_KOR1$ + Pop_KOR2$ )/2)*1000

KOR_CDR <- sum(Mx_KOR$ * (Pop_KOR1$ + Pop_KOR2$ )/2)*1000



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
ASFR_KOR <-  
  

ASFR_KOR$Age[ASFR_KOR$Age=="12-"] <- "12"
ASFR_KOR$Age[ASFR_KOR$Age=="55+"] <- "55"
ASFR_KOR$Age <- as.numeric(ASFR_KOR$Age)

ASFR_KOR <- ASFR_KOR[ASFR_KOR$Year==2019,"ASFR"]

ASFR_USA <-  

ASFR_USA$Age[ASFR_USA$Age=="12-"] <- "12"
ASFR_USA$Age[ASFR_USA$Age=="55+"] <- "55"
ASFR_USA$Age <- as.numeric(ASFR_USA$Age)

ASFR_USA <- ASFR_USA[ASFR_USA$Year==2019,"ASFR"]

Pop_KOR <- 

Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Exp_KOR <- 
  (Pop_KOR[Pop_KOR$Year==2019,"Female"]+
     Pop_KOR[Pop_KOR$Year==2020,"Female"])/2

Pop_USA <- 
  

Pop_USA$Age[Pop_USA$Age=="110+"] <- "110"
Pop_USA$Age <- as.numeric(Pop_USA$Age)

Exp_USA <- (Pop_USA[Pop_USA$Year==2019,"Female"]+
            Pop_USA[Pop_USA$Year==2020,"Female"])/2

ENB_KOR_NS <- sum(c(rep(0,12),ASFR_KOR,rep(0,55))*Exp_ )/100000

ENB_KOR_DS <- sum(c(rep(0,12),ASFR_KOR,rep(0,55))*Exp_ )/100000

ENB_USA_NS <- sum(c(rep(0,12),ASFR_USA,rep(0,55))*Exp_ )/100000

ENB_USA_DS <- sum(c(rep(0,12),ASFR_USA,rep(0,55))*Exp_ )/100000

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







### General Fertility Rate

ASFR_KOR <- 
  

ASFR_KOR$Age[ASFR_KOR$Age=="12-"] <- "12"
ASFR_KOR$Age[ASFR_KOR$Age=="55+"] <- "55"
ASFR_KOR$Age <- as.numeric(ASFR_KOR$Age)

ASFR_KOR1 <- ASFR_KOR[ASFR_KOR$Year==2000,]
ASFR_KOR2 <- ASFR_KOR[ASFR_KOR$Year==2020,]

Ex_KOR <- 

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





### CDR comparison, Korea vs Japan

### Korea

Mx_KOR <- 

Mx_KOR <- Mx_KOR[Mx_KOR$Year==2020,]

Mx_KOR$Age[Mx_KOR$Age=="110+"] <- "110"
Mx_KOR$Age <- as.numeric(Mx_KOR$Age)

Mx_KOR$Total <- as.numeric(Mx_KOR$Total)

Mx_KOR$Male <- Mx_KOR$Female <- NULL

Pop_KOR <-
  read.table("data/Standard/KOR.Population.txt",
                   header=TRUE,fill=TRUE, skip=2) 

Pop_KOR$Age[Pop_KOR$Age=="110+"] <- "110"
Pop_KOR$Age <- as.numeric(Pop_KOR$Age)

Year1 <- 2020
Year2 <- 2021

Pop_KOR1 <- Pop_KOR[Pop_KOR$Year==Year1,]
Pop_KOR2 <- Pop_KOR[Pop_KOR$Year==Year2,]

Pop_KOR1$Total <- 
  Pop_KOR1$Total/sum(Pop_KOR1$Total)

Pop_KOR2$Total <- 
  Pop_KOR2$Total/sum(Pop_KOR2$Total)

Exposure1 <- (Pop_KOR1$Total+Pop_KOR2$Total)/2

### Japan

Mx_JPN <-  

Mx_JPN <- Mx_JPN[Mx_JPN$Year==2020,]

Mx_JPN$Age[Mx_JPN$Age=="110+"] <- "110"
Mx_JPN$Age <- as.numeric(Mx_JPN$Age)

Mx_JPN$Total <- as.numeric(Mx_JPN$Total)

Mx_JPN$Male <- Mx_JPN$Female <- NULL

Pop_JPN <-
  read.table("data/Standard/JPN.Population.txt",
                   header=TRUE,fill=TRUE, skip=2) 

Pop_JPN$Age[Pop_JPN$Age=="110+"] <- "110"
Pop_JPN$Age <- as.numeric(Pop_JPN$Age)

Year1 <- 2020
Year2 <- 2021

Pop_JPN1 <- Pop_JPN[Pop_JPN$Year==Year1,]
Pop_JPN2 <- Pop_JPN[Pop_JPN$Year==Year2,]

Pop_JPN1$Total <- 
  Pop_JPN1$Total/sum(Pop_JPN1$Total)

Pop_JPN2$Total <- 
  Pop_JPN2$Total/sum(Pop_JPN2$Total)

Exposure2 <- (Pop_JPN1$Total+Pop_JPN2$Total)/2

### 

KOR_CDR_IS <- sum((   )/2*Exposure1)*1000

KOR_CDR <- sum(Mx_KOR$Total*Exposure1)*1000

JPN_CDR_IS <- sum((   )/2*Exposure2)*1000

JPN_CDR <- sum(Mx_JPN$Total*Exposure2)*1000

table <- matrix(c(round(KOR_CDR,1),
                  round(KOR_CDR_IS,1),
                  round(JPN_CDR,1),
                  round(JPN_CDR_IS,1)),
                ncol = 2)

row.names(table) <- c("CDR", 
                      "Indirect Standardized CDR")

colnames(table) <- c("Korea", "Japan")

kable(table, caption = "CDR comparison, Korea vs Japan")





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







