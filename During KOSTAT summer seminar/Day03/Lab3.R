
library(ggplot2)
library(knitr)
library(colorRamps)
library(RColorBrewer)
library(tidyverse)
library(data.table)
library(reshape2)
options(scipen = 100000,digits=4)

### read excel files
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
          
  qx<-                                        ## fix this
    
  # Chiang's conversion from age-specific mortality
  # to age-specific probability of death
  
  qx[N] <-                                        ## fix this
    
    
  # Everyone dies in a life table eventually
  # with the last value of qx = 1
  
  px<-1-qx
  
  lx<-                                       ## fix this
    
  
  for(y in 1:(N-1)){
    lx[y+1]<-lx[y]*px[y]
  }
  lx <- ifelse(lx<0,0,lx)
  
  dx<-                                       ## fix this
    
  # Calculating the death distribution of the 
  # life table
  
  Lx<-lx[-1]+ax[-N]*dx[-N] 
  Lx[N]<-ifelse(mx[N]>0,lx[N]/mx[N],0)                  
  # Person-year lived within each age interval.
  
  Tx<-c() 
  for(y in 1:N){
    Tx[y]<-sum(Lx[y:N]) 
  }
  
  ex<-                                       ## fix this
    
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
    delta[i]    <-                             ## fix this
      
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




USA_Mx <-                                        ## fix this
  

KOR_Mx <-                                        ## fix this
  
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
  c(                                       ## fix this
  
    ),
  1),ncol = 1)

row.names(table) <- 
  c("Life expectancy at birth for Korea",
            "Life expectancy at birth for USA",
            "Life expectancy difference betwen Korea and USA",
            "Estimated difference from decompositon")

colnames(table) <- "Arriaga"

kable(table,caption = "Female life expectancy gap, Korea - USA, 2019 ")





ggplot()+
  geom_col(                                       ## fix this 
    )+
  scale_x_continuous(breaks = seq(0,110,10))+
  theme_minimal()+
  labs(title = "Age-decomposition of the difference in female life expectancy \n between Korea and United States, 2019",
       x="Age",y="Contributions")







### Life expectancy decomposition
### Female life expectancy gap, Korea - USA

USA_Mx <-                                        ## fix this
  

KOR_Mx <-                                        ## fix this
  
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
  c(                                       ## fix this
  ),ncol = 1)

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

ggplot(                                       ## fix this
)+
  geom_col()+
  scale_x_continuous(breaks = seq(0,110,10))+
  theme_minimal()+
  labs(title = "Age-decomposition of the difference in female life expectancy \n between Korea and United States, 2019",
       x="Age",y="Contributions")






## Age- & Cause- decomposition of life expectancy
USA_Mx <-                                        ## fix this
  
Mx1 <-                                        ## fix this
  
Mx2 <-                                        ## fix this
  
ex_diff <- 
  life.table(Mx2,"f")$ex[1]-
  life.table(Mx1,"f")$ex[1]

ex_decomp <- 
arriaga(as.numeric(Mx1),
        as.numeric(Mx2),
        sex = "f",
        breakdown = F)

cause_count <-                                        ## fix this
  
cause_count1 <-                                        ## fix this
  
cause_count1 <- matrix(c(cause_count1$Count),
                      ncol = length(unique(cause_count1$Cause)),
                      nrow = length(unique(cause_count1$Age)))

row.names(cause_count1) <- 0:110

colnames(cause_count1) <- 1:16

cause_total1 <- rowSums(cause_count1)

cause_prop1 <-                                        ## fix this
  
cause_count2 <-                                        ## fix this
  
cause_count2 <- matrix(c(cause_count2$Count),
                      ncol = length(unique(cause_count2$Cause)),
                      nrow = length(unique(cause_count2$Age)))
# Each row is an age and each column is a cause.

row.names(cause_count2) <- 0:110

colnames(cause_count2) <- 1:16

cause_total2 <- rowSums(cause_count2)

cause_prop2 <-                                        ## fix this
  
## Now we construct the cause-specific factor.

cause_fac1 <-                                        ## fix this
  
cause_fac2 <-                                        ## fix this
  
# sum(cause_fac2-cause_fac1) 
# This get you the same results as the Arriaga method by age

cause_mat <- cause_fac2 - cause_fac1

table <- matrix(round(
  c(                                       ## fix this
  ),2))

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
  "Other causes component",
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
  geom_col(                                       ## fix this
  )+
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



ggplot()+
  geom_col(aes(x=0:110,y=ex_diff),fill="blue")+
  scale_x_continuous(breaks = seq(0,110,10))+
  theme_bw()+
  labs(title = "Age-decomposition of the change in American male  \n  life expectancy between 2014 and 2019",
       x="Age",y="Contributions")







########## life expectancy decomposition continuous
Y1 <- 1950
Y2 <- 1955

# The period of time, this is to calculate annualized change 
tp <- Y2-Y1

# For life table, I think they used the both sex one from Sweden so this is the one (Sweden) from HMD(2023), the results might be different due to different treatments at different ages to mortality statistics across 20 years by the HMD team.

LT1 <-                                        ## fix this
  
LT1 <- LT1[LT1$Year==Y1,]

LT2 <-                                        ## fix this
  
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



ex <-                                        ## fix this
  

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

rho <-                                        ## fix this
  

fx <-                                        ## fix this
  

main <- sum(rho*fx)*edag[1]
# direct component, as equation shows is the product
# between average improvements in mortality and
# life disparity or e-dagger at birth.

error <-                                        ## fix this
  

gap2 <- main+error

table <- 
  matrix(round(c(                                       ## fix this
  ),3),ncol=1)

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
  geom_line(                                       ## fix this
  ))+
  geom_smooth(                                       ## fix this
    ,se=F,linetype=2,show.legend = F)+
  geom_line(                                       ## fix this
  )+
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
    
    pxCH<-                                       ## fix this
      
 	
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
    pxCH<-                                       ## fix this
      

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





LT1 <-                                        ## fix this
  

Mx1 <- matrix(c(1-LT1$qx),nrow = 111)

row.names(Mx1) <- 0:110
colnames(Mx1) <- unique(LT1$Year)

LT2 <-                                        ## fix this
  

Mx2 <- matrix(c(1-LT2$qx),nrow = 111)

row.names(Mx2) <- 0:110
colnames(Mx2) <- unique(LT2$Year)

Y <- 2020

Y1 <- 1909
# This is the year where we start doing the
# decomposition minus one. 

result <-                                        ## fix this
  
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
			CALlxD[x,(111-y+x)]<-                                  ## fix this
			  
			CALlxDS[x,(111-y+x)]<-                                 ## fix this
			  
		}
}

par(cex.axis=1.1)
filled.contour(                                       ## fix this
  ,
               key.axes=customAxis(),ylab="Age-contribution",
               xlab="Year",cex.lab=1.2)
title("CAL-difference between \n Italy and France,2020",
      adj=0.3)
mtext("Italy",1,0.5,adj=.9,cex=1.1)
mtext("France",3,0.5,adj=.9,cex=1.1)

par(cex.axis=1.1)


filled.contour(                                       ## fix this
  ,
               key.axes=customAxis(),ylab="Age-contribution",
               xlab="Year",cex.lab=1.2)
title("CAL-difference between Italy and \n France, cumulative changes, 2020",adj=0.1)
mtext("Italy",1,0.5,adj=.9,cex=1.1)
mtext("France",3,0.5,adj=.9,cex=1.1)























