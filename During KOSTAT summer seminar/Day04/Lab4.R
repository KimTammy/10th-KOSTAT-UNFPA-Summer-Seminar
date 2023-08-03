

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

### read excel files
library(readxl)


#### Fertility decomposition

data <-                                        ## fix this
  
data$edu <-                                        ## fix this
  
# We can first visualize the education composition
# change across time.

ggplot(                                       ## fix this
)+
  geom_col(width = 1)+
  scale_y_continuous(labels = function(x){paste0(x*100,"%")})+
  theme_bw()+
  labs(title ="Education-composition for cohorts of South Korean women 1930 to 1970.",x="Cohort",y="Proportion",fill="Education level")






data <-                                        ## fix this
  
ggplot(                                       ## fix this
)+
  geom_line(linewidth=1)+
  theme_bw()+
  labs(title = "Korean Complete Cohort Fertility, 1930-1970",
       x="Cohort",y="Complete Cohort Fertility",
       color = "Education level and National level",
       linetype = "Education level and National level")+
  theme(legend.position = "bottom")





Year1 <- 1940

Year2 <- 1950

data <-                                        ## fix this
  
data <-                                        ## fix this
  
data1 <- data[data$cohort==Year1,]

data2 <- data[data$cohort==Year2,]

mat1 <- as.matrix(data1[,3:7])
row.names(mat1) <- unique(data1$edu)

mat2 <- as.matrix(data2[,3:7])
row.names(mat2) <- unique(data2$edu)

mid_E <-                                        ## fix this
  
mid_F <-                                        ## fix this
  
deriv_E <-                                        ## fix this
  
deriv_F <-                                        ## fix this
  
Results_E <- deriv_E*mid_F

Results_F <- deriv_F*mid_E

Results <-  
  data.frame(Edu = rep(row.names(mat1),3),
             variable = c(rep(c("Fertility",
                              "Education-composition"),
                              each=3),
                          rep("Total",3)),
             value = c(                                       ## fix this
             ))

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

ggplot(                                       ## fix this
)+
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
  
  PxCh <-                                        ## fix this
    
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
  A4 <-                                        ## fix this
    
  
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
  A1 <-                                        ## fix this
    
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
  A1_per <-                                        ## fix this
    
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
    CALlxD[x, (38 - y + x)]  <-                          ## fix this
      
    # cumulative age-cohort difference
    CALlxDS[x, (38 - y + x)] <-                          ## fix this
      
  }
}

options(scipen = 10)

par(cex.axis = 1)
par(oma = c(1, 0, 0, 0))
filled.contour(                                       ## fix this
  , 
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
filled.contour(                                       ## fix this
  , 
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







############### NRR decomposition

# data cleaning ----

Birth <-                                        ## fix this
  
Birth$Age[Birth$Age=="12-"] <- "12"
Birth$Age[Birth$Age=="55+"] <- "55"

Birth$Age <- as.numeric(Birth$Age)

Pop <-                                        ## fix this
  
Birth <- left_join(Birth,Pop)

Tot.birth =                                        ## fix this
  
Tot.birth = Tot.birth %>% mutate(F.per = Female/Total)

lx =                                        ## fix this
  
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

ggplot(                                       ## fix this
)+
  geom_line()+
  scale_x_continuous(n.breaks = 10)+
  geom_vline(xintercept = c(1970,1975),linetype=3)+
  theme_bw()+
  labs(x="Year",y="NRR",title = "Time trend in the Net-Reproductive Rate, France 1946-2020")




# main decomposition ----

Year1 <- 1970

Year2 <- 1975

r_nrr <-                   ## fix this
  

d_nrr <-                    ## fix this
  
  
r_tfr <-                             ## fix this
  

r_tfr[is.na(r_tfr)] <- 0
r_tfr[is.nan(r_tfr)] <- 0
r_tfr[is.infinite(r_tfr)] <- 0
  
r_lx <-                                        ## fix this
  

r_per <-                                        ## fix this
  
  
m_tfr <-                                        ## fix this
  

m_lx <-                                        ## fix this
  

m_per <-                                        ## fix this
  
  
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






#######################

####### Nigeria at the end of the booklet

######################



c="Nigeria"

p = c("1980-1985","1985-1990","1990-1995",
      "1995-2000","2000-2005","2005-2010",
      "2010-2015","2015-2020")

yeard = 5

# data cleaning ----
SRB <-                                         ## fix this
  
  

Fertility <-                                         ## fix this
  
  

Fertility$Age <- substr(Fertility$Age,1,2)

Fertility <- left_join(Fertility,SRB,
                       by = c("Region, subregion, country or area *",
                              "Period"))

Mortility <-                                         ## fix this
  
  

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

r_nrr <-                                         ## fix this
  
  

d_nrr <-                                         ## fix this
  
  

r_tfr <-                                         ## fix this
  
  

r_tfr[is.na(r_tfr)] <- 0
r_tfr[is.nan(r_tfr)] <- 0
r_tfr[is.infinite(r_tfr)] <- 0

r_lx <-                                         ## fix this
  
  

r_per <-                                         ## fix this
  
  

m_tfr <-                                         ## fix this
  
  

m_lx <-                                        ## fix this
  
  

m_per <-                                         ## fix this
  
  

d_tfr <-                                         ## fix this
  
  
d_lx <-                                         ## fix this
  
  
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


