library(ggplot2)
library(knitr)
library(colorRamps)
library(RColorBrewer)
library(tidyverse)
library(data.table)
library(reshape2)
options(scipen = 100000,digits=4)
library(readxl)

############### NRR decomposition

# data cleaning ----

Birth <- read.table("C:/Users/Tammy/Documents/During KOSTAT summer seminar/birthcount_usa.txt", header = TRUE, fill = TRUE, skip = 2)                                        ## fix this

Birth$Age[Birth$Age=="12-"] <- "12"
Birth$Age[Birth$Age=="55+"] <- "55"


Birth$Age <- as.numeric(Birth$Age)

Pop <- read.table("C:/Users/Tammy/Documents/During KOSTAT summer seminar/femaleexposure_usa.txt", header = TRUE, fill = TRUE, skip = 2)                                         ## fix this

Birth <- left_join(Birth,Pop)

Tot.birth = read.table("C:/Users/Tammy/Documents/During KOSTAT summer seminar/birth_usa.txt", header = TRUE, fill = TRUE, skip = 2)

Tot.birth = Tot.birth %>% mutate(F.per = Female/Total)

lx = read.table("C:/Users/Tammy/Documents/During KOSTAT summer seminar/lifetable_usa.txt", header = TRUE, fill = TRUE, skip = 1)[,c(1,2,6)]
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
  geom_vline(xintercept = c(1945,1957),linetype=4)+
  theme_bw()+
  labs(x="Year",y="NRR",title = "Time trend in the Net-Reproductive Rate, USA 1933-2021")




# main decomposition ----

Year1 <- 1945

Year2 <- 1957

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


m_lx <- 
  sqrt(Birth.C$lx[which(Birth.C$Year==Year1)]*
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

colnames(table) <- "USA"

kable(table,caption = "NRR decomposition")


### Decrease### --> why failed?

Year3 <- 1960
Year4 <- 1975

r_nrr <-
  log(NRR$NRR[which(NRR$Year==Year4)]/
        NRR$NRR[which(NRR$Year==Year3)])/(Year4-Year3)

d_nrr <-
  sqrt(NRR$NRR[which(NRR$Year==Year4)]*
         NRR$NRR[which(NRR$Year==Year3)])*r_nrr

r_tfr <- 
  log(Birth.C$ASFR[which(Birth.C$Year==Year4)]/
        Birth.C$ASFR[which(Birth.C$Year==Year3)])/(Year4-Year3)

r_tfr[is.na(r_tfr)] <- 0
r_tfr[is.nan(r_tfr)] <- 0
r_tfr[is.infinite(r_tfr)] <- 0

r_lx <- 
  log(Birth.C$lx[which(Birth.C$Year==Year4)]/
        Birth.C$lx[which(Birth.C$Year==Year3)])/(Year4-Year3)

r_per <- 
  log(Birth.C$F.per[which(Birth.C$Year==Year4)]/
        Birth.C$F.per[which(Birth.C$Year==Year3)])/(Year4-Year3)

m_tfr <- 
  sqrt(Birth.C$ASFR[which(Birth.C$Year==Year3)]*
         Birth.C$ASFR[which(Birth.C$Year==Year4)])

m_lx <- 
  sqrt(Birth.C$lx[which(Birth.C$Year==Year3)]*
         Birth.C$lx[which(Birth.C$Year==Year4)])

m_per <- sqrt(Birth.C$F.per[which(Birth.C$Year==Year3)]*
                Birth.C$F.per[which(Birth.C$Year==Year4)])

d_tfr <- sum(m_tfr*m_lx*m_per*r_tfr)
d_lx <- sum(m_tfr*m_lx*m_per*r_lx)
d_per <- d_nrr-d_tfr-d_lx

table <- 
  matrix(round(
    c(NRR[NRR$Year==Year3,]$NRR,
      NRR[NRR$Year==Year4,]$NRR,
      d_nrr,d_lx,d_tfr+d_per),3), ncol = 3)

# Fix the row names here (make sure the length is equal to the number of rows)
row.names(table) <- 
  c(paste0("NRR in ",Year3),
    paste0("NRR in ",Year4),
    "Annualized change of NRR",
    "Mortality Component",
    "Fertility Component")


colnames(table) <- "USA"

kable(table,caption = "NRR decomposition")

