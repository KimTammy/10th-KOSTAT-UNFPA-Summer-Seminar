
###
### Data cleaning for CCF decomposition
###

library(tidyverse)

data <- read.csv("data/FertDecomp/South Korea_Census 2010.csv")

data <- data[data$cohort!="Total",]

data$edu <- factor(data$edu,
                   levels = c("ISCED0-2A","ISCED3C-A",
                              "ISCED5B-6"),
                   labels = c("Low","Medium","High"))

Prop_edu <- data %>% 
  as.data.frame() %>% 
  mutate(Total = 
           parity_0 + parity_1 + parity_2 + 
           parity_3 + parity_4 + parity_5 + 
           parity_6 + parity_7 + parity_8p) %>% 
  select(cohort, edu, Total) %>% 
  group_by(cohort, edu) %>% 
  mutate(T_edu = sum(Total)) %>% 
  ungroup(cohort, edu) %>% 
  group_by(cohort) %>% 
  mutate(T_bc = sum(Total)) %>% 
  ungroup(cohort) %>% 
  mutate(Prop_edu = T_edu / T_bc,
         edu = factor(edu, levels = c("High", "Medium", "Low")))

write.csv(Prop_edu,"data/FertDecomp/data_base_prop.csv")

data_edu <- data %>% 
  as.data.frame() %>% 
  mutate(We = parity_0 + parity_1 + parity_2 + parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B0 = We,
         B1 = parity_1 + parity_2 + parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B2 = parity_2 + parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B3 = parity_3 + parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B4 = parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B5 = parity_5 + parity_6 + parity_7 + parity_8p,
         B6 = parity_6 + parity_7 + parity_8p,
         B7 = parity_7 + parity_8p,
         B8 = parity_8p,
         B3p = B3 + B4 + B5 + B6 + B7 + B8,
         B  = B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8) %>% 
  select(cohort, edu, We, B0, B1, B2, B3, B4, B5, B6, B7, B8, B3p, B)

data_edu_all <- data_edu %>% 
  group_by(cohort) %>% 
  summarise(W = sum(We)) %>% 
  #ungroup(cohort) %>% 
  left_join(data_edu, by = "cohort") %>%
  filter(cohort %in% seq(1940, 1970, 5)) %>%
  mutate(E = We / W,
         F1e = B1 / We,
         F2e = B2 / We,
         F3e = B3 / We,
         F4e = B4 / We,
         F5e = B5 / We,
         F6e = B6 / We,
         F7e = B7 / We,
         F8e = B8 / We,
         Fe = 
           F1e+F2e+F3e+F4e+
           F5e+F6e+F7e+F8e)

data_edu_all <- data_edu_all[,c("cohort","edu",
                                "W","We","B",
                                "E","Fe")]

write.csv(data_edu_all,"data/FertDecomp/data_base_decomp.csv")

data_edu <- data %>% 
  as.data.frame() %>% 
  mutate(W = 
           parity_0 + parity_1 + parity_2 + parity_3 + parity_4 + 
           parity_5 + parity_6 + parity_7 + parity_8p,
         B0 = W,
         B1 = 
           parity_1 + parity_2 + parity_3 + parity_4 + 
           parity_5 + parity_6 + parity_7 + parity_8p,
         B2 = 
           parity_2 + parity_3 + parity_4 + parity_5 + 
           parity_6 + parity_7 + parity_8p,
         B3 = 
           parity_3 + parity_4 + parity_5 + parity_6 + 
           parity_7 + parity_8p,
         B4 = parity_4 + parity_5 + parity_6 + parity_7 + parity_8p,
         B5 = parity_5 + parity_6 + parity_7 + parity_8p,
         B6 = parity_6 + parity_7 + parity_8p,
         B7 = parity_7 + parity_8p,
         B8 = parity_8p,
         B3p = B3 + B4 + B5 + B6 + B7 + B8,
         B  = 
           B1 + B2 + B3 + B4 + 
           B5 + B6 + B7 + B8) %>% 
  select(cohort, edu, W, B0, B1, B2, B3p, B)

data_all <- data_edu %>% 
  group_by(cohort) %>% 
  summarise(W = sum(W),
            B0 = sum(B0),
            B1 = sum(B1),
            B2 = sum(B2),
            B3p = sum(B3p),
            B = sum(B)) %>% 
  # ungroup(cohort) %>% 
  mutate(edu = "All") %>% 
  bind_rows(data_edu) %>% 
  mutate(CCF = B / W,
         edu = factor(edu, levels = c("All", "High", "Medium", "Low")))

write.csv(data_all,"data/FertDecomp/data_base_fert.csv")
