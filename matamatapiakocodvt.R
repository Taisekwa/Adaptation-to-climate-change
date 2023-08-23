df<-read.csv("C:/Users/chikazhet/deep_southcodvt.csv")

# All mitigations plotted
deep_south<- df
#MyData$nlosschange <- is.numeric(MyData$nlosschange)
library(tidyverse)
library(ggplot2)
library("reshape2")
library("ggplot2")
library(KernSmooth)
library(dplyr)
library(stringr)

##Test plot

ggplot()+        
  geom_point(data=deep_south, mapping=aes(y = kgDM_ha, x = month, colour=option)) +
  scale_x_discrete(limits = c("Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec", "Jan", "Feb","Mar", "Apr", "May"))

#RCP4.5
# filter mid-century optimistic
temp0 <- deep_south %>% 
  select(dacade, pathway, option, kgDM_ha, month )%>%
filter(dacade %in% c("2010_2020", "2040_2050" )) %>% filter(option %in% c("demand" , "pgr_optimistic")) %>% filter(pathway %in% c("kurrent","rcp4.5"))
  
ggplot()+
geom_point(data=temp0, mapping=aes(y = kgDM_ha, x = month, colour=option))+
  scale_x_discrete(limits = c("Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec", "Jan", "Feb","Mar", "Apr", "May"))

# filter mid century less optimistic
temp1 <- deep_south %>% 
  select(dacade, pathway, option, kgDM_ha, month )%>%
  filter(dacade %in% c("2010_2020", "2040_2050" )) %>% filter(option %in% c("demand" , "pgr_less_optimistic")) %>% filter(pathway %in% c("kurrent","rcp4.5"))

# filter late century optimistic
temp2 <- deep_south %>% 
  select(dacade, pathway, option, kgDM_ha, month )%>%
  filter(dacade %in% c("2010_2020", "2090_2100" )) %>% filter(option %in% c("demand" , "pgr_optimistic")) %>% filter(pathway %in% c("kurrent","rcp4.5"))

# filter late century less optimistic
temp3 <- deep_south %>% 
  select(dacade, pathway, option, kgDM_ha, month )%>%
  filter(dacade %in% c("2010_2020", "2090_2100" )) %>% filter(option %in% c("demand" , "pgr_less_optimistic")) %>% filter(pathway %in% c("kurrent","rcp4.5"))


# Binding  
temp <-  rbind(temp0 %>% mutate(Option = "Mid-Century optimistic"), 
               temp1 %>% mutate(Option = "Mid-Century less optimistic"),
               temp2 %>% mutate(Option = "End-Century optimistic"),
               temp3 %>% mutate(Option = "End-Century less optimistic"))
temp$Option<-  factor(temp$Option, levels=c("Mid-Century optimistic","Mid-Century less optimistic","End-Century optimistic","End-Century less optimistic" )) # Ordering for the facet
temp$option<-  factor(temp$option, levels=c("demand","pgr_optimistic","pgr_less_optimistic")) # Ordering for the legend.



 # ggplot rCP 4.5 growth rates.
ggplot(data = temp, mapping = aes(y = kgDM_ha, x = month, group=option, color=option)) +
  geom_line()+
 geom_point(aes(color=option), size=2)+  
  labs(y="Pasture growth rates kgDM/day", x="Month") + theme(legend.title=element_blank()) + theme(legend.position = "bottom") +
  scale_x_discrete(limits = c("Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec", "Jan", "Feb","Mar", "Apr", "May"))+
  facet_wrap (~Option, scales = "fixed")  + theme(plot.title = element_text(hjust = 0.5))


# RCP 8.5
# filter mid-century optimistic
temp4 <- deep_south %>% 
  select(dacade, pathway, option, kgDM_ha, month )%>%
  filter(dacade %in% c("2010_2020", "2040_2050" )) %>% filter(option %in% c("demand" , "pgr_optimistic")) %>% filter(pathway %in% c("kurrent","rcp8.5"))

ggplot()+
  geom_point(data=temp4, mapping=aes(y = kgDM_ha, x = month, colour=option))+
  scale_x_discrete(limits = c("Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec", "Jan", "Feb","Mar", "Apr", "May"))

# filter mid century less optimistic
temp5 <- deep_south %>% 
  select(dacade, pathway, option, kgDM_ha, month )%>%
  filter(dacade %in% c("2010_2020", "2040_2050" )) %>% filter(option %in% c("demand" , "pgr_less_optimistic")) %>% filter(pathway %in% c("kurrent","rcp8.5"))

# filter late century optimistic
temp6 <- deep_south %>% 
  select(dacade, pathway, option, kgDM_ha, month )%>%
  filter(dacade %in% c("2010_2020", "2090_2100" )) %>% filter(option %in% c("demand" , "pgr_optimistic")) %>% filter(pathway %in% c("kurrent","rcp8.5"))

# filter late century less optimistic
temp7 <- deep_south %>% 
  select(dacade, pathway, option, kgDM_ha, month )%>%
  filter(dacade %in% c("2010_2020", "2090_2100" )) %>% filter(option %in% c("demand" , "pgr_less_optimistic")) %>% filter(pathway %in% c("kurrent","rcp8.5"))


# Binding  
tempz <-  rbind(temp4 %>% mutate(Option = "Mid-Century optimistic"), 
               temp5 %>% mutate(Option = "Mid-Century less optimistic"),
               temp6 %>% mutate(Option = "End-Century optimistic"),
               temp7 %>% mutate(Option = "End-Century less optimistic"))
tempz$Option<-  factor(tempz$Option, levels=c("Mid-Century optimistic","Mid-Century less optimistic","End-Century optimistic","End-Century less optimistic" )) # Ordering for the facet
tempz$option<-  factor(tempz$option, levels=c("demand","pgr_optimistic","pgr_less_optimistic")) # Ordering for the legend.



# ggplot RCP 8.5 growth rates.
ggplot(data = tempz, mapping = aes(y = kgDM_ha, x = month, group=option, color=option)) +
  geom_line()+
  geom_point(aes(color=option), size=2)+  
  labs(y="Pasture growth rates kgDM/day", x="Month") + theme(legend.title=element_blank()) + theme(legend.position = "bottom") +
  scale_x_discrete(limits = c("Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec", "Jan", "Feb","Mar", "Apr", "May"))+
  facet_wrap (~Option, scales = "fixed")  + theme(plot.title = element_text(hjust = 0.5))




    