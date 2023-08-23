df<-read.csv("C:/Users/chikazhet/edendale_deep_south.csv")

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
deep_south$option<-  factor(deep_south$option, levels=c("current","optimistic","less_optimistic", "demand")) # Ordering for the legend.
ggplot()+        
  geom_point(data=deep_south, mapping=aes(y = pgr, x = month, group=option, colour=option)) +
  theme_grey() +
  scale_color_manual(values = c("red", "green", "cornflowerblue", "black")) +
    scale_x_discrete(limits = c("Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec", "Jan", "Feb","Mar", "Apr", "May"))+
  ggtitle(" Figure 3, RCP 4.5 & 8.5 predicted pasture growth rates mid to end century") +
  scale_fill_discrete(breaks=c('current', 'optimistic', 'less_optimistic', 'demand')) + 
  theme(legend.position = "bottom") +
  labs(y="Pasture growth rates kgDM/ha/day", x="Month")

  

#RCP4.5
# filter mid-century optimistic
temp0 <- deep_south %>% 
  select(dacade, pathway, option, pgr, month )%>%
filter(dacade %in% c("2010_2020", "2040_2050" )) %>% filter(option %in% c("demand" , "optimistic")) %>% filter(pathway %in% c("kurrent","rcp4.5"))
  
ggplot()+
geom_point(data=temp0, mapping=aes(y = pgr, x = month, colour=option))+ theme_grey() +
  scale_x_discrete(limits = c("Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec", "Jan", "Feb","Mar", "Apr", "May"))

# filter mid century less optimistic
temp1 <- deep_south %>% 
  select(dacade, pathway, option, pgr, month )%>%
  filter(dacade %in% c("2010_2020", "2040_2050" )) %>% filter(option %in% c("demand" , "less_optimistic")) %>% filter(pathway %in% c("kurrent","rcp4.5"))

# filter late century optimistic
temp2 <- deep_south %>% 
  select(dacade, pathway, option, pgr, month )%>%
  filter(dacade %in% c("2010_2020", "2090_2100" )) %>% filter(option %in% c("demand" , "optimistic")) %>% filter(pathway %in% c("kurrent","rcp4.5"))

# filter late century less optimistic
temp3 <- deep_south %>% 
  select(dacade, pathway, option, pgr, month )%>%
  filter(dacade %in% c("2010_2020", "2090_2100" )) %>% filter(option %in% c("demand" , "less_optimistic")) %>% filter(pathway %in% c("kurrent","rcp4.5"))


# Binding  
temp <-  rbind(temp0 %>% mutate(Option = "Mid-Century optimistic"), 
               temp1 %>% mutate(Option = "Mid-Century less optimistic"),
               temp2 %>% mutate(Option = "End-Century optimistic"),
               temp3 %>% mutate(Option = "End-Century less optimistic"))
temp$Option<-  factor(temp$Option, levels=c("Mid-Century optimistic","Mid-Century less optimistic","End-Century optimistic","End-Century less optimistic" )) # Ordering for the facet
temp$option<-  factor(temp$option, levels=c("current","optimistic","less_optimistic", "demand")) # Ordering for the legend.



 # ggplot rCP 4.5 growth rates.
ggplot(data = temp, mapping = aes(y = pgr, x = month, group=option, color=option)) +
  #geom_line()+
 geom_line(aes(color=option), size=1)+  
  labs(y="kgDM/ha/day", x="Month") + 
  scale_color_manual(values = c("green", "cornflowerblue", "black"))+
  theme(legend.title=element_blank()) + theme(legend.position = "bottom") +
  scale_x_discrete(limits = c("Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec", "Jan", "Feb","Mar", "Apr", "May"))+
  facet_wrap (~Option, scales = "fixed") +  theme_grey() +
  ggtitle(" Figure 1b, RCP4.5 demand(black) vs supply(green & blue)") +
  theme(plot.title = element_text(hjust = 0.5))+
  guides(color = guide_legend(title = "Outlook"))


# RCP 8.5
# filter mid-century optimistic
temp4 <- deep_south %>% 
  select(dacade, pathway, option, pgr, month )%>%
  filter(dacade %in% c("2010_2020", "2040_2050" )) %>% filter(option %in% c("demand" , "optimistic")) %>% filter(pathway %in% c("kurrent","rcp8.5"))

ggplot()+
  geom_point(data=temp4, mapping=aes(y = pgr, x = month, colour=option))+ theme_grey() +
  scale_x_discrete(limits = c("Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec", "Jan", "Feb","Mar", "Apr", "May"))

# filter mid century less optimistic
temp5 <- deep_south %>% 
  select(dacade, pathway, option, pgr, month )%>%
  filter(dacade %in% c("2010_2020", "2040_2050" )) %>% filter(option %in% c("demand" , "less_optimistic")) %>% filter(pathway %in% c("kurrent","rcp8.5"))

# filter late century optimistic
temp6 <- deep_south %>% 
  select(dacade, pathway, option, pgr, month )%>%
  filter(dacade %in% c("2010_2020", "2090_2100" )) %>% filter(option %in% c("demand" , "optimistic")) %>% filter(pathway %in% c("kurrent","rcp8.5"))

# filter late century less optimistic
temp7 <- deep_south %>% 
  select(dacade, pathway, option, pgr, month )%>%
  filter(dacade %in% c("2010_2020", "2090_2100" )) %>% filter(option %in% c("demand" , "less_optimistic")) %>% filter(pathway %in% c("kurrent","rcp8.5"))


# Binding  
tempz <-  rbind(temp4 %>% mutate(Option = "Mid-Century optimistic"), 
               temp5 %>% mutate(Option = "Mid-Century less optimistic"),
               temp6 %>% mutate(Option = "End-Century optimistic"),
               temp7 %>% mutate(Option = "End-Century less optimistic"))
tempz$Option<-  factor(tempz$Option, levels=c("Mid-Century optimistic","Mid-Century less optimistic","End-Century optimistic","End-Century less optimistic" )) # Ordering for the facet
tempz$option<-  factor(tempz$option, levels=c("optimistic","less_optimistic", "demand")) # Ordering for the legend.



# ggplot RCP 8.5 growth rates.
ggplot(data = tempz, mapping = aes(y = pgr, x = month, group=option, color=option)) +
  #geom_line()+
  geom_line(aes(color=option),size=1)+  
  labs(y="kgDM/ha/day", x="Month") + 
  scale_color_manual(values = c("green", "cornflowerblue", "black"))+
  theme(legend.title=element_blank()) + 
  theme(legend.position = "bottom") +
  scale_x_discrete(limits = c(
    "Jun", "Jul", "Aug","Sep", "Oct", "Nov","Dec", "Jan", "Feb","Mar", "Apr", "May"))+
  facet_wrap (~Option, scales = "fixed") + theme_grey() +
  ggtitle("Figure 2b, RCP8.5 demand(black) vs supply(green & blue)") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(title = "Outlook"))
  
 



    