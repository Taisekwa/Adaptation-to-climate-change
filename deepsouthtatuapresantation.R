#Load the necessary libraries
library(tidyverse)
library(ggplot2)
library("reshape2")
library("ggplot2")
library(KernSmooth)
library(dplyr)
library("ggpubr")
theme_set(theme_pubr())


df<-read.csv("C:/Users/chikazhet/matamata1.csv")
df1<-read.csv("C:/Users/chikazhet/matamata2.csv")
df2<-read.csv("C:/Users/chikazhet/matamata3.csv")

# All mitigations plotted
Tai<- df
Tai1 <- df1
Tai2 <- df2
Tai$Month <- as.character(Tai$Month)
#Then turn it back into a factor with the levels in the correct order
Tai$Month <- factor(Tai$Month, levels=unique(Tai$Month))

Tai1$Month <- as.character(Tai1$Month)
#Then turn it back into a factor with the levels in the correct order
Tai1$Month <- factor(Tai1$Month, levels=unique(Tai1$Month))

Tai2$Month <- as.character(Tai2$Month)
#Then turn it back into a factor with the levels in the correct order
Tai2$Month <- factor(Tai2$Month, levels=unique(Tai2$Month))


p<- ggplot(data = Tai, mapping = aes(y = DM_ha_day, x = Month, group= Demand_Supply)) +
  
geom_line(aes(color=Demand_Supply), size=1)+
  
geom_point( size=3) + labs(title="PGR 2020 vs Demand") +
  
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme (legend.position="bottom")
p

g<- ggplot(data = Tai1, mapping = aes(y = DM_ha_day, x = Month, group= Demand_Supply)) +
  
  geom_line(aes(color=Demand_Supply), size=1)+ 
  
  geom_point( size=3) +labs(title="PGR 2040 vs Demand")+
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  theme (legend.position="bottom")
g

f<- ggplot(data = Tai2, mapping = aes(y = DM_ha_day, x = Month, group= Demand_Supply)) +
  
  geom_line(aes(color=Demand_Supply), size=1)+ 
  
  geom_point( size=3) +labs(title="PGR 2090 vs Demand")+
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  theme (legend.position="bottom")
f




  
