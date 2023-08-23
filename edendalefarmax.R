

df<-read.csv("C:/Users/chikazhet/edendalesouthdeep.csv")
edendalesouthdeep<- df
library(tidyverse)
library(ggplot2)
library("reshape2")
library("ggplot2")
library("ggpubr")
theme_set(theme_pubr())


#Plotting on impact on profit

edendalesouthdeep$Outlook <- factor(edendalesouthdeep$Outlook, 
                                    levels = c("base", "optimistic", "less_optimistic")) 
ggplot() +
  geom_point(data=edendalesouthdeep,
             mapping= aes(y = operating_profit,x = decade, shape=Impact, color=Outlook), size=3) +
   theme_grey() +
  labs(y="Operating profit $/ha", x="Decade")   +
  facet_wrap("Pathway")+
  scale_x_discrete(limits = c("current", "2040_2050", "2090_2100")) +
  ggtitle(" Figure 4 Predicted operating profit mid & end-century")+
theme(plot.title = element_text(hjust = 0.5)) 
  
  


dp<- ggplot(data=edendalesouthdeep, mapping=aes(y = annual_pasture, x = decade, colour=Outlook)) +
  
  scale_color_manual(values = c("red", "green", "cornflowerblue")) +
  
  geom_point(aes( shape=Pathway),size=3) + 
  scale_y_continuous(limits = c(8, 15)) +
  theme_grey() +
  labs(y="Annual pasture yield tDM/ha", x="Decade") + 
  scale_x_discrete(limits = c("current", "2040_2050", "2090_2100"))+
  ggtitle("Figure 3 Predicted annual pasture yield mid & end-century.") +
  theme(plot.title = element_text(hjust = 0.5))
edendalesouthdeep$Outlook <- factor(edendalesouthdeep$Outlook, levels = c("base", "optimistic", "less_optimistic")) 
dp

t <-ggplot(data=edendalesouthdeep, mapping=aes(y = KgMs_ha, x = decade, colour=Outlook)) +
  scale_color_manual(values = c("red", "green", "cornflowerblue")) +
  geom_point(aes( shape=Impact),size=3) + 
  scale_y_continuous(limits = c(500, 1200)) +
  facet_wrap("Pathway")+
   theme_grey() +
  labs(y="Milk production KgMs/ha", x="Decade") + 
  scale_x_discrete(limits = c("current", "2040_2050", "2090_2100"))+
  ggtitle(" Predicted milk production mid & end-century.")  + 
  theme(plot.title = element_text(hjust = 0.5)) 
edendalesouthdeep$Outlook <- factor(edendalesouthdeep$Outlook, levels = c("base", "optimistic", "less_optimistic")) 
t

tc <- ggplot(data=edendalesouthdeep, mapping=aes(y = purchased_Nsurplus, x = decade, colour=Outlook)) +
  scale_color_manual(values = c("red", "green", "cornflowerblue")) +
  geom_point(aes( shape=Impact),size=3) +
  theme_grey() +
  facet_wrap("Pathway")+
  labs(y="Purchased N-surplus kg/ha", x="Decade") + 
  scale_x_discrete(limits = c("current", "2040_2050", "2090_2100"))+
  ggtitle("Figure 6 Predicted purchased N surplus mid & end-century.")+ 
 theme(plot.title = element_text(hjust = 0.5))
edendalesouthdeep$Outlook <- factor(edendalesouthdeep$Outlook, levels = c("base", "optimistic", "less_optimistic")) 
tc


tlc <-ggplot(data=edendalesouthdeep, mapping=aes(y = methane_t_ha, x = decade, colour=Outlook)) +
  scale_color_manual(values = c("red", "green", "cornflowerblue")) +
  geom_point(aes( shape=Impact),size=3) +
  scale_y_continuous(limits = c(6.2, 8)) +
  facet_wrap("Pathway")+
  theme_grey() + 
  labs(y="Methane  CO2 equivalent t/ha", x="Decade") + 
  scale_x_discrete(limits = c("current", "2040_2050", "2090_2100"))+ 
  ggtitle("Figure 5 Predicted enteric methane emmissions mid & end-century.") +
  theme(plot.title = element_text(hjust = 0.5))
edendalesouthdeep$Outlook <- factor(edendalesouthdeep$Outlook, levels = c("base", "optimistic", "less_optimistic")) 
tlc




