df<-read.csv("C:/Users/chikazhet/midcentury.csv")


midendcentury<- df

library(tidyverse)
library(ggplot2)
library("reshape2")
library("ggplot2")
library(KernSmooth)
library(dplyr)
library(stringr)

# filtering mid century adaptations.
midcentury <- midendcentury %>% 
  filter(Century %in% c("Current", "Mid"))

#plotting mid century adaptations profitability.
ggplot(data=midcentury, mapping=aes(y = profit, x = PGR, colour=Adaptation)) +
  geom_point( size=3)+ labs(title="Mid century adaptations profitability")+theme(plot.title = element_text(hjust = 0.5))+
  labs(y="Operating profit $/ha", x="Pasture perfomance") +
   scale_x_discrete(limits = c("Current","Optimistic", "Less optimistic"))


#plotting mid century adaptations methane emissions.
ggplot(data=midcentury, mapping=aes(y = Methane, x = PGR, colour=Adaptation)) +
  geom_point( size=3)+ labs(title="Mid century adaptations methane emissions")+theme(plot.title = element_text(hjust = 0.5))+
  labs(y="Methane t C02 equivalent/ha", x="Pasture perfomance") +
  ylim(7,10)+
  scale_x_discrete(limits = c("Current","Optimistic", "Less optimistic"))

# filtering end century adaptations.
endcentury <- midendcentury %>% 
  filter(Century %in% c("Current", "End"))

#plotting end century adaptations profitability.
ggplot(data=endcentury, mapping=aes(y = profit, x = PGR, colour=Adaptation)) +
  geom_point( size=3)+ labs(title="End century adaptations profitability")+theme(plot.title = element_text(hjust = 0.5))+
  labs(y="Operating profit $/ha", x="Pasture perfomance") +
  scale_x_discrete(limits = c("Current","Optimistic", "Less optimistic"))

#plotting end century adaptations methane emissions.
ggplot(data=endcentury, mapping=aes(y = Methane, x = PGR, colour=Adaptation)) +
  geom_point( size=3)+ labs(title="End century adaptations methane emissions")+theme(plot.title = element_text(hjust = 0.5))+
  labs(y="Methane t C02 equivalent/ha", x="Pasture perfomance") +
  ylim(7,10)+
  scale_x_discrete(limits = c("Current","Optimistic", "Less optimistic"))

