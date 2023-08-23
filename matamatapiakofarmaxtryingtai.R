
#install.packages("remotes")
#remotes::install_github("coolbutuseless/ggpattern")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("resharpe2")
#install.packages("ggpubr")

df<-read.csv("C:/Users/chikazhet/taisouthdeeptai.csv")
tf<-read.csv("C:/Users/chikazhet/taisouthdeeptai2.csv")
taisouthdeeptai<- df
taisouthdeeptai2<- tf
library(tidyverse)
library(ggplot2)
library("reshape2")
library("ggplot2")
library("ggpubr")
library("ggpattern")
theme_set(theme_pubr())



## Plotting annual pasture yield over the century 4.5
taisouthdeeptai$level <- factor(taisouthdeeptai$level, levels = c("base", "optimistic", "less_optimistic"))

ggplot(data = taisouthdeeptai, aes(x = decade, y= annual_pasture,  fill = level)) +
  scale_color_manual(values = c("red","cornflowerblue" ,"green" )) +
  geom_bar(stat ="identity",
                   width=0.5,
                   size=5, 
                   alpha=60, 
                   position=position_dodge()) +
  labs(y="Annual pasture yield tDM/ha", x="Decade") +             
    scale_x_discrete(limits = c("current","2040_2050", "2090_2100")) + 
  ggtitle(" RCP 4.5") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Decade", y = "Annual pasture yield tDM/ha") + theme(legend.position = "none") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")) ,
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=12)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))


## Plotting annual pasture yield over the century 8.5
taisouthdeeptai2$level <- factor(taisouthdeeptai2$level, levels = c("base", "optimistic", "less_optimistic"))

ggplot(data = taisouthdeeptai2, aes(x = decade, y= annual_pasture,  fill = level)) +
  scale_color_manual(values = c("red","cornflowerblue" ,"green" )) +
  geom_bar(stat ="identity",
           width=0.5,
           size=5, 
           alpha=60, 
           position=position_dodge()) +
  labs(y="Annual pasture yield tDM/ha", x="Decade") +             
  scale_x_discrete(limits = c("current","2040_2050", "2090_2100")) + 
  ggtitle(" RCP 8.5") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Decade", y = "Annual pasture yield tDM/ha") + theme(legend.position = "right") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")) ,
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  theme(axis.title.x = element_text(face="bold", size=12),axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(face="bold", size=12),axis.text.y  = element_text(size=12)) +
  theme(axis.line.x = element_line(color="black", size = 0.5),axis.line.y = element_line(color="black", size = 0.5))

