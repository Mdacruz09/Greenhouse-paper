##Packages needed
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("lmtest")
# install.packages("agricolae")
# install.packages("cowplot")


#Data for one measurement variables.

library(readxl)
Data_2 <- read_excel("Data_2.xlsx")
View(Data_2)

#Subset for weed or crop

DS <- subset(Data_2, Plant == "S. oleraceus")
DR <- subset (Data_2, Plant == "R. sativus")

#ROOT LENGTH
#Plot Root length
library(ggplot2)

p1.1 <-ggplot(data=DS, aes(x=Treatment, y=Root_length_cm)) + 
  geom_bar(stat="identity", width = 0.5, fill= "mediumturquoise", position=position_dodge())+
  ggtitle(expression (paste( italic("Sonchus oleraceus")," ")))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(x=1, y=33, label="a", color="black", size = 3.5)+
  geom_text(x=2, y=24, label="a", color="black", size = 3.5)+
  geom_text(x=3, y=29, label="a", color="black", size = 3.5)+
  geom_text(x=4, y=36, label="a", color="black", size = 3.5)
p1.2 <-  ggplot(data=DR, aes(x=Treatment, y=Root_length_cm)) + 
  geom_bar(stat="identity", width = 0.5, fill= "lightcoral",position=position_dodge())+
  ggtitle(expression (paste( italic("Raphanus sativus")," ")))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(x=1, y=16, label="a", color="black", size = 3.5)+
  geom_text(x=2, y=6, label="b", color="black", size = 3.5)+
  geom_text(x=3, y=11.5, label="ab", color="black", size = 3.5)+
  geom_text(x=4, y=12.5, label="ab", color="black", size = 3.5)


library(ggpubr)


cowplot::plot_grid(p1.1, p1.2)+
  labs(title=expression(paste("Effect of drought and salinity on Root Lengtht in ", italic("Sonchus oleraceus "),"and",italic(" Raphanus sativus "),
                              "after 50 days")))+
  theme(plot.title = element_text(face="bold"))+
  theme(plot.title = element_text(lineheight = 0.9, vjust = 8))+
  theme(plot.margin=margin(21,15,4,4))

#analyses and statistical differences

model_DS <- lm(Root_length_cm ~ Treatment, data = DS, )
shap_DS <- shapiro.test(model_DS[["residuals"]])
shap_DS #Normal

library(lmtest)
bptest( model_DS )#Confirmed

model_DR <- lm(Root_length_cm ~ Treatment, data = DR, )
shap_DR <- shapiro.test(model_DR[["residuals"]])
shap_DR#Normal

bptest( model_DR )#Confirmed


library(agricolae)

An_DS <- aov(Root_length_cm ~ Treatment, data = DS)
summary.aov(An_DS)
agri_DS <- agricolae::HSD.test(An_DS, "Treatment", group = TRUE, console = TRUE)
agri_DS

An_DR <- aov(Root_length_cm ~ Treatment, data = DR)
summary.aov(An_DR)
agri_DR<- agricolae::HSD.test(An_DR, "Treatment", group = TRUE, console = TRUE)
agri_DR


#FRESH AND DRY WEIGHT

pa <- ggplot(data=DS, aes(x=Treatment, y=Fresh_weight_g)) + 
  geom_bar(stat="identity", width = 0.5, fill= "mediumturquoise", position=position_dodge())+
  ggtitle(expression (paste( italic("Sonchus oleraceus")," ")))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  ylim(0, 6.5)+
  geom_text(x=1, y=6.5, label="a", color="black", size = 3.5)+
  geom_text(x=2, y=0.5, label="c", color="black", size = 3.5)+
  geom_text(x=3, y=2.5, label="a", color="black", size = 3.5)+
  geom_text(x=4, y=1.9, label="b", color="black", size = 3.5)
pe<- ggplot(data=DR, aes(x=Treatment, y=Fresh_weight_g)) + 
  geom_bar(stat="identity", width = 0.5, fill= "lightcoral",position=position_dodge())+
  ggtitle(expression (paste( italic("Raphanus sativus")," ")))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  theme(axis.title.y=element_blank())+
  ylim(0, 0.9)+
  geom_text(x=1, y=0.63, label="a", color="black", size = 3.5)+
  geom_text(x=2, y=0.12, label="c", color="black", size = 3.5)+
  geom_text(x=3, y=0.85, label="a", color="black", size = 3.5)+
  geom_text(x=4, y=0.4, label="b", color="black", size = 3.5)
pi <- ggplot(data=DS, aes(x=Treatment, y=Dry_weight_g)) + 
  geom_bar(stat="identity", width = 0.5, fill= "mediumturquoise", position=position_dodge())+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(0, 6.5)+
  geom_text(x=1, y=3.6, label="a", color="black", size = 3.5)+
  geom_text(x=2, y=0.3, label="c", color="black", size = 3.5)+
  geom_text(x=3, y=0.6, label="a", color="black", size = 3.5)+
  geom_text(x=4, y=0.4, label="b", color="black", size = 3.5)
po <- ggplot(data=DR, aes(x=Treatment, y=Dry_weight_g)) + 
  geom_bar(stat="identity", width = 0.5, fill= "lightcoral",position=position_dodge())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.y=element_blank())+
  ylim(0, 0.9)+
  geom_text(x=1, y=0.1, label="a", color="black", size = 3.5)+
  geom_text(x=2, y=0.05, label="b", color="black", size = 3.5)+
  geom_text(x=3, y=0.1, label="ab", color="black", size = 3.5)+
  geom_text(x=4, y=0.05, label="b", color="black", size = 3.5)

library(ggplot2)
library(cowplot)

cowplot::plot_grid(pa, pe, pi, po)+
  labs(title=expression(paste("Effect of drought and salinity on Fresh and Dry Weight in ", italic("Sonchus oleraceus "),"and",italic(" Raphanus sativus "),
                 "after 50 days")))+
  theme(plot.title = element_text(face="bold"))+
  theme(plot.title = element_text(lineheight = 0.9, vjust = 8))+
  theme(plot.margin=margin(21,15,4,4))


#analyses and statistical differences

model_pa <- lm(Fresh_weight_g ~ Treatment, data = DS)
shap_pa <- shapiro.test(model_pa[["residuals"]])
shap_pa #Not normal

model_pe <- lm(Fresh_weight_g ~ Treatment, data = DR)
shap_pe <- shapiro.test(model_pe[["residuals"]])
shap_pe #Not normal

model_pi <- lm(Dry_weight_g ~ Treatment, data = DS)
shap_pi <- shapiro.test(model_pi[["residuals"]])
shap_pi#Not normal

model_po <- lm(Dry_weight_g ~ Treatment, data = DR)
shap_po <- shapiro.test(model_po[["residuals"]])
shap_po #Not normal

library(agricolae)
Krus_pa <- agricolae::kruskal(DS$Fresh_weight_g, DS$Treatment)
Krus_pa

Krus_pe <- agricolae::kruskal(DR$Fresh_weight_g, DR$Treatment)
Krus_pe

Krus_pi <- agricolae::kruskal(DS$Dry_weight_g, DS$Treatment)
Krus_pi

Krus_po <- agricolae::kruskal(DR$Dry_weight_g, DR$Treatment)
Krus_po

