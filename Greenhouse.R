# Data for variables taken over 50 days 

library(readxl)

Data1 <-read_excel("Data.xlsx")
View(Data)

#Subset for weed or crop

DSonc <- subset(Data1, Plant == "S. Oleraceus")
DRaph <- subset (Data1, Plant == "R. Sativus")

#Subset both data frames for leaf number mean

DS1mean <- aggregate(DSonc$LeafN , list(DSonc$Days, DSonc$Treatment), mean)
colnames(DS1mean)[colnames(DS1mean) == "Group.1"] <- "Day"
colnames(DS1mean)[colnames(DS1mean) == "Group.2"] <- "Treatment"
colnames(DS1mean)[colnames(DS1mean) == "x"] <- "Leaf_number"


DR1mean <- aggregate(DRaph$LeafN , list(DRaph$Days, DRaph$Treatment), mean)
colnames(DR1mean)[colnames(DR1mean) == "Group.1"] <- "Day"
colnames(DR1mean)[colnames(DR1mean) == "Group.2"] <- "Treatment"
colnames(DR1mean)[colnames(DR1mean) == "x"] <- "Leaf_number"

#Subset both data frames for height mean

DS2mean <- aggregate(as.numeric(DSonc$Height) , list(DSonc$Days, DSonc$Treatment), 
                     mean)
colnames(DS2mean)[colnames(DS2mean) == "Group.1"] <- "Day"
colnames(DS2mean)[colnames(DS2mean) == "Group.2"] <- "Treatment"
colnames(DS2mean)[colnames(DS2mean) == "x"] <- "Height_cm"

DR2mean <- aggregate(as.numeric(DRaph$Height) , list(DRaph$Days, DRaph$Treatment), 
                     mean)
colnames(DR2mean)[colnames(DR2mean) == "Group.1"] <- "Day"
colnames(DR2mean)[colnames(DR2mean) == "Group.2"] <- "Treatment"
colnames(DR2mean)[colnames(DR2mean) == "x"] <- "Height_cm"

#Subset both data frames for humidity mean


DS3mean <- aggregate(DSonc$Humidity , list(DSonc$Days, DSonc$Treatment), mean, 
                     na.rm=TRUE)
colnames(DS3mean)[colnames(DS3mean) == "Group.1"] <- "Day"
colnames(DS3mean)[colnames(DS3mean) == "Group.2"] <- "Treatment"
colnames(DS3mean)[colnames(DS3mean) == "x"] <- "Humidity_pct"

DR3mean <- aggregate(DRaph$Humidity, list(DRaph$Days, DRaph$Treatment), mean,  
                     na.rm=TRUE)
colnames(DR3mean)[colnames(DR3mean) == "Group.1"] <- "Day"
colnames(DR3mean)[colnames(DR3mean) == "Group.2"] <- "Treatment"
colnames(DR3mean)[colnames(DR3mean) == "x"] <- "Humidity_pct"

#Subset both data frames for conductivity mean

DS4mean <- aggregate(as.numeric(DSonc$conductivty) , list(DSonc$Days, DSonc$Treatment),
                     mean,  na.rm=TRUE)
colnames(DS4mean)[colnames(DS4mean) == "Group.1"] <- "Day"
colnames(DS4mean)[colnames(DS4mean) == "Group.2"] <- "Treatment"
colnames(DS4mean)[colnames(DS4mean) == "x"] <- "Conductivity"

DR4mean <- aggregate(as.numeric(DRaph$conductivty) , list(DRaph$Days, DRaph$Treatment),
                     mean,  na.rm=TRUE)
colnames(DR4mean)[colnames(DR4mean) == "Group.1"] <- "Day"
colnames(DR4mean)[colnames(DR4mean) == "Group.2"] <- "Treatment"
colnames(DR4mean)[colnames(DR4mean) == "x"] <- "Conductivity"

#plots 

library(ggplot2)

p1 <- ggplot(data=DS1mean, aes(x=Day, y=Leaf_number, group = Treatment, 
                      color = as.factor(Treatment))) + 
                      labs(color = "Treatment")+
                       geom_line(size=1)+
                       ylim(0, 8)+
                      theme(legend.position = "none")+
                      theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  geom_text(x=50.5, y=8, label="a", color="black", size = 3.5)+
  geom_text(x=50.5, y=6, label="b", color="black", size = 3.5)+
  geom_text(x=50.5, y=5.5, label="b", color="black", size = 3.5)+
  geom_text(x=50.5, y=0, label="c", color="black", size = 3.5)
p2 <-ggplot(data=DR1mean, aes(x=Day, y=Leaf_number, group = Treatment, 
                               color = as.factor(Treatment))) +
  labs(color = "Treatment")+
  geom_line(size=1)+
  ylim(0, 8)+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  geom_text(x=50.5, y=4.5, label="a", color="black", size = 3.5)+
  geom_text(x=50.5, y=2.3, label="b", color="black", size = 3.5)+
  geom_text(x=50.5, y=1.7, label="b", color="black", size = 3.5)+
  geom_text(x=50.5, y=0.3, label="c", color="black", size = 3.5)
p3 <- ggplot(data=DS2mean, aes(x=Day, y=Height_cm, group = Treatment, 
                               color = as.factor(Treatment))) +
  labs(color = "Treatment")+
  geom_line(size=1)+
  ylim(0, 15)+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  geom_text(x=50.5, y=11.7, label="a", color="black", size = 3.5)+
  geom_text(x=50.5, y=11, label="a", color="black", size = 3.5)+
  geom_text(x=50.5, y=7, label="ab", color="black", size = 3.5)+
  geom_text(x=50.5, y=4, label="b", color="black", size = 3.5)
p4 <- ggplot(data=DR2mean, aes(x=Day, y=Height_cm, group = Treatment, 
                               color = as.factor(Treatment))) +
  labs(color = "Treatment")+
  geom_line(size=1)+
  ylim(0, 15)+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  geom_text(x=50.5, y=6, label="a", color="black", size = 3.5)+
  geom_text(x=50.5, y=4.8, label="b", color="black", size = 3.5)+
  geom_text(x=50.5, y=2.7, label="c", color="black", size = 3.5)+
  geom_text(x=50.5, y=0, label="d", color="black", size = 3.5)
p5 <- ggplot(data=DS3mean, aes(x=Day, y=Humidity_pct, group = Treatment, 
                               color = as.factor(Treatment))) +
  labs(color = "Treatment")+
  geom_line(size=1)+
  ylim(0, 60)+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())+
  geom_text(x=50.5, y=39, label="a", color="black", size = 3.5)+
  geom_text(x=50.5, y=35, label="a", color="black", size = 3.5)+
  geom_text(x=50.5, y=31, label="a", color="black", size = 3.5)
p6 <- ggplot(data=DR3mean, aes(x=Day, y=Humidity_pct, group = Treatment, 
                                  color = as.factor(Treatment))) +
  labs(color = "Treatment")+
  geom_line(size=1)+ 
  ylim(0, 60)+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())
p7 <- ggplot(data=DS4mean, aes(x=Day, y=Conductivity, group = Treatment, 
                               color = as.factor(Treatment))) +
  labs(color = "Treatment")+
  geom_line(size=1)+
  ylim(0, 60)+
  theme(legend.position = "none")+
  geom_text(x=50.5, y=31, label="a", color="black", size = 3.5)+
  geom_text(x=50.5, y=27, label="a", color="black", size = 3.5)+
  geom_text(x=50.5, y=2, label="b", color="black", size = 3.5)
p8 <- ggplot(data=DR4mean, aes(x=Day, y=Conductivity, group = Treatment, 
                               color = as.factor(Treatment))) +
  labs(color = "Treatment")+
  geom_line(size=1)+
  ylim(0, 60)+
  theme(legend.position = "none")


library(ggpubr)

plots <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, common.legend = TRUE, 
                   legend = "bottom",
          labels = c("  S.Oleraceus","      R.Sativus",'', '', '', '', '',''), 
          hjust = -1.9, vjust = -0.5,
          ncol = 2, nrow = 4)+
  theme(plot.margin = margin(1,0.1,1,0.1, "cm"))
  annotate_figure(plots,
                  top = text_grob("           Effect of drought and salinity on leaf number, 
                  height, humidity and conductivity in Sonchus oleraceus and 
                  Raphanus sativus over a 50 day period", 
                                  face = "bold", size = 14))
  
#analyses and statistical differences

#subset of 50 days.

DS1 <- DSonc[DSonc$Days == 50, ]
DR1 <- DRaph[DRaph$Days == 50,]

# Leaf Number 50 days
model_DS1 <- lm(LeafN ~ Treatment, data = DS1)
shap_DS1 <- shapiro.test(model_DS1[["residuals"]])
shap_DS1 #Not normal.

model_DR1 <- lm(LeafN ~ Treatment, data = DR1)
shap_DR1 <- shapiro.test(model_DR1[["residuals"]])
shap_DR1 #Not normal.

#Height 50 days
model_DS2 <- lm(Height ~ Treatment, data = DS1)
shap_DS2 <- shapiro.test(model_DS2[["residuals"]])
shap_DS2 #Normal.

model_DR2 <- lm(Height ~ Treatment, data = DR1)
shap_DR2 <- shapiro.test(model_DR2[["residuals"]])
shap_DR2 #Not normal.

#Humidity 50 days

model_DS3 <- lm(Humidity ~ Treatment, data = DS1, na.rm=TRUE)
shap_DS3 <- shapiro.test(model_DS3[["residuals"]])
shap_DS3  #Normal.

model_DR3 <- lm(Humidity ~ Treatment, data = DR1)
shap_DR3 <- shapiro.test(model_DR3[["residuals"]])
shap_DR3 #Normal.

#Conductivity 50 days.

model_DS4 <- lm(conductivty ~ Treatment, data = DS1, na.rm=TRUE)
shap_DS4 <- shapiro.test(model_DS4[["residuals"]])
shap_DS4 #Not normal.



# Analyses 
library(agricolae)#Kruskall wallis

Krus_DS1 <- agricolae::kruskal(DS1$LeafN, DS1$Treatment)
Krus_DS1

Krus_DR1 <- agricolae::kruskal(DR1$LeafN, DR1$Treatment)
Krus_DR1

An_DS2 <- aov(as.numeric(Height) ~ Treatment, data = DS1)
summary.aov(An_DS2)
agri_DS2 <- agricolae::HSD.test(An_DS2, "Treatment", group = TRUE, console = TRUE)
agri_DS2

Krus_DR2 <- agricolae::kruskal(as.numeric(DR1$Height), DR1$Treatment)
Krus_DR2
  
An_DS3 <- aov(Humidity ~ Treatment, data = DS1)
summary.aov(An_DS3)
agri_DS3 <- agricolae::HSD.test(An_DS3, "Treatment", group = TRUE, console = TRUE)
agri_DS3

An_DR3 <- aov(Humidity ~ Treatment, data = DR1)
summary.aov(An_DR3)
agri_DR3 <- agricolae::HSD.test(An_DR3, "Treatment", group = TRUE, console = TRUE)
agri_DR3 #No result, there is just 1 group

Krus_DS4 <- agricolae::kruskal(as.numeric(DS1$conductivty), DS1$Treatment)
Krus_DS4

#Did not do analysis for DR5 because there is just 1 Treatment that has values 
#at 50 days.
