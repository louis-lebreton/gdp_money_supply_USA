## Fluctuations du PIB et masse monétaire aux USA

#####################################################################################################

## Définition de l'emplacement du répertoire de travail
setwd(dir='C:/Users/lebre/OneDrive/Bureau/Projet')

## Packages
# install.packages("mFilter") 
library(mFilter) # filtre HP
library(ggplot2)
library(tidyverse)
library("reshape2")

#####################################################################################################
# Importation et traitement des données
#####################################################################################################

# Source des données : databank.worldbank.org/reports.aspx?source=2&series=FM.LBL.BMNY.GD.ZS&country=#

## Importation des données
df <- read.table(file="world_bank_data.csv", header= TRUE, sep=",", dec= ".",fileEncoding="UTF-8-BOM")

# Suppression & renommage de colonnes et lignes inutiles
df <- na.omit(df[,c(-1,-2,-4)])
colnames(df) <- c("annee","PIB","masse_monetaire")
df$masse_monetaire <- as.numeric(df$masse_monetaire)


#####################################################################################################
# 2 - Analyse séparée
#####################################################################################################
# Moyenne / Quartiles
summary(df)

# Ecarts-types
sd(df$PIB)
sd(na.omit(df$masse_monetaire))

# TCAM
tcam_pib <- ((tail(df$PIB,1)/head(df$PIB,1))^(1/(2021-1990))-1)*100
tcam_mm <- ((tail(na.omit(df$masse_monetaire),1)/head(df$masse_monetaire,1))^(1/(2021-1990))-1)*100
tcam_pib
tcam_mm

# données avant 2008
dfav2008 <- df[df$annee<=2008,]
tcam_pib <- ((tail(dfav2008$PIB,1)/head(dfav2008$PIB,1))^(1/(2009-1990))-1)*100
tcam_mm <- ((tail(na.omit(dfav2008$masse_monetaire),1)/head(dfav2008$masse_monetaire,1))^(1/(2009-1990))-1)*100
tcam_pib
tcam_mm

# données après 2008
dfap2008 <- df[df$annee>2008,]
tcam_pib <- ((tail(dfap2008$PIB,1)/head(dfap2008$PIB,1))^(1/(2021-2008))-1)*100
tcam_mm <- ((tail(na.omit(dfap2008$masse_monetaire),1)/head(dfap2008$masse_monetaire,1))^(1/(2021-2008))-1)*100
tcam_pib
tcam_mm


# Evolution temporelle des deux variables ###########################################################

# Graphique : Evolution PIB
plot <- ggplot(df, aes(annee)) + 
  geom_line(aes(y = PIB,colour="red"),size=1.5) + 
  geom_hline(yintercept=0,color='black',size=0.4)+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  geom_smooth(aes(y=PIB),method=lm, se=FALSE,color="#5d5e5e",linetype="longdash")+
  theme_light()+
  # échelle choisie
  scale_y_continuous(limits=c(9000000000000,21000000000000), breaks=seq(9000000000000,21000000000000,2000000000000))+
  # titres + légende
  labs(title = "Evolution du PIB en volume",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "US-dollars (constant 2015)",
       color = "Valeur" # titre de la légende
  )+
  scale_color_manual(values = "red",
                     labels = "PIB en volume")

plot

# Régression de la variable PIB par la variable année
reg <- lm(df$PIB~df$annee)
confidence1 <- round(as.numeric(confint(reg)[2]),2) # 2.5 %
confidence2 <- round(as.numeric(confint(reg)[4]),2) # 97.5 %
confidence1
confidence2
reg$coefficients # coefficients
summary(reg)$coefficients[8] # p-value
summary(reg)$r.squared # R2

# Graphique : Evolution Masse monétaire
plot <- ggplot(df, aes(annee)) + 
  geom_line(aes(y = masse_monetaire,colour='darkblue'),size=1.5)+
  geom_smooth(aes(y=masse_monetaire),method=lm, se=FALSE,color="#5d5e5e",linetype="longdash")+
  geom_hline(yintercept=0,color='black',size=0.4)+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  theme_light()+
  # titres + légende
  labs(title = "Evolution de la masse monétaire",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "US-dollars",
       color = "Valeur" # titre de la légende
  )+
  scale_color_manual(values = "darkblue", # couleurs des variables
                     labels = "Masse monétaire") # labels des variables
plot

# Régression de la variable Masse monétaire par la variable année
reg <- lm(df$masse_monetaire~df$annee)
confidence1 <- round(as.numeric(confint(reg)[2]),2) # 2.5 %
confidence2 <- round(as.numeric(confint(reg)[4]),2) # 97.5 %
confidence1
confidence2
reg$coefficients
summary(reg)$coefficients[8] # p-value
summary(reg)$r.squared # R2

# Evolution temporelle des taux de croissance des deux variables ####################################

# Ajout des taux de croissance au dataframe pour chaque variable 
df = df %>%
  arrange(annee) %>%
  mutate(diff_pib = PIB - lag(PIB),
         TC_pib = (diff_pib/lag(PIB)) * 100,
         diff_mm = masse_monetaire - lag(masse_monetaire),
         TC_mm = (diff_mm/lag(masse_monetaire)) * 100)

# Calcul de l'amplitude maximum pour les deux variables
amplitude_max_pib <- max(na.omit(df$TC_pib))
amplitude_max_pib
df$annee[which(df$TC_pib==amplitude_max_pib)]
amplitude_max_mm <- max(na.omit(df$TC_mm))
amplitude_max_mm
df$annee[which(df$TC_mm==amplitude_max_mm)]

# Réalisation du graphique : Evolution du taux de croissance PIB
plot<-ggplot(df, aes(x=annee, y=TC_pib)) +   
  geom_line(size=1.2,color='red') +
  # échelle choisie
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  scale_y_continuous(limits=c(-4,6), breaks=seq(-4,6,1))+
  geom_hline(yintercept=0,color='black')+
  # traçage du tcam du PIB
  geom_hline(yintercept=tcam_pib,color='black',linetype="longdash", size=1)+
  annotate(geom="text", x=1993, y=2.6,size=4.5, label=paste0("TCAM PIB = ",round(tcam_pib,2),"%"),
           color="black")+
  # légende
  labs(title = "Evolution du taux de croissance du PIB en volume",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "Taux de croissance du PIB en %")
plot

# Régression de la variable taux de croissance du PIB par la variable année
reg <- lm(df$TC_pib~df$annee)
confidence1 <- round(as.numeric(confint(reg)[2]),2) # 2.5 %
confidence2 <- round(as.numeric(confint(reg)[4]),2) # 97.5 %
reg$coefficients
summary(reg)$coefficients[8] # p-value
summary(reg)$r.squared # R2

# Réalisation du graphique : Evolution du taux de croissance de la masse monétaire
plot<-ggplot(df, aes(x=annee, y=TC_mm)) +   
  geom_line(size=1.2,color="darkblue") +
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  # échelle choisie
  scale_y_continuous(limits=c(-3,18), breaks=seq(-3,18,1))+
  geom_hline(yintercept=0,color='black')+
  # traçage du tcam de la masse monetaire
  geom_hline(yintercept=tcam_mm,color='black',linetype="longdash", size=1)+
  annotate(geom="text", x=1994, y=6.5,size=4.5, label=paste0("TCAM Masse monétaire = ",round(tcam_mm,2),"%"),
           color="black")+
  # légende
  labs(title = "Evolution du taux de croissance de la masse monétaire",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "Taux de croissance de la masse monétaire en %")
plot

# Régression de la variable taux de croissance de la masse monétaire par la variable année
reg <- lm(df$TC_mm~df$annee)
confidence1 <- round(as.numeric(confint(reg)[2]),2) # 2.5 %
confidence2 <- round(as.numeric(confint(reg)[4]),2) # 97.5 %
reg$coefficients
summary(reg)$coefficients[8] # p-value
summary(reg)$r.squared # R2


# Filtre HP #########################################################################################

# Création de deux séries temporelles pour le PIB et la masse monétaire
ts_pib <- ts(df$PIB,start=c(1990,1),frequency=1)
ts_mm <- ts(na.omit(df$masse_monetaire),start=c(1990,1),frequency=1)

# Application du filtre Hodrick et Prescott aux séries temporelles
hp_pib <- hpfilter(ts_pib)
hp_mm <- hpfilter(ts_mm)

### Représentation des trends HP avec un trend linéaire puis une interpolation polynomiale

### Choix de trend :
### Trend linéaire :
trend_lin = FALSE
### ou Interpolation polynomiale :
inter_poly = TRUE

# création d'un dataframe avec les trends du PIB et de la masse monétaire
df_trend <- data.frame(year=rep(1990:2021,2),
                       Valeur=c(rep("PIB",2021-1990+1),rep("Masse monétaire",2021-1990+1)),
                       trend=rbind(hp_pib$trend,hp_mm$trend,NA))

# renommage des colonnes
colnames(df_trend)[3] <- "trend"

# Choix des couleurs par variable
couleurs <- c("red", "darkblue")

# réalisation du graphique : Trends issus du filtre HP
plot<-ggplot(df_trend[df_trend$Valeur=="Masse monétaire",], aes(x=year, y=trend,group=Valeur,color=Valeur)) +   
  geom_line(size=1.5) +
  scale_color_manual(values = "darkblue")+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  # trend
  {if(trend_lin==TRUE)geom_smooth(method=lm, se=FALSE,color="#4B4B4B",linetype="longdash")}+
  {if(inter_poly==TRUE)geom_smooth(method=lm,formula = y ~ poly(x, 2), se=FALSE,color="#4B4B4B",linetype="longdash")}+
  theme_light()+
  # légende
  labs(title = "Evolution de la masse monétaire  - Trend du filtre HP",
       subtitle = "USA - 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "Trend en US-dollars ")
plot


plot<-ggplot(df_trend[df_trend$Valeur=="PIB",], aes(x=year, y=trend,group=Valeur,color=Valeur)) +   
  geom_line(size=1.5) +
  scale_color_manual(values = "red")+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  # trend
  {if(trend_lin==TRUE)geom_smooth(method=lm, se=FALSE,color="#4B4B4B",linetype="longdash")}+
  {if(inter_poly==TRUE)geom_smooth(method=lm,formula = y ~ poly(x, 2), se=FALSE,color="#4B4B4B",linetype="longdash")}+
  theme_light()+
  # légende
  labs(title = "Evolution du PIB  - Trend du filtre HP",
       subtitle = "USA - 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "Trend en US-dollars (constant 2015)")
plot

# Composantes cycliques #############################################################################

# création d'un dataframe avec les composantes cycliques
df_cycle=data.frame(annee=1990:2021,
                    composante_cyclique_PIB=hp_pib$cycle,
                    composante_cyclique_MM=c(hp_mm$cycle,NA))

# réalisation du graphique : composantes cycliques : PIB
plot<-ggplot(df_cycle, aes(x=annee)) +   
  geom_line(aes(y = composante_cyclique_PIB,colour="PIB"),size=1.5) + 
  # échelle choisie
  scale_color_manual(values = "red", # couleurs des variables
                     labels = "PIB")+ # labels des variables
  geom_hline(yintercept=0,color='black')+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  theme_light()+
  # légende
  labs(title = "Composantes cycliques - PIB",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "US-dollars (constant 2015)")

plot

# réalisation du graphique : composantes cycliques : Masse monétaire
plot<-ggplot(df_cycle, aes(x=annee)) +   
  geom_line(aes(y = composante_cyclique_MM,colour='Masse monétaire'),size=1.5)+
  # échelle choisie
  scale_color_manual(values = "darkblue")+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  geom_hline(yintercept=0,color='black')+
  theme_light()+
  # légende
  labs(title = "Composantes cycliques - Masse monétaire",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "Composante cyclique - Masse monétaire")

plot





#####################################################################################################
# 3 - Analyse conjointe
#####################################################################################################


# Evolution temporelle des deux variables ###########################################################

# Graphique : Evolution PIB & Masse monétaire
plot <- ggplot(df, aes(annee)) + 
  geom_line(aes(y = PIB,colour="darkblue"),size=1.5) + 
  geom_line(aes(y = masse_monetaire,colour='red'),size=1.5)+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  theme_light()+
  # titres + légende
  labs(title = "Evolution du PIB en volume et de la masse monétaire",
       subtitle = "USA - 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "US-dollars",
       color = "Valeurs" # titre de la légende
  )+
  scale_color_manual(values = c("red","darkblue"), # couleurs des variables
                     labels = c("PIB en volume","Masse monétaire")) # labels des variables
plot

# Evolution temporelle des taux de croissance des deux variables ####################################

plot <- ggplot(df, aes(annee)) + 
  geom_line(aes(y = TC_pib,colour="red"),size=1.5) + 
  geom_line(aes(y = TC_mm,colour='darkblue'),size=1.5)+
  scale_y_continuous(limits=c(-3,18), breaks=seq(-3,18,1))+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  geom_hline(yintercept=0,color='black')+
  # traçage du tcam du PIB
  geom_hline(yintercept=tcam_pib,color='black',linetype="longdash", size=1)+
  annotate(geom="text", x=1991.4, y=3,size=4.5, label=paste0("TCAM PIB = ",round(tcam_pib,2),"%"),
           color="black")+
  # traçage du tcam de la masse monetaire
  geom_hline(yintercept=tcam_mm,color='black',linetype="longdash", size=1)+
  annotate(geom="text", x=1993, y=6.5,size=4.5, label=paste0("TCAM Masse monétaire = ",round(tcam_mm,2),"%"),
           color="black")+
  # titres + légende
  labs(title = "Evolution du taux de croissance du PIB et de la masse monétaire",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "Taux de croissance du PIB et de la masse monétaire en %",
       color = "Valeurs" # titre de la légende
  )+
  scale_color_manual(values = c("darkblue", "red"), # couleurs des variables
                     labels = c("Masse monétaire", "PIB en volume")) # labels des variables

plot

# Filtre HP ########################################################################################

### Choix de trend :
### Trend linéaire :
trend_lin = FALSE
### ou Interpolation polynomiale :
inter_poly = TRUE

plot<-ggplot(df_trend, aes(x=year, y=trend,group=Valeur,color=Valeur)) +   
  geom_line(size=1.5) +
  scale_color_manual(values = c('darkblue','red'))+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  # trend
  {if(trend_lin==TRUE)geom_smooth(method=lm, se=FALSE,color="#4B4B4B",linetype="longdash")}+
  {if(inter_poly==TRUE)geom_smooth(method=lm,formula = y ~ poly(x, 2), se=FALSE,color="#4B4B4B",linetype="longdash")}+
  theme_light()+
  # légende
  labs(title = "Evolution du PIB & Masse monétaire  - Trends du filtre HP",
       subtitle = "USA - 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "Trend en US-dollars")
plot

# Composantes cycliques #####################################################################################

# réalisation du graphique : composantes cycliques : PIB & Masse monétaire
plot<-ggplot(df_cycle, aes(x=annee)) +   
  geom_line(aes(y = composante_cyclique_PIB,colour="PIB"),size=1.5) + 
  geom_line(aes(y = composante_cyclique_MM,colour='Masse monétaire'),size=1.5)+
  # échelle choisie
  scale_color_manual(values = c("darkblue","red"))+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  geom_hline(yintercept=0,color='black')+
  theme_light()+
  # légende
  labs(title = "Composantes cycliques - PIB & Masse monétaire",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "Composante cyclique - PIB & Masse monétaire")

plot

# Composantes cycliques pour le taux de croissance #############################################################

ts_tc_pib <- ts(df$TC_pib,start=c(1990,1),frequency=1)
ts_tc_mm <- ts(na.omit(df$TC_mm),start=c(1990,1),frequency=1)

# Application du filtre Hodrick et Prescott aux séries temporelles
hp_tc_pib <- hpfilter(na.omit(ts_tc_pib))
hp_tc_mm <- hpfilter(na.omit(ts_tc_mm))

df_tc_cycle=data.frame(annee=1991:2021,
                    composante_cyclique_PIB=hp_tc_pib$cycle,
                    composante_cyclique_MM=c(hp_tc_mm$cycle,NA))

plot<-ggplot(df_tc_cycle, aes(x=annee)) +   
  geom_line(aes(y = composante_cyclique_PIB,colour="PIB"),size=1.5) + 
  geom_line(aes(y = composante_cyclique_MM,colour='Masse monétaire'),size=1.5)+
  # échelle choisie
  scale_color_manual(values = c("darkblue","red"))+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  geom_hline(yintercept=0,color='black')+
  theme_light()+
  # légende
  labs(title = "Composantes cycliques - Taux de croissance PIB & Masse monétaire",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Année", y = "Composante cyclique - Taux de croissance PIB & Masse monétaire")

plot

# Test de correlation entre les deux variables #####################################################

# Régression de la variable PIB par la variable masse monétaire

reg <- lm(df$PIB~df$masse_monetaire)
confidence1 <- round(as.numeric(confint(reg)[2]),2) # 2.5 %
confidence2 <- round(as.numeric(confint(reg)[4]),2) # 97.5 %
confidence1
confidence2
reg$coefficients
summary(reg)$coefficients[8] # p-value
summary(reg)$r.squared # R2


# Barplots #########################################################################################

# conversion des données en long format
dflf <- melt(df, id="annee")

# Barplot : comparaison PIB et masse monétaire
barplot <- ggplot(data=dflf[dflf$variable%in%c("PIB","masse_monetaire"),], aes(x=annee, y=value, fill=variable, width=0.7)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  theme_classic()+
  scale_fill_manual(values=c("red", "darkblue"),labels=c('PIB','Masse monétaire'))+
  # légende
  labs(title = "Comparaison : PIB & Masse monétaire ",
       subtitle = "USA : 1990-2021",
       x = 'Année', y = 'PIB & Masse monétaire - en US-dollars',
       caption = paste0("Données : The World Bank "))
barplot

# Barplot : comparaison taux de croissance PIB et masse monétaire
barplot <- ggplot(data=dflf[dflf$variable%in%c("TC_pib","TC_mm"),], aes(x=annee, y=value, fill=variable, width=0.7)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_hline(yintercept=0,color='black')+
  scale_x_continuous(limits=c(1990,2021), breaks=seq(1990,2021,2))+
  scale_y_continuous(limits=c(-6,18), breaks=seq(-6,18,2))+
  theme_classic()+
  scale_fill_manual(values=c("red", "darkblue"),labels=c('Taux de croissance : PIB','Taux de croissance : Masse monétaire'))+
  # légende
  labs(title = "Comparaison : Taux de croissance PIB & Masse monétaire ",
       subtitle = "USA : 1990-2021",
       x = 'Année', y = 'Taux de croissance PIB & Masse monétaire - en %',
       caption = paste0("Données : The World Bank "))
barplot

# Scatterplots #####################################################################################

# Nuage de points : comparaison taux de croissance PIB et masse monétaire
scatterplot <- ggplot(df, aes(x=TC_pib,y=TC_mm)) +   
  geom_point(size=5,color="purple")+
  # échelle choisie
  scale_x_continuous(limits=c(-4,8), breaks=seq(-4,8,2))+
  scale_y_continuous(limits=c(-4,20), breaks=seq(-4,20,2))+
  theme_light()+
  geom_hline(yintercept=0,color='black')+
  geom_vline(xintercept=0,color='black')+
  geom_text(label=df$annee,size=5,hjust=-0.1, vjust=-0.1)+
  # légende
  labs(title = "Taux de croissance du PIB x Variation de la masse monétaire",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Taux de croissance (en %)", y = "Variation de la masse monétaire (en %)")

scatterplot



reg <- lm(df$TC_pib~df$TC_mm)
confidence1 <- round(as.numeric(confint(reg)[2]),2) # 2.5 %
confidence2 <- round(as.numeric(confint(reg)[4]),2) # 97.5 %
reg$coefficients
summary(reg)$coefficients[8] # p-value
summary(reg)$r.squared # R2

# Scatter plot pour : variation du PIB en T+1 et de la masse monétaire en T


scatterplot <- ggplot(df, aes(x=df$TC_pib,y=c(df$TC_mm[2:32],NA))) +   
  geom_point(size=5,color="#FF33AF")+
  # échelle choisie
  scale_x_continuous(limits=c(-4,8), breaks=seq(-4,8,2))+
  scale_y_continuous(limits=c(-4,20), breaks=seq(-4,20,2))+
  theme_light()+
  geom_hline(yintercept=0,color='black')+
  geom_vline(xintercept=0,color='black')+
  geom_smooth(method=lm, se=FALSE,color="#5d5e5e",linetype="longdash",size=1.5)+
  # légende
  labs(title = "Taux de croissance du PIB en t+1 x Variation de la masse monétaire en t",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Taux de croissance (en %)", y = "Variation de la masse monétaire (en %)")

scatterplot

reg <- lm(df$TC_pib~c(df$TC_mm[2:32],NA))
confidence1 <- round(as.numeric(confint(reg)[2]),2) # 2.5 %
confidence2 <- round(as.numeric(confint(reg)[4]),2) # 97.5 %
reg$coefficients
summary(reg)$coefficients[8] # p-value
summary(reg)$r.squared # R2


# Boxplots ######################################################################################

boxplot <- ggplot(dflf[dflf$variable%in%c("TC_pib","TC_mm"),], aes(y=value,x=variable,fill=variable))+
  geom_boxplot(color="black")+
  geom_hline(yintercept=0,color='black')+
  scale_y_continuous(limits=c(-4,20), breaks=seq(-4,20,2))+
  # légende du boxplot
  scale_fill_manual(values=c("red", "darkblue"),labels=c('Taux de croissance : PIB','Taux de croissance : Masse monétaire'))+
  labs(title = "Comparaison des taux de croissance annuels : PIB & Masse monétaire",
       subtitle = "USA : 1990-2021",
       caption = "Données : The World Bank ",
       x = "Variable", y = "Variation annuelle en %")
boxplot

# Output_gap & inflation ###############################################################################

# création de la variable output
df["output_gap"]=df["PIB"]-df_trend[df_trend$Valeur=="PIB",]$trend

plot <- ggplot(df, aes(x=output_gap,y=TC_mm)) +   
  geom_point(size=4.5,color="#1DBC6A")+
  theme_light()+
  geom_hline(yintercept=0,color='black',size=0.4)+
  geom_vline(xintercept=0,color='black',size=0.4)+
  geom_text(label=df$annee,size=4.5,hjust=-0.15, vjust=-0.15)+
  # légende
  labs(title = "Output gap x Variation de la masse monétaire",
       subtitle = "1990-2021",
       caption = "Données : The World Bank ",
       x = "Output gap (filtre HP)", y = "Variation de la masse monétaire (en %)")
plot

# Régression de la variable Ouputgap par la variable taux de croissance de la masse monétaire
reg <- lm(df$output_gap~df$TC_mm)
confidence1 <- round(as.numeric(confint(reg)[2]),2) # 2.5 %
confidence2 <- round(as.numeric(confint(reg)[4]),2) # 97.5 %
reg$coefficients
summary(reg)$coefficients[8]
summary(reg)$r.squared

plot <- ggplot(df, aes(x=output_gap,y=c(df$TC_mm[2:32],NA))) +   
  geom_point(size=4.5,color="#13B8A7")+
  theme_light()+
  geom_smooth(method=lm, se=FALSE,color="#5d5e5e",linetype="longdash",size=1.5)+
  geom_hline(yintercept=0,color='black',size=0.4)+
  geom_vline(xintercept=0,color='black',size=0.4)+
  # légende
  labs(title = "Output gap en t+1 x Variation de la masse monétaire en t",
       subtitle = "1990-2021",
       caption = "Données : The World Bank ",
       x = "Output gap (filtre HP)", y = "Variation de la masse monétaire (en %)")
plot

reg <- lm(df$output_gap~c(df$TC_mm[2:32],NA))
confidence1 <- round(as.numeric(confint(reg)[2]),2) # 2.5 %
confidence2 <- round(as.numeric(confint(reg)[4]),2) # 97.5 %
reg$coefficients
summary(reg)$coefficients[8]
summary(reg)$r.squared
