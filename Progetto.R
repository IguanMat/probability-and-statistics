# FISH
# prca: prezzo per i compratori Asiatici
# prcw: prezzo per i compratori bianchi
# qtya: quantità venduta agli Asiatici
# qtyw: quantità venduta ai bianchi
# mon: 1 se venduto di Lunedì
# tues: 1 se venduto di Martedì
# wed: 1 se venduto di Mercoledì
# thurs: 1 se venduto di Giovedì
# speed2: velocità minima del vento negli ultimi 2 giorni
# wave2: altezza media delle onde negli ultimi 2 giorni
# speed3: velocità massima del vento ritardata di 3 giorni
# wave3: altezza massima media ritardata di 3 e 4 giorni
# avgprc: prezzo medio
# totqty: quantità totale
# lavgprc: logaritmo del prezzo medio
# ltotqty: logaritmo della quantità totale
# t: trend temporale
# lavgp_1: ?
# gavgprc: ?
# gavgp_1: ?

#install.packages("wooldridge")
library("wooldridge")
data(fish)
View(fish)
attach(fish)

qqnorm(lavgp_1)
qqline(lavgp_1)
qqnorm(gavgp_1)
qqline(gavgp_1)

fish <- fish[, -c(18,19,20)]

fry <- c(0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
fish <- cbind(fish, fry)

nvariable <- c("prca", "prcw", "qtya", "qtyw", "mon", "tues", "wed", "thurs", "fry", "speed2", "wave2", "speed3", "wave3", "avgprc", "totqty", "lavgprc", "ltotqty", "t")
fish <- fish[nvariable]

summary(fish)

#install.packages("ggplot2")
library("ggplot2")

ggplot()+
  geom_line(data = fish, mapping = aes(x=t, y=qtya), color="orange")+
  geom_point(data = fish, mapping = aes(x=t, y=qtya), color="orange")+
  geom_line(data = fish, mapping = aes(x=t, y=qtyw), color="blue")+
  geom_point(data = fish, mapping = aes(x=t, y=qtyw), color="blue")+  
  labs(subtitle="Quantità venduta agli Asiatici Vs Quantità venduta ai bianchi", x="Trend temporale",y="Quantità venduta agli Asiatici (arancione), Quantità venduta ai bianchi (blu)", title="Grafico delle serie temporali", caption = "Fonte: fish")

ggplot()+
  geom_line(data = fish, mapping = aes(x=t, y=prca), color="orange")+
  geom_point(data = fish, mapping = aes(x=t, y=prca), color="orange")+
  geom_line(data = fish, mapping = aes(x=t, y=prcw), color="blue")+
  geom_point(data = fish, mapping = aes(x=t, y=prcw), color="blue")+
  labs(subtitle="Prezzo per i compratori Asiatici Vs Prezzo per i compratori bianchi", x="Trend temporale",y="Prezzo per i compratori Asiatici (arancione), Prezzo per i compratori bianchi (blu)",  title="Grafico delle serie temporali", caption = "Fonte: fish")

t.test(fish$prca, fish$prcw)

par(mfrow=c(2,2))
hist(avgprc, main = "Istogramma del prezzo medio", col = "lightgreen")
hist(lavgprc, main = "Istogramma del logaritmo del prezzo medio", col = "lightyellow")
hist(totqty, main = "Istogramma della quantità totale venduta", col = "lightblue")
hist(ltotqty, main= "Istogramma del logaritmo della quantità totale venduta",col = "pink")

# Il boxplot dà le stesse informazioni sulla forma della distribuzione che ci dà l'istogramma
boxplot(avgprc, main = "Boxplot del prezzo medio")
boxplot(lavgprc, main = "Boxplot del logaritmo del prezzo medio")
boxplot(totqty, main = "Boxplot della quantità totale venduta")
boxplot(ltotqty, main = "Boxplot del logaritmo della quantità totale venduta")

#####
#install.packages("gamlss")
library(gamlss)
histDist(lavgprc,"NO")
histDist(lavgprc, "TF")
#####

par(mfrow=c(1,1))
qqnorm(lavgprc)
qqline(lavgprc)

shapiro.test(lavgprc)

#install.packages("ggpubr")
library(ggpubr)

b1 <- ggplot(data = fish, aes(y=qtya)) +
  geom_boxplot() +
  facet_wrap(~mon+tues+wed+thurs, nrow=1) +
  labs(subtitle="In ordine: Venerdì, Giovedì, Mercoledì, Martedì, Lunedì", y="Quantità venduta agli Asiatici", x="Giorni della settimana",  title="Boxplot della quantità di pesce venduta agli Asiatici nell'arco di una settimana", caption = "Fonte: fish")

b2 <- ggplot(data = fish, aes(y=qtyw)) +
  geom_boxplot() +
  facet_wrap(~mon+tues+wed+thurs, nrow=1) +
  labs(subtitle="In ordine: Venerdì, Giovedì, Mercoledì, Martedì, Lunedì", y="Quantità venduta ai bianchi", x="Giorni della settimana",  title="Boxplot della quantità di pesce venduta ai bianchi nell'arco di una settimana", caption = "Fonte: fish")

ggarrange(b1, b2, nrow = 2, labels = "AUTO")

pwave2 <- ggplot(fish, aes(x=wave2, y=avgprc)) + 
  geom_point(aes(col=speed2, size=totqty)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Altezza media delle onde negli ultimi 2 giorni Vs Prezzo medio", y="Prezzo medio", x="Altezza media delle onde negli ultimi 2 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

pspeed3 <- ggplot(fish, aes(x=speed3, y=avgprc)) + 
  geom_point(aes(col=wave3, size=totqty)) + 
  geom_smooth(method="loess", se=F) +  
  labs(subtitle="Velocità massima del vento prevista tra 3 giorni Vs Prezzo medio", y="Prezzo medio", x="Velocità massima del vento prevista tra 3 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

pspeed2 <- ggplot(fish, aes(x=speed2, y=avgprc)) + 
  geom_point(aes(col=wave2, size=totqty)) + 
  geom_smooth(method="loess", se=F) +  
  labs(subtitle="Velocità minima del vento negli utlimi 2 giorni Vs Prezzo medio", y="Prezzo medio", x="Velocità minima del vento negli ultimi 2 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

pwave3 <- ggplot(fish, aes(x=wave3, y=avgprc)) + 
  geom_point(aes(col=speed3, size=totqty)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Altezza massima media delle onde prevista tra 3 giorni Vs Prezzo medio", y="Prezzo medio", x="Altezza massima media delle onde prevista tra 3 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

ggarrange(pspeed2, pspeed3, nrow = 2) 
ggarrange(pwave2, pwave3, nrow = 2) 

qwave2 <- ggplot(fish, aes(x=wave2, y=totqty)) + 
  geom_point(aes(col=speed2, size=avgprc)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Altezza media delle onde negli ultimi 2 giorni Vs Quantità totale venduta", y="Quantità totale venduta", x="Altezza media delle onde negli ultimi 2 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

qspeed3 <- ggplot(fish, aes(x=speed3, y=totqty)) + 
  geom_point(aes(col=wave3, size=avgprc)) + 
  geom_smooth(method="loess", se=F) +  
  labs(subtitle="Velocità massima del vento prevista tra 3 giorni Vs Quantità totale venduta", y="Quantità totale venduta", x="Velocità massima del vento prevista tra 3 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

qspeed2 <- ggplot(fish, aes(x=speed2, y=totqty)) + 
  geom_point(aes(col=wave2, size=avgprc)) + 
  geom_smooth(method="loess", se=F) +  
  labs(subtitle="Velocità minima del vento negli utlimi 2 giorni Vs Quantità totale venduta", y="Quantità totale venduta", x="Velocità minima del vento negli utlimi 2 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

qwave3 <- ggplot(fish, aes(x=wave3, y=totqty)) + 
  geom_point(aes(col=speed3, size=avgprc)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Altezza massima media delle onde prevista tra 3 giorni Vs Quantità totale venduta", y="Quantità totale venduta", x="Altezza massima media delle onde prevista tra 3 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

ggarrange(qspeed2, qspeed3, nrow = 2) 
ggarrange(qwave2, qwave3, nrow = 2) 

cor.test(speed2, avgprc)
cor.test(wave2, avgprc)
cor.test(speed3, avgprc)
cor.test(wave3, avgprc)
cor.test(speed2, totqty)
cor.test(wave2, totqty)
cor.test(speed3, totqty)
cor.test(wave3, totqty)
cor.test(totqty, avgprc)
cor.test(wave2, speed2)

#install.packages("ggcorplot")
library(ggcorrplot)
# Matrice di correlazione
corr <- round(cor(fish), 1)

ggcorrplot(corr, 
           type = "full", 
           lab = T, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogramma di fish",
           ggtheme=theme_bw)

summary(lm(avgprc~speed2+wave2+speed3+wave3))
summary(lm(totqty~speed2+wave2+speed3+wave3))
summary(lm(wave2~speed2))

rsw <- ggplot(fish, aes(x=speed2, y=wave2)) + 
  geom_point(aes(col=speed2, size=wave2)) + 
  geom_smooth(method="lm")
  
rpwave2 <- ggplot(fish, aes(x=wave2, y=avgprc)) + 
  geom_point(aes(col=speed2, size=totqty)) + 
  geom_smooth(method="lm") + 
  labs(subtitle="Altezza media delle onde negli ultimi 2 giorni Vs Prezzo medio", y="Prezzo medio", x="Altezza media delle onde negli ultimi 2 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

rpspeed3 <- ggplot(fish, aes(x=speed3, y=avgprc)) + 
  geom_point(aes(col=wave3, size=totqty)) + 
  geom_smooth(method="lm") +  
  labs(subtitle="Velocità massima del vento prevista tra 3 giorni Vs Prezzo medio", y="Prezzo medio", x="Velocità massima del vento prevista tra 3 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

rpspeed2 <- ggplot(fish, aes(x=speed2, y=avgprc)) + 
  geom_point(aes(col=wave2, size=totqty)) + 
  geom_smooth(method="lm") +  
  labs(subtitle="Velocità minima del vento negli utlimi 2 giorni Vs Prezzo medio", y="Prezzo medio", x="Velocità minima del vento negli ultimi 2 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

rpwave3 <- ggplot(fish, aes(x=wave3, y=avgprc)) + 
  geom_point(aes(col=speed3, size=totqty)) + 
  geom_smooth(method="lm") + 
  labs(subtitle="Altezza massima media delle onde prevista tra 3 giorni Vs Prezzo medio", y="Prezzo medio", x="Altezza massima media delle onde prevista tra 3 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

ggarrange(rpspeed2, rpspeed3, nrow = 2) 
ggarrange(rpwave2, rpwave3, nrow = 2) 

rqwave2 <- ggplot(fish, aes(x=wave2, y=totqty)) + 
  geom_point(aes(col=speed2, size=avgprc)) + 
  geom_smooth(method="lm") + 
  labs(subtitle="Altezza media delle onde negli ultimi 2 giorni Vs Quantità totale venduta", y="Quantità totale venduta", x="Altezza media delle onde negli ultimi 2 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

rqspeed3 <- ggplot(fish, aes(x=speed3, y=totqty)) + 
  geom_point(aes(col=wave3, size=avgprc)) + 
  geom_smooth(method="lm") +  
  labs(subtitle="Velocità massima del vento prevista tra 3 giorni Vs Quantità totale venduta", y="Quantità totale venduta", x="Velocità massima del vento prevista tra 3 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

rqspeed2 <- ggplot(fish, aes(x=speed2, y=totqty)) + 
  geom_point(aes(col=wave2, size=avgprc)) + 
  geom_smooth(method="lm") +  
  labs(subtitle="Velocità minima del vento negli utlimi 2 giorni Vs Quantità totale venduta", y="Quantità totale venduta", x="Velocità minima del vento negli utlimi 2 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

rqwave3 <- ggplot(fish, aes(x=wave3, y=totqty)) + 
  geom_point(aes(col=speed3, size=avgprc)) + 
  geom_smooth(method="lm") + 
  labs(subtitle="Altezza massima media delle onde prevista tra 3 giorni Vs Quantità totale venduta", y="Quantità totale venduta", x="Altezza massima media delle onde prevista tra 3 giorni", title="Grafico a dispersione", caption = "Fonte: fish")

ggarrange(rqspeed2, rqspeed3, nrow = 2) 
ggarrange(rqwave2, rqwave3, nrow = 2) 