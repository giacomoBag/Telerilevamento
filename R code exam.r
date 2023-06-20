
# VARIAZIONE DELL'INDICE SPETTRALE IN VAL DI FIEMME
#l'indice di vegetazione coglie il cambio di colore di una pianta che in caso di stress o particolari periodi può cambiare il proprio spettro di emissione
#Nell'ambito di questo elaborato andremo ad osservare la variazione del DVI dei dintorni della val di fiemme in seguito alla tempesta Vaia
#Verranno osservati:
#-DVI
#-NDVI: normalized differetion vegetation index. Consiste nella differenza tra due bande 
# % di foresta persa in seguito all'evento Vaia

install.packages("ggplot2") #analisi ed elaborazione grafica
install.packages("patchwork")
install.packages("raster") #analisi ed elaborazione spaziale
install.packages("rgdal")
library(raster)
library(rgdal)
library(ggplot2)
library(patchwork)

#imposto la working directory
setwd("C:/Users/giaco/OneDrive/Documenti/UNIBO/Telerilevamento geo-ecologico/LabR")

#importiamo e nominiamo le immagini reali e i falsi colori
real2018 <- brick("2018-08-27-00_00_2018-08-27-23_59_Sentinel-2_L2A_True_color.jpg")
real2019 <- brick("2019-06-28-00_00_2019-06-28-23_59_Sentinel-2_L2A_True_color.jpg")
jpeg(filename = "val di fiemme 08_2018-06_2019.jpg", 1200, 1200)
par(mfrow=c(2,1))
plotRGB(real2018,r=1, g=2, b=3, stretch="lin")
plotRGB(real2019,r=1, g=2, b=3, stretch="lin")
dev.off()

#nei falsi colori:
#layer 1 = NIR
#leyer 2 = red
#leyer 3 = green

l2018 <- brick("2018-08-27-00_00_2018-08-27-23_59_Sentinel-2_L2A_False_color.jpg")
l2018
plotRGB(l2018, r=1, g=2, b=3, stretch="lin") 
l2019 <- brick("2019-06-28-00_00_2019-06-28-23_59_Sentinel-2_L2A_False_color.jpg")
l2019
plotRGB(l2019, r=1, g=2, b=3, stretch="lin")
dev.off()

jpeg(filename = "NIR 08_2018-06_2019.jpg", 1400, 1400)
par(mfrow=c(2,2))
plotRGB(l2018, r=1, g=2, b=3, stretch="lin") 
plotRGB(l2019, r=1, g=2, b=3, stretch="lin")
plotRGB(real2018,r=1, g=2, b=3, stretch="lin")
plotRGB(real2019,r=1, g=2, b=3, stretch="lin")
dev.off()

#Calcolo del DVI
#cerchiamo di capire lo stato di salue della vegetazione osservando la differenza tra le bande NIR-red da dove otterremo valori compresi tra 0 e 255

dvi2018 = l2018[[1]] -  l2018[[2]] 
dvi2018
dvi2019 = l2019[[1]] -  l2019[[2]]

#per plottarli
jpeg(filename = "DVI 08_2018-06_2019.jpg", 1400, 1400)
par(mfrow=c(2,2))
cl <- colorRampPalette(c("darkblue", "yellow","red", "black"))(100)
plot(dvi2018, col=cl) 
plotRGB(real2018,r=1, g=2, b=3, stretch="lin")
plot(dvi2019, col=cl)
plotRGB(real2019,r=1, g=2, b=3, stretch="lin")
dev.off()
#l'immagini 2018-2019 mostrano il dvi nelle due annate dove un valore molto basso rappresenta le aree non forestate.


#STANDARDIZZARE il dvi, ovvero facciamo una normalizzazione della somma delle due bande
#se elaborassimo due bande create con un numero di bit differente avremmo una scala di valori molto diversa
#su un immagine a 8 bit i valori possibili vanno da 0 a 255
#su un immagine a 16 bit avremmo invece 65536 valori possibi percui il confronto dei valori e dei colori non avrebbero lo stesso significato

#il range DVI a 8 bit: -255 a 255
#il range NDVI a 8 bit: -1 a 1

#il rage DVI a 16 bit: -65535 a 65535
#il range NDVI a 16 bit: -1 a 1

#con i NDVI posso fare il confronto tra qualsiasi immagine

#NDVI_2018
dvi2018 = l2018 [[1]] - l2018[[2]] #differenza NIR e red

Ndvi2018 = dvi2018 / (l2018 [[1]] + l2018[[2]]) #differenza NIR-red diviso la loro somma
Ndvi2018
dvi2018

#creazione multiframe con immagine falsi colori e l'immagine NDVI
jpeg(filename = "NDVI2018.jpg", 1400, 1400)
par(mfrow=c(2,1))
cl <- colorRampPalette(c("darkblue", "yellow","red", "black"))(100)
plotRGB(l2018, r=1, g=2, b=3, stretch="lin")
plot(Ndvi2018, col=cl)
#nell'immagini il valore minimo -1 è l'acqua (blu) che però non compare poichè l'acqua è ricca in sedimento
dev.off()

#NDVI_2019
Ndvi2019 = dvi2019 / (l2019 [[1]] + l2019[[2]]) 

#creazione multiframe con le immagini NDVI 2018 e 2019


jpeg(filename = "NDVI2018_2019.jpg", 1200, 1200)
par(mfrow=c(1,2))
plot(Ndvi2018, col=cl)
plot(Ndvi2019, col=cl)
dev.off()
#ndvi vicino intorno allo zero rappresentano suoli più spogli da vegetazione

#####################################################################
####################################################################

#proviamo in fine a creare una distribuzione percentuale delle zone colpite dalla tempesta e le zone che invece hanno resistito 
#iniziamo richiamando le immaggini NIR e plottandole

Vdefor1 <- brick("2018-08-27-falsecolor.jpg")
Vdefor2 <- brick("2019-06-28-00_00_2019-06-28-23_59_Sentinel-2_L2A_False_color.jpg")
Vdefor1
Vdefor2

par(mfrow=c(2,1))
plotRGB(Vdefor1, 1, 2, 3, stretch="lin")
plotRGB(Vdefor2, 1, 2, 3, stretch="lin")
dev.off()
#CREO la CLASSIFICAZIONE

#---- Classificazione dell'immagine 2018
# 1. vengono acquisiti i valori nelle immagini da 0 a 255, si creano 3 vettori di numeri
singlenr1 <- getValues(Vdefor1)
singlenr1
# 2. vengono i valori dei vettori vengono assegnati a 2 classi
kcluster1 <- kmeans(singlenr1, centers = 2)
kcluster1

# 3. creeamoo l'immagine
defor1class <- setValues(Vdefor1[[1]], kcluster1$cluster) # i valori delle classi vengono assegnati al raster

plot(defor1class)

# class1: foresta
# class2: suolo

#---- Classificazione dell'immagine 2019

# 1. 
singlenr2 <- getValues(Vdefor2)
singlenr2
# 2. 
kcluster2 <- kmeans(singlenr2, centers = 2)
kcluster2

# 3. 
defor2class <- setValues(Vdefor2[[1]], kcluster2$cluster) 

plot(defor2class)

# class1: foresta
# class2: suolo

#plotto i risultati ottenuti

par(mfrow=c(2,1))
plot(defor1class)
plot(defor2class)

#--- percentuali delle classi 2018

frequencies1 <- freq(defor1class)
frequencies1

tot1 <- ncell(defor1class)
tot1

percentages1 <- frequencies1 * 100 / tot1
percentages1

# foresta: 93.79
# suolo: 6.20

#--- percentuali delle classi 2019

frequencies2 <- freq(defor2class)
frequencies2

tot2 <- ncell(defor2class)
tot2

percentages2 <- frequencies2 * 100 / tot2
percentages2

# foresta: 93.10
# suolo: 6.90

# distribuzione percentuale delle classi
cover <- c("Forest","Bare soil")
percent2018 <- c(93.80, 6.20)
percent2019 <- c(93.10, 6.90)

percentages <- data.frame(cover, percent2018, percent2019)
percentages

# utilizziamo ggplot2 per la creazione di grafici

ggplot(percentages, aes(x=cover, y=percent2018, color=cover)) +
  geom_bar(stat="identity", fill="white")

ggplot(percentages, aes(x=cover, y=percent2019, color=cover)) +
  geom_bar(stat="identity", fill="white")

# patchwork
p1 <- ggplot(percentages, aes(x=cover, y=percent2018, color=cover)) +
  geom_bar(stat="identity", fill="white") +
  ggtitle("Year 2018")

p2 <- ggplot(percentages, aes(x=cover, y=percent2019, color=cover)) +
  geom_bar(stat="identity", fill="white") + 
  ggtitle("Year 2019")

p1 + p2

#confronto statistico con medesima scala

p1 <- ggplot(percentages, aes(x=cover, y=percent2018, color=cover)) +
  geom_bar(stat="identity", fill="white") +
  ggtitle("Year 2018") +
  ylim(c(0,100))

p2 <- ggplot(percentages, aes(x=cover, y=percent2019, color=cover)) +
  geom_bar(stat="identity", fill="white") + 
  ggtitle("Year 2019") +
  ylim(c(0,100))

p1 + p2

