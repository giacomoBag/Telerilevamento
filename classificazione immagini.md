# classificazione immagini
#passare da dati continui a delle classi (che possono essere mineralogiche o animali..etc)
#l'immagine satellitare si compone di una serie di pixel,
#in ogni piexel ci sarà un valore di riflettanza tipica riconducibile ad una certa caretteristica (suolo, vegetazione, urbanizzazione, acqua)
#ogni pixel sarà associato alla classe che più "gli assomiglia"

library(raster)
# install.packages("RStoolbox")
library(RStoolbox)

setwd("~/UNIBO/Telerilevamento geo-ecologico/LabR") 

# data import
so <- brick("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")

plotRGB(so, 1, 2, 3, stretch="lin")
plotRGB(so, 1, 2, 3, stretch="hist")

# Classifying the solar data 
soc <- unsuperClass(so, nClasses=3)

cl <- colorRampPalette(c('yellow','black','red'))(100)
plot(soc$map, col=cl)

# set.seed can be used for repeating the experiment in the same manner for N times
# http://rfunction.com/archives/62 

###############################################################
#############################################################

# day 2 Grand Canyon

gc <- brick("dolansprings_oli_2013088_canyon_lrg.jpg")
gc

# rosso = 1
# verde = 2
# blu = 3

plotRGB(gc, r=1, g=2, b=3, stretch="lin")
#l'immagine presa è nel visibile e quindi plottando RGB otterremo un immagine nel reale,
#se così non foss dovremo cambiare il posizionamento di 1, 2, e 3 in r, g e b

# change the stretch to histogram stretching
plotRGB(gc, r=1, g=2, b=3, stretch="hist")

# classification
gcclass2 <- unsuperClass(gc, nClasses=2)
gcclass2

plot(gcclass2$map)
# set.seed(17)

# Exercise: classify the map with 4 classes
gcclass4 <- unsuperClass(gc, nClasses=4)
gcclass4

clc <- colorRampPalette(c('yellow','red','blue','black'))(100)
plot(gcclass4$map, col=clc)

# compare the classified map with the original set
par(mfrow=c(2,1))
plot(gcclass4$map, col=clc)
plotRGB(gc, r=1, g=2, b=3, stretch="hist")
