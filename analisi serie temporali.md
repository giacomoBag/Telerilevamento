# ANALISI SERIE TEMPORALI
# GROELLANDIA
#######################################################

#LEZIONE 01/04/2022


#LST:land surface temperature

library(raster)
setwd("~/UNIBO/Telerilevamento geo-ecologico/LabR/greenland")
# importiamo tutte le immagini dalla cartella in cui abbiamo 4 dati diversi;
#lst 2000
#lst 2005
#lst 2015
#..etc..

lst2000 <- raster("lst_2000.tif")
lst2000 #è un immagine a 16bit perchè i values arrivano a 65535
plot(lst2000) #vedo la temperature della groellandia

# IMPORTO UN DATO ALLA VOLTA
lst2005 <- raster("lst_2005.tif")
lst2010 <- raster("lst_2010.tif")
lst2015 <- raster("lst_2015.tif")

cl <- colorRampPalette(c("blue", "light blue", "pink", "red")) (100)

par(mfrow=c(2,2))
plot(lst2000, col=cl)
plot(lst2005, col=cl)
plot(lst2010, col=cl)
plot(lst2015, col=cl)
dev.off()
############################################################
# ALTERNATIVA PIù BELLA

# IMPORTARE TUTTE LE IMMAGNI INSIEME
# vogliamo applicare una funzione ad un intera lista di file utilizzando "lapply"
# ad esempio applicare la funzione raster a tutte le mio immagini della cartella groellandia

# creamo una lista di file
rlist <- list.files(pattern="lst") #prende tutti i file che presentano "lst" nel proprio nome

# poi applico lapply sulla lista
import <- lapply(rlist, raster)#alla lista "rlist" applico la funzione raster


# da questa lista di dati posso farne un unico file dove metto tutti i layer utilizzando la funzione Stack
tgr <- stack(import)
tgr
# utilizzando meno righe di codice lavoro su più layer contemporaneamente

plot(tgr, col=cl) #plotta tutt4 e 4 i layer, occhio a non aver apero altro prima che lo incasina
dev.off()
plot(tgr[[1]], col=cl)
dev.off()
# posso anche giocarci nel plotrgb:
plotRGB (tgr, r=1, g=2, b=3, stretch="lin") #in questo caso avviene la sovrapposizione dei layer presenti in tgr
# si ottiene una serie di dati invece che una serie di bande e queste immagini le si sovreppoone
# si osservano i punti più scuri generalmente più freddi ed i punti più chiari più caldi
#################################################
####################################################
###################################################à
# ANALISI DI SERIE TEMPORALI: EN
#### Example 2: NO2 decrease during the lockdown period
##########################

library(raster)
setwd("~/UNIBO/Telerilevamento geo-ecologico/LabR/EN")

# impoerto due immagini su 13 delle presenti nella cartella
en01 <- raster("EN_0001.png") 

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(en01, col=cl)

en13 <- raster("EN_0013.png")
plot(en13, col=cl)

# imprto tutte le immagini presenti nella cartella 

# Exercise: import the whole as in the Greenland example
# by the following steps: list.files, lapply, stack 

rlist <- list.files(pattern="EN") #estraggo i file che abbiano un pattern comune

# lapply(X,FUN)
rimp <- lapply(rlist, raster)

# stack mi permetterà si mettere insieme le immagini e plottarle in seguito tutti insieme
en <- stack(rimp)

# posso plottare tutto 
plot(en, col=cl)

# oppure decidere di plottare solo alcuni [[elementi]] presenti in en 
par(mfrow=c(1,2))
plot(en[[1]], col=cl)
plot(en[[13]], col=cl)

# or:
en113 <- stack(en[[1]], en[[13]])
plot(en113, col=cl)

# let's make the difference:
difen <-  en[[1]] - en[[13]]
cldif <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difen, col=cldif)

# plotRGB of three files together
plotRGB(en, r=1, g=7, b=13, stretch="lin")
plotRGB(en, r=1, g=7, b=13, stretch="hist")









