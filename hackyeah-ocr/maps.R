#install.packages('rgdal')
#install.packages("GISTools")

library(maptools)
library(rgdal)

ogrListLayers(dsn="POWIATY.shp")
powiatyOGR <- readOGR("POWIATY.shp") 

powiaty_map <- readShapePoly("POWIATY")

iconv(powiaty_map$NAZWA, from="windows-1250", to="UTF-8")

sort(as.character(powiaty_map$NAZWA))


# konwertujemy nazwy kolumn
colnames(diagnozaOsoby2011) <- iconv(colnames(diagnozaOsoby2011), from="windows-1250", to="ASCII//TRANSLIT")

library(stringi)
stri_encode(powiaty_map$NAZWA, "", "UTF-8")

