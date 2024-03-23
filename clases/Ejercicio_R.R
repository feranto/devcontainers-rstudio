# dir <- "C:\\Users\\carli\\OneDrive\\Documents\\AS10\\AS10 Material de apoyo\\R\\Paquete estadistico R\\Ficheros de datos"
# setwd(dir)

# Matriz de datos da3240, del Bar?metro de octubre de 2009
anchos <- c(-9, 2, 2, -25, 1, -2, 1, -147, 1, 2)
nvar <- c('ccaa', 'prov', 'siteco', 'sitpol', 'sexo', 'edad')
d3240 <- read.fwf('DA3240', widths=anchos, col.names=nvar)
d3240[1:20,]; head(d3240) # La instrucci?n head saca por pantalla las primeras filas de la hoja de datos
names(d3240)
#---------------------------------------------------
# CONVERSI?N DE VARIABLES NUM?RICAS EN FACTORES
#---------------------------------------------------
b <- d3240
# Conversion de las variables en factores con bucle for
# (convertimos todas las variables excepto 'edad', que es la sexta y ?ltima)
for (i in 1:5) b[ , i] <- as.factor(b[ , i])
levels(b$siteco)
#---------------------------------------------
# MEDIDAS DE POSICI?N, DISPERSI?N Y FORMA
#---------------------------------------------
ed <- b$edad
ed <- ed[ed < 99]
#--------------
# Tarea 1
#--------------
ed2 <- ed
# calcular la media
media <- mean(ed2)
print(media)



#-------------------------------------------------------------------
# IMPORTACI?N DE DATOS DESDE FICHEROS DE SPSS (Ficheros .sav)
#-------------------------------------------------------------------
#library(foreign)
# Si queremos que los factores nos aparezcan con su literal
#d <- read.spss('3240.sav', to.data.frame=T)
#d[1:20, 1:10]

#--------------
# Tarea 2
#--------------

#--------------
# Tarea 3
#--------------
