# fvtt-module-creator
fvtt-module-creator v1.1

# Instrucciones

Instalar R

[The R Project for Statistical Computing](https://www.r-project.org/ "R Project")

(Opcional) Instalar una GUI para R, por ejemplo, esta:

[RStudio](https://www.rstudio.com/ "RStudio")

En R, primero instalamos algunos paquetes que son necesarios:

```
install.packages("jsonlite")
install.packages("tidyverse")
```

Se descarga el respositorio a un directorio del disco duro y desde ahí podéis ejecutar la función *module.creator* con los parámetros necesarios, se ha incluido un ejemplo de creación de módulo en el repositorio (fvtt-module-creator - ejemplo.R), su contenido es este:

# Importante

La especificación de directorios ha de hacerse usando como separador /, y no \, de modo que C:\Users\Manolo es incorrecto, lo correcto es C:/Users/Manolo

```
library(jsonlite)
library(tidyverse)

setwd("directorio/donde/esta/fvtt-module-creator - functions.R")
# setwd("H:/R/fvtt-module-creator")

source("fvtt-module-creator - functions.R", encoding="ISO-8859-1")

module.creator(world = "mis-compendios", # nombre de la carpeta del mundo de origen
               module = "compendios-mios-es", # nombre de la carpeta del modulo destino
               title = "[ES] Compendios mios", # título del modulo destino
               description = "Compendios mios", # descripción del modulo destino
               author = "Yo", # autor
               copy = c("icons", "pdfs", "images/animales"), # carpetas que queréis copiar del mundo al módulo, que contendrán imágenes, o pdfs que han sido usados y enlazados en los compendios del mundo
               foundrydata = "D:/games/foundrydata-v10", # carpeta data de foundry
               foundryversion = 10, # versión de foundry, soporta 9 y 10
               compendiumfolders = "configcompendiumfolders.txt", # fichero de compendium folders que hemos usado en el mundo para poner carpetas y colores a los compendios; si no se ha usado compendium folders, no usar este parámetro
               clean.descriptions=F, # si se quieren borrar las descripciones de todos los objetos antes de copiarlos
               remove.compendiums = c("spells", "creatures", "tables") # si queréis evitar que algún compendio se copie del mundo al módulo
)
```
