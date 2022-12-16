setwd('/Users/juanmatillavaras/Desktop/Master\ Data\ Science/TFM/Data/')

library(dplyr)
library(tidyverse)
library(readxl)
library(stringi)
library(sp)
library(rgdal)
library(zoo)
library(usethis)
library(sf)

#Descarga datos Ookla del tercer trimestre del año 2022.
use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=fixed/year=2022/quarter=3/2022-07-01_performance_fixed_tiles.zip", destdir = "data")

#Lectura fichero de secciones censales descargado del INE.
secs <- read_sf("/Users/juanmatillavaras/Desktop/Master\ Data\ Science/TFM/Espana_Seccionado2022_ETRS89H30/SECC_CE_20220101.shp")


###################################################
#Procesado de los datos de renta descargados del INE
##################################################

#Leemos el fichero
renta <- read_xls('30824.xls')

#Eliminamos filas y columnas que no nos interesan
renta_def <- renta[-(1:7),-(14:37)]

#Cambiamos el nombre a la primera columna
colnames(renta_def)[1] <- 'separar'

#Separamos en dos columnas el código y el nombre
renta_def <- separate(renta_def, separar, into = c('cod', 'name'), sep = '^\\S*\\K\\s+')

#Nos quedamos con los registros que tengan 10 digitos en el código que son las secciones censales.
renta_def <- renta_def[(which(nchar(renta_def$cod) == 10)), ]

#Seleccionamos las columnas con con el codigo y los ultimos datos de renta disponibles
renta_def <- renta_def %>% select(1, 3, 9) %>% set_names('CUSEC', 'avg_renta_persona_20', 'avg_renta_hogar_20')

#Creamos el fichero unificado con el dataset de secciones censales
renta_secs <- left_join(secs, renta_def, by=c('CUSEC'='CUSEC'))

#Exportamos en geojson
sf::st_write(renta_secs, 'renta_secs.geojson')


#############################################################
#Procesado de los datos de censo electoral descargados del INE
#############################################################

#Creamos un diccionario de nombres de provincia y sus códigos.
provs_cods <- data.frame(prov = c('Araba/Álava', 'Albacete', 'Alicante / Alacant', 'Almería', 'Ávila', 'Badajoz', 'Balears, Illes', 'Barcelona', 'Burgos', 'Cáceres',
                                  'Cádiz', 'Castellón / Castelló', 'Ciudad Real', 'Córdoba', 'Coruña, A', 'Cuenca', 'Girona', 'Granada', 'Guadalajara', 'Gipuzkoa', 
                                  'Huelva', 'Huesca', 'Jaén', 'León', 'Lleida', 'Rioja, La', 'Lugo', 'Madrid', 'Málaga', 'Murcia', 
                                  'Navarra', 'Ourense', 'Asturias', 'Palencia', 'Palmas, Las', 'Pontevedra', 'Salamanca', 'Santa Cruz de Tenerife', 'Cantabria', 'Segovia',
                                  'Sevilla', 'Soria', 'Tarragona', 'Teruel', 'Toledo', 'Valencia / València', 'Valladolid', 'Bizkaia', 'Zamora', 'Zaragoza',
                                  'Ceuta', 'Melilla'),
                         cod_prov = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20',
                                      '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '40',
                                      '41', '42', '43', '44', '45', '46', '47', '48', '49', '50', '51', '52'))

#Lectura del fichero
censo <- read_xls('33527.xls')

#Eliminamos las primeras filas que no nos interesan
censo_def <- censo[-(1:7),]

#Renombramos columnas
colnames(censo_def) <- c('age', 'cer_muj', 'cer_hom', 'cer_tot', 'cera_muj', 'cera_hom', 'cera_tot', 'muj_tot', 'hom_tot', 'tot')

#Rellenamos la fila que contiene el nombre de la provincia con los datos de Total
censo_def <- na.locf(censo_def, na.rm = FALSE, fromLast = TRUE)

#Creamos una columna para tener el código de cada provincia
censo_def$cod_prov <- provs_cods$cod_prov[match(censo_def$age, provs_cods$prov)]

#Aignamos ese código a todas las filas
censo_def <- na.locf(censo_def, na.rm = FALSE)

#Eliminamos las filas que contengan 'Total'
censo_def <- censo_def[!grepl('Total', censo_def$age),]

#Cambiamos el nombre de las provincias por 'Total'
censo_def <- censo_def %>% mutate(age = ifelse(age %in% provs_cods$prov, 'Total', age))

#Pivotamos la tabla para tener las edades como variables
censo_def <- censo_def %>% pivot_wider(names_from = age, values_from = c(cer_muj, cer_hom, cer_tot, cera_muj, cera_hom, cera_tot, muj_tot, hom_tot, tot))

#Reemplazamos espacios
names(censo_def) <- gsub(names(censo_def), pattern = ' a ', replacement = '_')                                          
         
#Creamos el fichero unificado con el dataset de secciones censales                      
censo_secs <- left_join(secs, censo_def, by=c('CPRO'='cod_prov'))

#Exportamos en geojson
sf::st_write(censo_secs, 'censo_secs.geojson')                                
                                            
