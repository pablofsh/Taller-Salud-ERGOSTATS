# Cargar librerias --------------------------------------------------------

library(sf)           # Manipulación de datos espaciales
library(tidyverse)    # Manipulación de datos 
library(hereR)        # Paquete para acceder a la API HERE
library(osmdata)      # Paquete para descargar datos de OpenStreetMaps (OSM)
library(leaflet)      # Crear mapas interactivos


# Importar datos ----------------------------------------------------------

ruta <- "../data/gpkg/base_map_dmq.gpkg"
mapa_base_dmq <- st_read(ruta,
                         layer = "dmq")


# Transformación de los datos ---------------------------------------------

# Obtener los centroides de cada zona censal
centroides_dmq <- st_centroid(mapa_base_dmq) %>% 
  # Transformar de UTM a Geograficas 
  st_transform(4326)


# Crear Isocronas ---------------------------------------------------------

# Configurar la llave de HERE
# here_key <- ""
# set_key(here_key)

# Crear una carpeta para guardar los shapefiles de las isocronas
dir.create("../data/shp/isocronas/", 
           showWarnings = FALSE)

# Bluce para crear un conjunto de isocronas de 0-10 y 10-20 min por cada centroide
for(i in 1:nrow(centroides_dmq)){
  
  isocrona <- isoline(
    # Punto(s) desde donde parte la isocrona. Debe ser un objeto sf
    poi = centroides_dmq[1,],
    
    # Vector que contiene los intervalos de corte en segundos (10 y 20 min) 
    range = c(10,20)*60,
    
    # Indica el tipo de rango: tiempo, distancia o energia 
    range_type = "time",
    
    # Fecha y tiempo del calculo para simular trafico en tiempo real 
    datetime = as.POSIXct(paste0(Sys.Date(), " 10:00"))
    
  ) %>%
    # Le asignamos una columna nueva para identificar cada nivel de las isocronas
    mutate(name = paste0((range - 600) / 60," to ", range / 60, " mins")) 
  
  # Guardar poligonos en carpeta isocronas
  dsn <- paste0("../data/shp/isocronas/")
  
  # Asignar un nombre a cada archivo shapefile
  layer <- centroides_dmq$zon[i]
  
  st_write(obj = isocrona,
           dsn = dsn,
           layer = layer,
           driver = "ESRI Shapefile",
           append = TRUE)
  
  # Añadir una pausa de 1 segundo al crear los archivos para prevenir sobrecarga
  pause(1)
}


# Obtener las ubicaciones de los hospitales en el DMQ ---------------------

hospitales_sf <- 
  # Obtenemos la zona de donde queremos consultar
  getbb("Quito Ecuador") %>% 
  
  # Creamos la consulta 
  opq() %>% 
  
  # Especificamos las caracteristicas deseadas de OSM
  add_osm_feature(key = "amenity",
                  value = "hospital") %>% 
  
  # Especificamos el formato de salida de los datos como sf
  osmdata_sf()


# Obtener los centroides de los hospitales --------------------------------

hospitales_centroides <- hospitales_sf$osm_polygons %>%
  # Seleccionamos los nombres de los hospitales asi como su geomtria 
  select(name, geometry) %>% 
  
  # Obtenemos los centroides de los poligonos
  st_centroid() %>% 
  
  # Se omiten las filas sin nombre
  na.omit()


# Crear la matriz de influencia  ------------------------------------------


# Ruta donde se encuentran las isocronas
isocronas_lista <- "../data/shp/isocronas/"

# Ruta completa de cada isocrona
isocrona_ruta <- list.files(path = isocronas_lista,
                            pattern = ".shp",
                            full.names = TRUE)

# Nombres de cada sector censal para cada isocrona
shape.names <- list.files(path = isocronas_lista,
                          pattern = ".shp",
                          full.names = FALSE) %>%
  str_remove(".shp")

# Creamos una lista vacia donde guardar nuestra operacion 
lista <- list()

# Iteramos 
for(i in seq_along(isocrona_ruta)){
  
  # Leer el shapefile i dentro de la carpeta isocronas
  shapefile <- st_read(isocrona_ruta[i])
  
  # Guardamos el resultado de la sobreposicion de los hospitales por cada shapefile en la lista 
  lista[[i]] <- st_join(hospitales_centroides,
                        shapefile, 
                        join = st_within) %>% 
    
    # Agrupar por identificador de hospital
    group_by(name.x) %>%  
    
    # Selccionamos solo un hospital por cada isocrona 
    slice_min(order_by = name.y, n = 1, with_ties = FALSE) %>%  
    ungroup() %>% 
    
    # Seleccionamos los nombres de los hospitales y rango temporal 
    select(Hospital = name.x, name.y) %>% 
    
    # Nombramos las columnas segun el indice i 
    rename_with(.cols = name.y, 
                .fn = ~shape.names[i]) %>% 
    as_tibble()
}

# Combinamos todos los elementos de la lista en un solo data frame usando un left_join
matriz_completa <- reduce(lista, left_join)


# Crear la función de impedancia ------------------------------------------

# Funcion gausiana (impedancia)
impedancia <- function(mintiempo, subzonatiempo, maxtiempo){
  beta <- -((maxtiempo-mintiempo)^2)/log(0.01)
  peso <- exp(-(subzonatiempo-mintiempo)^2/beta)
  peso <- round(peso, 2)
  return(peso)
}

# Crear pesos para la matriz de pesos
peso_NA <- 0
peso_10 <- impedancia(mintiempo = 5, subzonatiempo =  5, maxtiempo = 20)
peso_20 <- impedancia(mintiempo = 5, subzonatiempo = 15, maxtiempo = 20)

# Definir los nombres de las subzonas en la matriz de pesos
sub1 <- "0 to 10 mins"
sub2 <- "10 to 20 mins"




