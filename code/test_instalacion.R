library(sf)
library(tidyverse)
library(leaflet)

# Unidades de salud A, B y C
unidad_salud <- read_csv("data/cs_pichincha_isocronas_input.csv")

# Convertirlas a objetos sf
unidad_salud_sf <- st_as_sf(
  unidad_salud,
  coords = c("lon", "lat"),  # primero X (longitud), luego Y (latitud)
  crs = 4326                 # WGS84
)


leaflet(unidad_salud_sf) |>
  addTiles() |>
  addCircleMarkers(
    radius = 5,
    color = "red"
  )
