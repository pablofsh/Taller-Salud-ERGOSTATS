# ==============================================================================
# Procesamiento RAS 2021 - Índice de Accesibilidad a la Salud (Pichincha)
# Versión tidyverse — código verificado paso a paso
# ==============================================================================
# NOTA METODOLÓGICA (Guía INEC, Sección 11):
# "A partir de 2018 en adelante: ya no es necesario aplicar la sintaxis de
# ponderación, ya que las bases de datos contienen las ponderaciones y las
# variables pueden utilizarse directamente."
# "Al final del cálculo del número de profesionales de la salud, se debe
# redondear el resultado obtenido."
# ==============================================================================

library(tidyverse)
library(haven)     # Para as_factor() con variables labelled del INEC

# ==============================================================================
# BLOQUE 1: Cargar datos
# ==============================================================================

ras <- readRDS("Base_de_Datos_RAS_2021_R/RAS_2021.rds")

# ==============================================================================
# BLOQUE 2: Filtrar Pichincha y traducir códigos a nombres
# ==============================================================================
# Las variables del RAS son haven_labelled (formato SPSS): cada valor numérico
# tiene una etiqueta incorporada. haven::as_factor() extrae esas etiquetas
# directamente, sin necesidad de diccionarios externos ni joins.

ras_pichincha <- ras %>% 
  mutate(
    # Extraer nombres legibles ANTES de convertir a character
    nombre_canton    = as.character(as_factor(cant_ubi)),
    nombre_parroquia = as.character(as_factor(parr_ubi)),
    nombre_clase     = as.character(as_factor(clase)),
    nombre_entidad   = as.character(as_factor(entidad)),
    nombre_sector    = as.character(as_factor(sector)),
    nombre_tipo      = as.character(as_factor(tipo)),
    nombre_area      = as.character(as_factor(area)),
    nombre_region    = as.character(as_factor(region)),
    # Convertir códigos a character limpio
    prov_ubi = as.character(unclass(prov_ubi)),
    cant_ubi = as.character(unclass(cant_ubi)),
    parr_ubi = as.character(unclass(parr_ubi)),
    clase    = as.character(unclass(clase)),
    entidad  = as.character(unclass(entidad)),
    sector   = as.character(unclass(sector)),
    tipo     = as.character(unclass(tipo)),
    area     = as.character(unclass(area)),
    region   = as.character(unclass(region))
  ) %>% 
  filter(prov_ubi == "17")

# Verificación
cat("Establecimientos en Pichincha:", nrow(ras_pichincha), "\n")
cat("NAs nombre_clase:", sum(is.na(ras_pichincha$nombre_clase)), "\n")
cat("NAs nombre_entidad:", sum(is.na(ras_pichincha$nombre_entidad)), "\n")
cat("NAs nombre_parroquia:", sum(is.na(ras_pichincha$nombre_parroquia)), "\n")

# ==============================================================================
# BLOQUE 3: Calcular profesionales por especialidad
# ==============================================================================
# Los valores en las columnas k ya están ponderados por el INEC (desde 2018).
# Solo sumamos directamente y redondeamos. NO aplicamos FTE adicional.

# Función reutilizable: suma las 4 franjas horarias y redondea
agregar_especialidad <- function(df, nombre, v8, v6, v4, v_menos4) {
  df %>% 
    mutate(
      across(all_of(c(v8, v6, v4, v_menos4)), ~replace_na(as.numeric(.), 0)),
      "{nombre}" := round(.data[[v8]] + .data[[v6]] + .data[[v4]] + .data[[v_menos4]])
    )
}

ras_pichincha <- ras_pichincha %>% 
  agregar_especialidad("medicina_general",  "k7",   "k8",   "k9",   "k10") %>% 
  agregar_especialidad("medicina_familiar", "k152", "k153", "k154", "k155") %>% 
  agregar_especialidad("pediatria",         "k127", "k128", "k129", "k130") %>% 
  agregar_especialidad("odontologia",       "k995", "k996", "k997", "k998") %>% 
  agregar_especialidad("medicina_interna",  "k22",  "k23",  "k24",  "k25") %>% 
  # Ginecología/Obstetricia: suma ginecólogos (k137-k140) + obstetrices (k202-k205)
  mutate(
    across(all_of(c("k137","k138","k139","k140",
                    "k202","k203","k204","k205")), ~replace_na(as.numeric(.), 0)),
    ginecologia_obstetricia = round(
      (k137 + k202) + (k138 + k203) + (k139 + k204) + (k140 + k205)
    )
  )

# ==============================================================================
# BLOQUE 4: Exportar resultados
# ==============================================================================

# Tabla a nivel de establecimiento (máxima granularidad)
establecimientos <- ras_pichincha %>% 
  select(
    secuencial, prov_ubi, cant_ubi, nombre_canton, parr_ubi, nombre_parroquia,
    clase, nombre_clase, entidad, nombre_entidad,
    sector, nombre_sector, tipo, nombre_tipo, area, nombre_area, region, nombre_region,
    medicina_general, medicina_familiar, pediatria,
    ginecologia_obstetricia, odontologia, medicina_interna
  )

write_csv(establecimientos, "capacidad_pichincha_establecimientos.csv")
cat("Exportado: capacidad_pichincha_establecimientos.csv —", nrow(establecimientos), "filas ×", ncol(establecimientos), "columnas\n")

# Tabla agregada por parroquia
parroquias <- establecimientos %>% 
  group_by(parr_ubi, nombre_parroquia, cant_ubi, nombre_canton) %>% 
  summarise(
    n_establecimientos = n(),
    across(medicina_general:medicina_interna, sum),
    .groups = "drop"
  )

write_csv(parroquias, "capacidad_pichincha_parroquias.csv")
cat("Exportado: capacidad_pichincha_parroquias.csv —", nrow(parroquias), "parroquias\n")

cat("\n¡Proceso finalizado exitosamente!\n")