# =============================================================
# analisis_accesibilidad.R
# E2SFCA Spatial Accessibility Analysis — Pichincha Province
# Authors: Alex Bajaña & Gary Santiana — ERGOSTATS
# Sources: CPV2022, ACESS, RAS
# =============================================================

# -------------------------------------------------------------
# 0. Libraries
# -------------------------------------------------------------
library(sf)
library(tidyverse)
library(lubridate)
library(hereR)
library(maptiles)     # free basemap tiles (no API key)
library(tidyterra)    # geom_spatraster for maptiles output
library(ggspatial)    # north arrow + scale bar
library(patchwork)    # panel layouts
library(scales)       # number formatting
library(gt)           # publication tables
library(gtExtras)     # gt extras

# Output directory
dir.create("output/maps",   showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# Slide dimensions (16:9, 300 dpi)
W <- 13.33
H <- 7.5
DPI <- 300

# Shared caption
CAPTION <- "Sources: CPV2022 · ACESS · RAS | Elaboración: los autores"

# Shared theme for choropleth maps
tema_mapa <- theme_void(base_family = "sans") +
  theme(
    legend.position      = "right",
    legend.title         = element_text(size = 9, face = "bold"),
    legend.text          = element_text(size = 8),
    legend.key.height    = unit(0.9, "cm"),
    legend.key.width     = unit(0.35, "cm"),
    plot.caption         = element_text(size = 7, colour = "grey50",
                                        hjust = 0, margin = margin(t = 6)),
    plot.margin          = margin(4, 4, 4, 4)
  )

# Quintile palette (matches reference figure: red → yellow-green)
QUINTILE_COLORS <- c(
  "Q1 (lowest)" = "#D73027",
  "Q2"          = "#FC8D59",
  "Q3"          = "#FEE090",
  "Q4"          = "#A6D96A",
  "Q5 (highest)"= "#1A9641"
)

# Helper: assign quintile labels
quintile_label <- function(x) {
  cut(x,
      breaks = quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE),
      labels = names(QUINTILE_COLORS),
      include.lowest = TRUE)
}

# -------------------------------------------------------------
# 1. Data preparation
# -------------------------------------------------------------

unidad_salud <- read_csv("data/cs_pichincha_isocronas_input.csv")

unidad_salud_sf <- st_as_sf(
  unidad_salud,
  coords = c("lon", "lat"),
  crs    = 4326
)

unidad_salud_sf_metros <- st_transform(unidad_salud_sf, 32717)

rm("unidad_salud")

df <- readRDS("data/rds/poblacion_poligonos_pichincha.rds")
df <- st_transform(df, crs = 4326)
df <- st_make_valid(df)

prr <- df %>% group_by(parroqu) %>% summarise()

centroides_dmq <- st_centroid(df) %>%
  st_transform(4326) %>%
  select(sec, poblacion)

centroides_dmq_metros <- st_centroid(df) %>%
  select(sec, poblacion)

# -------------------------------------------------------------
# 2. Basemap — OpenStreetMap tiles via maptiles (no API key)
# -------------------------------------------------------------

# maptiles downloads OpenStreetMap tiles and returns a SpatRaster
# that tidyterra renders with geom_spatraster_rgb() inside ggplot2.
# provider = "OpenStreetMap" is free and requires no registration.

# Reemplaza el bloque get_tiles del basemap provincial por esto:
# Forzar R a usar su propio PROJ, ignorando PostgreSQL
Sys.setenv(PROJ_LIB = system.file("proj", package = "sf"))
Sys.setenv(GDAL_DATA = system.file("gdal", package = "sf"))

# Luego prueba de nuevo
bbox_sf <- st_as_sfc(st_bbox(df)) %>% st_set_crs(4326)

basemap <- get_tiles(
  x        = bbox_sf,
  provider = "OpenStreetMap",
  zoom     = 10,
  crop     = TRUE
)



# For the facility-level zoom map we build it on demand inside each map block.

# -------------------------------------------------------------
# 3. Isochrones
# -------------------------------------------------------------

# --- Generation loop (eval=FALSE — reference only) ----------
# hereR::set_key("YOUR_KEY_HERE")
# resultados <- list(); contador <- 1
# for (i in 1:nrow(unidad_salud_sf)) {
#   tryCatch({
#     isocrona <- isoline(
#       poi            = unidad_salud_sf[i, ],
#       range          = seq(10, 30, 10) * 60,
#       range_type     = "time",
#       datetime       = ymd_hms("2026-05-13 13:00:00"),
#       traffic        = TRUE,
#       transport_mode = "car"
#     ) %>% mutate(
#       hospital_id = unidad_salud_sf$unicodigo[i],
#       minutos     = range / 60,
#       banda       = case_when(
#         minutos == 10 ~ "0-10",
#         minutos == 20 ~ "10-20",
#         minutos == 30 ~ "20-30"
#       )
#     )
#     resultados[[contador]] <- isocrona; contador <- contador + 1
#     Sys.sleep(1)
#   }, error = function(e) message("ERROR facility ", i, ": ", e))
# }
# bind_rows(resultados) %>%
#   st_write("data/gpkg/isocronas_clean.gpkg", layer = "isocronas",
#            delete_layer = TRUE)
# ------------------------------------------------------------

iso <- st_read("data/gpkg/isocronas_clean.gpkg")

# Coverage — isochrones
cobertura_iso <- iso %>%
  st_set_geometry("geom") %>%
  st_union() %>%
  st_make_valid() %>%
  st_as_sf() %>%
  filter(!st_is_empty(x)) %>%
  st_collection_extract("POLYGON") %>%
  st_union()

cobertura_iso_sf <- st_as_sf(data.frame(geometry = cobertura_iso))

fuera_iso <- centroides_dmq %>%
  filter(!lengths(st_intersects(., cobertura_iso)))

dentro_iso <- centroides_dmq %>%
  filter(lengths(st_intersects(., cobertura_iso)) > 0)

# -------------------------------------------------------------
# 4. Buffers
# -------------------------------------------------------------

distancias <- c(5000, 10000, 15000)

buffers_final <- purrr::map_dfr(
  distancias,
  ~ st_buffer(unidad_salud_sf_metros, dist = .x) %>%
    mutate(
      distancia_m  = .x,
      distancia_km = .x / 1000,
      banda        = case_when(
        distancia_km == 5  ~ "0-5",
        distancia_km == 10 ~ "5-10",
        distancia_km == 15 ~ "10-15"
      )
    )
)

cobertura_buf <- buffers_final %>%
  st_union() %>%
  st_make_valid() %>%
  st_transform(4326)

cobertura_buf_sf <- st_as_sf(data.frame(geometry = cobertura_buf))

fuera_buf <- centroides_dmq %>%
  filter(!lengths(st_intersects(., cobertura_buf)))

dentro_buf <- centroides_dmq %>%
  filter(lengths(st_intersects(., cobertura_buf)) > 0)

# -------------------------------------------------------------
# 5. Supply
# -------------------------------------------------------------

parroquias <- read_csv("data/capacidad_pichincha_parroquias.csv") %>%
  mutate(
    medicos  = medicina_general + medicina_familiar + pediatria +
               ginecologia_obstetricia + odontologia + medicina_interna,
    parr_ubi = as.character(parr_ubi)
  )

unidad_interseccion <- st_join(unidad_salud_sf, df, join = st_intersects)

parroquias_medicos <- unidad_interseccion %>%
  select(hospital_id = unicodigo, parroqu) %>%
  left_join(parroquias, by = c("parroqu" = "parr_ubi")) %>%
  select(hospital_id, parroqu, medicos)

isocronas <- iso %>%
  left_join(as_tibble(parroquias_medicos), by = "hospital_id") %>%
  st_set_geometry("geom")

buffers <- buffers_final %>%
  left_join(
    as_tibble(parroquias_medicos) %>% rename(unicodigo = hospital_id),
    by = "unicodigo"
  )

# -------------------------------------------------------------
# 6. OD matrices
# -------------------------------------------------------------

interseccion_iso <- st_join(
  centroides_dmq, isocronas, join = st_intersects
) %>%
  group_by(sec, hospital_id) %>%
  arrange(minutos, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

interseccion_buffers <- st_join(
  centroides_dmq_metros,
  st_transform(buffers, st_crs(centroides_dmq_metros)),
  join = st_intersects
) %>%
  group_by(sec, unicodigo) %>%
  arrange(banda, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# -------------------------------------------------------------
# 7. Impedance weights
# -------------------------------------------------------------

impedancia <- function(mintiempo, subzonatiempo, maxtiempo) {
  beta <- -((maxtiempo - mintiempo)^2) / log(0.01)
  round(exp(-(subzonatiempo - mintiempo)^2 / beta), 2)
}

peso_iso_10 <- impedancia(5,  5, 30)
peso_iso_20 <- impedancia(5, 15, 30)
peso_iso_30 <- impedancia(5, 25, 30)

interseccion_iso <- interseccion_iso %>%
  mutate(w = case_when(
    banda == "0-10"  ~ peso_iso_10,
    banda == "10-20" ~ peso_iso_20,
    banda == "20-30" ~ peso_iso_30,
    TRUE             ~ NA_real_
  ))

peso_buf_5  <- impedancia(5,  2.5, 15)
peso_buf_10 <- impedancia(5,  7.5, 15)
peso_buf_15 <- impedancia(5, 12.5, 15)

interseccion_buffers <- interseccion_buffers %>%
  mutate(w = case_when(
    banda == "0-5"   ~ peso_buf_5,
    banda == "5-10"  ~ peso_buf_10,
    banda == "10-15" ~ peso_buf_15,
    TRUE             ~ NA_real_
  ))

# -------------------------------------------------------------
# 8. E2SFCA index
# -------------------------------------------------------------

# --- Isochrones ---
step1_iso <- interseccion_iso %>%
  st_drop_geometry() %>%
  filter(!is.na(hospital_id)) %>%
  distinct(hospital_id, sec, .keep_all = TRUE) %>%
  group_by(hospital_id) %>%
  summarise(poblacion_atendida = sum(poblacion * w, na.rm = TRUE), .groups = "drop")

ratios_iso <- isocronas %>%
  as_tibble() %>%
  distinct(hospital_id, medicos) %>%
  left_join(step1_iso, by = "hospital_id") %>%
  mutate(ratio_final = medicos / poblacion_atendida)

accesibilidad_iso <- interseccion_iso %>%
  st_drop_geometry() %>%
  filter(!is.na(hospital_id)) %>%
  left_join(ratios_iso %>% select(hospital_id, ratio_final), by = "hospital_id") %>%
  mutate(contribucion = ratio_final * w) %>%
  group_by(sec) %>%
  summarise(accesibilidad = sum(contribucion, na.rm = TRUE), .groups = "drop") %>%
  mutate(indice = accesibilidad / mean(accesibilidad, na.rm = TRUE))

# --- Buffers ---
step1_buffers <- interseccion_buffers %>%
  st_drop_geometry() %>%
  filter(!is.na(unicodigo)) %>%
  distinct(unicodigo, sec, .keep_all = TRUE) %>%
  group_by(unicodigo) %>%
  summarise(poblacion_atendida = sum(poblacion * w, na.rm = TRUE), .groups = "drop")

ratios_buffers <- buffers %>%
  as_tibble() %>%
  distinct(unicodigo, medicos) %>%
  left_join(step1_buffers, by = "unicodigo") %>%
  mutate(ratio_final = medicos / poblacion_atendida)

accesibilidad_buffers <- interseccion_buffers %>%
  st_drop_geometry() %>%
  filter(!is.na(unicodigo)) %>%
  left_join(ratios_buffers %>% select(unicodigo, ratio_final), by = "unicodigo") %>%
  mutate(contribucion = ratio_final * w) %>%
  group_by(sec) %>%
  summarise(accesibilidad = sum(contribucion, na.rm = TRUE), .groups = "drop") %>%
  mutate(indice = accesibilidad / mean(accesibilidad, na.rm = TRUE))

# --- Join to polygons ---
iso_sf <- df %>% left_join(accesibilidad_iso, by = "sec") %>%
  mutate(quintil = quintile_label(indice))

buf_sf <- df %>% left_join(accesibilidad_buffers, by = "sec") %>%
  mutate(quintil = quintile_label(indice))

# =============================================================
# MAPS
# =============================================================

# Helper: add facilities layer (small enough to see polygons below)
capa_unidades <- function(p) {
  p +
    geom_sf(
      data  = unidad_salud_sf,
      size  = 0.9,
      shape = 21,
      fill  = "white",
      color = "#1a1a2e",
      stroke = 0.4
    )
}

# Helper: north arrow + scale bar
elementos_cartograficos <- function(p) {
  p +
    annotation_north_arrow(
      location = "tl",
      which_north = "true",
      style = north_arrow_minimal(text_size = 7)
    ) +
    annotation_scale(
      location = "bl",
      width_hint = 0.25,
      text_cex = 0.6
    )
}

# -------------------------------------------------------------
# MAP 1 — Population per census zone
# TITLE: "Population distribution — Pichincha census zones (CPV2022)"
# -------------------------------------------------------------

p_poblacion <- ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = df, aes(fill = poblacion),
          colour = "white", linewidth = 0.08,
          alpha = 0.78, inherit.aes = FALSE) +
  scale_fill_distiller(
    palette  = "YlOrRd",
    direction = 1,
    name     = "Population\nper zone",
    labels   = comma_format(big.mark = ",")
  ) +
  tema_mapa +
  labs(caption = CAPTION)

p_poblacion <- capa_unidades(p_poblacion)
p_poblacion <- elementos_cartograficos(p_poblacion)

ggsave("output/maps/01_poblacion.png",
       p_poblacion, width = W, height = H, dpi = DPI, bg = "white")

# -------------------------------------------------------------
# MAP 2 — Isochrone coverage
# TITLE: "Isochrone catchment coverage — PHC facilities, Pichincha"
# -------------------------------------------------------------

p_cobertura_iso <- ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data  = cobertura_iso_sf,
          fill  = "#2166ac", colour = NA, alpha = 0.3,
          inherit.aes = FALSE) +
  geom_sf(data  = prr,
          fill  = NA, colour = "grey40", linewidth = 0.2,
          inherit.aes = FALSE) +
  geom_sf(data  = fuera_iso,
          colour = "#d73027", size = 0.5, alpha = 0.7,
          inherit.aes = FALSE) +
  tema_mapa +
  labs(caption = CAPTION)

p_cobertura_iso <- capa_unidades(p_cobertura_iso)
p_cobertura_iso <- elementos_cartograficos(p_cobertura_iso)

ggsave("output/maps/02_cobertura_isocronas.png",
       p_cobertura_iso, width = W, height = H, dpi = DPI, bg = "white")

# -------------------------------------------------------------
# MAP 3 — Buffer coverage
# TITLE: "Buffer catchment coverage (0–15 km) — PHC facilities, Pichincha"
# -------------------------------------------------------------

p_cobertura_buf <- ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data  = cobertura_buf_sf,
          fill  = "#d6604d", colour = NA, alpha = 0.3,
          inherit.aes = FALSE) +
  geom_sf(data  = prr,
          fill  = NA, colour = "grey40", linewidth = 0.2,
          inherit.aes = FALSE) +
  geom_sf(data  = fuera_buf,
          colour = "#d73027", size = 0.5, alpha = 0.7,
          inherit.aes = FALSE) +
  tema_mapa +
  labs(caption = CAPTION)

p_cobertura_buf <- capa_unidades(p_cobertura_buf)
p_cobertura_buf <- elementos_cartograficos(p_cobertura_buf)

ggsave("output/maps/03_cobertura_buffers.png",
       p_cobertura_buf, width = W, height = H, dpi = DPI, bg = "white")

# -------------------------------------------------------------
# MAP 4 — Side-by-side: isochrone vs buffer for one facility
# TITLE: "Catchment area comparison — isochrone vs buffer, facility 1"
# -------------------------------------------------------------

facility_id    <- unidad_salud_sf$unicodigo[1]
iso_ejemplo    <- iso %>% filter(hospital_id == facility_id) %>%
                    mutate(banda = factor(banda, levels = c("0-10","10-20","20-30")))
buffer_ejemplo <- buffers_final %>%
                    filter(unicodigo == facility_id) %>%
                    st_transform(4326) %>%
                    mutate(banda = factor(banda, levels = c("0-5","5-10","10-15")))

basemap_fac <- get_tiles(
  x        = iso_ejemplo,
  provider = "OpenStreetMap",
  zoom     = 13,
  crop     = TRUE
)

colores_bandas_iso <- c("0-10" = "#2166ac", "10-20" = "#74add1", "20-30" = "#abd9e9")
colores_bandas_buf <- c("0-5"  = "#d6604d", "5-10"  = "#f4a582", "10-15" = "#fddbc7")

fac_punto <- unidad_salud_sf %>% filter(unicodigo == facility_id)

p_fac_iso <- ggplot() +
  geom_spatraster_rgb(data = basemap_fac) +
  geom_sf(data = iso_ejemplo, aes(fill = banda),
          colour = "white", linewidth = 0.3, alpha = 0.55,
          inherit.aes = FALSE) +
  scale_fill_manual(values = colores_bandas_iso, name = "Travel time") +
  geom_sf(data = fac_punto, size = 2.5, shape = 21,
          fill = "white", colour = "black", stroke = 0.6,
          inherit.aes = FALSE) +
  tema_mapa +
  labs(subtitle = "Isochrones") +
  theme(plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5))

p_fac_buf <- ggplot() +
  geom_spatraster_rgb(data = basemap_fac) +
  geom_sf(data = buffer_ejemplo, aes(fill = banda),
          colour = "white", linewidth = 0.3, alpha = 0.45,
          inherit.aes = FALSE) +
  scale_fill_manual(values = colores_bandas_buf, name = "Distance") +
  geom_sf(data = fac_punto, size = 2.5, shape = 21,
          fill = "white", colour = "black", stroke = 0.6,
          inherit.aes = FALSE) +
  tema_mapa +
  labs(subtitle = "Buffers") +
  theme(plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5))

p_comparacion <- p_fac_iso + p_fac_buf +
  plot_annotation(caption = CAPTION,
                  theme = theme(plot.caption = element_text(size = 7,
                                colour = "grey50", hjust = 0)))

ggsave("output/maps/04_comparacion_catchment.png",
       p_comparacion, width = W, height = H, dpi = DPI, bg = "white")

# -------------------------------------------------------------
# MAP 5 — E2SFCA index by quintiles — isochrones
# TITLE: "E2SFCA spatial accessibility index (isochrones) — Pichincha"
# -------------------------------------------------------------

p_indice_iso <- ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = iso_sf %>% filter(!is.na(quintil)),
          aes(fill = quintil),
          colour = "white", linewidth = 0.05, alpha = 0.82,
          inherit.aes = FALSE) +
  geom_sf(data = iso_sf %>% filter(is.na(indice)),
          fill = "grey80", colour = "white", linewidth = 0.05, alpha = 0.5,
          inherit.aes = FALSE) +
  geom_sf(data = prr, fill = NA, colour = "grey30", linewidth = 0.25,
          inherit.aes = FALSE) +
  scale_fill_manual(
    values = QUINTILE_COLORS,
    name   = "Accessibility\nquintile",
    na.value = "grey80",
    drop = FALSE
  ) +
  tema_mapa +
  labs(caption = CAPTION)

p_indice_iso <- capa_unidades(p_indice_iso)
p_indice_iso <- elementos_cartograficos(p_indice_iso)

ggsave("output/maps/05_indice_isocronas.png",
       p_indice_iso, width = W, height = H, dpi = DPI, bg = "white")

# -------------------------------------------------------------
# MAP 6 — E2SFCA index by quintiles — buffers
# TITLE: "E2SFCA spatial accessibility index (buffers) — Pichincha"
# -------------------------------------------------------------

p_indice_buf <- ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = buf_sf %>% filter(!is.na(quintil)),
          aes(fill = quintil),
          colour = "white", linewidth = 0.05, alpha = 0.82,
          inherit.aes = FALSE) +
  geom_sf(data = buf_sf %>% filter(is.na(indice)),
          fill = "grey80", colour = "white", linewidth = 0.05, alpha = 0.5,
          inherit.aes = FALSE) +
  geom_sf(data = prr, fill = NA, colour = "grey30", linewidth = 0.25,
          inherit.aes = FALSE) +
  scale_fill_manual(
    values = QUINTILE_COLORS,
    name   = "Accessibility\nquintile",
    na.value = "grey80",
    drop = FALSE
  ) +
  tema_mapa +
  labs(caption = CAPTION)

p_indice_buf <- capa_unidades(p_indice_buf)
p_indice_buf <- elementos_cartograficos(p_indice_buf)

ggsave("output/maps/06_indice_buffers.png",
       p_indice_buf, width = W, height = H, dpi = DPI, bg = "white")

# -------------------------------------------------------------
# MAP 7 — Side-by-side quintile comparison
# TITLE: "E2SFCA accessibility index: isochrones vs buffers — Pichincha"
# -------------------------------------------------------------

p_panel_iso <- ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = iso_sf %>% filter(!is.na(quintil)),
          aes(fill = quintil),
          colour = "white", linewidth = 0.04, alpha = 0.82,
          inherit.aes = FALSE) +
  geom_sf(data = prr, fill = NA, colour = "grey30", linewidth = 0.2,
          inherit.aes = FALSE) +
  scale_fill_manual(values = QUINTILE_COLORS, name = "Quintile", drop = FALSE) +
  tema_mapa +
  labs(subtitle = "Isochrones") +
  theme(plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.position = "none")

p_panel_buf <- ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = buf_sf %>% filter(!is.na(quintil)),
          aes(fill = quintil),
          colour = "white", linewidth = 0.04, alpha = 0.82,
          inherit.aes = FALSE) +
  geom_sf(data = prr, fill = NA, colour = "grey30", linewidth = 0.2,
          inherit.aes = FALSE) +
  scale_fill_manual(values = QUINTILE_COLORS, name = "Quintile", drop = FALSE) +
  tema_mapa +
  labs(subtitle = "Buffers") +
  theme(plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5))

p_panel <- p_panel_iso + p_panel_buf +
  plot_layout(guides = "collect") +
  plot_annotation(caption = CAPTION,
                  theme = theme(
                    plot.caption = element_text(size = 7, colour = "grey50", hjust = 0),
                    legend.position = "right"
                  ))

ggsave("output/maps/07_panel_quintiles.png",
       p_panel, width = W, height = H, dpi = DPI, bg = "white")

# -------------------------------------------------------------
# MAP 8 — Low-access zones comparison (Q1 zones highlighted)
# TITLE: "Zones in the lowest accessibility quintile (Q1): isochrones vs buffers"
# -------------------------------------------------------------

q1_iso <- iso_sf %>% filter(quintil == "Q1 (lowest)")
q1_buf <- buf_sf %>% filter(quintil == "Q1 (lowest)")

p_q1 <- ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = prr, fill = NA, colour = "grey50", linewidth = 0.2,
          inherit.aes = FALSE) +
  geom_sf(data = q1_iso, fill = "#D73027", colour = "white",
          linewidth = 0.05, alpha = 0.7, inherit.aes = FALSE) +
  geom_sf(data = q1_buf, fill = "#F97B22", colour = "white",
          linewidth = 0.05, alpha = 0.5, inherit.aes = FALSE) +
  geom_sf(data = unidad_salud_sf, size = 1, shape = 21,
          fill = "white", colour = "#1a1a2e", stroke = 0.5,
          inherit.aes = FALSE) +
  scale_colour_identity() +
  annotate("text", x = -Inf, y = -Inf,
           label = "Red: Q1 isochrones  |  Orange: Q1 buffers",
           hjust = -0.05, vjust = -0.5, size = 3, colour = "grey30") +
  tema_mapa +
  labs(caption = CAPTION)

p_q1 <- elementos_cartograficos(p_q1)

ggsave("output/maps/08_zonas_baja_accesibilidad.png",
       p_q1, width = W, height = H, dpi = DPI, bg = "white")

# -------------------------------------------------------------
# MAP 9 — Housing density  [PLACEHOLDER — uncomment when data available]
# TITLE: "Housing density per census zone — Pichincha (CPV2022)"
# -------------------------------------------------------------
# df_viv <- df %>% left_join(viviendas, by = "sec") %>%
#   mutate(densidad_viv = n_viviendas / (Shap_Ar / 1e6))   # viviendas per km²
#
# p_viviendas <- ggplot() +
#   geom_spatraster_rgb(data = basemap) +
#   geom_sf(data = df_viv, aes(fill = densidad_viv),
#           colour = "white", linewidth = 0.08, alpha = 0.78,
#           inherit.aes = FALSE) +
#   scale_fill_distiller(palette = "Blues", direction = 1,
#                        name = "Dwellings\nper km²",
#                        labels = comma_format()) +
#   tema_mapa + labs(caption = CAPTION)
# p_viviendas <- capa_unidades(p_viviendas)
# p_viviendas <- elementos_cartograficos(p_viviendas)
# ggsave("output/maps/09_densidad_viviendas.png",
#        p_viviendas, width = W, height = H, dpi = DPI, bg = "white")

# =============================================================
# TABLES
# =============================================================

# -------------------------------------------------------------
# TABLE 1 — Population summary
# -------------------------------------------------------------

tbl_poblacion <- df %>%
  as_tibble() %>%
  summarise(
    `Census zones`        = n(),
    `Total population`    = sum(poblacion, na.rm = TRUE),
    `Mean pop. per zone`  = mean(poblacion, na.rm = TRUE),
    `Median pop. per zone`= median(poblacion, na.rm = TRUE),
    `Min`                 = min(poblacion, na.rm = TRUE),
    `Max`                 = max(poblacion, na.rm = TRUE),
    `SD`                  = sd(poblacion, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Value") %>%
  mutate(Value = round(Value, 1))

gt_tbl1 <- tbl_poblacion %>%
  gt() %>%
  tab_header(title = "Population — Pichincha census zones (CPV2022)") %>%
  fmt_number(columns = Value, decimals = 0) %>%
  tab_source_note("Source: CPV2022 | Elaboración: los autores")

gtsave(gt_tbl1, "output/tables/01_poblacion.html")

# -------------------------------------------------------------
# TABLE 2 — Housing density  [PLACEHOLDER]
# -------------------------------------------------------------
# tbl_viviendas <- df_viv %>%
#   as_tibble() %>%
#   summarise(
#     `Census zones`             = n(),
#     `Total dwellings`          = sum(n_viviendas, na.rm = TRUE),
#     `Mean density (dwell/km²)` = mean(densidad_viv, na.rm = TRUE),
#     `Median density`           = median(densidad_viv, na.rm = TRUE),
#     `Min`                      = min(densidad_viv, na.rm = TRUE),
#     `Max`                      = max(densidad_viv, na.rm = TRUE)
#   ) %>%
#   pivot_longer(everything(), names_to = "Metric", values_to = "Value") %>%
#   mutate(Value = round(Value, 1))

# -------------------------------------------------------------
# TABLE 3 — Physician distribution
# -------------------------------------------------------------

tbl_medicos <- parroquias %>%
  summarise(
    `Parishes`              = n(),
    `Total physicians`      = sum(medicos, na.rm = TRUE),
    `Mean per parish`       = mean(medicos, na.rm = TRUE),
    `Median per parish`     = median(medicos, na.rm = TRUE),
    `Min`                   = min(medicos, na.rm = TRUE),
    `Max`                   = max(medicos, na.rm = TRUE),
    `General medicine`      = sum(medicina_general, na.rm = TRUE),
    `Family medicine`       = sum(medicina_familiar, na.rm = TRUE),
    `Pediatrics`            = sum(pediatria, na.rm = TRUE),
    `OB-GYN`                = sum(ginecologia_obstetricia, na.rm = TRUE),
    `Dentistry`             = sum(odontologia, na.rm = TRUE),
    `Internal medicine`     = sum(medicina_interna, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Value") %>%
  mutate(Value = round(Value, 1))

gt_tbl3 <- tbl_medicos %>%
  gt() %>%
  tab_header(title = "Physician supply by specialty — Pichincha parishes") %>%
  fmt_number(columns = Value, decimals = 0) %>%
  tab_source_note("Sources: ACESS · RAS | Elaboración: los autores")

gtsave(gt_tbl3, "output/tables/03_medicos.html")

# -------------------------------------------------------------
# TABLE 4 — Coverage counts: inside / outside catchments
# -------------------------------------------------------------

tbl_cobertura <- tibble(
  Method    = c("Isochrones", "Isochrones", "Buffers", "Buffers"),
  Status    = c("Inside coverage", "Outside coverage",
                "Inside coverage", "Outside coverage"),
  `N zones` = c(nrow(dentro_iso), nrow(fuera_iso),
                nrow(dentro_buf), nrow(fuera_buf)),
  `Population` = c(
    sum(dentro_iso$poblacion, na.rm = TRUE),
    sum(fuera_iso$poblacion,  na.rm = TRUE),
    sum(dentro_buf$poblacion, na.rm = TRUE),
    sum(fuera_buf$poblacion,  na.rm = TRUE)
  )
) %>%
  group_by(Method) %>%
  mutate(
    `% zones` = round(100 * `N zones` / sum(`N zones`), 1),
    `% pop`   = round(100 * Population / sum(Population), 1)
  ) %>%
  ungroup()

gt_tbl4 <- tbl_cobertura %>%
  gt(groupname_col = "Method") %>%
  tab_header(title = "Catchment coverage: zones and population inside / outside") %>%
  fmt_number(columns = c(`N zones`, Population), decimals = 0) %>%
  tab_source_note("Sources: CPV2022 · ACESS · RAS | Elaboración: los autores")

gtsave(gt_tbl4, "output/tables/04_cobertura.html")

# -------------------------------------------------------------
# TABLE 5 — E2SFCA index summary statistics
# -------------------------------------------------------------

resumen_indice <- function(acc_df, method_label) {
  acc_df %>%
    summarise(
      Method             = method_label,
      `N zones (index)`  = sum(!is.na(indice)),
      `Regional mean`    = mean(indice, na.rm = TRUE),
      `Median`           = median(indice, na.rm = TRUE),
      `SD`               = sd(indice, na.rm = TRUE),
      `Min`              = min(indice, na.rm = TRUE),
      `Max`              = max(indice, na.rm = TRUE),
      `Q1 threshold`     = quantile(indice, 0.20, na.rm = TRUE),
      `Q5 threshold`     = quantile(indice, 0.80, na.rm = TRUE),
      `% zones < 0.25`   = round(100 * mean(indice < 0.25, na.rm = TRUE), 1),
      `% zones > 1.5`    = round(100 * mean(indice > 1.5,  na.rm = TRUE), 1),
      `Gini coefficient` = {
        x <- sort(accesibilidad_iso$indice[!is.na(accesibilidad_iso$indice)])
        n <- length(x)
        round((2 * sum(seq_len(n) * x) / (n * sum(x))) - (n + 1) / n, 4)
      }
    )
}

tbl_indice <- bind_rows(
  resumen_indice(accesibilidad_iso,     "Isochrones"),
  resumen_indice(accesibilidad_buffers, "Buffers")
) %>%
  pivot_longer(-Method, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = Method, values_from = Value)

gt_tbl5 <- tbl_indice %>%
  gt() %>%
  tab_header(title = "E2SFCA accessibility index — summary statistics") %>%
  fmt_number(columns = c(Isochrones, Buffers), decimals = 3) %>%
  tab_source_note("Sources: CPV2022 · ACESS · RAS | Elaboración: los autores")

gtsave(gt_tbl5, "output/tables/05_indice_resumen.html")

# -------------------------------------------------------------
# TABLE 6 — Quintile distribution of the index
# -------------------------------------------------------------

quintile_dist <- function(sf_df, method_label) {
  sf_df %>%
    as_tibble() %>%
    filter(!is.na(quintil)) %>%
    group_by(quintil) %>%
    summarise(
      `N zones`   = n(),
      Population  = sum(poblacion, na.rm = TRUE),
      `Mean index`= round(mean(indice, na.rm = TRUE), 3),
      `Min index` = round(min(indice,  na.rm = TRUE), 3),
      `Max index` = round(max(indice,  na.rm = TRUE), 3),
      .groups = "drop"
    ) %>%
    mutate(
      Method    = method_label,
      `% pop`   = round(100 * Population / sum(Population), 1)
    )
}

tbl_quintiles <- bind_rows(
  quintile_dist(iso_sf, "Isochrones"),
  quintile_dist(buf_sf, "Buffers")
)

gt_tbl6 <- tbl_quintiles %>%
  gt(groupname_col = "Method") %>%
  tab_header(title = "E2SFCA index — quintile distribution") %>%
  fmt_number(columns = c(`N zones`, Population), decimals = 0) %>%
  cols_move_to_start(columns = quintil) %>%
  tab_source_note("Sources: CPV2022 · ACESS · RAS | Elaboración: los autores")

gtsave(gt_tbl6, "output/tables/06_quintiles.html")

# -------------------------------------------------------------
# TABLE 7 — Coefficient: normalized index validation
# -------------------------------------------------------------

coef_validation <- bind_rows(
  accesibilidad_iso %>%
    mutate(method = "Isochrones",
           coeficiente = indice / mean(indice, na.rm = TRUE)),
  accesibilidad_buffers %>%
    mutate(method = "Buffers",
           coeficiente = indice / mean(indice, na.rm = TRUE))
) %>%
  group_by(method) %>%
  summarise(
    `Mean coefficient`      = round(mean(coeficiente,  na.rm = TRUE), 4),
    `SD coefficient`        = round(sd(coeficiente,    na.rm = TRUE), 4),
    `% zones = 1 (±0.05)`   = round(100 * mean(abs(coeficiente - 1) < 0.05, na.rm = TRUE), 1),
    `Min coefficient`       = round(min(coeficiente,   na.rm = TRUE), 4),
    `Max coefficient`       = round(max(coeficiente,   na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  rename(Method = method)

gt_tbl7 <- coef_validation %>%
  gt() %>%
  tab_header(title = "Normalized accessibility coefficient — validation") %>%
  tab_source_note("Note: the mean coefficient should equal 1 by construction.")

gtsave(gt_tbl7, "output/tables/07_coeficiente.html")

message("=== Done. Maps → output/maps/  |  Tables → output/tables/ ===")
