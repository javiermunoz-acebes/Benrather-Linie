# ==============================================================================
# GEOCODIFICACIÓN GLOBAL Y PROYECCIÓN
# ==============================================================================

cat(">> Geocodificando ubicaciones (dominio global)...\n")

# 1) Geocodificar direcciones únicas SIN forzar "Deutschland"
unique_locs <- clean_data %>%
  select(original_ort) %>%
  distinct() %>%
  mutate(search_query = original_ort) %>%
  geocode(
    address = search_query,
    method  = "arcgis",
    verbose = TRUE
  )

# 2) Unir a los datos lingüísticos y filtrar geocodificaciones válidas
geocoded_full_empire <- clean_data %>%
  left_join(unique_locs, by = "original_ort") %>%
  filter(!is.na(lat), !is.na(long))

# Guardar dominio global (Europa completa)
write_csv(geocoded_full_empire, here(output_dir, "wenker_geocoded_full_empire.csv"))


# 3) Recorte histórico para modelo centrado en Alemania (bounding box)
geocoded_full_clean <- geocoded_full_empire %>%
  filter(
    lat  >= 47, lat  <= 55,
    long >= 5,  long <= 16
  )

# 4) Proyección a 3035 (solo recorte Alemania)
sf_projected <- geocoded_full_clean %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3035)

coords_proj <- st_coordinates(st_geometry(sf_projected))

sf_projected <- sf_projected %>%
  mutate(
    X_meter = coords_proj[, 1],
    Y_meter = coords_proj[, 2]
  )

final_data <- sf_projected %>% st_drop_geometry()
write_csv(final_data, here(output_dir, "wenker_final_completo_02.csv"))

# ==============================================================================
# FASE 3: MODELOS GAM (ALEMANIA) Y TABLA k
# ==============================================================================

library(mgcv)
library(dplyr)
library(scales)

k_values <- c(20, 60, 100, 200, 300)

gam_models <- lapply(k_values, function(k_val) {
  gam(
    is_hochdeutsch ~ s(X_meter, Y_meter, k = k_val),
    data   = final_data,
    family = binomial,
    method = "REML"
  )
})

names(gam_models) <- paste0("GAM_k", k_values)

gam_stats <- tibble(
  Modelo = names(gam_models),
  k      = k_values,
  AIC    = sapply(gam_models, AIC),
  Devianza_Explicada = sapply(gam_models, function(m) summary(m)$dev.expl)
) %>%
  mutate(
    Devianza_Explicada = percent(Devianza_Explicada, accuracy = 0.1)
  )

print(gam_stats)

# GLM de referencia (Alemania)
glm_de <- glm(
  is_hochdeutsch ~ X_meter + Y_meter,
  data   = final_data,
  family = binomial
)

stats_completas <- bind_rows(
  tibble(
    Modelo = "GLM",
    k      = NA_integer_,
    AIC    = AIC(glm_de),
    Devianza_Explicada = percent(
      (glm_de$null.deviance - glm_de$deviance) / glm_de$null.deviance,
      accuracy = 0.1
    )
  ),
  gam_stats
)

print(stats_completas)

# ==============================================================================
# FASE 4: OBJETO ESPACIAL GLOBAL Y MAPAS (EUROPA / ALEMANIA)
# ==============================================================================

library(rnaturalearth)
library(ggplot2)

# Puntos globales (Europa completa) en 3035
sf_raw <- geocoded_full %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3035) %>%
  mutate(
    Variante = factor(
      is_hochdeutsch,
      levels = c(0, 1),
      labels = c("maken (Low German)", "machen (High German)")
    )
  )

# Mapa base Europa
europe_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Europe") %>%
  st_transform(3035)

p_europa <- ggplot() +
  geom_sf(data = europe_sf, fill = "grey95", color = "grey80") +
  geom_sf(data = sf_raw, aes(color = Variante), size = 0.6, alpha = 0.7) +
  scale_color_manual(
    values = c(
      "maken (Low German)"   = "#1f78b4",
      "machen (High German)" = "#e31a1c"
    )
  ) +
  coord_sf(
    xlim = st_bbox(europe_sf)[c("xmin", "xmax")],
    ylim = st_bbox(europe_sf)[c("ymin", "ymax")]
  ) +
  labs(
    title = "Muestra Wenker: puntos en Europa",
    x = "Longitud proyectada (m)",
    y = "Latitud proyectada (m)"
  ) +
  theme_minimal()

print(p_europa)

# Mapa solo Alemania
germany_border <- ne_countries(
  scale = "medium", country = "Germany", returnclass = "sf"
) %>%
  st_transform(3035)

sf_de <- sf_raw[st_within(sf_raw, germany_border, sparse = FALSE), ] %>%
  mutate(
    Variante = factor(
      is_hochdeutsch,
      levels = c(0, 1),
      labels = c("maken (Low German)", "machen (High German)")
    )
  )

p_de <- ggplot() +
  geom_sf(data = germany_border, fill = "grey95", color = "grey80") +
  geom_sf(data = sf_de, aes(color = Variante), size = 0.8, alpha = 0.8) +
  scale_color_manual(
    values = c(
      "maken (Low German)"   = "#1f78b4",
      "machen (High German)" = "#e31a1c"
    )
  ) +
  coord_sf(
    xlim = st_bbox(germany_border)[c("xmin", "xmax")],
    ylim = st_bbox(germany_border)[c("ymin", "ymax")]
  ) +
  labs(
    title = "Muestra Wenker: puntos dentro de Alemania",
    x = "Longitud proyectada (m)",
    y = "Latitud proyectada (m)"
  ) +
  theme_minimal()

print(p_de)

# ==============================================================================
# FASE 5: GAM GLOBAL (EUROPA) Y DOMINIO "IMPERO ALEMÁN" APROX.
# ==============================================================================

# Coordenadas métricas globales
coords_raw <- st_coordinates(st_geometry(sf_raw))

sf_raw <- sf_raw %>%
  mutate(
    X_meter = coords_raw[, 1],
    Y_meter = coords_raw[, 2]
  )

final_europa <- sf_raw %>% st_drop_geometry()

gam_europa_k300 <- gam(
  is_hochdeutsch ~ s(X_meter, Y_meter, k = 300),
  data   = final_europa,
  family = binomial,
  method = "REML"
)

summary(gam_europa_k300)

# Dominio "Imperio alemán" aproximado
europe_countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(3035)

imperio_actual <- europe_countries %>%
  filter(admin %in% c(
    "Germany", "Poland", "France", "Denmark",
    "Belgium", "Luxembourg", "Czechia", "Austria", "Netherlands"
  ))

imperio_sf <- st_union(imperio_actual) %>% st_make_valid()

sf_imperio <- sf_raw[st_within(sf_raw, imperio_sf, sparse = FALSE), ]

final_imperio <- sf_imperio %>%
  mutate(
    X_meter = st_coordinates(st_geometry(.))[, 1],
    Y_meter = st_coordinates(st_geometry(.))[, 2]
  ) %>%
  st_drop_geometry()

gam_imperio_k300 <- gam(
  is_hochdeutsch ~ s(X_meter, Y_meter, k = 300),
  data   = final_imperio,
  family = binomial,
  method = "REML"
)

summary(gam_imperio_k300)

tibble(
  Dominio = "Imperio alemán (aprox.)",
  n       = nrow(final_imperio),
  AIC     = AIC(gam_imperio_k300),
  DevExpl = percent(summary(gam_imperio_k300)$dev.expl, accuracy = 0.1)
) %>% print()

# Mapa centrado en el dominio imperial
puntos_3035 <- st_transform(sf_imperio, 3035)
bb_puntos   <- st_bbox(puntos_3035)

margen <- 50000
xlim_puntos <- c(bb_puntos["xmin"] - margen, bb_puntos["xmax"] + margen)
ylim_puntos <- c(bb_puntos["ymin"] - margen, bb_puntos["ymax"] + margen)

p_imperio <- ggplot() +
  geom_sf(data = imperio_sf, fill = "grey95", color = "grey80") +
  geom_sf(data = sf_imperio,
          aes(color = factor(is_hochdeutsch)),
          size = 0.6, alpha = 0.7) +
  scale_color_manual(
    values = c("0" = "#1f78b4", "1" = "#e31a1c"),
    labels = c("maken (Low German)", "machen (High German)"),
    name   = "Variante"
  ) +
  coord_sf(
    crs    = st_crs(3035),
    xlim   = xlim_puntos,
    ylim   = ylim_puntos,
    expand = FALSE
  ) +
  labs(
    title = "Muestra Wenker: Centro de Europa / antiguo Imperio alemán (aprox.)",
    x = "Longitud proyectada (m)",
    y = "Latitud proyectada (m)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

print(p_imperio)

# Probabilidades predichas (escala 0–1)
prob_europa <- predict(gam_europa_k300, type = "response")

# Clasificación con umbral 0.5
clase_pred <- ifelse(prob_europa > 0.5, 1, 0)

# Matriz de confusión
matriz <- table(
  Real     = final_europa$is_hochdeutsch,
  Predicho = clase_pred
)

print(matriz)

# Precisión global
precision_global <- sum(diag(matriz)) / sum(matriz)
precision_global

pts <- read_csv(here(output_dir, "wenker_geocoded_full_empire.csv"))

library(rnaturalearth)
library(sf)
library(dplyr)
library(ggplot2)

# 1) Puntos desde  CSV geocodificado (lat/long)
sf_pts <- readr::read_csv(here(output_dir, "wenker_geocoded_full_empire.csv")) %>%
  filter(!is.na(lat), !is.na(long)) %>%
  st_as_sf(coords = c("long","lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3035)

# 2) Dominio "tipo imperio" pero con fronteras actuales
dominio_sf <- ne_countries(scale="medium", returnclass="sf") %>%
  filter(admin %in% c("Germany","Austria","Switzerland","Poland")) %>%  # ajusta lista
  st_transform(3035) %>%
  st_union() %>%
  st_make_valid()

sf_dom <- sf_pts[st_within(sf_pts, dominio_sf, sparse = FALSE), ]

# 3) Bbox y plot estilo el tuyo
bb <- st_bbox(dominio_sf); margen <- 50000
xlim <- c(bb["xmin"]-margen, bb["xmax"]+margen)
ylim <- c(bb["ymin"]-margen, bb["ymax"]+margen)

p <- ggplot() +
  geom_sf(data = dominio_sf, fill="grey95", color="grey80") +
  geom_sf(data = sf_dom, aes(color = factor(is_hochdeutsch)),
          size=0.6, alpha=0.7) +
  scale_color_manual(values=c("0"="#1f78b4","1"="#e31a1c"),
                     labels=c("maken (Low German)","machen (High German)"),
                     name="Variante") +
  coord_sf(crs = st_crs(3035), xlim=xlim, ylim=ylim, expand=FALSE) +
  theme_minimal() +
  theme(panel.grid = element_blank())

print(p)
ggsave(here(output_dir, "wenker_map_empire_domain.png"), p,
       
       width=8, height=6, dpi=300)

