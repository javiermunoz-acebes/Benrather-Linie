# ==============================================================================
# PROTOCOLO DE ANÁLISIS GEOLINGÜÍSTICO: LA LÍNEA DE BENRATH (WENKERBÖGEN)
# ==============================================================================
# Autor: Javier Muñoz-Acebes
# Descripción: Flujo de trabajo completo para la adquisición, procesamiento
# geostadístico y modelado de la isoglosa maken/machen basada en el corpus Wenker.
# ==============================================================================

# 1. CONFIGURACIÓN DEL ENTORNO -------------------------------------------------
# Usamos 'pacman' para asegurar que las dependencias se instalan si faltan
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Manipulación de datos (dplyr, readr, stringr)
  fs,         # Manejo robusto de sistema de archivos
  here,       # Rutas relativas seguras
  zip,        # Descompresión de archivos
  janitor     # Limpieza de nombres
)

# Definir rutas 
data_dir <- here("data", "raw_wenker")
output_dir <- here("data", "processed")
dir_create(data_dir)
dir_create(output_dir)

# 2. ADQUISICIÓN DE DATOS  -------------------------------------------

repo_url <- "https://github.com/engsterhold/wenker-storage/archive/refs/heads/master.zip"
zip_path <- path(data_dir, "wenker_storage_snapshot.zip")

cat(">> Iniciando descarga del repositorio completo...\n")
if (!file_exists(zip_path)) {
  download.file(repo_url, destfile = zip_path, mode = "wb")
  cat(">> Descarga completada.\n")
} else {
  cat(">> Archivo ZIP ya existente. Omitiendo descarga.\n")
}

# Descomprimir los archivos CSV necesarios
cat(">> Descomprimiendo archivos...\n")
unzip(zip_path, exdir = data_dir, junkpaths = FALSE)

# Localizar todos los archivos.csv extraídos recursivamente
csv_files <- dir_ls(data_dir, glob = "*.csv", recurse = TRUE)
cat(sprintf(">> Se encontraron %d archivos CSV para procesar.\n", length(csv_files)))

# 3. FUNCIÓN DE EXTRACCIÓN  ---------------------------------------------
extract_wenker_data <- function(filepath) {
  # Leer el archivo como texto crudo para evitar problemas de parsing de read_csv
  # con formatos irregulares.
  raw_text <- read_file(filepath)
  
  # A. Extracción de ORT (Ubicación)
  # Mejora: Usamos flag (?m) para multilínea y anclamos al inicio (^) y fin ($) de línea.
  # Capturamos todo después de "Ort:" hasta el final de la línea.
  ort_match <- str_match(raw_text, "(?mi)^Ort\\s*:\\s*(.+)$")
  ort_extracted <- if (!is.na(ort_match[1, 2])) str_trim(ort_match[1, 2]) else NA_character_
  
  # Limpieza específica de Ort: eliminar punto y coma final si existe 
  ort_clean <- str_remove(ort_extracted, ";\\s*$")
  
  # B. Extracción de WS17 (Frase 17)
  # Buscamos la línea que empieza explícitamente con "WS17" seguida de tabulador.
  ws17_match <- str_match(raw_text, "(?mi)^WS17\\t(.+)$")
  ws17_extracted <- if (!is.na(ws17_match[1, 2])) str_trim(ws17_match[1, 2]) else NA_character_
  
  return(tibble(
    file_id = path_file(filepath),
    original_ort = ort_clean,
    ws17_raw = ws17_extracted,
    extraction_status = case_when(
      is.na(ort_clean) & is.na(ws17_extracted) ~ "EMPTY_FILE",
      is.na(ort_clean) ~ "MISSING_ORT",
      is.na(ws17_extracted) ~ "MISSING_WS17",
      TRUE ~ "OK"
    )
  ))
}

# 4. PROCESAMIENTO EN LOTE  -----------------------

cat(">> Procesando archivos (Extracción de texto)...\n")
raw_data <- map_dfr(csv_files, extract_wenker_data,.id = "source_path")

# 5. INGENIERÍA DE VARIABLES LINGÜÍSTICAS  ------------------------

clean_data <- raw_data %>%
  filter(extraction_status == "OK") %>%
  mutate(
    # A. Normalización básica
    ws17_norm = str_to_lower(ws17_raw),
    ws17_norm = str_replace_all(ws17_norm, "[[:punct:]]", ""), # Eliminar puntuación
    
    # B. Detección de variante basada en TOKENS (Palabras completas)
    # Buscamos variantes específicas de "machen" usando límites de palabra (\\b)
    # Esto evita que palabras como "lachen" o "drachen" cuenten como "machen".
    
    has_machen = str_detect(ws17_norm, "\\bma(ch|h)en\\b|\\bma(ch|h)e\\b"),
    has_maken  = str_detect(ws17_norm, "\\bma(k|g)en\\b|\\bma(k|g)e\\b"),
    
    # C. Clasificación Rigurosa
    linguistic_variant = case_when(
      has_machen &!has_maken ~ "machen (High German)",
      !has_machen & has_maken ~ "maken (Low German)",
      has_machen & has_maken  ~ "AMBIGUOUS/MIXED", # Importante: detectar mezclas en la misma frase
      TRUE                    ~ "NOT_FOUND"        # La palabra no está o usa otra raíz 
    ),
    
    # Variable binaria para el modelo (solo si la clasificación es clara)
    is_hochdeutsch = case_when(
      linguistic_variant == "machen (High German)" ~ 1,
      linguistic_variant == "maken (Low German)" ~ 0,
      TRUE ~ NA_real_
    )
  )

# 6. INFORME DE CALIDAD DE DATOS  --------------------------------
cat("\n=== INFORME DE HIGIENE DE DATOS ===\n")
cat("Total archivos procesados:", nrow(raw_data), "\n")
print(table(raw_data$extraction_status))

cat("\nDistribución de Variantes Lingüísticas detectadas:\n")
clean_data %>% 
  count(linguistic_variant) %>% 
  mutate(percentage = n / sum(n) * 100) %>%
  print()

# 7. GUARDADO DE DATOS CURADOS -------------------------------------------------
# Guardamos dos versiones: 
# 1. Los datos "sucios" con logs de error (para auditoría).
# 2. Los datos "limpios" listos para geocodificación.

write_csv(raw_data, path(output_dir, "wenker_audit_log.csv"))
write_csv(clean_data %>% filter(!is.na(is_hochdeutsch)), path(output_dir, "wenker_clean_linguistic.csv"))

cat("\n>> Proceso completado. Datos guardados en 'data/processed/'.\n")

# ==============================================================================
# FASE 2: PROTOCOLO DE GEOESTADÍSTICA 
# ==============================================================================
# Objetivos:
# 1. Priorizar coordenadas curadas (locations.txt) sobre geocodificación ciega.
# 2. Geocodificación de respaldo con contexto ("Münster" -> "Münster, Germany").
# 3. PROYECCIÓN DE MAPA: Transformar Lat/Lon (grados) a ETRS89 (metros).
# 4. Filtrado espacial de puntos fuera de Europa Central.
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  here,
  sf,             # Simple Features (El estándar para GIS en R)
  rnaturalearth,  # Mapas base para validación
  tidygeocoder    # Geocodificación (solo para fallbacks)
)

# Rutas (usando las salidas de la Fase 1)
input_file <- here("data", "processed", "wenker_clean_linguistic.csv")
raw_dir    <- here("data", "raw_wenker")
output_dir <- here("data", "processed")

# 1. CARGAR DATOS LINGÜÍSTICOS  -----------------------------------------
ling_data <- read_csv(input_file, show_col_types = FALSE)
cat(">> Datos lingüísticos cargados:", nrow(ling_data), "registros.\n")

# 2. ESTRATEGIA HÍBRIDA DE GEOLOCALIZACIÓN -------------------------------------
# El repositorio original suele tener un 'locations.txt'. Vamos a intentar usarlo
# porque esas coordenadas suelen ser más fiables (históricas) que las de OSM.

loc_file_path <- here(raw_dir, "locations.txt")
repo_locations <- NULL

if (file.exists(loc_file_path)) {
  cat(">> 'locations.txt' detectado. Intentando cargar metadatos de ubicación...\n")
  
  # Intentamos leerlo. A veces estos archivos no tienen encabezados o usan separadores raros.
  try({
    repo_locations <- read_delim(loc_file_path, delim = "\t", col_names = FALSE, show_col_types = FALSE)
    # Heurística simple: buscar columnas numéricas que parezcan lat/lon (47-55 N, 5-15 E)
  })
}

# 3. PROCESO DE FUSIÓN Y GEOCODIFICACIÓN (FALLBACK) ----------------------------

# Si no tenemos locations.txt confiable, preparamos geocodificación robusta
data_to_geo <- ling_data %>%
  # Limpieza adicional para geocoder: quitar paréntesis históricos que confunden a OSM
  # Ej: "Münster (Westf.)" -> OSM lo entiende bien, pero "Klein-Kleckersdorf bei X" a veces no.
  mutate(
    search_query = paste0(original_ort, ", Germany") # Añadimos contexto nacional
  )

cat(">> Iniciando geocodificación (esto puede tardar unos minutos)...\n")
cat("   Nota: Se usa OSM (Nominatim). Respetamos el 'rate limit' automáticamente.\n")

# Usamos tidygeocoder con gestión de errores
geocoded_data <- data_to_geo %>%
  geocode(
    address = search_query,
    method = 'osm', 
    lat = latitude, 
    long = longitude,
    verbose = TRUE
  )

# 4. CONTROL DE CALIDAD ESPACIAL  ----------------------------------

# Definir una "Bounding Box" generosa para Alemania histórica (Imperio Alemán + Austria)
# Lat: 45 a 56, Lon: 5 a 23. Todo lo que caiga fuera es un error de geocodificación (e.g. un pueblo en USA).
valid_geo <- geocoded_data %>%
  filter(
    !is.na(latitude) &!is.na(longitude),
    latitude > 45 & latitude < 56,
    longitude > 5 & longitude < 23
  )

dropped_count <- nrow(geocoded_data) - nrow(valid_geo)
if(dropped_count > 0) {
  cat(sprintf("\n>> ALERTA: Se eliminaron %d puntos por estar fuera de la zona geográfica plausible o no encontrados.\n", dropped_count))
  # Guardar los fallos para revisión manual
  geocoded_data %>% 
    filter(is.na(latitude) | latitude <= 45 | latitude >= 56) %>%
    write_csv(here(output_dir, "geocoding_failures.csv"))
}

# 5. PROYECCIÓN CARTOGRÁFICA  ---------------------
# Convertimos de Lat/Lon (WGS84) a ETRS89-LAEA (EPSG:3035).

cat(">> Proyectando coordenadas a sistema métrico (EPSG:3035)...\n")

sf_object <- st_as_sf(valid_geo, coords = c("longitude", "latitude"), crs = 4326) # WGS84
sf_projected <- st_transform(sf_object, crs = 3035) # ETRS89-LAEA

# Extraemos las nuevas coordenadas X/Y (en metros) para usarlas en el GAM
final_data <- sf_projected %>%
  mutate(
    X_meter = st_coordinates(.)[,1],
    Y_meter = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() # Volvemos a data.frame plano para compatibilidad con mgcv

# 6. VISUALIZACIÓN DE CONTROL DE CALIDAD ---------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")
europe_crop <- st_crop(world, xmin = 5, xmax = 20, ymin = 46, ymax = 56)

qa_map <- ggplot() +
  geom_sf(data = europe_crop, fill = "gray95", color = "gray80") +
  geom_point(data = final_data, aes(x = X_meter, y = Y_meter, color = factor(is_hochdeutsch)), 
             size = 0.5, alpha = 0.6) +
  # Nota: geom_point usa coords proyectadas, el mapa base necesita ser transformado o coordinado.
  # Para este chequeo rápido, usamos coord_sf para alinear.
  coord_sf(crs = 3035, xlim = range(final_data$X_meter), ylim = range(final_data$Y_meter)) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), name = "Variante") +
  labs(title = "QA Espacial: Distribución de puntos proyectados",
       subtitle = "Rojo = machen, Azul = maken. Comprobar outliers visibles.") +
  theme_minimal()
plot(qa_map)
ggsave(here(output_dir, "qa_spatial_distribution.png"), qa_map, width = 8, height = 6)

# 7. GUARDADO FINAL PARA ANÁLISIS ----------------------------------------------
write_csv(final_data, here(output_dir, "wenker_final_projected.csv"))

cat("\n>> Fase 2 Completada.\n")
cat(">> Datos guardados en 'wenker_final_projected.csv'.\n")
cat(">> Coordenadas listas en columnas 'X_meter' y 'Y_meter' para el modelo GAM.\n")

# ==============================================================================
# FASE 3: MODELADO ESTADÍSTICO ESPACIAL (GAM)
# ==============================================================================
# Objetivos:
# 1. Ajustar un modelo binomial para predecir la probabilidad de 'machen'.
# 2. Capturar la variación espacial no lineal usando 'thin plate splines'.
# 3. Generar una superficie de predicción (isoglosa).
# ==============================================================================

pacman::p_load(
  mgcv,      # Modelos Aditivos Generalizados
  gratia,    # Visualización elegante de GAMs
  metR       # Herramientas para contornos en ggplot2
)

# 1. CARGA DE DATOS PROYECTADOS ------------------------------------------------
final_data <- read_csv(here(output_dir, "wenker_final_projected.csv"))

# 2. AJUSTE DEL MODELO GAM -----------------------------------------------------
# k = 50 es un punto de partida; controla la "flexibilidad" de la curva.
# family = binomial porque nuestra variable es 0 o 1.

cat(">> Ajustando Modelo Aditivo Generalizado (GAM)... \n")
gam_model <- gam(
  is_hochdeutsch ~ s(X_meter, Y_meter, k = 50), 
  data = final_data, 
  family = binomial, 
  method = "REML"
)

cat(">> Resumen del modelo:\n")
print(summary(gam_model))

# 3. CREACIÓN DE MALLA DE PREDICCIÓN (GRID) ------------------------------------
# Creamos una cuadrícula regular sobre Alemania para interpolar los valores.

grid_res <- 5000 # Resolución de 5km por celda
bbox <- list(
  x = seq(min(final_data$X_meter), max(final_data$X_meter), by = grid_res),
  y = seq(min(final_data$Y_meter), max(final_data$Y_meter), by = grid_res)
)

pred_grid <- expand.grid(X_meter = bbox$x, Y_meter = bbox$y)

# Predecir probabilidades (type = "response" nos da valores de 0 a 1)
pred_grid$prob <- predict(gam_model, newdata = pred_grid, type = "response")

# ==============================================================================
# FASE 4: VISUALIZACIÓN CARTOGRÁFICA DE LA ISOGLOSA
# ==============================================================================

# 1. OBTENER FRONTERAS PARA EL RECORTADO  -----------------------------
germany_border <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf") %>%
  st_transform(crs = 3035)

# 2. GENERACIÓN DEL MAPA FINAL -------------------------------------------------
cat(">> Generando mapa final de la isoglosa...\n")

final_plot <- ggplot() +
  # Capa 1: Superficie de probabilidad (Calor)
  geom_tile(data = pred_grid, aes(x = X_meter, y = Y_meter, fill = prob), alpha = 0.8) +
  
  # Capa 2: La Isoglosa (Línea donde la probabilidad es exactamente 0.5)
  geom_contour(data = pred_grid, aes(x = X_meter, y = Y_meter, z = prob),
               breaks = 0.5, color = "white", size = 1.2) +
  
  # Capa 3: Fronteras administrativas
  geom_sf(data = germany_border, fill = NA, color = "black", size = 0.5) +
  
  # Estética
  scale_fill_viridis_c(
    option = "magma", 
    name = "Probabilidad\nHochdeutsch",
    labels = scales::percent
  ) +
  labs(
    title = "Transición Dialectal: La Línea de Benrath (maken/machen)",
    subtitle = "Basado en encuestas Wenker. La línea blanca indica el límite 50/50.",
    caption = "Modelo: GAM (Thin Plate Splines) | Proyección: ETRS89-LAEA",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())
plot(final_plot)

# 3. GUARDADO Y EXPORTACIÓN ----------------------------------------------------
ggsave(here(output_dir, "wenker_isogloss_map.png"), final_plot, width = 10, height = 12, dpi = 300)

cat("\n==================================================================\n")
cat("PROCESO FINALIZADO CON ÉXITO\n")
cat("Archivo generado: data/processed/wenker_isogloss_map.png\n")
cat("Interpretación: Los valores cercanos a 1 (rojo/amarillo) indican predominancia de 'machen'.\n")
cat("==================================================================\n")

# ==============================================================================
# FASE 5: ANALÍTICA AVANZADA Y DIAGNÓSTICOS VISUALES
# ==============================================================================
# Objetivos:
# 1. Visualizar la densidad de los datos (¿Dónde tenemos más información?).
# 2. Perfil de transición (S-Curve): Cómo cambia la lengua de Norte a Sur.
# 3. Diagnóstico del modelo: ¿Es fiable nuestra isoglosa?
# ==============================================================================

pacman::p_load(patchwork, scales, gratia)

# 1. DISTRIBUCIÓN DE FRECUENCIAS POR VARIANTE ----------------------------------
p1 <- final_data %>%
  count(linguistic_variant) %>%
  ggplot(aes(x = reorder(linguistic_variant, n), y = n, fill = linguistic_variant)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Frecuencia de Variantes", x = NULL, y = "Número de localidades") +
  theme_minimal()
plot(p1)

# 2. DENSIDAD GEOGRÁFICA DE MUESTREO ------------------------------------------
# Esto nos dice si la isoglosa es robusta o si hay "vacíos" de datos.
p2 <- ggplot(final_data, aes(x = X_meter, y = Y_meter)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.3) +
  geom_point(size = 0.1, alpha = 0.2) +
  scale_fill_viridis_c(option = "mako", guide = "none") +
  labs(title = "Densidad de Puntos Wenker", subtitle = "Concentración de encuestas") +
  theme_void()
plot(p2)

# 3. LA "S-CURVE" DE TRANSICIÓN (NORTE-SUR) -----------------------------------
# Graficamos la probabilidad de 'machen' respecto a la coordenada Y (Latitud).
p3 <- ggplot(final_data, aes(x = Y_meter, y = is_hochdeutsch)) +
  geom_jitter(height = 0.05, alpha = 0.1, size = 0.5, color = "gray") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "red") +
  scale_y_continuous(labels = percent) +
  labs(title = "Perfil de Transición Norte-Sur",
       x = "Latitud Proyectada (Metros)", 
       y = "Probabilidad de 'machen'") +
  theme_minimal()
plot(p3)

# 4. DIAGNÓSTICO DEL MODELO GAM ------------------------------------------------
# Visualizamos el "smooth" (la superficie de suavizado) en 3D/Calor
p4 <- draw(gam_model, select = 1) + 
  labs(title = "Efecto Espacial del Modelo", subtitle = "Interacción X-Y en el log-odds")
plot(p4)

# COMBINAR TODO EN UN DASHBOARD ------------------------------------------------
layout <- (p1 | p2) / (p3 | p4)
final_dashboard <- layout + 
  plot_annotation(
    title = "Dashboard de Análisis Dialectométrico - Proyecto Wenker",
    caption = "Generado automáticamente por el Protocolo Robusto 2.0"
  )

ggsave(here(output_dir, "wenker_analytics_dashboard.png"), final_dashboard, width = 12, height = 10)

cat(">> Dashboard analítico guardado en 'data/processed/wenker_analytics_dashboard.png'\n")


# ==============================================================================
# FASE 6: COMPARATIVA RIGUROSA GLM VS. GAM
# ==============================================================================
# Ajustar un modelo logístico lineal (GLM).
# Comparar la capacidad predictiva (AIC, Deviance).
# Visualizar la "rigidez" del GLM frente a la "flexibilidad" del GAM.
# ==============================================================================

pacman::p_load(broom, modelr)

# 1. AJUSTE DEL MODELO GLM (LOGÍSTICO LINEAL) ----------------------------------
# asumimos que la probabilidad de 'machen' cambia linealmente con X e Y.
glm_model <- glm(
  is_hochdeutsch ~ X_meter + Y_meter, 
  data = final_data, 
  family = binomial
)

cat(">> Resumen del modelo GLM:\n")
print(summary(glm_model))

# 2. COMPARACIÓN DE MÉTRICAS DE AJUSTE -----------------------------------------
# El AIC más bajo indica un mejor equilibrio entre complejidad y precisión.
stats_comparison <- tibble(
  Modelo = c("GLM (Lineal)", "GAM (No-Lineal)"),
  AIC = c(AIC(glm_model), AIC(gam_model)),
  Deviance_Explicada = c(
    (glm_model$null.deviance - glm_model$deviance) / glm_model$null.deviance,
    (gam_model$null.deviance - gam_model$deviance) / gam_model$null.deviance
  )
)

print(stats_comparison)

# 3. GENERACIÓN DE PREDICCIONES COMPARATIVAS -----------------------------------
# Usamos la misma malla  de la Fase 3
pred_grid_comp <- pred_grid %>%
  mutate(
    prob_glm = predict(glm_model, newdata = ., type = "response"),
    prob_gam = predict(gam_model, newdata = ., type = "response")
  )

# 4. VISUALIZACIÓN: LA RIGIDEZ CONTRA LA REALIDAD ------------------------------

# Mapa GLM (Lineal)
p_glm <- ggplot() +
  geom_tile(data = pred_grid_comp, aes(x = X_meter, y = Y_meter, fill = prob_glm)) +
  geom_contour(data = pred_grid_comp, aes(x = X_meter, y = Y_meter, z = prob_glm), 
               breaks = 0.5, color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
  labs(title = "Predicción GLM (Lineal)", subtitle = "Isoglosa como plano rígido") +
  theme_void()

# Mapa GAM (Flexible)
p_gam <- ggplot() +
  geom_tile(data = pred_grid_comp, aes(x = X_meter, y = Y_meter, fill = prob_gam)) +
  geom_contour(data = pred_grid_comp, aes(x = X_meter, y = Y_meter, z = prob_gam), 
               breaks = 0.5, color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
  labs(title = "Predicción GAM (Suavizado)", subtitle = "Isoglosa adaptada a los datos") +
  theme_void()
plot(p_gam)

# Combinación para reporte
comparativo_plot <- p_glm + p_gam + 
  plot_layout(guides = "collect") +
  plot_annotation(title = "Comparación de Modelos Espaciales")
plot(comparativo_plot)
ggsave(here(output_dir, "comparativa_glm_vs_gam.png"), comparativo_plot, width = 12, height = 6)

# ==============================================================================
# FASE 7: ANÁLISIS DE RESIDUOS Y DETECCIÓN DE ENCLAVES
# ==============================================================================
# Calcular la diferencia entre la realidad y la predicción del modelo (Residuos).
# Identificar localidades "anómalas" (Enclaves).
# Mapear dónde el modelo falla para entender la resistencia dialectal.
# ==============================================================================

# 1. CÁLCULO DE RESIDUOS Y PROBABILIDADES --------------------------------------
# Añadimos las predicciones del modelo a nuestros datos originales.

analysis_data <- final_data %>%
  mutate(
    prob_predicha = predict(gam_model, type = "response"),
    # Residuo: 1 (Realidad) - Probabilidad (Predicción)
    # Un residuo positivo alto: El modelo esperaba 'maken' (0) pero encontró 'machen' (1).
    # Un residuo negativo bajo: El modelo esperaba 'machen' (1) pero encontró 'maken' (0).
    residuo = is_hochdeutsch - prob_predicha
  )

# 2. IDENTIFICACIÓN DE ENCLAVES -------------------------
# Definimos un umbral de "sorpresa" (p.ej. una probabilidad > 80% pero el dato sería opuesto)

enclaves <- analysis_data %>%
  mutate(
    tipo_anomalia = case_when(
      is_hochdeutsch == 1 & prob_predicha < 0.2 ~ "Isla de High German (en zona Low)",
      is_hochdeutsch == 0 & prob_predicha > 0.8 ~ "Isla de Low German (en zona High)",
      TRUE ~ "Normal"
    )
  ) %>%
  filter(tipo_anomalia != "Normal")

cat(sprintf(">> Se han detectado %d enclaves o anomalías lingüísticas.\n", nrow(enclaves)))
plot(enclaves)

# 3. VISUALIZACIÓN DE LA RESISTENCIA DIALECTAL ---------------------------------

mapa_residuos <- ggplot() +
  # Mapa base de fondo (limpio)
  geom_sf(data = germany_border, fill = "gray98", color = "gray80") +
  
  # Dibujamos la isoglosa de referencia (la frontera del modelo)
  geom_contour(data = pred_grid, aes(x = X_meter, y = Y_meter, z = prob),
               breaks = 0.5, color = "black", linetype = "dashed", alpha = 0.5) +
  
  # Mapeamos los residuos: los puntos más grandes son donde el modelo más se equivoca
  geom_point(data = analysis_data, 
             aes(x = X_meter, y = Y_meter, color = residuo, size = abs(residuo)), 
             alpha = 0.4) +
  
  # Resaltamos los enclaves detectados con un anillo
  geom_point(data = enclaves, 
             aes(x = X_meter, y = Y_meter), 
             color = "black", shape = 1, size = 3, stroke = 1) +
  
  scale_color_gradient2(low = "#0571b0", mid = "white", high = "#ca0020", 
                        midpoint = 0, name = "Error del Modelo\n(Dato - Pred)") +
  labs(
    title = "Mapa de Residuos: Anomalías y Enclaves Lingüísticos",
    subtitle = "Los puntos rojos/azules intensos son localidades que contradicen la tendencia regional.",
    caption = "Puntos con círculo negro = Enclaves estadísticamente significativos."
  ) +
  theme_void()
plot(mapa_residuos)

# 4. GUARDAR  --------------------------------------------------------

# Guardamos  mapa
ggsave(here(output_dir, "wenker_residual_map.png"), mapa_residuos, width = 10, height = 10)

# Exportamos la lista de pueblos anómalos para investigación histórica
enclaves %>%
  select(original_ort, is_hochdeutsch, prob_predicha, residuo, tipo_anomalia) %>%
  write_csv(here(output_dir, "enclaves_detectados.csv"))

cat(">> Análisis de residuos completado. Lista de enclaves guardada en 'enclaves_detectados.csv'.\n")
# Convertir matrices a vectores numéricos simples
analysis_data <- analysis_data %>%
  mutate(
    prob_predicha = as.numeric(prob_predicha),
    residuo = as.numeric(residuo)
  )

# select y write_csv funcionarán sin errores
enclaves <- analysis_data %>%
  filter(abs(residuo) > 0.7) # Un criterio de filtrado más limpio

write_csv(enclaves, here(output_dir, "enclaves_detectados.csv"))

# ==============================================================================
# FASE 8: ANÁLISIS DE AUTOCORRELACIÓN ESPACIAL (LISA)
# ==============================================================================
# 1. Detectar Clústeres de anomalías.
# 2. Diferenciar entre ruido aleatorio y desviaciones regionales sistemáticas.
# ==============================================================================

pacman::p_load(spdep)

# 1. PREPARAR VECNDARIOS ESPACIALES -------------------------------------------
# Creamos una red de vecinos basada en la distancia (p.ej. 20km)
coords <- cbind(analysis_data$X_meter, analysis_data$Y_meter)
vecinos <- dnearneigh(coords, 0, 20000) # 20km de radio
pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

# 2. CÁLCULO DE MORAN LOCAL (LISA) ---------------------------------------------
# Esto identifica dónde los residuos similares se agrupan geográficamente.
moran_local <- localmoran(analysis_data$residuo, pesos, zero.policy = TRUE)

analysis_data <- analysis_data %>%
  mutate(
    lisa_stat = moran_local[,1],
    p_value = moran_local[,5],
    cluster = case_when(
      p_value > 0.05 ~ "No significativo",
      residuo > 0 & lisa_stat > 0 ~ "Hotspot (H-H): Foco de machen",
      residuo < 0 & lisa_stat > 0 ~ "Coldspot (L-L): Foco de maken",
      TRUE ~ "Outlier/Mezcla"
    )
  )

# 3. VISUALIZACIÓN DE CLÚSTERES ------------------------------------------------
p_clusters <- ggplot() +
  geom_sf(data = germany_border, fill = "gray95", color = "white") +
  geom_point(data = analysis_data, aes(x = X_meter, y = Y_meter, color = cluster), 
             size = 1, alpha = 0.7) +
  scale_color_manual(values = c(
    "Hotspot (H-H): Foco de machen" = "#ca0020", 
    "Coldspot (L-L): Foco de maken" = "#0571b0",
    "No significativo" = "gray90",
    "Outlier/Mezcla" = "#f4a582"
  )) +
  labs(title = "Análisis de Autocorrelación Espacial (LISA)",
       subtitle = "Identificación de regiones con comportamiento lingüístico divergente") +
  theme_minimal()

ggsave(here(output_dir, "wenker_spatial_clusters.png"), p_clusters, width = 10, height = 8)

# ==============================================================================
# FASE 9: EVALUACIÓN DE CONFIANZA REGIONAL DEL MODELO
# ==============================================================================
# 1. Calcular el error medio por cuadrícula geográfica.
# 2. Generar una tabla resumen de precisión para el informe final.
# ==============================================================================

# 1. CREAR CUADRÍCULAS DE ANÁLISIS  ---------------------------
# Dividimos el mapa en celdas de 50km para evaluar la fiabilidad local.
grid_size <- 50000 

regional_stats <- analysis_data %>%
  mutate(
    grid_x = floor(X_meter / grid_size) * grid_size,
    grid_y = floor(Y_meter / grid_size) * grid_size
  ) %>%
  group_by(grid_x, grid_y) %>%
  summarise(
    n_pueblos = n(),
    precision_media = mean(abs(residuo) < 0.3), # % de pueblos que el modelo "acertó"
    error_maximo = max(abs(residuo)),
    variabilidad_local = sd(is_hochdeutsch),
    .groups = 'drop'
  ) %>%
  mutate(
    nivel_confianza = case_when(
      precision_media > 0.9 ~ "ALTA (Consistencia total)",
      precision_media > 0.7 ~ "MEDIA (Zona de transición)",
      TRUE ~ "BAJA (Alta disputa lingüística)"
    )
  )

# 2. TABLA RESUMEN  ---------------------------------------------
cat("\n=== RESUMEN DE FIABILIDAD DEL MODELO ===\n")
confianza_resumen <- regional_stats %>%
  count(nivel_confianza) %>%
  mutate(porcentaje_territorio = n / sum(n) * 100)

print(confianza_resumen)

# 3. GUARDAR EL PROYECTO --------------------------------------
# Guardamos todo el espacio de trabajo para futuras consultas
save.image(here(output_dir, "wenker_final_workspace.RData"))

write_csv(regional_stats, here(output_dir, "metricas_confianza_regional.csv"))

cat("\n>> PROTOCOLO COMPLETADO.")
cat("\n>> Archivos finales listos en 'data/processed/'.")
cat("\n>> El modelo es confiable en un", 
    round(sum(confianza_resumen$porcentaje_territorio[confianza_resumen$nivel_confianza != "BAJA (Alta disputa lingüística)"]), 2),
    "% del territorio analizado.\n")

# gráficos añadidos
# ==============================================================================
# SUPLEMENTO 1: DISTRIBUCIÓN ESTADÍSTICA DE VARIANTES
# ==============================================================================
p_distribucion_limpio <- clean_data %>%
  # Filtramos para quedarnos solo con las dos variantes de interés
  filter(linguistic_variant %in% c("machen (High German)", "maken (Low German)")) %>%
  count(linguistic_variant) %>%
  ggplot(aes(x = reorder(linguistic_variant, n), y = n, fill = linguistic_variant)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = n), hjust = -0.2, size = 5, fontface = "bold") + 
  coord_flip() +
  # Usamos los colores consistentes: Rojo para High, Verde/Azul para Low
  scale_fill_manual(values = c("machen (High German)" = "#ca0020", 
                               "maken (Low German)" = "#0571b0")) +
  labs(
    title = "Distribución de la Muestra Analítica (Frase 17)",
    subtitle = "Frecuencia de variantes válidas para el modelado de la Línea Benrath",
    x = NULL, 
    y = "Número de localidades"
  ) +
  # Ajustamos límites para que el texto no se corte
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold")
  )
plot(p_distribucion_limpio)
ggsave(here(output_dir, "resultado_distribucion_variantes_final.png"), 
       p_distribucion_limpio, width = 9, height = 4, dpi = 300)
# ==============================================================================
# SUPLEMENTO 2: MAPA DE INCERTIDUMBRE (ERROR ESTÁNDAR)
# ==============================================================================
# Calculamos el error estándar de la predicción en el grid
pred_se <- predict(gam_model, newdata = pred_grid, type = "link", se.fit = TRUE)
pred_grid$se_link <- pred_se$se.fit

p_incertidumbre <- ggplot() +
  geom_tile(data = pred_grid, aes(x = X_meter, y = Y_meter, fill = se_link)) +
  geom_sf(data = germany_border, fill = NA, color = "white", size = 0.4) +
  scale_fill_viridis_c(option = "magma", name = "Error Estándar\n(Link Scale)") +
  labs(
    title = "Geografía de la Incertidumbre Lingüística",
    subtitle = "Zonas claras: Alta variabilidad o baja densidad de datos (isoglosas difusas)",
    caption = "Nota: El error aumenta en las fronteras del área de muestreo y zonas de contacto intenso."
  ) +
  theme_void()
plot(p_incertidumbre)
ggsave(here(output_dir, "analisis_incertidumbre_gam.png"), p_incertidumbre, width = 10, height = 10)

# ==============================================================================
# SUPLEMENTO 3: TRANSECTOS COMPARATIVOS (ASIMETRÍA OESTE-ESTE)
# ==============================================================================
# Calculamos X_meter para Longitud 7 (Oeste) y Longitud 13 (Este)
puntos_ref <- tibble(lon = c(7, 13), lat = c(51, 51)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3035) %>%
  st_coordinates()

x_oeste <- puntos_ref[1, 1]
x_este <- puntos_ref[2, 1]

# Generamos predicciones solo para esos dos carriles de Norte a Sur
y_seq <- seq(min(final_data$Y_meter), max(final_data$Y_meter), length.out = 200)
transect_grid <- bind_rows(
  tibble(X_meter = x_oeste, Y_meter = y_seq, Zona = "Oeste (Rin / 7°E)"),
  tibble(X_meter = x_este, Y_meter = y_seq, Zona = "Este (Sajonia / 13°E)")
)

transect_grid$prob <- predict(gam_model, newdata = transect_grid, type = "response")

p_transectos <- ggplot(transect_grid, aes(x = Y_meter, y = prob, color = Zona)) +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("blue", "red")) +
  labs(
    title = "Asimetría en la Transición Dialectal",
    subtitle = "La curva más empinada (roja) indica una isoglosa más brusca en el Este.",
    x = "Latitud Proyectada (Metros)", y = "Probabilidad de 'machen'"
  ) +
  theme_minimal()
plot(p_transectos)
ggsave(here(output_dir, "analisis_transectos_oeste_este.png"), p_transectos, width = 10, height = 6)

# ==============================================================================
# SUPLEMENTO 4: MAPA DE FIABILIDAD LOCAL 
# ==============================================================================
p_confianza <- ggplot(regional_stats) +
  # Usamos geom_tile sobre el grid de 50km que calculamos en la Fase 9
  geom_tile(aes(x = grid_x, y = grid_y, fill = nivel_confianza), alpha = 0.8) +
  geom_sf(data = germany_border, fill = NA, color = "black", size = 0.3) +
  scale_fill_manual(values = c(
    "ALTA (Consistencia total)" = "#2ca25f", 
    "MEDIA (Zona de transición)" = "#feb24c", 
    "BAJA (Alta disputa lingüística)" = "#de2d26"
  ), name = "Nivel de Confianza") +
  labs(
    title = "Validación de Confianza del Modelo Espacial",
    subtitle = "Basado en la precisión de la predicción local vs. datos reales de Wenker",
    caption = "Cuadrículas de 50x50 km."
  ) +
  theme_void()
plot(p_confianza)
ggsave(here(output_dir, "analisis_confianza_regional.png"), p_confianza, width = 10, height = 10)
