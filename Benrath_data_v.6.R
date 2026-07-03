# ==============================================================================
# PROTOCOLO DE ANÁLISIS GEOLINGÜÍSTICO: LA LÍNEA DE BENRATH (WENKERBÖGEN)
# ==============================================================================
# Autor: Ángeles González Miguel / Francisco Javier Muñoz-Acebes / 
# Universidad de Valladolid
# GIR: FILOLOGÍA DIGITAL
# ==============================================================================
# 0. CONFIGURACIÓN GLOBAL ------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, fs, here, sf, rnaturalearth, mgcv, gratia, spdep, 
  metR, patchwork, scales, tidygeocoder, cshapes, stringi
)

# Definición de rutas
data_dir   <- here("datos_wenker", "raw_wenker")
output_dir <- here("datos_wenker", "processed")
dir_create(data_dir, output_dir, recurse = TRUE)

# Crear directorios si no existen
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat(sprintf(">> Directorio de trabajo: %s\n", here()))
cat(sprintf(">> Directorio de datos: %s\n", data_dir))
cat(sprintf(">> Directorio de salida: %s\n", output_dir))

cat("=== INICIANDO PROTOCOLO BENRATH ===\n")

# ==============================================================================
# FASE 1: EXTRACCIÓN Y PROCESAMIENTO LINGÜÍSTICO -------------------------------
# ==============================================================================

repo_url <- "https://github.com/engsterhold/wenker-storage/archive/refs/heads/master.zip"
zip_path <- path(data_dir, "wenker_storage_snapshot.zip")

if (!file_exists(zip_path)) {
  cat(">> Descargando repositorio...\n")
  download.file(repo_url, destfile = zip_path, mode = "wb")
}

unzip(zip_path, exdir = data_dir, junkpaths = FALSE)
csv_files <- dir_ls(data_dir, glob = "*.csv", recurse = TRUE)

extract_wenker_data <- function(filepath) {
  raw_text <- read_file(filepath)
  # Se usa [ \\t] (espacio o tabulador) en vez de \\s genérico entre el nombre
  # de campo y el valor, para no cruzar saltos de línea cuando un campo está
  # vacío. (\\S.*) exige que el valor capturado empiece por un carácter no
  # blanco, de modo que un campo vacío se registre como NA en vez de
  # arrastrar el contenido del campo siguiente.
  ort_match     <- str_match(raw_text, "(?mi)^Ort[ \\t]*:[ \\t]*(\\S.*)$")
  ws17_match    <- str_match(raw_text, "(?mi)^WS17[ \\t]+(\\S.*)$")
  sprache_match <- str_match(raw_text, "(?mi)^Sprache[ \\t]*:[ \\t]*(\\S.*)$")
  
  tibble(
    file_id = path_file(filepath),
    original_ort = if (!is.na(ort_match[1, 2])) str_trim(str_remove(ort_match[1, 2], ";\\s*$")) else NA_character_,
    ws17_raw = if (!is.na(ws17_match[1, 2])) str_trim(ws17_match[1, 2]) else NA_character_,
    sprache_raw = if (!is.na(sprache_match[1, 2])) str_trim(sprache_match[1, 2]) else NA_character_
  ) %>%
    mutate(extraction_status = case_when(
      is.na(original_ort) & is.na(ws17_raw) ~ "EMPTY_FILE",
      is.na(original_ort) ~ "MISSING_ORT",
      is.na(ws17_raw) ~ "MISSING_WS17",
      TRUE ~ "OK"
    ))
}

cat(">> Procesando archivos lingüísticos...\n")
raw_data <- map_dfr(csv_files, extract_wenker_data)

# ==============================================================================
# FILTRO POR IDIOMA DECLARADO
# ==============================================================================
# Excluye los Bögen cuyo campo "Sprache" declara explícitamente una lengua
# distinta del alemán (p.ej. sorbio, polaco). Si el campo no existe, se
# asume alemán.
raw_data <- raw_data %>%
  mutate(es_aleman = is.na(sprache_raw) | str_detect(str_to_lower(sprache_raw), "deutsch"))

cat(sprintf(">> Excluidos por idioma declarado no alemán (campo 'Sprache'): %d Bögen\n",
            sum(!raw_data$es_aleman, na.rm = TRUE)))

clean_data <- raw_data %>%
  filter(extraction_status == "OK", es_aleman) %>%
  mutate(
    # Elimina anotaciones del transcriptor. Las anotaciones en prosa
    # (delimitador + contenido con espacio/coma, o de 4+ caracteres sin
    # espacio, p.ej. "{3 über a}", "[ne,m]ihen", "{durchgestrichen}") se
    # eliminan enteras, para no fundir texto ajeno dentro de la palabra
    # objetivo. Las anotaciones cortas de una sola letra (p.ej. "<a>" en
    # "mo<a>ke") no coinciden con estos patrones: solo se les retira el
    # delimitador en el paso siguiente, conservando la letra.
    ws17_norm = str_to_lower(ws17_raw) %>%
      str_replace_all("[<(\\[{][^<>()\\[\\]{}]*[\\s,][^<>()\\[\\]{}]*[>)\\]}]", "") %>%
      str_replace_all("[<(\\[{][^<>()\\[\\]{}]{4,}[>)\\]}]", "") %>%
      str_replace_all("[^\\p{L}\\s]", ""),
    
    # Normalización ortográfica: se conserva solo el contenido alfabético y
    # los espacios; a continuación se aplica una descomposición Unicode NFKD
    # (separa cualquier letra con diacrítico en su letra base más su marca o
    # marcas combinantes, incluida la s larga histórica "ſ"), y se eliminan
    # todas las marcas combinantes resultantes. El schwa fonético (ə/ɛ) se
    # normaliza aparte, ya que no es una letra latina con diacrítico y NFKD
    # no lo descompone.
    ws17_clean = ws17_norm %>%
      stringi::stri_trans_general("NFKD") %>%
      str_replace_all("\\p{Mn}", "") %>%
      str_replace_all("[əɛ]", "e"),
    
    # Clasificación léxica de la variante (machen/maken). Se admite una
    # racha inicial no anclada de hasta seis caracteres, que cubre prefijos
    # y fusiones dialectales (rein-, sauber-, zu-, auf-, entre otros) sin
    # necesidad de enumerarlos; el patrón discrimina principalmente por el
    # grupo consonántico final (ch/hh/hch/chch/x/h para el alto alemán;
    # k/kk/ck/g/gg, con una h epentética opcional, para el bajo alemán) y
    # admite las terminaciones -n y -ng.
    has_machen = str_detect(ws17_clean, "\\b[a-z]{0,6}?m[aeiou]{1,3}(hch|chch|ch|hh|x|h)[aeiou]{0,2}(n|ng)?\\b"),
    has_maken  = str_detect(ws17_clean, "\\b[a-z]{0,6}?m[aeiou]{1,3}h?(k|kk|ck|g|gg)[aeiou]{0,2}(n|ng)?\\b"),
    
    # Cuando una misma frase contiene coincidencias válidas para ambos
    # patrones (p.ej. porque otra palabra de la traducción, ajena al verbo
    # objetivo, coincide por azar con uno de los dos), se resuelve la
    # ambigüedad seleccionando la coincidencia que aparece más tarde en la
    # frase: en el enunciado 17 canónico, el verbo objetivo ("rein machen" /
    # "reen maken") cierra siempre la traducción.
    pos_machen = stringi::stri_locate_last_regex(
      ws17_clean, "\\b[a-z]{0,6}?m[aeiou]{1,3}(hch|chch|ch|hh|x|h)[aeiou]{0,2}(n|ng)?\\b"
    )[, "start"],
    pos_maken = stringi::stri_locate_last_regex(
      ws17_clean, "\\b[a-z]{0,6}?m[aeiou]{1,3}h?(k|kk|ck|g|gg)[aeiou]{0,2}(n|ng)?\\b"
    )[, "start"],
    
    is_hochdeutsch = case_when(
      has_machen & !has_maken ~ 1,
      !has_machen & has_maken ~ 0,
      has_machen & has_maken & pos_machen > pos_maken ~ 1,
      has_machen & has_maken & pos_maken > pos_machen ~ 0,
      TRUE ~ NA_real_
    ),
    linguistic_variant = ifelse(is_hochdeutsch == 1, "machen (High German)", "maken (Low German)")
  )

# Verificar y forzar creación de carpeta
if (!dir_exists(output_dir)) dir_create(output_dir, recurse = TRUE)

# ==============================================================================
# INFORME DE DIAGNÓSTICO SOBRE CASOS NO CLASIFICADOS
# ==============================================================================
# Exporta todos los casos que quedan sin clasificar, categorizados de forma
# automática y aproximada, para inspección manual. No detecta de forma
# automática los Bögen cuyo campo "WS17" no corresponde a la frase 17
# canónica (probablemente por usar una numeración de frases anterior a la
# estandarización de 1879); esos casos quedan en "revisar_manualmente".
# ==============================================================================
na_report <- clean_data %>%
  filter(is.na(is_hochdeutsch)) %>%
  mutate(
    categoria_diagnostica = case_when(
      is.na(ws17_raw) | ws17_raw == "" ~ "vacio_sin_respuesta",
      str_detect(str_to_lower(ws17_raw), "^\\{?leer\\}?$") ~ "placeholder_leer",
      str_detect(str_to_lower(ws17_raw), "nicht übersetzt|nicht uebersetzt|platzmangel|kein.*übersetz") ~ "no_traducido_explicito",
      # Variante no canónica de la frase 17: constatada en un clúster
      # geográfico del Bajo Rin, donde WS17 traduce "el hermano mayor irá de
      # aprendiz con el maestro" en vez de la frase de "limpiar con el cepillo".
      str_detect(ws17_clean, "(meest|meist|mest)[ae]?r.*(li[ae]|lih)r") |
        str_detect(ws17_clean, "(li[ae]|lih)r.*(meest|meist|mest)") ~ "frase17_no_canonica_hermano_aprendiz",
      # Sustitución léxica: el informante tradujo con un verbo distinto a
      # machen/maken (p.ej. "bürsten"/"putzen", frecuente en el subcorpus
      # suizo-alemánico; o una forma emparentada con el danés "gøre" en el
      # área fronteriza de Schleswig). No es un fallo de regex: la oposición
      # maken/machen simplemente no está presente en esa respuesta.
      str_detect(ws17_clean, "b[ou]e?rs?[tc]") ~ "sustitucion_lexica_buersten",
      str_detect(ws17_clean, "p[ou]tz|b[ou]e?tz") ~ "sustitucion_lexica_putzen",
      str_detect(ws17_clean, "g[ou]e?re? dem reen|g[ou]re ") ~ "sustitucion_lexica_danesa",
      nchar(ws17_clean) < 15 ~ "sospechosamente_corto",
      TRUE ~ "revisar_manualmente"
    )
  ) %>%
  select(file_id, original_ort, sprache_raw, ws17_raw, categoria_diagnostica)

cat(sprintf(">> Casos sin clasificar: %d de %d (%.1f%%)\n",
            nrow(na_report), nrow(clean_data),
            100 * nrow(na_report) / nrow(clean_data)))
print(count(na_report, categoria_diagnostica))

write_csv(na_report, path(output_dir, "informe_NA_diagnostico.csv"))

clean_data <- clean_data %>% filter(!is.na(is_hochdeutsch))

# Escribir
write_csv(clean_data, path(output_dir, "wenker_clean_linguistic.csv"))


# ==============================================================================
# FASE 2: GEOCODIFICACIÓN Y PROYECCIÓN ESPACIAL --------------------------------
# ==============================================================================
# Se construyen dos dominios territoriales:
#   (a) Alemania actual        -> polígono real del país contemporáneo
#   (b) Imperio alemán (histórico) -> frontera real de 1900, vía el paquete
#       'cshapes' (Weidmann & Gleditsch)
# ==============================================================================

cat(">> Geocodificando con ArcGIS...\n")

unique_locs <- clean_data %>%
  select(original_ort) %>%
  distinct() %>%
  mutate(search_query = original_ort) %>%
  geocode(address = search_query, method = 'arcgis', verbose = TRUE)

geocoded_full <- clean_data %>%
  left_join(unique_locs, by = "original_ort") %>%
  filter(!is.na(lat), !is.na(long))

write_csv(geocoded_full, path(output_dir, "wenker_geocoded_full.csv"))

sf_raw <- geocoded_full %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE)

# ---- DOMINIO A: Alemania actual (frontera real del país) ----------------------
# Se usa el polígono real de Alemania en vez de un bounding box rectangular,
# que capturaría localidades de países vecinos en las esquinas. El bounding
# box se mantiene como pre-filtro laxo, solo por rendimiento.
germany_polygon <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf") %>%
  st_transform(3035)

final_actual <- geocoded_full %>%
  filter(lat >= 46 & lat <= 56, long >= 4 & long <= 17) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3035) %>%
  st_filter(germany_polygon, .predicate = st_within) %>%
  mutate(X_meter = st_coordinates(.)[, 1], Y_meter = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

# ---- DOMINIO B: Imperio alemán histórico (frontera real, 'cshapes') -----------
# El país aparece etiquetado como "Germany (Prussia)" en esta fecha del
# conjunto de datos de cshapes.
imperio_poly <- cshp(date = as.Date("1900-01-01"), useGW = TRUE) %>%
  filter(country_name == "Germany (Prussia)") %>%
  st_transform(3035)

final_imperio <- sf_raw %>%
  st_transform(3035) %>%
  st_filter(imperio_poly, .predicate = st_within) %>%
  mutate(X_meter = st_coordinates(.)[, 1], Y_meter = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

cat(sprintf(">> Dominio Alemania actual: %d cuestionarios (%d localidades únicas)\n",
            nrow(final_actual), n_distinct(final_actual$original_ort)))
cat(sprintf(">> Dominio Imperio alemán histórico: %d cuestionarios (%d localidades únicas)\n",
            nrow(final_imperio), n_distinct(final_imperio$original_ort)))

write_csv(final_actual, path(output_dir, "wenker_final_actual.csv"))
write_csv(final_imperio, path(output_dir, "wenker_final_imperio.csv"))

# El resto del script (mapas, GAM/GLM, residuos, LISA, transectos, cartografía
# k=300) trabaja sobre el dominio "Alemania actual"; 'final_data' apunta a él.
final_data <- final_actual
write_csv(final_data, path(output_dir, "wenker_final_projected.csv"))
# =====================================================================
# FIGURA -1: Composición de la muestra (machen vs. maken)
# =====================================================================

freq_variantes <- clean_data %>% 
  count(linguistic_variant) %>%
  # ordenar para que machen quede arriba como en tu ejemplo
  mutate(linguistic_variant = fct_rev(fct_relevel(
    linguistic_variant,
    "maken (Low German)", "machen (High German)"
  )))

p_freq <- ggplot(freq_variantes,
                 aes(x = n,
                     y = linguistic_variant,
                     fill = linguistic_variant)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n),
            hjust = -0.1,
            size = 4,
            fontface = "bold") +
  scale_fill_manual(values = c(
    "machen (High German)" = "#c8102e",   # rojo
    "maken (Low German)"  = "#00529b"    # azul
  ), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = NULL, y = NULL,
    title = "Composición de la muestra analítica"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y  = element_text(size = 13),
    axis.text.x  = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_freq)
ggsave(here(output_dir, "figura1_machen_maken.png"),
       p_freq, width = 10, height = 6, dpi = 600)

# ==============================================================================
# QA ESPACIAL: VALIDACIÓN DE LA ISOGLOSA (PUNTOS PROYECTADOS)
# ==============================================================================

# 1. Preparar el mapa base en la misma proyección que los datos (EPSG:3035)
europe_base <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(3035)
germany_border <- ne_countries(country = "Germany", returnclass = "sf") |>
  st_transform(3035)

# 2. Generar el mapa de diagnóstico
qa_map <- ggplot() +
  # Fondo de Alemania
  geom_sf(data = germany_border, fill = "gray97", color = "gray80", size = 0.2) +
  
  # Puntos filtrados
  geom_point(data = final_data, 
             aes(x = X_meter, y = Y_meter, color = factor(is_hochdeutsch)), 
             size = 0.6, alpha = 0.7) +
  
  # Encuadre exacto al Bounding Box de Alemania
  coord_sf(
    crs = 3035, 
    xlim = c(st_bbox(germany_border)["xmin"], st_bbox(germany_border)["xmax"]), 
    ylim = c(st_bbox(germany_border)["ymin"], st_bbox(germany_border)["ymax"]),
    datum = st_crs(3035)
  ) +
  
  scale_color_manual(
    values = c("0" = "#00529b", "1" = "#c8102e"), 
    labels = c("maken", "machen"),
    name = "Variante"
  ) +
  theme_minimal() +
  labs(title = "QA Espacial: Solo Territorio Alemán")

print(qa_map)
ggsave(here(output_dir, "qa_mapa_proyeccion_alemania.png"),
       qa_map, width = 8, height = 10, dpi = 600)
# ==============================================================================
# FASE 3: MODELADO GAM Y COMPARATIVA -------------------------------------------
# ==============================================================================

cat(">> Ajustando modelos GAM y GLM...\n")
gam_model <- gam(is_hochdeutsch ~ s(X_meter, Y_meter, k = 300), 
                 data = final_data, family = binomial, method = "REML") # podemos escoger ML o REML

glm_model <- glm(is_hochdeutsch ~ X_meter + Y_meter, 
                 data = final_data, family = binomial)

# ==============================================================================
# TABLA COMPARATIVA: DOS DOMINIOS x (GLM + GAM k=60,100,200,300) --------------
# ==============================================================================
# Calcula AIC, devianza explicada, precisión de clasificación y R² ajustado
# para ambos dominios (Alemania actual e Imperio alemán histórico).
# ==============================================================================

fit_domain_stats <- function(data, domain_label, k_values = c(60, 100, 200, 300)) {
  
  glm_m    <- glm(is_hochdeutsch ~ X_meter + Y_meter, data = data, family = binomial)
  prob_glm <- predict(glm_m, type = "response")
  
  glm_row <- tibble(
    Dominio = domain_label, Modelo = "GLM", k = NA_integer_,
    AIC = AIC(glm_m),
    Devianza_Explicada = (glm_m$null.deviance - glm_m$deviance) / glm_m$null.deviance,
    Precision = mean(ifelse(prob_glm > 0.5, 1, 0) == data$is_hochdeutsch),
    R2_ajustado = NA_real_
  )
  
  gam_rows <- map_dfr(k_values, function(k_val) {
    m    <- gam(is_hochdeutsch ~ s(X_meter, Y_meter, k = k_val),
                data = data, family = binomial, method = "REML")
    prob <- predict(m, type = "response")
    tibble(
      Dominio = domain_label, Modelo = paste0("GAM (k=", k_val, ")"), k = k_val,
      AIC = AIC(m),
      Devianza_Explicada = summary(m)$dev.expl,
      Precision = mean(ifelse(prob > 0.5, 1, 0) == data$is_hochdeutsch),
      R2_ajustado = summary(m)$r.sq
    )
  })
  
  bind_rows(glm_row, gam_rows)
}

tabla_actual  <- fit_domain_stats(final_actual,  "Alemania actual")
tabla_imperio <- fit_domain_stats(final_imperio, "Imperio alemán (histórico)")

tabla_completa <- bind_rows(tabla_actual, tabla_imperio) %>%
  mutate(
    Devianza_Explicada = percent(Devianza_Explicada, accuracy = 0.1),
    Precision           = percent(Precision, accuracy = 0.1),
    R2_ajustado         = round(R2_ajustado, 2)
  )

cat("\n==============================================================\n")
cat("TABLA COMPARATIVA COMPLETA (copiar a la Tabla del artículo):\n")
cat("==============================================================\n")
print(tabla_completa, n = Inf)
write_csv(tabla_completa, here(output_dir, "tabla_comparativa_dominios.csv"))

cat(">> Ajustando modelos GAM y GLM...\n") 

# ==============================================================================
# MÉTRICAS DEL DOMINIO ALEMÁN ACTUAL
# ==============================================================================

cat(">> Calculando métricas para reporte en artículo...\n")

# 1. Número de localidades en dominio alemán actual
n_alemania_actual <- nrow(final_data)

# 2. Precisión de clasificación del GAM (k=300)
prob_pred_de <- predict(gam_model, type = "response")
clase_pred_de <- ifelse(prob_pred_de > 0.5, 1, 0)
precision_de <- sum(clase_pred_de == final_data$is_hochdeutsch) / nrow(final_data)

# 3. R² ajustado del GAM
r2_ajustado <- summary(gam_model)$r.sq

# 4. Devianza explicada
dev_expl <- summary(gam_model)$dev.expl

# REPORTE EN CONSOLA (copia estos valores a tu artículo)
cat("\n==============================================================\n")
cat("DATOS:\n")
cat("==============================================================\n")
cat(sprintf("n (Alemania actual): %d localidades\n", n_alemania_actual))
cat(sprintf("Precisión clasificación: %.1f%%\n", precision_de * 100))
cat(sprintf("R² ajustado: %.3f\n", r2_ajustado))
cat(sprintf("Devianza explicada: %.1f%%\n", dev_expl * 100))
cat("==============================================================\n\n")
# Localidades ÚNICAS (sin repeticiones)
n_localidades_unicas <- final_data %>% 
  distinct(original_ort) %>% 
  nrow()

cat(sprintf("Localidades únicas: %d\n", n_localidades_unicas))
cat(sprintf("Cuestionarios totales: %d\n", n_alemania_actual))

aic_glm <- AIC(glm_model)
dev_glm <- (glm_model$null.deviance - glm_model$deviance) / glm_model$null.deviance
prob_glm <- predict(glm_model, type = "response")
clase_glm <- ifelse(prob_glm > 0.5, 1, 0)
precision_glm <- sum(clase_glm == final_data$is_hochdeutsch) / nrow(final_data)

cat(sprintf("AIC GLM: %.0f\n", aic_glm))
cat(sprintf("Devianza GLM: %.1f%%\n", dev_glm * 100))
cat(sprintf("Precisión GLM: %.1f%%\n", precision_glm * 100))

# Malla de predicción
grid_res <- 3000 # resolución de la malla en metros
bbox <- list(
  x = seq(min(final_data$X_meter), max(final_data$X_meter), by = grid_res),
  y = seq(min(final_data$Y_meter), max(final_data$Y_meter), by = grid_res)
)
pred_grid <- expand.grid(X_meter = bbox$x, Y_meter = bbox$y)
pred_grid$prob <- as.numeric(predict(gam_model, newdata = pred_grid, type = "response"))

# Frontera de Alemania para recortes
germany_border <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf") %>%
  st_transform(3035)

# ==============================================================================
# FASE 4: ANÁLISIS DE RESIDUOS Y CLÚSTERES (LISA) ------------------------------
# ==============================================================================

analysis_data <- final_data %>%
  mutate(
    prob_predicha = as.numeric(predict(gam_model, type = "response")),
    residuo = is_hochdeutsch - prob_predicha
  )

# LISA (Autocorrelación espacial)
coords <- cbind(analysis_data$X_meter, analysis_data$Y_meter)
nb <- dnearneigh(coords, 0, 25000)
weights <- nb2listw(nb, style = "W", zero.policy = TRUE)
lisa <- localmoran(analysis_data$residuo, weights, zero.policy = TRUE)

analysis_data <- analysis_data %>%
  mutate(
    p_val = lisa[,5],
    cluster = case_when(
      p_val > 0.05 ~ "No significativo",
      residuo > 0 ~ "Hotspot (machen)",
      residuo < 0 ~ "Coldspot (maken)",
      TRUE ~ "Otros"
    )
  )

# ==============================================================================
# FASE 5: VISUALIZACIONES FINALES ----------------------------------------------
# ==============================================================================

# 1. Mapa de la Isoglosa
p_isoglosa <- ggplot() +
  geom_tile(data = pred_grid, aes(X_meter, Y_meter, fill = prob)) +
  geom_contour(data = pred_grid, aes(X_meter, Y_meter, z = prob), breaks = 0.5, color = "white", size = 1) +
  geom_sf(data = germany_border, fill = NA, color = "black") +
  scale_fill_viridis_c(option = "magma", labels = percent, name = "Prob. machen") +
  labs(title = "Línea de Benrath (GAM)") +
  theme_void()
plot(p_isoglosa) # la curva fuera del area de Alemania es un comportamiento normal del modelo ante falta de datos para completar la predicción

# 2. Mapa de Fiabilidad Regional
grid_size <- 50000
regional_stats <- analysis_data %>%
  mutate(grid_x = floor(X_meter/grid_size)*grid_size, grid_y = floor(Y_meter/grid_size)*grid_size) %>%
  group_by(grid_x, grid_y) %>%
  summarise(prec = mean(abs(residuo) < 0.3), .groups = 'drop') %>%
  mutate(Confianza = case_when(prec > 0.8 ~ "ALTA", prec > 0.5 ~ "MEDIA", TRUE ~ "BAJA"))

p_confianza <- ggplot(regional_stats) +
  geom_sf(data = germany_border, fill = "grey85", color = "black") +
  geom_tile(aes(grid_x, grid_y, fill = Confianza), alpha = 0.8, color = "white") +
  geom_sf(data = germany_border, fill = NA, color = "black") +
  # coord_sf explícito: sin esto, ggplot no calcula bien la extensión
  # combinada de geom_tile() + geom_sf(), y el mapa sale recortado.
  coord_sf(xlim = st_bbox(germany_border)[c("xmin", "xmax")],
           ylim = st_bbox(germany_border)[c("ymin", "ymax")],
           expand = TRUE) +
  scale_fill_manual(values = c("ALTA" = "#1b9e77", "MEDIA" = "#e6ab02", "BAJA" = "#d95f02"),
                    name = "Confianza", na.value = "grey85") +
  labs(title = "Fiabilidad Local del Modelo",
       subtitle = "Resolución de cuadrícula: 50×50 km. Las celdas en gris no contienen cuestionarios Wenker.") +
  theme_void()
plot(p_confianza)
ggsave(path(output_dir, "mapa_fiabilidad.png"), p_confianza, width = 8, height = 10)
# 3. Curva de Transición (S-Curve)
p_curve <- ggplot(analysis_data, aes(x = Y_meter, y = is_hochdeutsch)) +
  geom_jitter(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "red") +
  labs(title = "Transición Norte-Sur", x = "Latitud (m)", y = "Prob. machen")
plot(p_curve)

# ==============================================================================
# EXPORTACIÓN DE RESULTADOS ----------------------------------------------------
# ==============================================================================

cat(">> Guardando productos finales...\n")

ggsave(path(output_dir, "mapa_isoglosa_final.png"), p_isoglosa, width = 8, height = 10)
ggsave(path(output_dir, "mapa_fiabilidad_final.png"), p_confianza, width = 8, height = 10)
ggsave(path(output_dir, "curva_transicion.png"), p_curve, width = 7, height = 5)

# Dashboard combinado
dashboard <- (p_isoglosa + p_confianza) / p_curve + plot_annotation(title = "Análisis Wenker: Línea de Benrath")
ggsave(path(output_dir, "wenker_dashboard_final.png"), dashboard, width = 12, height = 14)
plot(dashboard)

# CSV de enclaves
analysis_data %>%
  filter(p_val < 0.05 & abs(residuo) > 0.6) %>%
  select(original_ort, is_hochdeutsch, prob_predicha, residuo, cluster) %>%
  write_csv(path(output_dir, "enclaves_detectados.csv"))

# ==============================================================================
# FASE 10: MÉTRICAS AVANZADAS 
# ==============================================================================

cat(">> Calculando gradientes espaciales y métricas de validación...\n")

# 1. CÁLCULO DE GRADIENTE (Intensidad del cambio manual) -----------------------
# Definimos un pequeño incremento (1 metro) para calcular la pendiente
h <- 1 

# Creamos dos mallas desplazadas para ver cuánto cambia la probabilidad
grid_x_plus <- pred_grid %>% mutate(X_meter = X_meter + h)
grid_y_plus <- pred_grid %>% mutate(Y_meter = Y_meter + h)

# Calculamos predicciones en escala 'link' (log-odds) para mayor precisión lineal
p_base <- predict(gam_model, newdata = pred_grid, type = "link")
p_x    <- predict(gam_model, newdata = grid_x_plus, type = "link")
p_y    <- predict(gam_model, newdata = grid_y_plus, type = "link")

# Calculamos las derivadas parciales (pendiente en X y pendiente en Y)
df_dx <- (p_x - p_base) / h
df_dy <- (p_y - p_base) / h

# El gradiente es la magnitud del vector (hipotenusa de las pendientes)
# Representa la "velocidad" a la que cambia el dialecto en ese punto
pred_grid$gradiente <- sqrt(df_dx^2 + df_dy^2)

# Visualización del Gradiente (Fuerza de la Isoglosa)
p_gradiente <- ggplot() +
  geom_tile(data = pred_grid, aes(x = X_meter, y = Y_meter, fill = gradiente)) +
  geom_sf(data = germany_border, fill = NA, color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "inferno", 
    name = "Pendiente\nDialectal",
    guide = guide_colorbar(barheight = unit(5, "cm"))
  ) +
  labs(
    title = "Intensidad de la Transición (Gradiente)",
    subtitle = "Zonas 'calientes' indican una frontera lingüística abrupta (Muro).",
    caption = "Cálculo: Magnitud del vector de derivadas parciales (X, Y)"
  ) +
  theme_void()
plot(p_gradiente)
ggsave(here(output_dir, "mapa_gradiente_dialectal.png"), p_gradiente, width = 10, height = 10)

# 2. MAPA DE INCERTIDUMBRE ------------------------------------
preds_se <- predict(gam_model, newdata = pred_grid, type = "response", se.fit = TRUE)
pred_grid$se_fit <- preds_se$se.fit

p_incertidumbre_pro <- ggplot() +
  geom_tile(data = pred_grid, aes(x = X_meter, y = Y_meter, fill = se_fit)) +
  geom_sf(data = germany_border, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Error Estándar") +
  labs(
    title = "Mapa de Incertidumbre Predictiva",
    subtitle = "Zonas rojas: áreas donde el modelo tiene menos confianza estadística."
  ) +
  theme_void()
plot(p_incertidumbre_pro)
ggsave(here(output_dir, "mapa_incertidumbre_predictiva.png"), p_incertidumbre_pro, width = 10, height = 10)
# 3. MATRIZ DE CONFUSIÓN -------------------------------------------------------
# Clasificación final
analisis_validacion <- analysis_data %>%
  mutate(
    clase_real = factor(is_hochdeutsch, levels = c(0, 1), labels = c("maken", "machen")),
    clase_pred = factor(ifelse(prob_predicha > 0.5, 1, 0), levels = c(0, 1), labels = c("maken", "machen"))
  )

matriz <- table(Real = analisis_validacion$clase_real, Predicho = analisis_validacion$clase_pred)
precision_global <- sum(diag(matriz)) / sum(matriz)

# Imprimir resultados
cat("\n==================================================================\n")
cat(sprintf("PRECISIÓN GLOBAL DEL MODELO: %s%%\n", round(precision_global * 100, 2)))
print(matriz)
cat("==================================================================\n")

# Guardar gráficos
ggsave(here(output_dir, "analisis_gradiente_frontera.png"), p_gradiente, width = 10, height = 10)
ggsave(here(output_dir, "analisis_incertidumbre_se.png"), p_incertidumbre_pro, width = 10, height = 10)
# ==============================================================================
# ESTADÍSTICAS COMPARATIVAS DE RENDIMIENTO
# ==============================================================================

stats_comparison <- tibble(
  Modelo = c("GLM (Lineal)", "GAM (No-Lineal)"),
  Parametros = c("Coordenadas X, Y", "Splines (k=300)"),
  # Cálculo de AIC
  AIC = c(AIC(glm_model), AIC(gam_model)),
  # Cálculo de Devianza Explica (Manual para GLM, Automática para GAM)
  Devianza_Explicada = c(
    (glm_model$null.deviance - glm_model$deviance) / glm_model$null.deviance,
    summary(gam_model)$dev.expl
  )
)

# Imprimir tabla con formato de porcentaje
stats_comparison %>%
  mutate(Devianza_Explicada = scales::percent(Devianza_Explicada, accuracy = 0.1)) %>%
  print()

# Ajuste final de alta resolución
gam_model_final <- gam(
  is_hochdeutsch ~ s(X_meter, Y_meter, k = 300), 
  data = final_data, 
  family = binomial, 
  method = "REML"
)
# Verifica precisión
prob_predicha_nueva <- as.numeric(predict(gam_model_final, type = "response"))
clase_pred_nueva <- ifelse(prob_predicha_nueva > 0.5, 1, 0)
precision_nueva <- sum(clase_pred_nueva == final_data$is_hochdeutsch) / nrow(final_data)

cat(sprintf("Nueva Precisión Global (k=300): %s%%\n", round(precision_nueva * 100, 2)))

#CÁLCULO DE RESIDUOS Y PROBABILIDADES --------------------------------------

analysis_data <- final_data %>%
  mutate(
    # Obtenemos la probabilidad (0 a 1) según el modelo GAM
    prob_predicha = predict(gam_model, newdata = ., type = "response"),
    # Residuo: 1 (Machen real) - Prob (Predicción)
    # Si residuo > 0: El punto es 'machen' pero el modelo lo situaba en zona 'maken'
    # Si residuo < 0: El punto es 'maken' pero el modelo lo situaba en zona 'machen'
    residuo = is_hochdeutsch - prob_predicha
  )

# 2. IDENTIFICACIÓN DE ENCLAVES (ANOMALÍAS) ------------------------------------

enclaves <- analysis_data %>%
  mutate(
    tipo_anomalia = case_when(
      is_hochdeutsch == 1 & prob_predicha < 0.2 ~ "Isla de Machen (en zona Maken)",
      is_hochdeutsch == 0 & prob_predicha > 0.8 ~ "Isla de Maken (en zona Machen)",
      TRUE ~ "Normal"
    )
  ) %>%
  filter(tipo_anomalia != "Normal")

cat(sprintf(">> Análisis finalizado: %d localidades (%.2f%%) actúan como enclaves.\n", 
            nrow(enclaves), (nrow(enclaves)/nrow(analysis_data))*100))

# ==============================================================================
# CARTOGRAFÍA (MODELO DE ALTA RESOLUCIÓN k=300)
# ==============================================================================

cat(">> Generando cartografía final con k=300...\n")

# 1. Actualizar malla de predicción
# Usamos escala 'response' para la probabilidad y 'link' para el gradiente
pred_grid$prob <- as.numeric(predict(gam_model_final, newdata = pred_grid, type = "response"))

# 2. Recalcular Gradiente con el  modelo
h <- 1
p_base_link <- predict(gam_model_final, newdata = pred_grid, type = "link")
p_x_link    <- predict(gam_model_final, newdata = pred_grid %>% mutate(X_meter = X_meter + h), type = "link")
p_y_link    <- predict(gam_model_final, newdata = pred_grid %>% mutate(Y_meter = Y_meter + h), type = "link")

pred_grid$gradiente <- sqrt(((p_x_link - p_base_link)/h)^2 + ((p_y_link - p_base_link)/h)^2)

# 3. MAPA A:  Isoglosa 
p_isoglosa_final <- ggplot() +
  geom_tile(data = pred_grid, aes(x = X_meter, y = Y_meter, fill = prob)) +
  geom_contour(data = pred_grid, aes(x = X_meter, y = Y_meter, z = prob), 
               breaks = 0.5, color = "white", linewidth = 0.8) +
  geom_sf(data = germany_border, fill = NA, color = "black", linewidth = 0.4) +
  # coord_sf explícito para no recortar el mapa (ver nota en p_confianza)
  coord_sf(xlim = st_bbox(germany_border)[c("xmin", "xmax")],
           ylim = st_bbox(germany_border)[c("ymin", "ymax")],
           expand = TRUE) +
  scale_fill_viridis_c(option = "magma", labels = percent, name = "Prob. machen") +
  labs(title = "Isoglosa de Benrath: Modelo Final (k=300)",
       subtitle = "La línea blanca delimita la frontera exacta 50/50 entre variantes.",
       caption = "Datos: Corpus Wenker | Modelo: GAM (Thin Plate Splines)") +
  theme_void()
plot(p_isoglosa_final)

# 4. MAPA B: Intensidad del Cambio (Gradiente)
p_gradiente_final <- ggplot() +
  geom_tile(data = pred_grid, aes(x = X_meter, y = Y_meter, fill = gradiente)) +
  geom_sf(data = germany_border, fill = NA, color = "white", linewidth = 0.3) +
  coord_sf(xlim = st_bbox(germany_border)[c("xmin", "xmax")],
           ylim = st_bbox(germany_border)[c("ymin", "ymax")],
           expand = TRUE) +
  scale_fill_viridis_c(option = "inferno", name = "Intensidad") +
  labs(title = "Gradiente Espacial: Rigidez de la Frontera",
       subtitle = "Zonas brillantes indican un cambio lingüístico abrupto (isoglosa compacta).",
       caption = "La intensidad refleja la velocidad de cambio en el espacio proyectado.") +
  theme_void()
plot(p_gradiente_final)
# Visualizar y Guardar
print(p_isoglosa_final)
ggsave(here(output_dir, "benrath_isoglosa_k300.png"), p_isoglosa_final, width = 10, height = 12)
ggsave(here(output_dir, "benrath_gradiente_k300.png"), p_gradiente_final, width = 10, height = 12)


stats_table <- tibble(
  Modelo = c("GLM (Lineal)", "GAM (k=60)", "GAM (k=300)"),
  Parametros = c("Coordenadas X, Y", "Splines (k=60)", "Splines (k=300)"),
  
  # Cálculo de AIC para cada uno
  AIC = c(AIC(glm_model), AIC(gam_model), AIC(gam_model_final)),
  
  # Cálculo de Devianza Explicada
  Devianza_Explicada = c(
    (glm_model$null.deviance - glm_model$deviance) / glm_model$null.deviance, # GLM manual
    summary(gam_model)$dev.expl,                                           # GAM k=60
    summary(gam_model_final)$dev.expl                                      # GAM k=200
  )
) %>%
  mutate(Devianza_Explicada = percent(Devianza_Explicada, accuracy = 0.1))

# Visualizar tabla
print(stats_table)

# ==============================================================================
# GENERACIÓN DEL MAPA DE ISOGLOSA 
# ==============================================================================


# Cargamos la frontera de Alemania y la proyectamos a nuestro sistema métrico (EPSG:3035)
germany_border_sf <- ne_countries(scale = "medium", country = "Germany", returnclass = "sf") %>%
  st_transform(3035)

# Convertir la malla de predicción a objeto espacial ---
pred_grid_sf <- pred_grid %>%
  st_as_sf(coords = c("X_meter", "Y_meter"), crs = 3035)

# La Intersección  ---
pred_grid_recortado_sf <- st_intersection(pred_grid_sf, germany_border_sf)

# Preparar para ggplot ---
# Para usar geom_tile de forma eficiente, volvemos a convertir el objeto espacial a una tabla normal,
# extrayendo las coordenadas X e Y limpias.
pred_grid_final_plot <- pred_grid_recortado_sf %>%
  mutate(
    X_plot = st_coordinates(.)[,1],
    Y_plot = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() # Quitamos la columna de geometría compleja

# --- PASO 5: Generar el Gráfico Final ---
cat(">> Generando el gráfico final recortado...\n")

p_isoglosa_recortada <- ggplot() +
  # CAPA 1:  relleno de color 
  geom_tile(data = pred_grid_final_plot, aes(x = X_plot, y = Y_plot, fill = prob)) +
  
  # CAPA 2: La línea blanca del 50% 
  geom_contour(data = pred_grid_final_plot, aes(x = X_plot, y = Y_plot, z = prob),
               breaks = 0.5, color = "white", linewidth = 1) +
  
  # CAPA 3: El borde negro de Alemania encima de todo 
  geom_sf(data = germany_border_sf, fill = NA, color = "black", linewidth = 0.6) +
  
  # Estética y etiquetas
  scale_fill_viridis_c(
    option = "magma",
    labels = scales::percent_format(accuracy = 1),
    name = "Probabilidad\nde 'machen'"
  ) +
  labs(
    title = "La Línea de Benrath: Isoglosa Modelada (GAM k=300)",
    subtitle = "Probabilidad predicha de la variante altoalemana 'machen'. La línea blanca indica la frontera del 50%.",
    caption = "Datos: Corpus Wenker | Proyección: ETRS89-LAEA | Recorte: Fronteras actuales"
  ) +
  theme_void() + # theme_void quita ejes y fondos grises
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15)),
    legend.position = "right"
  )

# Visualizar el resultado
print(p_isoglosa_recortada)

# Guardar  versión final 
ggsave(here(output_dir, "benrath_isoglosa_final_recortada.png"), 
       p_isoglosa_recortada, width = 10, height = 12, dpi = 600, bg = "white")



# ==============================================================================
# SUPLEMENTO: TRANSECTOS COMPARATIVOS (ASIMETRÍA OESTE-ESTE)
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
# ANÁLISIS DE RESIDUOS Y DETECCIÓN DE ENCLAVES
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
  # 1. Convertimos las columnas conflictivas en números simples
  mutate(
    prob_predicha = as.numeric(prob_predicha),
    residuo = as.numeric(residuo)
  ) %>%
  # 2. Seleccionamos las columnas
  select(original_ort, is_hochdeutsch, prob_predicha, residuo, tipo_anomalia) %>%
  # 3. Importante: Si es un objeto sf, hay que quitar la geometría para el CSV
  st_drop_geometry() %>% 
  # 4. Guardar
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
#  ANÁLISIS DE AUTOCORRELACIÓN ESPACIAL (LISA)
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
# Los puntos no significativos se dibujan primero, pequeños y translúcidos,
# de fondo; los significativos se dibujan encima, más grandes y opacos, para
# que destaquen sobre el resto de la muestra.
analysis_data_no_sig <- analysis_data %>% filter(cluster == "No significativo")
analysis_data_sig    <- analysis_data %>% filter(cluster != "No significativo")

p_clusters <- ggplot() +
  geom_sf(data = germany_border, fill = "gray93", color = "gray40") +
  geom_point(data = analysis_data_no_sig, aes(x = X_meter, y = Y_meter),
             color = "gray75", size = 0.6, alpha = 0.5) +
  geom_point(data = analysis_data_sig, aes(x = X_meter, y = Y_meter, color = cluster),
             size = 1.8, alpha = 0.9) +
  coord_sf(xlim = st_bbox(germany_border)[c("xmin", "xmax")],
           ylim = st_bbox(germany_border)[c("ymin", "ymax")],
           expand = TRUE) +
  scale_color_manual(values = c(
    "Hotspot (H-H): Foco de machen" = "#ca0020", 
    "Coldspot (L-L): Foco de maken" = "#0571b0",
    "Outlier/Mezcla" = "#f4a582"
  ), name = "Clúster (p < 0,05)") +
  labs(title = "Análisis de Autocorrelación Espacial (LISA)",
       subtitle = "Identificación de regiones con comportamiento lingüístico divergente") +
  theme_minimal()
plot(p_clusters)
ggsave(here(output_dir, "wenker_spatial_clusters.png"), p_clusters, width = 10, height = 8)



save.image("workspace_wenker.RData")

# Filtrado de puntos dentro de Alemania
sf_projected <- sf_raw %>% st_transform(3035)
germany_border <- germany_border %>% st_transform(3035)

# Mantener solo puntos dentro de Alemania
sf_de <- sf_projected[st_within(sf_projected, germany_border, sparse = FALSE), ]
sf_de <- sf_de %>%
  mutate(
    Variante = factor(is_hochdeutsch,
                      levels = c(0, 1),
                      labels = c("maken (Low German)", "machen (High German)"))
  )

p_muestra_de <- ggplot() +
  geom_sf(data = germany_border, fill = "grey95", color = "grey80") +
  geom_sf(data = sf_de, aes(color = Variante), size = 0.8, alpha = 0.8) +
  scale_color_manual(
    values = c("maken (Low German)" = "#1f78b4",
               "machen (High German)" = "#e31a1c")
  ) +
  coord_sf(xlim = st_bbox(germany_border)[c("xmin","xmax")],
           ylim = st_bbox(germany_border)[c("ymin","ymax")]) +
  labs(x = "Longitud proyectada (m)", y = "Y_meter", color = "Variante") +
  theme_minimal(base_size = 12)

print(p_muestra_de)
ggsave(path(output_dir, "muestra_wenker_alemania.png"), p_muestra_de, width = 8, height = 10)


# ==============================================================================
# ANEXO: ANÁLISIS Y CARTOGRAFÍA A ESCALA CONTINENTAL (EUROPA COMPLETA)
# ==============================================================================
# Este bloque procesa la totalidad del corpus geocodificado sin las
# restricciones geográficas de los dominios principales, evaluando el 
# comportamiento global del modelo GAM y la distribución macro de la muestra.
# ==============================================================================

cat("\n>> [ANEXO] Iniciando protocolo de extensión continental...\n")

# 1. PREPARACIÓN DE DATOS Y MODELADO CONTINENTAL -------------------------------
# Proyectar todos los puntos originales (sf_raw de Fase 2) a EPSG:3035
sf_europa_proj <- sf_raw %>% st_transform(3035)
coords_europa  <- st_coordinates(sf_europa_proj)

final_europa <- sf_europa_proj %>%
  mutate(
    X_meter = coords_europa[, 1],
    Y_meter = coords_europa[, 2]
  ) %>%
  st_drop_geometry()

# Ajuste del modelo GAM global con todos los puntos del corpus
gam_europa_k300 <- gam(
  is_hochdeutsch ~ s(X_meter, Y_meter, k = 300),
  data   = final_europa,
  family = binomial,
  method = "REML"
)

# Reporte de métricas continentales en la consola de R
cat("\n==============================================================\n")
cat("RESUMEN GAM CONTINENTAL (ANEXO - EUROPA COMPLETE):\n")
cat("==============================================================\n")
print(summary(gam_europa_k300))

# Matriz de confusión global y precisión predictiva macro
prob_europa   <- predict(gam_europa_k300, type = "response")
clase_pred_eu <- ifelse(prob_europa > 0.5, 1, 0)
matriz_europa <- table(Real = final_europa$is_hochdeutsch, Predicho = clase_pred_eu)
precision_europa <- sum(diag(matriz_europa)) / sum(matriz_europa)

cat(sprintf("Precisión Global Continental: %.2f%%\n", precision_europa * 100))
print(matriz_europa)
cat("==============================================================\n\n")


# 2. CARTOGRAFÍA PANEUROPEA DE LA MUESTRA --------------------------------------
cat(">> [ANEXO] Generando mapa base de Europa y distribución global...\n")

# Descargar y proyectar el mapa base de Europa vía Natural Earth
europe_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(region_un == "Europe") %>%
  st_transform(3035)

# Preparar objeto espacial con etiquetas de factores para la leyenda
sf_europa_plot <- sf_raw %>% 
  st_transform(3035) %>%
  mutate(
    Variante = factor(is_hochdeutsch, 
                      levels = c(0, 1), 
                      labels = c("maken (Low German)", "machen (High German)"))
  )

# Construcción del gráfico con ggplot2
p_europa <- ggplot() +
  geom_sf(data = europe_sf, fill = "grey95", color = "grey80") +
  geom_sf(data = sf_europa_plot, aes(color = Variante), size = 0.5, alpha = 0.6) +
  scale_color_manual(
    values = c("maken (Low German)" = "#1f78b4", "machen (High German)" = "#e31a1c"),
    name = "Variante Dialectal"
  ) +
  coord_sf(
    xlim = st_bbox(europe_sf)[c("xmin", "xmax")],
    ylim = st_bbox(europe_sf)[c("ymin", "ymax")]
  ) +
  labs(
    title = "Muestra Wenker: Distribución de Puntos en Europa",
    subtitle = "Visualización completa del corpus geocodificado (Anexo)",
    x = "Longitud proyectada (m)",
    y = "Latitud proyectada (m)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid = element_blank()
  )

# Renderizar y guardar el mapa en el directorio de salida
print(p_europa)
ggsave(path(output_dir, "wenker_muestra_europa_completa.png"), p_europa, width = 11, height = 8, dpi = 300)

cat(">> [ANEXO] Protocolo continental finalizado y productos guardados.\n")
# ==============================================================================