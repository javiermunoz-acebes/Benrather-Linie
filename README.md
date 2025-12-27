# Benrather-Linie

## Análisis de la Línea Benrath en R

Este proyecto utiliza R para visualizar y analizar la Línea Benrath, una isoglosa lingüística que separa los dialectos del bajo alemán (Niederdeutsch) de los dialectos del alto alemán (Hochdeutsch) en Alemania.

## Contenido

1. [Descripción](#descripción)
2. [Requisitos](#requisitos)
3. [Instalación](#instalación)
4. [Uso](#uso)
5. [Explicación del Código](#explicación-del-código)

## Descripción

El código crea dos visualizaciones diferentes de la Línea Benrath:

1. Un mapa que muestra la distribución geográfica de los grupos dialectales.
2. Un mapa que ilustra las características fonéticas específicas asociadas con la Línea Benrath.

## Requisitos

Para ejecutar este código, necesitarás R y las siguientes bibliotecas:

- ggplot2
- sf
- rnaturalearth
- rnaturalearthdata

## Instalación

Para instalar las bibliotecas necesarias, ejecuta los siguientes comandos en R:

```r
install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata"))
```

## Uso

1. Asegúrate de tener todos los requisitos instalados.
2. Copia el código en un script de R.
3. Ejecuta el script para generar los mapas.

## Explicación del Código

### Carga de Bibliotecas y Datos

```r
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

url <- "https://raw.githubusercontent.com/javiermunoz-acebes/Benrather-Linie/main/linea_benrath_data.csv"
datos <- read.csv(url)

datos_benrath <- read.csv("linea_benrath_data.csv", sep = ";", encoding = "UTF-8")
```

Esta sección carga las bibliotecas necesarias y los datos de la Línea Benrath desde un archivo CSV.

### Preparación de Datos

```r
alemania <- ne_countries(scale = "medium", returnclass = "sf", country = "germany")

puntos_frontera <- data.frame(
  region = c("Düsseldorf", "Benrath", "Wuppertal", "Bochum", "Dortmund", "Paderborn", "Hannover", "Magdeburg", "Berlin", "Cottbus"), 
  longitude = c(6.7735, 6.8755, 7.1800, 7.2161, 7.4652, 8.7521, 9.7320, 11.6276, 13.4050, 14.3349),
  latitude = c(51.2277, 51.1616, 51.2562, 51.4818, 51.5136, 51.718, 52.3759, 52.1205, 52.5200, 51.7607),
  dialect_group = rep("Niederdeutsch", 10)
)
```

Aquí se obtiene el mapa base de Alemania y se crea un dataframe con los puntos de la frontera dialectal.

### Creación de Gráficos

#### Mapa de Grupos Dialectales

```r
ggplot() +
  geom_sf(data = alemania) +
  geom_point(data = datos_benrath, aes(x = longitude, y = latitude, color = dialect_group), size = 3) +
  geom_text(data = datos_benrath, aes(x = longitude, y = latitude, label = region),
            vjust = -1, size = 3) +
  geom_path(data = puntos_frontera, 
            aes(x = longitude, y = latitude), 
            color = "blue", linetype = "solid", size = 1) +
  theme_minimal() +
  labs(title = "Línea Benrath", color = "Grupo Dialectal") +
  theme(legend.position = "bottom")
```

Este código crea un mapa que muestra la distribución de los grupos dialectales a lo largo de la Línea Benrath.

#### Mapa de Características Fonéticas

```r
ggplot() +
  geom_sf(data = alemania) +
  geom_point(data = datos_benrath, 
             aes(x = longitude, y = latitude, color = phonetic_characteristic),
             size = 3) +
  geom_text(data = datos_benrath,
            aes(x = longitude, y = latitude, label = region),
            vjust = -1, size = 3) +
  scale_color_manual(values = c("maken" = "blue", "machen" = "green", "mache" = "green")) +
  theme_minimal() +
  labs(title = "Características fonéticas en la Línea Benrath",
       color = "Característica fonética") +
  theme(legend.position = "bottom")
```
