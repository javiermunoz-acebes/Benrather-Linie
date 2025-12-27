# Benrather Linie (Benrath Line)

A linguistic dataset documenting the Benrath Line (*Benrather Linie*), one of the most important isoglosses in the German language that separates Low German (Niederdeutsch) from Central German (Mitteldeutsch) dialects.

## Overview

The Benrath Line is a linguistic boundary running east-west across Germany, approximately along the line of Benrath (a district of Düsseldorf). It represents a major division in German dialects based on the Second Germanic consonant shift, particularly the pronunciation of words like "make" (*maken* in Low German vs. *machen* in Central German).

This repository contains geographic and linguistic data for various German cities, documenting which side of the Benrath Line they fall on and their characteristic phonetic features.

## Data

The main dataset is contained in `linea_benrath_data.csv`, a semicolon-separated CSV file with the following fields:

- **region**: City or region name
- **longitude**: Geographic longitude
- **latitude**: Geographic latitude  
- **dialect_group**: Dialect classification (Niederdeutsch or Mitteldeutsch)
- **phonetic_characteristic**: How the word "make" is pronounced (*maken* or *machen*)
- **linguistic_feature**: Type of consonant (Consonante oclusiva or Consonante fricativa)

### Data Format

```csv
region;longitude;latitude;dialect_group;phonetic_characteristic;linguistic_feature
Düsseldorf;6.7735;51.2277;Niederdeutsch;maken;Consonante oclusiva
Köln;6.9603;50.9375;Mitteldeutsch;machen;Consonante fricativa
```

The dataset includes 39 German cities spanning both sides of the Benrath Line.

## Usage

You can use this data for:

- Linguistic analysis and visualization
- Educational purposes
- Mapping German dialect boundaries
- Research on German dialectology

### Example: Loading the Data in R

```r
# Load the data
datos_benrath <- read.csv("linea_benrath_data.csv", sep = ";", encoding = "UTF-8")

# View structure
str(datos_benrath)

# Or load directly from GitHub
url <- "https://raw.githubusercontent.com/javiermunoz-acebes/Benrather-Linie/main/linea_benrath_data.csv"
datos_benrath <- read.csv(url, sep = ";", encoding = "UTF-8")
```

### Example: Loading the Data in Python

```python
import pandas as pd

# Load from local file
df = pd.read_csv('linea_benrath_data.csv', sep=';', encoding='utf-8')

# Or load directly from GitHub
url = "https://raw.githubusercontent.com/javiermunoz-acebes/Benrather-Linie/main/linea_benrath_data.csv"
df = pd.read_csv(url, sep=';', encoding='utf-8')

print(df.head())
```

### Visualization Example

Here's a simple example to visualize the data using R and ggplot2:

```r
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)  # Required for ne_countries()

# Load data
datos_benrath <- read.csv("linea_benrath_data.csv", sep = ";", encoding = "UTF-8")

# Get Germany map
alemania <- ne_countries(scale = "medium", returnclass = "sf", country = "germany")

# Create map
ggplot() +
  geom_sf(data = alemania) +
  geom_point(data = datos_benrath, 
             aes(x = longitude, y = latitude, color = dialect_group), 
             size = 3) +
  geom_text(data = datos_benrath, 
            aes(x = longitude, y = latitude, label = region),
            vjust = -1, size = 2.5) +
  theme_minimal() +
  labs(title = "Benrather Linie - Dialect Distribution",
       subtitle = "Low German (Niederdeutsch) vs Central German (Mitteldeutsch)",
       color = "Dialect Group",
       x = "Longitude",
       y = "Latitude")
```

## About the Benrath Line

The Benrath Line (German: *Benrather Linie*) is named after Benrath, a district in the southern part of Düsseldorf. It marks the northern boundary of the High German consonant shift (*Zweite Lautverschiebung*), which is one of the defining features of High German dialects.

Key linguistic features:
- **North of the line (Low German)**: "maken" (make), "Dorp" (village), "ik" (I)
- **South of the line (Central German)**: "machen" (make), "Dorf" (village), "ich" (I)

The line runs approximately through: Düsseldorf-Benrath, Kassel, Magdeburg, and Frankfurt an der Oder.

## License

This project is licensed under the terms specified in the LICENSE file.

## Contributing

Contributions to improve or expand the dataset are welcome. Please ensure data accuracy when adding new locations.

---

## Español

### Descripción

Este repositorio contiene datos geográficos y lingüísticos que documentan la Línea Benrath (*Benrather Linie*), una de las isoglosas más importantes del idioma alemán que separa los dialectos del bajo alemán (Niederdeutsch) de los dialectos del alemán central (Mitteldeutsch).

### Datos

El archivo `linea_benrath_data.csv` contiene información de 39 ciudades alemanas, documentando su ubicación geográfica, clasificación dialectal y características fonéticas específicas relacionadas con la Segunda mutación consonántica germánica.
