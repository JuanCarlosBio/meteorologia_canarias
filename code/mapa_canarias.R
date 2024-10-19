#!/usr/bin/env Rscript

library(tidyverse)
library(sf)
library(plotly)

lista_estaciones <- list.files("data/processed/")
metadata_estaciones <- read_csv("data/raw/estaciones.csv", locale = locale(encoding = "ISO-8859-1"))
datos_estaciones <- read_csv(glue("data/processed/{lista_estaciones}"))
municipios_canarias <- read_sf("data/islands_shp/municipios.shp") %>%
  mutate(geometry = st_transform(geometry, crs = 4326))

datos_espaciales <- left_join(metadata_estaciones, datos_estaciones, by = "thing_id") %>%
  select(thing_id, location_description, location_coordinates, datastream_name, year, month, result)

sf_precipitaciones <- datos_espaciales %>% 
  filter(str_detect(datastream_name, "Rain") & year == year(today()) & month == month %>% tail(n=1)) %>%
  group_by(
    location_description, 
    datastream_name, 
    year, 
    month, 
    location_coordinates
    ) %>%
  summarise(sum_precipitation = sum(result, na.rm = TRUE)) %>%
  mutate(location_coordinates = st_as_sfc(location_coordinates, crs = 4326)) %>%
  st_as_sf()

plot_precipitaciones <- sf_precipitaciones %>%
  ggplot() +
  geom_sf(data = municipios_canarias) +
  geom_sf()

ggplotly(plot_precipitaciones)  
