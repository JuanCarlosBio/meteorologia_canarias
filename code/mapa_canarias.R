#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmltools)

lista_estaciones <- list.files("data/processed/")
metadata_estaciones <- read_csv("data/raw/estaciones.csv", locale = locale(encoding = "ISO-8859-1"))
datos_estaciones <- read_csv(glue("data/processed/{lista_estaciones}"))
# anio_actual <- year(today()) #Obviamente, no hay datos todavía para 2025 :/ 
anio_actual <- 2024
municipios_canarias <- read_sf("data/islands_shp/municipios.shp") %>%
  mutate(geometry = st_transform(geometry, crs = 4326))


datos_espaciales <- left_join(metadata_estaciones, datos_estaciones, by = "thing_id") %>%
  select(thing_id, location_description, location_coordinates, datastream_name, year, month, result) %>%
  mutate(month = factor(month,
                        levels = unique(sort(month)),
                        labels = c("enero", "febrero", "marzo",
                                   "abril", "mayo", "junio",
                                   "julio", "agosto", "septiempre",
                                   "octubre", "noviembre", "diciembre")))

sf_precipitaciones <- datos_espaciales %>% 
  filter(str_detect(datastream_name, "Rain")) %>%
  group_by(
    location_description, 
    datastream_name, 
    year, 
    month, 
    location_coordinates
    ) %>%
  summarise(sum_precipitation = round(sum(result, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  group_by(
    location_description, 
    datastream_name, 
    month, 
    location_coordinates
    ) %>% 
  mutate(mean_rain = mean(sum_precipitation),
         variation_rain = round((sum_precipitation - mean_rain),2),
         location_coordinates = st_as_sfc(location_coordinates, crs = 4326)) %>%
  ungroup() %>%
  filter(year == anio_actual) %>%
  arrange(
    year, 
    month, 
    ) %>%
  filter(month == month %>% tail(n=1)) %>% 
  st_as_sf() %>%
  mutate(
    lon = st_coordinates(location_coordinates)[, 1], # Longitude (X)
    lat = st_coordinates(location_coordinates)[, 2]  # Latitude (Y)
  )

sf_avg_temperature <- datos_espaciales %>% 
  filter(str_detect(datastream_name, "Air temperature") ) %>% 
  group_by(
    location_description, 
    datastream_name, 
    year, 
    month, 
    location_coordinates
  ) %>% 
  summarise(avg_temperature = round(mean(result, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  mutate(datastream_name = tolower(str_replace_all(datastream_name, pattern = " ", replacement = "_")),
         datastream_name = tolower(str_replace_all(datastream_name, pattern = "[\\(\\)\\.]", replacement = ""))) %>%  
  ungroup() %>%
  group_by(
    location_description, 
    datastream_name, 
    month, 
    location_coordinates
  ) %>%   
  mutate(
    mean_temp_years = mean(avg_temperature),
    variation_temp_years = round((avg_temperature - mean_temp_years),2),
    location_coordinates = st_as_sfc(location_coordinates, crs = 4326)) %>%
  select(-mean_temp_years) %>%
  pivot_wider(names_from = "datastream_name", values_from = c("avg_temperature", "variation_temp_years")) %>% 
  ungroup() %>% 
  arrange(
    year, 
    month, 
    ) %>%
  filter(year == anio_actual & month == month %>% tail(n=1)) %>%   
  st_as_sf()  %>%
  mutate(
    lon = st_coordinates(location_coordinates)[, 1], # Longitude (X)
    lat = st_coordinates(location_coordinates)[, 2]  # Latitude (Y)
  )

pal_prep <- colorNumeric(
  palette = "Blues",
  domain = sf_precipitaciones$sum_precipitation
)

pal_varprep <- colorNumeric(
  palette = "RdYlGn",
  domain = sf_precipitaciones$variation_rain
)

bins <- c(0, 10, 20, 30, Inf)
pal_temp <- colorBin("YlOrRd", domain = sf_avg_temperature$avg_temperature_air_temperature_avg, bins = bins)

pal_var_avgtemp <- colorNumeric(
  palette = c("blue", "white", "red"),
  domain = sf_avg_temperature$variation_temp_years_air_temperature_avg
)

## Resumir nombre de las variables:
## Precipitaciones
sum_precip <- sf_precipitaciones$sum_precipitation
var_precip <- sf_precipitaciones$variation_rain 
## Temperatura 
temp_air_min <- sf_avg_temperature$avg_temperature_air_temperature_min
temp_air_avg <- sf_avg_temperature$avg_temperature_air_temperature_avg
temp_air_max <- sf_avg_temperature$avg_temperature_air_temperature_max

var_air_min <- sf_avg_temperature$variation_temp_years_air_temperature_min
var_air_avg <- sf_avg_temperature$variation_temp_years_air_temperature_avg
var_air_max <- sf_avg_temperature$variation_temp_years_air_temperature_max

map <- leaflet() %>%
  setView(-15.8, 28.4, zoom = 8)  %>% 
  addProviderTiles(
    providers$Esri.WorldImagery,
    options = providerTileOptions(
      minZoom = 8
    ) 
  ) %>%
  addCircleMarkers(
    data = sf_precipitaciones, 
    fillColor = ~pal_prep(sum_precipitation),
    weight = .3,
    fillOpacity = 1,
    radius = 10,
    popup = paste0(
      "<p align='left'>",
      glue("<strong>Central</strong>: <i>{sf_precipitaciones$location_description}</i><br>"),
      "----<br>",
      glue("Datos del mes de <strong>{sf_precipitaciones$month}</strong> del año <strong>{sf_precipitaciones$year} y su variación con respecto a los años</strong>:<br>"),
      glue("<strong>Precipitación acumulada</strong>: <u>{sum_precip} mm</u> (<span style='color:{ifelse(var_precip >= 0, 'green', 'red')}'>{ifelse(var_precip >= 0, paste0('+', var_precip), var_precip)}</span> mm)"),
      "</p>" 
    ) %>% lapply(htmltools::HTML),
    label = paste0(
      "<p align='left'>",
      "<strong>Nombre de la central:</strong><br>",
      glue("<i><u>{sf_precipitaciones$location_description}</u></i>"),
      "</>"
    ) %>% lapply(htmltools::HTML),
    group = "Precipitaciones" 
  ) %>%
  addCircleMarkers(
    data = sf_precipitaciones, 
    fillColor = ~pal_varprep(variation_rain),
    weight = .3,
    fillOpacity = 1,
    radius = 10,
    popup = paste0(
      "<p align='left'>",
      glue("<strong>Central</strong>: <i>{sf_precipitaciones$location_description}</i><br>"),
      "----<br>",
      glue("Datos del mes de <strong>{sf_precipitaciones$month}</strong> del año <strong>{sf_precipitaciones$year} y su variación con respecto a los años</strong>:<br>"),
      glue("<strong>Precipitación acumulada</strong>: <u>{sum_precip} mm</u> (<span style='color:{ifelse(var_precip >= 0, 'green', 'red')}'>{ifelse(var_precip >= 0, paste0('+', var_precip), var_precip)}</span> mm)"),
      "</p>" 
    ) %>% lapply(htmltools::HTML),
    label = paste0(
      "<p align='left'>",
      "<strong>Nombre de la central:</strong><br>",
      glue("<i><u>{sf_precipitaciones$location_description}</u></i>"),
      "</>"
    ) %>% lapply(htmltools::HTML),
    group = "Var. precipitaciones" 
  ) %>%
  addCircleMarkers(
    data = sf_avg_temperature, 
    fillColor = ~pal_temp(avg_temperature_air_temperature_avg),
    weight = .3,
    fillOpacity = 1,
    radius = 10,
    popup = paste0(
      "<p align='left'>",
      glue("<strong>Central</strong>: <i>{sf_avg_temperature$location_description}</i><br>"),
      "----<br>",
      glue("Datos del mes de <strong>{sf_avg_temperature$month}</strong> del año <strong>{sf_avg_temperature$year} y su variación promedio (respecto a los años)</strong>:<br>"),
      glue("<strong>Temperatura mínima</strong>:   <u>{temp_air_min} ºC</u> (<span style='color:{ifelse(var_air_min >= 0, 'red', 'blue')}'>{ifelse(var_air_min >= 0, paste0('+', var_air_min), var_air_min)} ºC</span>)<br>"),
      glue("<strong>Temperatura promedio</strong>: <u>{temp_air_avg} ºC</u> (<span style='color:{ifelse(var_air_avg >= 0, 'red', 'blue')}'>{ifelse(var_air_avg >= 0, paste0('+', var_air_avg), var_air_avg)} ºC</span>)<br>"),
      glue("<strong>Temperatura máxima</strong>:   <u>{temp_air_max} ºC</u> (<span style='color:{ifelse(var_air_max >= 0, 'red', 'blue')}'>{ifelse(var_air_max >= 0, paste0('+', var_air_max), var_air_max)} ºC</span>)"),
      "</p>" 
    ) %>% lapply(htmltools::HTML),
    label = paste0(
      "<p align='left'>",
      "<strong>Nombre de la central:</strong><br>",
      glue("<i><u>{sf_avg_temperature$location_description}</u></i>"),
      "</>"
    ) %>% lapply(htmltools::HTML),
    group = "Temperatura" 
  ) %>%
  addCircleMarkers(
    data = sf_avg_temperature, 
    fillColor = ~pal_var_avgtemp(variation_temp_years_air_temperature_avg),
    weight = .3,
    fillOpacity = 1,
    radius = 10,
    popup = paste0(
      "<p align='left'>",
      glue("<strong>Central</strong>: <i>{sf_avg_temperature$location_description}</i><br>"),
      "----<br>",
      glue("Datos del mes de <strong>{sf_avg_temperature$month}</strong> del año <strong>{sf_avg_temperature$year} y su variación promedio (respecto a los años)</strong>:<br>"),
      glue("<strong>Temperatura mínima</strong>:   <u>{temp_air_min} ºC</u> (<span style='color:{ifelse(var_air_min >= 0, 'red', 'blue')}'>{ifelse(var_air_min >= 0, paste0('+', var_air_min), var_air_min)} ºC</span>)<br>"),
      glue("<strong>Temperatura promedio</strong>: <u>{temp_air_avg} ºC</u> (<span style='color:{ifelse(var_air_avg >= 0, 'red', 'blue')}'>{ifelse(var_air_avg >= 0, paste0('+', var_air_avg), var_air_avg)} ºC</span>)<br>"),
      glue("<strong>Temperatura máxima</strong>:   <u>{temp_air_max} ºC</u> (<span style='color:{ifelse(var_air_max >= 0, 'red', 'blue')}'>{ifelse(var_air_max >= 0, paste0('+', var_air_max), var_air_max)} ºC</span>)"),
      "</p>" 
    ) %>% lapply(htmltools::HTML),
    label = paste0(
      "<p align='left'>",
      "<strong>Nombre de la central:</strong><br>",
      glue("<i><u>{sf_avg_temperature$location_description}</u></i>"),
      "</>"
    ) %>% lapply(htmltools::HTML),
    group = "Var. Temperatura" 
  ) %>%
  addLegend(
    data = sf_precipitaciones,
    pal = pal_prep,
    values = ~sum_precipitation,
    title = paste0(
      "<p align='left'>",
      "Leyenda<br>Precip. Acumulada",
      "</p>"
      ) %>% lapply(htmltools::HTML),
    opacity = 1,
    group = "Leyenda precipitaciones",
    "bottomright"
  ) %>%
  addLegend(
    data = sf_avg_temperature,
    pal = pal_temp,
    values = ~avg_temperature_air_temperature_avg,
    title = paste0(
      "<p align='left'>",
      "Leyenda<br>Temp. Promedio",
      "</p>"
      ) %>% lapply(htmltools::HTML),
    opacity = 1,
    group = "Leyenda Temperatura",
    "bottomright"
  )%>%
  addLayersControl(
    baseGroups = c("Precipitaciones", "Temperatura", "Var. precipitaciones", "Var. Temperatura"),
    overlayGroups = c("Leyenda precipitaciones", "Leyenda Temperatura"),
    options = layersControlOptions(collapsed = T, autoZIndex = TRUE)
  ) %>%
  addResetMapButton() %>% 
  addScaleBar("bottomleft", scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
  htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      });

      var css = '.info.legend.leaflet-control { text-align: left; }';
      var head = document.head || document.getElementsByTagName('head')[0];
      var style = document.createElement('style');
      style.type = 'text/css';
      if (style.styleSheet) {
        style.styleSheet.cssText = css;
      } else {
        style.appendChild(document.createTextNode(css));
      }
      head.appendChild(style);
    }
  ") 
