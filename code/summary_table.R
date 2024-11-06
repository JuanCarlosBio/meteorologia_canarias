#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(gt)
library(htmltools)

lista_estaciones <- list.files("data/processed/")
anio_actual <- year(today())
datos_estaciones <- read_csv(glue("data/processed/{lista_estaciones}")) %>%
  mutate(
    es_anio_actual = year == anio_actual,
    month = factor(
      month,
      levels = c(1:12),
      labels = c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
    )  
  )

precipitaciones <- datos_estaciones %>%
  filter(str_detect(datastream_name, "Rain")) %>%
  group_by(
    datastream_name, 
    year, 
    month 
  ) %>%
  summarise(sum_precipitation = round(sum(result, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  group_by(
    datastream_name, 
    month 
    ) %>% 
  mutate(mean_rain = mean(sum_precipitation),
         variation_rain = round((sum_precipitation - mean_rain),2)) %>%
  ungroup() %>%
  filter(year == anio_actual) %>%
  select(-datastream_name, -mean_rain) %>%
  arrange(year, month)

temperatura <- datos_estaciones %>%
  filter(str_detect(datastream_name, "Air temperature")) %>%
  group_by(
    datastream_name, 
    year, 
    month 
  ) %>%
  summarise(avg_temperature = round(mean(result, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  mutate(datastream_name = tolower(str_replace_all(datastream_name, pattern = " ", replacement = "_")),
         datastream_name = tolower(str_replace_all(datastream_name, pattern = "[\\(\\)\\.]", replacement = ""))) %>%  
  ungroup() %>%
  group_by(
    datastream_name, 
    month 
  ) %>%   
  mutate(
    mean_temp_years = mean(avg_temperature),
    variation_temp_years = round((avg_temperature - mean_temp_years),2),
  ) %>%
  select(-mean_temp_years) %>%
  pivot_wider(names_from = "datastream_name", values_from = c("avg_temperature", "variation_temp_years")) %>% 
  ungroup() %>% 
  filter(year == anio_actual) %>%
  arrange(year, month)

# Data para el 2024
df_join <- left_join(precipitaciones, temperatura, by="month") %>%
  select(-starts_with("year"))

gt_table <- df_join %>%
  mutate(
    month = str_to_title(month),
    variation_rain = paste0(glue("<span style='color: {ifelse(variation_rain >= 0, 'blue', 'red')}'>{ifelse(variation_rain >= 0, paste0('+', variation_rain), variation_rain)}</span>")) %>% lapply(HTML),
    variation_temp_years_air_temperature_min = paste0(glue("<span style='color: {ifelse(variation_temp_years_air_temperature_min >= 0, 'red', 'green')}'>{ifelse(variation_temp_years_air_temperature_min >= 0, paste0('+', variation_temp_years_air_temperature_min), variation_temp_years_air_temperature_min)}</span>")) %>% lapply(HTML), 
    variation_temp_years_air_temperature_avg = paste0(glue("<span style='color: {ifelse(variation_temp_years_air_temperature_avg >= 0, 'red', 'green')}'>{ifelse(variation_temp_years_air_temperature_avg >= 0, paste0('+', variation_temp_years_air_temperature_avg), variation_temp_years_air_temperature_avg)}</span>")) %>% lapply(HTML), 
    variation_temp_years_air_temperature_max = paste0(glue("<span style='color: {ifelse(variation_temp_years_air_temperature_max >= 0, 'red', 'green')}'>{ifelse(variation_temp_years_air_temperature_max >= 0, paste0('+', variation_temp_years_air_temperature_max), variation_temp_years_air_temperature_max)}</span>")) %>% lapply(HTML) 
  ) %>%
  gt() %>%
    cols_move_to_start(
    columns = c(
      month,
      sum_precipitation, variation_rain, 
      avg_temperature_air_temperature_min, variation_temp_years_air_temperature_min,
      avg_temperature_air_temperature_avg, variation_temp_years_air_temperature_avg, 
      avg_temperature_air_temperature_max, variation_temp_years_air_temperature_max
    )
  ) %>%
  cols_label(
    month                                    = md("<p align='center'><strong>Mes</strong></p>"),
    sum_precipitation                        = md("<p align='center'><strong>Precipitación<br>Acumulada</strong></p>"),
    variation_rain                           = md("<p align='center'><strong>Variación de la<br>Precipitación</strong></p>"),
    avg_temperature_air_temperature_min      = md("<p align='center'><strong>Temperatura<br>Mínima</strong></p>"),
    avg_temperature_air_temperature_avg      = md("<p align='center'><strong>Temperatura<br>Promedio</strong></p>"),
    avg_temperature_air_temperature_max      = md("<p align='center'><strong>Temperatura<br>Máxima</strong></p>"),
    variation_temp_years_air_temperature_min = md("<p align='center'><strong>Variación de la<br>T. Mínima</strong></p>"),
    variation_temp_years_air_temperature_avg = md("<p align='center'><strong>Variación de la<br>T. Promedio</strong></p>"),
    variation_temp_years_air_temperature_max = md("<p align='center'><strong>Variación de la<br>T. Máxima</strong></p>")
  ) %>%
  tab_header(
  title = glue("TABLA DE METEOROLOGÍA DE LAS ISLAS CANARIAS PARA EL AÑO {anio_actual}"),
  subtitle = "Variables de precipitaciones y temperaturas"
  ) 
   
 