#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(gt)
library(htmltools)

lista_estaciones <- list.files("data/processed/")
anio_actual <- year(today()) #Obviamente, no hay datos todavía para 2025 :/ 
# anio_actual <- 2024
datos_estaciones <- read_csv(glue("data/processed/{lista_estaciones}")) %>%
  mutate(
    es_anio_actual = year == anio_actual,
    month = factor(
      month,
      levels = c(1:12),
      labels = c("enero", "febrero", "marzo", 
                 "abril", "mayo", "junio", 
                 "julio", "agosto", "septiembre", 
                 "octubre", "noviembre", "diciembre")),
      isla = case_when(
        thing_id %in% c(6, 8, 11, 23, 31, 32) ~ "LANZAROTE",
        thing_id %in% c(12, 16, 17, 21, 48, 52, 53, 58) ~ "FUERTEVENTURA",
        thing_id %in% c(2, 7, 10, 14, 15, 19, 20, 25, 27, 41, 42, 43) ~ "GRAN CANARIA",
        thing_id %in% c(3, 5, 9, 18, 22, 28, 36, 37, 44, 45, 46, 47, 
                        49, 50, 51, 54, 56) ~ "TENERIFE",
        thing_id %in% c(4, 24, 35, 39, 57) ~ "LA PALMA",
        thing_id %in% c(29, 30, 38, 40, 55) ~ "LA GOMERA",
        thing_id %in% c(13, 26, 33, 34) ~ "EL HIERRO",
        TRUE ~ NA_character_
      ),
      thing_id = str_pad(as.character(thing_id), width = 2, pad = "0")
   )  

precipitaciones <- datos_estaciones %>%
  filter(str_detect(datastream_name, "Rain")) %>%
  group_by(
    datastream_name, 
    year, 
    month,
    isla,
    thing_id 
  ) %>%
  summarise(sum_precipitation = round(sum(result, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  group_by(
    datastream_name, 
    month,
    isla 
    ) %>% 
  mutate(mean_rain = mean(sum_precipitation),
         variation_rain = round((sum_precipitation - mean_rain),2)) %>%
  ungroup() %>%
#  filter(year == anio_actual) %>%
  select(-datastream_name, -mean_rain) %>%
  arrange(year, month, isla)

temperatura <- datos_estaciones %>%
  filter(str_detect(datastream_name, "Air temperature")) %>%
  group_by(
    datastream_name, 
    year, 
    month,
    isla,
    thing_id 
  ) %>%
  summarise(avg_temperature = round(mean(result, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  mutate(datastream_name = tolower(str_replace_all(datastream_name, pattern = " ", replacement = "_")),
         datastream_name = tolower(str_replace_all(datastream_name, pattern = "[\\(\\)\\.]", replacement = ""))) %>%  
  ungroup() %>%
  group_by(
    datastream_name, 
    month,
    isla
  ) %>%   
  mutate(
    mean_temp_years = mean(avg_temperature),
    variation_temp_years = round((avg_temperature - mean_temp_years),2),
  ) %>%
  select(-mean_temp_years) %>%
  pivot_wider(names_from = "datastream_name", 
              values_from = c("avg_temperature", "variation_temp_years")) %>% 
  ungroup() %>% 
#  filter(year == anio_actual) %>%
  arrange(year, month)

# Data para el 2024
df_join <- left_join(precipitaciones, temperatura, by=c("year", "month","isla", "thing_id")) 

gt_table <- df_join %>%
  mutate(
    month = str_to_title(month),
    variation_rain = paste0(glue("<span style='color: {ifelse(variation_rain >= 0, 'green', 'brown')}'>{ifelse(variation_rain >= 0, paste0('+', variation_rain), variation_rain)}</span>")) %>% lapply(HTML),
    variation_temp_years_air_temperature_min = paste0(glue("<span style='color: {ifelse(variation_temp_years_air_temperature_min >= 0, 'red', 'blue')}'>{ifelse(variation_temp_years_air_temperature_min >= 0, paste0('+', variation_temp_years_air_temperature_min), variation_temp_years_air_temperature_min)}</span>")) %>% lapply(HTML), 
    variation_temp_years_air_temperature_avg = paste0(glue("<span style='color: {ifelse(variation_temp_years_air_temperature_avg >= 0, 'red', 'blue')}'>{ifelse(variation_temp_years_air_temperature_avg >= 0, paste0('+', variation_temp_years_air_temperature_avg), variation_temp_years_air_temperature_avg)}</span>")) %>% lapply(HTML), 
    variation_temp_years_air_temperature_max = paste0(glue("<span style='color: {ifelse(variation_temp_years_air_temperature_max >= 0, 'red', 'blue')}'>{ifelse(variation_temp_years_air_temperature_max >= 0, paste0('+', variation_temp_years_air_temperature_max), variation_temp_years_air_temperature_max)}</span>")) %>% lapply(HTML) 
  ) %>%
  gt() %>%
    cols_move_to_start(
    columns = c(
      isla,
      thing_id,
      month,
      year,
      sum_precipitation, variation_rain, 
      avg_temperature_air_temperature_min, variation_temp_years_air_temperature_min,
      avg_temperature_air_temperature_avg, variation_temp_years_air_temperature_avg, 
      avg_temperature_air_temperature_max, variation_temp_years_air_temperature_max
    )
  ) %>%
  cols_label(
    isla                                     = md("<p align='center'><strong>Isla</strong></p>"),
    thing_id                                  = md("<p align='center'><strong>ID Central</strong></p>"),
    month                                    = md("<p align='center'><strong>Mes</strong></p>"),
    year                                    = md("<p align='center'><strong>Año</strong></p>"), 
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
  subtitle = md(paste('
  Variables de precipitaciones y temperaturas<br>',
  '**Recomendación**: Puedes filtrar el mes o la isla en el buscador "Search",',
  ' <u>por ejemplo: "gran canaria" filtra la isla de Gran Canaria</u>.',
  "También puedes filtrar en cada cuadro de búsqueda de cada una de las columnas."))
  ) %>%
  tab_footnote(
    footnote = md(paste0(
      "<strong>Leyenda de colores de la tabla:</strong>",
      "<br>",
      "* <u>Variación de la precipitación acumulada: </u>",
      "<span style='color: green'><i>&uarr; Incremento</i></span>", 
      " | ", 
      "<span style='color: brown'><i>&darr; Descenso</i></span>",
      "<br>",
      "* <u>Variación de la temperatura del aire: </u>", 
      "<span style='color: red'><i>&uarr; Incremento</i></span>", 
      " | ", 
      "<span style='color: blue'><i>&darr; Descenso</i></span>"
    ))
  ) %>%
  opt_interactive(
    use_search = TRUE,
    use_filters = TRUE,
    use_resizers = TRUE,
    use_highlight = TRUE,
    use_compact_mode = TRUE,
    use_text_wrapping = FALSE,
    use_page_size_select = TRUE
  ) %>%
  tab_options(
    heading.border.bottom.color = "#0C2B3D",
    table.border.top.color="#0C2B3D", 
    table.border.bottom.color="#0C2B3D",
    column_labels.border.top.color = "#0C2B3D",
    column_labels.border.bottom.color = "#0C2B3D",
    table_body.border.bottom.color = "#0C2B3D",
    table.background.color = "#d6f0ff",
    summary_row.border.color = "#0C2B3D"
  )