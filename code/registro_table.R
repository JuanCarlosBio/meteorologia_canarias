#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(gt)
library(htmltools)

lista_estaciones <- list.files("data/processed/")

meses_labels <- c("enero", "febrero", "marzo", 
                  "abril", "mayo", "junio", 
                  "julio", "agosto", "septiembre", 
                  "octubre", "noviembre", "diciembre")

datos_estaciones <- read_csv(glue("data/processed/{lista_estaciones}")) %>% 
    mutate(
        month = factor(
          month,
          levels = c(1:12),
          labels = meses_labels),
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
        month,
        isla,
        thing_id,
        year
    ) %>%
    summarise(sum_precipitation = round(sum(result, na.rm = TRUE), 2)) %>%
    ungroup() %>%
    filter(year != year(today())) %>%
    arrange(month, year, thing_id, isla)

temperatura <- datos_estaciones %>%
  filter(str_detect(datastream_name, "Air temperature")) %>%
  group_by(
    datastream_name, 
    month,
    isla,
    thing_id,
    year
  ) %>%
  summarise(avg_temperature = round(mean(result, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  mutate(datastream_name = tolower(str_replace_all(datastream_name, pattern = " ", replacement = "_")),
         datastream_name = tolower(str_replace_all(datastream_name, pattern = "[\\(\\)\\.]", replacement = ""))) %>% 
  pivot_wider(names_from = "datastream_name", 
              values_from = "avg_temperature") %>%
  filter(year != year(today())) %>%
  arrange(month)


df_join <- left_join(precipitaciones, 
                     temperatura, 
                     by=c("month", "year","isla", "thing_id")) %>%
    filter(month %in% meses_labels[1:month(today())]) %>%
    mutate(month = str_to_title(month)) %>%
    select(-datastream_name)

record_table <- df_join %>%
    gt() %>%
    cols_move_to_start(
        columns = c(
          isla,
          thing_id,
          month,
          year,
          sum_precipitation,
          air_temperature_min,
          air_temperature_avg,
          air_temperature_max,
        )
    )  %>%
    cols_label(
        isla                 = md("<p align='center'><strong>Isla</strong></p>"),
        thing_id             = md("<p align='center'><strong>ID Central</strong></p>"),
        month                = md("<p align='center'><strong>Mes</strong></p>"),
        year                 = md("<p align='center'><strong>Año</strong></p>"),
        sum_precipitation    = md("<p align='center'><strong>Precipitación<br>Acumulada</strong></p>"),
        air_temperature_min  = md("<p align='center'><strong>Temperatura<br>Mínima</strong></p>"),
        air_temperature_avg  = md("<p align='center'><strong>Temperatura<br>Promedio</strong></p>"),
        air_temperature_max  = md("<p align='center'><strong>Temperatura<br>Máxima</strong></p>")
    ) %>% tab_header(
        title = md("TABLA CON EL REGISTRO DE METEOROLOGÍA DE LAS ISLAS CANARIAS PARA LOS ANTERIORES AÑOS"),
        subtitle = md(paste(
            glue('**<u>Variables de precipitaciones y temperaturas</u>** (última actualización {format(lubridate::today(), "%d/%m/%Y")})<br>'),
            '**Recomendación**: Puedes filtrar el mes o la isla en el buscador "Search",',
            ' <i>por ejemplo: "gran canaria" filtra la isla de Gran Canaria</i>.',
            "También puedes filtrar en cada cuadro de búsqueda de cada una de las columnas."
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