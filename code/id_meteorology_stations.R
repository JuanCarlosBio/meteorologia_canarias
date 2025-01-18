#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(gt)

metadata_estaciones <- read_csv(
    "data/raw/estaciones.csv", 
    locale = locale(encoding = "ISO-8859-1")
  ) %>%
  select(-date_from, -date_to, -location_coordinates) %>%
  mutate(
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
    thing_id = str_pad(as.character(thing_id), 
                       width = 2, pad = "0"),
    location_id = str_pad(as.character(location_id), 
                          width = 2, pad = "0"),
  ) %>% 
  gt() %>%
  cols_align(
    align = "center"
  ) %>% 
  cols_label(
    thing_id = md("<strong>ID ESTACIÓN</strong>"),
    thing_name = md("<strong>CÓDIGO ESTACIÓN</strong>"),
    location_id = md("<strong>ID LOCALIZACIÓN</strong>"),
    isla = md("<strong>ISLA</strong>"),
    location_description = md("<strong>NOPMBRE CENTRAL</strong>")
  ) %>% tab_header(
    title = md("ÍNDICE DE LAS ESTACIONES METEOROLÓGICAS DE CANARIAS"),
    subtitle = md(
      glue('Última actualización {format(lubridate::today(), "%d/%m/%Y")}')
    )
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
