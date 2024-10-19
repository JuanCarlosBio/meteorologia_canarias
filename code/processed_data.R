library(tidyverse)
library(glue)
library(lubridate)

archivo <- commandArgs(trailingOnly = TRUE)

estacion_datos <- read_csv(glue("data/raw/{archivo}"))

# Datos resumidos para la temperatura 
# ----------------------------------------------------------------------------
estacion_datos %>% 
  mutate(year = year(result_time),
         month = month(result_time)) %>%
  filter(observation_validity == "valid" & 
           datastream_name %in% c(
             "Air temperature (avg.)",
             "Air temperature (max.)",
             "Air temperature (min.)"
           )) %>%
  group_by(datastream_name, unit_of_measurement, thing_id, year, month) %>% 
  summarise(
    result = mean(result) 
  ) %>% 
  write_csv(glue("data/processed/{archivo}.temperatura.csv"))

# Datos resumidos para la precipitación acumulada  
# ----------------------------------------------------------------------------
estacion_datos %>% 
  mutate(year = year(result_time),
         month = month(result_time)) %>%
  filter(observation_validity == "valid" & 
           datastream_name %in% c("Rain (daily accumulated)")) %>%
  group_by(datastream_name, unit_of_measurement, thing_id, year, month) %>% 
  summarise(
    result = sum(result) 
  ) %>% 
  write_csv(glue("data/processed/{archivo}.rain_acumulated.csv"))
 
