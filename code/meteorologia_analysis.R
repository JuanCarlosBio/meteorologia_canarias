#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(ggtext)

lista_estaciones <- list.files("data/processed/")
datos_estaciones <- read_csv(glue("data/processed/{lista_estaciones}"))
anio_actual <- year(today())

# GRÁFICO PARA LAS PRECIPITACIONES
# -------------------------------------------------------------
precipitaciones_plot <- datos_estaciones %>%
  filter(datastream_name == "Rain (daily accumulated)") %>%
  group_by(year, month) %>%
  summarise(precipitacion_acumulada = sum(result)) %>%
  mutate(es_anio_actual = as.character(year == anio_actual),
         precipitacion_acumulada = precipitacion_acumulada / 10^3) %>%
  ungroup() %>%
  ggplot(aes(month, precipitacion_acumulada, 
             color = es_anio_actual, 
             group = year)) +
  geom_line(linewidth = 1, show.legend = F) +
  geom_smooth(aes(group = 1), se = F, show.legend = F) +
  scale_x_continuous(limits = c(1,12),
                     breaks = seq(1,12,1),
                     labels = c(
                       "Enero", "Febrero", "Marzo",
                       "Abril", "Mayo", "Junio",
                       "Julio", "Agosto", "Septiembre",
                       "Octubre", "Noviembre", "Dicembre")) +
  scale_color_manual(values = c("gray", "red")) +
  labs(
    title = glue("Precipitación acumulada para el <span style='color: red'><i>{anio_actual}</i></span> frente al <span style='color: gray'><i>resto de años</i></span> y su <span style='color: blue'><i>promedio</i></span>"),
    subtitle = "Datos públicos de GRAPHCAN",
    x = "Meses del año",
    y = "Precipitación acumulada (mm) x 10<sup>3</sup>"
  ) +
  theme_classic() +
  theme(
    plot.title = element_markdown(face = "bold"),
    axis.title = element_markdown(face = "bold")
  )

# GRÁFICO PARA LA TEMPERATURA PROMEDIO
# -------------------------------------------------------------
temperatura_plot <- datos_estaciones %>%
  filter(datastream_name == c("Air temperature (avg.)")) %>%
  group_by(year, month) %>%
  summarise(precipitacion_acumulada = mean(result)) %>%
  mutate(es_anio_actual = as.character(year == anio_actual),
         precipitacion_acumulada = precipitacion_acumulada / 10^3) %>%
  ungroup() %>%
  ggplot(aes(month, precipitacion_acumulada, 
             color = es_anio_actual, 
             group = year)) +
  geom_line(linewidth = 1, show.legend = F) +
  geom_smooth(aes(group = 1), se = F, show.legend = F) +
  scale_x_continuous(limits = c(1,12),
                     breaks = seq(1,12,1),
                     labels = c(
                       "Enero", "Febrero", "Marzo",
                       "Abril", "Mayo", "Junio",
                       "Julio", "Agosto", "Septiembre",
                       "Octubre", "Noviembre", "Dicembre")) +
  scale_color_manual(values = c("gray", "red")) +
  labs(
    title = glue("Temperatura del aire promedio para el <span style='color: red'><i>{anio_actual}</i></span> frente al <span style='color: gray'><i>resto de años</i></span> y su <span style='color: blue'><i>promedio general</i></span>"),
    subtitle = "Datos públicos de GRAPHCAN",
    x = "Meses del año",
    y = "Temperatura (ºC)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_markdown(face = "bold"),
    axis.title = element_markdown(face = "bold")
  )
