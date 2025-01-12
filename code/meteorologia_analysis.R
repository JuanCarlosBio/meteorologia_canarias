#!/usr/bin/env Rscript

library(tidyverse)
library(glue)
library(ggtext)

lista_estaciones <- list.files("data/processed/")
datos_estaciones <- read_csv(glue("data/processed/{lista_estaciones}"))
anio_actual <- year(today()) #Obviamente, no hay datos todavía para 2025 :/
#anio_actual <- 2024

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
  geom_line(linewidth = 1, show.legend = F, aes(
    text = paste0(
      glue("Fecha: mes {month} del año {year}"), "\n", 
      glue("Precipitación acumulada: {round(precipitacion_acumulada * 10^3,2)} mm")
    ))) +
  geom_point(aes(text = paste0(
      glue("Fecha: mes {month} del año {year}"), "\n", 
      glue("Precipitación acumulada: {round(precipitacion_acumulada * 10^3,2)} mm")
    )), show.legend = FALSE) +
  geom_smooth(aes(group = 1), se = F, show.legend = F) +
  scale_x_continuous(limits = c(1,12),
                     breaks = seq(1,12,1),
                     labels = c(
                       "Ene.", "Feb.", "Mar.",
                       "Abr.", "May.", "Jun.",
                       "Jul.", "Ago.", "Sep.",
                       "Oct.", "Nov.", "Dic.")) +
  scale_color_manual(values = c("white", "red")) +
  labs(
    title = glue("Precipitación acumulada para el <span style='color: red'><i>{anio_actual}</i></span> frente al <span style='color: white'><i>resto de años</i></span> y su <span style='color: #5680FF'><i>tendencia</i></span>"),
    x = "Meses del año",
    y = "Precipitación acumulada (mm) x 10<sup>3</sup>"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray",
                                    linewidth = 0.5),
    plot.background = element_rect(fill = "#0C2B3D", color = "#0C2B3D"),
    panel.background = element_rect(fill = "#0C2B3D", color = "#0C2B3D"),
    panel.grid = element_line(color = "white", linetype = "dashed"),
    plot.title = element_markdown(face = "bold", size = 11, color = "lightgray"),
    axis.title = element_markdown(face = "bold", color = "white"),
    axis.text = element_text(color = "lightgray")
  )

# GRÁFICO PARA LA TEMPERATURA PROMEDIO
# -------------------------------------------------------------
temperatura_plot <- datos_estaciones %>%
  filter(datastream_name == c("Air temperature (avg.)")) %>%
  group_by(year, month) %>%
  summarise(temp_avg = mean(result)) %>%
  mutate(es_anio_actual = as.character(year == anio_actual)) %>%
  ungroup() %>%
  ggplot(aes(month, temp_avg, 
             color = es_anio_actual, 
             group = year)) +
  geom_line(linewidth = 1, show.legend = F, aes(
    text = paste0(
     glue("Fecha: mes {month} del año {year}"), "\n", 
     glue("Temperatura promedio: {round(temp_avg,2)} ºC")
    )
  )) +
  geom_point(aes(text = paste0(
     glue("Fecha: mes {month} del año {year}"), "\n", 
     glue("Temperatura promedio: {round(temp_avg,2)} ºC")
    )), show.legend = FALSE) +
  geom_smooth(aes(group = 1), se = F, show.legend = F) +
  scale_x_continuous(limits = c(1,12),
                     breaks = seq(1,12,1),
                     labels = c(
                       "Ene.", "Feb.", "Mar.",
                       "Abr.", "May.", "Jun.",
                       "Jul.", "Ago.", "Sep.",
                       "Oct.", "Nov.", "Dic.")) +
  scale_color_manual(values = c("white", "red")) +
  labs(
    title = glue("Temperatura del aire promedio para el <span style='color: red'><i>{anio_actual}</i></span> frente al <span style='color: white'><i>resto de años</i></span> y su <span style='color: #5680FF'><i>tendencia</i></span>"),
    x = "Meses del año",
    y = "Temperatura (ºC)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray",
                                    linewidth = 0.2),
    panel.grid.minor = element_line(color = "gray",
                                    linewidth = 0.2),
    plot.background = element_rect(fill = "#0C2B3D", color = "#0C2B3D"),
    panel.background = element_rect(fill = "#0C2B3D", color = "#0C2B3D"), 
    panel.grid = element_line(color = "white", linetype = "dashed"),
    plot.title = element_markdown(face = "bold", size = 11, color = "lightgray"),
    axis.title = element_markdown(face = "bold", color = "white"),
    axis.text = element_text(color = "lightgray")
  )

ggsave(
  "plots/portfolio_plot.png",
  plot = temperatura_plot,
  width = 7.75,
  height = 3.5
)