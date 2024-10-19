#!/usr/bin/env bash

# URL de los datos públicos del IDECacanarias (GRAPHCAN)
url_estaciones_metadata="https://opendata.sitcan.es/upload/meteorologia/estaciones.csv"
url_estaciones_datos="https://opendata.sitcan.es/upload/meteorologia/observaciones.zip" 

mkdir -p data/raw/ data/processed/

wget -P data/raw/ $url_estaciones_metadata -N -q  
wget -P data/raw/ $url_estaciones_datos -N -q  
