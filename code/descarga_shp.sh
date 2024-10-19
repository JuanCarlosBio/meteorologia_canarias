#!/usr/bin/env bash

url_muni=https://opendata.sitcan.es/upload/unidades-administrativas/gobcan_unidades-administrativas_municipios.zip

wget -P data/raw/ $url_muni

mkdir -p data/islands_shp

unzip data/raw/gobcan_unidades-administrativas_municipios.zip -d data/islands_shp
