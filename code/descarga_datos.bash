#!/usr/bin/env bash

url1=https://opendata.sitcan.es/upload/unidades-administrativas/gobcan_unidades-administrativas_municipios.zip
url2=https://opendata.sitcan.es/upload/medio-ambiente/gobcan_medio-ambiente_zepa.zip

for url in $url1 $url2 $url3;do
    wget $url
done