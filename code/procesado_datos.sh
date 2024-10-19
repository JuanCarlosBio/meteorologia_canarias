#!/usr/bin/env bash

files=$(zipinfo data/raw/observaciones.zip | \
	grep csv$ | \
	cut -d " " -f 13)

for file in $files;do

	unzip data/raw/observaciones.zip $file -d data/raw/

	Rscript code/processed_data.R $file

	rm data/raw/${file}

done
