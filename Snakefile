
rule targets:
  input:
    "data/raw/estaciones.csv",
    "data/raw/observaciones.zip",
    "data/islands_shp/municipios.shp",
    "tasks/process_data_ok.done",
    "index.html"


rule download_data:
  input:
    bash_script = "code/descarga_datos.sh"
  output:
    "data/raw/estaciones.csv",
    "data/raw/observaciones.zip"
  conda:
    "code/environment/env.yml"
  shell:
    """
    bash {input.bash_script}
    """

rule download_shp:
  input:
    bash_script = "code/descarga_shp.sh"
  output:
    "data/islands_shp/municipios.shp"
  conda:
    "code/environment/env.yml"
  shell:
    """
    bash {input.bash_script}
    """

rule processed_data:
  input:
    bash_script = "code/procesado_datos.sh",
    r_script    = "code/processed_data.R",
    zip_file    = "data/raw/observaciones.zip",
    csv_file    = "data/raw/estaciones.csv"
  output:
    touch("tasks/process_data_ok.done")
  conda:
    "code/environment/env.yml"
  shell:
    """
    bash {input.bash_script}
    """

rule create_dashboard:
  input:
    rmd_index             = "index.Rmd",
    map_r_script          = "code/mapa_canarias.R",
    plots_r_script        = "code/meteorologia_analysis.R",
    table_r_script        = "code/summary_table.R",
    table_record_r_script = "code/registro_table.R",
    shp                   = "data/islands_shp/municipios.shp",
    csv_file              = "data/raw/estaciones.csv",
    processed_done        = "tasks/process_data_ok.done"
  output:
    "index.html"
  conda:
    "code/environment/env.yml"
  shell:
    """
    R -e "library(rmarkdown) ; render('{input.rmd_index}')"
    """