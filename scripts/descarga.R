pacman::p_load("tidyverse", "magrittr", "wid")

### Descarga de datos

# Especifica la ruta de la carpeta donde se encuentran los archivos .csv
ruta_carpeta <- "data/wid_all_data"

# Obtener la lista de archivos .csv que empiezan con "WID_data"
archivos_csv <- list.files(path = ruta_carpeta, pattern = "^WID_data.*\\.csv$", full.names = TRUE)

# Leer todos los archivos .csv y combinarlos en un solo dataframe
datos_combinados <- lapply(archivos_csv, read.csv) %>%
  bind_rows()

