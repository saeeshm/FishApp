# Libraries
library(data.table) # Necessary for data manipulation via data.table syntax

# Read in the cleaned datasets from Saeesh's data creation workflow.
load("./data/fishdbase.rda")

# Add in additional column that lists fishing technique in English.

## fishdbase
unique(fishdbase$tipo_arte)

fishdbase[, fishing_technique := data.table::fcase(
  fishdbase$tipo_arte == 'Chinchorro', 'Chinchorro',
  fishdbase$tipo_arte == 'Linea de Mano', 'Handline',
  fishdbase$tipo_arte == 'Trasmallo', 'Trammel Net',
  fishdbase$tipo_arte == 'Arpon', 'Harpoon',
  fishdbase$tipo_arte == 'Nasa', 'Fish Trap',
  is.na(fishdbase$tipo_arte), 'NA'
)]

## lfqdbase
unique(lfqdbase$tipo_arte)

lfqdbase[, fishing_technique := data.table::fcase(
  lfqdbase$tipo_arte == 'Chinchorro', 'Chinchorro',
  lfqdbase$tipo_arte == 'Linea de Mano', 'Handline',
  lfqdbase$tipo_arte == 'Trasmallo - Desconocido', 'Trammel Net - Unknown',
  lfqdbase$tipo_arte == 'Trasmallo - 2\"', 'Trammel Net - 2\"',
  lfqdbase$tipo_arte == 'Trasmallo - 3\"', 'Trammel Net - 3\"'
)]

# Temporarily, save these mildly modified versions of fishdbase and lfqdbase as their
# own .rds files.
saveRDS(fishdbase, file = "./data/fishdbase.rds")
saveRDS(lfqdbase, file = "./data/lfqdbase.rds")
