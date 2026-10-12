# Author: Saeesh Mangwani
# Date: 2026-10-12

# Description: Builds the `gear_names` lookup table, used by translate_gear()
# to translate the standardized (Spanish) gear names in the fish databases to
# display names in the selected language, with a consistent order and colour
# per gear. Run from the project root.
#
# Inputs: data-raw/app_metadata/gear_names.csv
# Outputs: data/gear_names.rda

# ==== Gear name lookup data ====

# One row per standardized gear (columns: tipo_arte, sort_order, name_es,
# name_en, colour). tipo_arte must exactly match the standardized names
# produced in data-raw/fish_databases.R. Inch marks (") are written unquoted
# in the CSV, and quote handling is disabled (quote = '') so they are read
# literally. This means no field in the CSV may contain a comma.
gear_names <- data.table::fread(
  'data-raw/app_metadata/gear_names.csv',
  quote = '',
  encoding = 'UTF-8'
)

# ==== Saving ====
usethis::use_data(gear_names, overwrite = TRUE)
