# Author: Saeesh Mangwani
# Date: 2026-10-12

# Description: Builds the `timeline_milestones` table of management milestones
# shown on the Datos generales page (dg_timeline()). Run from the project root.
#
# Inputs: data-raw/app_metadata/timeline_milestones.csv
# Outputs: data/timeline_milestones.rda

# ==== Timeline milestone data ====

# One row per milestone (columns: database, sort_order, period_en, period_es,
# label_en, label_es)
timeline_milestones <- data.table::fread('data-raw/app_metadata/timeline_milestones.csv')

# ==== Saving ====
usethis::use_data(timeline_milestones, overwrite = TRUE)
