# Author: Saeesh Mangwani
# Date: 2026-10-12

# Description: Builds the `card_descriptions` table of plot/card titles and
# descriptions shown in the dashboard modals. Run from the project root.
#
# Inputs: data-raw/app_metadata/card_descriptions.yaml
# Outputs: data/card_descriptions.rda

# ==== Description data ====

# Authored in YAML (one record per list item, keyed by idx + category; desc_*
# fields hold Markdown). read_yaml() returns an unnamed list of records;
# rbindlist() stacks them into the data.table (columns: idx, category,
# title_en, desc_en, title_es, desc_es) that get_desc() consumes.
card_descriptions <- data.table::rbindlist(
  yaml::read_yaml('data-raw/app_metadata/card_descriptions.yaml')
)

# ==== Saving ====
usethis::use_data(card_descriptions, overwrite = TRUE)
