# Author: Saeesh Mangwani
# Date: 2024-08-14

# Description: Draft synthesis of code for datapreparation

# ==== Map data ====
# hpath <- '../fisheries-analysis/data/tela-site-data'
# comunidades <- st_read(file.path(hpath, 'tela_comunidades.geojson'))
# zdr <- st_read(file.path(hpath, '180910 zonas recuperacion pesquera Tela.shp'))
# apmc <- st_read(file.path(hpath, 'APMC_HN_2019.shp'))
# save(comunidades, zdr, apmc, file='data/tela_mapa.rda')

# ==== fishdata ====

# # Paths to cleaned tables for now
# fishdbase <- fread(path <- '../fisheries-analysis/data/dbase_tela_clean.csv')
# lfqdbase <- fread('../fisheries-analysis/data/stock_analysis/tela_lfq_adjust.csv')
# 
# # Preparing trophic level table
# troph_cwalk <- fread('../fisheries-analysis/data/fishbase_tables/taxa_trophic_level_cwalk.csv')
# troph_cwalk[, nombre_cientifico := paste(genus, species)]
# troph_cwalk[, troph := ifelse(is.na(troph_diet), troph_food, troph_diet)]
# 
# # Joining trophic level data to lfqadj database
# setkey(lfqdbase, nombre_cientifico)
# setkey(troph_cwalk, nombre_cientifico)
# lfqdbase <- merge(lfqdbase,
# troph_cwalk[, c('nombre_cientifico', 'troph'), with=F],
# by='nombre_cientifico')
# 
# # Species/scientific name selection table
# spectab <- unique(na.omit(fishdbase[,.(nombre_comun_cln, nombre_cientifico)]))
# # Only getting those common names with at least 30 obs in the table
# valid_specs <- fishdbase |> 
#   _[nombre_comun_cln %chin% spectab$nombre_comun_cln,] |> 
#   _[,.(nobs = .N), by=nombre_comun_cln] |> 
#   _[nobs >= 30,] |> 
#   getElement('nombre_comun_cln')
# spectab <- spectab[nombre_comun_cln %chin% valid_specs,]
# 
# # Saving
# save(fishdbase, lfqdbase, troph_cwalk, spectab, file='data/fishdbase.rda')

