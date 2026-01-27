library(openalexR)
library(zen4R)
library(arrow)
library(dplyr)


# Set update flags to FALSE to prevent unnecessary reloading of data
seed_knowledge_update <- FALSE
expanded_knowledge_update <- FALSE
GOKH_version="0.0" 

# shinyapps.io: use a writable temp folder
CACHE_DIR <- file.path(tempdir(), "zenodo_cache")

col_oasis <- c(rgb(60/255, 77/255, 103/255), rgb(160/255, 177/255, 203/255), "#50521A", "#A19658","#794839", "#D1B091")
col_water_scale <- colorRampPalette(col_oasis[2:1])
col_green_scale <- colorRampPalette(col_oasis[4:3])
col_sand_scale <- colorRampPalette(col_oasis[6:5])
col_diverging_scale <- colorRampPalette(c(col_oasis[6],"white", col_oasis[2],"#333"))
col_oasis_scale <- colorRampPalette(col_oasis)
# 


