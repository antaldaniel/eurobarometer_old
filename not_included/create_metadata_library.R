library (eurobarometer)
library (tidyverse)
devtools::install_github("tidyverse/haven")
gesis_file <- "C:/Users/Daniel Antal/OneDrive - Visegrad Investments/_data/data-raw/gesis/ZA6863_v1-0-0.sav"

see_log = TRUE
create_log = TRUE
log_prefix = NA
log_id = NA
my_treshold = futile.logger::INFO
importdir <- "C:/Users/Daniel Antal/OneDrive - Visegrad Investments/_data/data-raw/gesis/"
exportdir <- importdir
sav_files <- dir ( importdir)[which(grepl(".sav", dir(importdir)))]
i = 1
try <- analyze_gesis_file (gesis_file = paste0(importdir, sav_files[i]))
#metadata <- readRDS("C:/Users/Daniel Antal/OneDrive - Visegrad Investments/_data/data-raw/gesis/ZA6925_v1-0-0_metadata.rds") %>%
#  filter ( suggested_class == "factor")

create_metadata_library <- function ( importdir, exportdir ) {
  importdir <- "C:/Users/Daniel Antal/OneDrive - Visegrad Investments/_data/data-raw/gesis/"
  exportdir <- importdir
  sav_files <- dir ( importdir)[which(grepl(".sav", dir(importdir)))]
  for (i in rev(1:length(sav_files))) {
    metadata_file <- gsub(".sav", "_metadata.rds", sav_files[i])
    cat ( metadata_file)
    if ( metadata_file %in% dir (importdir) ) next
    metadata_df <- analyze_gesis_file (paste0(importdir, sav_files[i]))
    saveRDS(metadata_df,paste0(exportdir, metadata_file ))
    message ( "Written: ", paste0(exportdir, metadata_file ))
  }


}

create_metadata_library (importdir)




