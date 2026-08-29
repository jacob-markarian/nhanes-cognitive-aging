# Download NHANES 2011-2014 data components
# Author: Jacob Markarian

library(nhanesA)
library(here)

file_codes <- c(
  "DEMO_G", "PAQ_G", "DPQ_G", "CFX_G",
  "DEMO_H", "PAQ_H", "DPQ_H", "CFX_H"
)

raw_data_directory <- here("data", "raw")

dir.create(
  raw_data_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

for (file_code in file_codes) {
  message("Downloading ", file_code)
  
  dataset <- nhanes(file_code)
  
  saveRDS(
    dataset,
    here("data", "raw", paste0(file_code, ".rds"))
  )
}

message("NHANES data download complete.")


