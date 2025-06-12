# This file contains loop to generate the combined_tandem sheet.


# packages
library(here)
library(tidyverse)
library(patchwork)
library(ggsankey)


# Load data

# Set your directory path here
dir_path <- here("data/extraction/DT_extractedData_withoutQuality")

# List all files in the directory
all_files <- list.files(path = dir_path, pattern = "^tandem.*\\.csv$", full.names = TRUE)


df_list <- list()

for (file in all_files) {
  # Read second row as header
  header <- read.csv(file, header = FALSE, skip = 1, nrows = 1, stringsAsFactors = FALSE)
  header <- as.character(unlist(header))
  
  # Read data starting from row 3
  data <- read.csv(file, header = FALSE, skip = 2, stringsAsFactors = FALSE)
  
  # Assign header
  colnames(data) <- header
  
  # Convert all columns to factors
  data[] <- lapply(data, as.factor)
  
  df_list[[file]] <- data
}

# Merge all data frames by matching columns, fill missing columns with NA
all_data <- bind_rows(df_list)


# Optional: save combined file
# write.csv(all_data, file = file.path(dir_path, "combined_tandem_meta.csv"), row.names = FALSE)


# old 
#data_extract <- read.csv2(here("data/extraction/tandem5_transformation_extraction - DT_results_extraction_new.csv"), sep = ",", header = T, skip = 1)

# data_extract = all_data

# Remove NA (only for now!)
# data_extract <- data_extract[!is.na(data_extract$n_with_exclusions), ]

# Customize some variables
# Age as numeric
# data_extract$age_mean_total <- as.numeric(data_extract$age_mean_total)
# data_extract$age_sd_total <- as.numeric(data_extract$age_sd_total)


