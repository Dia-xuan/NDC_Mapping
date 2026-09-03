library(stringr)
library(dplyr)
library(purrr)
library(tibble)

# ------------------------------------------------------------
# Function: parse one NDC raw filename
# ------------------------------------------------------------

parse_ndc_filename <- function(file_path) {
  
  file_name <- basename(file_path)
  
  # Remove ".csv"
  file_stem <- str_remove(file_name, "\\.csv$")
  
  # Split filename by "_"
  parts <- str_split(file_stem, "_")[[1]]
  
  # Parent folders identify medication category and broad group
  parent_dir <- basename(dirname(file_path))
  grandparent_dir <- basename(dirname(dirname(file_path)))
  
  category <- parent_dir
  
  broad_group <- if_else(
    grandparent_dir == "raw",
    parent_dir,
    grandparent_dir
  )
  
  # First field is always the medication name
  medication <- parts[1]
  
  # Identify whether this is a combination medication
  is_combo <- "combo" %in% parts
  
  # Identify search type
  if ("nonproprietary" %in% parts) {
    search_type <- "nonproprietary"
  } else if ("proprietary" %in% parts) {
    search_type <- "proprietary"
  } else {
    search_type <- NA_character_
  }
  
  # Extract search term
  if (search_type == "nonproprietary") {
    
    search_term <- medication
    
  } else if (search_type == "proprietary") {
    
    proprietary_position <- which(parts == "proprietary")
    
    # Everything between medication/combo field and "proprietary"
    start_position <- if (is_combo) 3 else 2
    
    if (proprietary_position > start_position) {
      search_term <- paste(
        parts[start_position:(proprietary_position - 1)],
        collapse = "_"
      )
    } else {
      search_term <- NA_character_
    }
    
  } else {
    
    search_term <- NA_character_
    
  }
  
  tibble(
    broad_group = broad_group,
    category = category,
    medication = medication,
    combo = is_combo,
    search_type = search_type,
    search_term = search_term,
    file_name = file_name,
    file_path = file_path
  )
}


# ------------------------------------------------------------
# Find all raw CSV files
# ------------------------------------------------------------

raw_files <- list.files(
  path = "data/raw",
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)


# ------------------------------------------------------------
# Parse every filename
# ------------------------------------------------------------

ndc_file_metadata <- map_dfr(
  raw_files,
  parse_ndc_filename
)


# ------------------------------------------------------------
# Inspect result
# ------------------------------------------------------------

print(ndc_file_metadata)