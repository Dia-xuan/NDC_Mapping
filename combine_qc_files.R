library(tidyverse)

qc_types <- c(
  "missing_ndc",
  "conflicting_ndc",
  "sparse_records"
)


# ============================================================
# 1. Function: combine one QC file type
# ============================================================

combine_qc_type <- function(qc_type) {
  
  qc_files <- list.files(
    path = "data/processed",
    pattern = paste0("^", qc_type, "\\.csv$"),
    recursive = TRUE,
    full.names = TRUE
  )
  
  # return empty tibble if no QC files found
  if (length(qc_files) == 0) {
    
    return(
      tibble()
    )
  }
  
  combined <- qc_files %>%
    map_dfr(
      ~ read_csv(
        .x,
        col_types = cols(.default = col_character()),
        show_col_types = FALSE,
        na = ""
      )
    )
  
  combined
}



missing_review <- combine_qc_type(
  "missing_ndc"
)

conflicting_review <- combine_qc_type(
  "conflicting_ndc"
) %>% 
  arrange(
    `NDC Package Code`,
    broad_group,
    category,
    medication
  )

sparse_review <- combine_qc_type(
  "sparse_records"
)


# output folder

review_dir <- file.path(
  "data",
  "processed",
  "qc_review"
)

dir.create(
  review_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


write_excel_csv(
  missing_review,
  file.path(
    review_dir,
    "all_missing_ndc.csv"
  )
)

write_excel_csv(
  conflicting_review,
  file.path(
    review_dir,
    "all_conflicting_ndc.csv"
  )
)

write_excel_csv(
  sparse_review,
  file.path(
    review_dir,
    "all_sparse_records.csv"
  )
)


qc_review_summary <- tibble(
  qc_type = c(
    "missing_ndc",
    "conflicting_ndc",
    "sparse_records"
  ),
  
  rows = c(
    nrow(missing_review),
    nrow(conflicting_review),
    nrow(sparse_review)
  )
)

print(qc_review_summary)