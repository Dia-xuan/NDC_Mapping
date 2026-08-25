# ============================================================
# merge_ndc_raw_files.R
# ============================================================

library(tidyverse)

source("parse_ndc_filenames.R")


# ============================================================
# 1. Read FDA CSV with detected encoding
# ============================================================

read_fda_csv <- function(path) {
  
  enc <- guess_encoding(path, n_max = 10000) %>%
    slice(1) %>%
    pull(encoding)
  
  if (length(enc) == 0 || is.na(enc)) {
    enc <- "UTF-8"
  }
  
  read_csv(
    path,
    locale = locale(encoding = enc),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    na = ""
  ) %>%
    mutate(
      across(
        where(is.character),
        ~ .x %>%
          str_replace_all("\u00A0", " ") %>%
          str_trim()
      )
    )
}


# ============================================================
# 2. Read one raw file + attach metadata
# ============================================================

read_one_file <- function(meta) {
  
  df <- read_fda_csv(meta$file_path)
  
  ndc_col <- names(df)[
    str_detect(
      names(df),
      regex("^NDC[ ._]?Package[ ._]?Code$", ignore_case = TRUE)
    )
  ][1]
  
  if (is.na(ndc_col)) {
    stop(
      "NDC Package Code column not found in: ",
      meta$file_name
    )
  }
  
  names(df)[names(df) == ndc_col] <- "NDC Package Code"
  
  df %>%
    mutate(
      category    = meta$category,
      medication  = meta$medication,
      combo       = meta$combo,
      search_type = meta$search_type,
      search_term = meta$search_term,
      source_file = meta$file_name,
      .before = 1
    )
}


# ============================================================
# 3. Process one medication
# ============================================================

process_medication <- function(meta) {
  
  # ----------------------------------------------------------
  # Read all files belonging to the same medication
  # ----------------------------------------------------------
  
  raw <- meta %>%
    split(seq_len(nrow(.))) %>%
    map_dfr(read_one_file)
  
  
  # ----------------------------------------------------------
  # Missing NDC
  # ----------------------------------------------------------
  
  missing <- raw %>%
    filter(
      is.na(`NDC Package Code`) |
        `NDC Package Code` == ""
    )
  
  valid <- raw %>%
    filter(
      !is.na(`NDC Package Code`),
      `NDC Package Code` != ""
    )
  
  
  # ----------------------------------------------------------
  # Sparse records
  #
  # Flag if >= 3 key descriptive fields are missing
  # ----------------------------------------------------------
  
  qc_fields <- intersect(
    c(
      "Strength",
      "Dosage Form",
      "Route",
      "Application No.",
      "Labeler Name"
    ),
    names(valid)
  )
  
  valid <- valid %>%
    mutate(
      qc_missing_fields = rowSums(
        across(
          all_of(qc_fields),
          ~ is.na(.x) | .x == ""
        )
      ),
      qc_sparse = qc_missing_fields >= 3
    )
  
  sparse <- valid %>%
    filter(qc_sparse)
  
  
  # ----------------------------------------------------------
  # Exact duplicate detection
  #
  # Ignore provenance columns when deciding whether two
  # FDA records are otherwise identical.
  # ----------------------------------------------------------
  
  provenance_cols <- c(
    "category",
    "medication",
    "combo",
    "search_type",
    "search_term",
    "source_file",
    "qc_missing_fields",
    "qc_sparse"
  )
  
  content_cols <- setdiff(
    names(valid),
    provenance_cols
  )
  
  exact_duplicates_removed <-
    nrow(valid) -
    nrow(
      valid %>%
        distinct(
          across(all_of(content_cols))
        )
    )
  
  
  # ----------------------------------------------------------
  # Collapse exact duplicates but preserve search provenance
  # ----------------------------------------------------------
  
  merged <- valid %>%
    group_by(
      across(all_of(content_cols))
    ) %>%
    summarise(
      category   = first(category),
      medication = first(medication),
      combo      = first(combo),
      
      search_type = paste(
        unique(na.omit(search_type)),
        collapse = " | "
      ),
      
      search_term = paste(
        unique(na.omit(search_term)),
        collapse = " | "
      ),
      
      source_file = paste(
        unique(na.omit(source_file)),
        collapse = " | "
      ),
      
      qc_missing_fields = first(qc_missing_fields),
      qc_sparse = first(qc_sparse),
      
      .groups = "drop"
    ) %>%
    relocate(
      category,
      medication,
      combo,
      search_type,
      search_term,
      source_file
    )
  
  
  # ----------------------------------------------------------
  # Conflicting NDC
  #
  # If the same NDC still has >1 row after exact duplicates
  # were removed, descriptive information differs.
  #
  # Keep ALL such rows in the master.
  # ----------------------------------------------------------
  
  merged <- merged %>%
    group_by(`NDC Package Code`) %>%
    mutate(
      qc_conflict = n() > 1
    ) %>%
    ungroup()
  
  conflicts <- merged %>%
    filter(qc_conflict)
  
  
  # ----------------------------------------------------------
  # Summary
  # ----------------------------------------------------------
  
  stats <- tibble(
    category = meta$category[1],
    medication = meta$medication[1],
    combo = any(meta$combo),
    
    raw_files = nrow(meta),
    raw_rows = nrow(raw),
    
    nonproprietary_rows =
      sum(raw$search_type == "nonproprietary", na.rm = TRUE),
    
    proprietary_rows =
      sum(raw$search_type == "proprietary", na.rm = TRUE),
    
    exact_duplicates_removed =
      exact_duplicates_removed,
    
    conflicting_ndc_codes =
      conflicts %>%
      distinct(`NDC Package Code`) %>%
      nrow(),
    
    conflicting_rows_retained =
      nrow(conflicts),
    
    missing_ndc_rows =
      nrow(missing),
    
    sparse_rows =
      nrow(sparse),
    
    distinct_ndc_codes =
      merged %>%
      distinct(`NDC Package Code`) %>%
      nrow(),
    
    final_master_rows =
      nrow(merged)
  )
  
  
  list(
    master = merged,
    missing = missing,
    sparse = sparse,
    conflicts = conflicts,
    stats = stats
  )
}


# ============================================================
# 4. Group files by category + medication
# ============================================================

medication_groups <- ndc_file_metadata %>%
  group_by(
    category,
    medication
  ) %>%
  group_split()


# ============================================================
# 5. Process all medications
# ============================================================

results <- map(
  medication_groups,
  process_medication
)


# ============================================================
# 6. Combine results
# ============================================================

master_all <- map_dfr(results, "master")
missing_all <- map_dfr(results, "missing")
sparse_all <- map_dfr(results, "sparse")
conflict_all <- map_dfr(results, "conflicts")
medication_summary <- map_dfr(results, "stats")


# ============================================================
# 7. Export by category
# ============================================================

categories <- unique(
  ndc_file_metadata$category
)

walk(
  categories,
  
  function(cat) {
    
    category_dir <- file.path(
      "data",
      "processed",
      cat
    )
    
    qc_dir <- file.path(
      category_dir,
      "qc"
    )
    
    dir.create(
      category_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    dir.create(
      qc_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    
    # --------------------------------------------------------
    # Remove OLD QC csv files first
    # --------------------------------------------------------
    
    old_qc <- list.files(
      qc_dir,
      pattern = "\\.csv$",
      full.names = TRUE
    )
    
    if (length(old_qc) > 0) {
      file.remove(old_qc)
    }
    
    
    # --------------------------------------------------------
    # Master file
    # --------------------------------------------------------
    
    category_master <- master_all %>%
      filter(category == cat)
    
    write_excel_csv(
      category_master,
      file.path(
        category_dir,
        paste0(cat, "_master.csv")
      )
    )
    
    
    # --------------------------------------------------------
    # Missing NDC
    # Only create file if rows exist
    # --------------------------------------------------------
    
    category_missing <- missing_all %>%
      filter(category == cat)
    
    if (nrow(category_missing) > 0) {
      
      write_excel_csv(
        category_missing,
        file.path(
          qc_dir,
          "missing_ndc.csv"
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Sparse records
    # Only create file if rows exist
    # --------------------------------------------------------
    
    category_sparse <- sparse_all %>%
      filter(category == cat)
    
    if (nrow(category_sparse) > 0) {
      
      write_excel_csv(
        category_sparse,
        file.path(
          qc_dir,
          "sparse_records.csv"
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Conflicting NDC
    # Only create file if rows exist
    # --------------------------------------------------------
    
    category_conflicts <- conflict_all %>%
      filter(category == cat)
    
    if (nrow(category_conflicts) > 0) {
      
      write_excel_csv(
        category_conflicts,
        file.path(
          qc_dir,
          "conflicting_ndc.csv"
        )
      )
    }
  }
)


# ============================================================
# 8. Summary output
# ============================================================

summary_dir <- file.path(
  "data",
  "processed",
  "summary"
)

dir.create(
  summary_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


write_excel_csv(
  medication_summary,
  file.path(
    summary_dir,
    "medication_merge_summary.csv"
  )
)


# ============================================================
# 9. Category-level summary
# ============================================================

category_summary <- medication_summary %>%
  group_by(category) %>%
  summarise(
    medications =
      n_distinct(medication),
    
    combo_medications =
      n_distinct(
        medication[combo]
      ),
    
    raw_files =
      sum(raw_files),
    
    raw_rows =
      sum(raw_rows),
    
    nonproprietary_rows =
      sum(nonproprietary_rows),
    
    proprietary_rows =
      sum(proprietary_rows),
    
    exact_duplicates_removed =
      sum(exact_duplicates_removed),
    
    conflicting_ndc_codes =
      sum(conflicting_ndc_codes),
    
    conflicting_rows_retained =
      sum(conflicting_rows_retained),
    
    missing_ndc_rows =
      sum(missing_ndc_rows),
    
    sparse_rows =
      sum(sparse_rows),
    
    .groups = "drop"
  )


category_master_counts <- master_all %>%
  group_by(category) %>%
  summarise(
    distinct_ndc_codes =
      n_distinct(`NDC Package Code`),
    
    final_master_rows =
      n(),
    
    .groups = "drop"
  )


category_summary <- category_summary %>%
  left_join(
    category_master_counts,
    by = "category"
  )


write_excel_csv(
  category_summary,
  file.path(
    summary_dir,
    "category_merge_summary.csv"
  )
)


# ============================================================
# 10. Print summaries
# ============================================================

print(medication_summary)
print(category_summary)