# ============================================================
# REDUCE — MESOSCALE BYCATCH
# 00_explore_raw_data.R
#
# PURPOSE
# -------
# Initial exploration and audit of the raw IEO bycatch data.
#
# This script:
#   1. Finds BYC_0002, BYC_0003 and BYC_0004
#   2. Inventories all raw files
#   3. Reads fishing-operation tables
#   4. Describes dimensions and variables
#   5. Examines IDs, temporal coverage and coordinates
#   6. Examines missing data
#   7. Produces simple exploratory maps
#
# IMPORTANT
# ---------
# This script is DIAGNOSTIC ONLY.
#
# It does not:
#   - modify raw data
#   - remove operations
#   - perform spatial QC exclusions
#   - match operations to eddies
#   - create modelling data
#
# Raw data must remain untouched.
# ============================================================


# ============================================================
# 0. CLEAN SESSION
# ============================================================

rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999
)


# ============================================================
# 1. PACKAGES
# ============================================================

packages <- c(
  "dplyr",
  "readr",
  "purrr",
  "stringr",
  "tidyr",
  "ggplot2",
  "sf",
  "rnaturalearth"
)

missing_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  
  stop(
    paste0(
      "Missing packages: ",
      paste(
        missing_packages,
        collapse = ", "
      ),
      "\nInstall them before running this script."
    )
  )
}

invisible(
  lapply(
    packages,
    library,
    character.only = TRUE
  )
)


# ============================================================
# 2. PROJECT PATHS
# ============================================================

PROJECT_DIR <- paste0(
  "/Users/jazelouled-cheikhbonan/",
  "Dropbox/2026_REDUCE_MesoscaleBycatch"
)

RAW_DIR <- file.path(
  PROJECT_DIR,
  "00rawData",
  "00bycatch_IEO"
)

OUT_DIR <- file.path(
  PROJECT_DIR,
  "outputs",
  "00_raw_exploration"
)

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


if (!dir.exists(RAW_DIR)) {
  
  stop(
    paste0(
      "Raw-data directory not found:\n",
      RAW_DIR
    )
  )
}


cat(
  "\n====================================================\n",
  "REDUCE — RAW IEO DATA EXPLORATION\n",
  "====================================================\n\n",
  "Raw directory:\n",
  RAW_DIR,
  "\n\n",
  "Output directory:\n",
  OUT_DIR,
  "\n\n",
  sep = ""
)


# ============================================================
# 3. LOCATE DATASETS
# ============================================================

dataset_dirs <- list.dirs(
  RAW_DIR,
  recursive = FALSE,
  full.names = TRUE
)

DIR_0002 <- dataset_dirs[
  str_detect(
    basename(dataset_dirs),
    "^BYC_0002"
  )
]

DIR_0003 <- dataset_dirs[
  str_detect(
    basename(dataset_dirs),
    "^BYC_0003"
  )
]

DIR_0004 <- dataset_dirs[
  str_detect(
    basename(dataset_dirs),
    "^BYC_0004"
  )
]


check_one_directory <- function(x, dataset) {
  
  if (length(x) == 0) {
    
    stop(
      paste0(
        dataset,
        " directory not found."
      )
    )
  }
  
  if (length(x) > 1) {
    
    stop(
      paste0(
        "More than one directory found for ",
        dataset,
        ":\n",
        paste(
          x,
          collapse = "\n"
        )
      )
    )
  }
  
}


check_one_directory(
  DIR_0002,
  "BYC_0002"
)

check_one_directory(
  DIR_0003,
  "BYC_0003"
)

check_one_directory(
  DIR_0004,
  "BYC_0004"
)


cat(
  "Datasets found:\n\n",
  "BYC_0002: ", DIR_0002, "\n",
  "BYC_0003: ", DIR_0003, "\n",
  "BYC_0004: ", DIR_0004, "\n\n",
  sep = ""
)


# ============================================================
# 4. INVENTORY ALL RAW FILES
# ============================================================

all_files <- list.files(
  RAW_DIR,
  recursive = TRUE,
  full.names = TRUE
)

file_inventory <- tibble(
  full_path = all_files
) %>%
  
  mutate(
    
    relative_path = str_remove(
      full_path,
      fixed(
        paste0(
          RAW_DIR,
          "/"
        )
      )
    ),
    
    dataset = case_when(
      
      str_detect(
        relative_path,
        "^BYC_0002"
      ) ~ "BYC_0002",
      
      str_detect(
        relative_path,
        "^BYC_0003"
      ) ~ "BYC_0003",
      
      str_detect(
        relative_path,
        "^BYC_0004"
      ) ~ "BYC_0004",
      
      TRUE ~ "other"
    ),
    
    filename = basename(
      full_path
    ),
    
    extension = tools::file_ext(
      full_path
    ),
    
    size_MB = file.info(
      full_path
    )$size / 1024^2
  )


write_csv(
  file_inventory,
  file.path(
    OUT_DIR,
    "00_raw_file_inventory.csv"
  )
)


cat(
  "RAW FILE INVENTORY\n",
  "------------------\n"
)

print(
  file_inventory %>%
    
    count(
      dataset,
      extension
    ),
  n = Inf
)


# ============================================================
# 5. FIND FISHING-OPERATION DIRECTORIES
# ============================================================

find_fishing_operations_dir <- function(dataset_dir) {
  
  candidates <- list.dirs(
    dataset_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  
  candidates[
    str_detect(
      tolower(
        basename(candidates)
      ),
      "fishingoperations"
    )
  ]
}


FO_DIR_0002 <- find_fishing_operations_dir(
  DIR_0002
)

FO_DIR_0003 <- find_fishing_operations_dir(
  DIR_0003
)

FO_DIR_0004 <- find_fishing_operations_dir(
  DIR_0004
)


cat(
  "\n\nFISHING OPERATION DIRECTORIES\n",
  "-----------------------------\n"
)

print(
  list(
    BYC_0002 = FO_DIR_0002,
    BYC_0003 = FO_DIR_0003,
    BYC_0004 = FO_DIR_0004
  )
)


# ============================================================
# 6. FUNCTION TO FIND TABULAR FILES
# ============================================================

find_tables <- function(x) {
  
  list.files(
    x,
    pattern = "\\.(csv|txt|tsv)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
}


tables_0002 <- find_tables(
  FO_DIR_0002
)

tables_0003 <- find_tables(
  FO_DIR_0003
)

tables_0004 <- find_tables(
  FO_DIR_0004
)


cat(
  "\n\nTABLES FOUND\n",
  "------------\n\n"
)

cat(
  "BYC_0002\n"
)

print(
  tables_0002
)

cat(
  "\nBYC_0003\n"
)

print(
  tables_0003
)

cat(
  "\nBYC_0004\n"
)

print(
  tables_0004
)


# ============================================================
# 7. ROBUST TABLE READER
# ============================================================

read_table_auto <- function(path) {
  
  ext <- tolower(
    tools::file_ext(path)
  )
  
  if (ext == "csv") {
    
    return(
      read_csv(
        path,
        show_col_types = FALSE,
        progress = FALSE
      )
    )
  }
  
  if (ext == "tsv") {
    
    return(
      read_tsv(
        path,
        show_col_types = FALSE,
        progress = FALSE
      )
    )
  }
  
  if (ext == "txt") {
    
    return(
      read_delim(
        path,
        delim = NULL,
        show_col_types = FALSE,
        progress = FALSE
      )
    )
  }
  
  stop(
    paste(
      "Unsupported extension:",
      ext
    )
  )
  
}


# ============================================================
# 8. INSPECT EVERY TABLE
# ============================================================

inspect_tables <- function(
    paths,
    dataset
) {
  
  map_dfr(
    paths,
    function(path) {
      
      dat <- tryCatch(
        
        read_table_auto(
          path
        ),
        
        error = function(e) NULL
      )
      
      if (is.null(dat)) {
        
        return(
          tibble(
            dataset = dataset,
            file = path,
            rows = NA_integer_,
            columns = NA_integer_,
            read_success = FALSE
          )
        )
      }
      
      tibble(
        dataset = dataset,
        file = path,
        rows = nrow(dat),
        columns = ncol(dat),
        read_success = TRUE
      )
      
    }
  )
  
}


table_summary <- bind_rows(
  
  inspect_tables(
    tables_0002,
    "BYC_0002"
  ),
  
  inspect_tables(
    tables_0003,
    "BYC_0003"
  ),
  
  inspect_tables(
    tables_0004,
    "BYC_0004"
  )
  
)


write_csv(
  table_summary,
  file.path(
    OUT_DIR,
    "01_raw_table_summary.csv"
  )
)


cat(
  "\n\nRAW TABLE SUMMARY\n",
  "-----------------\n"
)

print(
  table_summary,
  n = Inf
)


# ============================================================
# 9. VARIABLE INVENTORY
# ============================================================

get_variable_inventory <- function(
    paths,
    dataset
) {
  
  map_dfr(
    paths,
    function(path) {
      
      dat <- tryCatch(
        
        read_table_auto(
          path
        ),
        
        error = function(e) NULL
      )
      
      if (is.null(dat)) {
        return(NULL)
      }
      
      tibble(
        
        dataset = dataset,
        
        file = basename(
          path
        ),
        
        variable = names(
          dat
        ),
        
        class = map_chr(
          dat,
          ~ paste(
            class(.x),
            collapse = "/"
          )
        ),
        
        missing_N = map_int(
          dat,
          ~ sum(
            is.na(.x)
          )
        ),
        
        missing_percent = map_dbl(
          dat,
          ~ 100 *
            mean(
              is.na(.x)
            )
        )
        
      )
      
    }
  )
  
}


variable_inventory <- bind_rows(
  
  get_variable_inventory(
    tables_0002,
    "BYC_0002"
  ),
  
  get_variable_inventory(
    tables_0003,
    "BYC_0003"
  ),
  
  get_variable_inventory(
    tables_0004,
    "BYC_0004"
  )
  
)


write_csv(
  variable_inventory,
  file.path(
    OUT_DIR,
    "02_raw_variable_inventory.csv"
  )
)


# ============================================================
# 10. FIND LIKELY MAIN OPERATION TABLE
# ============================================================
#
# We choose the readable table containing the largest
# number of rows inside fishingOperations.
#
# This is exploratory only.
#
# Script 01 will explicitly define the files used once their
# structure has been verified here.
# ============================================================

main_tables <- table_summary %>%
  
  filter(
    read_success
  ) %>%
  
  group_by(
    dataset
  ) %>%
  
  slice_max(
    rows,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup()


cat(
  "\n\nLIKELY MAIN FISHING-OPERATION TABLES\n",
  "------------------------------------\n"
)

print(
  main_tables,
  n = Inf
)


# ============================================================
# 11. READ LIKELY MAIN TABLES
# ============================================================

get_main_file <- function(dataset_name) {
  
  main_tables %>%
    
    filter(
      dataset == dataset_name
    ) %>%
    
    pull(
      file
    )
  
}


ops_0002_raw <- read_table_auto(
  get_main_file(
    "BYC_0002"
  )
)

ops_0003_raw <- read_table_auto(
  get_main_file(
    "BYC_0003"
  )
)

ops_0004_raw <- read_table_auto(
  get_main_file(
    "BYC_0004"
  )
)


# ============================================================
# 12. BASIC DIMENSIONS
# ============================================================

basic_summary <- tibble(
  
  dataset = c(
    "BYC_0002",
    "BYC_0003",
    "BYC_0004"
  ),
  
  rows = c(
    nrow(
      ops_0002_raw
    ),
    nrow(
      ops_0003_raw
    ),
    nrow(
      ops_0004_raw
    )
  ),
  
  columns = c(
    ncol(
      ops_0002_raw
    ),
    ncol(
      ops_0003_raw
    ),
    ncol(
      ops_0004_raw
    )
  )
  
)


cat(
  "\n\nMAIN TABLE DIMENSIONS\n",
  "---------------------\n"
)

print(
  basic_summary
)


write_csv(
  basic_summary,
  file.path(
    OUT_DIR,
    "03_main_table_dimensions.csv"
  )
)


# ============================================================
# 13. PRINT VARIABLE NAMES
# ============================================================

cat(
  "\n\n============================================\n",
  "BYC_0002 VARIABLES\n",
  "============================================\n"
)

print(
  names(
    ops_0002_raw
  )
)


cat(
  "\n\n============================================\n",
  "BYC_0003 VARIABLES\n",
  "============================================\n"
)

print(
  names(
    ops_0003_raw
  )
)


cat(
  "\n\n============================================\n",
  "BYC_0004 VARIABLES\n",
  "============================================\n"
)

print(
  names(
    ops_0004_raw
  )
)


# ============================================================
# 14. COMPARE VARIABLE NAMES BETWEEN DATASETS
# ============================================================

variable_presence <- tibble(
  
  variable = sort(
    unique(
      c(
        names(
          ops_0002_raw
        ),
        names(
          ops_0003_raw
        ),
        names(
          ops_0004_raw
        )
      )
    )
  )
  
) %>%
  
  mutate(
    
    BYC_0002 = variable %in%
      names(
        ops_0002_raw
      ),
    
    BYC_0003 = variable %in%
      names(
        ops_0003_raw
      ),
    
    BYC_0004 = variable %in%
      names(
        ops_0004_raw
      )
    
  )


write_csv(
  variable_presence,
  file.path(
    OUT_DIR,
    "04_variable_presence_between_datasets.csv"
  )
)


cat(
  "\n\nVARIABLE PRESENCE BETWEEN DATASETS\n",
  "----------------------------------\n"
)

print(
  variable_presence,
  n = Inf
)


# ============================================================
# 15. IDENTIFY POTENTIALLY IMPORTANT VARIABLES
# ============================================================

important_patterns <- c(
  
  "operation",
  "trip",
  "vessel",
  
  "date",
  "time",
  "year",
  
  "lat",
  "lon",
  
  "set",
  "haul",
  
  "hook",
  "effort",
  
  "catch",
  "bycatch",
  
  "species",
  "scientific",
  "common",
  
  "comment"
)


important_variables <- variable_presence %>%
  
  filter(
    
    str_detect(
      
      tolower(
        variable
      ),
      
      paste(
        important_patterns,
        collapse = "|"
      )
      
    )
    
  )


cat(
  "\n\nPOTENTIALLY IMPORTANT VARIABLES\n",
  "-------------------------------\n"
)

print(
  important_variables,
  n = Inf
)


write_csv(
  important_variables,
  file.path(
    OUT_DIR,
    "05_potentially_important_variables.csv"
  )
)


# ============================================================
# 16. DATASET HEADS
# ============================================================

cat(
  "\n\n============================================\n",
  "FIRST ROWS — BYC_0002\n",
  "============================================\n"
)

print(
  ops_0002_raw %>%
    head(
      5
    ),
  width = Inf
)


cat(
  "\n\n============================================\n",
  "FIRST ROWS — BYC_0003\n",
  "============================================\n"
)

print(
  ops_0003_raw %>%
    head(
      5
    ),
  width = Inf
)


cat(
  "\n\n============================================\n",
  "FIRST ROWS — BYC_0004\n",
  "============================================\n"
)

print(
  ops_0004_raw %>%
    head(
      5
    ),
  width = Inf
)


# ============================================================
# 17. MISSINGNESS
# ============================================================

missing_summary <- function(
    dat,
    dataset
) {
  
  tibble(
    
    dataset = dataset,
    
    variable = names(
      dat
    ),
    
    N = nrow(
      dat
    ),
    
    missing = map_int(
      dat,
      ~ sum(
        is.na(.x)
      )
    ),
    
    missing_percent = map_dbl(
      dat,
      ~ 100 *
        mean(
          is.na(.x)
        )
    )
    
  ) %>%
    
    arrange(
      desc(
        missing_percent
      )
    )
  
}


missing_all <- bind_rows(
  
  missing_summary(
    ops_0002_raw,
    "BYC_0002"
  ),
  
  missing_summary(
    ops_0003_raw,
    "BYC_0003"
  ),
  
  missing_summary(
    ops_0004_raw,
    "BYC_0004"
  )
  
)


write_csv(
  missing_all,
  file.path(
    OUT_DIR,
    "06_missingness_main_tables.csv"
  )
)


# ============================================================
# 18. CHECK KEY IDS IF PRESENT
# ============================================================

inspect_id <- function(
    dat,
    variable,
    dataset
) {
  
  if (
    !variable %in%
    names(
      dat
    )
  ) {
    
    return(
      tibble(
        dataset = dataset,
        variable = variable,
        N = nrow(dat),
        unique_values = NA_integer_,
        missing = NA_integer_
      )
    )
    
  }
  
  tibble(
    
    dataset = dataset,
    
    variable = variable,
    
    N = nrow(
      dat
    ),
    
    unique_values = n_distinct(
      dat[[variable]],
      na.rm = TRUE
    ),
    
    missing = sum(
      is.na(
        dat[[variable]]
      )
    )
    
  )
  
}


candidate_ids <- c(
  "operationID",
  "tripID",
  "vesselID"
)


id_summary <- bind_rows(
  
  map_dfr(
    candidate_ids,
    ~ inspect_id(
      ops_0002_raw,
      .x,
      "BYC_0002"
    )
  ),
  
  map_dfr(
    candidate_ids,
    ~ inspect_id(
      ops_0003_raw,
      .x,
      "BYC_0003"
    )
  ),
  
  map_dfr(
    candidate_ids,
    ~ inspect_id(
      ops_0004_raw,
      .x,
      "BYC_0004"
    )
  )
  
)


write_csv(
  id_summary,
  file.path(
    OUT_DIR,
    "07_identifier_summary.csv"
  )
)


cat(
  "\n\nIDENTIFIER SUMMARY\n",
  "------------------\n"
)

print(
  id_summary,
  n = Inf
)


# ============================================================
# 19. SAVE SESSION INFORMATION
# ============================================================

capture.output(
  
  sessionInfo(),
  
  file = file.path(
    OUT_DIR,
    "sessionInfo.txt"
  )
  
)


# ============================================================
# 20. FINAL REPORT
# ============================================================

cat(
  "\n\n====================================================\n",
  "RAW EXPLORATION COMPLETE\n",
  "====================================================\n\n",
  sep = ""
)


cat(
  "Nothing in the raw data has been modified.\n\n"
)


cat(
  "Outputs written to:\n",
  OUT_DIR,
  "\n\n",
  sep = ""
)


cat(
  "Important outputs:\n",
  "  00_raw_file_inventory.csv\n",
  "  01_raw_table_summary.csv\n",
  "  02_raw_variable_inventory.csv\n",
  "  03_main_table_dimensions.csv\n",
  "  04_variable_presence_between_datasets.csv\n",
  "  05_potentially_important_variables.csv\n",
  "  06_missingness_main_tables.csv\n",
  "  07_identifier_summary.csv\n",
  "  sessionInfo.txt\n\n",
  sep = ""
)


cat(
  "NEXT STEP:\n",
  "Inspect these outputs and define the exact transformations\n",
  "implemented in 01_build_standardised_operations.R\n\n",
  sep = ""
)


cat(
  "====================================================\n"
)