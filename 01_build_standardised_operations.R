# ============================================================
# REDUCE — MESOSCALE BYCATCH
# 01_build_standardised_operations.R
#
# PURPOSE
# -------
# Build a standardised operation-level dataset from the raw
# IEO fishing-operation data.
#
# INPUT DATASETS
# --------------
# BYC_0002 = Atlantic purse seine
# BYC_0003 = Mediterranean -> Atlantic longline
# BYC_0004 = Galician / Atlantic longline
#
# OUTPUT
# ------
# One standardised operation-level table containing:
#
#   dataset
#   fishery
#   operation_id
#   trip_id
#   vessel_id
#   date
#   year
#   month
#
#   representative_lon
#   representative_lat
#
#   set_lon1
#   set_lat1
#   set_lon2
#   set_lat2
#
#   haul_lon1
#   haul_lat1
#   haul_lon2
#   haul_lat2
#
#   set_length_km
#   haul_length_km
#   set_haul_displacement_km
#   mcp_area_km2
#
#   excluded
#   exclusion_reason
#
# IMPORTANT
# ---------
# No rows are deleted.
#
# Operations/trips identified during spatial QC are retained
# and marked:
#
#       excluded = TRUE
#
# They MUST be removed explicitly before modelling:
#
#       filter(!excluded)
#
# Raw data are never modified.
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
  "lubridate",
  "sf",
  "geosphere"
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
      )
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
  "01_standardised_data"
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


# ============================================================
# 3. LOCATE DATASET DIRECTORIES
# ============================================================

dataset_dirs <- list.dirs(
  RAW_DIR,
  recursive = FALSE,
  full.names = TRUE
)


find_dataset_dir <- function(code) {
  
  x <- dataset_dirs[
    str_detect(
      basename(dataset_dirs),
      paste0(
        "^",
        code
      )
    )
  ]
  
  if (length(x) != 1) {
    
    stop(
      paste0(
        "Could not uniquely identify ",
        code,
        ". Found ",
        length(x),
        " directories."
      )
    )
  }
  
  x
}


DIR_0002 <- find_dataset_dir(
  "BYC_0002"
)

DIR_0003 <- find_dataset_dir(
  "BYC_0003"
)

DIR_0004 <- find_dataset_dir(
  "BYC_0004"
)


# ============================================================
# 4. FIND fishingOperations DIRECTORIES
# ============================================================

find_fishing_operations_dir <- function(dataset_dir) {
  
  x <- list.dirs(
    dataset_dir,
    recursive = TRUE,
    full.names = TRUE
  )
  
  x <- x[
    str_detect(
      tolower(
        basename(x)
      ),
      "^fishingoperations$"
    )
  ]
  
  if (length(x) != 1) {
    
    stop(
      paste0(
        "Could not uniquely identify fishingOperations in:\n",
        dataset_dir
      )
    )
  }
  
  x
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
  "\nFishing-operation directories:\n\n",
  "BYC_0002:\n", FO_DIR_0002, "\n\n",
  "BYC_0003:\n", FO_DIR_0003, "\n\n",
  "BYC_0004:\n", FO_DIR_0004, "\n\n",
  sep = ""
)


# ============================================================
# 5. GENERIC TABLE READER
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
      "Unsupported file type:",
      path
    )
  )
}


# ============================================================
# 6. IDENTIFY MAIN OPERATION TABLE
# ============================================================
#
# We identify the largest readable tabular file inside each
# fishingOperations directory.
#
# This makes the script independent of exact filenames.
# ============================================================

find_main_table <- function(directory) {
  
  files <- list.files(
    directory,
    pattern = "\\.(csv|txt|tsv)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(files) == 0) {
    
    stop(
      paste0(
        "No tabular files found in:\n",
        directory
      )
    )
  }
  
  
  info <- map_dfr(
    files,
    function(f) {
      
      x <- tryCatch(
        read_table_auto(f),
        error = function(e) NULL
      )
      
      tibble(
        file = f,
        rows = ifelse(
          is.null(x),
          NA_integer_,
          nrow(x)
        )
      )
    }
  )
  
  
  info <- info %>%
    filter(
      !is.na(rows)
    ) %>%
    arrange(
      desc(rows)
    )
  
  
  if (nrow(info) == 0) {
    
    stop(
      paste0(
        "No readable tables found in:\n",
        directory
      )
    )
  }
  
  
  info$file[1]
}


FILE_0002 <- find_main_table(
  FO_DIR_0002
)

FILE_0003 <- find_main_table(
  FO_DIR_0003
)

FILE_0004 <- find_main_table(
  FO_DIR_0004
)


cat(
  "\nMain operation tables selected:\n\n",
  "BYC_0002:\n", FILE_0002, "\n\n",
  "BYC_0003:\n", FILE_0003, "\n\n",
  "BYC_0004:\n", FILE_0004, "\n\n",
  sep = ""
)


# ============================================================
# 7. READ RAW OPERATION TABLES
# ============================================================

raw_0002 <- read_table_auto(
  FILE_0002
)

raw_0003 <- read_table_auto(
  FILE_0003
)

raw_0004 <- read_table_auto(
  FILE_0004
)


cat(
  "\nRaw dimensions:\n",
  "BYC_0002: ", nrow(raw_0002), " x ", ncol(raw_0002), "\n",
  "BYC_0003: ", nrow(raw_0003), " x ", ncol(raw_0003), "\n",
  "BYC_0004: ", nrow(raw_0004), " x ", ncol(raw_0004), "\n\n",
  sep = ""
)


# ============================================================
# 8. COLUMN-NAME HELPERS
# ============================================================
#
# Raw names may differ slightly between datasets.
#
# We therefore search using several possible names.
# ============================================================

find_col <- function(
    dat,
    candidates,
    required = FALSE
) {
  
  nm <- names(dat)
  
  nm_clean <- nm %>%
    tolower() %>%
    str_replace_all(
      "[^a-z0-9]",
      ""
    )
  
  
  candidates_clean <- candidates %>%
    tolower() %>%
    str_replace_all(
      "[^a-z0-9]",
      ""
    )
  
  
  idx <- match(
    candidates_clean,
    nm_clean
  )
  
  idx <- idx[
    !is.na(idx)
  ]
  
  
  if (length(idx) == 0) {
    
    if (required) {
      
      stop(
        paste0(
          "Required column not found.\n",
          "Candidates: ",
          paste(
            candidates,
            collapse = ", "
          )
        )
      )
    }
    
    return(
      NA_character_
    )
  }
  
  
  nm[
    idx[1]
  ]
}


pull_optional <- function(
    dat,
    col
) {
  
  if (
    length(col) == 0 ||
    is.na(col) ||
    !col %in% names(dat)
  ) {
    
    return(
      rep(
        NA,
        nrow(dat)
      )
    )
  }
  
  dat[[col]]
}


# ============================================================
# 9. DATE PARSER
# ============================================================

parse_date_safe <- function(x) {
  
  if (inherits(x, "Date")) {
    return(x)
  }
  
  if (
    inherits(
      x,
      c(
        "POSIXct",
        "POSIXt"
      )
    )
  ) {
    
    return(
      as.Date(x)
    )
  }
  
  
  x <- as.character(x)
  
  
  suppressWarnings(
    
    as.Date(
      
      parse_date_time(
        
        x,
        
        orders = c(
          "ymd",
          "dmy",
          "mdy",
          
          "ymd HMS",
          "dmy HMS",
          "mdy HMS",
          
          "ymd HM",
          "dmy HM",
          "mdy HM"
        ),
        
        quiet = TRUE
      )
      
    )
    
  )
}


# ============================================================
# 10. DISTANCE HELPERS
# ============================================================

distance_km <- function(
    lon1,
    lat1,
    lon2,
    lat2
) {
  
  ok <- is.finite(lon1) &
    is.finite(lat1) &
    is.finite(lon2) &
    is.finite(lat2)
  
  
  out <- rep(
    NA_real_,
    length(lon1)
  )
  
  
  if (any(ok)) {
    
    out[ok] <- geosphere::distHaversine(
      
      cbind(
        lon1[ok],
        lat1[ok]
      ),
      
      cbind(
        lon2[ok],
        lat2[ok]
      )
      
    ) / 1000
  }
  
  
  out
}


# ============================================================
# 11. MCP AREA HELPER
# ============================================================
#
# MCP uses the four endpoints:
#
# SET start
# SET end
# HAUL start
# HAUL end
#
# Area is calculated after projection to an automatically
# selected UTM zone.
#
# This variable is descriptive and is NOT the position used
# for eddy matching.
# ============================================================

mcp_area_one <- function(
    set_lon1,
    set_lat1,
    set_lon2,
    set_lat2,
    haul_lon1,
    haul_lat1,
    haul_lon2,
    haul_lat2
) {
  
  coords <- tibble(
    
    lon = c(
      set_lon1,
      set_lon2,
      haul_lon1,
      haul_lon2
    ),
    
    lat = c(
      set_lat1,
      set_lat2,
      haul_lat1,
      haul_lat2
    )
    
  ) %>%
    
    filter(
      is.finite(lon),
      is.finite(lat)
    ) %>%
    
    distinct()
  
  
  if (nrow(coords) < 3) {
    
    return(
      NA_real_
    )
  }
  
  
  mean_lon <- mean(
    coords$lon
  )
  
  mean_lat <- mean(
    coords$lat
  )
  
  
  zone <- floor(
    (mean_lon + 180) / 6
  ) + 1
  
  
  zone <- max(
    1,
    min(
      60,
      zone
    )
  )
  
  
  epsg <- ifelse(
    mean_lat >= 0,
    32600 + zone,
    32700 + zone
  )
  
  
  pts <- st_as_sf(
    coords,
    coords = c(
      "lon",
      "lat"
    ),
    crs = 4326
  )
  
  
  pts_projected <- tryCatch(
    
    st_transform(
      pts,
      epsg
    ),
    
    error = function(e) NULL
  )
  
  
  if (is.null(pts_projected)) {
    
    return(
      NA_real_
    )
  }
  
  
  hull <- st_convex_hull(
    st_union(
      pts_projected
    )
  )
  
  
  as.numeric(
    st_area(
      hull
    )
  ) / 1e6
}


# ============================================================
# 12. DETECT COMMON RAW VARIABLES
# ============================================================

detect_columns <- function(dat) {
  
  list(
    
    operation_id = find_col(
      dat,
      c(
        "operationID",
        "operation_id",
        "operation",
        "fishingOperationID",
        "fishing_operation_id"
      )
    ),
    
    trip_id = find_col(
      dat,
      c(
        "tripID",
        "trip_id",
        "trip",
        "fishingTripID",
        "fishing_trip_id"
      )
    ),
    
    vessel_id = find_col(
      dat,
      c(
        "vesselID",
        "vessel_id",
        "vessel",
        "vesselCode",
        "vessel_code"
      )
    ),
    
    date = find_col(
      dat,
      c(
        "date",
        "operationDate",
        "operation_date",
        "fishingDate",
        "fishing_date",
        "setDate",
        "set_date"
      )
    ),
    
    lon = find_col(
      dat,
      c(
        "longitude",
        "lon",
        "operationLongitude",
        "operation_longitude"
      )
    ),
    
    lat = find_col(
      dat,
      c(
        "latitude",
        "lat",
        "operationLatitude",
        "operation_latitude"
      )
    ),
    
    
    # ---------------- SET ----------------
    
    set_lon1 = find_col(
      dat,
      c(
        "set_lon1",
        "setLon1",
        "setStartLongitude",
        "set_start_longitude",
        "settingStartLongitude",
        "setting_start_longitude"
      )
    ),
    
    set_lat1 = find_col(
      dat,
      c(
        "set_lat1",
        "setLat1",
        "setStartLatitude",
        "set_start_latitude",
        "settingStartLatitude",
        "setting_start_latitude"
      )
    ),
    
    set_lon2 = find_col(
      dat,
      c(
        "set_lon2",
        "setLon2",
        "setEndLongitude",
        "set_end_longitude",
        "settingEndLongitude",
        "setting_end_longitude"
      )
    ),
    
    set_lat2 = find_col(
      dat,
      c(
        "set_lat2",
        "setLat2",
        "setEndLatitude",
        "set_end_latitude",
        "settingEndLatitude",
        "setting_end_latitude"
      )
    ),
    
    
    # ---------------- HAUL ----------------
    
    haul_lon1 = find_col(
      dat,
      c(
        "haul_lon1",
        "haulLon1",
        "haulStartLongitude",
        "haul_start_longitude",
        "haulingStartLongitude",
        "hauling_start_longitude"
      )
    ),
    
    haul_lat1 = find_col(
      dat,
      c(
        "haul_lat1",
        "haulLat1",
        "haulStartLatitude",
        "haul_start_latitude",
        "haulingStartLatitude",
        "hauling_start_latitude"
      )
    ),
    
    haul_lon2 = find_col(
      dat,
      c(
        "haul_lon2",
        "haulLon2",
        "haulEndLongitude",
        "haul_end_longitude",
        "haulingEndLongitude",
        "hauling_end_longitude"
      )
    ),
    
    haul_lat2 = find_col(
      dat,
      c(
        "haul_lat2",
        "haulLat2",
        "haulEndLatitude",
        "haul_end_latitude",
        "haulingEndLatitude",
        "hauling_end_latitude"
      )
    )
    
  )
}


cols_0002 <- detect_columns(
  raw_0002
)

cols_0003 <- detect_columns(
  raw_0003
)

cols_0004 <- detect_columns(
  raw_0004
)


# ============================================================
# 13. SAVE COLUMN MAPPING
# ============================================================

column_mapping <- bind_rows(
  
  enframe(
    cols_0002,
    name = "standard_variable",
    value = "raw_variable"
  ) %>%
    mutate(
      dataset = "BYC_0002"
    ),
  
  enframe(
    cols_0003,
    name = "standard_variable",
    value = "raw_variable"
  ) %>%
    mutate(
      dataset = "BYC_0003"
    ),
  
  enframe(
    cols_0004,
    name = "standard_variable",
    value = "raw_variable"
  ) %>%
    mutate(
      dataset = "BYC_0004"
    )
  
) %>%
  
  select(
    dataset,
    standard_variable,
    raw_variable
  )


write_csv(
  column_mapping,
  file.path(
    OUT_DIR,
    "column_mapping.csv"
  )
)


cat(
  "\nDetected column mapping:\n\n"
)

print(
  column_mapping,
  n = Inf
)


# ============================================================
# 14. STANDARDISE PURSE-SEINE OPERATIONS
# ============================================================
#
# BYC_0002 is represented as an operation POINT.
#
# We do NOT create artificial SET/HAUL lines for purse seine.
# ============================================================

standardise_purse_seine <- function(
    dat,
    dataset,
    fishery,
    cols
) {
  
  n <- nrow(dat)
  
  
  operation_id <- pull_optional(
    dat,
    cols$operation_id
  )
  
  trip_id <- pull_optional(
    dat,
    cols$trip_id
  )
  
  vessel_id <- pull_optional(
    dat,
    cols$vessel_id
  )
  
  date_raw <- pull_optional(
    dat,
    cols$date
  )
  
  lon <- suppressWarnings(
    as.numeric(
      pull_optional(
        dat,
        cols$lon
      )
    )
  )
  
  lat <- suppressWarnings(
    as.numeric(
      pull_optional(
        dat,
        cols$lat
      )
    )
  )
  
  
  date <- parse_date_safe(
    date_raw
  )
  
  
  tibble(
    
    dataset = dataset,
    
    fishery = fishery,
    
    operation_id = as.character(
      operation_id
    ),
    
    trip_id = as.character(
      trip_id
    ),
    
    vessel_id = as.character(
      vessel_id
    ),
    
    date = date,
    
    year = year(
      date
    ),
    
    month = month(
      date
    ),
    
    
    representative_lon = lon,
    
    representative_lat = lat,
    
    
    set_lon1 = NA_real_,
    set_lat1 = NA_real_,
    
    set_lon2 = NA_real_,
    set_lat2 = NA_real_,
    
    haul_lon1 = NA_real_,
    haul_lat1 = NA_real_,
    
    haul_lon2 = NA_real_,
    haul_lat2 = NA_real_,
    
    
    set_length_km = NA_real_,
    
    haul_length_km = NA_real_,
    
    set_haul_displacement_km = NA_real_,
    
    mcp_area_km2 = NA_real_,
    
    
    excluded = FALSE,
    
    exclusion_reason = NA_character_
    
  )
}


# ============================================================
# 15. STANDARDISE LONGLINE OPERATIONS
# ============================================================

standardise_longline <- function(
    dat,
    dataset,
    fishery,
    cols
) {
  
  operation_id <- pull_optional(
    dat,
    cols$operation_id
  )
  
  trip_id <- pull_optional(
    dat,
    cols$trip_id
  )
  
  vessel_id <- pull_optional(
    dat,
    cols$vessel_id
  )
  
  date_raw <- pull_optional(
    dat,
    cols$date
  )
  
  
  set_lon1 <- suppressWarnings(
    as.numeric(
      pull_optional(
        dat,
        cols$set_lon1
      )
    )
  )
  
  set_lat1 <- suppressWarnings(
    as.numeric(
      pull_optional(
        dat,
        cols$set_lat1
      )
    )
  )
  
  set_lon2 <- suppressWarnings(
    as.numeric(
      pull_optional(
        dat,
        cols$set_lon2
      )
    )
  )
  
  set_lat2 <- suppressWarnings(
    as.numeric(
      pull_optional(
        dat,
        cols$set_lat2
      )
    )
  )
  
  
  haul_lon1 <- suppressWarnings(
    as.numeric(
      pull_optional(
        dat,
        cols$haul_lon1
      )
    )
  )
  
  haul_lat1 <- suppressWarnings(
    as.numeric(
      pull_optional(
        dat,
        cols$haul_lat1
      )
    )
  )
  
  haul_lon2 <- suppressWarnings(
    as.numeric(
      pull_optional(
        dat,
        cols$haul_lon2
      )
    )
  )
  
  haul_lat2 <- suppressWarnings(
    as.numeric(
      pull_optional(
        dat,
        cols$haul_lat2
      )
    )
  )
  
  
  date <- parse_date_safe(
    date_raw
  )
  
  
  # ----------------------------------------------------------
  # Representative position
  #
  # For now:
  # centroid of all available SET/HAUL endpoints.
  #
  # This is useful as an operation-level reference position.
  #
  # IMPORTANT:
  # the exact spatial representation used for eddy matching
  # will be defined explicitly in script 03.
  # ----------------------------------------------------------
  
  representative_lon <- rowMeans(
    
    cbind(
      set_lon1,
      set_lon2,
      haul_lon1,
      haul_lon2
    ),
    
    na.rm = TRUE
    
  )
  
  
  representative_lat <- rowMeans(
    
    cbind(
      set_lat1,
      set_lat2,
      haul_lat1,
      haul_lat2
    ),
    
    na.rm = TRUE
    
  )
  
  
  representative_lon[
    !is.finite(
      representative_lon
    )
  ] <- NA_real_
  
  
  representative_lat[
    !is.finite(
      representative_lat
    )
  ] <- NA_real_
  
  
  # ----------------------------------------------------------
  # Geometry metrics
  # ----------------------------------------------------------
  
  set_length_km <- distance_km(
    set_lon1,
    set_lat1,
    set_lon2,
    set_lat2
  )
  
  
  haul_length_km <- distance_km(
    haul_lon1,
    haul_lat1,
    haul_lon2,
    haul_lat2
  )
  
  
  # displacement between midpoints of SET and HAUL
  
  set_mid_lon <- rowMeans(
    cbind(
      set_lon1,
      set_lon2
    ),
    na.rm = TRUE
  )
  
  set_mid_lat <- rowMeans(
    cbind(
      set_lat1,
      set_lat2
    ),
    na.rm = TRUE
  )
  
  
  haul_mid_lon <- rowMeans(
    cbind(
      haul_lon1,
      haul_lon2
    ),
    na.rm = TRUE
  )
  
  haul_mid_lat <- rowMeans(
    cbind(
      haul_lat1,
      haul_lat2
    ),
    na.rm = TRUE
  )
  
  
  set_mid_lon[
    !is.finite(
      set_mid_lon
    )
  ] <- NA_real_
  
  set_mid_lat[
    !is.finite(
      set_mid_lat
    )
  ] <- NA_real_
  
  haul_mid_lon[
    !is.finite(
      haul_mid_lon
    )
  ] <- NA_real_
  
  haul_mid_lat[
    !is.finite(
      haul_mid_lat
    )
  ] <- NA_real_
  
  
  displacement <- distance_km(
    set_mid_lon,
    set_mid_lat,
    haul_mid_lon,
    haul_mid_lat
  )
  
  
  # ----------------------------------------------------------
  # MCP
  # ----------------------------------------------------------
  
  mcp_area <- pmap_dbl(
    
    list(
      set_lon1,
      set_lat1,
      set_lon2,
      set_lat2,
      haul_lon1,
      haul_lat1,
      haul_lon2,
      haul_lat2
    ),
    
    mcp_area_one
    
  )
  
  
  tibble(
    
    dataset = dataset,
    
    fishery = fishery,
    
    operation_id = as.character(
      operation_id
    ),
    
    trip_id = as.character(
      trip_id
    ),
    
    vessel_id = as.character(
      vessel_id
    ),
    
    date = date,
    
    year = year(
      date
    ),
    
    month = month(
      date
    ),
    
    
    representative_lon = representative_lon,
    
    representative_lat = representative_lat,
    
    
    set_lon1 = set_lon1,
    set_lat1 = set_lat1,
    
    set_lon2 = set_lon2,
    set_lat2 = set_lat2,
    
    
    haul_lon1 = haul_lon1,
    haul_lat1 = haul_lat1,
    
    haul_lon2 = haul_lon2,
    haul_lat2 = haul_lat2,
    
    
    set_length_km = set_length_km,
    
    haul_length_km = haul_length_km,
    
    set_haul_displacement_km = displacement,
    
    mcp_area_km2 = mcp_area,
    
    
    excluded = FALSE,
    
    exclusion_reason = NA_character_
    
  )
}


# ============================================================
# 16. BUILD THE THREE STANDARDISED DATASETS
# ============================================================

ops_0002 <- standardise_purse_seine(
  
  raw_0002,
  
  dataset = "BYC_0002",
  
  fishery = "purse_seine",
  
  cols = cols_0002
)


ops_0003 <- standardise_longline(
  
  raw_0003,
  
  dataset = "BYC_0003",
  
  fishery = "longline",
  
  cols = cols_0003
)


ops_0004 <- standardise_longline(
  
  raw_0004,
  
  dataset = "BYC_0004",
  
  fishery = "longline",
  
  cols = cols_0004
)


# ============================================================
# 17. SPATIAL-QC EXCLUSION REGISTER
# ============================================================
#
# These trips were identified during exploratory spatial QC
# as containing clearly implausible operation geometries.
#
# IMPORTANT:
#
# They are NOT deleted here.
#
# They remain in the master dataset with:
#
#     excluded = TRUE
#
# so that exclusion is:
#
#     - explicit
#     - reproducible
#     - reversible
#     - auditable
#
# ============================================================

exclusion_register <- tribble(
  
  ~dataset,
  ~trip_id,
  ~exclusion_reason,
  
  "BYC_0004",
  "68",
  "Spatial QC: implausible SET/HAUL geometries",
  
  "BYC_0004",
  "69",
  "Spatial QC: implausible SET/HAUL geometries",
  
  "BYC_0004",
  "140",
  "Spatial QC: implausible SET/HAUL geometries",
  
  "BYC_0004",
  "168",
  "Spatial QC: implausible SET/HAUL geometries"
  
)


write_csv(
  exclusion_register,
  file.path(
    OUT_DIR,
    "exclusion_register.csv"
  )
)


# ============================================================
# 18. APPLY EXCLUSION FLAGS
# ============================================================

ops_0004 <- ops_0004 %>%
  
  left_join(
    exclusion_register,
    by = c(
      "dataset",
      "trip_id"
    ),
    suffix = c(
      "",
      "_qc"
    )
  ) %>%
  
  mutate(
    
    excluded = !is.na(
      exclusion_reason_qc
    ),
    
    exclusion_reason = coalesce(
      exclusion_reason_qc,
      exclusion_reason
    )
    
  ) %>%
  
  select(
    -exclusion_reason_qc
  )


# ============================================================
# 19. COMBINE DATASETS
# ============================================================

operations_all <- bind_rows(
  
  ops_0002,
  ops_0003,
  ops_0004
  
) %>%
  
  arrange(
    dataset,
    date,
    trip_id,
    operation_id
  )


# ============================================================
# 20. BASIC VALIDATION
# ============================================================

cat(
  "\n====================================================\n",
  "STANDARDISED DATA VALIDATION\n",
  "====================================================\n\n"
)


# ------------------------------------------------------------
# Number of rows
# ------------------------------------------------------------

row_summary <- operations_all %>%
  
  count(
    dataset,
    fishery,
    name = "operations"
  )


print(
  row_summary
)


# ------------------------------------------------------------
# Exclusions
# ------------------------------------------------------------

exclusion_summary <- operations_all %>%
  
  group_by(
    dataset
  ) %>%
  
  summarise(
    
    operations = n(),
    
    excluded_operations = sum(
      excluded
    ),
    
    included_operations = sum(
      !excluded
    ),
    
    excluded_percent =
      100 *
      mean(
        excluded
      ),
    
    .groups = "drop"
  )


cat(
  "\nExclusion summary:\n\n"
)

print(
  exclusion_summary
)


# ------------------------------------------------------------
# Temporal coverage
# ------------------------------------------------------------

temporal_summary <- operations_all %>%
  
  group_by(
    dataset
  ) %>%
  
  summarise(
    
    first_date = min(
      date,
      na.rm = TRUE
    ),
    
    last_date = max(
      date,
      na.rm = TRUE
    ),
    
    first_year = min(
      year,
      na.rm = TRUE
    ),
    
    last_year = max(
      year,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


cat(
  "\nTemporal coverage:\n\n"
)

print(
  temporal_summary
)


# ------------------------------------------------------------
# Spatial coverage
# ------------------------------------------------------------

spatial_summary <- operations_all %>%
  
  filter(
    !excluded
  ) %>%
  
  group_by(
    dataset
  ) %>%
  
  summarise(
    
    min_lon = min(
      representative_lon,
      na.rm = TRUE
    ),
    
    max_lon = max(
      representative_lon,
      na.rm = TRUE
    ),
    
    min_lat = min(
      representative_lat,
      na.rm = TRUE
    ),
    
    max_lat = max(
      representative_lat,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


cat(
  "\nSpatial coverage (excluding QC exclusions):\n\n"
)

print(
  spatial_summary
)


# ============================================================
# 21. CHECK IMPOSSIBLE COORDINATES
# ============================================================

coordinate_check <- operations_all %>%
  
  mutate(
    
    invalid_representative_coordinate =
      
      !is.na(
        representative_lon
      ) &
      
      !is.na(
        representative_lat
      ) &
      
      (
        representative_lon < -180 |
          representative_lon > 180 |
          representative_lat < -90 |
          representative_lat > 90
      )
    
  )


n_invalid <- sum(
  coordinate_check$invalid_representative_coordinate,
  na.rm = TRUE
)


cat(
  "\nInvalid representative coordinates:",
  n_invalid,
  "\n"
)


if (n_invalid > 0) {
  
  warning(
    paste(
      n_invalid,
      "operations contain impossible coordinates."
    )
  )
}


operations_all <- coordinate_check


# ============================================================
# 22. CHECK DUPLICATE OPERATION IDS
# ============================================================
#
# We check within dataset because operation IDs do not need
# to be globally unique between IEO datasets.
# ============================================================

duplicates <- operations_all %>%
  
  filter(
    !is.na(
      operation_id
    )
  ) %>%
  
  count(
    dataset,
    operation_id
  ) %>%
  
  filter(
    n > 1
  )


cat(
  "\nDuplicated operation IDs:\n"
)

print(
  duplicates,
  n = Inf
)


# ============================================================
# 23. MODEL-ELIGIBLE FLAG
# ============================================================
#
# This is deliberately separate from `excluded`.
#
# excluded:
#     manual / QC decision
#
# model_eligible:
#     currently has the minimum information required for
#     spatial modelling.
#
# ============================================================

operations_all <- operations_all %>%
  
  mutate(
    
    has_date =
      !is.na(
        date
      ),
    
    has_position =
      is.finite(
        representative_lon
      ) &
      is.finite(
        representative_lat
      ),
    
    model_eligible =
      !excluded &
      has_date &
      has_position &
      !invalid_representative_coordinate
    
  )


# ============================================================
# 24. CREATE MODELLING VIEW
# ============================================================
#
# IMPORTANT:
#
# operations_all
#     = master dataset, including excluded records
#
# operations_model
#     = current analysis-ready view
#
# We preserve BOTH.
# ============================================================

operations_model <- operations_all %>%
  
  filter(
    model_eligible
  )


# ============================================================
# 25. FINAL SUMMARIES
# ============================================================

final_summary <- operations_all %>%
  
  group_by(
    dataset,
    fishery
  ) %>%
  
  summarise(
    
    N_operations = n(),
    
    N_trips = n_distinct(
      trip_id,
      na.rm = TRUE
    ),
    
    N_vessels = n_distinct(
      vessel_id,
      na.rm = TRUE
    ),
    
    N_excluded = sum(
      excluded
    ),
    
    N_missing_date = sum(
      !has_date
    ),
    
    N_missing_position = sum(
      !has_position
    ),
    
    N_model_eligible = sum(
      model_eligible
    ),
    
    first_year = min(
      year,
      na.rm = TRUE
    ),
    
    last_year = max(
      year,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


cat(
  "\n\nFINAL SUMMARY\n",
  "-------------\n"
)

print(
  final_summary
)


# ============================================================
# 26. WRITE OUTPUTS
# ============================================================

# ------------------------------------------------------------
# Master CSV
# ------------------------------------------------------------

write_csv(
  operations_all,
  file.path(
    OUT_DIR,
    "operations_standardised_all.csv"
  ),
  na = ""
)


# ------------------------------------------------------------
# Model-eligible CSV
# ------------------------------------------------------------

write_csv(
  operations_model,
  file.path(
    OUT_DIR,
    "operations_standardised_model.csv"
  ),
  na = ""
)


# ------------------------------------------------------------
# RDS versions
#
# Useful because Date classes are preserved exactly.
# ------------------------------------------------------------

saveRDS(
  operations_all,
  file.path(
    OUT_DIR,
    "operations_standardised_all.rds"
  )
)


saveRDS(
  operations_model,
  file.path(
    OUT_DIR,
    "operations_standardised_model.rds"
  )
)


# ------------------------------------------------------------
# Summaries
# ------------------------------------------------------------

write_csv(
  final_summary,
  file.path(
    OUT_DIR,
    "standardisation_summary.csv"
  )
)


write_csv(
  duplicates,
  file.path(
    OUT_DIR,
    "duplicate_operation_ids.csv"
  )
)


# ============================================================
# 27. SAVE EXCLUDED OPERATIONS SEPARATELY
# ============================================================
#
# This is useful for quickly checking exactly what has been
# set aside without searching the master table.
# ============================================================

excluded_operations <- operations_all %>%
  
  filter(
    excluded
  )


write_csv(
  excluded_operations,
  file.path(
    OUT_DIR,
    "excluded_operations.csv"
  ),
  na = ""
)


# ============================================================
# 28. DATA DICTIONARY
# ============================================================

data_dictionary <- tribble(
  
  ~variable,
  ~description,
  
  "dataset",
  "Original IEO dataset identifier",
  
  "fishery",
  "Standardised fishery type",
  
  "operation_id",
  "Fishing-operation identifier",
  
  "trip_id",
  "Fishing-trip identifier",
  
  "vessel_id",
  "Vessel identifier",
  
  "date",
  "Fishing-operation date",
  
  "year",
  "Calendar year",
  
  "month",
  "Calendar month",
  
  "representative_lon",
  "Representative longitude of operation",
  
  "representative_lat",
  "Representative latitude of operation",
  
  "set_lon1",
  "Longitude of first SET endpoint",
  
  "set_lat1",
  "Latitude of first SET endpoint",
  
  "set_lon2",
  "Longitude of second SET endpoint",
  
  "set_lat2",
  "Latitude of second SET endpoint",
  
  "haul_lon1",
  "Longitude of first HAUL endpoint",
  
  "haul_lat1",
  "Latitude of first HAUL endpoint",
  
  "haul_lon2",
  "Longitude of second HAUL endpoint",
  
  "haul_lat2",
  "Latitude of second HAUL endpoint",
  
  "set_length_km",
  "Great-circle distance between SET endpoints",
  
  "haul_length_km",
  "Great-circle distance between HAUL endpoints",
  
  "set_haul_displacement_km",
  "Distance between SET and HAUL midpoints",
  
  "mcp_area_km2",
  "Convex-hull area defined by available SET/HAUL endpoints",
  
  "excluded",
  "TRUE if operation was manually set aside following QC",
  
  "exclusion_reason",
  "Reason for manual/QC exclusion",
  
  "invalid_representative_coordinate",
  "TRUE if representative coordinate lies outside valid longitude/latitude limits",
  
  "has_date",
  "TRUE if operation has a usable date",
  
  "has_position",
  "TRUE if operation has a usable representative position",
  
  "model_eligible",
  "TRUE if operation currently passes minimum requirements for modelling"
  
)


write_csv(
  data_dictionary,
  file.path(
    OUT_DIR,
    "data_dictionary.csv"
  )
)


# ============================================================
# 29. SESSION INFORMATION
# ============================================================

capture.output(
  
  sessionInfo(),
  
  file = file.path(
    OUT_DIR,
    "sessionInfo.txt"
  )
  
)


# ============================================================
# 30. FINAL MESSAGE
# ============================================================

cat(
  "\n\n====================================================\n",
  "01 — STANDARDISATION COMPLETE\n",
  "====================================================\n\n",
  sep = ""
)


cat(
  "MASTER DATASET:\n",
  file.path(
    OUT_DIR,
    "operations_standardised_all.csv"
  ),
  "\n\n",
  sep = ""
)


cat(
  "MODELLING VIEW:\n",
  file.path(
    OUT_DIR,
    "operations_standardised_model.csv"
  ),
  "\n\n",
  sep = ""
)


cat(
  "IMPORTANT:\n",
  "operations_standardised_all contains ALL operations.\n",
  "QC exclusions are retained with excluded = TRUE.\n\n",
  sep = ""
)


cat(
  "Before modelling, exclusions must always be explicit:\n\n",
  "    filter(!excluded)\n\n",
  sep = ""
)


cat(
  "NEXT STEP:\n",
  "02_prepare_eddy_data.R\n",
  sep = ""
)


cat(
  "\n====================================================\n"
)