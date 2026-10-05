# ============================================================
# REDUCE — MESOSCALE BYCATCH
# 00_explore_raw_data.R
#
# PURPOSE
# -------
# Exploratory audit of the raw IEO datasets before any
# standardisation or mesoscale matching.
#
# DATASETS
# --------
# BYC_0002 = purse seine
# BYC_0003 = longline
# BYC_0004 = longline
#
# IMPORTANT
# ---------
# This script:
#   - NEVER modifies raw data
#   - NEVER excludes observations
#   - NEVER creates modelling data
#
# It only describes and flags potentially problematic records.
#
# OUTPUT
# ------
# 00inputOutput/00output/00_raw_exploration/
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

packages <- c(
  "here",
  "readr",
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "lubridate",
  "ggplot2",
  "sf",
  "geosphere",
  "rnaturalearth"
)


missing_packages <- packages[
  !vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]


if (length(missing_packages) > 0) {
  
  stop(
    paste0(
      "Missing packages: ",
      paste(
        missing_packages,
        collapse = ", "
      ),
      "\n\nInstall them before running this script."
    ),
    call. = FALSE
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
# 2. PATHS
# ============================================================

INPUT_IEO <- here::here(
  "00inputOutput",
  "00input",
  "00IEO"
)


OUTPUT_00 <- here::here(
  "00inputOutput",
  "00output",
  "00_raw_exploration"
)


FIG_DIR <- file.path(
  OUTPUT_00,
  "figures"
)


dir.create(
  OUTPUT_00,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  FIG_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. RAW FILES
# ============================================================

FILE_0002_FISHING <- file.path(
  INPUT_IEO,
  "BYC_0002",
  "fishingOperations",
  "Ouled-Cheikh_BYC_0002_PS_MUL_IEO_fishingOperations(in).csv"
)


FILE_0002_BYCATCH <- file.path(
  INPUT_IEO,
  "BYC_0002",
  "bycatchAggregated",
  "Ouled-Cheikh_BYC_0002_PS_MUL_IEO_bycatchAggregated(in).csv"
)


FILE_0002_INDIVIDUALS <- file.path(
  INPUT_IEO,
  "BYC_0002",
  "bycatchSampledIndividuals",
  "Ouled-Cheikh_BYC_0002_PS_MUL_IEO_bycatchSampledIndividuals(in).csv"
)


FILE_0003_FISHING <- file.path(
  INPUT_IEO,
  "BYC_0003",
  "fishingOperations",
  "Ouled-Cheikh_BYC_0003_LL_MUL_IEO_fishingOperations(in).csv"
)


FILE_0003_BYCATCH <- file.path(
  INPUT_IEO,
  "BYC_0003",
  "bycatch",
  "Ouled-Cheikh_BYC_0003_LL_MUL_IEO_bycatch(in).csv"
)


FILE_0004_FISHING <- file.path(
  INPUT_IEO,
  "BYC_0004",
  "fishingOperations",
  "Ouled-Cheikh_BYC_0004_LL_MUL_IEO_fishingOperations(in).csv"
)


FILE_0004_BYCATCH <- file.path(
  INPUT_IEO,
  "BYC_0004",
  "bycatch",
  "Ouled-Cheikh_BYC_0004_LL_MUL_IEO_bycatch(in).csv"
)


# ============================================================
# 4. CHECK FILES
# ============================================================

analysis_files <- c(
  
  FILE_0002_FISHING,
  FILE_0002_BYCATCH,
  FILE_0002_INDIVIDUALS,
  
  FILE_0003_FISHING,
  FILE_0003_BYCATCH,
  
  FILE_0004_FISHING,
  FILE_0004_BYCATCH
)


if (!all(file.exists(analysis_files))) {
  
  stop(
    paste0(
      "One or more required raw files are missing.\n\n",
      paste(
        analysis_files[
          !file.exists(analysis_files)
        ],
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 5. READ RAW DATA
# ============================================================

cat(
  "\n====================================================\n",
  "READING RAW IEO DATA\n",
  "====================================================\n\n",
  sep = ""
)


ops_0002_raw <- readr::read_csv(
  FILE_0002_FISHING,
  show_col_types = FALSE
)


bycatch_0002_raw <- readr::read_csv(
  FILE_0002_BYCATCH,
  show_col_types = FALSE
)


individuals_0002_raw <- readr::read_csv(
  FILE_0002_INDIVIDUALS,
  show_col_types = FALSE
)


ops_0003_raw <- readr::read_csv(
  FILE_0003_FISHING,
  show_col_types = FALSE
)


bycatch_0003_raw <- readr::read_csv(
  FILE_0003_BYCATCH,
  show_col_types = FALSE
)


ops_0004_raw <- readr::read_csv(
  FILE_0004_FISHING,
  show_col_types = FALSE
)


bycatch_0004_raw <- readr::read_csv(
  FILE_0004_BYCATCH,
  show_col_types = FALSE
)


cat(
  "BYC_0002 fishing operations: ",
  nrow(ops_0002_raw),
  "\n",
  sep = ""
)

cat(
  "BYC_0003 fishing operations: ",
  nrow(ops_0003_raw),
  "\n",
  sep = ""
)

cat(
  "BYC_0004 fishing operations: ",
  nrow(ops_0004_raw),
  "\n\n",
  sep = ""
)


# ============================================================
# 6. SAVE RAW COLUMN INVENTORY
#
# This is extremely useful later when we build script 01.
# ============================================================

column_inventory <- bind_rows(
  
  tibble(
    dataset = "BYC_0002",
    table = "fishingOperations",
    variable = names(ops_0002_raw)
  ),
  
  tibble(
    dataset = "BYC_0002",
    table = "bycatchAggregated",
    variable = names(bycatch_0002_raw)
  ),
  
  tibble(
    dataset = "BYC_0002",
    table = "bycatchSampledIndividuals",
    variable = names(individuals_0002_raw)
  ),
  
  tibble(
    dataset = "BYC_0003",
    table = "fishingOperations",
    variable = names(ops_0003_raw)
  ),
  
  tibble(
    dataset = "BYC_0003",
    table = "bycatch",
    variable = names(bycatch_0003_raw)
  ),
  
  tibble(
    dataset = "BYC_0004",
    table = "fishingOperations",
    variable = names(ops_0004_raw)
  ),
  
  tibble(
    dataset = "BYC_0004",
    table = "bycatch",
    variable = names(bycatch_0004_raw)
  )
)


write_csv(
  column_inventory,
  file.path(
    OUTPUT_00,
    "raw_column_inventory.csv"
  )
)


# ============================================================
# 7. RAW TABLE INVENTORY
# ============================================================

table_inventory <- tibble(
  
  dataset = c(
    "BYC_0002",
    "BYC_0002",
    "BYC_0002",
    "BYC_0003",
    "BYC_0003",
    "BYC_0004",
    "BYC_0004"
  ),
  
  table = c(
    "fishingOperations",
    "bycatchAggregated",
    "bycatchSampledIndividuals",
    "fishingOperations",
    "bycatch",
    "fishingOperations",
    "bycatch"
  ),
  
  rows = c(
    nrow(ops_0002_raw),
    nrow(bycatch_0002_raw),
    nrow(individuals_0002_raw),
    nrow(ops_0003_raw),
    nrow(bycatch_0003_raw),
    nrow(ops_0004_raw),
    nrow(bycatch_0004_raw)
  ),
  
  columns = c(
    ncol(ops_0002_raw),
    ncol(bycatch_0002_raw),
    ncol(individuals_0002_raw),
    ncol(ops_0003_raw),
    ncol(bycatch_0003_raw),
    ncol(ops_0004_raw),
    ncol(bycatch_0004_raw)
  )
)


write_csv(
  table_inventory,
  file.path(
    OUTPUT_00,
    "raw_table_inventory.csv"
  )
)


cat(
  "\nRAW TABLE INVENTORY\n\n"
)

print(
  table_inventory
)


# ============================================================
# 8. REQUIRED VARIABLES
# ============================================================
#
# We now check the variables we already know are important.
#
# No guessing or automatic renaming is done.
# ============================================================

check_variables <- function(
    dat,
    required,
    dataset
) {
  
  missing <- setdiff(
    required,
    names(dat)
  )
  
  
  if (length(missing) > 0) {
    
    cat(
      "\n[WARNING] ",
      dataset,
      " is missing:\n",
      paste(
        paste0(
          "  - ",
          missing
        ),
        collapse = "\n"
      ),
      "\n",
      sep = ""
    )
  }
  
  
  invisible(
    missing
  )
}


# Variables common to the operation tables

check_variables(
  ops_0002_raw,
  c(
    "operationID",
    "tripID"
  ),
  "BYC_0002 fishingOperations"
)


check_variables(
  ops_0003_raw,
  c(
    "operationID",
    "tripID",
    "set_lon1",
    "set_lat1",
    "set_lon2",
    "set_lat2",
    "haul_lon1",
    "haul_lat1",
    "haul_lon2",
    "haul_lat2"
  ),
  "BYC_0003 fishingOperations"
)


check_variables(
  ops_0004_raw,
  c(
    "operationID",
    "tripID",
    "set_lon1",
    "set_lat1",
    "set_lon2",
    "set_lat2",
    "haul_lon1",
    "haul_lat1",
    "haul_lon2",
    "haul_lat2"
  ),
  "BYC_0004 fishingOperations"
)


# ============================================================
# 9. HELPER: FIRST EXISTING COLUMN
#
# Only used for exploratory summaries where the raw datasets
# may use slightly different names.
# ============================================================

first_existing <- function(
    dat,
    candidates
) {
  
  found <- candidates[
    candidates %in% names(dat)
  ]
  
  
  if (length(found) == 0) {
    return(
      NA_character_
    )
  }
  
  
  found[1]
}


# ============================================================
# 10. IDENTIFY BASIC VARIABLES
# ============================================================

get_basic_columns <- function(dat) {
  
  list(
    
    operation = first_existing(
      dat,
      c(
        "operationID",
        "operation_id"
      )
    ),
    
    trip = first_existing(
      dat,
      c(
        "tripID",
        "trip_id"
      )
    ),
    
    vessel = first_existing(
      dat,
      c(
        "vesselID",
        "vessel_id"
      )
    ),
    
    date = first_existing(
      dat,
      c(
        "fishing_date",
        "date",
        "operationDate",
        "setDate"
      )
    ),
    
    year = first_existing(
      dat,
      c(
        "year",
        "fishingYear"
      )
    )
  )
}


basic_0002 <- get_basic_columns(
  ops_0002_raw
)

basic_0003 <- get_basic_columns(
  ops_0003_raw
)

basic_0004 <- get_basic_columns(
  ops_0004_raw
)


# ============================================================
# 11. DATASET INVENTORY
# ============================================================

safe_n_distinct <- function(
    dat,
    variable
) {
  
  if (is.na(variable)) {
    return(
      NA_integer_
    )
  }
  
  
  dplyr::n_distinct(
    dat[[variable]],
    na.rm = TRUE
  )
}


dataset_inventory <- tibble(
  
  dataset = c(
    "BYC_0002",
    "BYC_0003",
    "BYC_0004"
  ),
  
  fishery = c(
    "Purse seine",
    "Longline",
    "Longline"
  ),
  
  operations = c(
    nrow(ops_0002_raw),
    nrow(ops_0003_raw),
    nrow(ops_0004_raw)
  ),
  
  unique_operationID = c(
    safe_n_distinct(
      ops_0002_raw,
      basic_0002$operation
    ),
    safe_n_distinct(
      ops_0003_raw,
      basic_0003$operation
    ),
    safe_n_distinct(
      ops_0004_raw,
      basic_0004$operation
    )
  ),
  
  trips = c(
    safe_n_distinct(
      ops_0002_raw,
      basic_0002$trip
    ),
    safe_n_distinct(
      ops_0003_raw,
      basic_0003$trip
    ),
    safe_n_distinct(
      ops_0004_raw,
      basic_0004$trip
    )
  ),
  
  vessels = c(
    safe_n_distinct(
      ops_0002_raw,
      basic_0002$vessel
    ),
    safe_n_distinct(
      ops_0003_raw,
      basic_0003$vessel
    ),
    safe_n_distinct(
      ops_0004_raw,
      basic_0004$vessel
    )
  )
)


write_csv(
  dataset_inventory,
  file.path(
    OUTPUT_00,
    "dataset_inventory.csv"
  )
)


cat(
  "\nDATASET INVENTORY\n\n"
)

print(
  dataset_inventory
)


# ============================================================
# 12. BYCATCH → OPERATION-ID AUDIT
#
# Before joining anything we need to know whether bycatch
# records refer to valid fishing operations.
# ============================================================

audit_bycatch_ids <- function(
    operations,
    bycatch,
    dataset
) {
  
  if (
    !"operationID" %in% names(operations) ||
    !"operationID" %in% names(bycatch)
  ) {
    
    return(
      tibble(
        dataset = dataset,
        bycatch_rows = nrow(bycatch),
        bycatch_unique_operations = NA_integer_,
        operations_with_bycatch_record = NA_integer_,
        bycatch_operation_ids_not_in_fishing_operations = NA_integer_
      )
    )
  }
  
  
  bycatch_ids <- unique(
    bycatch$operationID[
      !is.na(
        bycatch$operationID
      )
    ]
  )
  
  
  operation_ids <- unique(
    operations$operationID[
      !is.na(
        operations$operationID
      )
    ]
  )
  
  
  tibble(
    
    dataset = dataset,
    
    bycatch_rows = nrow(
      bycatch
    ),
    
    bycatch_unique_operations = length(
      bycatch_ids
    ),
    
    operations_with_bycatch_record = sum(
      operation_ids %in% bycatch_ids
    ),
    
    bycatch_operation_ids_not_in_fishing_operations = sum(
      !bycatch_ids %in% operation_ids
    )
  )
}


bycatch_id_audit <- bind_rows(
  
  audit_bycatch_ids(
    ops_0002_raw,
    bycatch_0002_raw,
    "BYC_0002"
  ),
  
  audit_bycatch_ids(
    ops_0003_raw,
    bycatch_0003_raw,
    "BYC_0003"
  ),
  
  audit_bycatch_ids(
    ops_0004_raw,
    bycatch_0004_raw,
    "BYC_0004"
  )
)


write_csv(
  bycatch_id_audit,
  file.path(
    OUTPUT_00,
    "bycatch_operationID_audit.csv"
  )
)


cat(
  "\nBYCATCH / OPERATION-ID AUDIT\n\n"
)

print(
  bycatch_id_audit
)


# ============================================================
# 13. COORDINATE VALIDITY HELPER
# ============================================================

valid_lon <- function(x) {
  
  is.finite(x) &
    x >= -180 &
    x <= 180
}


valid_lat <- function(x) {
  
  is.finite(x) &
    x >= -90 &
    x <= 90
}


# ============================================================
# 14. DISTANCE HELPER
# ============================================================

distance_km <- function(
    lon1,
    lat1,
    lon2,
    lat2
) {
  
  valid <- valid_lon(lon1) &
    valid_lat(lat1) &
    valid_lon(lon2) &
    valid_lat(lat2)
  
  
  out <- rep(
    NA_real_,
    length(lon1)
  )
  
  
  if (any(valid)) {
    
    out[valid] <- geosphere::distHaversine(
      
      cbind(
        lon1[valid],
        lat1[valid]
      ),
      
      cbind(
        lon2[valid],
        lat2[valid]
      )
      
    ) / 1000
  }
  
  
  out
}


# ============================================================
# 15. MCP AREA
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
      valid_lon(lon),
      valid_lat(lat)
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
  
  
  pts <- sf::st_as_sf(
    coords,
    coords = c(
      "lon",
      "lat"
    ),
    crs = 4326
  )
  
  
  pts <- tryCatch(
    
    sf::st_transform(
      pts,
      epsg
    ),
    
    error = function(e) NULL
  )
  
  
  if (is.null(pts)) {
    
    return(
      NA_real_
    )
  }
  
  
  hull <- sf::st_convex_hull(
    sf::st_union(
      pts
    )
  )
  
  
  as.numeric(
    sf::st_area(
      hull
    )
  ) / 1e6
}


# ============================================================
# 16. PREPARE LONGLINE GEOMETRY
# ============================================================

prepare_longline_geometry <- function(
    dat,
    dataset
) {
  
  required <- c(
    "set_lon1",
    "set_lat1",
    "set_lon2",
    "set_lat2",
    "haul_lon1",
    "haul_lat1",
    "haul_lon2",
    "haul_lat2"
  )
  
  
  missing <- setdiff(
    required,
    names(dat)
  )
  
  
  if (length(missing) > 0) {
    
    stop(
      paste0(
        dataset,
        " is missing required geometry columns:\n",
        paste(
          missing,
          collapse = ", "
        )
      )
    )
  }
  
  
  out <- dat %>%
    
    mutate(
      
      dataset = dataset,
      
      set_length_km = distance_km(
        set_lon1,
        set_lat1,
        set_lon2,
        set_lat2
      ),
      
      haul_length_km = distance_km(
        haul_lon1,
        haul_lat1,
        haul_lon2,
        haul_lat2
      )
    )
  
  
  # ----------------------------------------------------------
  # SET midpoint
  # ----------------------------------------------------------
  
  set_mid_lon <- rowMeans(
    cbind(
      out$set_lon1,
      out$set_lon2
    ),
    na.rm = TRUE
  )
  
  
  set_mid_lat <- rowMeans(
    cbind(
      out$set_lat1,
      out$set_lat2
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
  
  
  # ----------------------------------------------------------
  # HAUL midpoint
  # ----------------------------------------------------------
  
  haul_mid_lon <- rowMeans(
    cbind(
      out$haul_lon1,
      out$haul_lon2
    ),
    na.rm = TRUE
  )
  
  
  haul_mid_lat <- rowMeans(
    cbind(
      out$haul_lat1,
      out$haul_lat2
    ),
    na.rm = TRUE
  )
  
  
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
  
  
  out$set_mid_lon <- set_mid_lon
  out$set_mid_lat <- set_mid_lat
  
  out$haul_mid_lon <- haul_mid_lon
  out$haul_mid_lat <- haul_mid_lat
  
  
  # ----------------------------------------------------------
  # SET–HAUL displacement
  # ----------------------------------------------------------
  
  out$set_haul_displacement_km <- distance_km(
    out$set_mid_lon,
    out$set_mid_lat,
    out$haul_mid_lon,
    out$haul_mid_lat
  )
  
  
  # ----------------------------------------------------------
  # MCP
  # ----------------------------------------------------------
  
  out$mcp_area_km2 <- purrr::pmap_dbl(
    
    list(
      out$set_lon1,
      out$set_lat1,
      out$set_lon2,
      out$set_lat2,
      out$haul_lon1,
      out$haul_lat1,
      out$haul_lon2,
      out$haul_lat2
    ),
    
    function(
    set_lon1,
    set_lat1,
    set_lon2,
    set_lat2,
    haul_lon1,
    haul_lat1,
    haul_lon2,
    haul_lat2
    ) {
      
      mcp_area_one(
        set_lon1,
        set_lat1,
        set_lon2,
        set_lat2,
        haul_lon1,
        haul_lat1,
        haul_lon2,
        haul_lat2
      )
    }
  )
  
  
  out
}


longline_0003 <- prepare_longline_geometry(
  ops_0003_raw,
  "BYC_0003"
)


longline_0004 <- prepare_longline_geometry(
  ops_0004_raw,
  "BYC_0004"
)


longline_all <- bind_rows(
  longline_0003,
  longline_0004
)


# ============================================================
# 17. GEOMETRY SUMMARY
# ============================================================

geometry_summary <- longline_all %>%
  
  group_by(
    dataset
  ) %>%
  
  summarise(
    
    operations = n(),
    
    set_length_median_km = median(
      set_length_km,
      na.rm = TRUE
    ),
    
    set_length_p95_km = quantile(
      set_length_km,
      0.95,
      na.rm = TRUE
    ),
    
    set_length_p99_km = quantile(
      set_length_km,
      0.99,
      na.rm = TRUE
    ),
    
    haul_length_median_km = median(
      haul_length_km,
      na.rm = TRUE
    ),
    
    haul_length_p95_km = quantile(
      haul_length_km,
      0.95,
      na.rm = TRUE
    ),
    
    haul_length_p99_km = quantile(
      haul_length_km,
      0.99,
      na.rm = TRUE
    ),
    
    displacement_median_km = median(
      set_haul_displacement_km,
      na.rm = TRUE
    ),
    
    displacement_p95_km = quantile(
      set_haul_displacement_km,
      0.95,
      na.rm = TRUE
    ),
    
    displacement_p99_km = quantile(
      set_haul_displacement_km,
      0.99,
      na.rm = TRUE
    ),
    
    mcp_median_km2 = median(
      mcp_area_km2,
      na.rm = TRUE
    ),
    
    mcp_p95_km2 = quantile(
      mcp_area_km2,
      0.95,
      na.rm = TRUE
    ),
    
    mcp_p99_km2 = quantile(
      mcp_area_km2,
      0.99,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


write_csv(
  geometry_summary,
  file.path(
    OUTPUT_00,
    "longline_geometry_summary.csv"
  )
)


cat(
  "\nLONGLINE GEOMETRY SUMMARY\n\n"
)

print(
  geometry_summary,
  width = Inf
)


# ============================================================
# 18. P99 QC THRESHOLDS
#
# Thresholds are calculated separately for each dataset.
#
# IMPORTANT:
# Being above P99 does NOT mean an operation is automatically
# wrong. It means "inspect this operation".
# ============================================================

qc_thresholds <- longline_all %>%
  
  group_by(
    dataset
  ) %>%
  
  summarise(
    
    p99_set_length_km = quantile(
      set_length_km,
      0.99,
      na.rm = TRUE
    ),
    
    p99_haul_length_km = quantile(
      haul_length_km,
      0.99,
      na.rm = TRUE
    ),
    
    p99_displacement_km = quantile(
      set_haul_displacement_km,
      0.99,
      na.rm = TRUE
    ),
    
    p99_mcp_area_km2 = quantile(
      mcp_area_km2,
      0.99,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


write_csv(
  qc_thresholds,
  file.path(
    OUTPUT_00,
    "spatial_qc_thresholds.csv"
  )
)


# ============================================================
# 19. FLAG P99 OPERATIONS
# ============================================================

longline_qc <- longline_all %>%
  
  left_join(
    qc_thresholds,
    by = "dataset"
  ) %>%
  
  mutate(
    
    flag_set_length =
      is.finite(
        set_length_km
      ) &
      set_length_km >
      p99_set_length_km,
    
    flag_haul_length =
      is.finite(
        haul_length_km
      ) &
      haul_length_km >
      p99_haul_length_km,
    
    flag_displacement =
      is.finite(
        set_haul_displacement_km
      ) &
      set_haul_displacement_km >
      p99_displacement_km,
    
    flag_mcp =
      is.finite(
        mcp_area_km2
      ) &
      mcp_area_km2 >
      p99_mcp_area_km2,
    
    qc_flag =
      flag_set_length |
      flag_haul_length |
      flag_displacement |
      flag_mcp,
    
    n_qc_flags =
      as.integer(
        flag_set_length
      ) +
      as.integer(
        flag_haul_length
      ) +
      as.integer(
        flag_displacement
      ) +
      as.integer(
        flag_mcp
      )
  )


# ============================================================
# 20. SAVE FLAGGED OPERATIONS
# ============================================================

id_columns <- intersect(
  
  c(
    "dataset",
    "operationID",
    "tripID",
    "vesselID",
    "year"
  ),
  
  names(
    longline_qc
  )
)


spatial_qc_operations <- longline_qc %>%
  
  filter(
    qc_flag
  ) %>%
  
  select(
    
    all_of(
      id_columns
    ),
    
    set_lon1,
    set_lat1,
    set_lon2,
    set_lat2,
    
    haul_lon1,
    haul_lat1,
    haul_lon2,
    haul_lat2,
    
    set_length_km,
    haul_length_km,
    set_haul_displacement_km,
    mcp_area_km2,
    
    flag_set_length,
    flag_haul_length,
    flag_displacement,
    flag_mcp,
    
    n_qc_flags
  ) %>%
  
  arrange(
    dataset,
    desc(
      n_qc_flags
    ),
    desc(
      set_haul_displacement_km
    )
  )


write_csv(
  spatial_qc_operations,
  file.path(
    OUTPUT_00,
    "spatial_qc_operations.csv"
  )
)


cat(
  "\nP99 FLAGGED OPERATIONS\n\n"
)

print(
  spatial_qc_operations,
  n = Inf,
  width = Inf
)


# ============================================================
# 21. QC BY TRIP
# ============================================================

if ("tripID" %in% names(longline_qc)) {
  
  spatial_qc_trips <- longline_qc %>%
    
    group_by(
      dataset,
      tripID
    ) %>%
    
    summarise(
      
      operations = n(),
      
      flagged_operations = sum(
        qc_flag,
        na.rm = TRUE
      ),
      
      flagged_percent =
        100 *
        mean(
          qc_flag,
          na.rm = TRUE
        ),
      
      max_set_length_km = max(
        set_length_km,
        na.rm = TRUE
      ),
      
      max_haul_length_km = max(
        haul_length_km,
        na.rm = TRUE
      ),
      
      max_displacement_km = max(
        set_haul_displacement_km,
        na.rm = TRUE
      ),
      
      max_mcp_area_km2 = max(
        mcp_area_km2,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    
    filter(
      flagged_operations > 0
    ) %>%
    
    arrange(
      dataset,
      desc(
        flagged_operations
      )
    )
  
  
  write_csv(
    spatial_qc_trips,
    file.path(
      OUTPUT_00,
      "spatial_qc_trips.csv"
    )
  )
  
  
  cat(
    "\nTRIPS CONTAINING P99 FLAGS\n\n"
  )
  
  print(
    spatial_qc_trips,
    n = Inf,
    width = Inf
  )
}


# ============================================================
# 22. HISTOGRAMS OF GEOMETRY
# ============================================================

geometry_long <- longline_all %>%
  
  select(
    dataset,
    set_length_km,
    haul_length_km,
    set_haul_displacement_km,
    mcp_area_km2
  ) %>%
  
  pivot_longer(
    
    cols = c(
      set_length_km,
      haul_length_km,
      set_haul_displacement_km,
      mcp_area_km2
    ),
    
    names_to = "metric",
    
    values_to = "value"
  ) %>%
  
  filter(
    is.finite(
      value
    )
  )


p_geometry <- ggplot(
  
  geometry_long,
  
  aes(
    x = value
  )
  
) +
  
  geom_histogram(
    bins = 50,
    fill = "grey35",
    colour = "white",
    linewidth = 0.15
  ) +
  
  facet_grid(
    metric ~ dataset,
    scales = "free"
  ) +
  
  labs(
    title = "Fishing-operation geometry",
    subtitle = "Raw longline datasets — no observations removed",
    x = NULL,
    y = "Operations"
  ) +
  
  theme_bw(
    base_size = 11
  ) +
  
  theme(
    
    panel.grid.minor =
      element_blank(),
    
    strip.background =
      element_rect(
        fill = "grey95"
      ),
    
    plot.title =
      element_text(
        face = "bold"
      )
  )


ggsave(
  
  file.path(
    FIG_DIR,
    "01_longline_geometry_distributions.png"
  ),
  
  p_geometry,
  
  width = 10,
  height = 9,
  dpi = 300
)


# ============================================================
# 23. LOG-SCALE GEOMETRY DISTRIBUTIONS
#
# Useful because extreme geometries compress the normal plots.
# ============================================================

p_geometry_log <- ggplot(
  
  geometry_long %>%
    filter(
      value > 0
    ),
  
  aes(
    x = value
  )
  
) +
  
  geom_histogram(
    bins = 50,
    fill = "grey35",
    colour = "white",
    linewidth = 0.15
  ) +
  
  scale_x_log10() +
  
  facet_grid(
    metric ~ dataset,
    scales = "free"
  ) +
  
  labs(
    title = "Fishing-operation geometry — log scale",
    subtitle = "Useful for visualising the tails of the distributions",
    x = NULL,
    y = "Operations"
  ) +
  
  theme_bw(
    base_size = 11
  ) +
  
  theme(
    panel.grid.minor =
      element_blank(),
    
    strip.background =
      element_rect(
        fill = "grey95"
      ),
    
    plot.title =
      element_text(
        face = "bold"
      )
  )


ggsave(
  
  file.path(
    FIG_DIR,
    "02_longline_geometry_distributions_log.png"
  ),
  
  p_geometry_log,
  
  width = 10,
  height = 9,
  dpi = 300
)


# ============================================================
# 24. MAP DATA FOR LONGLINE OPERATIONS
# ============================================================

map_longline <- longline_qc %>%
  
  mutate(
    
    set_mid_lon = rowMeans(
      cbind(
        set_lon1,
        set_lon2
      ),
      na.rm = TRUE
    ),
    
    set_mid_lat = rowMeans(
      cbind(
        set_lat1,
        set_lat2
      ),
      na.rm = TRUE
    )
  )


map_longline$set_mid_lon[
  !is.finite(
    map_longline$set_mid_lon
  )
] <- NA_real_


map_longline$set_mid_lat[
  !is.finite(
    map_longline$set_mid_lat
  )
] <- NA_real_


# ============================================================
# 25. WORLD MAP
# ============================================================

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)


# ============================================================
# 26. OVERALL LONGLINE SPATIAL COVERAGE
# ============================================================

p_longline_map <- ggplot() +
  
  geom_sf(
    data = world,
    fill = "grey96",
    colour = "grey65",
    linewidth = 0.2
  ) +
  
  geom_point(
    
    data = map_longline,
    
    aes(
      x = set_mid_lon,
      y = set_mid_lat,
      colour = qc_flag
    ),
    
    size = 0.8,
    alpha = 0.7
  ) +
  
  scale_colour_manual(
    values = c(
      "FALSE" = "grey30",
      "TRUE" = "red"
    ),
    labels = c(
      "FALSE" = "Within P99",
      "TRUE" = "P99 flag"
    ),
    name = NULL
  ) +
  
  facet_wrap(
    ~dataset
  ) +
  
  coord_sf(
    datum = NA
  ) +
  
  labs(
    title = "Spatial coverage of longline operations",
    subtitle = "Red = operation flagged by at least one P99 geometry metric",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_bw(
    base_size = 11
  ) +
  
  theme(
    
    panel.grid.minor =
      element_blank(),
    
    legend.position =
      "bottom",
    
    plot.title =
      element_text(
        face = "bold"
      )
  )


ggsave(
  
  file.path(
    FIG_DIR,
    "03_longline_spatial_coverage_QC.png"
  ),
  
  p_longline_map,
  
  width = 10,
  height = 7,
  dpi = 300
)


# ============================================================
# 27. MAP INDIVIDUAL FLAGGED OPERATIONS
#
# One PNG per flagged operation.
#
# This is deliberately NOT faceted because coord_sf() and
# free scales caused problems in the previous exploratory code.
# ============================================================

flagged_for_maps <- longline_qc %>%
  
  filter(
    qc_flag
  ) %>%
  
  mutate(
    qc_map_id = row_number()
  )


if (nrow(flagged_for_maps) > 0) {
  
  flagged_map_dir <- file.path(
    FIG_DIR,
    "flagged_operations"
  )
  
  
  dir.create(
    flagged_map_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  for (i in seq_len(nrow(flagged_for_maps))) {
    
    x <- flagged_for_maps[
      i,
      ,
      drop = FALSE
    ]
    
    
    all_lon <- c(
      x$set_lon1,
      x$set_lon2,
      x$haul_lon1,
      x$haul_lon2
    )
    
    
    all_lat <- c(
      x$set_lat1,
      x$set_lat2,
      x$haul_lat1,
      x$haul_lat2
    )
    
    
    all_lon <- all_lon[
      is.finite(
        all_lon
      )
    ]
    
    
    all_lat <- all_lat[
      is.finite(
        all_lat
      )
    ]
    
    
    if (
      length(all_lon) < 2 ||
      length(all_lat) < 2
    ) {
      next
    }
    
    
    lon_range <- range(
      all_lon
    )
    
    
    lat_range <- range(
      all_lat
    )
    
    
    lon_pad <- max(
      diff(
        lon_range
      ) * 0.20,
      0.25
    )
    
    
    lat_pad <- max(
      diff(
        lat_range
      ) * 0.20,
      0.25
    )
    
    
    p <- ggplot() +
      
      geom_sf(
        data = world,
        fill = "grey96",
        colour = "grey65",
        linewidth = 0.2
      ) +
      
      geom_segment(
        
        data = x,
        
        aes(
          x = set_lon1,
          y = set_lat1,
          xend = set_lon2,
          yend = set_lat2
        ),
        
        linewidth = 1
      ) +
      
      geom_segment(
        
        data = x,
        
        aes(
          x = haul_lon1,
          y = haul_lat1,
          xend = haul_lon2,
          yend = haul_lat2
        ),
        
        linewidth = 1,
        linetype = 2
      ) +
      
      geom_point(
        
        data = tibble(
          lon = all_lon,
          lat = all_lat
        ),
        
        aes(
          x = lon,
          y = lat
        ),
        
        size = 2
      ) +
      
      coord_sf(
        
        xlim = c(
          lon_range[1] - lon_pad,
          lon_range[2] + lon_pad
        ),
        
        ylim = c(
          lat_range[1] - lat_pad,
          lat_range[2] + lat_pad
        ),
        
        expand = FALSE,
        datum = NA
      ) +
      
      labs(
        
        title = paste0(
          x$dataset,
          " — flagged operation"
        ),
        
        subtitle = paste0(
          "SET = solid | HAUL = dashed | ",
          "P99 flags = ",
          x$n_qc_flags
        ),
        
        x = "Longitude",
        y = "Latitude"
      ) +
      
      theme_bw(
        base_size = 11
      ) +
      
      theme(
        panel.grid.minor =
          element_blank(),
        
        plot.title =
          element_text(
            face = "bold"
          )
      )
    
    
    operation_label <- if (
      "operationID" %in% names(x) &&
      !is.na(x$operationID)
    ) {
      
      as.character(
        x$operationID
      )
      
    } else {
      
      paste0(
        "row_",
        i
      )
    }
    
    
    operation_label <- str_replace_all(
      operation_label,
      "[^A-Za-z0-9_-]",
      "_"
    )
    
    
    ggsave(
      
      file.path(
        flagged_map_dir,
        paste0(
          x$dataset,
          "_operation_",
          operation_label,
          ".png"
        )
      ),
      
      p,
      
      width = 7,
      height = 6,
      dpi = 250
    )
  }
}


# ============================================================
# 28. SAMPLE SIZE BY YEAR
#
# Uses an existing year variable if available.
# ============================================================

year_tables <- list()


for (
  x in list(
    list(
      dataset = "BYC_0002",
      data = ops_0002_raw,
      basic = basic_0002
    ),
    
    list(
      dataset = "BYC_0003",
      data = ops_0003_raw,
      basic = basic_0003
    ),
    
    list(
      dataset = "BYC_0004",
      data = ops_0004_raw,
      basic = basic_0004
    )
  )
) {
  
  dat <- x$data
  
  year_col <- x$basic$year
  
  
  if (!is.na(year_col)) {
    
    tmp <- dat %>%
      
      transmute(
        dataset = x$dataset,
        year = as.integer(
          .data[[year_col]]
        )
      ) %>%
      
      filter(
        is.finite(
          year
        )
      ) %>%
      
      count(
        dataset,
        year,
        name = "operations"
      )
    
    
    year_tables[[
      x$dataset
    ]] <- tmp
  }
}


if (length(year_tables) > 0) {
  
  operations_by_year <- bind_rows(
    year_tables
  )
  
  
  write_csv(
    operations_by_year,
    file.path(
      OUTPUT_00,
      "operations_by_year.csv"
    )
  )
  
  
  p_year <- ggplot(
    
    operations_by_year,
    
    aes(
      x = year,
      y = operations
    )
    
  ) +
    
    geom_col(
      width = 0.8
    ) +
    
    facet_wrap(
      ~dataset,
      scales = "free_y"
    ) +
    
    labs(
      title = "Temporal sampling coverage",
      x = "Year",
      y = "Fishing operations"
    ) +
    
    theme_bw(
      base_size = 11
    ) +
    
    theme(
      panel.grid.minor =
        element_blank(),
      
      plot.title =
        element_text(
          face = "bold"
        )
    )
  
  
  ggsave(
    
    file.path(
      FIG_DIR,
      "04_operations_by_year.png"
    ),
    
    p_year,
    
    width = 10,
    height = 5,
    dpi = 300
  )
}


# ============================================================
# 29. QC SUMMARY
# ============================================================

qc_summary <- longline_qc %>%
  
  group_by(
    dataset
  ) %>%
  
  summarise(
    
    operations = n(),
    
    flagged_operations = sum(
      qc_flag,
      na.rm = TRUE
    ),
    
    flagged_percent =
      100 *
      mean(
        qc_flag,
        na.rm = TRUE
      ),
    
    flag_set_length = sum(
      flag_set_length,
      na.rm = TRUE
    ),
    
    flag_haul_length = sum(
      flag_haul_length,
      na.rm = TRUE
    ),
    
    flag_displacement = sum(
      flag_displacement,
      na.rm = TRUE
    ),
    
    flag_mcp = sum(
      flag_mcp,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


write_csv(
  qc_summary,
  file.path(
    OUTPUT_00,
    "spatial_qc_summary.csv"
  )
)


cat(
  "\nSPATIAL QC SUMMARY\n\n"
)

print(
  qc_summary,
  width = Inf
)


# ============================================================
# 30. SAVE FULL LONGLINE QC TABLE
#
# This is NOT modelling data.
# It is an exploratory diagnostic table.
# ============================================================

write_csv(
  longline_qc,
  file.path(
    OUTPUT_00,
    "longline_operations_with_QC.csv"
  )
)


# ============================================================
# 31. SESSION INFO
# ============================================================

capture.output(
  
  sessionInfo(),
  
  file = file.path(
    OUTPUT_00,
    "sessionInfo.txt"
  )
)


# ============================================================
# 32. FINAL MESSAGE
# ============================================================

cat(
  "\n\n====================================================\n",
  "00 — RAW DATA EXPLORATION COMPLETE\n",
  "====================================================\n\n",
  sep = ""
)


cat(
  "Nothing has been excluded or modified.\n\n"
)


cat(
  "Main outputs:\n\n",
  
  "  dataset_inventory.csv\n",
  "  raw_column_inventory.csv\n",
  "  raw_table_inventory.csv\n",
  "  bycatch_operationID_audit.csv\n",
  "  longline_geometry_summary.csv\n",
  "  spatial_qc_thresholds.csv\n",
  "  spatial_qc_operations.csv\n",
  "  spatial_qc_trips.csv\n",
  "  spatial_qc_summary.csv\n",
  "  longline_operations_with_QC.csv\n",
  "  figures/\n\n",
  
  sep = ""
)


cat(
  "IMPORTANT:\n",
  "P99 flags are diagnostic only.\n",
  "They are NOT automatic exclusions.\n\n",
  sep = ""
)


cat(
  "Next step after reviewing these outputs:\n",
  "01_build_standardised_data.R\n\n",
  sep = ""
)


cat(
  "====================================================\n"
)