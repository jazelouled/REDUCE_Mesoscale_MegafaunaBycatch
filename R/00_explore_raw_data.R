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
# It only describes the datasets and flags potentially
# problematic longline geometries for inspection.
#
# P99 flags are diagnostic, NOT automatic exclusions.
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
      "One or more required raw files are missing:\n\n",
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
# 6. PARSING AUDIT
# ============================================================

parsing_summary <- tibble::tibble(
  
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
  
  parsing_problems = c(
    nrow(readr::problems(ops_0002_raw)),
    nrow(readr::problems(bycatch_0002_raw)),
    nrow(readr::problems(individuals_0002_raw)),
    nrow(readr::problems(ops_0003_raw)),
    nrow(readr::problems(bycatch_0003_raw)),
    nrow(readr::problems(ops_0004_raw)),
    nrow(readr::problems(bycatch_0004_raw))
  )
)


readr::write_csv(
  parsing_summary,
  file.path(
    OUTPUT_00,
    "parsing_summary.csv"
  )
)


cat(
  "\nPARSING SUMMARY\n\n"
)

print(
  parsing_summary,
  n = Inf
)


# Save detailed parsing problems when present

raw_objects <- list(
  BYC_0002_fishingOperations = ops_0002_raw,
  BYC_0002_bycatchAggregated = bycatch_0002_raw,
  BYC_0002_bycatchSampledIndividuals = individuals_0002_raw,
  BYC_0003_fishingOperations = ops_0003_raw,
  BYC_0003_bycatch = bycatch_0003_raw,
  BYC_0004_fishingOperations = ops_0004_raw,
  BYC_0004_bycatch = bycatch_0004_raw
)


for (object_name in names(raw_objects)) {
  
  problems_this <- readr::problems(
    raw_objects[[object_name]]
  )
  
  
  if (nrow(problems_this) > 0) {
    
    readr::write_csv(
      problems_this,
      file.path(
        OUTPUT_00,
        paste0(
          "parsing_problems_",
          object_name,
          ".csv"
        )
      )
    )
  }
}


# ============================================================
# 7. RAW COLUMN INVENTORY
# ============================================================

column_inventory <- dplyr::bind_rows(
  
  tibble::tibble(
    dataset = "BYC_0002",
    table = "fishingOperations",
    variable = names(ops_0002_raw)
  ),
  
  tibble::tibble(
    dataset = "BYC_0002",
    table = "bycatchAggregated",
    variable = names(bycatch_0002_raw)
  ),
  
  tibble::tibble(
    dataset = "BYC_0002",
    table = "bycatchSampledIndividuals",
    variable = names(individuals_0002_raw)
  ),
  
  tibble::tibble(
    dataset = "BYC_0003",
    table = "fishingOperations",
    variable = names(ops_0003_raw)
  ),
  
  tibble::tibble(
    dataset = "BYC_0003",
    table = "bycatch",
    variable = names(bycatch_0003_raw)
  ),
  
  tibble::tibble(
    dataset = "BYC_0004",
    table = "fishingOperations",
    variable = names(ops_0004_raw)
  ),
  
  tibble::tibble(
    dataset = "BYC_0004",
    table = "bycatch",
    variable = names(bycatch_0004_raw)
  )
)


readr::write_csv(
  column_inventory,
  file.path(
    OUTPUT_00,
    "raw_column_inventory.csv"
  )
)


# ============================================================
# 8. RAW TABLE INVENTORY
# ============================================================

table_inventory <- tibble::tibble(
  
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


readr::write_csv(
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
  table_inventory,
  n = Inf
)


# ============================================================
# 9. REQUIRED VARIABLES
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
    
  } else {
    
    cat(
      "[OK] Required variables found in ",
      dataset,
      "\n",
      sep = ""
    )
  }
  
  
  invisible(
    missing
  )
}


# ------------------------------------------------------------
# BYC_0002 — purse seine
# No SET/HAUL geometry is expected here.
# ------------------------------------------------------------

check_variables(
  ops_0002_raw,
  c(
    "operationID",
    "tripID"
  ),
  "BYC_0002 fishingOperations"
)


# ------------------------------------------------------------
# BYC_0003 — longline
# ------------------------------------------------------------

check_variables(
  ops_0003_raw,
  c(
    "operationID",
    "tripID",
    "setLongitude1",
    "setLatitude1",
    "setLongitude2",
    "setLatitude2",
    "haulLongitude1",
    "haulLatitude1",
    "haulLongitude2",
    "haulLatitude2"
  ),
  "BYC_0003 fishingOperations"
)


# ------------------------------------------------------------
# BYC_0004 — longline
# ------------------------------------------------------------

check_variables(
  ops_0004_raw,
  c(
    "operationID",
    "tripID",
    "setLongitude1",
    "setLatitude1",
    "setLongitude2",
    "setLatitude2",
    "haulLongitude1",
    "haulLatitude1",
    "haulLongitude2",
    "haulLatitude2"
  ),
  "BYC_0004 fishingOperations"
)


# ============================================================
# 10. BASIC DATASET INVENTORY
# ============================================================

safe_n_distinct <- function(
    dat,
    variable
) {
  
  if (!variable %in% names(dat)) {
    return(
      NA_integer_
    )
  }
  
  
  dplyr::n_distinct(
    dat[[variable]],
    na.rm = TRUE
  )
}


dataset_inventory <- tibble::tibble(
  
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
      "operationID"
    ),
    safe_n_distinct(
      ops_0003_raw,
      "operationID"
    ),
    safe_n_distinct(
      ops_0004_raw,
      "operationID"
    )
  ),
  
  trips = c(
    safe_n_distinct(
      ops_0002_raw,
      "tripID"
    ),
    safe_n_distinct(
      ops_0003_raw,
      "tripID"
    ),
    safe_n_distinct(
      ops_0004_raw,
      "tripID"
    )
  ),
  
  vessels = c(
    safe_n_distinct(
      ops_0002_raw,
      "vesselID"
    ),
    safe_n_distinct(
      ops_0003_raw,
      "vesselID"
    ),
    safe_n_distinct(
      ops_0004_raw,
      "vesselID"
    )
  ),
  
  min_year = c(
    min(
      ops_0002_raw$year,
      na.rm = TRUE
    ),
    min(
      ops_0003_raw$year,
      na.rm = TRUE
    ),
    min(
      ops_0004_raw$year,
      na.rm = TRUE
    )
  ),
  
  max_year = c(
    max(
      ops_0002_raw$year,
      na.rm = TRUE
    ),
    max(
      ops_0003_raw$year,
      na.rm = TRUE
    ),
    max(
      ops_0004_raw$year,
      na.rm = TRUE
    )
  )
)


readr::write_csv(
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
  dataset_inventory,
  width = Inf
)


# ============================================================
# 11. DUPLICATE OPERATION-ID AUDIT
# ============================================================

duplicate_operation_audit <- dplyr::bind_rows(
  
  ops_0002_raw %>%
    count(
      operationID,
      name = "n"
    ) %>%
    filter(
      n > 1
    ) %>%
    mutate(
      dataset = "BYC_0002"
    ),
  
  ops_0003_raw %>%
    count(
      operationID,
      name = "n"
    ) %>%
    filter(
      n > 1
    ) %>%
    mutate(
      dataset = "BYC_0003"
    ),
  
  ops_0004_raw %>%
    count(
      operationID,
      name = "n"
    ) %>%
    filter(
      n > 1
    ) %>%
    mutate(
      dataset = "BYC_0004"
    )
  
) %>%
  select(
    dataset,
    operationID,
    n
  )


readr::write_csv(
  duplicate_operation_audit,
  file.path(
    OUTPUT_00,
    "duplicate_operationID_audit.csv"
  )
)


# ============================================================
# 12. BYCATCH → OPERATION-ID AUDIT
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
      tibble::tibble(
        dataset = dataset,
        fishing_operations = nrow(operations),
        bycatch_rows = nrow(bycatch),
        bycatch_unique_operations = NA_integer_,
        operations_with_bycatch_record = NA_integer_,
        operations_without_bycatch_record = NA_integer_,
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
  
  
  tibble::tibble(
    
    dataset = dataset,
    
    fishing_operations = length(
      operation_ids
    ),
    
    bycatch_rows = nrow(
      bycatch
    ),
    
    bycatch_unique_operations = length(
      bycatch_ids
    ),
    
    operations_with_bycatch_record = sum(
      operation_ids %in% bycatch_ids
    ),
    
    operations_without_bycatch_record = sum(
      !operation_ids %in% bycatch_ids
    ),
    
    bycatch_operation_ids_not_in_fishing_operations = sum(
      !bycatch_ids %in% operation_ids
    )
  )
}


bycatch_id_audit <- dplyr::bind_rows(
  
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


readr::write_csv(
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
  bycatch_id_audit,
  width = Inf
)


# ============================================================
# 13. CREATE WORKING GEOMETRY COPIES
#
# IMPORTANT:
# Raw objects remain untouched.
#
# BYC_0003 and BYC_0004 use exactly the same raw names for
# the principal SET/HAUL coordinates.
# ============================================================

ops_0003_geometry <- ops_0003_raw %>%
  
  dplyr::rename(
    set_lon1 = setLongitude1,
    set_lat1 = setLatitude1,
    set_lon2 = setLongitude2,
    set_lat2 = setLatitude2,
    haul_lon1 = haulLongitude1,
    haul_lat1 = haulLatitude1,
    haul_lon2 = haulLongitude2,
    haul_lat2 = haulLatitude2
  )


ops_0004_geometry <- ops_0004_raw %>%
  
  dplyr::rename(
    set_lon1 = setLongitude1,
    set_lat1 = setLatitude1,
    set_lon2 = setLongitude2,
    set_lat2 = setLatitude2,
    haul_lon1 = haulLongitude1,
    haul_lat1 = haulLatitude1,
    haul_lon2 = haulLongitude2,
    haul_lat2 = haulLatitude2
  )


# ============================================================
# 14. COORDINATE VALIDITY HELPERS
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
# 15. DISTANCE HELPER
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
# 16. MCP AREA HELPER
#
# MCP based on the four SET/HAUL endpoints.
# Area is returned in km2.
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
  
  coords <- tibble::tibble(
    
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
    
    dplyr::filter(
      valid_lon(lon),
      valid_lat(lat)
    ) %>%
    
    dplyr::distinct()
  
  
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
# 17. PREPARE LONGLINE GEOMETRY
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
      ),
      call. = FALSE
    )
  }
  
  
  out <- dat %>%
    
    dplyr::mutate(
      
      dataset = dataset,
      
      valid_set1 =
        valid_lon(set_lon1) &
        valid_lat(set_lat1),
      
      valid_set2 =
        valid_lon(set_lon2) &
        valid_lat(set_lat2),
      
      valid_haul1 =
        valid_lon(haul_lon1) &
        valid_lat(haul_lat1),
      
      valid_haul2 =
        valid_lon(haul_lon2) &
        valid_lat(haul_lat2),
      
      valid_all_four_points =
        valid_set1 &
        valid_set2 &
        valid_haul1 &
        valid_haul2,
      
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
  # SET → HAUL midpoint displacement
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


# ============================================================
# 18. CALCULATE LONGLINE GEOMETRY
# ============================================================

cat(
  "\nCalculating BYC_0003 geometry...\n"
)


longline_0003 <- prepare_longline_geometry(
  ops_0003_geometry,
  "BYC_0003"
)


cat(
  "Calculating BYC_0004 geometry...\n"
)


longline_0004 <- prepare_longline_geometry(
  ops_0004_geometry,
  "BYC_0004"
)


longline_all <- dplyr::bind_rows(
  longline_0003,
  longline_0004
)


# ============================================================
# 19. GEOMETRY SUMMARY
# ============================================================

safe_quantile <- function(
    x,
    probability
) {
  
  x <- x[
    is.finite(
      x
    )
  ]
  
  
  if (length(x) == 0) {
    return(
      NA_real_
    )
  }
  
  
  as.numeric(
    stats::quantile(
      x,
      probability,
      na.rm = TRUE
    )
  )
}


safe_median <- function(x) {
  
  x <- x[
    is.finite(
      x
    )
  ]
  
  
  if (length(x) == 0) {
    return(
      NA_real_
    )
  }
  
  
  stats::median(
    x,
    na.rm = TRUE
  )
}


geometry_summary <- longline_all %>%
  
  dplyr::group_by(
    dataset
  ) %>%
  
  dplyr::summarise(
    
    operations = dplyr::n(),
    
    operations_with_all_four_points = sum(
      valid_all_four_points,
      na.rm = TRUE
    ),
    
    set_length_median_km = safe_median(
      set_length_km
    ),
    
    set_length_p95_km = safe_quantile(
      set_length_km,
      0.95
    ),
    
    set_length_p99_km = safe_quantile(
      set_length_km,
      0.99
    ),
    
    haul_length_median_km = safe_median(
      haul_length_km
    ),
    
    haul_length_p95_km = safe_quantile(
      haul_length_km,
      0.95
    ),
    
    haul_length_p99_km = safe_quantile(
      haul_length_km,
      0.99
    ),
    
    displacement_median_km = safe_median(
      set_haul_displacement_km
    ),
    
    displacement_p95_km = safe_quantile(
      set_haul_displacement_km,
      0.95
    ),
    
    displacement_p99_km = safe_quantile(
      set_haul_displacement_km,
      0.99
    ),
    
    mcp_median_km2 = safe_median(
      mcp_area_km2
    ),
    
    mcp_p95_km2 = safe_quantile(
      mcp_area_km2,
      0.95
    ),
    
    mcp_p99_km2 = safe_quantile(
      mcp_area_km2,
      0.99
    ),
    
    .groups = "drop"
  )


readr::write_csv(
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
# 20. P99 QC THRESHOLDS
#
# Thresholds calculated independently for BYC_0003 and
# BYC_0004.
#
# Above P99 = inspect.
# Above P99 != automatically exclude.
# ============================================================

qc_thresholds <- longline_all %>%
  
  dplyr::group_by(
    dataset
  ) %>%
  
  dplyr::summarise(
    
    p99_set_length_km = safe_quantile(
      set_length_km,
      0.99
    ),
    
    p99_haul_length_km = safe_quantile(
      haul_length_km,
      0.99
    ),
    
    p99_displacement_km = safe_quantile(
      set_haul_displacement_km,
      0.99
    ),
    
    p99_mcp_area_km2 = safe_quantile(
      mcp_area_km2,
      0.99
    ),
    
    .groups = "drop"
  )


readr::write_csv(
  qc_thresholds,
  file.path(
    OUTPUT_00,
    "spatial_qc_thresholds.csv"
  )
)


# ============================================================
# 21. FLAG P99 OPERATIONS
# ============================================================

longline_qc <- longline_all %>%
  
  dplyr::left_join(
    qc_thresholds,
    by = "dataset"
  ) %>%
  
  dplyr::mutate(
    
    flag_invalid_coordinates =
      !valid_all_four_points,
    
    flag_set_length =
      is.finite(
        set_length_km
      ) &
      is.finite(
        p99_set_length_km
      ) &
      set_length_km >
      p99_set_length_km,
    
    flag_haul_length =
      is.finite(
        haul_length_km
      ) &
      is.finite(
        p99_haul_length_km
      ) &
      haul_length_km >
      p99_haul_length_km,
    
    flag_displacement =
      is.finite(
        set_haul_displacement_km
      ) &
      is.finite(
        p99_displacement_km
      ) &
      set_haul_displacement_km >
      p99_displacement_km,
    
    flag_mcp =
      is.finite(
        mcp_area_km2
      ) &
      is.finite(
        p99_mcp_area_km2
      ) &
      mcp_area_km2 >
      p99_mcp_area_km2,
    
    qc_flag =
      flag_invalid_coordinates |
      flag_set_length |
      flag_haul_length |
      flag_displacement |
      flag_mcp,
    
    n_qc_flags =
      as.integer(
        flag_invalid_coordinates
      ) +
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
# 22. SAVE FLAGGED OPERATIONS
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
  
  dplyr::filter(
    qc_flag
  ) %>%
  
  dplyr::select(
    
    dplyr::all_of(
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
    
    flag_invalid_coordinates,
    flag_set_length,
    flag_haul_length,
    flag_displacement,
    flag_mcp,
    
    n_qc_flags
  ) %>%
  
  dplyr::arrange(
    dataset,
    dplyr::desc(
      n_qc_flags
    ),
    dplyr::desc(
      set_haul_displacement_km
    )
  )


readr::write_csv(
  spatial_qc_operations,
  file.path(
    OUTPUT_00,
    "spatial_qc_operations.csv"
  )
)


cat(
  "\nP99 / COORDINATE FLAGGED OPERATIONS\n\n"
)

print(
  spatial_qc_operations,
  n = Inf,
  width = Inf
)


# ============================================================
# 23. QC BY TRIP
# ============================================================

spatial_qc_trips <- longline_qc %>%
  
  dplyr::group_by(
    dataset,
    tripID
  ) %>%
  
  dplyr::summarise(
    
    operations = dplyr::n(),
    
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
    
    .groups = "drop"
  ) %>%
  
  dplyr::filter(
    flagged_operations > 0
  ) %>%
  
  dplyr::arrange(
    dataset,
    dplyr::desc(
      flagged_operations
    ),
    dplyr::desc(
      flagged_percent
    )
  )


readr::write_csv(
  spatial_qc_trips,
  file.path(
    OUTPUT_00,
    "spatial_qc_trips.csv"
  )
)


cat(
  "\nTRIPS CONTAINING QC FLAGS\n\n"
)

print(
  spatial_qc_trips,
  n = Inf,
  width = Inf
)


# ============================================================
# 24. QC SUMMARY
# ============================================================

qc_summary <- longline_qc %>%
  
  dplyr::group_by(
    dataset
  ) %>%
  
  dplyr::summarise(
    
    operations = dplyr::n(),
    
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
    
    invalid_coordinates = sum(
      flag_invalid_coordinates,
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


readr::write_csv(
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
# 25. SAVE FULL LONGLINE QC TABLE
#
# Diagnostic only.
# This is NOT the modelling dataset.
# ============================================================

readr::write_csv(
  longline_qc,
  file.path(
    OUTPUT_00,
    "longline_operations_with_QC.csv"
  )
)


# ============================================================
# 26. OPERATIONS BY YEAR
# ============================================================

operations_by_year <- dplyr::bind_rows(
  
  ops_0002_raw %>%
    dplyr::count(
      year,
      name = "operations"
    ) %>%
    dplyr::mutate(
      dataset = "BYC_0002"
    ),
  
  ops_0003_raw %>%
    dplyr::count(
      year,
      name = "operations"
    ) %>%
    dplyr::mutate(
      dataset = "BYC_0003"
    ),
  
  ops_0004_raw %>%
    dplyr::count(
      year,
      name = "operations"
    ) %>%
    dplyr::mutate(
      dataset = "BYC_0004"
    )
  
) %>%
  
  dplyr::select(
    dataset,
    year,
    operations
  ) %>%
  
  dplyr::arrange(
    dataset,
    year
  )


readr::write_csv(
  operations_by_year,
  file.path(
    OUTPUT_00,
    "operations_by_year.csv"
  )
)


# ============================================================
# 27. FIGURE — OPERATIONS BY YEAR
# ============================================================

p_year <- ggplot2::ggplot(
  
  operations_by_year,
  
  ggplot2::aes(
    x = year,
    y = operations
  )
  
) +
  
  ggplot2::geom_col(
    width = 0.8
  ) +
  
  ggplot2::facet_wrap(
    ~dataset,
    scales = "free_y"
  ) +
  
  ggplot2::labs(
    title = "Temporal sampling coverage",
    subtitle = "Raw IEO fishing operations",
    x = "Year",
    y = "Fishing operations"
  ) +
  
  ggplot2::theme_bw(
    base_size = 11
  ) +
  
  ggplot2::theme(
    panel.grid.minor =
      ggplot2::element_blank(),
    
    plot.title =
      ggplot2::element_text(
        face = "bold"
      )
  )


ggplot2::ggsave(
  
  file.path(
    FIG_DIR,
    "01_operations_by_year.png"
  ),
  
  p_year,
  
  width = 10,
  height = 5,
  dpi = 300
)


# ============================================================
# 28. FIGURE — GEOMETRY DISTRIBUTIONS
# ============================================================

geometry_long <- longline_all %>%
  
  dplyr::select(
    dataset,
    set_length_km,
    haul_length_km,
    set_haul_displacement_km,
    mcp_area_km2
  ) %>%
  
  tidyr::pivot_longer(
    
    cols = c(
      set_length_km,
      haul_length_km,
      set_haul_displacement_km,
      mcp_area_km2
    ),
    
    names_to = "metric",
    
    values_to = "value"
  ) %>%
  
  dplyr::filter(
    is.finite(
      value
    )
  )


p_geometry <- ggplot2::ggplot(
  
  geometry_long,
  
  ggplot2::aes(
    x = value
  )
  
) +
  
  ggplot2::geom_histogram(
    bins = 50,
    fill = "grey35",
    colour = "white",
    linewidth = 0.15
  ) +
  
  ggplot2::facet_grid(
    metric ~ dataset,
    scales = "free"
  ) +
  
  ggplot2::labs(
    title = "Fishing-operation geometry",
    subtitle = "Raw longline datasets — no observations removed",
    x = NULL,
    y = "Operations"
  ) +
  
  ggplot2::theme_bw(
    base_size = 11
  ) +
  
  ggplot2::theme(
    
    panel.grid.minor =
      ggplot2::element_blank(),
    
    strip.background =
      ggplot2::element_rect(
        fill = "grey95"
      ),
    
    plot.title =
      ggplot2::element_text(
        face = "bold"
      )
  )


ggplot2::ggsave(
  
  file.path(
    FIG_DIR,
    "02_longline_geometry_distributions.png"
  ),
  
  p_geometry,
  
  width = 10,
  height = 9,
  dpi = 300
)


# ============================================================
# 29. FIGURE — LOG-SCALE GEOMETRY DISTRIBUTIONS
# ============================================================

p_geometry_log <- ggplot2::ggplot(
  
  geometry_long %>%
    dplyr::filter(
      value > 0
    ),
  
  ggplot2::aes(
    x = value
  )
  
) +
  
  ggplot2::geom_histogram(
    bins = 50,
    fill = "grey35",
    colour = "white",
    linewidth = 0.15
  ) +
  
  ggplot2::scale_x_log10() +
  
  ggplot2::facet_grid(
    metric ~ dataset,
    scales = "free"
  ) +
  
  ggplot2::labs(
    title = "Fishing-operation geometry — log scale",
    subtitle = "Tail structure of the longline geometry metrics",
    x = NULL,
    y = "Operations"
  ) +
  
  ggplot2::theme_bw(
    base_size = 11
  ) +
  
  ggplot2::theme(
    
    panel.grid.minor =
      ggplot2::element_blank(),
    
    strip.background =
      ggplot2::element_rect(
        fill = "grey95"
      ),
    
    plot.title =
      ggplot2::element_text(
        face = "bold"
      )
  )


ggplot2::ggsave(
  
  file.path(
    FIG_DIR,
    "03_longline_geometry_distributions_log.png"
  ),
  
  p_geometry_log,
  
  width = 10,
  height = 9,
  dpi = 300
)


# ============================================================
# 30. WORLD MAP
# ============================================================

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)


# ============================================================
# 31. FIGURE — LONGLINE SPATIAL COVERAGE
# ============================================================

map_longline <- longline_qc %>%
  
  dplyr::filter(
    is.finite(
      set_mid_lon
    ),
    is.finite(
      set_mid_lat
    )
  )


p_longline_map <- ggplot2::ggplot() +
  
  ggplot2::geom_sf(
    data = world,
    fill = "grey96",
    colour = "grey65",
    linewidth = 0.2
  ) +
  
  ggplot2::geom_point(
    
    data = map_longline,
    
    ggplot2::aes(
      x = set_mid_lon,
      y = set_mid_lat,
      colour = qc_flag
    ),
    
    size = 0.8,
    alpha = 0.7
  ) +
  
  ggplot2::scale_colour_manual(
    values = c(
      "FALSE" = "grey30",
      "TRUE" = "red"
    ),
    labels = c(
      "FALSE" = "Not flagged",
      "TRUE" = "QC flag"
    ),
    name = NULL
  ) +
  
  ggplot2::facet_wrap(
    ~dataset
  ) +
  
  ggplot2::coord_sf(
    datum = NA
  ) +
  
  ggplot2::labs(
    title = "Spatial coverage of longline operations",
    subtitle = "Red = operation flagged by invalid coordinates or a P99 geometry metric",
    x = "Longitude",
    y = "Latitude"
  ) +
  
  ggplot2::theme_bw(
    base_size = 11
  ) +
  
  ggplot2::theme(
    
    panel.grid.minor =
      ggplot2::element_blank(),
    
    legend.position =
      "bottom",
    
    plot.title =
      ggplot2::element_text(
        face = "bold"
      )
  )


ggplot2::ggsave(
  
  file.path(
    FIG_DIR,
    "04_longline_spatial_coverage_QC.png"
  ),
  
  p_longline_map,
  
  width = 10,
  height = 7,
  dpi = 300
)


# ============================================================
# 32. INDIVIDUAL MAPS OF FLAGGED OPERATIONS
#
# One file per flagged operation.
# No faceting with free scales.
# ============================================================

flagged_for_maps <- longline_qc %>%
  
  dplyr::filter(
    qc_flag
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
    
    
    valid_points <-
      is.finite(
        all_lon
      ) &
      is.finite(
        all_lat
      ) &
      all_lon >= -180 &
      all_lon <= 180 &
      all_lat >= -90 &
      all_lat <= 90
    
    
    all_lon <- all_lon[
      valid_points
    ]
    
    
    all_lat <- all_lat[
      valid_points
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
    
    
    plot_points <- tibble::tibble(
      lon = all_lon,
      lat = all_lat
    )
    
    
    p <- ggplot2::ggplot() +
      
      ggplot2::geom_sf(
        data = world,
        fill = "grey96",
        colour = "grey65",
        linewidth = 0.2
      ) +
      
      ggplot2::geom_segment(
        
        data = x %>%
          dplyr::filter(
            valid_lon(set_lon1),
            valid_lat(set_lat1),
            valid_lon(set_lon2),
            valid_lat(set_lat2)
          ),
        
        ggplot2::aes(
          x = set_lon1,
          y = set_lat1,
          xend = set_lon2,
          yend = set_lat2
        ),
        
        linewidth = 1
      ) +
      
      ggplot2::geom_segment(
        
        data = x %>%
          dplyr::filter(
            valid_lon(haul_lon1),
            valid_lat(haul_lat1),
            valid_lon(haul_lon2),
            valid_lat(haul_lat2)
          ),
        
        ggplot2::aes(
          x = haul_lon1,
          y = haul_lat1,
          xend = haul_lon2,
          yend = haul_lat2
        ),
        
        linewidth = 1,
        linetype = 2
      ) +
      
      ggplot2::geom_point(
        
        data = plot_points,
        
        ggplot2::aes(
          x = lon,
          y = lat
        ),
        
        size = 2
      ) +
      
      ggplot2::coord_sf(
        
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
      
      ggplot2::labs(
        
        title = paste0(
          x$dataset,
          " — flagged operation"
        ),
        
        subtitle = paste0(
          "SET = solid | HAUL = dashed | QC flags = ",
          x$n_qc_flags
        ),
        
        x = "Longitude",
        y = "Latitude"
      ) +
      
      ggplot2::theme_bw(
        base_size = 11
      ) +
      
      ggplot2::theme(
        
        panel.grid.minor =
          ggplot2::element_blank(),
        
        plot.title =
          ggplot2::element_text(
            face = "bold"
          )
      )
    
    
    operation_label <- if (
      "operationID" %in% names(x) &&
      !is.na(
        x$operationID
      )
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
    
    
    operation_label <- stringr::str_replace_all(
      operation_label,
      "[^A-Za-z0-9_-]",
      "_"
    )
    
    
    ggplot2::ggsave(
      
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
# 33. BYC_0004 ORIGINAL VS CURRENT COORDINATE AUDIT
#
# BYC_0004 contains both original* and current coordinates.
# We do NOT interpret or alter them here.
#
# We simply document the *_diff variables already supplied
# in the raw dataset.
# ============================================================

diff_variables_0004 <- intersect(
  
  c(
    "set1_longitude_diff",
    "set1_latitude_diff",
    "set2_longitude_diff",
    "set2_latitude_diff",
    "haul1_longitude_diff",
    "haul1_latitude_diff",
    "haul2_longitude_diff",
    "haul2_latitude_diff"
  ),
  
  names(
    ops_0004_raw
  )
)


if (length(diff_variables_0004) > 0) {
  
  coordinate_diff_summary_0004 <- purrr::map_dfr(
    
    diff_variables_0004,
    
    function(variable) {
      
      x <- ops_0004_raw[[variable]]
      
      x <- x[
        is.finite(
          x
        )
      ]
      
      
      tibble::tibble(
        
        variable = variable,
        
        n_available = length(
          x
        ),
        
        n_nonzero = sum(
          x != 0,
          na.rm = TRUE
        ),
        
        median = if (
          length(x) > 0
        ) {
          median(
            x,
            na.rm = TRUE
          )
        } else {
          NA_real_
        },
        
        p95_abs = if (
          length(x) > 0
        ) {
          as.numeric(
            quantile(
              abs(x),
              0.95,
              na.rm = TRUE
            )
          )
        } else {
          NA_real_
        },
        
        max_abs = if (
          length(x) > 0
        ) {
          max(
            abs(x),
            na.rm = TRUE
          )
        } else {
          NA_real_
        }
      )
    }
  )
  
  
  readr::write_csv(
    coordinate_diff_summary_0004,
    file.path(
      OUTPUT_00,
      "BYC_0004_original_current_coordinate_diff_summary.csv"
    )
  )
  
  
  cat(
    "\nBYC_0004 ORIGINAL / CURRENT COORDINATE DIFF SUMMARY\n\n"
  )
  
  print(
    coordinate_diff_summary_0004,
    n = Inf,
    width = Inf
  )
}


# ============================================================
# 34. SESSION INFO
# ============================================================

capture.output(
  
  sessionInfo(),
  
  file = file.path(
    OUTPUT_00,
    "sessionInfo.txt"
  )
)


# ============================================================
# 35. FINAL MESSAGE
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
  
  "  parsing_summary.csv\n",
  "  raw_column_inventory.csv\n",
  "  raw_table_inventory.csv\n",
  "  dataset_inventory.csv\n",
  "  duplicate_operationID_audit.csv\n",
  "  bycatch_operationID_audit.csv\n",
  "  longline_geometry_summary.csv\n",
  "  spatial_qc_thresholds.csv\n",
  "  spatial_qc_operations.csv\n",
  "  spatial_qc_trips.csv\n",
  "  spatial_qc_summary.csv\n",
  "  longline_operations_with_QC.csv\n",
  "  operations_by_year.csv\n",
  "  BYC_0004_original_current_coordinate_diff_summary.csv\n",
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
  "Next step after reviewing the QC:\n",
  "01_build_standardised_data.R\n\n",
  sep = ""
)


cat(
  "====================================================\n"
)