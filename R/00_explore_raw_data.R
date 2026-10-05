# ============================================================
# 00_raw_exploration.R
#
# REDUCE — MESOSCALE MEGAFAUNA BYCATCH
#
# PURPOSE
# ------------------------------------------------------------
# Exploratory inspection of the original IEO fishing-operation
# datasets before any standardisation, correction, exclusion,
# environmental matching or modelling.
#
# DATASETS
#   BYC_0002 = purse seine
#   BYC_0003 = longline
#   BYC_0004 = longline
#
# IMPORTANT
# ------------------------------------------------------------
# - Reads directly from RAW CSV files.
# - Does NOT modify the raw files.
# - Does NOT remove operations.
# - Does NOT correct coordinates.
# - Does NOT exclude suspicious trips.
# - P99 flags are exploratory diagnostics only.
#
# OUTPUT
# ------------------------------------------------------------
# 00inputOutput/00output/00_raw_exploration/
#
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

required_packages <- c(
  "here",
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "stringr",
  "tibble",
  "ggplot2",
  "patchwork",
  "maps",
  "scales"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing packages: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them before running 00_raw_exploration.R."
    )
  )
}

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(ggplot2)
library(patchwork)
library(maps)
library(scales)


# ============================================================
# 1. PATHS
# ============================================================

INPUT_ROOT <- here(
  "00inputOutput",
  "00input"
)

IEO_ROOT <- here(
  "00inputOutput",
  "00input",
  "00IEO"
)

OUTPUT_ROOT <- here(
  "00inputOutput",
  "00output",
  "00_raw_exploration"
)

OUTPUT_TABLES <- file.path(
  OUTPUT_ROOT,
  "tables"
)

OUTPUT_FIGURES <- file.path(
  OUTPUT_ROOT,
  "figures"
)

OUTPUT_MAPS <- file.path(
  OUTPUT_ROOT,
  "maps"
)

OUTPUT_QC <- file.path(
  OUTPUT_ROOT,
  "QC"
)

OUTPUT_DIAGNOSTICS <- file.path(
  OUTPUT_ROOT,
  "diagnostics"
)

dirs_to_create <- c(
  OUTPUT_ROOT,
  OUTPUT_TABLES,
  OUTPUT_FIGURES,
  OUTPUT_MAPS,
  OUTPUT_QC,
  OUTPUT_DIAGNOSTICS
)

for (this_dir in dirs_to_create) {
  dir.create(
    this_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ============================================================
# 2. BASIC CHECKS
# ============================================================

if (!dir.exists(IEO_ROOT)) {
  stop(
    paste0(
      "IEO input directory does not exist:\n",
      IEO_ROOT
    )
  )
}

datasets <- c(
  "BYC_0002",
  "BYC_0003",
  "BYC_0004"
)

dataset_paths <- file.path(
  IEO_ROOT,
  datasets
)

dataset_check <- tibble(
  dataset = datasets,
  path = dataset_paths,
  exists = dir.exists(dataset_paths)
)

write_csv(
  dataset_check,
  file.path(
    OUTPUT_TABLES,
    "00_dataset_directory_check.csv"
  )
)

print(dataset_check)

if (any(!dataset_check$exists)) {
  stop(
    "At least one expected dataset directory is missing."
  )
}


# ============================================================
# 3. INVENTORY ALL RAW FILES
# ============================================================

all_raw_files <- list.files(
  IEO_ROOT,
  recursive = TRUE,
  full.names = TRUE
)

raw_inventory <- tibble(
  full_path = all_raw_files,
  relative_path = str_remove(
    all_raw_files,
    paste0(
      "^",
      stringr::fixed(IEO_ROOT),
      "/?"
    )
  ),
  file_name = basename(all_raw_files),
  extension = tools::file_ext(all_raw_files),
  size_bytes = file.info(all_raw_files)$size
) %>%
  mutate(
    dataset = case_when(
      str_detect(relative_path, "BYC_0002") ~ "BYC_0002",
      str_detect(relative_path, "BYC_0003") ~ "BYC_0003",
      str_detect(relative_path, "BYC_0004") ~ "BYC_0004",
      TRUE ~ NA_character_
    ),
    data_type = case_when(
      str_detect(relative_path, "fishingOperations") ~ "fishingOperations",
      str_detect(relative_path, "bycatchAggregated") ~ "bycatchAggregated",
      str_detect(relative_path, "bycatchSampledIndividuals") ~ "bycatchSampledIndividuals",
      str_detect(relative_path, "metadata") ~ "metadata",
      TRUE ~ "other"
    )
  )

write_csv(
  raw_inventory,
  file.path(
    OUTPUT_TABLES,
    "01_raw_file_inventory.csv"
  )
)


# ============================================================
# 4. FIND FISHING-OPERATIONS FILES
# ============================================================

find_fishing_operations_file <- function(dataset) {
  
  dataset_dir <- file.path(
    IEO_ROOT,
    dataset
  )
  
  files <- list.files(
    dataset_dir,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )
  
  candidates <- files[
    str_detect(
      basename(files),
      regex(
        "fishingOperations",
        ignore_case = TRUE
      )
    )
  ]
  
  candidates <- candidates[
    !str_detect(
      candidates,
      regex(
        "variable_explanation|metadata",
        ignore_case = TRUE
      )
    )
  ]
  
  if (length(candidates) != 1) {
    
    cat(
      "\n",
      dataset,
      " fishingOperations candidates:\n",
      sep = ""
    )
    
    print(candidates)
    
    stop(
      paste0(
        "Could not uniquely identify fishingOperations CSV for ",
        dataset,
        "."
      )
    )
  }
  
  candidates
}


file_0002 <- find_fishing_operations_file(
  "BYC_0002"
)

file_0003 <- find_fishing_operations_file(
  "BYC_0003"
)

file_0004 <- find_fishing_operations_file(
  "BYC_0004"
)

selected_files <- tibble(
  dataset = datasets,
  fishing_operations_file = c(
    file_0002,
    file_0003,
    file_0004
  )
)

write_csv(
  selected_files,
  file.path(
    OUTPUT_TABLES,
    "02_selected_fishingOperations_files.csv"
  )
)

print(selected_files)


# ============================================================
# 5. READ RAW DATA
# ============================================================

read_raw_csv <- function(path) {
  
  read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
}


ops_0002 <- read_raw_csv(
  file_0002
)

ops_0003 <- read_raw_csv(
  file_0003
)

ops_0004 <- read_raw_csv(
  file_0004
)


cat(
  "\nRAW dimensions:\n"
)

cat(
  "BYC_0002:",
  nrow(ops_0002),
  "rows x",
  ncol(ops_0002),
  "columns\n"
)

cat(
  "BYC_0003:",
  nrow(ops_0003),
  "rows x",
  ncol(ops_0003),
  "columns\n"
)

cat(
  "BYC_0004:",
  nrow(ops_0004),
  "rows x",
  ncol(ops_0004),
  "columns\n\n"
)


# ============================================================
# 6. RAW COLUMN INVENTORY
# ============================================================

column_inventory <- bind_rows(
  tibble(
    dataset = "BYC_0002",
    column_number = seq_along(names(ops_0002)),
    column_name = names(ops_0002),
    class = vapply(
      ops_0002,
      function(x) paste(class(x), collapse = ";"),
      character(1)
    )
  ),
  tibble(
    dataset = "BYC_0003",
    column_number = seq_along(names(ops_0003)),
    column_name = names(ops_0003),
    class = vapply(
      ops_0003,
      function(x) paste(class(x), collapse = ";"),
      character(1)
    )
  ),
  tibble(
    dataset = "BYC_0004",
    column_number = seq_along(names(ops_0004)),
    column_name = names(ops_0004),
    class = vapply(
      ops_0004,
      function(x) paste(class(x), collapse = ";"),
      character(1)
    )
  )
)

write_csv(
  column_inventory,
  file.path(
    OUTPUT_TABLES,
    "03_raw_column_inventory.csv"
  )
)


# ============================================================
# 7. HELPER TO IDENTIFY RAW VARIABLES
# ============================================================

find_column <- function(
    dat,
    candidates,
    required = TRUE
) {
  
  nm <- names(dat)
  
  exact <- nm[
    tolower(nm) %in% tolower(candidates)
  ]
  
  if (length(exact) == 1) {
    return(exact)
  }
  
  if (length(exact) > 1) {
    return(exact[1])
  }
  
  for (candidate in candidates) {
    
    hit <- nm[
      str_detect(
        tolower(nm),
        fixed(
          tolower(candidate)
        )
      )
    ]
    
    if (length(hit) >= 1) {
      return(hit[1])
    }
  }
  
  if (required) {
    
    stop(
      paste0(
        "Could not identify required column.\nCandidates: ",
        paste(
          candidates,
          collapse = ", "
        ),
        "\nAvailable columns:\n",
        paste(
          nm,
          collapse = "\n"
        )
      )
    )
  }
  
  NA_character_
}


# ============================================================
# 8. IDENTIFY IMPORTANT RAW COLUMNS
# ============================================================

identify_columns <- function(
    dat,
    dataset
) {
  
  operation_col <- find_column(
    dat,
    c(
      "operationID",
      "operation_id",
      "operationId",
      "fishingOperationID"
    ),
    required = FALSE
  )
  
  trip_col <- find_column(
    dat,
    c(
      "tripID",
      "trip_id",
      "tripId"
    ),
    required = FALSE
  )
  
  vessel_col <- find_column(
    dat,
    c(
      "vesselID",
      "vessel_id",
      "vesselId"
    ),
    required = FALSE
  )
  
  date_col <- find_column(
    dat,
    c(
      "date",
      "operationDate",
      "operation_date",
      "setDate",
      "fishingDate"
    ),
    required = FALSE
  )
  
  if (dataset == "BYC_0002") {
    
    lon_col <- find_column(
      dat,
      c(
        "longitude",
        "lon",
        "setLongitude",
        "set_lon",
        "shootLong",
        "shootLongitude"
      )
    )
    
    lat_col <- find_column(
      dat,
      c(
        "latitude",
        "lat",
        "setLatitude",
        "set_lat",
        "shootLat",
        "shootLatitude"
      )
    )
    
    return(
      list(
        operation = operation_col,
        trip = trip_col,
        vessel = vessel_col,
        date = date_col,
        lon = lon_col,
        lat = lat_col
      )
    )
  }
  
  set_lon1 <- find_column(
    dat,
    c(
      "set_lon1",
      "setLon1",
      "setLongitude1",
      "set_longitude_1",
      "setStartLongitude",
      "set_start_lon",
      "shootLong1"
    )
  )
  
  set_lat1 <- find_column(
    dat,
    c(
      "set_lat1",
      "setLat1",
      "setLatitude1",
      "set_latitude_1",
      "setStartLatitude",
      "set_start_lat",
      "shootLat1"
    )
  )
  
  set_lon2 <- find_column(
    dat,
    c(
      "set_lon2",
      "setLon2",
      "setLongitude2",
      "set_longitude_2",
      "setEndLongitude",
      "set_end_lon",
      "shootLong2"
    )
  )
  
  set_lat2 <- find_column(
    dat,
    c(
      "set_lat2",
      "setLat2",
      "setLatitude2",
      "set_latitude_2",
      "setEndLatitude",
      "set_end_lat",
      "shootLat2"
    )
  )
  
  haul_lon1 <- find_column(
    dat,
    c(
      "haul_lon1",
      "haulLon1",
      "haulLongitude1",
      "haul_longitude_1",
      "haulStartLongitude",
      "haul_start_lon"
    )
  )
  
  haul_lat1 <- find_column(
    dat,
    c(
      "haul_lat1",
      "haulLat1",
      "haulLatitude1",
      "haul_latitude_1",
      "haulStartLatitude",
      "haul_start_lat"
    )
  )
  
  haul_lon2 <- find_column(
    dat,
    c(
      "haul_lon2",
      "haulLon2",
      "haulLongitude2",
      "haul_longitude_2",
      "haulEndLongitude",
      "haul_end_lon"
    )
  )
  
  haul_lat2 <- find_column(
    dat,
    c(
      "haul_lat2",
      "haulLat2",
      "haulLatitude2",
      "haul_latitude_2",
      "haulEndLatitude",
      "haul_end_lat"
    )
  )
  
  list(
    operation = operation_col,
    trip = trip_col,
    vessel = vessel_col,
    date = date_col,
    set_lon1 = set_lon1,
    set_lat1 = set_lat1,
    set_lon2 = set_lon2,
    set_lat2 = set_lat2,
    haul_lon1 = haul_lon1,
    haul_lat1 = haul_lat1,
    haul_lon2 = haul_lon2,
    haul_lat2 = haul_lat2
  )
}


cols_0002 <- identify_columns(
  ops_0002,
  "BYC_0002"
)

cols_0003 <- identify_columns(
  ops_0003,
  "BYC_0003"
)

cols_0004 <- identify_columns(
  ops_0004,
  "BYC_0004"
)


# ============================================================
# 9. SAVE VARIABLE MAPPING
# ============================================================

mapping_to_table <- function(
    mapping,
    dataset
) {
  
  tibble(
    dataset = dataset,
    standard_variable = names(mapping),
    raw_variable = unlist(
      mapping,
      use.names = FALSE
    )
  )
}


variable_mapping <- bind_rows(
  mapping_to_table(
    cols_0002,
    "BYC_0002"
  ),
  mapping_to_table(
    cols_0003,
    "BYC_0003"
  ),
  mapping_to_table(
    cols_0004,
    "BYC_0004"
  )
)

write_csv(
  variable_mapping,
  file.path(
    OUTPUT_TABLES,
    "04_raw_variable_mapping.csv"
  )
)

print(variable_mapping)


# ============================================================
# 10. HELPER FOR OPTIONAL RAW VARIABLES
# ============================================================

extract_optional <- function(
    dat,
    column_name
) {
  
  if (
    length(column_name) == 0 ||
    is.na(column_name) ||
    !column_name %in% names(dat)
  ) {
    return(
      rep(
        NA_character_,
        nrow(dat)
      )
    )
  }
  
  as.character(
    dat[[column_name]]
  )
}


# ============================================================
# 11. PREPARE BYC_0002 RAW POINTS
# ============================================================

points_0002 <- tibble(
  dataset = "BYC_0002",
  raw_row = seq_len(
    nrow(ops_0002)
  ),
  operation_id_raw = extract_optional(
    ops_0002,
    cols_0002$operation
  ),
  trip_id_raw = extract_optional(
    ops_0002,
    cols_0002$trip
  ),
  vessel_id_raw = extract_optional(
    ops_0002,
    cols_0002$vessel
  ),
  date_raw = extract_optional(
    ops_0002,
    cols_0002$date
  ),
  lon = suppressWarnings(
    as.numeric(
      ops_0002[[cols_0002$lon]]
    )
  ),
  lat = suppressWarnings(
    as.numeric(
      ops_0002[[cols_0002$lat]]
    )
  )
) %>%
  mutate(
    valid_coordinate = is.finite(lon) &
      is.finite(lat) &
      lon >= -180 &
      lon <= 180 &
      lat >= -90 &
      lat <= 90
  )


# ============================================================
# 12. PREPARE RAW LONGLINE SEGMENTS
# ============================================================

prepare_longline_segments <- function(
    dat,
    dataset,
    cols
) {
  
  operation_id <- extract_optional(
    dat,
    cols$operation
  )
  
  if (all(is.na(operation_id))) {
    operation_id <- as.character(
      seq_len(
        nrow(dat)
      )
    )
  }
  
  trip_id <- extract_optional(
    dat,
    cols$trip
  )
  
  vessel_id <- extract_optional(
    dat,
    cols$vessel
  )
  
  date_raw <- extract_optional(
    dat,
    cols$date
  )
  
  set_segments <- tibble(
    dataset = dataset,
    raw_row = seq_len(
      nrow(dat)
    ),
    operation_id_raw = operation_id,
    trip_id_raw = trip_id,
    vessel_id_raw = vessel_id,
    date_raw = date_raw,
    geometry_type = "SET",
    lon1 = suppressWarnings(
      as.numeric(
        dat[[cols$set_lon1]]
      )
    ),
    lat1 = suppressWarnings(
      as.numeric(
        dat[[cols$set_lat1]]
      )
    ),
    lon2 = suppressWarnings(
      as.numeric(
        dat[[cols$set_lon2]]
      )
    ),
    lat2 = suppressWarnings(
      as.numeric(
        dat[[cols$set_lat2]]
      )
    )
  )
  
  haul_segments <- tibble(
    dataset = dataset,
    raw_row = seq_len(
      nrow(dat)
    ),
    operation_id_raw = operation_id,
    trip_id_raw = trip_id,
    vessel_id_raw = vessel_id,
    date_raw = date_raw,
    geometry_type = "HAUL",
    lon1 = suppressWarnings(
      as.numeric(
        dat[[cols$haul_lon1]]
      )
    ),
    lat1 = suppressWarnings(
      as.numeric(
        dat[[cols$haul_lat1]]
      )
    ),
    lon2 = suppressWarnings(
      as.numeric(
        dat[[cols$haul_lon2]]
      )
    ),
    lat2 = suppressWarnings(
      as.numeric(
        dat[[cols$haul_lat2]]
      )
    )
  )
  
  bind_rows(
    set_segments,
    haul_segments
  ) %>%
    mutate(
      valid_segment = is.finite(lon1) &
        is.finite(lat1) &
        is.finite(lon2) &
        is.finite(lat2) &
        lon1 >= -180 &
        lon1 <= 180 &
        lon2 >= -180 &
        lon2 <= 180 &
        lat1 >= -90 &
        lat1 <= 90 &
        lat2 >= -90 &
        lat2 <= 90
    )
}


segments_0003 <- prepare_longline_segments(
  ops_0003,
  "BYC_0003",
  cols_0003
)

segments_0004 <- prepare_longline_segments(
  ops_0004,
  "BYC_0004",
  cols_0004
)

segments_longline <- bind_rows(
  segments_0003,
  segments_0004
)


# ============================================================
# 13. SAVE BASIC RAW COORDINATE DIAGNOSTICS
# ============================================================

coordinate_summary <- bind_rows(
  points_0002 %>%
    summarise(
      dataset = "BYC_0002",
      geometry = "POINT",
      n_records = n(),
      n_valid = sum(
        valid_coordinate,
        na.rm = TRUE
      ),
      n_invalid = sum(
        !valid_coordinate,
        na.rm = TRUE
      ),
      min_lon = min(
        lon[valid_coordinate],
        na.rm = TRUE
      ),
      max_lon = max(
        lon[valid_coordinate],
        na.rm = TRUE
      ),
      min_lat = min(
        lat[valid_coordinate],
        na.rm = TRUE
      ),
      max_lat = max(
        lat[valid_coordinate],
        na.rm = TRUE
      )
    ),
  segments_longline %>%
    group_by(
      dataset,
      geometry_type
    ) %>%
    summarise(
      geometry = geometry_type,
      n_records = n(),
      n_valid = sum(
        valid_segment,
        na.rm = TRUE
      ),
      n_invalid = sum(
        !valid_segment,
        na.rm = TRUE
      ),
      min_lon = min(
        c(
          lon1[valid_segment],
          lon2[valid_segment]
        ),
        na.rm = TRUE
      ),
      max_lon = max(
        c(
          lon1[valid_segment],
          lon2[valid_segment]
        ),
        na.rm = TRUE
      ),
      min_lat = min(
        c(
          lat1[valid_segment],
          lat2[valid_segment]
        ),
        na.rm = TRUE
      ),
      max_lat = max(
        c(
          lat1[valid_segment],
          lat2[valid_segment]
        ),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    select(
      dataset,
      geometry,
      n_records,
      n_valid,
      n_invalid,
      min_lon,
      max_lon,
      min_lat,
      max_lat
    )
)

write_csv(
  coordinate_summary,
  file.path(
    OUTPUT_TABLES,
    "05_raw_coordinate_summary.csv"
  )
)


# ============================================================
# 14. HAVERSINE DISTANCE
# ============================================================

haversine_km <- function(
    lon1,
    lat1,
    lon2,
    lat2
) {
  
  radius <- 6371.0088
  
  lon1_rad <- lon1 * pi / 180
  lat1_rad <- lat1 * pi / 180
  lon2_rad <- lon2 * pi / 180
  lat2_rad <- lat2 * pi / 180
  
  dlon <- lon2_rad - lon1_rad
  dlat <- lat2_rad - lat1_rad
  
  a <- sin(
    dlat / 2
  )^2 +
    cos(
      lat1_rad
    ) *
    cos(
      lat2_rad
    ) *
    sin(
      dlon / 2
    )^2
  
  a <- pmin(
    pmax(
      a,
      0
    ),
    1
  )
  
  2 *
    radius *
    asin(
      sqrt(a)
    )
}


# ============================================================
# 15. SEGMENT LENGTHS
# ============================================================

segments_longline <- segments_longline %>%
  mutate(
    segment_length_km = if_else(
      valid_segment,
      haversine_km(
        lon1,
        lat1,
        lon2,
        lat2
      ),
      NA_real_
    )
  )


# ============================================================
# 16. OPERATION-LEVEL RAW GEOMETRY
# ============================================================

operation_geometry <- segments_longline %>%
  filter(
    valid_segment
  ) %>%
  select(
    dataset,
    raw_row,
    operation_id_raw,
    trip_id_raw,
    vessel_id_raw,
    date_raw,
    geometry_type,
    lon1,
    lat1,
    lon2,
    lat2,
    segment_length_km
  ) %>%
  pivot_wider(
    names_from = geometry_type,
    values_from = c(
      lon1,
      lat1,
      lon2,
      lat2,
      segment_length_km
    )
  )


# ============================================================
# 17. SET–HAUL DISPLACEMENT
# ============================================================

operation_geometry <- operation_geometry %>%
  mutate(
    set_haul_displacement_km = if_else(
      is.finite(lon2_SET) &
        is.finite(lat2_SET) &
        is.finite(lon1_HAUL) &
        is.finite(lat1_HAUL),
      haversine_km(
        lon2_SET,
        lat2_SET,
        lon1_HAUL,
        lat1_HAUL
      ),
      NA_real_
    )
  )


# ============================================================
# 18. APPROXIMATE MCP AREA
#
# Four endpoints are projected locally using an equirectangular
# approximation and their convex-hull area is calculated.
#
# This is exploratory only.
# ============================================================

polygon_area_xy <- function(
    x,
    y
) {
  
  ok <- is.finite(x) &
    is.finite(y)
  
  x <- x[ok]
  y <- y[ok]
  
  if (length(x) < 3) {
    return(NA_real_)
  }
  
  coords <- unique(
    cbind(
      x,
      y
    )
  )
  
  if (nrow(coords) < 3) {
    return(0)
  }
  
  hull_index <- chull(
    coords[, 1],
    coords[, 2]
  )
  
  hull <- coords[
    hull_index,
    ,
    drop = FALSE
  ]
  
  hull <- rbind(
    hull,
    hull[1, ]
  )
  
  abs(
    sum(
      hull[-1, 1] * hull[-nrow(hull), 2] -
        hull[-nrow(hull), 1] * hull[-1, 2]
    )
  ) / 2
}


calculate_mcp_km2 <- function(
    lon_set1,
    lat_set1,
    lon_set2,
    lat_set2,
    lon_haul1,
    lat_haul1,
    lon_haul2,
    lat_haul2
) {
  
  lon <- c(
    lon_set1,
    lon_set2,
    lon_haul1,
    lon_haul2
  )
  
  lat <- c(
    lat_set1,
    lat_set2,
    lat_haul1,
    lat_haul2
  )
  
  ok <- is.finite(lon) &
    is.finite(lat)
  
  lon <- lon[ok]
  lat <- lat[ok]
  
  if (length(lon) < 3) {
    return(NA_real_)
  }
  
  lat0 <- mean(
    lat
  ) * pi / 180
  
  lon0 <- mean(
    lon
  )
  
  lat_mean <- mean(
    lat
  )
  
  x <- (
    lon - lon0
  ) *
    111.320 *
    cos(lat0)
  
  y <- (
    lat - lat_mean
  ) *
    110.574
  
  polygon_area_xy(
    x,
    y
  )
}


operation_geometry$mcp_area_km2 <- vapply(
  seq_len(
    nrow(operation_geometry)
  ),
  function(i) {
    
    calculate_mcp_km2(
      operation_geometry$lon1_SET[i],
      operation_geometry$lat1_SET[i],
      operation_geometry$lon2_SET[i],
      operation_geometry$lat2_SET[i],
      operation_geometry$lon1_HAUL[i],
      operation_geometry$lat1_HAUL[i],
      operation_geometry$lon2_HAUL[i],
      operation_geometry$lat2_HAUL[i]
    )
  },
  numeric(1)
)


# ============================================================
# 19. DATASET-SPECIFIC P99 THRESHOLDS
#
# IMPORTANT:
# These are NOT exclusion rules.
# They only identify extreme geometries for visual inspection.
# ============================================================

qc_thresholds <- operation_geometry %>%
  group_by(
    dataset
  ) %>%
  summarise(
    p99_set_length_km = quantile(
      segment_length_km_SET,
      probs = 0.99,
      na.rm = TRUE,
      names = FALSE
    ),
    p99_haul_length_km = quantile(
      segment_length_km_HAUL,
      probs = 0.99,
      na.rm = TRUE,
      names = FALSE
    ),
    p99_set_haul_displacement_km = quantile(
      set_haul_displacement_km,
      probs = 0.99,
      na.rm = TRUE,
      names = FALSE
    ),
    p99_mcp_area_km2 = quantile(
      mcp_area_km2,
      probs = 0.99,
      na.rm = TRUE,
      names = FALSE
    ),
    .groups = "drop"
  )

write_csv(
  qc_thresholds,
  file.path(
    OUTPUT_QC,
    "01_QC_thresholds_P99.csv"
  )
)


# ============================================================
# 20. APPLY EXPLORATORY QC FLAGS
# ============================================================

operation_geometry <- operation_geometry %>%
  left_join(
    qc_thresholds,
    by = "dataset"
  ) %>%
  mutate(
    flag_set_p99 = is.finite(
      segment_length_km_SET
    ) &
      segment_length_km_SET >
      p99_set_length_km,
    
    flag_haul_p99 = is.finite(
      segment_length_km_HAUL
    ) &
      segment_length_km_HAUL >
      p99_haul_length_km,
    
    flag_displacement_p99 = is.finite(
      set_haul_displacement_km
    ) &
      set_haul_displacement_km >
      p99_set_haul_displacement_km,
    
    flag_mcp_p99 = is.finite(
      mcp_area_km2
    ) &
      mcp_area_km2 >
      p99_mcp_area_km2,
    
    spatial_qc_flag =
      flag_set_p99 |
      flag_haul_p99 |
      flag_displacement_p99 |
      flag_mcp_p99
  )


# ============================================================
# 21. QC SUMMARY
# ============================================================

qc_summary <- operation_geometry %>%
  group_by(
    dataset
  ) %>%
  summarise(
    n_operations = n(),
    n_set_p99 = sum(
      flag_set_p99,
      na.rm = TRUE
    ),
    n_haul_p99 = sum(
      flag_haul_p99,
      na.rm = TRUE
    ),
    n_displacement_p99 = sum(
      flag_displacement_p99,
      na.rm = TRUE
    ),
    n_mcp_p99 = sum(
      flag_mcp_p99,
      na.rm = TRUE
    ),
    n_any_p99 = sum(
      spatial_qc_flag,
      na.rm = TRUE
    ),
    pct_any_p99 = 100 *
      mean(
        spatial_qc_flag,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

write_csv(
  qc_summary,
  file.path(
    OUTPUT_QC,
    "02_QC_summary.csv"
  )
)


# ============================================================
# 22. SAVE FLAGGED OPERATIONS
# ============================================================

flagged_operations <- operation_geometry %>%
  filter(
    spatial_qc_flag
  )

write_csv(
  flagged_operations,
  file.path(
    OUTPUT_QC,
    "03_flagged_operations.csv"
  )
)


# ============================================================
# 23. QC BY TRIP
# ============================================================

qc_by_trip <- operation_geometry %>%
  group_by(
    dataset,
    trip_id_raw
  ) %>%
  summarise(
    n_operations = n(),
    n_flagged = sum(
      spatial_qc_flag,
      na.rm = TRUE
    ),
    pct_flagged = 100 *
      mean(
        spatial_qc_flag,
        na.rm = TRUE
      ),
    max_set_km = max(
      segment_length_km_SET,
      na.rm = TRUE
    ),
    max_haul_km = max(
      segment_length_km_HAUL,
      na.rm = TRUE
    ),
    max_displacement_km = max(
      set_haul_displacement_km,
      na.rm = TRUE
    ),
    max_mcp_km2 = max(
      mcp_area_km2,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      starts_with("max_"),
      ~ ifelse(
        is.infinite(.x),
        NA_real_,
        .x
      )
    )
  ) %>%
  arrange(
    dataset,
    desc(pct_flagged),
    desc(n_flagged)
  )

write_csv(
  qc_by_trip,
  file.path(
    OUTPUT_QC,
    "04_QC_by_trip.csv"
  )
)


# ============================================================
# 24. TOP EXTREME OPERATIONS
# ============================================================

top_set <- operation_geometry %>%
  arrange(
    desc(
      segment_length_km_SET
    )
  ) %>%
  select(
    dataset,
    operation_id_raw,
    trip_id_raw,
    segment_length_km_SET,
    spatial_qc_flag
  ) %>%
  slice_head(
    n = 100
  )

top_haul <- operation_geometry %>%
  arrange(
    desc(
      segment_length_km_HAUL
    )
  ) %>%
  select(
    dataset,
    operation_id_raw,
    trip_id_raw,
    segment_length_km_HAUL,
    spatial_qc_flag
  ) %>%
  slice_head(
    n = 100
  )

top_displacement <- operation_geometry %>%
  arrange(
    desc(
      set_haul_displacement_km
    )
  ) %>%
  select(
    dataset,
    operation_id_raw,
    trip_id_raw,
    set_haul_displacement_km,
    spatial_qc_flag
  ) %>%
  slice_head(
    n = 100
  )

top_mcp <- operation_geometry %>%
  arrange(
    desc(
      mcp_area_km2
    )
  ) %>%
  select(
    dataset,
    operation_id_raw,
    trip_id_raw,
    mcp_area_km2,
    spatial_qc_flag
  ) %>%
  slice_head(
    n = 100
  )

write_csv(
  top_set,
  file.path(
    OUTPUT_QC,
    "05_top_SET_distances.csv"
  )
)

write_csv(
  top_haul,
  file.path(
    OUTPUT_QC,
    "06_top_HAUL_distances.csv"
  )
)

write_csv(
  top_displacement,
  file.path(
    OUTPUT_QC,
    "07_top_SET_HAUL_displacements.csv"
  )
)

write_csv(
  top_mcp,
  file.path(
    OUTPUT_QC,
    "08_top_MCP_areas.csv"
  )
)


# ============================================================
# 25. WORLD MAP
# ============================================================

world <- map_data(
  "world"
)


# ============================================================
# 26. AUTOMATIC MAP LIMITS
# ============================================================

get_map_limits <- function(
    lon,
    lat,
    margin_fraction = 0.06,
    min_margin_lon = 0.5,
    min_margin_lat = 0.5
) {
  
  ok <- is.finite(lon) &
    is.finite(lat) &
    lon >= -180 &
    lon <= 180 &
    lat >= -90 &
    lat <= 90
  
  lon <- lon[ok]
  lat <- lat[ok]
  
  if (length(lon) == 0) {
    stop(
      "No valid coordinates available for map limits."
    )
  }
  
  lon_range <- range(
    lon,
    na.rm = TRUE
  )
  
  lat_range <- range(
    lat,
    na.rm = TRUE
  )
  
  lon_span <- diff(
    lon_range
  )
  
  lat_span <- diff(
    lat_range
  )
  
  lon_margin <- max(
    lon_span * margin_fraction,
    min_margin_lon
  )
  
  lat_margin <- max(
    lat_span * margin_fraction,
    min_margin_lat
  )
  
  list(
    xlim = c(
      lon_range[1] - lon_margin,
      lon_range[2] + lon_margin
    ),
    ylim = c(
      lat_range[1] - lat_margin,
      lat_range[2] + lat_margin
    )
  )
}


base_map_zoomed <- function(
    lon,
    lat,
    margin_fraction = 0.06,
    min_margin_lon = 0.5,
    min_margin_lat = 0.5
) {
  
  limits <- get_map_limits(
    lon = lon,
    lat = lat,
    margin_fraction = margin_fraction,
    min_margin_lon = min_margin_lon,
    min_margin_lat = min_margin_lat
  )
  
  ggplot() +
    geom_polygon(
      data = world,
      aes(
        x = long,
        y = lat,
        group = group
      ),
      fill = "grey96",
      colour = "grey70",
      linewidth = 0.2
    ) +
    coord_quickmap(
      xlim = limits$xlim,
      ylim = limits$ylim,
      expand = FALSE
    ) +
    theme_minimal(
      base_size = 11
    ) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        linewidth = 0.15,
        colour = "grey90"
      ),
      plot.title = element_text(
        face = "bold"
      ),
      axis.title = element_text(
        colour = "black"
      ),
      axis.text = element_text(
        colour = "grey30"
      )
    ) +
    labs(
      x = "Longitude",
      y = "Latitude"
    )
}


# ============================================================
# 27. RAW PURSE-SEINE MAP
# ============================================================

plot_purse_seine <- function(
    dat
) {
  
  plot_dat <- dat %>%
    filter(
      valid_coordinate
    )
  
  base_map_zoomed(
    lon = plot_dat$lon,
    lat = plot_dat$lat
  ) +
    geom_point(
      data = plot_dat,
      aes(
        x = lon,
        y = lat
      ),
      size = 0.35,
      alpha = 0.40,
      colour = "black"
    ) +
    labs(
      title = "BYC_0002 — Purse seine",
      subtitle = paste0(
        nrow(plot_dat),
        " raw fishing operations | no exclusions"
      )
    )
}


p_0002 <- plot_purse_seine(
  points_0002
)

ggsave(
  filename = file.path(
    OUTPUT_MAPS,
    "01_BYC_0002_raw_operations.png"
  ),
  plot = p_0002,
  width = 10,
  height = 8,
  dpi = 300
)


# ============================================================
# 28. RAW LONGLINE MAP
#
# SET   = solid
# HAUL  = dashed
#
# Nothing filtered except impossible geographic coordinates
# that cannot be drawn on a longitude/latitude map.
# ============================================================

plot_raw_longline <- function(
    segments,
    dataset_name
) {
  
  dat <- segments %>%
    filter(
      dataset == dataset_name,
      valid_segment
    )
  
  map_lon <- c(
    dat$lon1,
    dat$lon2
  )
  
  map_lat <- c(
    dat$lat1,
    dat$lat2
  )
  
  base_map_zoomed(
    lon = map_lon,
    lat = map_lat,
    margin_fraction = 0.04,
    min_margin_lon = 0.5,
    min_margin_lat = 0.5
  ) +
    geom_segment(
      data = dat %>%
        filter(
          geometry_type == "SET"
        ),
      aes(
        x = lon1,
        y = lat1,
        xend = lon2,
        yend = lat2
      ),
      linewidth = 0.18,
      alpha = 0.30,
      colour = "black"
    ) +
    geom_segment(
      data = dat %>%
        filter(
          geometry_type == "HAUL"
        ),
      aes(
        x = lon1,
        y = lat1,
        xend = lon2,
        yend = lat2
      ),
      linewidth = 0.18,
      alpha = 0.30,
      colour = "grey35",
      linetype = "dashed"
    ) +
    geom_point(
      data = dat,
      aes(
        x = lon1,
        y = lat1
      ),
      size = 0.12,
      alpha = 0.30,
      colour = "black"
    ) +
    geom_point(
      data = dat,
      aes(
        x = lon2,
        y = lat2
      ),
      size = 0.12,
      alpha = 0.30,
      colour = "black"
    ) +
    labs(
      title = paste0(
        dataset_name,
        " — RAW fishing-operation geometry"
      ),
      subtitle = paste0(
        "Original coordinates | solid = SET | dashed = HAUL | ",
        "no QC exclusions"
      )
    )
}


p_0003_raw <- plot_raw_longline(
  segments_longline,
  "BYC_0003"
)

p_0004_raw <- plot_raw_longline(
  segments_longline,
  "BYC_0004"
)

ggsave(
  filename = file.path(
    OUTPUT_MAPS,
    "02_BYC_0003_RAW_SET_HAUL.png"
  ),
  plot = p_0003_raw,
  width = 9,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(
    OUTPUT_MAPS,
    "03_BYC_0004_RAW_SET_HAUL.png"
  ),
  plot = p_0004_raw,
  width = 9,
  height = 9,
  dpi = 300
)


# ============================================================
# 29. OPERATION-COLOURED RAW MAP
#
# Useful to understand which SET and HAUL belong to the
# same operation.
# ============================================================

plot_operations_coloured <- function(
    segments,
    dataset_name
) {
  
  dat <- segments %>%
    filter(
      dataset == dataset_name,
      valid_segment
    )
  
  map_lon <- c(
    dat$lon1,
    dat$lon2
  )
  
  map_lat <- c(
    dat$lat1,
    dat$lat2
  )
  
  base_map_zoomed(
    lon = map_lon,
    lat = map_lat,
    margin_fraction = 0.04,
    min_margin_lon = 0.5,
    min_margin_lat = 0.5
  ) +
    geom_segment(
      data = dat,
      aes(
        x = lon1,
        y = lat1,
        xend = lon2,
        yend = lat2,
        colour = operation_id_raw,
        linetype = geometry_type,
        group = interaction(
          operation_id_raw,
          geometry_type
        )
      ),
      linewidth = 0.35,
      alpha = 0.65
    ) +
    scale_linetype_manual(
      values = c(
        SET = "solid",
        HAUL = "dashed"
      )
    ) +
    guides(
      colour = "none"
    ) +
    labs(
      title = paste0(
        dataset_name,
        " — RAW operations"
      ),
      subtitle = paste0(
        "Colour = operation | solid = SET | dashed = HAUL"
      ),
      linetype = "Geometry"
    )
}


p_0003_colour <- plot_operations_coloured(
  segments_longline,
  "BYC_0003"
)

p_0004_colour <- plot_operations_coloured(
  segments_longline,
  "BYC_0004"
)

ggsave(
  filename = file.path(
    OUTPUT_MAPS,
    "04_BYC_0003_RAW_operations_coloured.png"
  ),
  plot = p_0003_colour,
  width = 9,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(
    OUTPUT_MAPS,
    "05_BYC_0004_RAW_operations_coloured.png"
  ),
  plot = p_0004_colour,
  width = 9,
  height = 9,
  dpi = 300
)


# ============================================================
# 30. QC MAP
# ============================================================

make_qc_map <- function(
    segments,
    metrics,
    dataset_name
) {
  
  dat <- segments %>%
    filter(
      dataset == dataset_name,
      valid_segment
    ) %>%
    left_join(
      metrics %>%
        select(
          dataset,
          raw_row,
          spatial_qc_flag
        ),
      by = c(
        "dataset",
        "raw_row"
      )
    )
  
  normal <- dat %>%
    filter(
      !spatial_qc_flag |
        is.na(spatial_qc_flag)
    )
  
  flagged <- dat %>%
    filter(
      spatial_qc_flag
    )
  
  map_lon <- c(
    dat$lon1,
    dat$lon2
  )
  
  map_lat <- c(
    dat$lat1,
    dat$lat2
  )
  
  base_map_zoomed(
    lon = map_lon,
    lat = map_lat,
    margin_fraction = 0.04,
    min_margin_lon = 0.5,
    min_margin_lat = 0.5
  ) +
    geom_segment(
      data = normal %>%
        filter(
          geometry_type == "SET"
        ),
      aes(
        x = lon1,
        y = lat1,
        xend = lon2,
        yend = lat2
      ),
      linewidth = 0.16,
      alpha = 0.20,
      colour = "grey30"
    ) +
    geom_segment(
      data = normal %>%
        filter(
          geometry_type == "HAUL"
        ),
      aes(
        x = lon1,
        y = lat1,
        xend = lon2,
        yend = lat2
      ),
      linewidth = 0.16,
      alpha = 0.20,
      colour = "grey30",
      linetype = "dashed"
    ) +
    geom_segment(
      data = flagged %>%
        filter(
          geometry_type == "SET"
        ),
      aes(
        x = lon1,
        y = lat1,
        xend = lon2,
        yend = lat2
      ),
      linewidth = 0.65,
      alpha = 0.95,
      colour = "red"
    ) +
    geom_segment(
      data = flagged %>%
        filter(
          geometry_type == "HAUL"
        ),
      aes(
        x = lon1,
        y = lat1,
        xend = lon2,
        yend = lat2
      ),
      linewidth = 0.65,
      alpha = 0.95,
      colour = "red",
      linetype = "dashed"
    ) +
    labs(
      title = paste0(
        dataset_name,
        " — exploratory spatial QC"
      ),
      subtitle = paste0(
        "Red = operation with ≥1 dataset-specific P99 flag | ",
        "solid = SET | dashed = HAUL"
      )
    )
}


p_0003_qc <- make_qc_map(
  segments_longline,
  operation_geometry,
  "BYC_0003"
)

p_0004_qc <- make_qc_map(
  segments_longline,
  operation_geometry,
  "BYC_0004"
)

ggsave(
  filename = file.path(
    OUTPUT_MAPS,
    "06_BYC_0003_P99_spatial_QC.png"
  ),
  plot = p_0003_qc,
  width = 9,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(
    OUTPUT_MAPS,
    "07_BYC_0004_P99_spatial_QC.png"
  ),
  plot = p_0004_qc,
  width = 9,
  height = 9,
  dpi = 300
)


# ============================================================
# 31. COMBINED RAW COVERAGE FIGURE
# ============================================================

combined_raw <- (
  p_0002 |
    p_0003_raw |
    p_0004_raw
) +
  plot_annotation(
    title = "RAW spatial coverage of fishing operations",
    subtitle = paste0(
      "BYC_0002 = purse-seine operation points | ",
      "BYC_0003 and BYC_0004 = original SET and HAUL geometries"
    )
  )

ggsave(
  filename = file.path(
    OUTPUT_FIGURES,
    "01_RAW_spatial_coverage_all_datasets.png"
  ),
  plot = combined_raw,
  width = 18,
  height = 7,
  dpi = 300
)


# ============================================================
# 32. DISTRIBUTIONS OF RAW SPATIAL METRICS
# ============================================================

metrics_long <- operation_geometry %>%
  select(
    dataset,
    segment_length_km_SET,
    segment_length_km_HAUL,
    set_haul_displacement_km,
    mcp_area_km2
  ) %>%
  pivot_longer(
    cols = c(
      segment_length_km_SET,
      segment_length_km_HAUL,
      set_haul_displacement_km,
      mcp_area_km2
    ),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      segment_length_km_SET = "SET length (km)",
      segment_length_km_HAUL = "HAUL length (km)",
      set_haul_displacement_km = "SET–HAUL displacement (km)",
      mcp_area_km2 = "MCP area (km²)"
    )
  )


p_metrics <- metrics_long %>%
  filter(
    is.finite(value),
    value > 0
  ) %>%
  ggplot(
    aes(
      x = value
    )
  ) +
  geom_histogram(
    bins = 50,
    fill = "grey45",
    colour = "white",
    linewidth = 0.15
  ) +
  scale_x_log10() +
  facet_grid(
    dataset ~ metric,
    scales = "free"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      face = "bold"
    )
  ) +
  labs(
    title = "RAW spatial characteristics of longline operations",
    subtitle = "Log10 x-axis | no operations removed",
    x = "Value",
    y = "Number of operations"
  )

ggsave(
  filename = file.path(
    OUTPUT_FIGURES,
    "02_RAW_spatial_metric_distributions.png"
  ),
  plot = p_metrics,
  width = 16,
  height = 8,
  dpi = 300
)


# ============================================================
# 33. P99 THRESHOLD FIGURE
# ============================================================

threshold_long <- qc_thresholds %>%
  pivot_longer(
    cols = -dataset,
    names_to = "threshold",
    values_to = "threshold_value"
  ) %>%
  mutate(
    metric = recode(
      threshold,
      p99_set_length_km = "SET length (km)",
      p99_haul_length_km = "HAUL length (km)",
      p99_set_haul_displacement_km = "SET–HAUL displacement (km)",
      p99_mcp_area_km2 = "MCP area (km²)"
    )
  ) %>%
  select(
    dataset,
    metric,
    threshold_value
  )

metrics_threshold_plot <- metrics_long %>%
  left_join(
    threshold_long,
    by = c(
      "dataset",
      "metric"
    )
  ) %>%
  filter(
    is.finite(value),
    value > 0
  )

p_p99 <- ggplot(
  metrics_threshold_plot,
  aes(
    x = value
  )
) +
  geom_histogram(
    bins = 50,
    fill = "grey45",
    colour = "white",
    linewidth = 0.15
  ) +
  geom_vline(
    aes(
      xintercept = threshold_value
    ),
    colour = "red",
    linetype = "dashed",
    linewidth = 0.6
  ) +
  scale_x_log10() +
  facet_grid(
    dataset ~ metric,
    scales = "free"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      face = "bold"
    )
  ) +
  labs(
    title = "Exploratory P99 spatial QC",
    subtitle = "Red dashed line = dataset-specific 99th percentile | no removal",
    x = "Value",
    y = "Number of operations"
  )

ggsave(
  filename = file.path(
    OUTPUT_FIGURES,
    "03_P99_spatial_QC_distributions.png"
  ),
  plot = p_p99,
  width = 16,
  height = 8,
  dpi = 300
)


# ============================================================
# 34. TRIP DIAGNOSTIC FUNCTION
#
# Tight automatic zoom around each trip.
# Operations have different colours.
# SET = solid
# HAUL = dashed
# ============================================================

plot_trip_diagnostic <- function(
    dataset_name,
    trip_id,
    segments,
    metrics
) {
  
  dat <- segments %>%
    filter(
      dataset == dataset_name,
      trip_id_raw == trip_id,
      valid_segment
    ) %>%
    left_join(
      metrics %>%
        select(
          dataset,
          raw_row,
          spatial_qc_flag
        ),
      by = c(
        "dataset",
        "raw_row"
      )
    )
  
  if (nrow(dat) == 0) {
    return(NULL)
  }
  
  map_lon <- c(
    dat$lon1,
    dat$lon2
  )
  
  map_lat <- c(
    dat$lat1,
    dat$lat2
  )
  
  n_operations <- n_distinct(
    dat$raw_row
  )
  
  n_flagged <- dat %>%
    distinct(
      raw_row,
      spatial_qc_flag
    ) %>%
    summarise(
      n = sum(
        spatial_qc_flag,
        na.rm = TRUE
      )
    ) %>%
    pull(n)
  
  base_map_zoomed(
    lon = map_lon,
    lat = map_lat,
    margin_fraction = 0.08,
    min_margin_lon = 0.15,
    min_margin_lat = 0.15
  ) +
    geom_segment(
      data = dat,
      aes(
        x = lon1,
        y = lat1,
        xend = lon2,
        yend = lat2,
        colour = factor(raw_row),
        linetype = geometry_type,
        group = interaction(
          raw_row,
          geometry_type
        )
      ),
      linewidth = 0.75,
      alpha = 0.85
    ) +
    geom_point(
      data = dat,
      aes(
        x = lon1,
        y = lat1,
        colour = factor(raw_row)
      ),
      size = 0.65,
      alpha = 0.90
    ) +
    geom_point(
      data = dat,
      aes(
        x = lon2,
        y = lat2,
        colour = factor(raw_row)
      ),
      size = 0.65,
      alpha = 0.90
    ) +
    scale_linetype_manual(
      values = c(
        SET = "solid",
        HAUL = "dashed"
      )
    ) +
    guides(
      colour = "none"
    ) +
    labs(
      title = paste0(
        dataset_name,
        " — trip ",
        trip_id
      ),
      subtitle = paste0(
        n_operations,
        " operations | ",
        n_flagged,
        " P99 flagged | colour = operation | ",
        "solid = SET | dashed = HAUL"
      ),
      linetype = "Geometry"
    )
}


# ============================================================
# 35. AUTOMATIC DIAGNOSTIC MAPS FOR EXTREME TRIPS
# ============================================================

top_trips <- qc_by_trip %>%
  filter(
    n_flagged > 0,
    !is.na(trip_id_raw)
  ) %>%
  group_by(
    dataset
  ) %>%
  arrange(
    desc(pct_flagged),
    desc(n_flagged),
    .by_group = TRUE
  ) %>%
  slice_head(
    n = 6
  ) %>%
  ungroup()


if (nrow(top_trips) > 0) {
  
  for (i in seq_len(nrow(top_trips))) {
    
    this_dataset <- top_trips$dataset[i]
    this_trip <- top_trips$trip_id_raw[i]
    
    this_plot <- plot_trip_diagnostic(
      dataset_name = this_dataset,
      trip_id = this_trip,
      segments = segments_longline,
      metrics = operation_geometry
    )
    
    if (!is.null(this_plot)) {
      
      safe_trip <- str_replace_all(
        as.character(this_trip),
        "[^A-Za-z0-9_-]",
        "_"
      )
      
      ggsave(
        filename = file.path(
          OUTPUT_DIAGNOSTICS,
          paste0(
            this_dataset,
            "_trip_",
            safe_trip,
            "_RAW_diagnostic.png"
          )
        ),
        plot = this_plot,
        width = 9,
        height = 7,
        dpi = 300
      )
    }
  }
}


# ============================================================
# 36. SAVE RAW EXPLORATORY OBJECTS
#
# These are derived copies for convenience only.
# The original CSV files remain untouched.
# ============================================================

saveRDS(
  points_0002,
  file.path(
    OUTPUT_TABLES,
    "BYC_0002_raw_points.rds"
  )
)

saveRDS(
  segments_0003,
  file.path(
    OUTPUT_TABLES,
    "BYC_0003_raw_segments.rds"
  )
)

saveRDS(
  segments_0004,
  file.path(
    OUTPUT_TABLES,
    "BYC_0004_raw_segments.rds"
  )
)

saveRDS(
  operation_geometry,
  file.path(
    OUTPUT_TABLES,
    "longline_raw_operation_geometry.rds"
  )
)


# ============================================================
# 37. SAVE SESSION INFORMATION
# ============================================================

capture.output(
  sessionInfo(),
  file = file.path(
    OUTPUT_ROOT,
    "sessionInfo.txt"
  )
)


# ============================================================
# 38. FINAL CONSOLE SUMMARY
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "00_raw_exploration.R COMPLETED\n",
  "============================================================\n",
  sep = ""
)

cat(
  "\nRaw operations:\n"
)

cat(
  "BYC_0002:",
  nrow(ops_0002),
  "\n"
)

cat(
  "BYC_0003:",
  nrow(ops_0003),
  "\n"
)

cat(
  "BYC_0004:",
  nrow(ops_0004),
  "\n"
)

cat(
  "\nExploratory longline QC:\n"
)

print(
  qc_summary
)

cat(
  "\nIMPORTANT:\n",
  "No operations have been removed.\n",
  "No coordinates have been corrected.\n",
  "P99 flags are exploratory only.\n",
  "The original RAW CSV files remain untouched.\n",
  sep = ""
)

cat(
  "\nOutputs written to:\n",
  OUTPUT_ROOT,
  "\n\n",
  sep = ""
)

cat(
  "Next step after reviewing these outputs:\n",
  "01_standardize_data.R\n\n"
)