# ============================================================
# REDUCE — MESOSCALE MEGAFAUNA BYCATCH
#
# 01_prepare_IEO_data.R
#
# PURPOSE
# ============================================================
#
# Organise all raw IEO fishing-operation and bycatch tables
# into explicit analytical blocks:
#
#   CORE
#   ATTRIBUTES
#   QC
#   AUXILIARY
#
# IMPORTANT PRINCIPLES
# ============================================================
#
# - Raw files are never modified.
# - No rows are removed.
# - No dataset is forced to share variables with another.
# - Purse seine and longline retain their original structures.
# - Every raw variable must be explicitly classified.
# - operationID preserves relationships between tables.
# - CSV and RDS versions are saved.
# - If a new/unclassified raw variable appears, the script stops.
#
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

required_packages <- c(
  "here",
  "readr",
  "dplyr",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing packages: ",
      paste(missing_packages, collapse = ", ")
    )
  )
}

library(here)
library(readr)
library(dplyr)
library(tibble)


# ============================================================
# 1. PATHS
# ============================================================

INPUT_DIR <- here(
  "00inputOutput",
  "00input",
  "00IEO"
)

OUTPUT_DIR <- here(
  "00inputOutput",
  "00output",
  "01_IEO_data"
)

METADATA_DIR <- file.path(
  OUTPUT_DIR,
  "metadata"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  METADATA_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 2. EXPECTED RAW STRUCTURE
# ============================================================

raw_structure <- tribble(
  ~dataset,   ~data_type,                   ~folder,
  
  "BYC_0002", "fishingOperations",          "fishingOperations",
  "BYC_0002", "bycatchAggregated",          "bycatchAggregated",
  "BYC_0002", "bycatchSampledIndividuals",  "bycatchSampledIndividuals",
  
  "BYC_0003", "fishingOperations",          "fishingOperations",
  "BYC_0003", "bycatch",                    "bycatch",
  
  "BYC_0004", "fishingOperations",          "fishingOperations",
  "BYC_0004", "bycatch",                    "bycatch"
)


# ============================================================
# 3. FIND RAW CSV
# ============================================================

find_raw_csv <- function(dataset, folder) {
  
  folder_path <- file.path(
    INPUT_DIR,
    dataset,
    folder
  )
  
  if (!dir.exists(folder_path)) {
    return(NA_character_)
  }
  
  files <- list.files(
    folder_path,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  files <- files[
    !grepl(
      "variable_explanation|metadata",
      basename(files),
      ignore.case = TRUE
    )
  ]
  
  if (length(files) == 0) {
    return(NA_character_)
  }
  
  if (length(files) > 1) {
    
    cat(
      "\nMultiple candidate CSV files found in:\n",
      folder_path,
      "\n\n",
      sep = ""
    )
    
    print(files)
    
    stop(
      paste0(
        "More than one raw CSV candidate for ",
        dataset,
        " / ",
        folder
      )
    )
  }
  
  files[1]
}


# ============================================================
# 4. RAW FILE INVENTORY
# ============================================================

raw_inventory <- raw_structure

raw_inventory$path <- NA_character_

for (i in seq_len(nrow(raw_inventory))) {
  
  raw_inventory$path[i] <- find_raw_csv(
    dataset = raw_inventory$dataset[i],
    folder = raw_inventory$folder[i]
  )
}

raw_inventory <- raw_inventory |>
  mutate(
    exists = !is.na(path)
  )

write_csv(
  raw_inventory,
  file.path(
    METADATA_DIR,
    "raw_file_inventory.csv"
  )
)

cat(
  "\n============================================================\n",
  "RAW FILE INVENTORY\n",
  "============================================================\n\n",
  sep = ""
)

print(
  raw_inventory,
  n = Inf
)


# ============================================================
# 5. CHECK EXPECTED RAW FILES
# ============================================================

missing_raw <- raw_inventory |>
  filter(
    !exists
  )

if (nrow(missing_raw) > 0) {
  
  cat(
    "\nEXPECTED RAW FILES NOT FOUND:\n\n"
  )
  
  print(
    missing_raw,
    n = Inf
  )
  
  stop(
    "Expected raw files are missing. Check 00input/00IEO."
  )
}


# ============================================================
# 6. READ ALL RAW TABLES
# ============================================================

raw_objects <- list()

for (i in seq_len(nrow(raw_inventory))) {
  
  dataset <- raw_inventory$dataset[i]
  data_type <- raw_inventory$data_type[i]
  path <- raw_inventory$path[i]
  
  key <- paste(
    dataset,
    data_type,
    sep = "__"
  )
  
  cat(
    "\nReading ",
    dataset,
    " / ",
    data_type,
    "...\n",
    sep = ""
  )
  
  x <- read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
  
  raw_objects[[key]] <- x
  
  cat(
    "  ",
    nrow(x),
    " rows | ",
    ncol(x),
    " variables\n",
    sep = ""
  )
}


# ============================================================
# 7. RAW SUMMARY
# ============================================================

raw_summary_list <- list()

for (key in names(raw_objects)) {
  
  parts <- strsplit(
    key,
    "__",
    fixed = TRUE
  )[[1]]
  
  raw_summary_list[[key]] <- tibble(
    dataset = parts[1],
    data_type = parts[2],
    n_rows = nrow(raw_objects[[key]]),
    n_variables = ncol(raw_objects[[key]])
  )
}

raw_summary <- bind_rows(
  raw_summary_list
)

write_csv(
  raw_summary,
  file.path(
    METADATA_DIR,
    "raw_summary.csv"
  )
)


# ============================================================
# 8. EXPLICIT VARIABLE CLASSIFICATION
# ============================================================
#
# CORE
#   Minimum information defining the observational unit,
#   space/time links and primary bycatch response.
#
# ATTRIBUTES
#   Fishing, biological and observational characteristics
#   that may later be evaluated for modelling.
#
# QC
#   Original values, corrections, diagnostic variables and
#   notes associated with data quality.
#
# AUXILIARY
#   Redundant/supporting variables retained for traceability.
#
# ============================================================

classification <- list()


# ============================================================
# 8.1 BYC_0002 — fishingOperations
# ============================================================

classification[["BYC_0002__fishingOperations"]] <- list(
  
  CORE = c(
    "datasetID",
    "vesselID",
    "tripID",
    "setID",
    "operationID",
    "setDateTime1",
    "setLongitude1",
    "setLatitude1",
    "haulDateTime2"
  ),
  
  ATTRIBUTES = c(
    "year",
    "vesselClass",
    "gearCode",
    "metier",
    "gearDescription",
    "vesselFlag",
    "setDepth",
    "targetCatchSpeciesRetained",
    "targetCatchSpeciesDiscarded",
    "notesTargetCatchSpecies",
    "targetCatchWeightRetained",
    "targetCatchWeightDiscarded",
    "targetCatchWeight",
    "fishModeCode",
    "mitigationMeasures",
    "notes"
  ),
  
  QC = character(0),
  
  AUXILIARY = character(0)
)


# ============================================================
# 8.2 BYC_0002 — bycatchAggregated
# ============================================================
#
# catchIndividuals is the corrected number of bycaught
# individuals and therefore belongs in CORE.
#
# Original/corrected pairs and correction notes go to QC.
#
# ============================================================

classification[["BYC_0002__bycatchAggregated"]] <- list(
  
  CORE = c(
    "datasetID",
    "operationID",
    "taxon",
    "scientificName",
    "speciesCode",
    "catchIndividuals"
  ),
  
  ATTRIBUTES = c(
    "catchWeight",
    "animalCondition",
    "organismFate",
    "organismSex",
    "organismSize1",
    "organismSizeMeasurementType1",
    "organismWeight",
    "organismWeightMeasurementType",
    "catchType"
  ),
  
  QC = c(
    "originalCatchIndividuals",
    "notesCatchIndividuals",
    "originalOrganismFate",
    "notesOrganismFate",
    "originalOrganismWeight",
    "notesOrganismWeight"
  ),
  
  AUXILIARY = character(0)
)


# ============================================================
# 8.3 BYC_0002 — bycatchSampledIndividuals
# ============================================================
#
# This is an individual-level sampled table.
#
# catchIndividuals is retained as an attribute rather than
# defining the operation-level response.
#
# ============================================================

classification[["BYC_0002__bycatchSampledIndividuals"]] <- list(
  
  CORE = c(
    "datasetID",
    "operationID",
    "taxon",
    "scientificName",
    "speciesCode"
  ),
  
  ATTRIBUTES = c(
    "catchIndividuals",
    "catchWeight",
    "animalCondition",
    "organismFate",
    "organismSex",
    "organismSize1",
    "organismSizeMeasurementType1",
    "organismWeight",
    "organismWeightMeasurementType",
    "catchType"
  ),
  
  QC = c(
    "originalOrganismFate",
    "notesOrganismSize1",
    "notesOrganismWeight"
  ),
  
  AUXILIARY = character(0)
)


# ============================================================
# 8.4 BYC_0003 — fishingOperations
# ============================================================

classification[["BYC_0003__fishingOperations"]] <- list(
  
  CORE = c(
    "datasetID",
    "vesselID",
    "tripID",
    "setID",
    "operationID",
    
    "setDateTime1",
    "setLongitude1",
    "setLatitude1",
    
    "setDateTime2",
    "setLongitude2",
    "setLatitude2",
    
    "haulDateTime1",
    "haulLongitude1",
    "haulLatitude1",
    
    "haulDateTime2",
    "haulLongitude2",
    "haulLatitude2"
  ),
  
  ATTRIBUTES = c(
    "year",
    "vesselClass",
    "gearCode",
    "metier",
    "gearDescription",
    "vesselFlag",
    "hooksDeployed",
    "hooksSampled",
    "hooksLost",
    "setDepth",
    "haulDepth",
    "targetCatchSpecies",
    "targetCatchWeight",
    "baitType",
    "hookType",
    "mitigationMeasures",
    "electricLightsDeployed",
    "foatlineLength_m"
  ),
  
  QC = character(0),
  
  AUXILIARY = character(0)
)


# ============================================================
# 8.5 BYC_0003 — bycatch
# ============================================================

classification[["BYC_0003__bycatch"]] <- list(
  
  CORE = c(
    "datasetID",
    "operationID",
    "captureIDperSet",
    "taxon",
    "scientificName",
    "speciesCode",
    "catchIndividuals"
  ),
  
  ATTRIBUTES = c(
    "catchWeight",
    "animalCondition",
    "organismFate",
    "organismSex",
    "organismSize1",
    "organismSizeMeasurementType1",
    "organismWeight",
    "organismWeightMeasurementType",
    "catchType"
  ),
  
  QC = c(
    "notes"
  ),
  
  AUXILIARY = character(0)
)


# ============================================================
# 8.6 BYC_0004 — fishingOperations
# ============================================================

classification[["BYC_0004__fishingOperations"]] <- list(
  
  CORE = c(
    "datasetID",
    "vesselID",
    "tripID",
    "setID",
    "operationID",
    
    "setDateTime1",
    "setLongitude1",
    "setLatitude1",
    
    "setDateTime2",
    "setLongitude2",
    "setLatitude2",
    
    "haulDateTime1",
    "haulLongitude1",
    "haulLatitude1",
    
    "haulDateTime2",
    "haulLongitude2",
    "haulLatitude2"
  ),
  
  ATTRIBUTES = c(
    "year",
    "vesselClass",
    "vesselLength",
    "vesselTonnage",
    "vesselEnginePower",
    "gearCode",
    "metier",
    "gearDescription",
    "vesselFlag",
    
    "startTrip",
    "endTrip",
    
    "observationProtocol",
    "observerCode",
    
    "hooksDeployed",
    "hooksSampled",
    "lostHooks",
    "propHooksDeclaredObserved",
    "numberHooksPerBasket",
    "totalNumberBaskets",
    
    "minDepthHook",
    "maxDepthHook",
    
    "lengthFloatLine1",
    "lengthFloatLine2",
    "lengthBranchLine",
    
    "targetCatchSpecies",
    "fishModeCode",
    "baitType",
    
    "hookShape",
    "hookSizeType",
    "hookHeight",
    "hookWidth",
    "hookDiameter",
    
    "electricLightsDeployed",
    "setShape",
    "notes"
  ),
  
  QC = c(
    "originalSetDateTime1",
    "originalSetDate1",
    "originalSetTime1",
    "set1_datetime_diff",
    "originalSetLongitude1",
    "set1_longitude_diff",
    "originalSetLatitude1",
    "set1_latitude_diff",
    
    "originalSetDateTime2",
    "originalSetDate2",
    "originalSetTime2",
    "set2_datetime_diff",
    "originalSetLongitude2",
    "set2_longitude_diff",
    "originalSetLatitude2",
    "set2_latitude_diff",
    
    "originalHaulDateTime1",
    "originalHaulDate1",
    "originalHaulTime1",
    "haul1_datetime_diff",
    "originalHaulLongitude1",
    "haul1_longitude_diff",
    "originalHaulLatitude1",
    "haul1_latitude_diff",
    
    "originalHaulDateTime2",
    "originalHaulDate2",
    "originalHaulTime2",
    "haul2_datetime_diff",
    "originalHaulLongitude2",
    "haul2_longitude_diff",
    "originalHaulLatitude2",
    "haul2_latitude_diff",
    
    "hooksNotes",
    "extraTripsComments"
  ),
  
  AUXILIARY = c(
    "setDate1",
    "setDate2",
    "haulDate1",
    "haulDate2"
  )
)


# ============================================================
# 8.7 BYC_0004 — bycatch
# ============================================================

classification[["BYC_0004__bycatch"]] <- list(
  
  CORE = c(
    "datasetID",
    "operationID",
    "taxon",
    "scientificName",
    "speciesCode",
    "catchIndividuals"
  ),
  
  ATTRIBUTES = c(
    "catchWeight",
    "animalCondition",
    "organismFate",
    "organismSex",
    
    "organismSize1",
    "organismSizeMeasurementType1",
    "organismSize2",
    "organismSizeMeasurementType2",
    
    "organismWeight",
    "organismWeightMeasurementType",
    
    "catchType",
    
    "gearInteraction",
    "gearInteractionPosition",
    
    "beaufortScale",
    "cloudCover",
    "waterTemperature",
    "seaColour"
  ),
  
  QC = c(
    "incongruenceConditionFate",
    "notes"
  ),
  
  AUXILIARY = character(0)
)


# ============================================================
# 9. CHECK THAT EVERY RAW TABLE HAS A CLASSIFICATION
# ============================================================

missing_classifications <- setdiff(
  names(raw_objects),
  names(classification)
)

if (length(missing_classifications) > 0) {
  
  cat(
    "\nTABLES WITHOUT CLASSIFICATION:\n\n"
  )
  
  print(
    missing_classifications
  )
  
  stop(
    "At least one raw table has no classification."
  )
}


# ============================================================
# 10. BUILD VARIABLE DICTIONARY
# ============================================================

dictionary_list <- list()

for (key in names(raw_objects)) {
  
  x <- raw_objects[[key]]
  
  parts <- strsplit(
    key,
    "__",
    fixed = TRUE
  )[[1]]
  
  dataset <- parts[1]
  data_type <- parts[2]
  
  class_key <- classification[[key]]
  
  destination <- rep(
    "UNCLASSIFIED",
    ncol(x)
  )
  
  destination[
    names(x) %in% class_key$CORE
  ] <- "CORE"
  
  destination[
    names(x) %in% class_key$ATTRIBUTES
  ] <- "ATTRIBUTES"
  
  destination[
    names(x) %in% class_key$QC
  ] <- "QC"
  
  destination[
    names(x) %in% class_key$AUXILIARY
  ] <- "AUXILIARY"
  
  dictionary_list[[key]] <- tibble(
    dataset = dataset,
    data_type = data_type,
    raw_variable = names(x),
    destination = destination
  )
}

variable_dictionary <- bind_rows(
  dictionary_list
)

write_csv(
  variable_dictionary,
  file.path(
    METADATA_DIR,
    "variable_dictionary.csv"
  )
)


# ============================================================
# 11. CHECK FOR UNCLASSIFIED RAW VARIABLES
# ============================================================

unclassified <- variable_dictionary |>
  filter(
    destination == "UNCLASSIFIED"
  )

write_csv(
  unclassified,
  file.path(
    METADATA_DIR,
    "unclassified_variables.csv"
  )
)

if (nrow(unclassified) > 0) {
  
  cat(
    "\n============================================================\n",
    "UNCLASSIFIED RAW VARIABLES FOUND\n",
    "============================================================\n\n",
    sep = ""
  )
  
  print(
    unclassified,
    n = Inf
  )
  
  stop(
    "Some raw variables have not been classified."
  )
}


# ============================================================
# 12. CHECK FOR CLASSIFIED VARIABLES THAT DO NOT EXIST
# ============================================================

classification_check_list <- list()

for (key in names(classification)) {
  
  x <- raw_objects[[key]]
  
  class_key <- classification[[key]]
  
  requested_variables <- unique(
    c(
      class_key$CORE,
      class_key$ATTRIBUTES,
      class_key$QC,
      class_key$AUXILIARY
    )
  )
  
  absent_variables <- setdiff(
    requested_variables,
    names(x)
  )
  
  if (length(absent_variables) > 0) {
    
    parts <- strsplit(
      key,
      "__",
      fixed = TRUE
    )[[1]]
    
    classification_check_list[[key]] <- tibble(
      dataset = parts[1],
      data_type = parts[2],
      variable = absent_variables
    )
  }
}

if (length(classification_check_list) > 0) {
  
  absent_from_raw <- bind_rows(
    classification_check_list
  )
  
} else {
  
  absent_from_raw <- tibble(
    dataset = character(),
    data_type = character(),
    variable = character()
  )
}

write_csv(
  absent_from_raw,
  file.path(
    METADATA_DIR,
    "classified_but_absent_from_raw.csv"
  )
)

if (nrow(absent_from_raw) > 0) {
  
  cat(
    "\n============================================================\n",
    "CLASSIFIED VARIABLES NOT PRESENT IN RAW DATA\n",
    "============================================================\n\n",
    sep = ""
  )
  
  print(
    absent_from_raw,
    n = Inf
  )
  
  stop(
    "Classification contains variable names absent from raw data."
  )
}


# ============================================================
# 13. CHECK THAT VARIABLES ARE NOT CLASSIFIED TWICE
# ============================================================

duplicate_classification_list <- list()

for (key in names(classification)) {
  
  class_key <- classification[[key]]
  
  all_classified <- c(
    class_key$CORE,
    class_key$ATTRIBUTES,
    class_key$QC,
    class_key$AUXILIARY
  )
  
  duplicated_variables <- unique(
    all_classified[
      duplicated(all_classified)
    ]
  )
  
  if (length(duplicated_variables) > 0) {
    
    parts <- strsplit(
      key,
      "__",
      fixed = TRUE
    )[[1]]
    
    duplicate_classification_list[[key]] <- tibble(
      dataset = parts[1],
      data_type = parts[2],
      variable = duplicated_variables
    )
  }
}

if (length(duplicate_classification_list) > 0) {
  
  duplicate_classification <- bind_rows(
    duplicate_classification_list
  )
  
} else {
  
  duplicate_classification <- tibble(
    dataset = character(),
    data_type = character(),
    variable = character()
  )
}

write_csv(
  duplicate_classification,
  file.path(
    METADATA_DIR,
    "duplicate_variable_classification.csv"
  )
)

if (nrow(duplicate_classification) > 0) {
  
  print(
    duplicate_classification,
    n = Inf
  )
  
  stop(
    "At least one variable is assigned to more than one destination."
  )
}


# ============================================================
# 14. MANUAL OPERATION EXCLUSIONS
# ============================================================
#
# No operation is excluded automatically.
#
# P99 spatial diagnostics from script 00 are exploratory QC,
# NOT automatic exclusion criteria.
#
# Confirmed invalid operations can later be entered here.
#
# ============================================================

manual_exclusions <- tibble(
  dataset = character(),
  operationID = character(),
  exclusion_reason = character()
)


# ============================================================
# 15. FUNCTION: ADD EXCLUSION FLAGS
# ============================================================

add_exclusion_flags <- function(x, dataset) {
  
  if (!"operationID" %in% names(x)) {
    
    x$excluded <- FALSE
    x$exclusion_reason <- NA_character_
    
    return(x)
  }
  
  x <- x |>
    mutate(
      excluded = FALSE,
      exclusion_reason = NA_character_
    )
  
  dataset_exclusions <- manual_exclusions |>
    filter(
      .data$dataset == dataset
    )
  
  if (nrow(dataset_exclusions) == 0) {
    return(x)
  }
  
  exclusion_lookup <- dataset_exclusions |>
    select(
      operationID,
      exclusion_reason
    ) |>
    distinct() |>
    rename(
      manual_exclusion_reason = exclusion_reason
    )
  
  x <- x |>
    left_join(
      exclusion_lookup,
      by = "operationID"
    ) |>
    mutate(
      excluded = !is.na(
        manual_exclusion_reason
      ),
      exclusion_reason = manual_exclusion_reason
    ) |>
    select(
      -manual_exclusion_reason
    )
  
  x
}


# ============================================================
# 16. PREPARE ALL TABLES
# ============================================================

prepared_objects <- list()

for (key in names(raw_objects)) {
  
  parts <- strsplit(
    key,
    "__",
    fixed = TRUE
  )[[1]]
  
  dataset <- parts[1]
  
  x <- raw_objects[[key]]
  
  x <- add_exclusion_flags(
    x = x,
    dataset = dataset
  )
  
  prepared_objects[[key]] <- x
}


# ============================================================
# 17. SPLIT ALL TABLES
# ============================================================

prepared_blocks <- list()

for (key in names(prepared_objects)) {
  
  x <- prepared_objects[[key]]
  
  class_key <- classification[[key]]
  
  link_vars <- intersect(
    c(
      "datasetID",
      "operationID"
    ),
    names(x)
  )
  
  exclusion_vars <- intersect(
    c(
      "excluded",
      "exclusion_reason"
    ),
    names(x)
  )
  
  
  # ----------------------------------------------------------
  # CORE
  # ----------------------------------------------------------
  
  core_vars <- unique(
    c(
      class_key$CORE,
      exclusion_vars
    )
  )
  
  core <- x |>
    select(
      all_of(core_vars)
    )
  
  
  # ----------------------------------------------------------
  # ATTRIBUTES
  # ----------------------------------------------------------
  
  attributes_vars <- unique(
    c(
      link_vars,
      class_key$ATTRIBUTES,
      exclusion_vars
    )
  )
  
  attributes <- x |>
    select(
      all_of(attributes_vars)
    )
  
  
  # ----------------------------------------------------------
  # QC
  # ----------------------------------------------------------
  
  qc_vars <- unique(
    c(
      link_vars,
      class_key$QC,
      exclusion_vars
    )
  )
  
  qc <- x |>
    select(
      all_of(qc_vars)
    )
  
  
  # ----------------------------------------------------------
  # AUXILIARY
  # ----------------------------------------------------------
  
  auxiliary_vars <- unique(
    c(
      link_vars,
      class_key$AUXILIARY
    )
  )
  
  auxiliary <- x |>
    select(
      all_of(auxiliary_vars)
    )
  
  
  prepared_blocks[[paste(key, "CORE", sep = "__")]] <- core
  prepared_blocks[[paste(key, "ATTRIBUTES", sep = "__")]] <- attributes
  prepared_blocks[[paste(key, "QC", sep = "__")]] <- qc
  prepared_blocks[[paste(key, "AUXILIARY", sep = "__")]] <- auxiliary
}


# ============================================================
# 18. SAVE ALL OUTPUTS — CSV + RDS
# ============================================================

for (key in names(prepared_objects)) {
  
  parts <- strsplit(
    key,
    "__",
    fixed = TRUE
  )[[1]]
  
  dataset <- parts[1]
  data_type <- parts[2]
  
  output_path <- file.path(
    OUTPUT_DIR,
    dataset,
    data_type
  )
  
  dir.create(
    output_path,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  for (destination in c(
    "CORE",
    "ATTRIBUTES",
    "QC",
    "AUXILIARY"
  )) {
    
    block_key <- paste(
      key,
      destination,
      sep = "__"
    )
    
    x <- prepared_blocks[[block_key]]
    
    file_stub <- tolower(
      destination
    )
    
    write_csv(
      x,
      file.path(
        output_path,
        paste0(
          file_stub,
          ".csv"
        )
      )
    )
    
    saveRDS(
      x,
      file.path(
        output_path,
        paste0(
          file_stub,
          ".rds"
        )
      )
    )
  }
}


# ============================================================
# 19. ROW PRESERVATION CHECK
# ============================================================

row_check_list <- list()

for (key in names(raw_objects)) {
  
  parts <- strsplit(
    key,
    "__",
    fixed = TRUE
  )[[1]]
  
  core_key <- paste(
    key,
    "CORE",
    sep = "__"
  )
  
  attributes_key <- paste(
    key,
    "ATTRIBUTES",
    sep = "__"
  )
  
  qc_key <- paste(
    key,
    "QC",
    sep = "__"
  )
  
  auxiliary_key <- paste(
    key,
    "AUXILIARY",
    sep = "__"
  )
  
  row_check_list[[key]] <- tibble(
    dataset = parts[1],
    data_type = parts[2],
    
    raw_rows = nrow(
      raw_objects[[key]]
    ),
    
    core_rows = nrow(
      prepared_blocks[[core_key]]
    ),
    
    attributes_rows = nrow(
      prepared_blocks[[attributes_key]]
    ),
    
    qc_rows = nrow(
      prepared_blocks[[qc_key]]
    ),
    
    auxiliary_rows = nrow(
      prepared_blocks[[auxiliary_key]]
    )
  )
}

row_preservation_check <- bind_rows(
  row_check_list
) |>
  mutate(
    rows_preserved =
      raw_rows == core_rows &
      raw_rows == attributes_rows &
      raw_rows == qc_rows &
      raw_rows == auxiliary_rows
  )

write_csv(
  row_preservation_check,
  file.path(
    METADATA_DIR,
    "row_preservation_check.csv"
  )
)

if (any(!row_preservation_check$rows_preserved)) {
  
  print(
    row_preservation_check,
    n = Inf
  )
  
  stop(
    "Row preservation check failed."
  )
}


# ============================================================
# 20. VARIABLE PRESERVATION CHECK
# ============================================================

variable_preservation_list <- list()

for (key in names(raw_objects)) {
  
  x <- raw_objects[[key]]
  
  class_key <- classification[[key]]
  
  classified_vars <- unique(
    c(
      class_key$CORE,
      class_key$ATTRIBUTES,
      class_key$QC,
      class_key$AUXILIARY
    )
  )
  
  missing_from_classification <- setdiff(
    names(x),
    classified_vars
  )
  
  extra_in_classification <- setdiff(
    classified_vars,
    names(x)
  )
  
  parts <- strsplit(
    key,
    "__",
    fixed = TRUE
  )[[1]]
  
  variable_preservation_list[[key]] <- tibble(
    dataset = parts[1],
    data_type = parts[2],
    
    n_raw_variables = ncol(x),
    
    n_classified_raw_variables = length(
      intersect(
        names(x),
        classified_vars
      )
    ),
    
    n_missing_from_classification = length(
      missing_from_classification
    ),
    
    n_extra_in_classification = length(
      extra_in_classification
    ),
    
    all_raw_variables_classified =
      length(missing_from_classification) == 0,
    
    all_classified_variables_exist =
      length(extra_in_classification) == 0
  )
}

variable_preservation_check <- bind_rows(
  variable_preservation_list
)

write_csv(
  variable_preservation_check,
  file.path(
    METADATA_DIR,
    "variable_preservation_check.csv"
  )
)


# ============================================================
# 21. BYCATCH → fishingOperations RELATIONSHIP CHECK
# ============================================================

relationship_list <- list()

for (dataset in c(
  "BYC_0002",
  "BYC_0003",
  "BYC_0004"
)) {
  
  ops_key <- paste(
    dataset,
    "fishingOperations",
    sep = "__"
  )
  
  ops <- raw_objects[[ops_key]]
  
  bycatch_keys_dataset <- names(raw_objects)[
    grepl(
      paste0(
        "^",
        dataset,
        "__"
      ),
      names(raw_objects)
    ) &
      !grepl(
        "__fishingOperations$",
        names(raw_objects)
      )
  ]
  
  for (bycatch_key in bycatch_keys_dataset) {
    
    bycatch <- raw_objects[[bycatch_key]]
    
    parts <- strsplit(
      bycatch_key,
      "__",
      fixed = TRUE
    )[[1]]
    
    data_type <- parts[2]
    
    if (
      !"operationID" %in% names(bycatch) ||
      !"operationID" %in% names(ops)
    ) {
      
      relationship_list[[bycatch_key]] <- tibble(
        dataset = dataset,
        data_type = data_type,
        n_bycatch_rows = nrow(bycatch),
        has_operationID = FALSE,
        n_missing_operationID = NA_integer_,
        n_unmatched_operationID = NA_integer_,
        proportion_unmatched = NA_real_
      )
      
      next
    }
    
    n_missing <- sum(
      is.na(
        bycatch$operationID
      )
    )
    
    unmatched <- (
      !is.na(bycatch$operationID) &
        !bycatch$operationID %in% ops$operationID
    )
    
    n_unmatched <- sum(
      unmatched
    )
    
    n_nonmissing <- sum(
      !is.na(
        bycatch$operationID
      )
    )
    
    relationship_list[[bycatch_key]] <- tibble(
      dataset = dataset,
      data_type = data_type,
      n_bycatch_rows = nrow(bycatch),
      has_operationID = TRUE,
      n_missing_operationID = n_missing,
      n_unmatched_operationID = n_unmatched,
      
      proportion_unmatched = ifelse(
        n_nonmissing > 0,
        n_unmatched / n_nonmissing,
        NA_real_
      )
    )
  }
}

relationship_check <- bind_rows(
  relationship_list
)

write_csv(
  relationship_check,
  file.path(
    METADATA_DIR,
    "bycatch_operationID_relationship_check.csv"
  )
)


# ============================================================
# 22. operationID DUPLICATE CHECK
# ============================================================
#
# Duplicates are NOT automatically considered errors.
#
# In bycatch tables, multiple records per operation are expected.
# This is purely descriptive.
#
# ============================================================

duplicate_check_list <- list()

for (key in names(raw_objects)) {
  
  x <- raw_objects[[key]]
  
  parts <- strsplit(
    key,
    "__",
    fixed = TRUE
  )[[1]]
  
  if (!"operationID" %in% names(x)) {
    
    duplicate_check_list[[key]] <- tibble(
      dataset = parts[1],
      data_type = parts[2],
      n_rows = nrow(x),
      n_unique_operationID = NA_integer_,
      n_rows_with_duplicated_operationID = NA_integer_
    )
    
    next
  }
  
  duplicated_operation <- (
    duplicated(x$operationID) |
      duplicated(
        x$operationID,
        fromLast = TRUE
      )
  )
  
  duplicated_operation[
    is.na(x$operationID)
  ] <- FALSE
  
  duplicate_check_list[[key]] <- tibble(
    dataset = parts[1],
    data_type = parts[2],
    
    n_rows = nrow(x),
    
    n_unique_operationID = n_distinct(
      x$operationID[
        !is.na(x$operationID)
      ]
    ),
    
    n_rows_with_duplicated_operationID = sum(
      duplicated_operation
    )
  )
}

duplicate_operationID_check <- bind_rows(
  duplicate_check_list
)

write_csv(
  duplicate_operationID_check,
  file.path(
    METADATA_DIR,
    "duplicate_operationID_check.csv"
  )
)


# ============================================================
# 23. CLASSIFICATION SUMMARY
# ============================================================

classification_summary <- variable_dictionary |>
  count(
    dataset,
    data_type,
    destination,
    name = "n_variables"
  ) |>
  arrange(
    dataset,
    data_type,
    destination
  )

write_csv(
  classification_summary,
  file.path(
    METADATA_DIR,
    "classification_summary.csv"
  )
)


# ============================================================
# 24. BYCATCH CLASSIFICATION TABLE
# ============================================================

bycatch_classification <- variable_dictionary |>
  filter(
    data_type != "fishingOperations"
  ) |>
  arrange(
    dataset,
    data_type,
    destination,
    raw_variable
  )

write_csv(
  bycatch_classification,
  file.path(
    METADATA_DIR,
    "bycatch_classification.csv"
  )
)


# ============================================================
# 25. fishingOperations CLASSIFICATION TABLE
# ============================================================

fishing_operations_classification <- variable_dictionary |>
  filter(
    data_type == "fishingOperations"
  ) |>
  arrange(
    dataset,
    destination,
    raw_variable
  )

write_csv(
  fishing_operations_classification,
  file.path(
    METADATA_DIR,
    "fishingOperations_classification.csv"
  )
)


# ============================================================
# 26. SAVE SESSION INFO
# ============================================================

capture.output(
  sessionInfo(),
  file = file.path(
    METADATA_DIR,
    "sessionInfo.txt"
  )
)


# ============================================================
# 27. FINAL REPORT
# ============================================================

cat(
  "\n\n============================================================\n"
)

cat(
  "01 — IEO DATA PREPARATION COMPLETE\n"
)

cat(
  "============================================================\n\n"
)


cat(
  "RAW TABLES\n\n"
)

print(
  raw_summary,
  n = Inf
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "CLASSIFICATION SUMMARY\n"
)

cat(
  "------------------------------------------------------------\n\n"
)

print(
  classification_summary,
  n = Inf
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "ROW PRESERVATION\n"
)

cat(
  "------------------------------------------------------------\n\n"
)

print(
  row_preservation_check,
  n = Inf
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "VARIABLE PRESERVATION\n"
)

cat(
  "------------------------------------------------------------\n\n"
)

print(
  variable_preservation_check,
  n = Inf
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "BYCATCH → fishingOperations RELATIONSHIPS\n"
)

cat(
  "------------------------------------------------------------\n\n"
)

print(
  relationship_check,
  n = Inf
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "operationID DUPLICATES\n"
)

cat(
  "------------------------------------------------------------\n\n"
)

print(
  duplicate_operationID_check,
  n = Inf
)


cat(
  "\n============================================================\n"
)

cat(
  "OUTPUT DIRECTORY\n\n"
)

cat(
  OUTPUT_DIR,
  "\n"
)


cat(
  "\n============================================================\n"
)

cat(
  "01 FINISHED SUCCESSFULLY\n"
)

cat(
  "============================================================\n"
)