# ============================================================
# REDUCE — MESOSCALE BYCATCH
# 00_main.R
#
# Master pipeline
#
# Run from the project root.
# All paths are handled with {here}.
# Raw input data are NEVER modified.
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
# 1. REQUIRED PACKAGE
# ============================================================

if (!requireNamespace("here", quietly = TRUE)) {
  stop(
    "Package 'here' is required. Install it with install.packages('here').",
    call. = FALSE
  )
}

library(here)


# ============================================================
# 2. PROJECT ROOT
# ============================================================

PROJECT_ROOT <- here::here()

cat(
  "\n====================================================\n",
  "REDUCE — MESOSCALE BYCATCH\n",
  "====================================================\n\n",
  "Project root:\n",
  PROJECT_ROOT,
  "\n\n",
  sep = ""
)


# ============================================================
# 3. CREATE PROJECT DIRECTORIES
#
# IMPORTANT:
# Nothing is created inside the BYC raw-data folders.
# Their original IEO structure is preserved exactly.
# ============================================================

project_dirs <- c(
  
  here::here("R"),
  
  here::here(
    "00inputOutput"
  ),
  
  here::here(
    "00inputOutput",
    "00input"
  ),
  
  here::here(
    "00inputOutput",
    "00input",
    "00IEO"
  ),
  
  here::here(
    "00inputOutput",
    "00input",
    "00enviro"
  ),
  
  here::here(
    "00inputOutput",
    "00input",
    "00enviro",
    "eddies"
  ),
  
  here::here(
    "00inputOutput",
    "00input",
    "00enviro",
    "eddies",
    "META_DT"
  ),
  
  here::here(
    "00inputOutput",
    "00input",
    "00enviro",
    "eddies",
    "META_NRT"
  ),
  
  here::here(
    "00inputOutput",
    "00output"
  ),
  
  here::here(
    "00inputOutput",
    "00output",
    "00_raw_exploration"
  ),
  
  here::here(
    "00inputOutput",
    "00output",
    "01_standardised_data"
  ),
  
  here::here(
    "00inputOutput",
    "00output",
    "02_eddy_data"
  ),
  
  here::here(
    "00inputOutput",
    "00output",
    "03_matching"
  ),
  
  here::here(
    "00inputOutput",
    "00output",
    "04_exploration"
  ),
  
  here::here(
    "00inputOutput",
    "00output",
    "05_models"
  ),
  
  here::here(
    "00inputOutput",
    "00output",
    "figures"
  )
)


for (d in project_dirs) {
  
  if (!dir.exists(d)) {
    
    dir.create(
      d,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    cat(
      "[CREATED] ",
      d,
      "\n",
      sep = ""
    )
  }
}


# ============================================================
# 4. DEFINE RAW IEO FILES
# ============================================================

FILE_0002_FISHING <- here::here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0002",
  "fishingOperations",
  "Ouled-Cheikh_BYC_0002_PS_MUL_IEO_fishingOperations(in).csv"
)

FILE_0002_BYCATCH_AGG <- here::here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0002",
  "bycatchAggregated",
  "Ouled-Cheikh_BYC_0002_PS_MUL_IEO_bycatchAggregated(in).csv"
)

FILE_0002_BYCATCH_IND <- here::here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0002",
  "bycatchSampledIndividuals",
  "Ouled-Cheikh_BYC_0002_PS_MUL_IEO_bycatchSampledIndividuals(in).csv"
)

FILE_0002_META_BYCATCH <- here::here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0002",
  "metadata",
  "BYC_0002_PS_MUL_IEO_bycatch_variable_explanation(in).csv"
)

FILE_0002_META_FISHING <- here::here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0002",
  "metadata",
  "BYC_0002_PS_MUL_IEO_fishingOperations_variable_explanation(in).csv"
)


FILE_0003_FISHING <- here::here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0003",
  "fishingOperations",
  "Ouled-Cheikh_BYC_0003_LL_MUL_IEO_fishingOperations(in).csv"
)

FILE_0003_BYCATCH <- here::here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0003",
  "bycatch",
  "Ouled-Cheikh_BYC_0003_LL_MUL_IEO_bycatch(in).csv"
)


FILE_0004_FISHING <- here::here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0004",
  "fishingOperations",
  "Ouled-Cheikh_BYC_0004_LL_MUL_IEO_fishingOperations(in).csv"
)

FILE_0004_BYCATCH <- here::here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0004",
  "bycatch",
  "Ouled-Cheikh_BYC_0004_LL_MUL_IEO_bycatch(in).csv"
)


# ============================================================
# 5. CHECK RAW INPUT FILES
# ============================================================

required_files <- c(
  
  BYC_0002_fishingOperations =
    FILE_0002_FISHING,
  
  BYC_0002_bycatchAggregated =
    FILE_0002_BYCATCH_AGG,
  
  BYC_0002_bycatchSampledIndividuals =
    FILE_0002_BYCATCH_IND,
  
  BYC_0002_metadata_bycatch =
    FILE_0002_META_BYCATCH,
  
  BYC_0002_metadata_fishingOperations =
    FILE_0002_META_FISHING,
  
  BYC_0003_fishingOperations =
    FILE_0003_FISHING,
  
  BYC_0003_bycatch =
    FILE_0003_BYCATCH,
  
  BYC_0004_fishingOperations =
    FILE_0004_FISHING,
  
  BYC_0004_bycatch =
    FILE_0004_BYCATCH
)


files_exist <- file.exists(
  required_files
)


cat(
  "\n====================================================\n",
  "RAW INPUT CHECK\n",
  "====================================================\n\n",
  sep = ""
)


for (i in seq_along(required_files)) {
  
  status <- ifelse(
    files_exist[i],
    "[OK]",
    "[MISSING]"
  )
  
  cat(
    status,
    " ",
    names(required_files)[i],
    "\n",
    sep = ""
  )
}


if (!all(files_exist)) {
  
  cat(
    "\nMissing files:\n\n"
  )
  
  for (x in required_files[!files_exist]) {
    
    cat(
      "  ",
      x,
      "\n",
      sep = ""
    )
  }
  
  stop(
    "Pipeline stopped because required raw IEO files are missing.",
    call. = FALSE
  )
}


cat(
  "\nAll required raw IEO files are available.\n"
)


# ============================================================
# 6. DEFINE PIPELINE
# ============================================================

pipeline <- c(
  
  "00_explore_raw_data.R",
  
  "01_build_standardised_data.R",
  
  "02_prepare_eddy_data.R",
  
  "03_match_operations_eddies.R",
  
  "04_exploratory_analysis.R",
  
  "05_models.R"
)


# ============================================================
# 7. RUN PIPELINE
#
# During development:
# scripts that do not exist yet are skipped.
#
# Once the workflow is complete, this can be changed so
# missing scripts stop the pipeline.
# ============================================================

cat(
  "\n====================================================\n",
  "PIPELINE\n",
  "====================================================\n\n",
  sep = ""
)


for (script in pipeline) {
  
  script_path <- here::here(
    "R",
    script
  )
  
  if (!file.exists(script_path)) {
    
    cat(
      "[SKIP] ",
      script,
      " — not created yet.\n",
      sep = ""
    )
    
    next
  }
  
  
  cat(
    "\n----------------------------------------------------\n",
    "RUNNING: ",
    script,
    "\n",
    "----------------------------------------------------\n\n",
    sep = ""
  )
  
  
  source(
    script_path,
    echo = FALSE,
    local = FALSE
  )
  
  
  cat(
    "\n[OK] ",
    script,
    "\n",
    sep = ""
  )
}


# ============================================================
# 8. FINISHED
# ============================================================

cat(
  "\n====================================================\n",
  "PIPELINE FINISHED\n",
  "====================================================\n\n",
  sep = ""
)