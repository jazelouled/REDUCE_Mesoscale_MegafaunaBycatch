# ============================================================
# REDUCE — MESOSCALE BYCATCH
# 00_main.R
#
# MASTER PIPELINE
#
# Run this script from a fresh R session.
#
# It:
#   1. creates the project directory structure
#   2. checks required inputs
#   3. runs the complete analysis pipeline
#
# All paths are relative to the project root using {here}.
#
# No absolute user-specific paths are used.
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
# 1. REQUIRED PACKAGE FOR PROJECT PATHS
# ============================================================

if (!requireNamespace(
  "here",
  quietly = TRUE
)) {
  
  stop(
    paste0(
      "Package 'here' is required.\n\n",
      "Install it with:\n\n",
      "install.packages(\"here\")"
    )
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
# 3. DEFINE DIRECTORY STRUCTURE
# ============================================================

dirs <- c(
  
  # ----------------------------------------------------------
  # Scripts
  # ----------------------------------------------------------
  
  here(
    "R"
  ),
  
  
  # ----------------------------------------------------------
  # Input
  # ----------------------------------------------------------
  
  here(
    "00inputOutput"
  ),
  
  here(
    "00inputOutput",
    "00input"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00IEO"
  ),
  
  
  # BYC_0002
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0002"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0002",
    "fishingOperations"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0002",
    "bycatch"
  ),
  
  
  # BYC_0003
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0003"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0003",
    "fishingOperations"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0003",
    "bycatch"
  ),
  
  
  # BYC_0004
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0004"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0004",
    "fishingOperations"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0004",
    "bycatch"
  ),
  
  
  # ----------------------------------------------------------
  # Environmental data
  # ----------------------------------------------------------
  
  here(
    "00inputOutput",
    "00input",
    "00enviro"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00enviro",
    "eddies"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00enviro",
    "eddies",
    "META_DT"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00enviro",
    "eddies",
    "META_NRT"
  ),
  
  
  # ----------------------------------------------------------
  # Output
  # ----------------------------------------------------------
  
  here(
    "00inputOutput",
    "00output"
  ),
  
  here(
    "00inputOutput",
    "00output",
    "00_raw_exploration"
  ),
  
  here(
    "00inputOutput",
    "00output",
    "01_standardised_data"
  ),
  
  here(
    "00inputOutput",
    "00output",
    "02_eddy_data"
  ),
  
  here(
    "00inputOutput",
    "00output",
    "03_matching"
  ),
  
  here(
    "00inputOutput",
    "00output",
    "04_exploration"
  ),
  
  here(
    "00inputOutput",
    "00output",
    "05_models"
  ),
  
  here(
    "00inputOutput",
    "00output",
    "figures"
  )
  
)


# ============================================================
# 4. CREATE DIRECTORIES
# ============================================================

for (d in dirs) {
  
  if (!dir.exists(d)) {
    
    dir.create(
      d,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    cat(
      "Created:",
      d,
      "\n"
    )
  }
}


cat(
  "\nDirectory structure ready.\n\n"
)


# ============================================================
# 5. DEFINE STANDARD PROJECT PATHS
# ============================================================

INPUT_DIR <- here(
  "00inputOutput",
  "00input"
)

OUTPUT_DIR <- here(
  "00inputOutput",
  "00output"
)


IEO_DIR <- here(
  "00inputOutput",
  "00input",
  "00IEO"
)


ENV_DIR <- here(
  "00inputOutput",
  "00input",
  "00enviro"
)


EDDY_DIR <- here(
  "00inputOutput",
  "00input",
  "00enviro",
  "eddies"
)


# ------------------------------------------------------------
# IEO datasets
# ------------------------------------------------------------

BYC0002_DIR <- here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0002"
)

BYC0003_DIR <- here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0003"
)

BYC0004_DIR <- here(
  "00inputOutput",
  "00input",
  "00IEO",
  "BYC_0004"
)


# ============================================================
# 6. CHECK FISHING INPUTS
# ============================================================

required_fishing_dirs <- c(
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0002",
    "fishingOperations"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0003",
    "fishingOperations"
  ),
  
  here(
    "00inputOutput",
    "00input",
    "00IEO",
    "BYC_0004",
    "fishingOperations"
  )
  
)


has_files <- vapply(
  
  required_fishing_dirs,
  
  function(x) {
    
    length(
      list.files(
        x,
        recursive = TRUE
      )
    ) > 0
    
  },
  
  logical(1)
  
)


# ============================================================
# 7. STOP IF RAW FISHING DATA ARE NOT PRESENT
# ============================================================

if (!all(has_files)) {
  
  missing <- required_fishing_dirs[
    !has_files
  ]
  
  
  cat(
    "\n====================================================\n",
    "PROJECT INITIALISED\n",
    "====================================================\n\n",
    sep = ""
  )
  
  
  cat(
    "The directory structure has been created successfully.\n\n"
  )
  
  
  cat(
    "Raw fishing-operation data are still missing from:\n\n"
  )
  
  
  for (x in missing) {
    
    cat(
      "  ",
      x,
      "\n",
      sep = ""
    )
  }
  
  
  cat(
    "\nPlace the raw IEO data in these directories and run\n",
    "00_main.R again.\n\n",
    sep = ""
  )
  
  
  stop(
    "Pipeline stopped: raw fishing data not yet available.",
    call. = FALSE
  )
}


# ============================================================
# 8. PIPELINE
# ============================================================
#
# Each script must:
#
#   - use here::here()
#   - read only from 00input or previous 00output stages
#   - write only to its own output directory
#   - never modify raw data
#
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
# 9. RUN AVAILABLE SCRIPTS
# ============================================================
#
# During development, scripts that do not exist yet are
# reported and skipped.
#
# Once the pipeline is final, we can change this behaviour
# so that a missing script causes the pipeline to stop.
# ============================================================

cat(
  "\n====================================================\n",
  "RUNNING PIPELINE\n",
  "====================================================\n\n",
  sep = ""
)


for (script in pipeline) {
  
  script_path <- here(
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
# 10. FINISHED
# ============================================================

cat(
  "\n\n====================================================\n",
  "PIPELINE FINISHED\n",
  "====================================================\n\n",
  sep = ""
)