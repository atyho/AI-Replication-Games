# ===========================================
# Master R Script for AI Paper Replication
# ===========================================

# ---- Load Required Packages ----

# List of packages you want to use
pkgs <- c(
  "here", "haven", "rmarkdown", "readxl", "dplyr",
  "stringr", "tidyr", "forcats", "janitor", "lubridate",
  "fixest", "purrr", "broom", "tibble", "car", "margins",
  "sandwich", "lmtest", "multcomp", "kableExtra", "ggplot2",
  "patchwork"
)

# Install any that are missing
installed <- rownames(installed.packages())
to_install <- setdiff(pkgs, installed)
if(length(to_install)) install.packages(to_install)

# Load all packages
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Set Paths ----
main_path <- "~/Dropbox/I4R/AI paper"
setwd(main_path)
log_file <- "output/master_log_R.log"

# ---- Start Logging ----
sink(log_file, split = TRUE)
cat("=== MASTER LOG START ===\n")

# ---- 1. Cleaning raw data ----
cat("\n--- Cleaning raw data ---\n")
source(here("code/R code", "cleaning.R"))  

# ---- 2. Main Tables (regression outputs) ----
cat("\n--- Main Table (OLS) ---\n")
source(here("code/R code", "main.R"))

cat("\n--- Main Table (Logit and Poisson) ---\n")
source(here("code/R code", "logit poisson.R"))

cat("\n--- Main Table (Full Controls) ---\n")
source(here("code/R code", "full controls.R"))

cat("\n--- Main Table (Softwares) ---\n")
source(here("code/R code", "softwares.R"))

# ---- 3. Branch Differences Table ----
cat("\n--- Branch Differences Table ---\n")
source(here("code/R code", "branches.R"))

# ---- 4. Balance Tables ----
cat("\n--- Balance Tables ---\n")
source(here("code/R code", "balance.R"))

# ---- 5. ChatGPT Skill Tables ----
cat("\n--- ChatGPT Skill Tables ---\n")
source(here("code/R code", "gpt skill.R"))

# ---- 6. Prompts Usage Tables ----
cat("\n--- Prompts Usage Tables ---\n")
source(here("code/R code", "prompts.R"))

# ---- 7. Figures ----
cat("\n--- Figure: Time to First Cumulative Density ---\n")
source(here("code/R code", "time to first.R"))

cat("\n--- Figure: Reproduction Rates Over Time ---\n")
source(here("code/R code", "reproduction rates.R"))

cat("\n=== MASTER LOG END ===\n")
sink()