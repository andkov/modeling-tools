rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console
# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: 
# http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr)
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(labelled)  # labels
library(dplyr)     # data wrangling
library(tidyr)     # data wrangling
library(scales)    # format
library(broom)     # for model
library(emmeans)   # for interpreting model results
library(ggalluvial)
# -- 2.Import only certain functions of a package into the search path.
# import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
base::source("./scripts/operational-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
target_window_opens  <- as.Date("2014-01-01")
target_window_closes <- as.Date("2025-04-30")
target_window <- c(target_window_opens, target_window_closes)
local_folder_name <- "forecasting-with-ai"
local_root <- paste0("./analysis/",local_folder_name,"/")
local_data <- paste0(local_root, "data-local/") # for local outputs

if (!fs::dir_exists(local_data)) {fs::dir_create(local_data)}

data_private_derived <- paste0("./data-private/derived/",local_folder_name,"/")
if (!fs::dir_exists(data_private_derived)) {fs::dir_create(data_private_derived)}

prints_folder <- paste0(local_root, "prints/")
if (!fs::dir_exists(prints_folder)) {fs::dir_create(prints_folder)}

sample_of_interest1 <- 4734747 # real id in focus
# sample_of_interest1 <- 179820 # scramble id in focus
sample_of_interest <- c(
  1017460
  ,1411830
  ,3777415
  ,4812318
  ,4734747
  ,2099597
)
sample_of_interest1 <- sample_of_interest[6]
# ---- declare-functions -------------------------------------------------------
# base::source(paste0(local_root,"local-functions.R")) # project-level
check_id_duplicates <- function(data, id_var) {
  id_var <- rlang::enquo(id_var)
  
  data %>%
    count(!!id_var, name = "n") %>%
    filter(n > 1) %>%
    arrange(desc(n))
}
# ----- define-query -----------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

# ---- tweak-data-0 -------------------------------------

# ---- tweak-data-1 -------------------------


# ---- analysis-below -------------------------------------

# ds2_tenant %>% group_by(tenant_status) %>% summarize(hh_count = n_distinct(tenant))
