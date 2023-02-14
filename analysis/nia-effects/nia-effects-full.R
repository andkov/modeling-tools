rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console
# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings, but consider `stringi` as more general
library(lubridate) # dates
library(labelled)  # labels
library(dplyr)
# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here when `quick_save("name",w=8,h=6)` is used:
prints_folder <- paste0("./analysis/nia-effects/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

path_data_input <- "./analysis/nia-effects/nia-3-cra-effects-full.csv"

intervention_labels <- c(
  "ab_job_corps"      = "Alberta Job Corps"
  ,"english_as_second"= "English as Second"           
  ,"exposure_course"  = "Exposure Course"             
  ,"job_placement"    = "Job Placement"               
  ,"training_for_work"= "Training for Work"           
  ,"work_foundation"  = "Work Foundations"            
  ,"career_planning"  = "Career Planning WS"          
  ,"workshop_noncp"   = "Workshop nonCP"               
)

intervention_names <- intervention_labels %>%  names()

outcome_labels <- c(
   "earnings_total_delta" = "Total earnings (delta)"
  ,"income_net_delta"     = "Net income (delta)"
  ,"income_taxable_delta" = "Taxable income (delta)"
  ,"income_total_delta"   = "Total income (delta)"
)
outcome_names <- outcome_labels %>%  names()

# ---- declare-functions -------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_csv(path_data_input) %>% janitor::clean_names()
ds0 %>% glimpse()
# ---- inspect-data ------------------------------------------------------------
ds0 %>% count(intervention)
ds0 %>% count(outcome)
ds0 %>% count(var_name)

# ---- tweak-data --------------------------------------------------------------
ds1 <- 
  ds0 %>% 
  mutate(
    intervention = factor(intervention, levels = intervention_names, labels = intervention_labels)
    ,outcome    = factor(outcome, levels = outcome_names, labels = outcome_labels)
  )
ds1 %>% count(intervention)
ds1 %>% count(outcome)
# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------

# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/.../report-isolated.Rmd" # connect with Rmd for publishing
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
