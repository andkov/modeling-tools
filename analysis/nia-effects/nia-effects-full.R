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


predictor_meta <- tibble::tribble(
  ~var_name,~value_label,~value_order,
  "tx", "FALSE",0,
  "tx", "TRUE",1,
  "gender2","(Missing)",0,
  "gender2","Men",1,
  "gender2","Women",2,
  "marital3","(Missing)",0,
  "marital3","never married",1,
  "marital3","apart",2,
  "marital3","together",3,
  "education4","(Missing)",0,
  "education4","Less HS",1,
  "education4","High School",2,
  "education4","Post HS",3,
  "education4","University Degree",4,
  "dependent4","0 dependents",0,
  "dependent4","1 dependent",1,
  "dependent4","2 dependent",2,
  "dependent4","3+ dependent",3,
  "disability2","Without Disability",0,
  "disability2","With Disability",1,
  "ethnicity","(Missing)",0,
  "ethnicity","Caucasian",1,
  "ethnicity","Visible Minority",2,
  "ethnicity","Indigenous",3,
  "immigration","(Missing)",0,
  "immigration","born in Canada",1,
  "immigration","immigrant",2,
  "region7","North West", 0,
  "region7","North Central",1,
  "region7","North East", 2,
  "region7","Edmonton",3 ,
  "region7","Central",4 ,
  "region7","Calgary",5,
  "region7","South",6,
  "year_before","2012",0,
  "year_before","2013",1,
  "year_before","2014",2,
  "year_before","2015",3,
  "year_before","2016",4,
  "year_before","2017",5,
  "spell_duration","0",0,
  "income_net_before","0",0,
  "earnings_total_before","0",0,
  "income_total_before","0",0,
  "income_taxable_before","0",0,
)

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
