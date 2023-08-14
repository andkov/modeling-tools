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
library(stringr)   # strings
library(lubridate) # dates
library(labelled)  # labels
library(scales)    # format
library(dplyr)     # data wrangling
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("dplyr"    )# Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# default location for prints by quick_save():
prints_folder <- paste0("./analysis/ua-war-religion/prints/")
if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))}

# ---- load-data ---------------------------------------------------------------
load("./analysis/ua-war-religion/materials/toy-data.RData")

# ---- inspect-data ------------------------------------------------------------
filtered_data %>% glimpse()
filtered_data %>% tableone::CreateTableOne(data=., strata = "wave")
filtered_data %>% explore::describe_all()

# describe items used to derive affected_index
ds_var <- 
  tibble(var_name = names(war_var_labels), var_value = war_var_labels) %>% 
  print()

# ---- tweak-data --------------------------------------------------------------
ds0 <- 
  filtered_data %>% 
  # integer indicator for the wave to ease some graphing
  mutate(waveL = case_when(wave =="wave1"~1L,TRUE ~ 2L) %>% as.integer()) %>%   # for geom_smooth to work
  select(
    key   # respondent identifier
    ,wave  # as a  factor
    ,waveL # as an integer # for geom_smooth
    
    # variables measured at both waves
    # outcome, religiosity 
    ,c15 # how religious are you ?         0-10 # filtered_data %>% count(c15)
    ,c16 # how often do you attend church? 1-7  # filtered_data %>% count(c16)
    ,c17 # how often do you pray?          1-7  # filtered_data %>% count(c17)
    ,religiosity # mean of standardized(M=0,SD=1) items c16, c16, c17
    
    # variables measured at wave 2 ONLY (because happened after full-scale invasion)
    ,loss_dummy3 # know someone who died from war
    ,displaced   # moved since full-scale invasion
    # overall index, sum of the following binary variables:
       # loss_dummy3, 
       # displaced,   
       # q2.2.1  # loss of income
       # q2.2.2  # loss of job
       # q2.2.3  # physical health deterioration
       # q2.2.4  # mental health deterioration
       # q2.2.5  # family separation
       # q2.2.6  # loss or damage to housing
       # q2.2.7  # loss or damage to other assets
       # q2.2.8  # injury to you or family members
    ,affected_index       # 0-10 # larger is more affected by war
    ,affected_index_std   # standardized: mean=0, sd=1 
    ,affected_index_dummy # above average adversity: affected_index_std > 0
 )
# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------


# ---- graph-2 -----------------------------------------------------------------

# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/.../report-isolated.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
