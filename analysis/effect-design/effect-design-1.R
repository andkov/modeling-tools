rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console
# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# Project Directory should be the root by default unless overwritten

# ---- load-packages -----------------------------------------------------------
library(dplyr)     # data wrangling
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings, but consider `stringi` as more general
library(lubridate) # dates
library(labelled)  # labels
library(magrittr)  # pipes
library(tableone)  # tables
# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here when `quick_save("name",w=8,h=6)` is used:
prints_folder <- paste0("./analysis/effect-design/prints-1/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }
# ---- declare-functions -------------------------------------------------------
shuffle_rows <- function(x){x[order(runif(length(x)))]}
# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_rds("./data-public/raw/example-prosthetic-1.rds")

ds0 <- ds0 %>% 
  mutate(sex = drop_unused_value_labels(sex))

levels(ds0$sex)

# ---- inspect-data ------------------------------------------------------------
ds0 %>% head()
ds0$date %>% summary()
ds0 %>% select(4:7) %>% CreateTableOne(data=.)
ds0 %>% select(4:7) %>% CreateTableOne(data=., strata = "sex")

# ---- tweak-data --------------------------------------------------------------

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


# ---- ------

shuffle_rows <- function(x){x[order(runif(length(x)))]}
d3 <-
  d2 %>%
  mutate(
    employed = shuffle_rows(employed)
    ,gender  = shuffle_rows(gender)
    ,age     = shuffle_rows(age)
    ,race    = shuffle_rows(race)
  )
look_for(d3)
library(labelled)
var_label(d3$id)       <- ("Survey ID")
var_label(d3$weight)   <- ("Survey Weight")
var_label(d3$date)     <- ("Date of Survey")
var_label(d3$employed) <- ("Employed at Survey")
var_label(d3$gender)   <- ("Gender")
var_label(d3$age)      <- ("Age")
var_label(d3$race)     <- ("Race")
d3 %>% look_for()

# verify
d2 %>% group_by(gender, age, race, employed) %>% count()
d3 %>% group_by(gender, age, race, employed) %>% count()