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
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# for asserting conditions meet expected patterns.
requireNamespace("scales"   )# formatting

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here when `quick_save("name",w=8,h=6)` is used:
prints_folder <- paste0("./analysis/nia-effects/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

path_data_input <- "./analysis/nia-effects/nia-3-cra-effects-null.csv"

intervention_labels <- c(
    "ab_job_corps"    = "Alberta Job Corps"
  , "english_as_second"= "English as Second"           
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
  ,"income_net_delta" = "Net income (delta)"
  ,"income_taxable_delta" = "Taxable income (delta)"
  ,"income_total_delta" = "Total income (delta)"
         
)
outcome_names <- outcome_labels %>%  names()

# ---- declare-functions -------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_csv(path_data_input) %>% janitor::clean_names()
ds0 %>% glimpse()
# ---- inspect-data ------------------------------------------------------------
ds0 %>% count(intervention)
ds0 %>% count(outcome)

# ---- tweak-data --------------------------------------------------------------
ds1 <- 
  ds0 %>% 
  mutate(
    intervention = factor(intervention, levels = intervention_names, labels = intervention_labels)
    ,outcome = factor(outcome, levels = outcome_names, labels = outcome_labels)
  )
ds1 %>% count(intervention)
ds1 %>% count(outcome)
# ---- table-1 -----------------------------------------------------------------


# ---- graph-1 -----------------------------------------------------------------
d <- 
  ds1 %>% 
  # filter(intervention == "career_planning")
  group_by(intervention, outcome) %>% 
  mutate(
    raw_diff = mean_raw - lag(mean_raw)
  ) %>% 
  ungroup() %>% 
  mutate(
    impact_direction = case_when(
      net_impact >0L ~ "positive",
      net_impact <=0L ~ "negative"
    )
    ,diff_direction = case_when(
      raw_diff >0L ~ "positive",
      raw_diff <=0L ~ "negative"
    )
    
  ) 
d
g1 <- 
  d %>% 
  # filter(outcome == "income_net_delta") %>% 
  # filter(tx) %>% 
  ggplot(aes(y=intervention))+
  geom_point(aes(shape = tx, x=mean_weighted))+
  scale_shape_manual(values = c("TRUE"=16, "FALSE"=21))+
  # geom_point(aes(x=mean_weighted))+
  # geom_point(aes(x=mean_weighted-net_impact))+
  geom_segment(aes(x=(mean_weighted-net_impact), xend = mean_weighted,
                   yend = intervention, color = impact_direction), alpha = .4)+
  geom_text(aes(label = scales::comma(net_impact,1) ,x=mean_weighted, color = impact_direction)
            ,size = 4, nudge_y = .2)+
  geom_text(aes(label = scales::comma(mean_weighted,1),x=mean_weighted), data = . %>% filter(tx==FALSE)
            ,size = 2.5, alpha = .5, color = "grey40",nudge_y=-.15)+
  # geom_text(aes(label = scales::comma(net_impact,accuracy = 1), color = impact_direction ), x=mean_weighted)+
  facet_wrap(facets = "outcome", nrow=1)+
  scale_color_manual(values = c("positive"="blue", "negative"="red"), drop = TRUE)+
  scale_x_continuous(
    breaks = seq(-2000,8000,2000)
    ,labels = scales::comma_format()
    ,limits = c(-2500,8000)
  )+
  labs(
    color = "Impact\ndirection"
    ,shape = "Received\nintervention"
    ,x = "Delta (pre- vs. post-Income Support) in 2022 dollars"
    ,title = "Net impact of intervention on reported income after the first Income Support spell"
  )
# g1
g1 %>% quick_save("1-net-impact-null",h=5,w=11)
# ---- graph-2 -----------------------------------------------------------------
g2 <- 
  d %>% 
  # filter(outcome == "income_net_delta") %>% 
  # filter(tx) %>% 
  ggplot(aes(y=intervention))+
  geom_point(aes(shape = tx, x=mean_raw))+
  scale_shape_manual(values = c("TRUE"=16, "FALSE"=21))+
  # geom_point(aes(x=mean_raw))+
  # geom_point(aes(x=mean_raw-raw_diff))+
  geom_segment(aes(x=(mean_raw-raw_diff), xend = mean_raw,
                   yend = intervention, color = diff_direction), alpha = .4)+
  geom_text(aes(label = scales::comma(raw_diff,1) ,x=mean_raw, color = diff_direction)
            ,size = 4, nudge_y = .2)+
  geom_text(aes(label = scales::comma(mean_raw,1),x=mean_raw), data = . %>% filter(tx==FALSE)
            ,size = 2.5, alpha = .5, color = "grey40",nudge_y=-.15)+
  # geom_text(aes(label = scales::comma(raw_diff,accuracy = 1), color = diff_direction ), x=mean_raw)+
  facet_wrap(facets = "outcome", nrow=1)+
  scale_color_manual(values = c("positive"="blue", "negative"="red"), drop = TRUE)+
  scale_x_continuous(
    breaks = seq(-2000,8000,2000)
    ,labels = scales::comma_format()
    ,limits = c(-2500,8000)
  )+
  labs(
    color = "Difference\ndirection"
    ,shape = "Received\nintervention"
    ,x = "Delta (pre- vs. post-Income Support) in 2022 dollars"
    ,title = "Observed group difference in reported income the first Income Support spell"
  )
# g2
g2 %>% quick_save("2-observed-diff",h=5,w=11)

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
