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


ds_pred <- 
  tibble::tribble(
  ~var_name,~value_level,~value_order,
  "tx",                    "FALSE",              0,
  "tx",                    "TRUE",               1,
  "gender2",               "(Missing)",          0,
  "gender2",               "Men",                1,
  "gender2",               "Women",              2,
  "marital3",              "(Missing)",          0,
  "marital3",              "never married",      1,
  "marital3",              "apart",              2,
  "marital3",              "together",           3,
  "education4",            "(Missing)",          0,
  "education4",            "Less HS",            1,
  "education4",            "High School",        2,
  "education4",            "Post HS",            3,
  "education4",            "University Degree",  4,
  "dependent4",            "0 dependents",       0,
  "dependent4",            "1 dependent",        1,
  "dependent4",            "2 dependents",       2,
  "dependent4",            "3+ dependents",      3,
  "disability2",           "Without Disability", 0,
  "disability2",           "With Disability",    1,
  "ethnicity",             "(Missing)",          0,
  "ethnicity",             "Caucasian",          1,
  "ethnicity",             "Visible Minority",   2,
  "ethnicity",             "Indigenous",         3,
  "immigration",           "(Missing)",          0,
  "immigration",           "born in Canada",     1,
  "immigration",           "immigrant",          2,
  "region7",               "North West",         0,
  "region7",               "North Central",      1,
  "region7",               "North East",         2,
  "region7",               "Edmonton",           3,
  "region7",               "Central",            4,
  "region7",               "Calgary",            5,
  "region7",               "South",              6,
  "year_before",           "2012",               0,
  "year_before",           "2013",               1,
  "year_before",           "2014",               2,
  "year_before",           "2015",               3,
  "year_before",           "2016",               4,
  "year_before",           "2017",               5,
  "spell_duration",        "0",                  0,
  "income_net_before",     "0",                  0,
  "earnings_total_before", "0",                  0,
  "income_total_before",   "0",                  0,
  "income_taxable_before", "0",                  0,
) %>% 
  mutate(
    reference = value_order==0L
  )

ds_pred
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

d1 <- 
  ds0 %>% 
  filter(intervention == "career_planning") %>% 
  filter(outcome == "income_net_delta") %>% 
  # full_join(
  #   ds_pred
  # 
  # ) %>% 
  mutate()
d1 %>% select(-c(5:9)) %>%  print_all()
# ---- graph-1 -----------------------------------------------------------------

graph_one_outcome <- function(d, outcome_name, w, h){
  # d <- d1
  # outcome_name = 'income_net_delta'
  # intervention_name = "career_planning"
  d1 <-
    d %>%
    # filter(outcome == "Applied for Benefits") %>%
    # filter(outcome == outcomes_of_interest_levels[outcome_name]) %>%
    filter(intervention == intervention_name) %>% 
    filter(outcome == outcome_name) %>% 
    filter(!is.na(value_level)) %>%
    mutate(
      predictor_level = paste0(var_name, " - ", value_level) 
    ) %>% 
    mutate(
      predictor_level = case_when(
        value_level=="(Intercept)" & is.na(var_name) ~ "Reference Group"
        ,TRUE ~ predictor_level
      ) %>% as_factor()
    ) %>% 
    # relocate(predictor_level, .before="term") %>%
    # select(-term) %>% 
    mutate(
      value_level = fct_drop(value_level)
      # ,sign_at_05 = ifelse(p.value <= .05, TRUE, FALSE)
      ,sign_at_05 = case_when(
        p_value <= .05 ~ TRUE, TRUE ~ NA
      )
    )
  (min_left <- d1 %>% filter(term!="(Intercept)") %>% summarize(min=min(estimate, na.rm = T) %>% pull(min))
  (min_left <- round(min_left - .1, 1))
  (min_left <- ifelse(min_left<0,0,min_left))
  (max_right <- d1 %>% summarize(max=max(estimate, na.rm =T)) %>% pull(max))
  (max_right <- round(max_right + .1, 1))
  # d1 %>% glimpse()
  # d1 %>% group_by(sign_at_05) %>% count()
  # browser()
  # d %>% glimpse()
  g <-
    d1 %>%
    {
      ggplot(., aes(x=estimate, y = predictor_level))+
        geom_vline(
          xintercept = d1 %>% filter(predictor_level == "aggregate - aggregate") %>% pull(probability)
          , linetype = "dashed", alpha = .5, size =1)+
        geom_point(shape = 21, aes(fill = sign_at_05), size = 3, alpha = .6)+
        geom_text(aes(label = scales::percent(probability, a=ccuracy = 1)), hjust = -0.3, color = "grey80")+
        scale_fill_manual(values = c("TRUE"="red"),na.value=NA)+
        scale_x_continuous(
          labels = scales::percent_format(accuracy=1)
          ,breaks = seq(0,1,.1)
          ,limits = c(min_left,max_right)
        )+
        labs(
          title = paste0("Performance of key demographic groups on\n ",toupper(outcomes_of_interest_levels[outcome_name]))
          # ,subtitle = paste0("Indicator: ", outcomes_of_interest_levels[outcome_name])
          ,x = "% clients with positive outcome"
          ,caption = "Statistical significance indicates levels of GROUP are different"
          ,color = "Significant at p < .05"
          ,fill = "Significant at p < .05"
        )+
        theme(
          legend.position = "bottom"
          ,strip.text.y = element_text(angle = 0)
        )+
        facet_grid(predictor ~ . ,space = "free",drop = TRUE, scales = "free")
    }
  g %>% quick_save(paste0("outcome - ",outcome_name),w=w, h=h)
  return(g)
}
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
