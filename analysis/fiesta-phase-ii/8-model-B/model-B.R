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
library(dplyr   )# Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
library(scales)
library(tidyr)
library(broom)
library(emmeans)
library(ggpubr)
library(twang)
# -- 2.Import only certain functions of a package into the search path.
library(magrittr)
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
base::source("./scripts/graphing/graph-presets.R") # project-level
base::source("./scripts/operational-functions.R") # project-level
base::source("./analysis/4-link-rdb-cra/binary-categorical-functions.R")
base::source("./analysis/4-link-rdb-cra/trajectory-change-functions.R")


base::source("./analysis/8-model-B/group-balancing-functions.R")
base::source("./analysis/8-model-B/nia-variables.R")
# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
# printed figures will go here:
prints_folder <- paste0("./analysis/8-model-B/prints/")
if (!fs::dir_exists(prints_folder)) {fs::dir_create(prints_folder)}

# to be used to print equations of the line in trajectory graphs
line_equation <- 
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        # ,aes(label = paste0(c(after_stat(rr.label),after_stat(eq.label)))) # can't get the same behavior
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse = TRUE
                        ,label.x = 0.1
                        ,label.y = 1.5,color = "blue",vjust=1.2) 
line_equation_only <- 
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        # ,aes(label = paste0(c(after_stat(rr.label),after_stat(eq.label)))) # can't get the same behavior
                        ,aes(label = paste(..eq.label.., sep = "~~~"))
                        ,parse = TRUE
                        ,label.x = 0.1
                        ,label.y = 1.5,color = "blue",vjust=1.2)  


# ---- declare-functions -------------------------------------------------------
`%not in%` <- Negate(`%in%`)

# ---- declare-globals-hidden --------------------------------------------------
sample_of_interest1 <- 1017460 


# ---- load-data ---------------------------------------------------------------
config      <- config::get()
# ds0 <- readr::read_rds("./data-private/derived/model-A/ds0.rds")
# ds1 <- readr::read_rds("./data-private/derived/model-A/ds1.rds")
# ds2 <- readr::read_rds("./data-private/derived/model-A/ds2.rds")
# ds3 <- readr::read_rds("./data-private/derived/model-A/ds3.rds")
ds4 <- readr::read_rds("./data-private/derived/eda-tax_year/ds4.rds")
# ds5 <- readr::read_rds("./data-private/derived/model-A/ds5.rds")
# ds_sample_track <- readr::read_rds("./data-private/derived/model-A/ds_sample_track.rds")

# ---- tweak-data-0 ------------------------------------------------------------


ds5B <-
  ds4 %>% 
  filter(has_before_and_after) %>% # cases with non-missing outcome BEFORE and AFTER, but may be missing at other time points
  # now keep only two time points
  filter(
    timeline_is == -1L | timeline_is > 0L # time points the year before or since exit == remove years on IS
    # notice that we exclude time points of Income Support (timeline_is == 0L)
  ) %>%
  mutate(
    across(
      .cols = where(is.factor)
      ,.fns = ~forcats::fct_drop(.) # to remove empty levels
    )
  ) %>% 
  mutate(
    waveL = case_when(timeline_is==-1L~0L,timeline_is >0 ~ timeline_is) # for easy specification in models
    ,waveF = waveF %>% relevel(ref="Before")
  )

# create a smaller verions to be used during development
ds5B_100 <-  ds5B %>% keep_random_id(n=100 ,seed = 1)
ds5B_1000 <- ds5B %>% keep_random_id(n=1000,seed = 1)

# ---- inspect-data-0 --------------
ds4 %>% glimpse()

ds4 %>% 
  filter(person_oid == sample_of_interest1) %>% 
  select(1:2, earnings_total, timeline_is, waveF, outcome_before) %>% 
  mask_ids()

ds5B %>% 
  filter(person_oid == sample_of_interest1) %>% 
  select(1:2, earnings_total, timeline_is, waveF, waveL, outcome_before) %>% 
  mask_ids()

ds5B %>%
  tableone::CreateTableOne(data=., strata = "waveF")

# ---- tweak-data-1 ------------------------------------------------------------
# on 2023-10-16 you stopped here
# It looks like we need to use ds6 and ds7 for model B. Reconcyle this
# data prep before stabilizing the modeling. 


# ---- inspect-data ------------------------------------------------------------
ds5B %>% filter(person_oid %in% sample_of_interest1) %>% mask_ids() %>%  glimpse()
# ---- inspect-design ------------------------------------------------------------

ds5B %>% filter(person_oid %in% sample_of_interest1) %>% mask_ids() %>%  select(all_of(c(design,outcomes)))
# Design variables
ds5B %>%  select(all_of(design)) %>% summary()
# ---- inspect-outcomes ------------------------------------------------------------
# Outcomes
tableone::CreateTableOne(
  data  = ds5B 
  ,vars = c(outcomes)
  ,strata = "waveF"
) %>% 
  print()

# What is the overall trend of the outcome over time since IS?
ds5B %>% 
  group_by(waveF) %>% 
  summarize(
    earnings_total_mean = mean(earnings_total,na.rm = T)
    ,earnings_total_median = median(earnings_total, na.rm = T)
    ,sample_size = n()
    ,.groups = "drop"
  ) %>% 
  print() %>% 
  tidyr::pivot_longer(
    cols = c("earnings_total_mean", "earnings_total_median","sample_size")
    ,names_to = "metric",names_prefix =  "earnings_total_"
  ) %>% 
  mutate(
    statistic = case_when(metric == "sample_size" ~ "count",
                          TRUE ~ "typical value")
  ) %>% 
  ggplot(aes(x=waveF, y = value, color=metric, shape=metric, group=metric))+
  geom_line()+
  # geom_point(size=3)+
  geom_label(aes(label=scales::comma(value,accuracy=1)))+
  scale_y_continuous(labels = scales::comma_format())+
  # geom_boxplot()+
  facet_wrap(facets = "statistic", ncol=1, scales = "free_y")+
  labs(
    y = "Total earnings (annual)"
    ,color = "Statistic", shape = "Statistic"
    ,x = "Time point relevant to the start of the Income Support spell"
  )

# ---- inspect-covariates ------------------------------------------------------------
# Covariates
# only those case were selected for estimating the impact of services
# which had the outcome measured at both waves (before/after IS)
# We examine the effect of this censorship on marginal distribution of  covariates
tableone::CreateTableOne(
  data = ds4 %>% filter(timeline_is == -1L) %>% mutate(across(where(is.factor),fct_drop))
  ,vars = covariates
  ,strata = "has_before_and_after"
) %>%
  print(formatOptions = list(big.mark = ","))

# tableone::CreateTableOne(
#   data = ds5B %>% filter(timeline_is == -1L)
#   ,vars = covariates
# ) %>%
#   print(formatOptions = list(big.mark = ","))

# ---- inspect-intervention ------------------------------------------------------------
# Outcomes
options(max.print = 2000)
# getOption("max.print")
tableone::CreateTableOne(
  data = ds5B %>% filter(waveL==1L) # no matter which wave, person-level
  ,vars = intervention
  
  ,factorVars = intervention
) %>% summary()
# student_finance_issues has no positives, drop


# Notes:
# 1. Less than 1% of our sample had Needs Identification assessment, which has
# extremely limited window of application
# We recommend it's being dropped from the list of "intervention"
# intervention_used <- setdiff(intervention,c("assessment_ni","ab_job_corps"))
# intervention_used <- setdiff(intervention,c("assessment_ni"))


# ---- empirical-reference-group -----------------------------------------------
# let us determine what combination of predictors used in the model is the most
# frequently occurring one, to serve as an "empirical reference group"

ds5B %>% 
  # filter(used_in_nia) %>% 
  filter(timeline_is == -1L) %>%
  # group_by_at( c( covariates )) %>%
  group_by_at( c( setdiff(covariates,c("outcome_before")) )) %>%
  # group_by_at( c( setdiff(covariates,c("age_in_years","outcome_before")),"age_category5" )) %>%
  # group_by_at( c( 
  #   str_replace_all(covariates,"spell_duration","spell_duration_f")
  #   ,"year_before")) %>%
  # group_by_at( setdiff(covariates,c("year_before")) ) %>% 
  summarize(
    person_count = n()
    ,.groups = "drop"
  ) %>% 
  ungroup() %>% 
  arrange(desc(person_count)) %>% 
  slice(1) %>% 
  t() 
# because of high  number of dimensions, the specific point of intersection maybe
# poorly populated. study the most common deviations from this point



# ---- group-balancing ---------------------------------------------------------
# script
# source("./analysis/8-model-B/group-balancing.R")
# implemented group balancing with twawng, using timeline_is == -1 as a time slice
# now the derived weights will be applied in estimation of effect

# ---- effect-presentation -----------------------
