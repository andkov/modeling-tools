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

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
# printed figures will go here:
prints_folder <- paste0("./analysis/8-model-A/prints/")
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

# ---- declare-globals-for-nia ------------------------------------------------

design_labels <- c(
  "person_oid"      = "Unique identifier"
  ,"date_start"     = "Starting date of the first Income Support spell"
  ,"date_end"       = "Ending date of the first Income Support spell"
  ,"tax_year"       = "Year of filing the taxes to the CRA"
  ,"wave"          = "Before or After Income Support Spell"
  ,"waveL"          = "Before(0) or After(1) IS"
  
)
design <- names(design_labels)

outcomes_labels <- c(
  # binary
  # "return12m"        = "Back on Income Support within 12 months of leaving it"
  # numeric
  # ,"spell_duration"  = "Duration (in  months) of the current Income Support spell"
  # ,"gap_duration"   = "Duration (in months) of the gap between the current and next IS spells"
  "earnings_total" = "Total amount reported as earnings in the tax year"
)
outcomes <- outcomes_labels %>% names()

covariates_labels <- c(
  "sex2"                = "Sex"
  # ,"age_category5"    = "Age Category"
  ,"age_in_years"       = "Age in years"
  ,"dependent2"         = "Has Dependents"
  # ,"dependent4"         = "Has Dependents"
  # ,"marital3"           = "Marital Status"
  ,"marital2"           = "Marital Status"
  ,"education4"         = "Education before IS"
  ,"disability2"        = "Lives with Disability"
  ,"ethnicity"          = "Ethnic category"
  ,"immigration"        = "Duration of Immigration"
  ,"region7"            = "Region of Service"
  # ,"region3"            = "Region of Service"
  ,"spell_duration_cat" = "IS spell duration (disretized)"
  ,"years_btw_waves_cat"     = "Number of years between waves"
  # ,"years_btw_waves"     = "Number of years between waves"
  ,"fy_is_start"        = "Fiscal Year in which IS started"
)
covariates <- covariates_labels %>% names()

intervention_impact <- c(
  # compute the impact of each, while balancing on all others:
  "career_planning"         = "Career Planning"                         
  ,"job_placement"           = "Job Placement"                             
  ,"exposure_course"         = "Exposure Course"                         
  
)
intervention_balance <- c(
  # Balance on but do NOT compute the impact of
  "assessment_snd"          = "Service Needs Determination" 
  ,"assessment_ea"           = "Employability"                             
  # ,"assessment_ni"           = "Needs Identification"   # ignore, too few
  
  ,"workshop_noncp"          = "Workshop (Non CP)"
  ,"english_as_second"       = "English as Second"                     
  ,"training_for_work"       = "Training for Work"                     
  ,"work_foundation"         = "Work Foundations"  
)

intervention_labels <- c(intervention_impact,intervention_balance )
intervention <- intervention_labels %>%  names()

# ---- declare-functions -------------------------------------------------------
`%not in%` <- Negate(`%in%`)



# ---- load-data ---------------------------------------------------------------

config      <- config::get()
ds0 <- readr::read_rds("./data-private/derived/model-A/ds0.rds")
ds1 <- readr::read_rds("./data-private/derived/model-A/ds1.rds")
ds2 <- readr::read_rds("./data-private/derived/model-A/ds2.rds")
ds3 <- readr::read_rds("./data-private/derived/model-A/ds3.rds")
ds3_long <- readr::read_rds("./data-private/derived/model-A/ds3_long.rds")
ds4 <- readr::read_rds("./data-private/derived/model-A/ds4.rds")
ds5 <- readr::read_rds("./data-private/derived/model-A/ds5.rds")
ds_sample_track <- readr::read_rds("./data-private/derived/model-A/ds_sample_track.rds")

# ---- tweak-data-0 ------------------------------------------------------------
ds5_100 <-  ds5 %>% keep_random_id(n=100 ,seed = 1)
ds5_1000 <- ds5 %>% keep_random_id(n=1000,seed = 1)
sample_of_interest1 <- 1017460 

# ---- tweak-data-1 ------------------------------------------------------------


# ---- inspect-design ------------------------------------------------------------
ds5 %>% keep_random_id() %>%  glimpse()
ds5 %>% filter(person_oid %in% sample_of_interest1) %>% mask_ids() %>%  glimpse()
ds5 %>% filter(person_oid %in% sample_of_interest1) %>% mask_ids() %>%  select(all_of(c(design,outcomes)))
# Design variables, describing the fundamental structure of the data
ds5 %>%  select(all_of(design)) %>% summary()
# ---- inspect-outcomes ------------------------------------------------------------
# Outcomes
tableone::CreateTableOne(
  data  = ds5 
  ,vars = c(outcomes,"wave")
  ,strata = "wave"
) %>% 
  print()
# ---- inspect-covariates ------------------------------------------------------------
# Covariates
tableone::CreateTableOne(
  data = ds5 %>% filter(wave=="Before")
  ,vars = covariates
  # ,factorVars = c("years_btw_waves")
) %>%
  summary()

# ---- empirical-reference-group -----------------------------------------------
# let us determine what combination of predictors used in the model is the most
# frequently occurring one, to serve as an "empirical reference group"

ds5 %>% 
  # filter(used_in_nia) %>% 
  group_by_at( c( covariates )) %>%
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

# ---- inspect-intervention ------------------------------------------------------------
# Outcomes
tableone::CreateTableOne(
  data = ds5 %>% filter(wave=="Before")
  ,vars = intervention
  ,factorVars = intervention
) %>% summary()
# We use the counts of intervention events  as quantification of the measure

# Notes:
# 1. Less than 1% of our sample had Needs Identification assessment, which has
# extremely limited window of application
# We recommend it's being dropped from the list of "intervention"
# intervention_used <- setdiff(intervention,c("assessment_ni","ab_job_corps"))
# intervention_used <- setdiff(intervention,c("assessment_ni"))



# ---- group-balancing ---------------------------------------------------------
# source("./analysis/8-model-A/group-balancing.R")
# producing the following:


# ---- longitudinal-graph-2 ---------------------
# let's plot the observed within-person change
# Conceptually, this is what we want to estimate, the equation of the line
ds5_100 %>% 
  ggplot(aes(x=waveL, y = earnings_total))+ 
  geom_point(shape=21, alpha = .5)+
  geom_line(aes(group = person_oid),alpha = .15)+
  # scale_x_continuous(breaks = c(0,1))+
  line_equation_only+
  geom_smooth(method = "lm",color="blue")+
  labs(
    title = "Individual change on the outcome "
    ,subtitle = paste0("Random sample of n = 100 out of N = ",
                       ds5 %>% summarize(n = n_distinct(person_oid)) %>% pull(n) %>% comma()
    )
  )


# ---- model-1 ---------------------
# empty growth

eq_formula <- as.formula("earnings_total ~ waveL")# Model equation
m <- glm(
  formula = eq_formula
  ,data = ds5
)

eq_emmeans <- "~ waveL" %>% as.formula() # EMM equation
e <- 
  m %>%  
  emmeans::emmeans(
    specs =   eq_emmeans# try adding pairwise before ~
    ,at   = list(waveL = c(0L,1L)) # custom points to evaluate
  )

# Create data set containing model predictions for conditions specified by `eq_emmeans` 
hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)
d_predict <-
  seq_len(nrow(e@linfct)) %>%                         # notice emmeans object `e`!
  purrr::map_dfr(function(i) as.data.frame(e[i])) %>% # notice emmeans object `e`!
  dplyr::mutate( #  now tweak for graphing
    outcome = "earnings_total"
    ,wave=factor(waveL,c(0,1),c("Before","After"))
  ) |>
  dplyr::select(
    outcome,
    waveL,
    wave,
    # rename on the fly and remind what we're bringing from emmeans object
    y_hat       = !!rlang::ensym(hat_name), # the outcome, modeled values
    se          = SE,
    ci_lower    = lower.CL,  #asymp.UCL
    ci_upper    = upper.CL #asymp.UCL
  ) 


# ---- model-2-examine-age -------
lsg <- list()
seed_range <- c(42,43,44,45)
sample_size <- 100
# i <- 1
for( i in seq_along(seed_range)){
  seed_i <- seed_range[i]
  lsg[[as.character(seed_i)]] <- 
    ds5%>%
    # ds5_100 %>%
    keep_random_id(n=sample_size,seed=seed_i) %>%
    ggplot(aes(x=age_in_years, y=earnings_total, color = wave))+
    geom_point(shape=21)+
    geom_smooth(aes(color=wave),method = "lm", se=F)+ # compare loess to lm
    geom_smooth(method = "lm", se=F,linetype = "dashed")+
    scale_y_continuous(labels = scales::comma_format())+
    line_equation+
    labs(
      title = "Relationship between Age and the Outcome in a random sample"
      ,subtitle = paste0(
        sample_size
        , " random cases (seed = ", seed_i, ") selected from the research population of N ="
        ,(ds5 %>% pull(person_oid) %>% unique() %>% length() %>% comma())
      )
    )
}
# lsg$`42`
# ---- model-2 ----------------------
eq_formula <- as.formula("earnings_total ~ waveL*age_in_years") # Model equation
m <- glm(
  formula = eq_formula
  ,data = ds5
)

eq_emmeans <- "~ age_in_years | waveL" %>% as.formula() # EMM equation
e <- 
  m %>%  
  emmeans::emmeans(
    specs =   eq_emmeans# try adding pairwise before ~
    ,at   = list(age_in_years = c(20,30,40,50,60)) # custom points to evaluate
  )

# Create data set containing model predictions for conditions specified by `eq_emmeans` 
hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)
d_predict <-
  seq_len(nrow(e@linfct)) %>%                         # notice emmeans object `e`!
  purrr::map_dfr(function(i) as.data.frame(e[i])) %>% # notice emmeans object `e`!
  dplyr::mutate( #  now tweak for graphing
    outcome = "earnings_total"
    ,wave=factor(waveL,c(0,1),c("Before","After"))
  ) |>
  dplyr::select(
    outcome,
    waveL,
    wave,
    age_in_years,
    # rename on the fly and remind what we're bringing from emmeans object
    y_hat       = !!rlang::ensym(hat_name), # the outcome, modeled values
    se          = SE,
    ci_lower    = lower.CL,  #asymp.UCL
    ci_upper    = upper.CL #asymp.UCL
  ) 


# ---- model-3-examine-sex -----------------------------------------------------

lsg <- list()
seed_range <- c(42,43,44,45)
sample_size <- 300
# i <- 1
for( i in seq_along(seed_range)){
  seed_i <- seed_range[i]
  lsg[[as.character(seed_i)]] <- 
    ds5%>%
    # ds5_100 %>%
    keep_random_id(n=sample_size,seed=seed_i) %>%
    ggplot(aes(x=waveL,y=earnings_total, color=sex2))+
    geom_point(alpha=.2, shape=21)+
    geom_line(aes(group=person_oid), alpha=.1)+
    geom_smooth(method="lm", aes(group = sex2), se=F)+
    geom_smooth(method="lm", aes(group = 1), se=F)+
    line_equation_only+
    facet_wrap("sex2")+
    labs(
      title = "Relationship between Sex and the Outcome in a random sample"
      ,subtitle = paste0(
        sample_size
        , " random cases (seed = ", seed_i, ") selected from the research population of N ="
        ,(ds5 %>% pull(person_oid) %>% unique() %>% length() %>% comma())
      )
    )
}
# lsg$`42`



# ---- model-3 ---------------------
# empty growth
eq_formula <- as.formula("earnings_total ~ waveL*sex2") # Model equation
m <- glm(
  formula = eq_formula
  ,data = ds5
)

eq_emmeans <- "~ sex2 | waveL" %>% as.formula() # EMM equation
e <- 
  m %>%  
  emmeans::emmeans(
    specs =   eq_emmeans# try adding pairwise before ~
    ,at   = list(sex2 = c("Women","Men")) # custom points to evaluate
  )

# Create data set containing model predictions for conditions specified by `eq_emmeans` 
hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)
d_predict <-
  seq_len(nrow(e@linfct)) %>%                         # notice emmeans object `e`!
  purrr::map_dfr(function(i) as.data.frame(e[i])) %>% # notice emmeans object `e`!
  dplyr::mutate( #  now tweak for graphing
    outcome = "earnings_total"
    ,wave=factor(waveL,c(0,1),c("Before","After"))
  ) |>
  dplyr::select(
    outcome,
    waveL,
    wave,
    sex2,
    # rename on the fly and remind what we're bringing from emmeans object
    y_hat       = !!rlang::ensym(hat_name), # the outcome, modeled values
    se          = SE,
    ci_lower    = lower.CL,  #asymp.UCL
    ci_upper    = upper.CL #asymp.UCL
  ) 

  

# ---- model-4-examine-sex-age -----------------------------------------------------

lsg <- list()
seed_range <- c(42,43,44,45)
sample_size <- 500
# i <- 1
for( i in seq_along(seed_range)){
  seed_i <- seed_range[i]
  lsg[[as.character(seed_i)]] <- 
    ds5%>%
    # ds5_100 %>%
    keep_random_id(n=sample_size,seed=seed_i) %>%
    ggplot(aes(x=age_in_seq, y=earnings_total, color = wave))+
    geom_point(shape=21)+
    geom_smooth(aes(color=wave),method = "lm", se=F)+ # compare loess to lm
    geom_smooth(method = "lm", se=F,linetype = "dashed")+
    line_equation_only+
    facet_wrap("age_category5", scales="free_x",nrow=1)+
    scale_y_continuous(labels = comma_format())+
    labs(
      title = "Relationship between Age and the Outcome in a random sample"
      ,subtitle = paste0(
        sample_size
        , " random cases (seed = ", seed_i, ") selected from the research population of N ="
        ,(ds5 %>% pull(person_oid) %>% unique() %>% length() %>% comma())
      )
    )
}
# lsg$`42`

# lsg$`42` %>% quick_save("temp",w=12,h=3)

# ---- model-4 ---------------------
# empty growth
eq_formula <- as.formula("earnings_total ~ waveL*sex2 + waveL*age_in_years + sex2*age_in_years*waveL") # Model equation
m <- glm(
  formula = eq_formula
  ,data = ds5
)

eq_emmeans <- "~ sex2*age_in_years | waveL" %>% as.formula() # EMM equation
e <- 
  m %>%  
  emmeans::emmeans(
    specs =   eq_emmeans# try adding pairwise before ~
    ,at   = list(
      sex2 = c("Women","Men")
      ,age_in_years = c(20,30,40,50,60)
      ) # custom points to evaluate
  )

# Create data set containing model predictions for conditions specified by `eq_emmeans` 
hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)
d_predict <-
  seq_len(nrow(e@linfct)) %>%                         # notice emmeans object `e`!
  purrr::map_dfr(function(i) as.data.frame(e[i])) %>% # notice emmeans object `e`!
  dplyr::mutate( #  now tweak for graphing
    outcome = "earnings_total"
    ,wave=factor(waveL,c(0,1),c("Before","After"))
  ) |>
  dplyr::select(
    outcome,
    waveL,
    wave,
    sex2,
    age_in_years,
    # rename on the fly and remind what we're bringing from emmeans object
    y_hat       = !!rlang::ensym(hat_name), # the outcome, modeled values
    se          = SE,
    ci_lower    = lower.CL,  #asymp.UCL
    ci_upper    = upper.CL #asymp.UCL
  ) 


# ---- model-5 ---------------------

# all covariates
eq_formula <- as.formula("earnings_total ~
                         waveL*sex2 +
                         waveL*age_in_years +
                         waveL*sex2*age_in_years + # interaction!!
                         waveL*dependent4 +
                         waveL*marital3 +
                         waveL*education4 +
                         waveL*disability2 +
                         waveL*ethnicity +
                         waveL*immigration +
                         waveL*region7 +
                         waveL*spell_duration_cat +
                         waveL*fy_is_start +
                         waveL*years_btw_waves_cat
                         ") # Model equation
m <- glm(
  formula = eq_formula
  ,data = ds5
)

eq_emmeans <- "~ 
sex2 *
age_in_years *
dependent4 *
marital3 *
education4 *
disability2 *
ethnicity *
immigration *
region7 *
spell_duration_cat *
fy_is_start *
years_btw_waves_cat *
waveL
" %>% as.formula() # EMM equation

e <- 
  m %>%  
  emmeans::emmeans(
    specs =   eq_emmeans# try adding pairwise before ~
    ,at   = list(
      # fix the levels of covariates 
      # if you don't specify the level, all possible are computed
      sex2 = c("Women","Men")
      ,age_in_years = c(20,30,40,50,60)
      # ,dependent4          = c("0 dependents")
      ,marital3            = c("never married")
      ,education4          = c("High School")
      ,disability2         = c("No Disability")
      ,ethnicity           = c("Caucasian")
      ,immigration         = c("born in Canada")
      ,region7             = c("Edmonton")
      ,spell_duration_cat  = c("2-3 months")
      ,fy_is_start         = c("2017")
      # ,years_btw_waves_cat = c("2")
      
    ) # custom points to evaluate
    ,
  )
# Create data set containing model predictions for conditions specified by `eq_emmeans` 
hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)
d_predict <-
  seq_len(nrow(e@linfct)) %>%                         # notice emmeans object `e`!
  purrr::map_dfr(function(i) as.data.frame(e[i])) %>% # notice emmeans object `e`!
  dplyr::mutate( #  now tweak for graphing
    outcome = "earnings_total"
    ,wave=factor(waveL,c(0,1),c("Before","After"))
  ) |>
  dplyr::select(
    outcome,
    waveL,
    wave,
    sex2,
    age_in_years,
    years_btw_waves_cat,
    # rename on the fly and remind what we're bringing from emmeans object
    y_hat       = !!rlang::ensym(hat_name), # the outcome, modeled values
    se          = SE,
    ci_lower    = lower.CL,  #asymp.UCL
    ci_upper    = upper.CL #asymp.UCL
    ,everything()
  ) 
# ---- model-6 ---------------------
# 
# # allowing focal covariates to interact, while holding the rest constant
# source("./analysis/8-model-A/group-balancing-functions.R")
# # why we should include both years_btw_wave and spell_duration
# ds5 %>% 
#   group_by(years_btw_waves_cat, spell_duration_cat) %>% 
#   summarize(
#     cell_size = n_distinct(person_oid)
#   )
# 
# # ds5 %>% 
# #   filter(years_btw_waves_cat=="3", spell_duration_cat=="24+ months") %>% 
# #   select(person_oid,tax_year, months_is_that_year, date_start, date_end, earnings_total) %>% print_all()
# #   
# 
# # all covariates
# eq_formula <- as.formula("earnings_total ~
#                          tx*waveL*sex2 +
#                          tx*waveL*age_in_years +
#                          tx*waveL*dependent2+
#                          tx*waveL*sex2*age_in_years*dependent2 
#                          # waveL*marital3 +
#                          # waveL*education4 +
#                          # waveL*disability2 +
#                          # waveL*ethnicity +
#                          # waveL*immigration +
#                          # waveL*region7 +
#                          # waveL*spell_duration_cat +
#                          # waveL*years_btw_waves_cat +
#                          # waveL*fy_is_start +
# 
#                          ") # Model equation
# m <- glm(
#   formula = eq_formula
#   ,data = ds5 %>% 
#     create_treatment_binary("career_planning")# %>% filter(person_oid==sample_of_interest1) %>% glimpse()
#     # make_treatment_binary("career_planning")# %>% filter(person_oid==sample_of_interest1) %>% glimpse()
#     
# )
# 
# eq_emmeans <- "~ 
# sex2 *
# age_in_years *
# dependent2 *
# # marital3 *
# # education4 *
# # disability2 *
# # ethnicity *
# # immigration *
# # region7 *
# # spell_duration_cat *
# # years_btw_waves_cat *
# # fy_is_start *
# tx *
# waveL
# " %>% as.formula() # EMM equation
# 
# e <- 
#   m %>%  
#   emmeans::emmeans(
#     specs =   eq_emmeans# try adding pairwise before ~
#     ,at   = list(
#       # fix the levels of covariates 
#       # if you don't specify the level, all possible are computed
#        sex2                 = c("Men","Women")
#       ,age_in_years        = c(20, 30, 40, 50, 60)
#       # ,dependent2          = c("0 dependents")
#       ,dependent2          = c("0 dependents","1+ dependents")
#       # ,marital3            = c("never married")
#       # ,education4          = c("High School")
#       # ,disability2         = c(FALSE)
#       # ,ethnicity           = c("Caucasian")
#       # ,immigration         = c("born in Canada")
#       # ,region7             = c("Edmonton")
#       # ,spell_duration_cat  = c("2-3 months")
#       # ,years_btw_waves_cat = c("2")
#       # ,fy_is_start         = c("2017")
#       ,tx                  = c(FALSE, TRUE)
#       
#     ) # custom points to evaluate
#     ,
#   )
# # Create data set containing model predictions for conditions specified by `eq_emmeans` 
# hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# # hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# # hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)
# d_predict <-
#   seq_len(nrow(e@linfct)) %>%                         # notice emmeans object `e`!
#   purrr::map_dfr(function(i) as.data.frame(e[i])) %>% # notice emmeans object `e`!
#   dplyr::mutate( #  now tweak for graphing
#     outcome = "earnings_total"
#     ,wave=factor(waveL,c(0,1),c("Before","After"))
#     ,age_in_years = factor(age_in_years)
#   ) |>
#   dplyr::select(
#     waveL,
#     wave,
#     tx,
#     outcome,
#     # rename on the fly and remind what we're bringing from emmeans object
#     y_hat       = !!rlang::ensym(hat_name), # the outcome, modeled values
#     se          = SE,
#     ci_lower    = lower.CL,  #asymp.UCL
#     ci_upper    = upper.CL #asymp.UCL
#     ,everything()
#   ) %>% as_tibble() 
#   # compute expected difference after the IS
# 
# d <- 
#   d_predict %>% 
#   # arrange(sex2, age_in_years, dependent2, wave, tx) %>%
#   # group_by(wave) %>% 
#   group_by_at(c("sex2", "age_in_years", "dependent2", "wave") )%>%
#   # group_by_at(c("sex2", "age_in_years", "wave") )%>% 
#   arrange(tx) %>% 
#   mutate(
#     change = y_hat -  lag(y_hat) 
#   ) %>% 
#   ungroup() %>% 
#   arrange_at(c("sex2", "age_in_years", "dependent2", "wave")) %>%
#   # arrange_at(c("sex2", "age_in_years", "wave")) %>% 
#   print_all()
# 
# 
# 
#   
# g <- 
#   d %>%# select()
#   mutate(tx_by_sex2 = paste0(tx,sex2)) %>% 
#   ggplot(aes(x=waveL,y = y_hat, color=tx, group = tx_by_sex2))+
#   geom_point(aes(shape = sex2), size=2)+
#   geom_line( linewidth=1.3, alpha = .4)+
#   facet_wrap(facets = c("dependent2", "age_in_years"), ncol=5)+
#   # facet_wrap(facets = c("age_in_years"), ncol=5)+
#   scale_shape_manual(values = c("Men"=25,"Women"=24))+
#   scale_color_brewer(type="qual",palette = "Dark2")+
#   scale_fill_brewer(type="qual",palette = "Dark2")+
#   scale_y_continuous(labels = scales::comma_format(), breaks = seq(0,50000,5000))+
#   scale_x_continuous(breaks = c(0,1), labels = scales::comma_format(accuracy = 1))+
#   labs(
#     title = "Estimated Marginal Means for chosen levels of covariates"
#     ,subtitle = "Other covariates fixed at: Never married | High School | Cacausian | Born in Canada | Edmonton | No disability | IS = 2-3 months | FY start = 2017"
#   )
# g


# ---- model-7 -------------------------
# main effect of the intervention ONLY 

source("./analysis/8-model-A/group-balancing-functions.R")
# all covariates
eq_formula <- as.formula("earnings_total ~ waveL*tx") # Model equation
m <- glm(
  formula = eq_formula
  ,data = ds5 %>% 
    create_treatment_binary("career_planning")# %>% filter(person_oid==sample_of_interest1) %>% glimpse()
  # make_treatment_binary("career_planning")# %>% filter(person_oid==sample_of_interest1) %>% glimpse()
  
)

eq_emmeans <- "~ tx *waveL" %>% as.formula() # EMM equation

e <- 
  m %>%  
  emmeans::emmeans(
    specs =   eq_emmeans# try adding pairwise before ~
    ,at   = list(
      # fix the levels of covariates 
      # if you don't specify the level, all possible are computed if variable found
      tx                   = c(FALSE, TRUE)
      ,sex2                = c("Men")
      # sex2                 = c("Men","Women")
      ,age_in_years        = c(21)
      # ,age_in_years        = c(20,30,40,50,60)
      ,dependent2          = c("0 dependents")
      # ,dependent2          = c("0 dependents","1+ dependents")
      ,marital3            = c("never married")
      ,education4          = c("High School")
      ,disability2         = c(FALSE)
      ,ethnicity           = c("Caucasian")
      ,immigration         = c("born in Canada")
      ,region7             = c("Edmonton")
      ,spell_duration_cat  = c("2-3 months")
      ,years_btw_waves_cat = c("2")
      ,fy_is_start         = c("2017")
      
    ) # custom points to evaluate
    
  )
# Create data set containing model predictions for conditions specified by `eq_emmeans` 
hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)
d_predict <-
  seq_len(nrow(e@linfct)) %>%                         # notice emmeans object `e`!
  purrr::map_dfr(function(i) as.data.frame(e[i])) %>% # notice emmeans object `e`!
  dplyr::mutate( #  now tweak for graphing
    outcome = "earnings_total"
    ,wave=factor(waveL,c(0,1),c("Before","After"))
    # ,age_in_years = factor(age_in_years)
  ) |>
  dplyr::select(
    waveL,
    wave,
    tx,
    outcome,
    # rename on the fly and remind what we're bringing from emmeans object
    y_hat       = !!rlang::ensym(hat_name), # the outcome, modeled values
    se          = SE,
    ci_lower    = lower.CL,  #asymp.UCL
    ci_upper    = upper.CL #asymp.UCL
    ,everything()
  ) %>% as_tibble() 
# compute expected difference after the IS



# ---- model-8 -------------------------
# main effect of the intervention ONLY, but for a fixed level of covariate
# you need to adjust the emmeans formula (not the model formula!) to specify
# the comparison across levels of covariates of interest 
# TODO: evaluate feasability of a dashboard servin2023-09-g all combinations



# all covariates
eq_formula <- as.formula("earnings_total ~
                         tx*waveL*sex2 +
                         tx*waveL*age_in_years +
                         tx*waveL*dependent2+
                         tx*waveL*marital3 +
                         tx*waveL*education4 +
                         tx*waveL*disability2 +
                         tx*waveL*ethnicity +
                         tx*waveL*immigration +
                         tx*waveL*region7 +
                         tx*waveL*spell_duration_cat +
                         tx*waveL*years_btw_waves_cat +
                         tx*waveL*fy_is_start 
                         ") # Model equation
m <- glm(
  formula = eq_formula
  ,data = ds5 %>% 
    create_treatment_binary("career_planning")# %>% filter(person_oid==sample_of_interest1) %>% glimpse()
  # make_treatment_binary("career_planning")# %>% filter(person_oid==sample_of_interest1) %>% glimpse()
  
)

eq_emmeans <- "~ 
# sex2 *
# age_in_years *
# dependent2 *
# marital3 *
# education4 *
# disability2 *
# ethnicity *
# immigration *
# region7 *
# spell_duration_cat *
# years_btw_waves_cat *
# fy_is_start *
tx *
waveL
" %>% as.formula() # EMM equation

e <- 
  m %>%  
  emmeans::emmeans(
    specs =   eq_emmeans# try adding pairwise before ~
    ,at   = list(
      # fix the levels of covariates 
      # if you don't specify the level, all possible are computed
      tx                  = c(FALSE, TRUE)
      ,sex2                 = c("Men")
      ,age_in_years        = c(21)
      ,dependent2          = c("0 dependents")
      # ,dependent2          = c("0 dependents","1+ dependents")
      ,marital3            = c("never married")
      ,education4          = c("High School")
      ,disability2         = c(FALSE)
      ,ethnicity           = c("Caucasian")
      ,immigration         = c("born in Canada")
      ,region7             = c("Edmonton")
      ,spell_duration_cat  = c("2-3 months")
      ,years_btw_waves_cat = c("2")
      ,fy_is_start         = c("2017")
      
    ) # custom points to evaluate

  )
# Create data set containing model predictions for conditions specified by `eq_emmeans` 
hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)
d_predict <-
  seq_len(nrow(e@linfct)) %>%                         # notice emmeans object `e`!
  purrr::map_dfr(function(i) as.data.frame(e[i])) %>% # notice emmeans object `e`!
  dplyr::mutate( #  now tweak for graphing
    outcome = "earnings_total"
    ,wave=factor(waveL,c(0,1),c("Before","After"))
    # ,age_in_years = factor(age_in_years)
  ) |>
  dplyr::select(
    waveL,
    wave,
    tx,
    outcome,
    # rename on the fly and remind what we're bringing from emmeans object
    y_hat       = !!rlang::ensym(hat_name), # the outcome, modeled values
    se          = SE,
    ci_lower    = lower.CL,  #asymp.UCL
    ci_upper    = upper.CL #asymp.UCL
    ,everything()
  ) %>% as_tibble() 
# compute expected difference after the IS










# ---- model-9 -----------------------------------------------------------------
# source("./analysis/8-model-A/group-balancing-functions.R")

# Model 9 is a modification of Model 8, in which 

# ---- save-to-disk ------------------------------------------------------------

# ---- publish, eval=F ------------------------------------------------------------
path <- "./analysis/6-eda-tax_year/eda-tax_year.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
