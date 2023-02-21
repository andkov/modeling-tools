rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console

# ---- load-packages -----------------------------------------------------------
# Choose to be greedy: load only what's needed
# Three ways, from least (1) to most(3) greedy:
# -- 1.Attach these packages so their functions don't need to be qualified: 
# http://r-pkgs.had.co.nz/namespace.html#search-path
library(ggplot2)   # graphs
library(forcats)   # factors
library(stringr)   # strings
library(lubridate) # dates
library(labelled)  # labels
library(dplyr)     # data wrangling
library(tidyr)     # data wrangling
library(twang)     # propensity score matching
library(explore)   # describe() function
library(magrittr)
# -- 2.Import only certain functions of a package into the search path.
# import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("janitor"  )# tidy data
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
base::source("./scripts/graphing/graph-presets.R") # project-level
base::source("./scripts/operational-functions.R") # project-level
source("./analysis/gender-1/binary-categorical-functions.R")
source("./analysis/gender-1/trajectory-change-functions.R")
# ---- declare-globals ---------------------------------------------------------
data_cache_folder <- "./data-private/derived/nia-3-cra/" # to sink modeling steps
# printed figures will go here:
prints_folder <- paste0("./analysis/nia-3-cra/prints/")
if (!fs::dir_exists(prints_folder)) {fs::dir_create(prints_folder)}
config      <- config::get()
sample_of_interest1_sin <- 642617708 # real id in focus
sample_of_interest1 <- 1017460 # real id in focus
# sample_of_interest1 <- 179820 # scramble id in focus

# what we are grabbing from the dispensary file
vars_demo <- c(
  "education3"
  ,"education4"
  ,"gender3"
  ,"gender2"
  ,"age_category3"
  ,"age_category5"
  ,"age_in_years"
  ,"age_in_years_out"
  ,"marital2"
  ,"marital3"
  ,"total_dependent_count"
  ,"dependent2"
  ,"dependent4"
  ,"disability2"
  ,"disability3"
  ,"ethnicity"
  ,"immigration"
  ,"region2"
  ,"region3"
  ,"region7"
  # ,"program_class2"
)

design_labels <- c(
  "person_oid"      = "Scrambled unique identifier"
  ,"date_start"     = "Starting date of the first Income Support spell"
  ,"date_end"       = "Ending date of the first Income Support spell"
)
design <- names(design_labels)

outcomes_labels <- c(
  # binary
  "return1_12m"        = "Back on Income Support within 12 months of leaving it"
  # numeric
  ,"spell_duration"      = "Duration (in  months) of the first Income Support spell"

  # ,"spell_duration" = "Duration (in months) of the first Income Support spell"
  # ,"gap_duration"   = "Duration (in months) of the gap between the first and second IS spells"
  # ,"return1_date"   = "Starting date of the second Income Support spell"
  # ,"return1"        = "Fist return to Income Support within..."
  
)
outcomes <- outcomes_labels %>% names()
outcomes_binary <- outcomes[1]

covariates_labels <- c(
   "gender2"            = "Sex"
  ,"age_category5"      = "Age Category"
  # ,"age_in_years"       = "Age in years"
  ,"marital3"           = "Marital Status"
  ,"education4"         = "Education before IS"
  ,"dependent4"         = "Has Dependents"
  ,"disability2"        = "Lives with Disability"
  ,"ethnicity"          = "Ethnic category"
  ,"immigration"        = "Duration of Immigration"
  ,"region7"            = "Region of survey"
  # ,"program_class2"     = "Program Class (2)"
  ,"year_before"        = "Year before IS"
  ,"spell_duration" = "Duration (in months) of the first Income Support spell"
 )
covariates <- covariates_labels %>% names()

intervention_labels <- c(
   "assessment_ea"    = "Employability"               
  ,"assessment_ni"    = "Needs Identification"        
  ,"assessment_snd"   = "Service Needs Determination" 
  ,"english_as_second"= "English as Second"           
  ,"exposure_course"  = "Exposure Course"             
  ,"job_placement"    = "Job Placement"               
  ,"training_for_work"= "Training for Work"           
  ,"work_foundation"  = "Work Foundations"            
  ,"career_planning"  = "Career Planning WS"          
  ,"workshop_noncp"   = "Workshop nonCP"      
  ,"ab_job_corps"     =   "Alberta Job Corps"   
)
intervention <- intervention_labels %>%  names()

# ---- declare-functions -------------------------------------------------------
rundown <- function(d, client_id = "person_oid",service_id = "edb_service_id"){
  d %>% 
    summarize(
      person_count = n_distinct(!!rlang::sym(client_id))
      ,event_count = n_distinct(!!rlang::sym(service_id))
      # ,row_count = n()
      ,.groups = "drop"
    )
}

`%not in%` <- Negate(`%in%`)

stint_rundown <- function(d_in){
  
  group_vars <- attr(d_in,"groups") %>% names() %>% setdiff(".rows")
  
  d_out <-
    d_in %>% 
    summarize(
      person_count = n_distinct(person_oid)
      ,duration_median = median(is_spell_duration) %>% round()
      ,duration_mean = mean(is_spell_duration) %>% round(1)
      ,duration_sd = sd(is_spell_duration,na.rm = T) %>% round(1)
      ,.groups="drop"
    )  %>% 
    mutate(
      person_prop = person_count/sum(person_count)
      ,person_pct = scales::percent(person_prop, accuracy = .1)
    ) %>% 
    select(any_of(group_vars),starts_with("person_"), everything())
  return(d_out)
}

# Compute difference in months
# turn a date into a 'monthnumber' relative to an origin
month_number <- function(d) { 
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon 
} 
# compute a month difference as a difference between two monnb's
month_diff <- function(date1, date2) { month_number(date2) - month_number(date1)+1 }

# drop variables with person IDs - handy when print illustrative cases
drop_idvars <- function(d,add_name="none"){
  known <- c("person_oid","edb_service_id","survey_id","sin")
  added <- add_name
  d_out <- d %>% select(-any_of(c(known,added)))
  return(d_out)
}
# how to use
# mtcars %>% drop_idvars("mpg")


# ---- load-data ---------------------------------------------------------------
ds_event    <- readr::read_rds("./data-private/derived/dispensary-ds4.rds") %>% 
  rename(event_duration = is_spell_duration) %>%  # temp fix
  select(all_of(c( setdiff( names(.),vars_demo),vars_demo)))

ds_interval <- readr::read_rds("./data-private/derived/dispensary-ds6.rds") 

# product of cra-eda-2.R (via dfa-2.R), linked taxes and spells
ds_delta <- readr::read_rds("./data-private/derived/cra-eda-2-ds3_wide.rds")

# ---- tweak-data -----------------------------------------------------------
# Next, we zoom on on the experiences of the first IS spell
random_id <- ds_interval %>% keep_random_id(n=1) %>% pull(person_oid) %>% unique()
d1 <- 
  ds_interval %>% 
  # filter(person_oid %in% random_id) %>% 
  # filter(person_oid %in% c(sample_of_interest1)) %>% #dev
  # filter(person_oid %in% c(sample_of_interest1,989022)) %>% #dev
  filter(interval_type == "stint" & interval_num == 1L) %>% # FIRST spell of Income Support
  select(person_oid, date_start, date_end) 
d1 %>%  filter(person_oid %in% c(sample_of_interest1)) %>% drop_idvars()
d2 <- 
  ds_interval %>% 
  # filter(person_oid %in% random_id) %>% 
  # filter(person_oid %in% c(sample_of_interest1)) %>% #dev
  # filter(person_oid %in% c(sample_of_interest1,989022)) %>% #dev
  select(1:7) %>% 
  filter(interval_type == "stint" & interval_num == 3L) %>% # SECOND spell of income Support
  group_by(person_oid) %>% 
  mutate(
    return1_date = date_start
  ) %>% 
  ungroup() %>% 
  select(person_oid, return1_date)
d2  %>%  filter(person_oid %in% c(sample_of_interest1)) %>% drop_idvars()
ds_spell <-
  left_join(
    d1
    ,d2
    ,by = "person_oid"
  ) %>% 
  mutate(
    spell_duration = month_diff(date_start, date_end)
    ,gap_duration = (month_diff(date_end,return1_date)-1) %>% replace_na(0L)
    ,return1 = case_when(
      is.na(return1_date) ~ "Did not return"
      ,return1_date < (date_end+365) ~ "Returned <=12m"
      ,return1_date > (date_end+365) ~ "Returned 12m+"
    ) %>% fct_relevel("Did not return","Returned <=12m", "Returned 12m+")
    # ,return_eventual = case_when(return1=="Did not return"~FALSE,TRUE~TRUE)
    ,return1_12m = case_when(
      return1_date < (date_end+365) ~ TRUE,TRUE ~ FALSE
    )
  ) %>% 
  select(
    1:3, spell_duration, gap_duration, return1_date, everything()
  )

rm(d1,d2) 
# ---- inspect-data ------------------------------------------------------------
# Table of Events - Instances of service engagement (IS,EA,ES)
ds_event %>% 
  filter(person_oid == sample_of_interest1 ) %>% # hand-picked person to illustrate typical data
  select(3,4,6,7,is_spell_number,event_duration, intervention) %>% drop_idvars()
# Demographic measures taken at the beginning of the spell
ds_event %>%  filter(person_oid %in% sample_of_interest1) %>% drop_idvars() %>% glimpse()

# Event table is then transformed to summarize intervals and their attributes
# Intervals - spells of Income Support or gaps between them
ds_interval %>% filter(person_oid %in% sample_of_interest1) %>% select(2:5)# rest of cols = intervention summary
# each intervention is represented 4 columns, each storing specific measure  
# `_count`     - total number events of this type  
# `_dur_total` - total duration (in days) of the intervention, summed across all events  
# `_start`     - the date of start of the earliest event   
# `_end`       - the date of end of the latest event   
ds_interval %>% filter(person_oid %in% sample_of_interest1)%>% drop_idvars() %>% glimpse()
# We focus on experiences of engaging Income Support for the first time

# Table of Spells (of engaging Income Support for the first time)
ds_spell %>% filter(person_oid %in% sample_of_interest1) %>% drop_idvars()
# we will use `ds_spell` as the pivotal table of our session, which can be
# augmented with necessary information, such as demographics at start of the spell
ds_spell %>% 
  filter(person_oid %in% sample_of_interest1) %>% 
  left_join(
    ds_event %>% 
      filter(program_class0=="IS" ) %>% # because we have other events too 
      select(
        person_oid, date_start, date_end, 
        gender2, education4
      )
  ) %>% 
  drop_idvars() %>% 
  glimpse()
# or augmented with the summary of the intervention relative to the first spell
ds_spell %>% 
  filter(person_oid %in% sample_of_interest1) %>% 
  left_join(ds_interval) %>% 
  drop_idvars() %>% 
  glimpse()
# we root the primary line of data forms in `ds_spell`
# i.e (ds_event + ds_interval) --> ds_spell --> ds0 --> ds1
ds_spell %>% summarize(person_count = n_distinct(person_oid))
# ---- spell-duration -------
# How long do clients typicaly stay on Income Support? 
ds_spell %>% 
  mutate(over20 = spell_duration>=20) %>% 
  ggplot(aes(x=spell_duration))+geom_histogram()+
  facet_wrap(facets = "over20", ncol=1, scales= "free_y")

# ---- reminder-1 ------------------------------------------------------------
# commented out lines below are a copy from above as a reminder
# intervention_labels <- c(
#   "Employability"                 = "assessment_ea"
#   ,"Needs Identification"         = "assessment_ni"
#   ,"Service Needs Determination"  = "assessment_snd"
#   ,"English as Second"            = "english_as_second"
#   ,"Exposure Course"              = "exposure_course"
#   ,"Job Placement"                = "job_placement"
#   ,"Training for Work"            = "training_for_work"
#   ,"Work Foundations"             = "work_foundation"
#   ,"Career Planning WS"           = "career_planning"
#   ,"Workshop nonCP"               = "workshop_noncp"
# )
# # intervention_levels <- intervention_labels %>%  names()
# intervention <- intervention_labels %>%  as.character()

# covariates_labels <- c(
#   "gender2"             = "Sex"
#   ,"age_category5"      = "Age Category"
#   ,"marital3"           = "Marital Status"
#   ,"education4"         = "Education before IS"
#   ,"dependent4"         = "Has Dependents"
#   ,"disability"         = "Lives with Disability"
#   ,"ethnicity"          = "Ethnic category"
#   ,"immigration"        = "Duration of Immigration"
#   ,"region7"            = "Region of survey"
# )
# covariates_names <- covariates_labels %>% names()

# ---- tweak-data-0 -----------------------------------------------------------
# this chunk will assemble the data table scoped for analysis
ds0 <- 
  ds_spell %>% 
  # filter(person_oid %in% sample_of_interest1) %>% # dev
  # add covariates as needed
  left_join(
    ds_event %>% 
      filter(program_class0=="IS" ) %>% # because we have other events too 
      select(person_oid,date_start,date_end, any_of(c(covariates,"age_in_years")))
  ) %>% 
  # add intervention
  left_join(
    ds_interval %>% select(1:5,ends_with("_count"))
  )

# ---- inspect-data-0 ---------------------------------------------  
# ds0 %>% filter(person_oid %in% sample_of_interest1) %>% glimpse()
# we chose `count` as the metric of intervention (other options: duration, start or end date)
names(ds0) <- str_remove_all(names(ds0),"_count$")
ds0 %>%  filter(person_oid %in% sample_of_interest1) %>% drop_idvars() %>% glimpse()

# ---- tweak-data-1 -------------------------------------------------------------
ds1_asis <-
  ds0 %>%
  as_tibble() %>%
  select(any_of(c(design, outcomes, covariates, intervention,"age_in_years"))) %>% 
  filter(region7 %not in% c("Virtual","Unknown")) %>% # based on later review
  # filter(age_category5 %not in% "(Missing)") %>%   # based on later review
  filter(!is.na(age_in_years)) %>% # too few cases with unknown
  # convert to factors for handling convenience
  mutate(
  # TODO for Matthew: finish the rest of the covariates
  gender2 = factor(gender2,
                   levels = c("male","female")
                   ,labels = c("Men", "Women")
                   ) %>% fct_explicit_na() %>% relevel(ref="Men")
  ,age_category5 = factor(age_category5,
                   levels = c("youth","middle age 1","middle age 2","middle age 3","senior")
                   ) %>% fct_explicit_na() %>% relevel(ref="middle age 1")
  ,marital3 = factor(marital3,
                      levels = c("never married", "apart", "together"))%>% 
    fct_explicit_na() %>% relevel(ref="never married")
  ,education4 = factor(education4,
                       levels = c("Less HS", "High School", "Post HS", "University Degree"))%>%
    fct_explicit_na() %>% relevel(ref="High School")
  ,dependent4 = factor(dependent4,
                       levels = c("0 dependents", "1 dependent", "2 dependents", "3+ dependents"))%>% 
    fct_explicit_na() %>% relevel(ref = "0 dependents")
  ,ethnicity = factor(ethnicity,
                      levels = c("Caucasian", "Visible Minority", "Indigenous"))%>%
    fct_explicit_na()%>% relevel(ref="Caucasian")
  ,immigration = factor(immigration) %>% fct_explicit_na() %>%
    fct_recode("born in Canada" =  "(Missing)") %>% relevel(ref="born in Canada") # !!! assumption  !!!
    
  # ,immigration = factor(immigration,
  #                       levels = c("born in Canada", "immigrant"))%>%
  #   fct_explicit_na()%>% relevel(ref="(Missing)")
  ,disability2 = factor(disability2,levels = c("TRUE","FALSE"),
                        labels = c("With Disability","Withouth Disability"))%>% 
    fct_explicit_na() %>% relevel(ref = "Withouth Disability")
  ,region7 = factor(region7,
    levels = c( "Edmonton","North West", "North Central", "North East", "Central", "Calgary", "South") 
  )
  ,spell_duration = case_when(
    spell_duration > 0 & spell_duration <= 1 ~ "1 month",
    spell_duration > 1 & spell_duration <= 3 ~ "2-3 months",
    spell_duration > 3 & spell_duration <= 6 ~ "4-6 months",
    spell_duration > 6 & spell_duration <= 11 ~"7-11 months",
    spell_duration >= 12 & spell_duration <= 23 ~"12-23 months",
    spell_duration >= 24 ~ "24+ months"

   ) %>%
    factor(levels = c("1 month", "2-3 months","4-6 months","7-11 months", "12-23 months","24+ months"))
  ) %>% 

  mutate(
    across(
      .cols = intervention
      ,.fns = ~ replace_na(.,0L)
    )
  ) 

# ds1_asis %>% count(region7)
# ds1_asis %>% select(any_of(covariates)) %>% look_for()
(ds1_asis %>% select(any_of(covariates))) %>% 
  tableone::CreateTableOne(data=.) %>% summary()

# ds1_asis %>% group_by(spell_duration, spell_duration_cat) %>% 
#   count() %>% arrange() %>% print_all()

# covariates

ds1_rollup <- 
  ds1_asis %>% 
  # right-truncate the cout values of interventions to prepare for balancing, based on visual investigation
  mutate(
    assessment_ea      = case_when(assessment_ea     >= 4L ~ 4L,TRUE~assessment_ea     )
    ,assessment_ni     = case_when(assessment_ni     >= 1L ~ 1L,TRUE~assessment_ni     )
    ,assessment_snd    = case_when(assessment_snd    >= 6L ~ 6L,TRUE~assessment_snd    )
    ,english_as_second = case_when(english_as_second >= 6L ~ 6L,TRUE~english_as_second )
    ,exposure_course   = case_when(exposure_course   >= 7L ~ 7L,TRUE~exposure_course   )
    ,job_placement     = case_when(job_placement     >= 3L ~ 3L,TRUE~job_placement     )
    ,training_for_work = case_when(training_for_work >= 3L ~ 3L,TRUE~training_for_work )
    ,work_foundation   = case_when(work_foundation   >= 5L ~ 5L,TRUE~work_foundation   )
    ,career_planning   = case_when(career_planning   >= 5L ~ 5L,TRUE~career_planning   )
    ,workshop_noncp    = case_when(workshop_noncp    >= 7L ~ 7L,TRUE~workshop_noncp    )
  ) %>% 
  mutate(
    across(
      .cols = intervention
      ,.fns = ~factor(.) %>% fct_reorder(.)
    )
  )

ds1_binary <- 
  ds1_asis %>% 
  mutate(
    across(
      .cols = intervention
      ,.fns = ~case_when(
        . == 0L ~ 0L # intervention absent
        ,. > 0L ~ 1L # intervention present
        ,TRUE ~ .
      )
    )
  )
# We would like to distinguish several versions of this data form `ds1`
# ds1_asis - intervention values (counts) remain as is
# ds1_rollup - intervention values are right-truncated at cut-offs derived during outlier management
# ds1_binary - intervention values converted: (value > 0) --> 1
ds1 <- ds1_binary # we begin with binary, as offering the simplest cases for group balancing
# but we may want to experiment with rollup and asis versions as well. 
# tableone::CreateTableOne(data=ds1,vars = covariates) %>% summary()
# ds0 %>% count(age_category5)
# ds1 %>% count(age_category5)

ds1_asis %>%  filter(person_oid %in% sample_of_interest1) %>% glimpse()
ds1_rollup %>%  filter(person_oid %in% sample_of_interest1) %>% glimpse()
ds1_binary %>%  filter(person_oid %in% sample_of_interest1) %>% glimpse()
ds1 %>%  filter(person_oid %in% sample_of_interest1) %>% glimpse()
ds1 %>% glimpse()

# --- save-to-disk-rdb --------------------------------------------------------
# At this point, we would like to save this table to disk and link it to tax data
ds1 %>% readr::write_rds("./data-private/derived/nia-3-ds1.rds")




# ---- tweak-data-2 ---------------------------------------------------------
# this chunks brings in taxed data from CRA  
ds_delta %>% rundown(service_id = "sin")
ds2 <- 
  # ds1 %>% 
  ds1_asis %>% 
  left_join(
    ds_delta %>% 
      mutate(
        year_before = factor(year_before) %>% relevel(ref="2016")
        ,used_in_nia = TRUE
      ) 
    
  ) %>% 
  mutate(
    used_in_nia = case_when(is.na(used_in_nia) ~ FALSE,TRUE~used_in_nia)
    ,used_in_nia = case_when(
      year_before=="2018" ~ FALSE # because year_after is during COVID
      ,TRUE ~ used_in_nia
    )
  )  
  # left_join(
  #   ds_delta %>% 
  #     filter(year_before!=2018) %>% # because year_after is during COVID
  #     mutate(
  #       year_before = factor(year_before) %>% relevel(ref="2016")
  #       
  #       ) 
  #   
  # ) %>% 
  # mutate(
  #   used_in_nia = sin %in% (ds_delta %>% pull(sin) %>% unique())
  # )
# ds2 %>% glimpse()
ds2 %>% 
  filter(used_in_nia) %>% 
  sample_frac(.1) %>% 
  ggplot(aes(x=age_in_years, y= income_total_before))+
  geom_point(shape=21)
# ---- inspect-data-2 ---------------------------------------------------
ds2 %>% 
  group_by(used_in_nia) %>% 
  summarize(
    count_oid = n_distinct(person_oid)
  )

ds2 %>% 
  filter(person_oid %in% sample_of_interest1) %>%
  drop_idvars() %>% 
  glimpse()

outcomes_labels_cra <- c(
  "income_total_delta"    = "Total Income (pre- vs post- IS delta)"
  ,"income_taxable_delta" = "Total Income (pre- vs post- IS delta)"
  ,"income_net_delta"     = "Total Income (pre- vs post- IS delta)"
  ,"earnings_total_delta" = "Total Income (pre- vs post- IS delta)"
)

outcomes_cra <- names(outcomes_labels_cra)

# ---- inspect-design ------------------------------------------------------------
# Design variables
ds2 %>% filter(used_in_nia) %>% select(all_of(design)) %>% summary()
# ---- inspect-outcomes ------------------------------------------------------------
# Outcomes
tableone::CreateTableOne(
  data  = ds2 
  ,vars = c(outcomes_cra)
  ,strata = "used_in_nia"
) %>% 
  print()
# ---- inspect-covariates ------------------------------------------------------------
# Covariates
tableone::CreateTableOne(
  data = ds2 %>% filter(used_in_nia)
  ,vars = covariates
) %>%
  summary()

# After linking Income Support data to CRA data, we reduced the effective size of cohort
# We examine the degree to which the linked data resemble the research cohort
tableone::CreateTableOne(
  data = ds2 #%>% filter(used_in_nia)
  ,vars = covariates
  ,strata = "used_in_nia"
) %>%
 print(formatOptions = list(big.mark = ","))


# ---- empirical-reference-group -----------------------------------------------
# let us determine what combination of predictors used in the model is the most
# frequently occurring one, to serve as an "empirical reference group"

ds2 %>% 
  filter(used_in_nia) %>% 
  group_by_at( c( covariates ,"year_before")) %>%
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
  data = ds2
  ,vars = intervention
  ,strata = "used_in_nia"
  ,factorVars = intervention
)
# Notes:
# 1. Less than 1% of our sample had Needs Identification assessment, which has
# extremely limited window of application
# We recommend it's being dropped from the list of "intervention"
# intervention_used <- setdiff(intervention,c("assessment_ni","ab_job_corps"))
intervention_used <- setdiff(intervention,c("assessment_ni"))


# ---- make-treatment-function ------------------------------------------------------------
# adds a binary variable 'tx', encoding the presence or absence of the intervention
make_treatment_binary <- function(
  d
  ,tx_var     # one of the intervention variable used to create a binary treatment variable
){
  d <- ds1
  d1 <- 
    d %>% 
    mutate(
     !!rlang::sym(tx_var) :=
        ( !!rlang::sym(tx_var) %>% as.character() %>% as.integer() ) > 0L
    )
}

# replaces the target variable with a binary variable `tx`
create_treatement_binary <- function(
  d
  ,tx_var     # one of the intervention variable used to create a binary treatment variable
){
  d1 <- 
    d %>% 
    mutate(
      tx = (!!rlang::sym(tx_var) %>% as.character() %>% as.integer()) > 0L
    )  %>% 
    select(-!!rlang::sym(tx_var)) 
}
# How to use:
# ds2 %>%
#   filter(person_oid %in% sample_of_interest1) %>%
#   create_treatement_binary("career_planning") %>%
#   select(-person_oid) %>%
#   glimpse()


# ---- inspect-treatment -------------------------------------------------------------
ds2 %>%
  filter(person_oid %in% sample_of_interest1) %>%
  create_treatement_binary("career_planning") %>%
  drop_idvars() %>% 
  glimpse()




# ---- rd1-initial-differences ------------------------------------------------
# for(i in seq_along(intervention)){intervention[i] %>% print()}
focal_intervention <- "career_planning" # this determines the loop
# d_nia - data analysis 1
d_nia <- 
  ds2 %>% filter(used_in_nia) %>% 
  select(-assessment_ni) %>% # because <1% of the sample have it
  create_treatement_binary(tx_var = focal_intervention)
# d_nia %>%  filter(person_oid %in% sample_of_interest1) %>% glimpse()

# nonnormal_vars <- c(intervention_used)# matters when not binary
tab1 <- tableone::CreateTableOne(data=d_nia %>% select(-person_oid), strata = "tx")
tab1 %>%  print(formatOptions = list(big.mark = ",")) # add nonnormal= when not binary


# ---- rd1-ps-model ---------------------------------------------------------------
# Propensity Score Model ----
dependent <- "tx"

explanatory <- c(covariates,setdiff(intervention_used,focal_intervention), 
                 c(
                   "income_total_before"
                   ,"income_net_before"
                   ,"income_taxable_before"
                   ,"earnings_total_before"
                 )
)
# explanatory <- setdiff(explanatory, "program_class2")
(eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) ))

# ps1 <-
#   ps(
#     # propensity score model
#     formula            = eq_formula
#     ,estimand          = "ATT" # use ATT because that is consistent with ex-post net impact
#     ,data              = d_nia %>% as.data.frame()
#     # ,sampw             = d_nia$survey_weight # !!! optional
#     # gradient boosting
#     ,stop.method       = c("es.mean", "es.max", "ks.mean", "ks.max")
#     ,n.trees           = 5000
#     ,interaction.depth = 2
#     ,shrinkage         = 0.01
#     ,n.minobsinnode    = 10
#     # computational efficiency
#     ,n.keep            = 1
#     ,n.grid            = 25
#     ,ks.exact          = NULL
#     ,version           = "gmb" # gmb, xboost, legacy
#     # other
#     ,verbose           = FALSE
#   )
# readr::write_rds(ps1,"./data-private/derived/ps1_binary.rds") # off after first run
ps1 <- readr::read_rds("./data-private/derived/ps1_binary.rds") # to speed up report




# ----- rd1-diagnostics-1 ---------------------------------------------
summary(ps1) %>% round(3)
bt1 <-
  ps1 %>%
  bal.table() %>% # gets balance table
  purrr::map(tibble::rownames_to_column,"covariate") # add covariate names
# Stop Method (es.mean, es.max, ks.mean, ks.max)
# What is being used to guide the search algorithm
# Group imbalance is measured with two metrics:
# `es` - effect size (standardized), (meanTX - meanCT)/sdTX (in ATT)
# `ks` - Kolmogorov-Smirnov (non-parametric)
# one table = one stopping methods ()
d <- bal.table(ps1)$es.mean.ATT
d %>% slice(11:17) %>% neat() # only for gender and age!
# ----- rd1-diagnostics-1b ---------------------------------------------
st <- summary(ps1) %>% round(4)
st[2,c(5:7,9)] # summary for es.mean.ATT as an example
d %>% summarize(  max.es  = std.eff.sz %>% max(na.rm =T)             )
d %>% summarize(  mean.es = std.eff.sz %>% abs() %>% mean(na.rm=T)  )
d %>% summarize(  max.ks  = ks %>% max(na.rm=T)                     )
d %>% summarize(  mean.ks = ks %>% abs() %>% mean(na.rm=T)          )

# ----- rd1-diagnostics-2 ---------------------------------------------
# we combine the balance tables from all methods into a single
# tibble for easier exploration
dbt <-
  bt1 %>%
  dplyr::bind_rows(.id = "method") %>%
  janitor::clean_names() %>%
  as_tibble()%>%
  mutate(
    covariate = as_factor(covariate)
  )
dbt
# dbt %>% group_by(method) %>% count() # 51 covariates (binary)
# dbt %>% group_by(covariate) %>% count() # 4 methods + unweighted

# ----- rd1-diagnostics-3 ---------------------------------------------
# we can compute other meaningful metrics of group imbalance, for example
# standard distance will measure an average effect size using squared values,
# as opposed to absolute values of `std.eff.sz`, reported in summary table as `es.mean`
d_other_summary_statistic <-
  dbt %>%
  group_by(method) %>%
  summarize(
    es_mean            = std_eff_sz %>% abs() %>% mean(na.rm=T) # reported in summary table
    ,standard_distance = (std_eff_sz)^2 %>% mean(na.rm=T) %>% sqrt()  # custom metric
    
  ) %>%
  arrange(desc(standard_distance))
d_other_summary_statistic #

# ----- rd1-diagnostics-4 ---------------------------------------------
d <-
  dbt %>%
  # filter(!method == "unw") %>%
  mutate(
    std_eff_sz = abs(std_eff_sz)
    ,metric = case_when(
      method %in% c("es.max.ATT","ks.max.ATT") ~ "max"
      ,method %in% c("es.mean.ATT","ks.mean.ATT") ~ "mean"
      ,TRUE ~ "unweighted"
    )
    ,distribution = case_when(
      method %in% c("es.mean.ATT","es.max.ATT") ~ "Standardized Effect Size"
      ,method %in% c("ks.mean.ATT","ks.max.ATT") ~ "Kolmogorov-Smirnov"
      ,TRUE ~ "unweighted"
    )
  ) %>%
  group_by(method) %>%
  mutate(
    es_mean            = std_eff_sz %>% abs() %>% mean(na.rm=T) # reported in summary table
    ,standard_distance = (std_eff_sz)^2 %>% mean(na.rm=T) %>% sqrt()  # custom metric
  ) %>%
  ungroup()
d %>% glimpse()

g <-
  d %>%
  ggplot(aes(
    x  = std_eff_sz
    ,y = covariate
    ,color = metric
    ,fill = metric
    # ,shape = metric
  )) +
  geom_vline(aes(xintercept= es_mean, color = metric),show.legend = F)+
  geom_vline(aes(xintercept= standard_distance, color = metric),show.legend = F,linetype="dotdash")+
  geom_vline(xintercept= .1, color = "black",linetype="dashed")+
  geom_vline(xintercept= 0, color = "black",linetype="solid",alpha =.3)+
  facet_wrap(facets = c("distribution"), scales = "free_x")+
  # scale_shape_manual(values = c("max"=20,"mean"=10))+
  # scale_shape_manual(values = c("20"))+
  geom_point(shape=21,alpha = .2, size = 3)+
  geom_point(shape=21,alpha = .8, size = 3,fill=NA)+
  # scale_x_continuous(breaks = seq(0,.3,.05), minor_breaks = seq(0,.3,.01),labels = RemoveLeadingZero)+
  scale_x_continuous(breaks = seq(-1,.9,.05), minor_breaks = seq(-1,.9,.01),labels = RemoveLeadingZero)+
  labs(
    x = "Group imbalance (lower = better)"
    ,fill = "Metric\nof group\nimbalance"
    ,color =  "Metric\nof group\nimbalance"
    ,title = "Group balance across individual covariates"
    ,subtitle = "solid color = average effect size; dotdash color = standard statistical distance "
    ,caption = "Standard Effect Size interpretation: <.01 = 'very small' | <.2 = 'Small` | <.5 = 'Medium'"
  )
g
g %>% quick_save("group-imbalance",width=11,height=7)

# ---- eval-convergence-1 -------------------------------------------
ps1 %>% plot("optimize") # balance as  function of gbm.iteration
# this command plots the size of the imbalance vs. the number of iterations.  For
# es.max.ATT and ks.max.ATT, this is the maximum of the absolute value of std.eff.sz
# (standardized bias).  For ex.mean.ATT and ks.mean.ATT, this is the mean of the
# absolute value of std.eff.sz (standardized bias)

ps1 %>% plot("boxplot") # distribution of propensity scores
# we have common support when the propensity scores approximately lineup between treatment and control
# we lack common support if propensity score is generally low for control and high for treatment

ps1 %>% plot("es") # standardized effect size of pre-treatment variables
# each dot is a variable and the higher the dot, the greater the dissimilarity between treatment and control
# es test assumes normal distribution to compute standardized effects

ps1 %>% plot("t") # t-test p-values for weighted pre-treatment variables
# we learned that es.max.ATT optimizes the worst match and so we should use it
# if we are afraid that the poorest match is the most important variable to balance on
# we should use one of the other three methods if all variables to balance on are equally important

ps1 %>% plot("ks") # kolmogorov-smirnov p-values for weighted pre-treatment variables
# ks test is a nonparametric test that compares cumulative distributions of two datasets
# Ho: both groups are sampled from populations with identical distributions
# Ha: null hypothesis violated:  different medians, variances, or distributions.

# ----- treatment-effect-1a ------------------------
compute_effect_on_binary <- function(
  ps_object # object from ps() model
  ,outcome_name = "return1_12m"
  ,treatment_name = "tx"
){
  # browser
  # ps_object <- ps1
  # outcome_name = "emp_at_survey"
  # treatment_name = "tx"
  
  d <-
    ps_object$data %>%
    mutate(
      w =  get.weights(ps_object,stop.method = "es.mean", estimand = "ATT" )
    ) %>%
    as_tibble()
  design.ps <- survey::svydesign(ids= ~1, weights = ~w, data = d)
  # d %>% group_by(tx) %>% count()
  
  outcome_n <- d %>%
    group_by(tx) %>%
    summarize(
      outcome_n = sum(!is.na(!!rlang::sym(outcome_name)))
    )
  
  sum(!is.na(d[outcome_name]))
  
  model_equation <- as.formula(paste0(outcome_name, "~", treatment_name))
  # model <- survey::svyglm(model_equation, design = design.ps, family = binomial(link="logit")) %>% suppressWarnings()
  model <- survey::svyglm(model_equation, design = design.ps, family = binomial(link="log")) %>% suppressWarnings()
  # summary(model)
  pattern_starts_with_explanatory <- paste0("^","tx", collapse = "|")
  d_estimates  <-
    model %>%
    broom::tidy(
      conf.int = TRUE
      # ,exp     = FALSE # when link logit, then use plogis() function to re-scales (see Gelman)
      ,exp     = TRUE # when link is "log", converts log-odds into odds-ratios (i.e. =exp(estimate))
    ) %>%
    mutate(
      conv_odds    = (estimate-1) # careful, this relies on broom::tidy(exp=TRUE)
      # otherwise: exp(estimate) - 1
      ,var_name    = stringr::str_extract(term, pattern_starts_with_explanatory)
      ,value_level = stringr::str_remove( term, pattern_starts_with_explanatory)
    )
  d_estimates
  
  d_net <-
    d %>%
    group_by(tx) %>%
    summarize(
      unweighted = mean(!!rlang::sym(outcome_name), na.rm=T)
      ,weighted = sum((!!rlang::sym(outcome_name))*w, na.rm =T) / sum(w, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      unw_diff = unweighted - lag(unweighted)
      ,wgt_diff = weighted - lag(weighted)
    )
  
  d_result <-
    dplyr::left_join(
      d_net %>%
        select(
          tx
          ,mean_raw = unweighted
          ,mean_weighted = weighted
          ,net_impact = wgt_diff
          
        )
      ,
      d_estimates %>%
        filter(term=="txTRUE") %>%
        mutate(tx=TRUE) %>%
        select(tx,conv_odds,p.value)
      ,by = "tx"
    )
  
  d_result <-
    d_result %>%
    left_join(outcome_n,by = "tx")%>%
    mutate(
      outcome = outcome_name
    ) %>%
    select(outcome,outcome_n,tx, everything())
  return(d_result)
}
compute_effect_on_continuous<- function(
  ps_object # object from ps() model
  ,outcome_name = "income_total_delta"
  ,treatment_name = "tx"
  ,covariate_names = NULL
  ,out_table = "nia" # nia,  est, fitted, plot
){
  # browser()
  # ps_object <- ps1
  # outcome_name = "income_total_delta"
  # treatment_name = "tx"
  
  d <-
    ps_object$data %>%
    mutate(
      w =  get.weights(ps_object,stop.method = "es.mean", estimand = "ATT" )
    ) %>%
    as_tibble()
  design.ps <- survey::svydesign(ids= ~1, weights = ~w, data = d)
  # d %>% group_by(tx) %>% count()
  
  outcome_n <- d %>%
    group_by(tx) %>%
    summarize(
      outcome_n = sum(!is.na(!!rlang::sym(outcome_name)))
    )
  
  sum(!is.na(d[outcome_name]))
  # browser()
  if(is.null(covariate_names)){
    
    model_equation <- as.formula(paste0(outcome_name, "~", treatment_name))
    pattern_starts_with_explanatory <- paste0("^","tx", collapse = "|")
    
  }else{
    
    model_equation <- as.formula(paste0(outcome_name, "~", treatment_name," + ",paste0(covariate_names, collapse = " + ")))
    pattern_dud <- paste0(c("tx",covariate_names),collapse="|")
    pattern_starts_with_explanatory <- paste0("^",pattern_dud, collapse = "|")
    
  }

  model <- survey::svyglm(model_equation, design = design.ps, family = "gaussian") %>% suppressWarnings()
  # summary(model)

  d_estimates  <-
    model %>%
    broom::tidy(
      conf.int = TRUE
      # ,exp     = TRUE # converts log-odds into odds-ratios (i.e. =exp(estimate))
    ) %>%
    mutate(
      # conv_odds    = (estimate-1) # careful, this relies on broom::tidy(exp=TRUE)
      # otherwise: exp(estimate) - 1
       var_name    = stringr::str_extract(term, pattern_starts_with_explanatory)
      ,value_level = stringr::str_remove( term, pattern_starts_with_explanatory)
    ) %>% 
    mutate(
      outcome = outcome_name
    ) %>%
    relocate(outcome)
  
  d_estimates
  
  d_net <-
    d %>%
    group_by(tx) %>%
    summarize(
      unweighted = mean(!!rlang::sym(outcome_name), na.rm=T)
      ,weighted = sum((!!rlang::sym(outcome_name))*w, na.rm =T) / sum(w, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      unw_diff = unweighted - lag(unweighted)
      ,wgt_diff = weighted - lag(weighted)
    )
  
  d_result <-
    dplyr::left_join(
      d_net %>%
        select(
          tx
          ,mean_raw = unweighted
          ,mean_weighted = weighted
          ,net_impact = wgt_diff
          
        )
      ,
      d_estimates %>%
        filter(term=="txTRUE") %>%
        mutate(tx=TRUE) %>%
        # select(tx,conv_odds,p.value)
        select(tx,estimate,p.value)
      ,by = "tx"
    )
  
  d_result <-
    d_result %>%
    left_join(outcome_n,by = "tx")%>%
    mutate(
      outcome = outcome_name
    ) %>%
    select(outcome,outcome_n,tx, everything())
  # browser()
  
  d_fitted <- 
    model %>% 
    broom::augment() %>% 
    group_by_at(all_of(c("tx",covariate_names))) %>%
    # group_by_at(all_of(c(covariate_names))) %>%
    mutate(
      person_count = n()
    ) %>% 
    ungroup() %>% 
    select(all_of(c("tx",covariate_names,".fitted","person_count"))) %>% 
    distinct() %>% 
    arrange(desc(person_count)) %>% 
    rename(
      predicted = `.fitted`
    ) %>% 
    mutate(
      outcome = outcome_name
      ,predicted = as.numeric(predicted)
    ) %>% relocate(outcome)
  
  model_plot <- model %>% GGally::ggcoef_model()
  
  if(out_table=="nia"){
    d_out <- d_result
  }
  
  if(out_table=="est"){
    d_out <- d_estimates
  }
  
  if(out_table == "fitted"){
    d_out <- d_fitted
  }
  
  if(out_table == "plot"){
    d_out <- model_plot
  }
  
  return(d_out)
}
# How to use
# ps1 %>% compute_effect_on_binary("return1_12m")
# ps1 %>% compute_effect_on_continuous("spell_duration")

# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "nia")
# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = NULL,out_table = "nia")
# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "est") %>% print_all()
# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "fitted")
# m <- ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "model")

# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "plot")
# g <- (m %>% GGally::ggcoef_model())
# g %>% quick_save("test-model-coef",h=12,w=10)


# ----- treatment-effect-1b ------------------------
# ps1 %>% compute_effect_on_binary("return1_12m")
# ps1 %>% compute_effect_on_continuous("spell_duration")
# ps1 %>% compute_effect_on_continuous("income_net_delta",out_table = "nia")
# ps1 %>% compute_effect_on_continuous("income_net_delta",out_table = "est")
# ps1 %>% compute_effect_on_continuous("income_net_delta", covariate_names = c("gender2","education4") )
# ps1 %>% compute_effect_on_continuous("income_net_delta", covariate_names = c("gender2","education4") ,out_table = "est")
# ps1 %>% compute_effect_on_continuous("income_net_delta", covariate_names = c("gender2","education4") ,out_table = "est")

ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "nia")
ps1 %>% compute_effect_on_continuous("earnings_total_delta",covariate_names = covariates,out_table = "nia")
ps1 %>% compute_effect_on_continuous("income_total_delta"  ,covariate_names = covariates,out_table = "nia")
ps1 %>% compute_effect_on_continuous("income_taxable_delta",covariate_names = covariates,out_table = "nia")



# ---- serialized-solution-1-ps -----------------------------------------------------
# 
# # compute PS weights for every intervention
# for(intervention_i in setdiff(intervention_used,c("assessment_ea","assessment_snd"))){
#   # identify the intervention in focus
#   # intervention_i <- "career_planning"
#   # intervention_i <- "english_as_second"
#   # create data set for analysis
#   d_nia <-
#     ds2 %>% filter(used_in_nia) %>%
#     select(-assessment_ni) %>% # because <1% of the sample have it
#     create_treatement_binary(tx_var = intervention_i)
#   # d_nia %>% glimpse()
#   # specify propensity score equation
#   dependent <- "tx"
# 
#   explanatory <- c(
#     covariates,
#     setdiff(intervention_used,intervention_i),
#     c(
#       "income_total_before"
#       ,"income_net_before"
#       ,"income_taxable_before"
#       ,"earnings_total_before"
#     )
#   )
#   # explanatory <- setdiff(explanatory, "program_class2")
#   (eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) ))
#   # compute propensity score weights
#   twang_object <-
#     twang::ps(
#       # propensity score model
#       formula            = eq_formula
#       ,estimand          = "ATT" # use ATT because that is consistent with ex-post net impact
#       ,data              = d_nia %>% as.data.frame()
#       # ,sampw             = d_nia$survey_weight # !!! optional
#       # gradient boosting
#       ,stop.method       = c("es.mean", "es.max", "ks.mean", "ks.max")
#       ,n.trees           = 1000
#       ,interaction.depth = 2
#       ,shrinkage         = 0.01
#       ,n.minobsinnode    = 10
#       # computational efficiency
#       ,n.keep            = 1
#       ,n.grid            = 25
#       ,ks.exact          = NULL
#       ,version           = "gmb" # gmb, xboost, legacy
#       # other
#       ,verbose           = FALSE
#     )
#   # save twang object for easier access later
# 
# 
#   path_save_twang_object <- paste0("./data-private/derived/twang/ps-",intervention_i,".rds")
#   twang_object %>% readr::write_rds(path_save_twang_object)
# }

# ---- serialized-solution-2-diagnostics ----------------------------------------

# read in PS objects
ps_path <- list.files("./data-private/derived/twang/",full.names = T)
ls_ps <- list()
for(i in seq_along(ps_path)){
  path_to_ps <- ps_path[i]
  name_of_ps <- basename(ps_path[i]) %>% str_remove("^ps-") %>% str_remove(".rds$")
  ls_ps[[name_of_ps]] <- readr::read_rds(path_to_ps)
}
names(ls_ps)

# print diagnositcs
for(intervention_i in names(ls_ps)){
  # intervention_i <- "workshop_noncp"
  twang_object <- ls_ps[[intervention_i]]
  
  # diagnositic graphs
  
  folder_for_diagnostic_graphs <- paste0("./analysis/nia-3-cra/twang-diagnostics/",intervention_i,"/")
  if (!fs::dir_exists(folder_for_diagnostic_graphs)) {fs::dir_create(folder_for_diagnostic_graphs)}
  
  png(paste0(folder_for_diagnostic_graphs,intervention_i,"-1-optimize.png"),w=900,h=300)
  twang_object %>% plot("optimize", main = "Number of iteration to find an optimal balance between groups") %>% print()
  dev.off()
  # balance as  function of gbm.iteration
  # this command plots the size of the imbalance vs. the number of iterations.  For
  # es.max.ATT and ks.max.ATT, this is the maximum of the absolute value of std.eff.sz
  # (standardized bias).  For ex.mean.ATT and ks.mean.ATT, this is the mean of the
  # absolute value of std.eff.sz (standardized bias)
  
  
  png(paste0(folder_for_diagnostic_graphs,intervention_i,"-2-boxplot.png"),w=900,h=300)
  twang_object %>% plot("boxplot", main = "Distribution of propensity scores (Low control & High Tx = No common support)" )%>% print()
  dev.off()
  # distribution of propensity scores
  # we have common support when the propensity scores approximately lineup between treatment and control
  # we lack common support if propensity score is generally low for control and high for treatment
  
  
  png(paste0(folder_for_diagnostic_graphs,intervention_i,"-3-es.png"),w=900,h=300)
  twang_object %>% plot("es", main = "Standard Effect Size (The higher the dot, the lower similarity between groups)") %>% print()
  dev.off()
  # standardized effect size of pre-treatment variables
  # each dot is a variable and the higher the dot, the greater the dissimilarity between treatment and control
  # es test assumes normal distribution to compute standardized effects
  
  png(paste0(folder_for_diagnostic_graphs,intervention_i,"-4-t.png"),w=900,h=300)
  twang_object %>% plot("t", main = "T-test p-values for weighted pre-treatment variables (higher = better)") %>% print()
  dev.off()
  # t-test p-values for weighted pre-treatment variables
  # we learned that es.max.ATT optimizes the worst match and so we should use it
  # if we are afraid that the poorest match is the most important variable to balance on
  # we should use one of the other three methods if all variables to balance on are equally important
  
  png(paste0(folder_for_diagnostic_graphs,intervention_i,"-5-ks.png"),w=900,h=300)
  twang_object %>% plot("ks", main = "Kolmogorov-Smirnov p-values for weighted pre-treatment variables (higher = better)") %>% print()
  dev.off()
  # kolmogorov-smirnov p-values for weighted pre-treatment variables
  # ks test is a nonparametric test that compares cumulative distributions of two datasets
  # Ho: both groups are sampled from populations with identical distributions
  # Ha: null hypothesis violated:  different medians, variances, or distributions.
  
  
  balance_table <-
    twang_object %>%
    bal.table() %>% # gets balance table
    purrr::map(tibble::rownames_to_column,"covariate") # add covariate names
  
  # dbt <-
  d_balance_table <-
    balance_table %>%
    dplyr::bind_rows(.id = "method") %>%
    janitor::clean_names() %>%
    as_tibble()%>%
    mutate(
      covariate = as_factor(covariate)
    )
  d_balance_table
  
  
  # group imbalance graph
  d <-
    d_balance_table %>%
    # filter(!method == "unw") %>%
    mutate(
      std_eff_sz = abs(std_eff_sz)
      ,metric = case_when(
        method %in% c("es.max.ATT","ks.max.ATT") ~ "max"
        ,method %in% c("es.mean.ATT","ks.mean.ATT") ~ "mean"
        ,TRUE ~ "unweighted"
      )
      ,distribution = case_when(
        method %in% c("es.mean.ATT","es.max.ATT") ~ "Standardized Effect Size"
        ,method %in% c("ks.mean.ATT","ks.max.ATT") ~ "Kolmogorov-Smirnov"
        ,TRUE ~ "unweighted"
      )
    ) %>%
    group_by(method) %>%
    mutate(
      es_mean            = std_eff_sz %>% abs() %>% mean(na.rm=T) # reported in summary table
      ,standard_distance = (std_eff_sz)^2 %>% mean(na.rm=T) %>% sqrt()  # custom metric
    ) %>%
    ungroup()
  # d %>% glimpse()
  
  g <-
    d %>%
    ggplot(aes(
      x  = std_eff_sz
      ,y = covariate
      ,color = metric
      ,fill = metric
      ,shape = metric
    )) +
    geom_vline(aes(xintercept= es_mean, color = metric),show.legend = F)+
    geom_vline(aes(xintercept= standard_distance, color = metric),show.legend = F,linetype="dotdash")+
    geom_vline(xintercept= .1, color = "black",linetype="dashed", alpha = .6)+
    geom_vline(xintercept= 0, color = "black",linetype="solid",alpha =.3)+
    facet_wrap(facets = c("distribution"), scales = "free_x")+
    scale_shape_manual(values = c("max"=21,"mean"=23,"unweighted"=22))+
    
    geom_point(alpha = .2, size = 3        , data = . %>% filter(metric!="mean"))+
    geom_point(alpha = .8, size = 3,fill=NA, data = . %>% filter(metric!="mean"))+
    
    geom_point(alpha = .2, size = 1        , data = . %>% filter(metric=="mean"),show.legend=F)+
    geom_point(alpha = .8, size = 1,fill=NA, data = . %>% filter(metric=="mean"),show.legend = F)+
    
    
    # scale_x_continuous(breaks = seq(0,.3,.05), minor_breaks = seq(0,.3,.01),labels = RemoveLeadingZero)+
    scale_x_continuous(breaks = seq(-1,.9,.05), minor_breaks = seq(-1,.9,.01),labels = RemoveLeadingZero)+
    scale_color_manual(values = c("max"="red",'mean'="black","unweighted"="blue"))+
    scale_fill_manual(values = c("max"="red",'mean'="black","unweighted"="blue"))+
    labs(
      x = "Group imbalance (lower = better)"
      ,fill  = "Metric\nof group\nimbalance"
      ,color =  "Metric\nof group\nimbalance"
      ,shape =  "Metric\nof group\nimbalance"
      ,title = paste0("Group balance across individual covariates. Intervention = ", intervention_labels[intervention_i])
      ,subtitle = "solid color = average effect size; dotdash color = standard statistical distance "
      ,caption = "Standard Effect Size interpretation: <.01 = 'very small' | <.2 = 'Small` | <.5 = 'Medium'"
    )
  # g
  ggsave(
    filename = paste0(intervention_i,"-0-group-imbalance.png")
    ,plot = g
    ,path = folder_for_diagnostic_graphs
    ,device = "png"
    ,width=12
    ,height=7
  )

}

# ---- serialized-solution-3-effects-nia ----------------------------------------
# compute effects 

# i
ls_effects <- list()
for(intervention_i in names(ls_ps)){
  
  twang_object <- ls_ps[[intervention_i]]
  ls_effect <- 
    list(
      # "return1_12m"            = twang_object %>% compute_effect_on_binary(    "return1_12m")
      # ,  "spell_duration"      = twang_object %>% compute_effect_on_continuous("spell_duration")
       "income_net_delta"     = twang_object %>% compute_effect_on_continuous( "income_net_delta"    ,covariate_names = c(covariates,"income_net_before")    ,out_table = "nia")
      , "earnings_total_delta" = twang_object %>% compute_effect_on_continuous("earnings_total_delta",covariate_names = c(covariates,"earnings_total_before"),out_table = "nia")
      , "income_total_delta"   = twang_object %>% compute_effect_on_continuous("income_total_delta"  ,covariate_names = c(covariates,"income_total_before")  ,out_table = "nia")
      , "income_taxable_delta" = twang_object %>% compute_effect_on_continuous("income_taxable_delta",covariate_names = c(covariates,"income_taxable_before"),out_table = "nia")
    ) 
  ds_effects <- ls_effect %>%  bind_rows() 
  ls_effects[[intervention_i]] <- ds_effects
}

ds_effect <- 
  ls_effects %>% 
  bind_rows(.id = "intervention")

ds_effect %>% 
  readr::write_csv("./analysis/nia-3-cra/model-solution/nia-3-cra-effects.csv")

# ---- serialized-solution-3-effects-est ----------------------------------------
# full model

ls_effects_est_all <- list()
# ls_effect_plots <- list
for(intervention_i in names(ls_ps)){
# for(intervention_i in "workshop_noncp"){
  # intervention_i <- "workshop_noncp"
  twang_object <- ls_ps[[intervention_i]]
  
  ls_effects_est <- list()
  ls_effects_est <- 
    list(
      # "return1_12m"            = twang_object %>% compute_effect_on_binary(    "return1_12m")
      # ,  "spell_duration"      = twang_object %>% compute_effect_on_continuous("spell_duration")
      "income_net_delta"     = twang_object %>% compute_effect_on_continuous("income_net_delta"     ,covariate_names =  c(covariates,"income_net_before")    ,out_table = "est")
      , "earnings_total_delta" = twang_object %>% compute_effect_on_continuous("earnings_total_delta",covariate_names = c(covariates,"earnings_total_before"),out_table = "est")
      , "income_total_delta"   = twang_object %>% compute_effect_on_continuous("income_total_delta"  ,covariate_names = c(covariates,"income_total_before")  ,out_table = "est")
      , "income_taxable_delta" = twang_object %>% compute_effect_on_continuous("income_taxable_delta",covariate_names = c(covariates,"income_taxable_before"),out_table = "est")
    ) 
  ds_effects <- ls_effects_est %>%  bind_rows() %>% mutate(intervention = intervention_i) %>% relocate(intervention)
  ls_effects_est_all[[intervention_i]] <- ds_effects
}

ds_effect_est <- 
  ls_effects_est_all %>% 
  bind_rows(.id = "intervention")

ds_effect_est %>% 
  readr::write_csv("./analysis/nia-3-cra/model-solution/nia-3-cra-effects-est.csv")

# ---- serialized-solution-3-effects-plots ----------------------------------------

ls_object <- list()
# ls_effect_plots <- list
for(intervention_i in names(ls_ps)){
  # for(intervention_i in "workshop_noncp"){
  # intervention_i <- "workshop_noncp"
  twang_object <- ls_ps[[intervention_i]]
  
  for(outcome_i in outcomes_cra){
    # outcome_i <- outcomes_cra[1]
    outcome_control_i <- str_replace(outcome_i,"_delta","_before")
    
    ((twang_object %>% 
      compute_effect_on_continuous(
        outcome_name = outcome_i     
        ,covariate_names =  c(covariates,outcome_control_i),
        out_table = "plot"
      ))+labs(
        title = paste0("Intervention = ", intervention_i, " | Outcome = ", outcome_i)
      )) %>% 
      quick_save(
        name = paste0("../model-solution/",intervention_i,"-",outcome_i)
        ,h=12,w=10
      )
  }
  
}
  

# ---- serialized-solution-3-effects-fitted ----------------------------------------
# fitted values
ls_object <- list()
for(intervention_i in names(ls_ps) ){
# for(intervention_i in "career_planning" ){
  # for(intervention_i in "workshop_noncp"){
  # intervention_i <- "workshop_noncp"
  ls_effects_fitted <- list()
  twang_object <- ls_ps[[intervention_i]]
  ls_effects_fitted <- 
    list(
      "income_net_delta"       = twang_object %>% compute_effect_on_continuous("income_net_delta"    ,covariate_names = c(covariates,"income_net_before")    ,out_table = "fitted")
      , "earnings_total_delta" = twang_object %>% compute_effect_on_continuous("earnings_total_delta",covariate_names = c(covariates,"earnings_total_before"),out_table = "fitted")
      , "income_total_delta"   = twang_object %>% compute_effect_on_continuous("income_total_delta"  ,covariate_names = c(covariates,"income_total_before")  ,out_table = "fitted")
      , "income_taxable_delta" = twang_object %>% compute_effect_on_continuous("income_taxable_delta",covariate_names = c(covariates,"income_taxable_before"),out_table = "fitted")
    ) 
  ds_effects <- ls_effects_fitted %>%  bind_rows() %>% mutate(intervention = intervention_i) %>% relocate(intervention)
  ls_object[[intervention_i]] <- ds_effects
}

ds_effect_fitted <- 
  ls_effects_fitted %>% 
  bind_rows(.id = "intervention")

ds_effect_fitted %>% 
  readr::write_rds("./analysis/nia-3-cra/model-solution/nia-3-cra-fitted.rds", compress = "xz")


# ---- serialized-solution-3-effects-model ----------------------------------------
# model objects - takes too long, seek alternatives
# 
# ls_object <- list()
# for(intervention_i in names(ls_ps)){
#   # for(intervention_i in "workshop_noncp"){
#   # intervention_i <- "workshop_noncp"
#   ls_effects_model <- list()
#   twang_object <- ls_ps[[intervention_i]]
#   ls_effects_model <- 
#     list(
#       "income_net_delta"       = twang_object %>% compute_effect_on_continuous("income_net_delta"    ,covariate_names = c(covariates,"income_net_before")    ,out_table = "model")
#       , "earnings_total_delta" = twang_object %>% compute_effect_on_continuous("earnings_total_delta",covariate_names = c(covariates,"earnings_total_before"),out_table = "model")
#       , "income_total_delta"   = twang_object %>% compute_effect_on_continuous("income_total_delta"  ,covariate_names = c(covariates,"income_total_before")  ,out_table = "model")
#       , "income_taxable_delta" = twang_object %>% compute_effect_on_continuous("income_taxable_delta",covariate_names = c(covariates,"income_taxable_before"),out_table = "model")
#     ) 
#   ls_object[[intervention_i]] <- ls_effects_model
# }
# 
# ls_object %>% 
#   readr::write_rds("./data-private/derived/nia-3/nia-3-cra-models.rds", compress = "xz")

# ---- visualize-models  ------------------------------
ls_graph <- list()
for(intervention_i in name())

ls_object

# g <- (m %>% GGally::ggcoef_model())
# g %>% quick_save("test-model-coef",h=12,w=10)


# ---- save-to-disk ------------------------------------------------------------

# ---- publish ------------------------------------------------------------
path <- "./analysis/nia-3-cra/nia-3.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # "word_document"
    # "pdf_document"
  ),
  clean=TRUE
)
