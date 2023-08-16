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
library(TabularManifest) # EDA  devtools::install_github(repo="Melinae/TabularManifest")
library(broom) # model inspection
library(ggpubr) # graphing
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
# filtered_data %>% tableone::CreateTableOne(data=., strata = "wave") # takes long
filtered_data %>% explore::describe_all()

# describe items used to derive affected_index
ds_var <- 
  tibble(var_name = names(war_var_labels), var_value = war_var_labels) %>% 
  print()

# ---- tweak-data-0 --------------------------------------------------------------
ds0 <- 
  filtered_data %>% # toy data
  # real_data %>% # full sample data, to be added during evaluation
  # integer indicator for the wave to ease some graphing
  mutate(
    # wave1 - Dec 2021, wave2 - Sep 2022. Fieldwork duration ~2 weeks
    wave = wave %>% fct_recode("before"="wave1", "after"="wave2")
    ,waveL = case_when(wave =="before" ~ 0L,TRUE ~ 1L) %>% as.integer()  # for geom_smooth to work
  ) %>% 
  select(
    key    # respondent identifier
    ,wave  # as a  factor
    ,waveL # as an integer # for geom_smooth
    
    # variables measured at both waves
    # outcome, religiosity 
    ,religious = c15 # how religious are you ?         0-10 # filtered_data %>% count(c15)
    ,church    = c16 # how often do you attend church? 1-7  # filtered_data %>% count(c16)
    ,pray      = c17 # how often do you pray?          1-7  # filtered_data %>% count(c17)
    ,religiosity # mean of standardized(M=0,SD=1) items c16, c16, c17 treated as ratio scale
    ,km_to_war = distance_to_ru_threat_blr_excluded
    
    # intervention variables measured at wave 2 ONLY (because happened after full-scale invasion)
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
ds0 %>% glimpse()

# rm(filtered_data, war_var_labels)
# ---- tweak-data-1 ------------------------------------------------------------

# compute new variables 
ds1 <-
  ds0 %>%
  mutate(
    km_to_war_bin = cut(km_to_war, breaks = c(10,seq(50,750,50)))
    ,church_weekly = case_when(
      church  %in% c(1,2,3)   ~ 1L
      ,church %in% c(4,5,6,7) ~ 0L
      ,TRUE ~ NA
    )
    ,church_monthly = case_when(
      church  %in% c(1,2,3,4)   ~ 1L
      ,church %in% c(5,6,7) ~ 0L
      ,TRUE ~ NA
    )
    ,pray_daily = case_when(
      pray %in% c(1) ~ 1L
      ,pray %in% c(2,3,4,5,6,7) ~ 0L
      ,TRUE ~ NA
    )
  )
ds1 %>% glimpse()
# ---- temp -------------
# 


# ---- table-1 -----------------------------------------------------------------
# Our outcome is self-reported religiosity of respondents (items c15:c17)
# How does it change since the onset of the full-scale invasion in Feb 2022?
# measures of religiosity
d1 <- 
  ds1 %>% 
  select(
    wave,waveL,
    religious, church, pray,                                # raw
    church_weekly, church_monthly, pray_daily, religiosity  # derived
    ) %>% 
  mutate(across(c("religious","church","pray"), .fns= ~as.integer(.))) 

d1 %>% filter(wave=="before") %>% explore::describe_all()
d1 %>% filter(wave=="after") %>% explore::describe_all()
d1 %>% tableone::CreateTableOne(data=., strata = "wave",testNonNormal = TRUE)
# examining the raw group differences we observe (cross-sectional view) 
# 1) an increase in self-reported religiosity from 4.7 to 5.68 (on a 11-point scale 0-10)
# means of `church` and `pray` summarize items on ordinal scale, 
# 2) an increase in composite index of religiosity from -.21 to .04 (but its scale is not interpretable)
# 3) percent who attends church at least weekly has doubled (.08 to .16)
# 4) percent who prays daily increased by 2/3 ( from 29% to %48)
# i.o. if before the war only every third Ukrainian prayed daily,
# after the full-scale invasion every second Ukrainian prays daily
# 5) the mean of religiosity index increased by .25 points (from -.21 to .04)
# however, the scale of this metric is not readily interpretable

# Measuring the degree to which responded were affected by war (items 2.2 and 2.3)
# measures of intervention
d2 <- 
  ds1 %>% 
  select(
    wave,
    loss_dummy3 # lost someone in war
    , displaced # moved since invasion
    , affected_index # sum of loss, displaced, and other affect items: 2.2.1-8
    , affected_index_std # M=0;SD=1
    , affected_index_dummy # affected_index_std > 0
  ) %>% 
  mutate(
    # sum_loss_or_displaced = loss_dummy3  + displaced
    sum_loss_or_displaced =  rowSums(across(.cols=c("loss_dummy3", "displaced")))
    
    # ,loss_or_displaced = loss_dummy3==1L | displaced==1L
    ,loss_or_displaced = case_when(
      sum_loss_or_displaced %in% c(1,2) ~ TRUE,
      sum_loss_or_displaced %in% c(0) ~ FALSE,
      TRUE ~ NA
    )
  )
d2 %>% filter(wave=="before") %>% explore::describe_all()
d2 %>% filter(wave=="after") %>% explore::describe_all()
d2 %>% tableone::CreateTableOne(data=., strata = "wave",testNonNormal = TRUE,
                               factorVars = c("sum_loss_or_displaced"))

# 1) 40% experienced loss of someone they knew personally (some NAs) / almost every 2nd person
# 2) 16% experienced change of residence since invasion / ~ every 8th respondent
# 3) 46% experienced either loss or displacement / ~ every 2nd respondent
# 4)  6% experienced both loss and displacement
# 3) average affected index is 3.29 (average sum of items 2.2 and 2.3)
# this index is not easily interpretable, but could be thought of as an average
# number of "loss" items a respondent endorsed in sections 2.3 and 2.2 
# std and std_dummy are questionable, disregarded in this exercise
ds1 %>% filter(wave=="after") %>% TabularManifest::histogram_continuous("affected_index")
ds1 %>% filter(wave=="after") %>% TabularManifest::histogram_continuous("affected_index_std")


# ---- graph-1 -----------------------------------------------------------------
# some views of the distribution of the modeled outcome measure
# we know that average religiosity shifted up
ds1 %>% select(wave, religiosity) %>% tableone::CreateTableOne(data=.,strata="wave")
# but we need to understand this shift in context of changing variability between waves
ds1 %>%
  group_by(wave) %>% 
  mutate(
    mean=mean(religiosity)
    ,median = quantile(religiosity,.50)
    ) %>% 
  ungroup %>% 
  ggplot(aes(x=religiosity))+
  geom_histogram(alpha = .4)+
  geom_vline(aes(xintercept = mean))+
  geom_vline(aes(xintercept = median),color = "blue", linetype="dashed")+
  geom_text(aes(label=round(mean,2), x = mean),y=Inf,vjust=1.2, hjust=1)+
  geom_text(aes(label=round(median,2), x = median),y=Inf,vjust=1.2, hjust=-.3,color="blue")+
  facet_grid(wave~.)+
  labs(x="Standardized index of religiosity")
# alternative view, with a statistical test
ds1 %>% 
  ggpubr::ggboxplot(x = "wave", y = "religiosity",
            color = "wave",
            add = "jitter")+
  stat_compare_means()
# alternative view, dedicated statistical procedure of the paired T-test 
rstatix::t_test(ds1,religiosity~wave,ref.group = "before",paired = TRUE)
result_of_t.test <- 
  t.test(
  ds1 %>% filter(wave=="before") %>% pull(religiosity)
  ,ds1 %>% filter(wave=="after") %>% pull(religiosity)
  ,paired = TRUE
) %>% print()

# But more specifically, we must evaluate individual change
g1 <- 
  ds1 %>% 
  ggplot(aes(x=waveL, y = religiosity))+ # note re-centering to align with model coefficients
  geom_point(alpha = .2)+
  geom_line(aes(group = key),alpha = .2)+
  # geom_smooth(method = "lm")+
  scale_x_continuous(breaks = c(0,1))
g1

# We can summarize this individual change by finding the "average" trajectory between two waves
line_equation <- 
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        ,aes(label = paste(after_stat(eq.label)))
                        ,parse = TRUE
                        ,label.x = 0.1
                        ,label.y = 1.5) 
g2 <-
  g1 +
  geom_smooth(method = "lm")+
  line_equation
g2  
# notice that it estimates the same group means
d1 %>% tableone::CreateTableOne(data=., strata = "wave",testNonNormal = TRUE)
# and the estimated group difference from the paired t.test() procedure
result_of_t.test
# and in the context of individual change

# compute raw equation to test hypotheses non-rigorously (no stat test of sign or adj of variance)

# Show the observed, person-level change in religiosity index
g1 <- 
  ds1 %>% 
  ggplot(aes(x=waveL-1, y = religiosity))+ # note re-centering to align with model coefficients
  geom_point(alpha = .2)+
  geom_line(aes(group = key),alpha = .2)+
  geom_smooth(method = "lm")+
  add_equation +
  scale_x_continuous(breaks = c(1,2))
g1
# we would like to evaluate the claim that: 
# H1: Religiosity increased since the full-scale invasion
# Which could be evaluated with the following model
m1 <- glm( religiosity ~ wave, data = ds1)
m1 %>% glance()
m1 %>% tidy()
# notice that 

m1 %>% GGally::ggcoef_model()

# ---- graph-2 -----------------------------------------------------------------

# ---- model-1 -----------------------------------------------------------------



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

# Scrap Book all the way down
# ----- scrapbook-1 -----------------------------------------------------------
# g1 <- 
#   ds1 %>% 
#   ggplot(aes(
#     x = c16
#     ,y = km_to_war_bin
#     ,color = wave
#   ))+
#   geom_point(
#     shape = 21, alpha = .4
#     # ,position="jitter"
#   )+
#   labs()
# g1