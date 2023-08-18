#' ---
#' title: "God in War: Changing religiosity since the full-scale invasion"
#' author: "TBD"
#' date: "last Updated: `r Sys.Date()`"
#' ---
#+ echo=F
# rmarkdown::render(input = "./analysis/ua-war-religion/god-in-war.R") # run to knit, don't uncomment
#+ echo=F ----------------------------------------------------------------------
library(knitr)
# align the root with the project working directory
knitr::opts_knit$set(root.dir='../../')  #Don't combine this call with any, align with project root
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
#+ echo=F ----------------------------------------------------------------------
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\014") # Clear the console
# verify root location
cat("Working directory: ", getwd()) # Must be set to Project Directory
# Project Directory should be the root by default unless overwritten
# rmarkdown::render(input = "./analysis/ua-war-religion/god-in-war.R") # run to knit, don't uncomment
#+ load-packages -----------------------------------------------------------
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

#+ load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
base::source("./scripts/modeling/model-basic.R") # project-level

#+ declare-globals ---------------------------------------------------------
# default location for prints by quick_save():
prints_folder <- paste0("./analysis/ua-war-religion/prints/")
if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))}

#+ load-data ---------------------------------------------------------------
load("./analysis/ua-war-religion/materials/toy-data.RData")

#+ inspect-data ------------------------------------------------------------
filtered_data %>% glimpse()
# filtered_data %>% tableone::CreateTableOne(data=., strata = "wave") # takes long
filtered_data %>% explore::describe_all()

# describe items used to derive affected_index
ds_var <- 
  tibble(var_name = names(war_var_labels), var_value = war_var_labels) %>% 
  print()

#+ tweak-data-0 --------------------------------------------------------------
ds0 <- 
  filtered_data %>% # toy data
  # real_data %>% # full sample data, to be added during evaluation
  # integer indicator for the wave to ease some graphing
  mutate(
    # wave1 - Dec 2021, wave2 - Sep 2022. Fieldwork duration ~2 weeks
    wave = wave %>% fct_recode("Before"="wave1", "After"="wave2")
    ,waveL = case_when(wave =="Before" ~ 0L,TRUE ~ 1L) %>% as.integer()  # for geom_smooth to work
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
       # loss_dummy3, # coarsened  q2.3 
       # displaced,   # coarsened  q1.3
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
#+ tweak-data-1 ------------------------------------------------------------

# compute new variables 
ds1 <-
  ds0 %>%
  arrange(key,waveL) %>% 
  mutate(
    km_to_war_bin = cut(km_to_war, breaks = c(10,seq(50,750,50)))
    ,km100_to_war = km_to_war/100 # re-scale for convenient interpretation
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
    ,sum_loss_or_displaced =  rowSums(across(.cols=c("loss_dummy3", "displaced")))
    ,loss_or_displaced = case_when(
      sum_loss_or_displaced %in% c(1,2) ~ TRUE,
      sum_loss_or_displaced %in% c(0) ~ FALSE,
      TRUE ~ NA
    )
    ,loss_and_displaced = case_when(
      sum_loss_or_displaced %in% c(2) ~ TRUE,
      sum_loss_or_displaced %in% c(0,1) ~ FALSE,
      TRUE ~ NA
    )
  ) %>% 
  group_by(key) %>% 
  arrange(key,waveL) %>% 
  mutate(
    relig_change =  religiosity - dplyr::lag(religiosity) 
    ,relig_increase = relig_change > 0
  ) %>% 
  # select(key, wave, religiosity, relig_change, relig_increase) %>%
  arrange(key,desc(waveL)) %>%
  tidyr::fill(relig_change, relig_increase) %>% # makes person-level
  arrange(key,waveL) %>%
  ungroup() 
ds1 %>% glimpse()
ds1 %>% select(key, wave, religiosity, relig_change, relig_increase) %>% head(6)

#+ outcome-1 -----------------------------------------------------------------
# Religiosity - Outcome variable
# Our outcome is self-reported religiosity of respondents (items c15:c17)
# How does it change since the onset of the full-scale invasion in Feb 2022?
d1 <- 
  ds1 %>% 
  select(
    wave,waveL,
    religious, church, pray,      # c15, c16, c17                              
    church_weekly
    ,church_monthly
    ,pray_daily
    ,religiosity   # additive index, standardized
    ,relig_change   # change in standardized religiosity index # person-level
    ,relig_increase # binary                                   # person-level
    ) %>% 
  mutate(across(c("religious","church","pray"), .fns= ~as.integer(.))) 

d1 %>% filter(wave=="Before") %>% explore::describe_all()
d1 %>% filter(wave=="After") %>% explore::describe_all()
d1 %>% tableone::CreateTableOne(data=., strata = "wave",testNonNormal = TRUE)
# examining the raw group differences we observe (cross-sectional view) 
# 1) an increase in self-reported religiosity from 4.7 to 5.68 (on a 11-point scale 0-10)
# means of `church` and `pray` summarize items are on ordinal scale, so not really kosher
# 2) an increase in composite index of religiosity from -.21 to .04 (but its scale is not interpretable)
# 3) percent who attends church at least weekly has doubled (.08 to .16)
# 4) percent who prays daily increased by 2/3 ( from 29% to %48)
# i.o. if before the war only every third Ukrainian prayed daily,
# after the full-scale invasion every second Ukrainian prays daily
# 5) the mean of religiosity index increased by .25 points (from -.21 to .04)
# however, the scale of this metric is not readily interpretable

# All but seven people are two nonmissing values of church_weekly
# ds1 |> 
#   dplyr::group_by(key) |> 
#   dplyr::summarize(
#     point_count = sum(!is.na(church_weekly))
#   ) |> 
#   dplyr::count(point_count)


#+ intervention-1 -----------------------------------------------------------------
# Affect of War - Intervention variable
# Measuring the degree to which responded were affected by war (items 2.2 and 2.3)
ds_var
d2 <- 
  ds1 %>% 
  select(
    wave,
    loss_dummy3 # lost someone in war, composite of 
    , displaced # moved since invasion
    , affected_index # sum of loss, displaced, and other affect items: 2.2.1-8
    , affected_index_std # M=0;SD=1
    , affected_index_dummy # affected_index_std > 0
    ,sum_loss_or_displaced
    ,loss_or_displaced
    ,loss_and_displaced
  ) 
d2 %>% filter(wave=="Before") %>% explore::describe_all()
d2 %>% filter(wave=="After") %>% explore::describe_all()
d2 %>% tableone::CreateTableOne(data=., strata = "wave",testNonNormal = TRUE,
                               factorVars = c("sum_loss_or_displaced"))

# 1) 36 % experienced loss of someone they knew personally (some NAs) / every third
# 2) 16% experienced change of residence since invasion / ~ every 8th respondent
# 3) 46% experienced either loss or displacement / ~ every 2nd respondent
# 4)  6% experienced both loss and displacement
# 3) average affected index is 3.29 (average sum of items 2.2 and 2.3)
# this index is not easily interpretable, but could be thought of as an average
# number of "loss" items a respondent endorsed in sections 2.3 and 2.2 
# std and std_dummy are questionable, disregarded in this exercise
ds1 %>% filter(wave=="After") %>% TabularManifest::histogram_continuous("affected_index") # only a few distinct values, as artifact of addition
ds1 %>% filter(wave=="After") %>% TabularManifest::histogram_continuous("affected_index_std") # same shape, different center and units

#+ graph-1-cross-sectional -----------------------------------------------------------------
# some views of the distribution of the modeled outcome measure
# we know that average religiosity shifted up
ds1 %>% select(wave, religiosity) %>% tableone::CreateTableOne(data=.,strata="wave")
# but we need to understand this shift in context of changing variability between waves
g1a <-
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
g1a
# alternative view, with a statistical test
g1b <- 
  ds1 %>% 
  ggpubr::ggboxplot(x = "wave", y = "religiosity",
            color = "wave",
            add = "jitter")+
  stat_compare_means()
g1b
# alternative view, dedicated statistical procedure of the paired T-test 
results_of_t_test <- 
  rstatix::t_test(ds1,religiosity~wave,ref.group = "After",paired = TRUE)
result_of_t.test <- 
  t.test(
  ds1 %>% filter(wave=="After") %>% pull(religiosity)
  ,ds1 %>% filter(wave=="Before") %>% pull(religiosity)
  ,paired = TRUE
) %>% print()
# However, this difference (.25) measures the cross-sectional change
# And we need to evaluate individual change as well

#+ graph-2-within-person-over-time -----------------------------------------
# let's plot the observed within-person change
g2a <- 
  ds1 %>% 
  ggplot(aes(x=waveL, y = religiosity))+ # note re-centering to align with model coefficients
  geom_point(alpha = .2)+
  geom_line(aes(group = key),alpha = .15)+
  # geom_smooth(method = "lm")+
  scale_x_continuous(breaks = c(0,1))+
  labs(
    title = "Individual change in standardized religiosity index "
    ,subtitle = "Religiosity Index = (Church attendance, Prayer, Self-reported Religiosity) | M=0,SD=1"
  )
g2a
# One way to summarize this individual change is by finding the "average" trajectory between two waves:
line_equation <- 
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        # ,aes(label = paste0(c(after_stat(rr.label),after_stat(eq.label)))) # can't get the same behavior
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse = TRUE
                        ,label.x = 0.1
                        ,label.y = 1.5,color = "blue",vjust=1.2) 
g2b <-
  g2a +
  geom_smooth(method = "lm",color="blue")+
  line_equation
g2b  
# notice that it estimates the same group means we see in manifest group means:
d1 %>% tableone::CreateTableOne(data=., strata = "wave",testNonNormal = TRUE)
# and the estimated group difference from the paired t.test() procedure
result_of_t.test


#+ model-1 -----------------------------------------------------------------
# Let's do a formal test of the hypothesis
# H1: Religiosity changed since the full-scale invasion
# it can be evaluated with the following model in glm()
# Baseline model, wave only
m1 <- glm( religiosity ~ wave, data = ds1)
m1 %>% tidy()
m1 %>% GGally::ggcoef()
emm1 <- m1 %>% emmeans::emmeans(~wave) %>% print()
emm1 %>% plot()
m1 %>% GGally::ggcoef_model()

# For learning purposes, let's compare model prediction to previous views of data
gm1 <- 
  m1 %>% 
  augment() %>% # add fitted values
  bind_cols(ds1 %>% select(-c("wave","religiosity"))) %>% # add original data
  # ggplot(aes(x=waveL, y = religiosity))+ # note re-centering to align with model coefficients
  # ggplot(aes(x=waveL, y = .fitted))+ # note re-centering to align with model coefficients
  ggplot(aes(x=waveL))+ 
  # Observed
  geom_point(aes(y=religiosity),alpha = .2)+
  geom_line(aes(y=religiosity, group = key),alpha = .2)+
  geom_smooth(aes(y=religiosity), method = "lm")+
  # Reconstructed from model prediction
  geom_line(aes(y=.fitted), color = "red", size=4, alpha = .2)+ # model predicted values 
  scale_x_continuous(breaks = c(0,1))+
  labs(
    title = "Individual change in standardized religiosity index "
    ,subtitle = "Religiosity Index = (Church attendance, Prayer, Self-reported Religiosity) | M=0,SD=1"
  )
gm1
# People became more religious, by (.25 on religiosity index) much  

#+ examine-covariate-1-km_to_war -------
# History and demography of Ukraine shape the relationship between geography and religiosity
# The closer to the West/farther from Russia, the more religious people become
g3a <-
  ds1 %>%
  ggplot(aes(x=km100_to_war, y=religiosity))+
  geom_point(shape=21)+
  geom_smooth(aes(color=wave),method = "loess")+
  geom_smooth(method = "lm", linetype = "dashed")+
  line_equation
g3a
# by an average of about .12 points of the religiosity index for every 100k away from russia
# Very similar relationship is observed for both waves
g3a +  facet_grid(.~wave)
# let's formally test the hypothesis
# H1: Distance to War/Russia explains variability in religiosity (index) 
model_rel_geo <- glm(religiosity ~ km100_to_war, data = ds1)
model_rel_geo %>% tidy()
model_rel_geo %>% get_rsquared()
# Yes, about 10% of it 
emm_rel_geo <- model_rel_geo %>%  
  emmeans::emmeans(
     specs = c("km100_to_war") 
    ,at   = list(km100_to_war = c(.1,1,2,3,4,5,6,7)) # custom points to evaluate
  )
emm_rel_geo
emm_rel_geo %>% plot()

#+ model-2 -----------------------------------------------------------------

# To formally test the hypothesis:
# H1: Distance to War/Russia affects the CHANGE in religiosity (index) since the full-scale invasion
m2 <- glm(religiosity ~ km100_to_war*wave, data = ds1)
m2 %>% tidy()
# The effect of interaction is not detected, in other words
# Increase in religiosity is universal across geography, in other words
# It's not the region/distance to war that can explain the change in religiosity since the full-scale invasion
emm2 <- m2 %>%  
  emmeans::emmeans(
    # specs = c("wave","km100_to_war")
    specs = c("km100_to_war","wave") # different perspective
    ,at   = list(km100_to_war = c(.1,1,2,3,4,5,6,7)) # custom points to evaluate
  )
emm2
emm2 %>% plot()

#+ graph -----
# let's examine this effect visually within different bins of the covariate
# to ensure we are not dealing with a type of simpson's paradox
g4 <- 
  ds1 %>% 
  ggplot(aes(x=waveL, y = religiosity))+ # note re-centering to align with model coefficients
  geom_point(alpha = .2)+
  geom_line(aes(group = key),alpha = .2)+
  # geom_smooth(method = "lm")+
  scale_x_continuous(breaks = c(0,1))+
  labs(
    title = "Individual change in standardized religiosity index by distance to occupied/Russia"
    ,subtitle = "Religiosity Index = (Church attendance, Prayer, Self-reported Religiosity) | M=0,SD=1"
  )+
  facet_wrap(facets = "km_to_war_bin", scales = "fixed") +
  geom_smooth(method = "lm")+
  line_equation
g4  

#+ model-3 -------------------------

# DO peple who live farther wave from Russia have different reaction (in reliogisity)
# to the affect of the war? 
dm <- ds1 %>%
  # tidyr::drop_na(loss_dummy3) %>% 
  select(key, waveL, loss_dummy3,religiosity ) %>% mutate(did = waveL*loss_dummy3)
# due to design. tje dif-in-dif dummy (interaction between time and treated) is
# perfectly confounded with the treated indicator
m <- glm( religiosity ~  waveL + did, data = dm );tidy(m)
m <- glm( religiosity ~  waveL + loss_dummy3, data = dm );tidy(m)
rm(dm,m)


ds1 |> 
  dplyr::count(wave, loss_dummy3)

ds1 |> 
  dplyr::group_by(key) |> 
  dplyr::mutate(
    ever_loss = max(loss_dummy3)
  ) |>
  dplyr::ungroup() |> 
  dplyr::group_by(wave, ever_loss) |> 
  dplyr::summarize(
    rel_mean = mean(religiosity)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::select(wave, ever_loss, rel_mean)

m3a <- glm( religiosity ~ wave + loss_dummy3 + km100_to_war + loss_dummy3*km100_to_war, data = ds1 )
# m3a <- glm( religiosity ~ wave + loss_dummy3 + km100_to_war                           , data = ds1 )
m3a %>% tidy() 
m3a %>% GGally::ggcoef_model()
emm3a <- m3a %>% emmeans::emmeans(
  specs = c("loss_dummy3", "km100_to_war") 
  ,at   = list(km100_to_war = c(.1,1,2,3,4,5,6,7)) # custom points to evaluate
) %>% print()
emm3a %>% plot()

hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)
# eq_emmeans <- "~ wave | km100_to_war"
# eq_emmeans <- "~ wave | loss_dummy3"
eq_emmeans <- "~ wave * loss_dummy3 | km100_to_war"

e <-
  emmeans::emmeans(
    object = m3a, 
    as.formula(eq_emmeans), 
    data = ds1,
    type = "response",
    at   = list(km100_to_war = c(.1,1,7))
    # at   = list(wave = c("Before", "After"), km100_to_war = c(.1,1,7))#, loss_dummy3 = c("0", "1"))
  )  
print(e)

d_predict <-
  seq_len(nrow(e@linfct)) |>
  purrr::map_dfr(function(i) as.data.frame(e[i])) |>
  dplyr::mutate(
    outcome = "religiosity",
    loss_dummy3 = factor(loss_dummy3),
  ) |>
  dplyr::select(
    outcome,
    wave,
    loss_dummy3,
    km100_to_war,
    y_hat       = !!rlang::ensym(hat_name),
    se          = SE,
    ci_lower    = lower.CL,  #asymp.UCL
    ci_upper    = upper.CL #asymp.UCL
  ) 

d_predict |> 
  ggplot(aes(x = wave, y = y_hat, group = loss_dummy3, color = loss_dummy3, fill = loss_dummy3)) +
  geom_point() +
  geom_line() + 
  facet_wrap("km100_to_war") +
  theme_minimal()
#       outcome   wave loss_dummy3        y_hat         se   ci_lower    ci_upper
# 1 religiosity Before           0 -0.203914108 0.05606794 -0.3141630 -0.09366524
# 2 religiosity Before           1 -0.109804913 0.13634093 -0.3778981  0.15828830
# 3 religiosity  After           0  0.001778506 0.07425803 -0.1442384  0.14779536
# 4 religiosity  After           1  0.095887701 0.09966701 -0.1000920  0.29186736
  
#   predict_cells_1 <- function(d, m, hat_name, outcome_name, eq_emmeans) {
#   # checkmate::assert_character(eq_emmeans, pattern = "^~.+", len = 1, any.missing = FALSE)
#   e <-
#     emmeans::emmeans(
#       m, 
#       as.formula(eq_emmeans),
#       data = d,
#       type = "response",
#       at   = list(tx = levels_tx_1, asthma = levels_asthma)
#     )  
#   print(e)
# 
#   d_predict <- 
#     seq_len(nrow(e@linfct)) |>
#     purrr::map_dfr(function(i) as.data.frame(e[i])) |>
#     dplyr::mutate(
#       outcome = outcome_name,
#     ) |>
#     dplyr::select(
#       outcome,
#       asthma,
#       tx,
#       y_hat       = !!rlang::ensym(hat_name),
#       se          = SE,
#       ci_lower    = asymp.LCL,
#       ci_upper    = asymp.UCL
#     )    
#   print(d_predict)
# 
#   d_predict
# }

#+ model-4 -----------------
m4 <- glm( religiosity ~ wave + loss_dummy3 + displaced + km100_to_war, data = ds1 )
# m4 <- glm( religiosity ~ wave + loss_dummy3 + displaced               , data = ds1 )
m4 %>% tidy() 
m4 %>% GGally::ggcoef_model()

# m4 %>% emmeans::emmeans(specs = pairwise ~ loss_dummy3 + displaced)

hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)

eq_emmeans <- " ~ wave * loss_dummy3 * displaced | km100_to_war"
e <-
  emmeans::emmeans(
    object = m4, 
    specs = as.formula(eq_emmeans), 
    data = ds1,
    type = "response",
    at   = list(km100_to_war = c(1,4,7))
    # at   = list(wave = c("Before", "After"), km100_to_war = c(.1,1,7))#, loss_dummy3 = c("0", "1"))
  )  
print(e)

d_predict <-
  seq_len(nrow(e@linfct)) |>
  purrr::map_dfr(function(i) as.data.frame(e[i])) |>
  dplyr::mutate(
    outcome = "religiosity",
    loss_dummy3 = factor(loss_dummy3),
    displaced = factor(displaced)
  ) |>
  dplyr::select(
    outcome,
    wave,
    loss_dummy3,
    displaced,
    km100_to_war,
    y_hat       = !!rlang::ensym(hat_name),
    se          = SE,
    ci_lower    = lower.CL,  #asymp.UCL
    ci_upper    = upper.CL #asymp.UCL
  ) 

d_predict |> 
  ggplot(aes(x = wave, y = y_hat, group = loss_dummy3, color = loss_dummy3, fill = loss_dummy3)) +
  geom_point() +
  geom_line() + 
  # facet_wrap("km100_to_war") +
  facet_grid(displaced ~ km100_to_war)+
  theme_minimal()

#+ model-5 -----------------
m5 <- glm( religiosity ~ wave + loss_dummy3 + displaced + km100_to_war +
             loss_dummy3*displaced + loss_dummy3*km100_to_war + displaced*km100_to_war 
             # + loss_dummy3*displaced*km100_to_war
           , data = ds1 )
m5 %>% tidy() 
m5 %>% GGally::ggcoef_model()

hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
# hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)

eq_emmeans <- " ~ wave * loss_dummy3 * displaced | km100_to_war"
e <-
  emmeans::emmeans(
    object = m5, 
    specs = as.formula(eq_emmeans), 
    data = ds1,
    type = "response",
    at   = list(km100_to_war = c(1,4,7))
    # at   = list(wave = c("Before", "After"), km100_to_war = c(.1,1,7))#, loss_dummy3 = c("0", "1"))
  )  
print(e)

d_predict <-
  seq_len(nrow(e@linfct)) |>
  purrr::map_dfr(function(i) as.data.frame(e[i])) |>
  dplyr::mutate(
    outcome = "religiosity",
    loss_dummy3 = factor(loss_dummy3),
    displaced = factor(displaced)
  ) |>
  dplyr::select(
    outcome,
    wave,
    loss_dummy3,
    displaced,
    km100_to_war,
    y_hat       = !!rlang::ensym(hat_name),
    se          = SE,
    ci_lower    = lower.CL,  #asymp.UCL
    ci_upper    = upper.CL #asymp.UCL
  ) 

d_predict |> 
  ggplot(aes(x = wave, y = y_hat, group = loss_dummy3, color = loss_dummy3, fill = loss_dummy3)) +
  geom_point() +
  geom_line() + 
  # facet_wrap("km100_to_war") +
  facet_grid(displaced ~ km100_to_war)+
  theme_minimal()





#+ graph-5 -------------
# to summarize the effect among those who increased religiosity vs those who decreased
g5 <- 
ds1 %>% 
  ggplot(aes(x=waveL, y = religiosity, color = relig_increase))+ # note re-centering to align with model coefficients
  geom_point(alpha = .2)+
  geom_line(aes(group = key),alpha = .15)+
  # geom_smooth(method = "lm")+
  scale_x_continuous(breaks = c(0,1))+
  labs(
    title = "Individual change in standardized religiosity index "
    ,subtitle = "Religiosity Index = (Church attendance, Prayer, Self-reported Religiosity) | M=0,SD=1"
  )
g5
ds1 %>% group_by(relig_increase, wave) %>% summarize(mean=mean(religiosity),n=n())
# those who declined in religiosity (composite index) declined by .38
# while those who increased their religiosity, increased it by much more, by .74
# to paraphrase:
# positive response (increase in religiosity) was about 2 time stronger than negative response( decrease in religiosity)
g5 <-
  g5 +
  geom_smooth(method = "lm",color="blue")+
  line_equation +
  facet_grid(.~relig_increase)
g5

#+ save-to-disk ------------------------------------------------------------


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