# run `./analysis/6-eda-tax_year/eda-tax_year.R` to create the environment
# on 2023-09-13 you stopped on serializing the twang solution
# the big issue was the role of wave during balancing, must graft weights onto
# the data originally used in twang, not the data object produced by it. 
source("./analysis/8-model-B/group-balancing-functions.R")
source("./analysis/8-model-B/effect-estimation-functions.R")
library(twang)
# ----- treatment-effect-1b ------------------------
# ps1 %>% compute_effect_on_binary("return12m")
# ps1 %>% compute_effect_on_continuous("spell_duration",covariate_names = "fy_is_start")
# ps1 %>% compute_effect_on_continuous("spell_duration")
# ps1 %>% compute_effect_on_continuous("income_net_delta",out_table = "nia")
# ps1 %>% compute_effect_on_continuous("income_net_delta",out_table = "est")
# ps1 %>% compute_effect_on_continuous("income_net_delta", covariate_names = c("gender2","education4") )
# ps1 %>% compute_effect_on_continuous("income_net_delta", covariate_names = c("gender2","education4") ,out_table = "est")
# ps1 %>% compute_effect_on_continuous("income_net_delta", covariate_names = c("gender2","education4") ,out_table = "est")

# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "nia")
# ps1 %>% compute_effect_on_continuous("earnings_total_delta",covariate_names = covariates,out_table = "nia")
# ps1 %>% compute_effect_on_continuous("income_total_delta"  ,covariate_names = covariates,out_table = "nia")
# ps1 %>% compute_effect_on_continuous("income_taxable_delta",covariate_names = covariates,out_table = "nia")



# ---- declare-globals-for-nia ------------------------------------------------
source("./analysis/8-model-B/nia-variables.R")
# ---- inspect-design ------------------------------------------------------------
ds5B %>% filter(person_oid %in% sample_of_interest1) %>% mask_ids() %>%  glimpse()
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
# ---- inspect-covariates ------------------------------------------------------------
# Covariates
# only those case were selected for estimating the impact of services
# which had the outcome measured at both waves (before/after IS)
tableone::CreateTableOne(
  data = ds4 %>% filter(timeline_is == -1L) %>% mutate(across(where(is.factor),fct_drop))
  ,vars = covariates
  ,strata = "has_before_and_after"
) %>%
  print(formatOptions = list(big.mark = ","))

tableone::CreateTableOne(
  data = ds5B %>% filter(timeline_is == -1L)
  ,vars = covariates
) %>%
  print(formatOptions = list(big.mark = ","))

# ---- empirical-reference-group -----------------------------------------------
# let us determine what combination of predictors used in the model is the most
# frequently occurring one, to serve as an "empirical reference group"

ds5B %>% 
  # filter(used_in_nia) %>% 
  filter(timeline_is == -1L) %>% # group balancing takes place in the time point before IS
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
  data = ds5B %>% filter(waveL==1L) # no matter which wave, person-level
  ,vars = intervention
  
  # ,factorVars = intervention
) %>% summary()
# Notes:
# 1. Less than 1% of our sample had Needs Identification assessment, which has
# extremely limited window of application
# We recommend it's being dropped from the list of "intervention"
# intervention_used <- setdiff(intervention,c("assessment_ni","ab_job_corps"))
# intervention_used <- setdiff(intervention,c("assessment_ni"))



# -----Single Propensity Score Model ----
# Go through an example of a single outcome - intervention pair

# ---- ds-nia-1-initial-differences ------------------------------------------------
# for(i in seq_along(intervention)){intervention[i] %>% print()}
focal_intervention <- "career_planning" # this determines the loop
# we do not care about outcomes at this point, they are outside of the set c(covariates,interventions)
# we want to derive a set of weight to balance the groups with respect to the

# d_nia - data rectangle used to derived balancing weights
# weights will be specific to the set of balancing variables (covariates +  other intervention - small counts)
d_nia <- 
  ds5B %>%
  filter(timeline_is == -1L) %>% 
  select(all_of(c(
    design, outcomes,covariates,intervention
  ))) %>% 
  # filter(fy_is_start != "2022") %>% # event too recent to observe the outcome
  # mutate(fy_is_start = fct_drop(fy_is_start)) %>% 
  create_treatment_binary(tx_var = focal_intervention)
# make_treatment_binary(tx_var = focal_intervention)

# we replaced the variables of the intervention in focus with a binary `tx`
d_nia %>%  filter(person_oid %in% sample_of_interest1) %>% glimpse()

# d_nia %>% group_by(region3) %>% count()
# nonnormal_vars <- c(intervention_used)# matters when not binary
tab1 <- tableone::CreateTableOne(data=d_nia %>% select(-person_oid), strata = "tx")
tab1 %>%  print(formatOptions = list(big.mark = ",")) # add nonnormal= when not binary

d_nia %>%
  group_by(waveF,tx) %>% summarize(
    sample_size = n()
    , mean_outcome = mean(earnings_total,na.rm=T)
  )
# ---- ds-nia-1-ps-model ---------------------------------------------------------------

# Treatment: Career Planning workshop 
# Note, for FIEST we use "anytime before the end of the IS" whil in CIES dashboard
# the criterion is more restrictive: (at least 1 event between the end of the program and 1y prior to its start)


# We want to compare the outcome (`earnings_total`) between
# clients who had treatment vs those who did not. Clients self-select into 
# treatment and control groups. The balancing procedure below removes the
# influence of the known confounders on the probability of receiving the TX,
# by finding such weights that minimize observed difference between groups

vars_to_balance_on <- c(
  covariates # ALL COVARIATES MUST BE FACTORS!!! OTHERSWISE WILL GENERATE ERROR
  ,setdiff(
    intervention # those to balance on  +  estimate impact of
    ,c( # but remove these one
      focal_intervention # the outcome of the PS model
      ,"assessment_ni" # too few obs
      ,"assessment_era" # too few obs
    )
  )
) %>% unique() %>% print()

dependent <- "tx" # the intervention in focus
explanatory <- vars_to_balance_on # serve as explanatory in PS model
(eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) ))
# (eq_formula <- as.formula(paste0(dependent," ~ ", paste(setdiff(intervention,"career_planning"), collapse = " + ") ) ))
ps1 <-
  twang::ps(
    # propensity score model
    formula            = eq_formula
    ,estimand          = "ATT" # use ATT because that is consistent with ex-post net impact
    # ,data              = d_nia %>% filter(waveL== 1L) %>% as.data.frame()
    ,data              = d_nia  %>% as.data.frame()
    # ,sampw             = d_nia$survey_weight # !!! optional
    # gradient boosting
    ,stop.method       = c("es.mean", "es.max", "ks.mean", "ks.max")
    ,n.trees           = 500
    ,interaction.depth = 2
    ,shrinkage         = 0.01
    ,n.minobsinnode    = 10
    # computational efficiency
    ,n.keep            = 1
    ,n.grid            = 25
    ,ks.exact          = NULL
    ,version           = "gmb" # gmb, xboost, legacy
    # other
    ,verbose           = FALSE
  )

# Apart from diagnostic tables and graphs which allow us to evaluate the quality
# of the balancing procedure, what we need from this product is the set of
# weights (in this case one for each person) that remove the influence of 
# known confounders on the probability of receiving a treatment

d_nia_w <-
  ps1$data %>% 
  mutate(
    w =  get.weights(ps1,stop.method = "es.mean", estimand = "ATT" )
  ) %>%
  as_tibble() %>% select(person_oid,w) %>% mutate(tx_name = focal_intervention)

ps1_meta <- list(
  "outcome" = "earning_total"
  ,"tx_name" = focal_intervention
  ,"balance_vars" = vars_to_balance_on
)
d_nia_w %>% filter(person_oid == sample_of_interest1)

ps1 %>% readr::write_rds(paste0("./analysis/8-model-B/twang/ps1-",focal_intervention,".rds") )# off after first run
# it appears one needs to select only a single wave, because twang produces different weights
# when only a single wave is selected (treats a row as a grain?)

# save the rectangle used for deriving balancing weights, the input into twang procedure
d_nia %>% readr::write_rds(paste0("./analysis/8-model-B/twang/d1-",focal_intervention,".rds")) # input file
# we save the rectangle with weights
d_nia_w %>% readr::write_rds(paste0("./analysis/8-model-B/twang/w1-",focal_intervention,".rds"))
ps1_meta %>% readr::write_rds(paste0("./analysis/8-model-B/twang/meta1-",focal_intervention,".rds"))



# ----- rd1-diagnostics-1 ---------------------------------------------
library(twang)
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
    ,title = paste0("Group balance across individual covariates. Intervention = ", intervention_labels[focal_intervention])
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




# ----- Serialization of the Workflow ------------------------------------------------
# ---- serialized-solution-1-ps -----------------------------------------------------

# compute PS weights for every intervention
intervention_impact # use as a `tx`
vars_to_balance_on # focal intervention excluded
for(intervention_i in names(intervention_impact)){
  # identify the intervention in focus
  # intervention_i <- "career_planning"
  # intervention_i <- "english_as_second"
  # create data set for analysis
  focal_intervention <- intervention_i
  d_nia <- 
    ds5B %>%
    filter(timeline_is == -1L) %>% # balancing at the time point before the IS
    select(all_of(c(
      design, outcomes,covariates,intervention
    ))) %>% 
    create_treatment_binary(tx_var = focal_intervention)
  # make_treatment_binary(tx_var = focal_intervention)
  # d_nia %>% glimpse()
  # specify propensity score equation
  dependent <- "tx"
  
  vars_to_balance_on <- c(
    covariates # ALL COVARIATES MUST BE FACTORS!!! OTHERSWISE WILL GENERATE ERROR
    ,setdiff(
      intervention # those to balance on  +  estimate impact of
      ,c( # but remove these one
        focal_intervention # the outcome of the PS model
        ,"assessment_ni" # too few obs
        ,"assessment_era" # too few obs
      )
    )
  ) %>% unique()
  
  explanatory <- vars_to_balance_on
  (eq_formula <- as.formula(paste0(dependent," ~ ", paste(explanatory, collapse = " + ") ) ))
  # compute propensity score weights
  twang_object <-
    twang::ps(
      # propensity score model
      formula            = eq_formula
      ,estimand          = "ATT" # use ATT because that is consistent with ex-post net impact
      ,data              = d_nia %>%  as.data.frame()
      # ,sampw             = d_nia$survey_weight # !!! optional
      # gradient boosting
      ,stop.method       = c("es.mean", "es.max", "ks.mean", "ks.max")
      ,n.trees           = 2000
      ,interaction.depth = 2
      ,shrinkage         = 0.01
      ,n.minobsinnode    = 10
      # computational efficiency
      ,n.keep            = 1
      ,n.grid            = 25
      ,ks.exact          = NULL
      ,version           = "gmb" # gmb, xboost, legacy
      # other
      ,verbose           = FALSE
    )
  # isolate derived weights for subsequent use
  d_nia_w <-
    twang_object$data %>% 
    mutate(
      w =  get.weights(twang_object,stop.method = "es.mean", estimand = "ATT" )
    ) %>%
    as_tibble() %>% select(person_oid,w,tx) %>% mutate(tx_name = focal_intervention)
  
  # meta-data of the twang object
  ps_meta <- list(
    "outcome" = "earning_total"
    ,"tx_name" = focal_intervention
    ,"balance_vars" = vars_to_balance_on
    ,"w" = d_nia_w
  )
  
  # save twang object for easier access later
  path_save_twang_object <- paste0("./analysis/8-model-B/twang/ps/ps-",intervention_i,".rds")
  path_save_d_nia        <- paste0("./analysis/8-model-B/twang/d/d-",intervention_i,".rds")
  path_save_d_nia_w      <- paste0("./analysis/8-model-B/twang/w/w-",intervention_i,".rds")
  path_save_ps_meta      <- paste0("./analysis/8-model-B/twang/meta/meta-",intervention_i,".rds")
  
  twang_object %>% readr::write_rds(path_save_twang_object)
  d_nia %>% readr::write_rds(path_save_d_nia)
  d_nia_w %>% readr::write_rds(path_save_d_nia_w)
  ps_meta %>% readr::write_rds(path_save_ps_meta)
}

# ---- serialized-solution-2-diagnostics ----------------------------------------

# print graphs that examine the quality of group balancing
# read in PS objects
ps_path <- list.files("./analysis/8-model-B/twang/ps",full.names = T)
ls_ps <- list()
for(i in seq_along(ps_path)){
  path_to_ps <- ps_path[i]
  name_of_ps <- basename(ps_path[i]) %>% str_remove("^ps-") %>% str_remove(".rds$")
  ls_ps[[name_of_ps]] <- readr::read_rds(path_to_ps)
}
names(ls_ps)

# read in meta data from twang balancing procedure
meta_path <- list.files("./analysis/8-model-B/twang/meta",full.names = T)
ls_meta <- list()
for(i in seq_along(meta_path)){
  path_to_ls <- meta_path[i]
  # ld <- readr::read_rds(path_to_d)
  ls_meta_i <- readr::read_rds(path_to_ls)
  tx_name <- ls_meta_i[["tx_name"]]
  ls_meta[[tx_name]] <- ls_meta_i
}
names(ls_meta)
ls_meta$career_planning$w # rectangle with derived weights
ls_meta[["career_planning"]][["w"]]

# print diagnostics
for(intervention_i in names(ls_ps)){
  # intervention_i <- "career_planning"
  # intervention_i <- "workshop_noncp"
  twang_object <- ls_ps[[intervention_i]]
  
  balance_table <-
    twang_object %>%
    bal.table() %>% # gets balance table
    purrr::map(tibble::rownames_to_column,"covariate") %>%  # add covariate names
    dplyr::bind_rows(.id = "method") %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(
      # covariate = as_factor(covariate)
      intervention = intervention_i
    )
  stem <- "./analysis/8-model-B/twang-diagnostics/"
  folder_for_diagnostic_graphs <- paste0(stem,intervention_i,"/")
  if (!fs::dir_exists(folder_for_diagnostic_graphs)) {fs::dir_create(folder_for_diagnostic_graphs)}
  
  # write the table
  balance_table %>% readr::write_csv(file= paste0(stem,"balance-table-",intervention_i,".csv") )
  
  # diagnositic graphs
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
  # copy, relevant when combining group balancing and effect estimation
  ggsave(
    filename = paste0(intervention_i,"-0-group-imbalance.png")
    ,plot = g
    ,path = "./analysis/8-model-B/model-solution/"
    ,device = "png"
    ,width=12
    ,height=7
  )
  
}


# In this edition of the workflow, we isolate group balancing, so that we can 
# use the same weights in effect estimation of different longitudinal models
# ----- interpret-one-model ---------------
# this code shows how to implement the computation of intervention effect
# however, we will be using different models (A, B, C) to summarize the longitudinal data
# so we want to work with the same set of weights to balance groups on receiving the intervention
# we omit this task (modeling) for now and push it closer to the specific model.

# ps_object <- readr::read_rds("./analysis/6-eda-tax_year/twang/ps1_full.rds")
ps_object <- readr::read_rds("./analysis/8-model-B/twang/ps/ps-career_planning.rds")
# d_nia <- readr::read_rds("./analysis/8-model-B/twang/d/d-career_planning.rds") #used for group balancing
# d_w <- readr::read_rds("./analysis/8-model-B/twang/w/w-career_planning.rds") #used for group balancing
ps_meta <- readr::read_rds("./analysis/8-model-B/twang/meta/meta-career_planning.rds") #used for group balancing

focal_intervention <- ps_meta$tx_name

ds5B %>% filter(person_oid == sample_of_interest1) %>% select(person_oid, tax_year,waveF, timeline_is, earnings_total, career_planning)
ps_meta[["w"]] %>% filter(person_oid == sample_of_interest1) 

# note that d_nia_w is joined onto d_nia, which has ALL waves
d_nia_w <- 
  ds5B %>% 
  left_join(
    ps_meta[["w"]] 
  )
# weights relate to focal intervention
d_nia_w %>% filter(person_oid == sample_of_interest1) %>% select(person_oid, tax_year,waveF, timeline_is, earnings_total, tx, w)

design.ps <- survey::svydesign(ids= ~1, weights = ~w, data = d_nia_w)
# d %>% group_by(tx) %>% count()

outcome_name <- "earnings_total"
treatment_name = "tx"
outcome_n <- 
  d_nia_w %>%
  group_by(waveF,tx) %>%
  summarize(
    outcome_n = sum(!is.na(!!rlang::sym(outcome_name)))
    ,.groups = "drop"
  ) %>% 
  tidyr::pivot_wider(names_from = "tx", names_prefix = "tx_", values_from = "outcome_n") %>% 
  mutate(
    total_n = tx_FALSE + tx_TRUE
  )
outcome_n

# (model_equation <- as.formula(paste0(outcome_name, "~", treatment_name)))
# (pattern_starts_with_explanatory <- paste0("^","tx", collapse = "|"))

# on 2023-11-28 you stopped here, you've lost the compute_effect_on_continuous function (accicendally deleted)
# that used the ps_meta = argument. must reconstruct. 

d_result <-
  compute_effect_on_continuous(
    data_raw = ds5B # in which effects must be estimated
    ,ps_meta = ls_meta # meta object with derived weights for a given intervention (all computed interventions)
    ,outcome_name = "earnings_total"
    ,treatment_name = "career_planning"
    ,covariate_names = NULL
    # ,covariate_names = c("sex2","age_in_years","marital2","dependent2"
    #                      # ,"education4"
    #                      # ,"disability2","ethnicity","immigration", "region7",
    #                      # "spell_duration_cat","years_btw_before_after_cat", "fy_is_start"
    #                      )
    ,out_table = "predict" # outputs table of predicted value for selected levels
  )
d_result


# ---- serialized-solution-3-effects-nia ----------------------------------------
# compute effects for all interventions

# Average effect of the treatment (outcome ~ tx*wave)
ls_effect <- list()
for(intervention_i in names(ls_meta)){
  
  ls_effect[[intervention_i ]] <- 
    compute_effect_on_continuous(
      data_raw = ds5B # in which effects must be estimated
      ,ps_meta = ls_meta # meta object with derived weights for a given intervention (all computed interventions)
      ,outcome_name = "earnings_total"
      ,treatment_name =intervention_i
      ,covariate_names = NULL
      # ,covariate_names = c("sex2","age_in_years","marital2")
      ,out_table = "predict" # outputs table of predicted value for selected levels
    )
  
  # )    
  # ds_effects <- ls_effect %>%  bind_rows() 
  # ls_effects[[intervention_i]] <- ds_effects
}

ds_effect <- 
  ls_effect %>% 
  bind_rows(.id = "intervention")

ds_effect %>% 
  readr::write_csv("./analysis/8-model-B/model-solution/nia-effects-null.csv")


# Conditional model with all covariates
ls_effect <- list()
for(intervention_i in names(ls_ps)){
  
  ls_effect[[intervention_i ]] <- 
    compute_effect_on_continuous(
      data_raw = ds5B # in which effects must be estimated
      ,ps_meta = ls_meta # meta object with derived weights for a given intervention (all computed interventions)
      ,outcome_name = "earnings_total"
      ,treatment_name =intervention_i
      # ,covariate_names = NULL
      ,covariate_names = c("sex2","age_in_years","dependent2","marital2")
      # ,covariate_names = c(covariates, setdiff(intervention,intervention_i))
      ,out_table = "predict" # outputs table of predicted value for selected levels
    )
}

ds_effect <- 
  ls_effect %>% 
  bind_rows(.id = "intervention")

ds_effect %>% 
  readr::write_csv("./analysis/8-model-B/model-solution/nia-effects-full.csv")

# on 2023-11-21 you stopped here. Next: repeat these computations in a clean way in model-B script
#
