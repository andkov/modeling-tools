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
prints_folder <- paste0("./analysis/nia-4-effects/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

# path_data_input <- "./analysis/nia-4-effects/osi/model-solution/nia-3-cra-effects-full.csv"
path_data_input <- "../sda-fiesta/analysis/nia-4-effects/osi/model-solution/nia-3-cra-effects-full.csv"


# ---- declare-functions -------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
ds0 <- 
  readr::read_csv(path_data_input) %>% 
  janitor::clean_names() %>% 
  mutate(
    value_level = case_when(
      term %in% c(
        "earnings_total_before" 
        ,"income_net_before"     
        ,"income_taxable_before" 
        ,"income_total_before"   
      ) ~ "1K 2022", TRUE ~ value_level # originally 1 CAN, but we'll rescale it to 1000
    )
  ) 



ds0 %>% glimpse()
# ---- inspect-data ------------------------------------------------------------
ds0 %>% count(intervention)
ds0 %>% count(outcome)
ds0 %>% count(var_name)
ds0 %>% group_by(var_name,value_level) %>% count() %>% print_all()



# ---- declare-labels ----------------------------------------------------------

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
  "earnings_total_delta"  = "Total earnings"
  ,"income_net_delta"     = "Net income"
  ,"income_taxable_delta" = "Taxable income"
  ,"income_total_delta"   = "Total income"
)
outcome_names <- outcome_labels %>%  names()

predictor_labels <- c(
  "tx"                      = "Intervention"
  ,"reference"               = "Reference group"
  ,"gender2"                 = "Sex"
  ,"age_category5"           = "Age group"
  ,"marital3"                = "Marital Status"
  ,"dependent4"              = "Dependents (N)"
  ,"education4"              = "Education"
  ,"ethnicity"               = "Ethnicity"
  ,"disability2"             = "Disability"
  ,"immigration"             = "Immigrant"
  ,"region7"                 = "Region"
  ,"spell_duration"          = "Duration of IS"
  ,"year_before"             = "Year before IS"
  ,"earnings_total_before"   = "Earnings before IS"
  ,"income_net_before"       = "Net Income before IS"
  ,"income_total_before"     = "Total Income before IS"
  ,"income_taxable_before"   = "Taxable Income before IS"
)
predictor_names <- predictor_labels %>% names()

# ds0 %>% group_by(var_name,value_level) %>% count()

ds_pred <- tibble::tribble(
  ~var_name,~value_level,~value_order,~value_level_display
  
  ,"tx"                    ,"FALSE"                ,0 , "No Intervention"
  ,"tx"                    ,"TRUE"                 ,1 , "Yes Intervention"
  
  ,"age_category5"         ,"middle age 1"         ,0  , "25-34"
  ,"age_category5"         ,"middle age 2"         ,1  , "35-44"
  ,"age_category5"         ,"middle age 3"         ,2  , "45-54"
  ,"age_category5"         ,"senior"               ,3  , "55+"
  ,"age_category5"         ,"youth"                ,-1 ,"16-24"
  
  ,"dependent4"            ,"0 dependents"         ,0  , "No dependents"
  ,"dependent4"            ,"1 dependent"          ,1  ,"1 dependent"   
  ,"dependent4"            ,"2 dependents"         ,2  ,"2 dependents"  
  ,"dependent4"            ,"3+ dependents"        ,3  ,"3+ dependents" 
  
  ,"disability2"           ,"Without Disability"   ,0  , "Without Disability" 
  ,"disability2"           ,"With Disability"      ,1  , "With Disability"    
  
  ,"education4"            ,"High School"          ,0  , "High School"        
  ,"education4"            ,"Less HS"              ,-1 , "Less HS"            
  ,"education4"            ,"Post HS"              ,1  , "Post HS"            
  ,"education4"            ,"University Degree"    ,2  , "University Degree"  
  ,"education4"            ,"(Missing)"            ,3  , "(Missing)"          
  
  ,"ethnicity"             ,"Caucasian"            ,0 , "Caucasian"          
  ,"ethnicity"             ,"Visible Minority"     ,1 , "Visible Minority"   
  ,"ethnicity"             ,"Indigenous"           ,2 , "Indigenous"         
  ,"ethnicity"             ,"(Missing)"            ,3 , "(Missing)"         
  
  ,"gender2"               ,"Men"                  ,0      ,"Men"                  
  ,"gender2"               ,"Women"                ,1      ,"Women"                
  ,"gender2"               ,"(Missing)"            ,2      ,"(Missing)"
  
  ,"immigration"           ,"Born in CAN"          ,0      ,"Born in CAN"          
  ,"immigration"           ,"immigrant"            ,1      ,"Immigrant"    
  
  ,"marital3"              ,"never married"        ,0      ,"Never Married"        
  ,"marital3"              ,"together"             ,1      ,"Together"             
  ,"marital3"              ,"apart"                ,2      ,"Apart"                
  ,"marital3"              ,"(Missing)"            ,3      ,"(Missing)"  
  
  ,"region7"               ,"Edmonton"             ,0      ,"Edmonton"             
  ,"region7"               ,"North West"           ,-3     ,"North West"           
  ,"region7"               ,"North Central"        ,-2     ,"North Central"        
  ,"region7"               ,"North East"           ,-1     ,"North East"           
  ,"region7"               ,"Central"              ,1      ,"Central"              
  ,"region7"               ,"Calgary"              ,2      ,"Calgary"              
  ,"region7"               ,"South"                ,3      ,"South"                
  
  ,"spell_duration"        ,"1 month"              ,0      ,"1 month"              
  ,"spell_duration"        ,"2-3 months"           ,1      ,"2-3 months"           
  ,"spell_duration"        ,"4-6 months"           ,2      ,"4-6 months"           
  ,"spell_duration"        ,"7-11 months"          ,3      ,"7-11 months"          
  ,"spell_duration"        ,"12-23 months"         ,4      ,"12-23 months"         
  ,"spell_duration"        ,"24+ months"           ,5      ,"24+ months"           
  
  ,"year_before"           ,"2018"                 ,1      ,"2018"                 
  ,"year_before"           ,"2017"                 ,0      ,"2017"                 
  ,"year_before"           ,"2016"                 ,-1      ,"2016"                 
  ,"year_before"           ,"2015"                 ,-2     ,"2015"                 
  ,"year_before"           ,"2014"                 ,-3     ,"2014"                 
  ,"year_before"           ,"2013"                 ,-4     ,"2013"                 
  ,"year_before"           ,"2012"                 ,-5     ,"2012"                 
  
  ,"earnings_total_before" ,"0 dollars"            ,0 , "0K 2022"
  ,"earnings_total_before" ,"1K 2022"              ,1 , "1K 2022"
  ,"income_net_before"     ,"0 dollars"            ,0 , "0K 2022"
  ,"income_net_before"     ,"1K 2022"              ,1 , "1K 2022"
  ,"income_taxable_before" ,"0 dollars"            ,0 , "0K 2022"
  ,"income_taxable_before" ,"1K 2022"              ,1 , "1K 2022"
  ,"income_total_before"   ,"0 dollars"            ,0 , "0K 2022"
  ,"income_total_before"   ,"1K 2022"              ,1 , "1K 2022"
) %>% 
  mutate(
    reference = value_order==0L
    ,row_number = row_number()
  )

ds_pred
# ---- tweak-data-raw --------------------------------------------------------------
# tidy up for generic reporting

ds_raw <- 
  ds0 %>%
  # add display labels
  mutate(
    intervention = factor(intervention, levels = intervention_names, labels = intervention_labels)
    ,outcome     = factor(outcome, levels = outcome_names, labels = outcome_labels)
  ) %>% 
  select(-term) %>% 
  # rename for meaningful and convenient reference
  rename(
    "predictor" = "var_name"
    ,"level"    = "value_level"
  )  %>% 
  # re-scale unique continuous for better comparison
  mutate(
    estimate = case_when(
      predictor %in% c(
        "income_net_before","income_taxable_before","income_total_before","total_earnings_before"
      ) ~ (estimate * 1000)
      , TRUE ~ estimate
    )
  ) %>%
  # round up for reporting
  mutate(
    across(
      .cols = c("estimate", "conf_low", "conf_high", "std_error")
      ,.fns = ~round(.,0)
    )
  ) %>% 
  mutate(
    across(
      .cols = c("p_value")
      ,.fns = ~round(.,3)
    )
  ) %>% 
  select(
    intervention, outcome, predictor, level, everything()
  )
ds_raw %>% filter(intervention=="Exposure Course",outcome=="Net income") %>% print_all()
# ds_raw %>% readr::write_rds("./analysis/effects-gaussian/ds1.rds")
# ds_raw %>% readr::write_csv("./analysis/effects-gaussian/nia-fiesta-model-results-raw.csv")
# ds_raw %>% names()
# ds_raw %>% count(intervention)
# ds_raw %>% count(outcome)

# `ds_raw` is a typical table of coefficients, but it lacks some details we could use in reporting effects
# To improve on this table, we re-shape and augment it with additional information

# ---- tweak-data-1 --------------------------------------------------------------
# prepare to create effect graph
ds0 %>% count(intervention)# 8 interventions
ds0 %>% count(outcome) # 4 outcomes
ds0 %>% group_by(var_name, value_level) %>% count() %>% print_all() # predictors and their levels


cat("\014")
ds_pred %>% print_all() # model stencil
d_model <- ds0 %>%  filter(intervention=="exposure_course",outcome=="income_net_delta") %>% print_all()


# table of slope coefficients
d_coef <- 
  ds0 %>%
  # d_model %>%
  filter(term != "(Intercept)") %>%
  select(-term) %>% 
  relocate(c("var_name","value_level"), .after = "outcome") 
# d_coef %>% print_all()

d_coef_intercept <-
  ds0 %>%
  # d_model %>%
  distinct(intervention,outcome) %>% 
  tidyr::expand_grid(
    ds_pred %>% 
      # filter(var_name == "tx",value_level == "FALSE") %>% 
      filter(reference) %>% 
      select(var_name, value_level)
  ) 
# d_coef_intercept %>% print_all()


ds1a <- 
  bind_rows(d_coef_intercept, d_coef) %>%# print_all()
  relocate(c("var_name","value_level"), .after = "outcome") %>%
  left_join(
    ds_pred
  ) %>% 
  left_join(
    ds0 %>% 
      filter(term == "(Intercept)") %>% 
      select(intervention, outcome, intercept = estimate)
  ) %>%
  # mutate(
  #   across(
  #     .cols = where(is.numeric)
  #     ,.fns = ~tidyr::replace_na(.,0)
  #   )
  # ) %>% 
  relocate(intercept, .after  = "value_level") %>% 
  arrange(intervention, outcome, row_number) 
ds1a %>% filter(intervention=="exposure_course",outcome=="income_net_delta") %>% print_all()
# correct for a unique predictor in each model (delta vs before)
# which also happens to be a continuous predictor
ds1 <- 
  ds1a %>% 
  mutate(
    unique_pred = (var_name %in% str_replace(outcome_names,"_delta","_before")) 
    ,target_pred = str_remove(outcome,"_delta$") ==
      str_remove(var_name,"_before$")
  ) %>% 
  filter(
    !(unique_pred & !target_pred)
  ) %>% 
  select(-c("unique_pred","target_pred"))
# ds1 %>% select(-c(8:11)) %>% print_all()

ds1 
# ds1 %>% print_all()
ds0 %>% filter(intervention=="exposure_course",outcome=="income_net_delta") %>% print_all()
ds1 %>% filter(intervention=="exposure_course",outcome=="income_net_delta") %>% print_all()


# original scale of unique (to each outcome) predictor was in dollors
# we want to change it to thousands of dollors for better display
ds2 <- 
  ds1 %>% 
  mutate(
    need_to_scale = case_when(
      (var_name %in% c(
        "income_net_before","income_taxable_before","income_total_before","earnings_total_before"
      )) & !reference ~ TRUE
      , TRUE ~ FALSE
    )
    ,estimate = case_when(need_to_scale ~ estimate*1000, TRUE~estimate)
    ,std_error  = case_when(need_to_scale ~ std_error*1000, TRUE~std_error )
    ,conf_low  = case_when(need_to_scale ~ conf_low *1000, TRUE~conf_low )
    ,conf_high  = case_when(need_to_scale ~ conf_high*1000, TRUE~conf_high )
    
  ) %>%
  filter(
    !( (need_to_scale & is.na(estimate))  )
  )  %>% 
  select(-need_to_scale) %>% 
  # rename unique continuous predictor to a common name
  mutate(
    var_name_common = case_when(
      var_name %in% c("income_net_before",
                      "income_total_before",
                      "income_taxable_before",
                      "earnings_total_before")
      ~ "reported_before_is",
      TRUE ~ var_name
    )
  )
# ds2 %>% print_all()
ds2 %>% filter(intervention=="exposure_course",outcome=="income_net_delta") %>% print_all()

# add intercept
ds3 <-
  ds2 %>% 
  bind_rows(
    ds0 %>% 
      filter(term == "(Intercept)") %>% 
      select(-term) %>% 
      mutate(
        value_level_display = "Reference Group"
        ,var_name = value_level
        ,reference = NA
        ,value_order = 0L
        ,row_number = 0L
        ,var_name_common = value_level
        
      )
  ) %>% 
  # mutate(
  #   intercept = case_when(var_name=="(Intercept)"~estimate,TRUE~intercept)
  # ) %>% 
  arrange(
    intervention, outcome, row_number
  ) #%>% 
  # select(
  #   -std_error
  #   ,-statistic
  # )
# ds3 %>% print_all()
ds3 %>% filter(intervention=="exposure_course",outcome=="income_net_delta") %>% print_all()

# factors 
ds4 <- 
  ds3 %>% 
  mutate(
    intervention = factor(intervention, levels = intervention_names, labels = intervention_labels)
    ,outcome     = factor(outcome, levels = outcome_names, labels = outcome_labels)
  ) %>% 
  # select(-term) %>% 
  # rename(
  #   "predictor" = "var_name"
  #   ,"level"    = "value_level"
  # )  %>% 
  mutate(
    var_name_display = factor(
      var_name
      , levels = c(predictor_names, "(Intercept)")
      , labels = c(predictor_labels, "(Intercept)")
    )
    
  ) %>% 
  relocate(
    c("var_name_display","value_level_display")
    ,.after = "outcome"
  )
# ds4 %>% print_all()

ds0 %>% filter(intervention=="exposure_course",outcome=="income_net_delta") %>% print_all()
ds1 %>% filter(intervention=="exposure_course",outcome=="income_net_delta") %>% print_all()
ds2 %>% filter(intervention=="exposure_course",outcome=="income_net_delta") %>% print_all()
ds3 %>% filter(intervention=="exposure_course",outcome=="income_net_delta") %>% print_all()
ds4 %>%  filter(intervention=="Exposure Course",outcome=="Net income") %>% print_all()

# ds2 %>% readr::write_csv("./analysis/effects-gaussian/nia-fiesta-model-results-tidy.csv")
# ds2 %>% readr::write_rds("./analysis/effects-gaussian/nia-fiesta-model-results-tidy.rds")
# ----- define-comparison-groups ------

# use the following table to see what you need to enter
contrast_options <- 
  ds4 %>% 
  # filter(reference) %>%
  select(var_name_common, value_level, reference) %>% 
  distinct() %>% 
  print_all()

# define the reference and the comparison group 
d_comparison_group <- 
  tibble::tribble(
  ~var_name, ~reference_value, ~comparison_value 
 ,"tx"                 ,"FALSE"                , "TRUE"                           
 ,"age_category5"      ,"middle age 1"         , "middle age 1"                    
 ,"dependent4"         ,"0 dependents"         , "0 dependents"                    
 ,"disability2"        ,"Without Disability"   , "Without Disability"              
 ,"education4"         ,"High School"          , "High School"                     
 ,"ethnicity"          ,"Caucasian"            , "Caucasian"                       
 ,"gender2"            ,"Men"                  , "Men"                             
 ,"immigration"        ,"Born in CAN"          , "Born in CAN"                     
 ,"marital3"           ,"never married"        , "never married"                   
 ,"region7"            ,"Edmonton"             , "Edmonton"                        
 ,"spell_duration"     ,"1 month"              , "1 month"                         
 ,"year_before"        ,"2016"                 , "2016"                            
 ,"reported_before_is" ,"0 dollars"            , "0 dollars"                 
) 

ds4 %>% 
  select(-c(3:4,9:13)) %>% 
  left_join(
    d_comparison_group %>% 
      select(var_name, comparison_value) %>% 
      mutate(comparison_group = )
  )
# ---- table-1 -----------------------------------------------------------------
# dynamic table for looking up
# ds1 %>% 
#   select(
#     intervention, outcome, predictor, level, estimate, p_value, conf_low, conf_high
#   ) %>% 
#   DT::datatable(
#     class   = 'cell-border stripe'
#     ,filter  = "top"
#     ,options = list(
#       pageLength = 34,
#       autoWidth  = FALSE
#     )
#   )



# ---- graph-0 -----------------------------------------------------------------

# definition of a reference group
d <- 
  ds_pred %>% 
  filter(reference) %>% 
  mutate(
    var_name = factor(var_name, levels = predictor_names, labels = predictor_labels)
  ) %>% 
  select(var_name, value_level_display) %>% 
  mutate(
    value_level_display = toupper(as.character(value_level_display))
  ) %>% 
  distinct() %>% 
  rename(
    'Predictor' = var_name
    ,'LEVEL' = value_level_display
  )
d %>% print_all()
d %>% neat(align=c("r","l"))



# ---- graph-1 -----------------------------------------------------------------
# demonstration of treatment effect for the reference group

dg1 <- 
  # ds0 %>%
  # ds_raw %>%
  ds4 %>% 
  # filter(intervention=="exposure_course",outcome=="income_net_delta") %>%
  # filter(term %in% c("(Intercept)","txTRUE") ) %>% 
  filter(var_name %in% c("tx")) %>% 
  # left_join(
  #   ds1 %>% distinct(intervention, outcome,intercept)
  # ) %>% 
  # mutate(
  #   intervention = factor(intervention, levels = intervention_names, labels = intervention_labels)
  #   ,outcome     = factor(outcome, levels = outcome_names, labels = outcome_labels)
  # ) %>% 
  # mutate(
  #   condition = value_level %>% factor() %>% fct_recode(
  #     "Reference" = "(Intercept)"
  #     ,"Intervention" = "TRUE" 
  #   )
  # ) %>% 
  # select(-var_name, -value_level, -term) %>% 
  # relocate(condition, .after = "outcome") %>% 
  mutate(
    impact_direction = case_when(
      estimate >0L ~ "positive",
      estimate <=0L ~ "negative"
    )
    ,sign_at_05 = p_value <= .05
  ) 
dg1

g1 <- 
  dg1 %>% 
  ggplot(aes(y =intervention, shape = value_level_display))+
  # Reference
  geom_point(
    aes(x = intercept)
    ,data = . %>% filter(value_level == FALSE)
    # ,shape = 21
  )+
  # Intervention
  geom_point(
    aes(x=estimate+intercept, fill = sign_at_05)
    # ,data = . %>% filter(value_level == TRUE)
    ,data = . %>% filter(value_level_display == "Yes Intervention")
    ,shape = 21
    ,size = 2
  )+
  geom_segment(
    aes(x=intercept+estimate, xend = intercept, yend = intervention)
  )+
  geom_text(aes(label = scales::comma(estimate,1) ,x=intercept+estimate, color = impact_direction, alpha = sign_at_05)
            ,size = 4, nudge_y = .2)+
  # geom_text(aes(label = scales::comma(intercept,1),x=intercept), data = . %>% filter(value_level=="FALSE")
  geom_text(aes(label = scales::comma(intercept,1),x=intercept), data = . %>% filter(value_level_display=="No Intervention")
            ,size = 2.5, alpha = .5, color = "grey40",nudge_y=-.15)+
  facet_wrap(facets = "outcome", nrow=1)
g1
g2 <- 
  g1 + 
  scale_color_manual(values = c("positive" = "blue", "negative" = "red"))+
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"))+
  # scale_shape_manual(values = c("TRUE"=21, "FALSE"=124))+
  scale_shape_manual(values = c("Yes Intervention"=21, "No Intervention"=124))+
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = .2))+
  scale_x_continuous(
    breaks = seq(10000,30000,5000)
    ,labels = scales::comma_format()
    # ,limits = c(14000,26000)
    ,expand =  expansion(mult = c(0.2,0.2))
  )+
  guides(alpha = F)+
  labs(
    shape = ""
    ,color = "Direction of impact"
    ,fill = "Significant at .05"
    ,x = "Change in the amount reported to CRA, in 2022 dollars"
    ,title = "Net impact of intervention in the year after the first Income Support spell"
    # ,subtitle  = ""
  )
g2
dg1
g2 %>% quick_save("1",w=11,h=5)



# ---- graph-2 -----------------------------------------------------------------
# Comparison of tx effect across value of a covariate

# target_predictor <- "gender2"
target_predictor <- "age_category5"
# target_predictor <- "dependent4"


dg1 <- 
  ds3 %>% 
  # filter(intervention %in% c("workshop_noncp")) %>%
  # filter(intervention %in% c("ab_job_corps","career_planning")) %>%
  filter(
    var_name %in% c("(Intercept)","tx",target_predictor)
  ) %>% 
  group_by(intervention, outcome) %>% 
  mutate(
    tx_effect = max(estimate[which(value_level_display == "Yes Intervention")],na.rm = T)
    ,std_error = case_when(
      reference & !var_name == "tx" ~ std_error[which(value_level_display == "Yes Intervention")]
      ,TRUE ~ std_error
    )
    ,statistic = case_when(
      reference & !var_name == "tx" ~ statistic[which(value_level_display == "Yes Intervention")]
      ,TRUE ~ statistic
    )
    ,p_value = case_when(
      reference & !var_name == "tx" ~ p_value[which(value_level_display == "Yes Intervention")]
      ,TRUE ~ p_value
    )
    ,conf_low = case_when(
      reference & !var_name == "tx" ~ conf_low[which(value_level_display == "Yes Intervention")]
      ,TRUE ~ conf_low
    )
    ,conf_high = case_when(
      reference & !var_name == "tx" ~ conf_high[which(value_level_display == "Yes Intervention")]
      ,TRUE ~ conf_high
    )
    ,estimate = case_when(reference ~ 0,TRUE ~ estimate)
  ) %>% 
  ungroup() %>% 
  relocate(tx_effect, .after = "intercept") %>% 
  # filter(!var_name %in% c("(Intercept)","tx")) %>% 
  filter(!var_name %in% c("tx")) %>% 
  mutate(
    estimate_adj = estimate + intercept + tx_effect 
    ,conf_low_adj = conf_low + intercept + tx_effect
    ,conf_high_adj = conf_high + intercept + tx_effect
    ,sign_05 = p_value <= .01
    ,estimate_adj = case_when(
      var_name == "(Intercept)" ~ estimate, TRUE ~ estimate_adj
      
    )
  ) %>% 
  select(-std_error, -statistic, -conf_low, -conf_high)
dg1 %>% print(n=16)

pred_levels <- dg1 %>% 
  filter(value_level != "(Intercept)") %>% 
  distinct(value_level, value_order,reference) %>% 
  arrange(desc(reference), value_order) 
custom_shapes <- c(3, 25,24, 21, 22, 23)
(pred_level_values <- c( "(Intercept)",(pred_levels %>% pull(value_level))) )
(pred_level_shapes <- custom_shapes[1:length(pred_level_values)])
names(pred_level_shapes) <- pred_level_values
pred_level_shapes

g1 <-
  dg1 %>% 
  
  ggplot(aes(x=estimate_adj, y = intervention, fill = sign_05, color = sign_05))+
  # geom_point()+
  # geom_point(aes(shape = value_level))+
  geom_point(aes(shape = value_level), data = . %>% filter(var_name !="(Intercept)"), size = 3, alpha = .6, color ="black")+
  geom_point(aes(shape = value_level), data = . %>% filter(var_name !="(Intercept)"), size = 3, alpha = .6)+
  geom_point(aes(shape = value_level), data = . %>% filter(var_name =="(Intercept)"), color="black", fill = "black",
             size = 4)+
  facet_wrap(facets = "outcome", nrow=1)
g2 <- g1 +
  scale_shape_manual(
    values = pred_level_shapes
  )+
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "white"))+
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "grey60"))+
  labs(
    fill = "Significant\nat .05", color = "Significant\nat .05"
  )

g2  
g2 %>% quick_save("test", w=11,h=5)


# ---- predicted-values --------------------------
# wrong save, intervention has values of the outcome, must redo before using
# ds_hat0 <- readr::read_rds("./analysis/nia-4-effects/osi/model-solution/nia-3-cra-fitted.rds")
# ds_hat0 %>% glimpse()
# 
# ds_hat1 <- 
#   ds_hat0 %>% 
#   filter(intervention=="exposure_course",outcome=="income_net_delta")
# 
# ds_hat1


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
