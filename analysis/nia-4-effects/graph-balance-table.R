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
base::source("./scripts/graphing/graph-presets.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here when `quick_save("name",w=8,h=6)` is used:
prints_folder <- paste0("./analysis/nia-4-effects/prints/")
if (!fs::dir_exists(prints_folder)) { fs::dir_create(prints_folder) }

# ---- declare-functions -------------------------------------------------------

# ---- background ----------------------
# see twang vignette at (https://cran.r-project.org/web/packages/twang/vignettes/twang.pdf

# given a `twang_object` (output of the twang::ps() function)
# diagnositic graphs

# twang_object %>% plot("optimize", main = "Number of iteration to find an optimal balance between groups") %>% print()
# balance as  function of gbm.iteration
# this command plots the size of the imbalance vs. the number of iterations.  For
# es.max.ATT and ks.max.ATT, this is the maximum of the absolute value of std.eff.sz
# (standardized bias).  For ex.mean.ATT and ks.mean.ATT, this is the mean of the
# absolute value of std.eff.sz (standardized bias)


# twang_object %>% plot("boxplot", main = "Distribution of propensity scores (Low control & High Tx = No common support)" )%>% print()
# distribution of propensity scores
# we have common support when the propensity scores approximately lineup between treatment and control
# we lack common support if propensity score is generally low for control and high for treatment


# twang_object %>% plot("es", main = "Standard Effect Size (The higher the dot, the lower similarity between groups)") %>% print()
# standardized effect size of pre-treatment variables
# each dot is a variable and the higher the dot, the greater the dissimilarity between treatment and control
# es test assumes normal distribution to compute standardized effects

# twang_object %>% plot("t", main = "T-test p-values for weighted pre-treatment variables (higher = better)") %>% print()
# t-test p-values for weighted pre-treatment variables
# we learned that es.max.ATT optimizes the worst match and so we should use it
# if we are afraid that the poorest match is the most important variable to balance on
# we should use one of the other three methods if all variables to balance on are equally important

# twang_object %>% plot("ks", main = "Kolmogorov-Smirnov p-values for weighted pre-treatment variables (higher = better)") %>% print()
# kolmogorov-smirnov p-values for weighted pre-treatment variables
# ks test is a nonparametric test that compares cumulative distributions of two datasets
# Ho: both groups are sampled from populations with identical distributions
# Ha: null hypothesis violated:  different medians, variances, or distributions.


# ---- load-data --------------------------------

# read in multiple files
# folder_path <- "../sda-fiesta/analysis/nia-4-effects/osi/twang-diagnostics/"
# file_path <- list.files(path=folder_path,pattern =".csv$",full.names = T)
# file_name <- basename(file_path) %>% str_remove("^balance-table-") %>% str_remove(".csv$")
# l_object <- list()
# for(i in seq_along(file_path)){
#   # i <- 1
#   l_object[[file_name[i]]]  <- readr::read_csv(file_path[i])
# }
# dbt <- l_object %>% bind_rows(.id = "intervention") %>% relocate(intervention)
# dbt %>% readr::write_csv("./analysis/nia-4-effects/osi/twang-diagnostics/balance-table.csv")

d_balance_table <-
  # readr::read_csv("../sda-fiesta/analysis/nia-4-effects/osi/twang-diagnostics/balance-table-career_planning.csv") %>% 
  readr::read_csv("./analysis/nia-4-effects/osi/twang-diagnostics/balance-table-career_planning.csv") %>% 
  relocate(intervention)

# ---- inspect-data ------------------------------------------------------------
d_balance_table %>% count(intervention) # for a given intervention variable (tx)
d_balance_table %>% count(method) # see more about methods in 

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



# ---- tweak-data --------------------------------
intervention_i <- "career_planning"
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

g %>% 
  quick_save(
    name = paste0(intervention_i,"-0-group-imbalance")
    ,width = 12
    ,height= 7
  )


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
