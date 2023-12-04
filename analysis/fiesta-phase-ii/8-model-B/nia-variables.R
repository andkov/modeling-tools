
design_labels <- c(
  "person_oid"      = "Unique identifier"
  ,"date_start"     = "Starting date of the first Income Support spell"
  ,"date_end"       = "Ending date of the first Income Support spell"
  ,"tax_year"       = "Year of filing the taxes to the CRA"
  ,"waveF"          = "Before or After Income Support Spell"
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
  ,"fy_is_start"        = "Fiscal Year in which IS started"
  ,"outcome_before"     = "Value of the outcome in the BEFORE time point"
  ,"years_btw_before_after_cat"     = "Number of years between BEFORE and AFTER waves"  # person-level
  # ,"tax_year_before" = "Tax year prior to enrolling in IS, filed to CRA"
  # ,"years_since_before_cat"     = "Number of years between BEFORE and current waves"  # time-variant!!!
  
)

covariates <- covariates_labels %>% names()


intervention_impact <- c(
  # compute the impact of each, while balancing on all others:
  "training_for_work"   =  "Training for Work" 
  ,"work_foundations"    =  "Work Foundations" 
  ,"career_planning"     =  "Career Planning"   
  ,"cover_letters"       =  "Cover Letters"     
  ,"exposure_course"     =  "Exposure Course"   
  ,"interview_skills"    =  "Interview Skills"  
  ,"job_placement"       =  "Job Placement"     
  ,"job_search"          =  "Job Search"        
  ,"labour_market_info"  =  "Labour Market Information"
  ,"resume_writing"      =  "Resume Writing"    
  ,"self_assessment"     =  "Self Assessment"     
)
intervention_balance <- c(
  # Balance on but do NOT compute the impact of
   "assessment_ea"       =  "Employability"     
  # ,"assessment_ni"       =  "Needs Identification"     # usually turned off due to small sample
  ,"assessment_snd"      =  "Service Needs Determination"    
  ,"workshop_other"      =  "Workshop(Other)"               
)

intervention_labels <- c(intervention_impact,intervention_balance )
intervention <- intervention_labels %>%  names()
