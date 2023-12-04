

sample_of_interest1 <-  4187213

# ----- compute-effect-functions ------------------------
# sandbox for re-writing the function


compute_effect_on_continuous<- function(
    data_raw# = ds5B # in which effects must be estimated, same that passed to twang
    ,ps_meta# = ls_meta # meta object with derived weights for a given intervention (all computed interventions)
    ,outcome_name #= "earnings_total"
    ,treatment_name #= "career_planning"
    ,covariate_names = NULL
    # ,covariate_names = c("sex2","age_in_years","marital2")
    ,out_table = "predict" # outputs table of predicted value for selected levels
){
  # browser()
  
  # data_raw = ds5B # in which effects must be estimated
  # ps_meta = ls_meta # meta object with derived weights for a given intervention (all computed interventions)
  # outcome_name = "earnings_total"
  # treatment_name = "career_planning"
  # # ,covariate_names = NULL
  # covariate_names = c("sex2","age_in_years","marital2","dependent2","education4")
  # out_table = "predict" # outputs table of predicted value for selected levels
  # # 
  
  # ps_object <- ps1
  # focal_outcome = "earnings_total"
  # treatment_name = "tx"
  # covariate_names = vars_to_balance_on
  
  # ps_object <- ls_ps[[focal_intervention]]
  # input_data <- ds5B
  setdiff(names(ds5B),covariate_names)
  setdiff(covariate_names,names(ds5B))
  
  # propensity score weight relative to this intervention
  # has already been computed and stored in ps_meta object, specific to the intervention
  d_nia_w <- 
    data_raw %>% # passed to twang to estimate weights for this intervention
    left_join(
      ps_meta[[treatment_name]][["w"]] 
    )
  
  design.ps <- survey::svydesign(ids= ~1, weights = ~w, data = d_nia_w)

  if(is.null(covariate_names)){
    
    model_equation <- as.formula(paste0(outcome_name, "~ tx*waveF"))
  
    # emmeans
    eq_emmeans <- "~ tx * waveF"  %>% as.formula() # EMM equation
    
  }else{
    
    model_equation <- as.formula(paste0(outcome_name, "~", 
                                        paste0("tx*waveF*",covariate_names, collapse = " + ")))
    # emmeans
    # eq_emmeans <- "~ tx * waveL * sex2 * age_in_years * dependent2 * marital2"  %>% as.formula() # EMM equation
    # we only specify the covariates that we'll test out, while holding other covariates fixed at some level
    # emmeans_vars <- intersect(c("sex2","age_in_years","dependent2","marital2"), covariate_names)
    emmeans_vars <- c("sex2","age_in_years","dependent2","marital2", covariate_names) %>% unique()
    eq_emmeans <- paste0("~ tx * waveF *", paste0(emmeans_vars, collapse = "*" ))  %>% as.formula() # EMM equation
  }
  
  model <- survey::svyglm(model_equation, design = design.ps, family = "gaussian") %>% suppressWarnings()
  # summary(model)
  # tidy(model)

  e <- 
    model %>%  
    emmeans::emmeans(
      specs =   eq_emmeans# try adding pairwise before ~
      ,at   = list(
        # fix the levels of covariates 
        # if you use a predictor in the model equation, but don't specify the level here => all possible levels are computed
        tx                   = c(FALSE, TRUE)
        ,waveF               = c("Before","After","After+1","After+2","After+3","After+4")
        ,sex2                = c("Men", "Women")
        ,age_in_years        = c(20, 30, 40, 50, 60)
        ,marital2            = c("married", "single")
        ,dependent2          = c("0 dependents", "1+ dependents")
        ,education4          = c("High School")
        ,disability2         = c("No Disability")
        ,ethnicity           = c("Caucasian")
        ,immigration         = c("born in Canada")
        ,region7             = c("Edmonton")
        ,spell_duration_cat  = c("2-3 months")
        ,years_btw_before_after_cat = c("2")
        ,fy_is_start         = c("2017")
        ,assessment_ea       = c(0)
        ,assessment_snd      = c(0)
        ,assessment_ni       = c(0)
        ,training_for_work   = c(0)
        ,work_foundations    = c(0)
        ,career_planning     = c(0)
        ,cover_letters       = c(0)
        ,exposure_course     = c(0)
        ,interview_skills    = c(0)
        ,job_placement       = c(0)
        ,job_search          = c(0)
        ,labour_market_info  = c(0)
        ,resume_writing      = c(0)
        ,self_assessment     = c(0)
        ,workshop_other      = c(0)
      ) # custom points to evaluate
    )
  e
  

  
  # raw counts
  # d_outcome_n <- 
  #   d_nia_w %>%
  #   # group_by(waveF,tx) %>%
  #   group_by_at(all_of(c("waveF","tx",covariate_names))) %>% 
  #   summarize(
  #     outcome_n = sum(!is.na(!!rlang::sym(outcome_name))) # non-missing values on the outcome
  #     ,.groups = "drop"
  #   ) %>% 
  #   mutate(
  #     intervention = treatment_name,
  #   )
  #   # filter(waveF=="Before",sex2=="Men",age_in_years==19,marital2=="single") %>% # dev + test
  #   # tidyr::pivot_wider(names_from = "tx", names_prefix = "tx_", values_from = "outcome_n") %>%
  #   # mutate(
  #   #   outcome_n = tx_FALSE + tx_TRUE
  #   # )
  # d_outcome_n
  
  
  
  # Create data set containing model predictions for conditions specified by `eq_emmeans` 
  hat_name <- "emmean" # Gaussian output from emmeans (as opposed to `fitted` from broom)
  # hat_name <- "prob" # logistic output from emmeans (as opposed to `fitted` from broom)
  # hat_name <- "rate" # Poisson output from emmeans (as opposed to `fitted` from broom)
  d_predict <-
    seq_len(nrow(e@linfct)) %>%                         # notice emmeans object `e`!
    purrr::map_dfr(function(i) as.data.frame(e[i])) %>% # notice emmeans object `e`!
    dplyr::mutate( #  now tweak for graphing
      outcome = outcome_name
      ,intervention = treatment_name
      # ,wave=factor(waveF,c(0,1),c("Before","After"))
      # ,age_in_years = factor(age_in_years)
      ,waveL = as.integer(waveF)-1
    ) %>% as_tibble() %>% 
    # left_join(d_outcome_n) %>% 
    dplyr::select( 
      waveL,
      waveF,
      tx,
      intervention,
      outcome,
      # outcome_n,
      # rename on the fly and remind what we're bringing from emmeans object
      y_hat       = !!rlang::ensym(hat_name), # the outcome, modeled values
      se          = SE,
      ci_lower    = lower.CL,  #asymp.UCL
      ci_upper    = upper.CL, #asymp.UCL
      df,
      everything()
      
    )# %>% as_tibble() 
  d_predict
  
  # compute expected difference after the IS
  if(out_table=="predict"){
    d_out <- d_predict
  }
  
  return(d_out)
}
# How to use
# ps1

# d_result <- 
#   compute_effect_on_continuous(
#     ps_object = ps1 # object from ps() model
#     ,outcome_name = "earnings_total"
#     ,covariate_names = NULL
#     ,treatment_name = "tx"
#     ,out_table = "predict" 
#     ,d_nia_input = d_nia# tibble used in the ps model
#   )
# 
# d_result <- 
#   compute_effect_on_continuous(
#     ps_object = ps1 # object from ps() model
#     ,outcome_name = "earnings_total"
#     ,covariate_names = c("sex2","age_in_years", "dependent2")
#     ,treatment_name = "tx"
#     ,out_table = "predict" 
#     ,d_nia_input = d_nia# tibble used in the ps model
#   )
# ps1 %>% compute_effect_on_binary("return12m")
# ps1 %>% compute_effect_on_continuous("spell_duration")

# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "nia")
# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = NULL,out_table = "nia")
# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "est") %>% print_all()
# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "fitted")
# m <- ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "model")

# ps1 %>% compute_effect_on_continuous("income_net_delta",    covariate_names = covariates,out_table = "plot")
# g <- (m %>% GGally::ggcoef_model())
# g %>% quick_save("test-model-coef",h=12,w=10)



compute_effect_on_binary <- function(
    ps_object # object from ps() model
    ,outcome_name = "return12m"
    ,treatment_name = "tx"
    ,covariate_names = NULL # only NULL for now, see comment about error when adding predictors
    ,out_table = "nia" # nia,  est, fitted, plot
){
  # browser
  # ps_object <- ps1
  # outcome_name = "return12m"
  # treatment_name = "tx"
  # covariate_names = c("fy_is_start")
  d <-
    ps_object$data %>%
    mutate(
      w =  get.weights(ps_object,stop.method = "es.mean", estimand = "ATT" )
    ) %>%
    as_tibble()
  # d %>% glimpse()
  design.ps <- survey::svydesign(ids= ~1, weights = ~w, data = d)
  # d %>% group_by(tx) %>% count()
  
  outcome_n <- d %>%
    group_by(tx) %>%
    summarize(
      outcome_n = sum(!is.na(!!rlang::sym(outcome_name)))
    )
  
  sum(!is.na(d[outcome_name])) # total sample size
  
  if(is.null(covariate_names)){
    
    model_equation <- as.formula(paste0(outcome_name, "~", treatment_name))
    pattern_starts_with_explanatory <- paste0("^","tx", collapse = "|")
    
  }else{
    
    model_equation <- as.formula(paste0(outcome_name, "~", treatment_name," + ",paste0(covariate_names, collapse = " + ")))
    pattern_dud <- paste0(c("tx",covariate_names),collapse="|")
    pattern_starts_with_explanatory <- paste0("^",pattern_dud, collapse = "|")
    
  }
  
  
  model <- survey::svyglm(model_equation, design = design.ps, family = binomial(link="logit")) %>% suppressWarnings()
  # model <- survey::svyglm(model_equation, design = design.ps, family = binomial(link="log")) %>% suppressWarnings()
  # model <- survey::svyglm(model_equation, design = design.ps, family = binomial(link="log")) %>% suppressWarnings()
  # model <- survey::svyglm(model_equation, design = design.ps, family = binomial(link="log"),start=c(.5,.5)) %>% suppressWarnings()
  # adding preditor cause the following error:
  # Error: no valid set of coefficients has been found: please supply starting values
  
  # summary(model)
  # moddel %>% 
  #   broom::augment(type.predict="response")
  # > qlogis(0.2151773 )
  # [1] -1.293995
  # > plogis(-1.293996)
  # [1] 0.2151772
  # 
  # When the link is logit (default for binomial), we need to exponentiation the
  # coefficient to obtain odds-ratio
  # Note, however, that when having covariates in addition to the treatment.
  # one must ADD THE EFFECTS before exponentiation
  
  d_estimates  <-
    model %>% 
    broom::tidy(
      conf.int = TRUE
      ,exp     = FALSE # use exp=FALSE when link="logit" (default when familiy="binomial")
      # ,exp     = TRUE # when link is "log", converts log-odds into odds-ratios (i.e. =exp(estimate)) !!! WRONG to use with link="logit"
      # when adding effect, add first then use plogis() function to re-scales (see Gelman & Hill, p.89)
    ) %>% 
    mutate(
      estimate_tx = sum(estimate) %>% plogis() 
      # see Gelmaan and Hill, page 89
      # Note: the means of calculation is specific to a single binary predictor model
      # !!! models with multiple parameters require a more general solution (emmeans) !!!
      # ,prob_outcome = estimate %>% plogis() # and now re-scale the estimates so you have a meaningful intercept
      ,prob_outcome = case_when(
        term == "(Intercept)" ~ estimate %>% plogis(),
        term == "txTRUE" ~ estimate_tx
      )
    )%>% 
    select(-estimate_tx)
  # Note that the txTRUE term becomes uninterpretable
  
  d_estimates
  
  d_net <-
    d %>% # raw data with added weights from twang::ps() balancing procedure
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
          # ,net_impact = wgt_diff
          ,mean_impact = wgt_diff
          
        )
      ,
      d_estimates %>%
        mutate(
          tx = case_when(
            term == "(Intercept)" ~ FALSE,
            term == "txTRUE" ~ TRUE
          )
        )
      
      # !!! Note that the above is a temp solution, before emmean are implemented
      # !!! provide p.value evaluates the txTRUE term, NOT the expected value
      ,by = "tx"
    )
  # add sample size calculation
  d_result <-
    d_result %>%
    left_join(outcome_n,by = "tx")%>%
    mutate(
      outcome = outcome_name
    ) %>%
    select(outcome,outcome_n,tx, everything()) 
  
  
  # d_fitted <- 
  #   model %>% 
  #   broom::augment(exp=F) %>% 
  #   group_by_at(all_of(c("tx",covariate_names))) %>%
  #   # group_by_at(all_of(c(covariate_names))) %>%
  #   mutate(
  #     person_count = n()
  #   ) %>% 
  #   ungroup() %>% 
  #   select(all_of(c("tx",covariate_names,".fitted","person_count"))) %>% 
  #   distinct() %>% 
  #   arrange(desc(person_count)) %>% 
  #   rename(
  #     predicted = `.fitted`
  #   ) %>% 
  #   mutate(
  #     outcome = outcome_name
  #     ,predicted = as.numeric(predicted)
  #   ) %>% relocate(outcome)
  # 
  # model_plot <- model %>% GGally::ggcoef_model(exponentiate = FALSE)
  
  if(out_table=="nia"){
    d_out <- d_result
  }
  
  if(out_table=="est"){
    d_out <- d_estimates
  }
  
  # if(out_table == "fitted"){
  #   d_out <- d_fitted
  # }
  # 
  # if(out_table == "plot"){
  #   d_out <- model_plot
  # }
  
  return(d_out)
  
  return(d_result)
}
