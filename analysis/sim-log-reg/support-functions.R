
make_auc_graph <- function(model){
  
  binary_colors <- c(
    "TRUE"     = "#f1a340" # orange
      ,"FALSE" = "#998ec3" # purple
  )
  # model <- m0
  d1 <-
    model %>%
    broom::augment()
  outcome_name <- names(d1 %>% select(1))
  d2 <- 
    d1 %>% 
    mutate(
      probability = predict(object = model, newdata = .,type = "response")
    ) 
  roc_obj   <- pROC::roc(d2[[outcome_name]], d2[["probability"]])
  auc_value <- pROC::auc(roc_obj) %>% as.numeric()
  g_roc <-  
    roc_obj %>%
    pROC::ggroc(color = "red")+
    # geom_abline(slope = 1, intercept = 1, color = "black")+
    geom_abline(slope = 1, intercept = 1, color = binary_colors["FALSE"])+
    geom_text(
      aes(
        label = paste0("AUC = ",auc_value %>% numformat(3))
        , x = .25
        , y = .25
      )
      ,color =  binary_colors["TRUE"]
    )
  return(g_roc)
  
}

get_rsquared <- function(m){
  cat("R-Squared, Proportion of Variance Explained = ",
      scales::percent((1 - (summary(m)$deviance/summary(m)$null.deviance)),accuracy = .01)
      , "\n")
}

get_model_fit <- function(m, print=T){
  mfit <- list(
    "chisquare" =  with(m, null.deviance - deviance)
    ,"df"       = with(m, df.null - df.residual)
    ,"pvalue"   = with(m, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    ,"aic"      = m$aic
    ,"rsquare"  = (1 - (summary(m)$deviance/summary(m)$null.deviance)) # variance explained
  )
  if(print){
    cat("MODEL FIT",
        "\nChi-Square = ", mfit$chisquare,
        "\ndf = ", mfit$df,
        "\np-value = ", mfit$pvalue,"\n"
    )
  }
  
  return(mfit)
}
model_comparison_test <- function(reduced,full){
  
  full_model_spec    <- explanatory
  reduced_model_spec <- explanatory[2:length(explanatory)]
  model_comparison   <- list("full" =full_model_spec , "reduced" = reduced_model_spec)
  ls_out <- list()
  ls_model <- list(
    "reduced"  = d %>% run_logistic_binary(dependent, reduced_model_spec)
    ,"full"    = d %>% run_logistic_binary(dependent, full_model_spec)
  )
  
  # Model comparison test
  chi_square_diff <- ls_model$full$model_fit$chisquare - ls_model$reduced$model_fit$chisquare
  df_diff         <- ls_model$full$model_fit$df - ls_model$reduced$model_fit$df
  pvalue          <- pchisq(chi_square_diff, df_diff, lower.tail = FALSE)
  pvalue_pretty   <- paste0("<= ", numformat(pvalue, decimal_count = 3))
  r_squared_full  <- ls_model$full$model_fit$rsquare
  r_squared_diff  <- (r_squared_full - ls_model$reduced$model_fit$rsquare)
  chi_squared_diff_test <- paste0(
    "Improvement over Reduced Model: "
    ,"Chi-Square (df = ",df_diff,") = ",scales::comma(chi_square_diff, accuracy = .1)
    # ,", df = ", df_diff,
    ,", p-value ", pvalue_pretty
    ,", R-Square  = ", numformat(r_squared_full, 3),", gained ",
    numformat(r_squared_diff,3)
  )
  
  # Make model comparison table
  t_reduced <- ls_model$reduced$model %>% gtsummary::tbl_regression(exponentiate=T)
  t_full    <- ls_model$full$model %>% tbl_regression(exponentiate=T)
  t_out <-
    gtsummary::tbl_merge(
      # tbls = list(t_reduced, t_full)
      tbls = list(t_full, t_reduced)
      ,tab_spanner = c("Full", "Reduced")
    ) %>%
    gtsummary::modify_caption(chi_squared_diff_test)
  ls_model[["compare"]] <- list(
    "test" = chi_squared_diff_test
    ,"table" = t_out
  )
}