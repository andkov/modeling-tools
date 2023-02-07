
library(tidyverse)
library(tableone)

source("./scripts/graphing/graph-presets.R")
source("./analysis/sim-log-reg/support-functions.R")
getwd()
## Data

set.seed(42)
sample_size  <- 1000
rate_outcome <- .2
rate_male    <- .5
effect_male  <- .3 # proportion of outcome==FALSE to be recoded to TRUE

d <- 
  tibble::tibble(id = 1:sample_size) %>% 
  mutate(
    outcome = case_when(runif(nrow(.)) <= rate_outcome ~ TRUE,TRUE ~ FALSE) 
    ,sex = case_when(
      runif(nrow(.)) < rate_male ~ "Male",TRUE ~ "Female"
    )
    ,outcome = case_when(
      outcome==FALSE & sex=="Male" & (runif(sample_size) < effect_male) ~ TRUE,
      TRUE ~ outcome
    )
  )
d %>% head() %>% print()

d %>% tableone::CreateTableOne(data=.)
d %>% tableone::CreateTableOne(data=.,strata="sex")
d %>% tableone::CreateTableOne(data=.,strata="outcome")

d %>% explore::explore(sex,target = outcome)
d %>% explore::explore(outcome,target = sex)

# From marginal distribution of variables we observe  that `24.2%`of cases have a positive outcome. We can think of it as a parameter of the intercept-only model:

# m0 <- glm(outcome ~ 1, data = d, family= binomial(link="log"))
m0 <- glm(outcome ~ 1, data = d, family= "binomial")
m0 %>% broom::tidy(exp=T)

It represents the probability with which a randomly selected case from this population will result in a positive outcome. We may be able to improve on this


m0 %>% broom::augment()
m0 %>% make_auc_graph()




m1 <- glm(outcome ~ 1 + sex, data = d, family= binomial(link="log"))
m1 %>% broom::tidy(exp=T)


m1 %>% get_rsquared()
m0 %>% get_rsquared()

m1 %>% broom::augment()
m1 %>% make_auc_graph()

# When fiting logistic regression on the same data (i.e. treating outcome as binary)
# our estimate is on the log-tranformed scale becuase `glm` tranforms the criterion
# using `ln(P/(1-P))` link function 
m0b <- glm(outcome ~ 1, data = d, family= 'binomial')
m0b %>% coef() 
# However, taking the exponent does not recover the expected value:
m0b %>% coef() %>% exp()

# This happens because the default link function for family="binomial" in `glm` 
# is  logit(`ln(P/(1-P))`) and not log (`ln()`) function. 
m0b_log <- glm(outcome ~ 1, data = d, family= binomial(link="log"))
m0b_log %>% coef() %>% exp()
# whereas
m0b_logit <- glm(outcome ~ 1, data = d, family= binomial(link="logit"))
m0b_logit %>% coef() %>% exp()                     # wrong!!
m0b_logit %>% coef() %>% LaplacesDemon::invlogit() # correct!!


