
library(tidyverse)
library(tableone)
library(broom)
library(broom.helpers)
library(LaplacesDemon)

source("./scripts/graphing/graph-presets.R")
source("./analysis/sim-log-reg/support-functions.R")
getwd()

## Log vs Logit reminder
d1 <-
  tibble(prob = seq(0,1,.01)) %>% 
  mutate(
    log = log(prob),
    logit = log(prob/(1-prob)),
    `exp(log)` = exp(log),
    `plogis(logit)` = plogis(logit),
    # MISTAKE !!!
    `exp(logit)WRONG` = exp(logit),
    `plogis(log)WRONG` = plogis(log),
  ) 
d1 %>% filter(prob %in% seq(0,1,.1))

d1 %>% 
  ggplot(aes(x=prob))+
  geom_line(aes(y = log), color = 'red')+
  geom_line(aes(y = logit), color = 'blue')+
  geom_text(label="logit",x=.85, y = 3.5, color  = "blue")+
  geom_text(label="log",x=.95, y = -1.5, color  = "red")+
  labs(x = "Observed probability", y = 'Log transform')


## Simulate Data

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

d %>% tableone::CreateTableOne(data=.) # %TRUE   32.6 
d %>% tableone::CreateTableOne(data=.,strata="sex") # 21% of female are TRUE, 45% male
d %>% tableone::CreateTableOne(data=.,strata="outcome") # 41% of TRUE are female; 68% male
# 
d %>% explore::explore(sex,target = outcome)
d %>% explore::explore(outcome,target = sex)

# From marginal distribution of variables we observe  that
#  `32.6`of cases have a positive outcome. We can think of it as a parameter of the intercept-only model:
# It represents the probability with which a randomly selected case from this population will result in a positive outcome. We may be able to improve on this
m0_default <- glm(outcome ~ 1, data = d, family= binomial)               # same as below
m0_logit   <- glm(outcome ~ 1, data = d, family= binomial(link="logit")) # same as above
m0_log     <- glm(outcome ~ 1, data = d, family= binomial(link="log"))

d %>% tableone::CreateTableOne(data=.) # Observed mean of TRUE = 32.6 

# when using  LOGIT link function
m0_logit %>% print() # raw coefficient is on the logit scale
# to transform back to the original scale (probability), must use `plogis()``
coef(m0_logit)[1] %>% plogis()  # to re-scale manually
coef(m0_logit)[1] %>% LaplacesDemon::invlogit()  # to re-scale manually
m0_logit %>% broom::tidy(exp=F) # default , leaves coefficient on original scale = logit(prop)
m0_logit %>% broom::tidy(exp=T) # or exp(coef(m0_logit)[1]); INCORRECT re-scaling
# Important: when using defaults (family = binomial), must NOT use broom::tidy(exp=T)
m0_logit %>% coef() %>% exp()                     # wrong!!
m0_logit %>% coef() %>% LaplacesDemon::invlogit() # correct!!
m0_logit %>% coef() %>% plogis() # correct!!

# when using  LOG link function
coef(m0_log)[1] %>% exp()  # to re-scale manually
m0_log   %>% broom::tidy(exp=F) # leaves coefficient on original scale = log(prob)
m0_log   %>% broom::tidy(exp=T) # re-scales the estimate correctly


# adding a covariate

m1_default <- glm(outcome ~ 1 + sex, data = d, family= binomial)
m1_logit <- glm(outcome ~ 1 + sex, data = d, family= binomial(link="logit"))
m1_log   <- glm(outcome ~ 1 + sex, data = d, family= binomial(link="log"))


# When using link="log', re-scaling of the coefficients is straightforward
m1_log %>% broom::tidy(exp=T)
# But to  interpret the coefficients we must add them before re-scaling
# Thus the expected value of sex='Male' will be calculated as
m1_logit %>% broom::tidy(exp=F) # raw, on logit scale
m1_logit %>% broom::tidy(exp=F) %>% 
  mutate(
    effect_of_Male = sum(estimate) %>% plogis() 
    # see Gelmaan and Hill, page 89
    # Note: the means of calculation is specific to a single binary predictor model
    # models with multiple parameters require a more general solution (emmeans)
  ) %>% # we"ll take pvalue from predictor until learn how to emmeans
  filter(term == "sexMale")
m1_default %>% broom::tidy(exp=T)
m1_logit %>% broom::tidy(exp=T)


m1_logit
EMM.emmeans <- emmeans::emmeans(m1_logit,by = "sex",specs = "outcome")


m1_logit %>% coef()

m1 <- glm(outcome ~ 1 + sex, data = d, family= binomial(link="log"))
m1_logit <- glm(outcome ~ 1 + sex, data = d, family= binomial(link="logit"))
m1_log   <- glm(outcome ~ 1 + sex, data = d, family= binomial(link="log"))
m1 %>% broom::tidy(exp=T)


m1_logit %>% get_rsquared()
m1_log %>% get_rsquared()
m0 %>% get_rsquared()

m1 %>% broom::augment()
m1 %>% make_auc_graph()

# When fiting logistic regression on the same data (i.e. treating outcome as binary)
# our estimate is on the log-transformed scale because `glm` transforms the criterion
# using `ln(P/(1-P))` link function (logit) (not log()!!!)
m0_default <- glm(outcome ~ 1, data = d, family= 'binomial')
m0_default %>% coef() 
# However, taking the exponent does not recover the expected value:
m0_default %>% coef() %>% exp()

# This happens because the default link function for family="binomial" in `glm` 
# is  logit(`ln(P/(1-P))`) and not log (`ln()`) function. 
m0_log <- glm(outcome ~ 1, data = d, family= binomial(link="log"))
m0_log %>% coef() %>% exp()
# whereas
m0_logit <- glm(outcome ~ 1, data = d, family= binomial(link="logit"))
m0_logit %>% coef() %>% exp()                     # wrong!!
m0_logit %>% coef() %>% LaplacesDemon::invlogit() # correct!!


