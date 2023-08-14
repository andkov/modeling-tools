library(tidyverse)
library(tableone)
source("./scripts/common-functions.R")
source("./scripts/graphing/graph-presets.R")
load("./analysis/ua-war-religion/toy-data.RData")


ds <- 
  filtered_data %>% 
  # integer indicator for the wave to ease some graphing
  mutate(waveL = case_when(wave =="wave1"~1L,TRUE ~ 2L) %>% as.integer())  # for geom_smooth to work
  
tibble(var_name = names(war_var_labels), var_value = war_var_labels)

ds %>% tableone::CreateTableOne(data=.)
ds %>% tableone::CreateTableOne(data=., strata = "wave")
ds %>% explore::describe_all()


# Show distribution of key outcome at both waves
# see more at http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
library(ggpubr)
p <- 
  ds %>% 
  mutate(wave = factor(wave,levels = c("wave1","wave2"))) %>% 
  ggboxplot(x = "wave", y = "religiosity",
               color = "wave",
               add = "jitter")
#  Add p-value
p + stat_compare_means()
# Change method
p + stat_compare_means(method = "t.test")


# compute raw equation to test hypotheses non-rigoroursly (no stat test of sign or adj of variance)
add_equation <- 
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse = TRUE
                        ,label.x = 0.1
                        ,label.y = 1.5) 

# Show the observed, person-level change
g1 <- 
  ds %>% 
  ggplot(aes(x=waveL, y = religiosity))+
  geom_point(alpha = .2)+
  geom_line(aes(group = key),alpha = .2)+
  geom_smooth(method = "lm")+
  add_equation +
  scale_x_continuous(breaks = c(1,2))
g1


# test the hypothesis visually (before adjusting for variance and measure stat significance)
g1 <- 
  ds %>% 
  ggplot(aes(x=waveL, y = religiosity, color = as_factor(affected_index_dummy)))+
  geom_point(alpha = .2)+
  geom_line(aes(group = key),alpha = .2)+
  geom_smooth(method = "lm")+
  add_equation+
  scale_x_continuous(breaks = c(1,2))
g1
# the variable `affected_index` appears to be wave-level, but should be person-level
ds %>% 
  filter( key %in% c(25070, 28593, 46674)) %>% 
  select(key, waveL, religiosity, affected_index, affected_index_dummy) %>% arrange(key)

g1 <- 
  ds %>% 
  ggplot(aes(x=waveL, y = c15, color = as_factor(affected_index_dummy))) +
  geom_line(aes(group = key))
g1


# Hypothesis 1:  Religiosity increases in wave 2
model_1 <- religiosity ~ wave 

# Hypothesis 2:  Religiosity increases differently for different intial religiosity
# initial_religiosity: high, med, low
model_2a <- religiosity ~ wave + initial_religiosity 
model_2b <- religiosity ~ wave + initial_religiosity + wave*initial_religisoty
# test the significant of the interaction term
anova(model_2a, model_2b)
# express the difference bw groupw with specific comarisons using emmeans:
# https://stats.stackexchange.com/questions/60352/comparing-levels-of-factors-after-a-glm-in-r





















