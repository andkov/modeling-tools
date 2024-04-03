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
library(stringr)   # strings
library(lubridate) # dates
library(labelled)  # labels
library(scales)    # format
library(dplyr)     # loading dplyr explicitly is my guilty pleasure
library(broom)     # for model
library(emmeans)   # for interpreting model results
library(magrittr)
# -- 2.Import only certain functions of a package into the search path.
import::from("magrittr", "%>%")
# -- 3. Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"    )# data import/export
requireNamespace("readxl"   )# data import/export
requireNamespace("tidyr"    )# tidy data
requireNamespace("janitor"  )# tidy data
requireNamespace("dplyr"    )# Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit"   )# For asserting conditions meet expected patterns.

# ---- load-sources ------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level

# ---- declare-globals ---------------------------------------------------------
# printed figures will go here:
prints_folder <- paste0("./analysis/phase-3-results/unbalanced/")
if(!file.exists(prints_folder)){dir.create(file.path(prints_folder))}

baseSize <- 10
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey95", color = NA)
      ,panel.grid = element_line(color = "grey95")
      ,panel.border = element_rect(color = "grey80")
      ,axis.ticks = element_blank()
      ,text=element_text(size=baseSize)
    )
)

# corporate colors
abcol <- c(
  "stone"     = "#5f6a72" # stone   - grey
  ,"dusk"     = "#d40072" # dusk    - magenta
  ,"sunset"   = "#ff7900" # sunset  - brown
  ,"pasture"  = "#77b800" # pasture - green
  ,"sky"      = "#00aad2" # sky     - blue
  ,"prairie"  = "#edb700" # prairie - yellow
)
# ---- declare-functions -------------------------------------------------------


# ---- load-data ---------------------------------------------------------------
ds0 <- readr::read_csv("./analysis/phase-3-results/data/2024-03-14/unbalanced-outcome-table.csv")
# ---- inspect-data-0 ------------------------------------------------------------

# ---- inspect-data-local -----------------------------------------------------
# this chunk is not sourced by the annotation layer, use a scratch pad
ds0 %>% 
  filter(waveL==0L) %>% 
  select(wk_combo, sample_size, earnings_mean, earnings_median,ntile20, ntile80) %>% 
  arrange(desc(sample_size))

# ---- tweak-data-1 --------------------------------------------------------------

ds_ref <- 
  ds0 %>% 
  filter(
    wk_combo == "__ + __ + __ + __"
  ) %>% 
  select(waveL, ref_median = earnings_median, ref80 = ntile80)

ds1 <- 
  ds0 %>% 
  left_join(ds_ref, by = "waveL")

# General
# - consider dropping the mean
# geom_text() with ymax = Inf and vjust = 1.1
# ---- g2 --------------------------------------------------------------

g2 <- 
  ds1 %>% 
  filter(waveL < 3L) %>% 
  mutate(
    tx_count = case_when(
      wk_combo %in% c(
        "__ + __ + __ + __"   
        ,"__ + cp + __ + __"  
        ,"wk + __ + __ + __"  
        ,"__ + __ + __ + jp"  
        ,"__ + __ + ec + __"  
        ,"__ + cp + __ + jp"  
        ,"wk + cp + __ + __"  
        ,"wk + __ + __ + jp"  
        ,"__ + __ + ec + jp"
      ) ~ TRUE, TRUE ~ FALSE
    ),
    sample_size_pretty = sprintf("%.1f",sample_size/1000)
  ) %>% 
  filter(tx_count) %>%
  ggplot(aes(x=waveL, group = wk_combo))+
  geom_ribbon(aes(ymin=ntile80, ymax=ntile90), fill = "#2c7bb6",alpha = .99)+
  geom_ribbon(aes(ymin=ntile70, ymax=ntile80), fill = "#abd9e9",alpha = .99)+
  geom_ribbon(aes(ymin=ntile60, ymax=ntile70), fill = "#ffffbf",alpha = .99)+
  geom_ribbon(aes(ymin=ntile50, ymax=ntile60), fill = "#fdae61",alpha = .99)+
  geom_ribbon(aes(ymin=ntile40, ymax=ntile50), fill = "#d7191c",alpha = .99)+
  geom_hline(yintercept = 0, color = "black", size = 1)+
  geom_line(aes(y=ref_median), size = 1.1, linetype = "22", color = "grey70")+
  geom_line(aes(y=ref80), size = 1.1, linetype = "42", color = "grey30")+
  # geom_text(aes(label=scales::comma(sample_size), y=Inf), vjust = 1.2)+
  geom_text(aes(label=sample_size_pretty, y=Inf), vjust = 1.2)+
  facet_wrap(facets = "wk_combo")+
  scale_y_continuous(labels = scales::comma_format(scale = .001, suffix = "K", prefix = "$"))+
  # scale_y_continuous(labels = scales::comma_format(scale = .001, prefix = "$"))+
  scale_x_continuous(breaks = seq(0,10,1))+
  theme(
    strip.text = element_text(size = 15)
  )+
  labs(
    title = "Point estimates of the unbalanced intervention groups"
    ,subtitle = "Means in black, medians in blue"
    ,x = "Year relative to the first Income Support spell"
    ,y = "Total earnings in 2022 dollars"
    ,color = "Workshop\nCombination"
    ,fill = "Workshop\nCombination"
  )
g2 
g2 %>% quick_save("test2",w=9, h=9)

# ---- g1 --------------------------------------------------------------

# 1. Repeat the median line of the null category in each facet
# 2. Use geom_ribbon( with )

g1 <- 
  ds0 %>% 
  ggplot(aes(x=waveL, y = earnings_mean, group = wk_combo))+
  geom_ribbon(aes(ymin=ntile40, ymax=ntile60), fill = "green",alpha = .3)+
  geom_ribbon(aes(ymin=ntile80, ymax=ntile90), fill = "blue",alpha = .3)+
  geom_point()+
  geom_line()+
  geom_point(aes(y= earnings_median), color = "blue")+
  geom_line(aes(y = earnings_median), color = "blue")+
  # geom_line(aes(y = ntile40), color = "red")+
  facet_wrap(facets = "wk_combo")+
  scale_y_continuous(labels = scales::comma_format())+
  scale_x_continuous(breaks = seq(0,10,1))+
  theme(
    strip.text = element_text(size = 15)
  )+
  labs(
    title = "Point estimates of the unbalanced intervention groups"
    ,subtitle = "Means in black, medians in blue"
    ,x = "Year relative to the first Income Support spell"
    ,y = "Total earnings in 2022 dollars"
    ,color = "Workshop\nCombination"
    ,fill = "Workshop\nCombination"
  )
g1 





# ---- save-to-disk ------------------------------------------------------------

