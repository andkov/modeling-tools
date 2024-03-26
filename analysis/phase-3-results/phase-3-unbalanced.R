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
  select(wk_combo, sample_size) %>% 
  arrange(desc(sample_size))
# ---- tweak-data-1 --------------------------------------------------------------


g1 <- 
  ds0 %>% 
  ggplot(aes(x=waveL, y = earnings_mean, group = wk_combo))+
  geom_point()+
  geom_line()+
  
  geom_point(aes(y= earnings_median), color = "blue")+
  geom_line(aes(y = earnings_median), color = "blue")+
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
g1 %>% quick_save("1-raw-means-facet",w=9, h=7)
g1 %>% print()



# ---- save-to-disk ------------------------------------------------------------

