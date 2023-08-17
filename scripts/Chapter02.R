
###
# Chapter 2
###

library(tidyverse)
library(GGally)
library(cowplot)
source("../00_HelperFunctions_20230624.R")

### Set up directory structure ====

make_new_dir <- 
  function(DIR_TO_MAKE){
    if(dir.exists(DIR_TO_MAKE) == FALSE){
      dir.create(DIR_TO_MAKE)
    }else{
      print("Directory exists")
    }
  }

make_new_dir("./data/")
make_new_dir("./data/input")
make_new_dir("./data/output")

### Read in the data ====
download.file(url = "https://www.statlearning.com/s/College.csv",
              destfile = "./data/input/College.csv")

college <- read.csv("./data/input/College.csv")

college %>% rownames()

college %>% glimpse()

college %>% summary()

college %>% 
  select(-X) %>%
  select(1:10) %>%
  ggpairs()

college %>%
  ggplot(aes(x = Private, y = Outstate)) +
    geom_boxplot()

college %>%
  mutate(Elite = case_when(
    Top10perc > 50 ~ "Yes",
    Top10perc <= 50 ~ "No",
    TRUE ~ "ERROR"
  )) %>%
  group_by(Elite) %>%
  tally()


college_histograms <- function(VAR, num_bins = 30){
  college %>%
    ggplot(aes({{VAR}})) +
    geom_histogram(bins = num_bins)
}

plot_grid(
  college_histograms(Apps, 100),
  college_histograms(PhD, 10),
  college_histograms(Top10perc, 50),
  college_histograms(Grad.Rate, 30)
)




