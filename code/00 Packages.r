### PACKAGES ###
  
  rm(list = ls())
  
  library(arrow)
  library(dplyr)
  library(eeptools)
  library(extrafont)
  library(formattable)
  library(ggplot2)
  library(ggpubr)
  library(ggthemes)
  library(haven)
  library(hrbrthemes)
  library(knitr)
  library(lfe)
  library(lmtest)
  library(lubridate)
  library(readxl)
  library(sandwich)
  library(stargazer)
  library(tictoc)
  library(tidyverse)
  
  extrafont::font_import(pattern = "Candara")
  loadfonts()
  
  setwd("C:/Users/osgil/OneDrive/Documents/Oeconomica - IO - Case Study")
  
  # Folder names
  FOLDER <- function(x) {file.path(paste(getwd(), x, sep = "/"))}
  
  # Run R files
  source(FOLDER("01 Functions.r"))
  # source(FOLDER("02 Creating data.r"))
  # source(FOLDER("03 Separating out datasets.r"))