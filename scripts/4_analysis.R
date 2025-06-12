
### Post-Simulation Analysis for LCrisks Model Arms ###

rm(list = ls())

getwd()
setwd("/Users/al8xi8/Documents/GitHub/lcrisks_simulation")


# --- Load Libraries and Packages ---
library(readr)
library(dplyr)
library(tidyr)


# --- Load *No Screening* Data ---
noscrn_path <- "results/output_noscrn/"

noscrn_files <- list.files(
  path = noscrn_path,
  pattern = "^no_scrn_\\d{1,3}\\.csv$",
  full.names = TRUE)

noscrn <- bind_rows(
  lapply(noscrn_files, function(file) {
    df <- read_csv(file, col_names = FALSE)
    df$file_id <- basename(file)  # optional: keep filename
    return(df)}))

# --- Load *Screening* Data ---
scrn_path <- "results/output_scrn/"

scrn_files <- list.files(
  path = scrn_path,
  pattern = "^scrn_tf111_100_50_80_20_15_100_0_\\d{1,3}\\.csv$",
  full.names = TRUE)

scrn <- bind_rows(
  lapply(scrn_files, function(file) {
    df <- read_csv(file, col_names = FALSE)
    df$file_id <- basename(file)
    return(df)}))


--------------------------------------------------------------------------------

 # Table 2 #

  
 # Table 5 #