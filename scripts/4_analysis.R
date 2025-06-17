
### Post-Simulation Analysis for LCrisks Model Arms ###

rm(list = ls())

getwd()
setwd("/Users/al8xi8/Documents/GitHub/lcrisks_simulation")


# --- Load Libraries and Packages ---
library(readr)
library(dplyr)
library(tidyr)
install.packages("janitor")
library(janitor)

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
  
  # Core variable names
  noscrn_core <- c(
  "Sex", "Lung Cancer Age", "Lung Cancer Death Age",
  "Other Cause Mortatlity Age", "Overall Death Age",
  "Histology", "Stage", "Treatment Burden", "LC risk at death")

  # Followed by PST, PKY, YSQ
  pst <- paste0("PST ", 1:6)
  pky <- paste0("PKY ", 0:100)
  ysq <- paste0("YSQ ", 0:100)

  noscrn_colnames <- c(noscrn_core, pst, pky, ysq)
  colnames(noscrn) <- noscrn_colnames

  
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

  # Core variable names
  scrn_colnames <- c(
  "Sex", "Lung Cancer Age", "Lung Cancer Death Age", "Other Cause Mortatlity Age",
  "Overall Death Age", "Histology", "Stage", "Place Holder", "Treatment Burden",
  "Eligible", "Overdiagnosis", "Detected", "Survival Time",
  "Diagnostic Death from Screening", "Quit Smoking",
  "Eligible for Cessation Program", "Entry Age")

  colnames(scrn)[1:17] <- scrn_colnames

--------------------------------------------------------------------------------

 # Table 2 #

  
 # Table 5 #