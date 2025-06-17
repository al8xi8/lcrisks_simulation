
### Post-Simulation Analysis for LCrisks Model Arms ###

rm(list = ls())

getwd()
setwd("/Users/al8xi8/Documents/GitHub/lcrisks_simulation")


# --- Load Libraries and Packages ---
library(readr)
library(dplyr)
library(tidyr)
if (!require("janitor")) install.packages("janitor")
library(janitor)
library(readr)

# --- Load Post-Imputation + Weight-Expanded Dataset ---
expanded_data <- read_csv("datasets/expanded_data.csv")

  # Assign row_id
  expanded_data$row_id <- 1:nrow(expanded_data)

  # Define comorbidity columns
  comorb_cols <- c("prshist", "hypertension", "chd", "angina", "heartattack",
                   "heartdisease", "stroke", "diab", "bron", "kidney", "liver", "spaceq")

  
### --- Load *No Screening* Data --- ###
  noscrn_path <- "results/output_noscrn/"
  noscrn_files <- list.files(
    path = noscrn_path,
    pattern = "^no_scrn_\\d{1,3}\\.csv$",
    full.names = TRUE
  )
  
  # Sort numerically to match expanded_data row order
  noscrn_files <- noscrn_files[order(as.numeric(gsub("[^0-9]", "", basename(noscrn_files))))]
  
  # Read in and skip first row (blank), add file_id column
  noscrn <- bind_rows(
    lapply(noscrn_files, function(file) {
      df <- read_csv(file, col_names = FALSE, skip = 1)
      df$file_id <- basename(file)
      return(df)
    })
  )
  
  # Core variable names
  noscrn_core <- c(
    "Sex", "Lung Cancer Age", "Lung Cancer Death Age",
    "Other Cause Mortatlity Age", "Overall Death Age",
    "Histology", "Stage", "Treatment Burden", "LC risk at death"
  )
  
  # Add PST, PKY, YSQ
  pst <- paste0("PST ", 1:6)
  pky <- paste0("PKY ", 0:100)
  ysq <- paste0("YSQ ", 0:100)
  noscrn_colnames <- c(noscrn_core, pst, pky, ysq)
  
  # Assign column names
  colnames(noscrn)[1:length(noscrn_colnames)] <- noscrn_colnames
  
  # Remove unnamed columns (e.g. file_id or blank col at end)
  noscrn <- noscrn[, !is.na(names(noscrn)) & names(noscrn) != ""]
  
  # Assign row_id
  noscrn$row_id <- 1:nrow(noscrn)
  
  
### --- Load *Screening* Data --- ###
  scrn_path <- "results/output_scrn/"
  scrn_files <- list.files(
    path = scrn_path,
    pattern = "^scrn_tf111_100_50_80_20_15_100_0_\\d{1,3}\\.csv$",
    full.names = TRUE
  )
  
  # Sort numerically
  scrn_files <- scrn_files[order(as.numeric(gsub("[^0-9]", "", basename(scrn_files))))]
  
  # Read in and skip first row
  scrn <- bind_rows(
    lapply(scrn_files, function(file) {
      df <- read_csv(file, col_names = FALSE, skip = 1)
      df$file_id <- basename(file)
      return(df)
    })
  )
  
  # Screening variable names (first 17 columns)
  scrn_colnames <- c(
    "Sex", "Lung Cancer Age", "Lung Cancer Death Age",
    "Other Cause Mortatlity Age", "Overall Death Age",
    "Histology", "Stage", "Place Holder", "Treatment Burden",
    "Eligible", "Overdiagnosis", "Detected", "Survival Time",
    "Diagnostic Death from Screening", "Quit Smoking",
    "Eligible for Cessation Program", "Entry Age"
  )
  
  colnames(scrn)[1:length(scrn_colnames)] <- scrn_colnames
  
  # Remove unnamed columns
  scrn <- scrn[, !is.na(names(scrn)) & names(scrn) != ""]
  
  # Assign row_id
  scrn$row_id <- 1:nrow(scrn)
  
  
  
# --- Join comorbidity info to each simulation dataset ---
  noscrn <- noscrn %>%
    left_join(expanded_data %>% select(row_id, all_of(comorb_cols)), by = "row_id")
  
  scrn <- scrn %>%
    left_join(expanded_data %>% select(row_id, all_of(comorb_cols)), by = "row_id")
  
# --- Compute comorbidity count and category ---
  scrn$comorb_count <- rowSums(scrn[, comorb_cols], na.rm = TRUE)
  scrn$comorb_cat <- cut(scrn$comorb_count, breaks = c(-1, 0, 1, Inf), labels = c("0", "1", "2+"))
  
  noscrn$comorb_count <- rowSums(noscrn[, comorb_cols], na.rm = TRUE)
  noscrn$comorb_cat <- cut(noscrn$comorb_count, breaks = c(-1, 0, 1, Inf), labels = c("0", "1", "2+"))
  
 # Check Missing 
  #noscrn_missing <- sapply(noscrn, function(x) sum(is.na(x)))
  #scrn_missing <- sapply(scrn, function(x) sum(is.na(x)))
  #noscrn_missing[noscrn_missing > 0]
  #scrn_missing[scrn_missing > 0]

# Check File Count
  #length(list.files("results/output_noscrn/", pattern = "^no_scrn_\\d{1,3}\\.csv$"))
  #length(list.files("results/output_scrn/", pattern = "^scrn_tf111_100_50_80_20_15_100_0_\\d{1,3}\\.csv$"))
  
  

#### --- Post-Simulation Analysis --- ###
## Table 1 #    
  
  
  
## Table 2 #
  table2_noscrn <- noscrn %>%
    filter(!is.na(Stage), !is.na(comorb_cat)) %>%
    mutate(Stage = as.factor(Stage)) %>%
    tabyl(Stage, comorb_cat) %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns() # No Screening

  table2_scrn <- scrn %>%
    filter(!is.na(Stage), !is.na(comorb_cat)) %>%
    mutate(Stage = as.factor(Stage)) %>%
    tabyl(Stage, comorb_cat) %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns() # Screening
  
  # Chi-square test
  chisq2_noscrn <- chisq.test(table(noscrn$Stage, noscrn$comorb_cat))
  chisq2_scrn <- chisq.test(table(scrn$Stage, scrn$comorb_cat))
  
  # Output
  print(table2_noscrn)
  cat("Chi-square p-value (no-screening):", 
      format.pval(chisq2_noscrn$p.value, digits = 3, eps = .Machine$double.eps), "\n")  
  print(table2_scrn)
  cat("Chi-square p-value (screening):", 
      format.pval(chisq2_scrn$p.value, digits = 3, eps = .Machine$double.eps), "\n")
 
## Table 3 # 
  
  
## Table 4 #
  
  
## Table 5 #
  table5_noscrn <- noscrn %>%
    filter(!is.na(Histology), !is.na(comorb_cat)) %>%
    count(Histology, comorb_cat) %>%
    group_by(comorb_cat) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      formatted = paste0(n, " (", pct, "%)")
    ) %>%
    ungroup() %>%
    select(Histology, comorb_cat, formatted) %>%
    tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)") # No Screening
  
  table5_scrn <- scrn %>%
    filter(!is.na(Histology), !is.na(comorb_cat)) %>%
    count(Histology, comorb_cat) %>%
    group_by(comorb_cat) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      formatted = paste0(n, " (", pct, "%)")
    ) %>%
    ungroup() %>%
    select(Histology, comorb_cat, formatted) %>%
    tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)") # Screening  
  
  # Chi-square test
  chisq5_noscrn <- chisq.test(table(noscrn$Histology, noscrn$comorb_cat))
  chisq5_scrn <- chisq.test(table(scrn$Histology, scrn$comorb_cat))
  
  # Output
  print(table5_noscrn)
  cat("Chi-square p-value (no-screening):", 
      format.pval(chisq5_noscrn$p.value, digits = 3, eps = .Machine$double.eps), "\n") 
  print(table5_scrn)
  cat("Chi-square p-value (screening):", 
      format.pval(chisq5_scrn$p.value, digits = 3, eps = .Machine$double.eps), "\n") 
    
  
## Table 6 #    
    
    
    
    