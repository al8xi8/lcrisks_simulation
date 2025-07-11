
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
if (!require("Hmisc")) install.packages("Hmisc")
library(Hmisc)
library(tibble)
library(survival)
library(broom)
install.packages("survminer")
library(survminer)
if (!require("cmprsk")) install.packages("cmprsk")
library(cmprsk)

# --- Load Post-Imputation + Weight-Expanded Dataset ---
expanded_data <- read_csv("datasets/expanded_data.csv")

# Assign row_id
expanded_data$row_id <- 1:nrow(expanded_data)

# Define comorbidity columns
comorb_cols <- c("copd", "prshist", "hypertension", "chd", "angina", "heartattack",
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
#noscrn <- noscrn[, !is.na(names(noscrn)) & names(noscrn) != ""]

# Assign row_id
noscrn$row_id <- 1:nrow(noscrn)


### --- Load *Screening* Data (lifetime + 3x) --- ###
scrn_path <- "results/output_scrn_lifetime/"
scrn3_path <- "results/output_scrn_3scrns/"  

scrn_files <- list.files(
  path = scrn_path,
  pattern = "^scrn_tf111_100_50_80_20_15_100_0_\\d{1,3}\\.csv$",
  full.names = TRUE
)

scrn3_files <- list.files(
  path = scrn3_path,
  pattern = "^scrn_tf111_100_50_80_20_15_100_0_\\d{1,3}\\.csv$",
  full.names = TRUE
)

# Sort numerically
scrn_files <- scrn_files[order(as.numeric(gsub("[^0-9]", "", basename(scrn_files))))]
scrn3_files <- scrn3_files[order(as.numeric(gsub("[^0-9]", "", basename(scrn3_files))))]

# Read in and skip first row
scrn <- bind_rows(
  lapply(scrn_files, function(file) {
    df <- read_csv(file, col_names = FALSE, skip = 1)
    df$file_id <- basename(file)
    return(df)
  })
)

scrn3 <- bind_rows(
  lapply(scrn3_files, function(file) {
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
colnames(scrn3)[1:length(scrn_colnames)] <- scrn_colnames


# Remove unnamed columns
#scrn <- scrn[, !is.na(names(scrn)) & names(scrn) != ""]

# Assign row_id
scrn$row_id <- 1:nrow(scrn)
scrn3$row_id <- 1:nrow(scrn3)



# --- Join comorbidity info to each simulation dataset ---
noscrn <- noscrn %>%
  left_join(expanded_data %>% select(row_id, all_of(comorb_cols)), by = "row_id")

scrn <- scrn %>%
  left_join(expanded_data %>% select(row_id, all_of(comorb_cols)), by = "row_id")

scrn3 <- scrn3 %>%
  left_join(expanded_data %>% select(row_id, all_of(comorb_cols)), by = "row_id")

# Check Missing 
#noscrn_missing <- sapply(noscrn, function(x) sum(is.na(x)))
#scrn_missing <- sapply(scrn, function(x) sum(is.na(x)))
#noscrn_missing[noscrn_missing > 0]
#scrn_missing[scrn_missing > 0]

# Check File Count
#length(list.files("results/output_noscrn/", pattern = "^no_scrn_\\d{1,3}\\.csv$"))
#length(list.files("results/output_scrn/", pattern = "^scrn_tf111_100_50_80_20_15_100_0_\\d{1,3}\\.csv$"))


#### --- Data Preparation and Recoding --- ###

    # Define smoking status columns
    expanded_data <- expanded_data %>%
      mutate(
        smokestatus = case_when(
          qtyears == 0 ~ "Current",
          qtyears > 0 ~ "Former",
          TRUE ~ NA_character_
        )
      )
    expanded_data$smokestatus <- factor(expanded_data$smokestatus, levels = c("Current", "Former"))
    

# Compute comorbidity count and category ---
scrn$comorb_count <- rowSums(scrn[, comorb_cols], na.rm = TRUE)
scrn$comorb_cat <- cut(scrn$comorb_count, breaks = c(-1, 0, 1, Inf), labels = c("0", "1", "2+"))

scrn3$comorb_count <- rowSums(scrn3[, comorb_cols], na.rm = TRUE)
scrn3$comorb_cat <- cut(scrn3$comorb_count, breaks = c(-1, 0, 1, Inf), labels = c("0", "1", "2+"))

noscrn$comorb_count <- rowSums(noscrn[, comorb_cols], na.rm = TRUE)
noscrn$comorb_cat <- cut(noscrn$comorb_count, breaks = c(-1, 0, 1, Inf), labels = c("0", "1", "2+"))

expanded_data$comorb_count <- rowSums(expanded_data[, comorb_cols], na.rm = TRUE)
expanded_data$comorb_cat <- cut(expanded_data$comorb_count, breaks = c(-1, 0, 1, Inf), labels = c("0", "1", "2+"))

# Screening group
scrn <- scrn %>%
  mutate(
    Stage.cat = case_when(
      Stage == 1 ~ "IA",
      Stage == 2 ~ "IB",
      Stage == 3 ~ "II",
      Stage == 4 ~ "IIIA",
      Stage == 5 ~ "IIIB",
      Stage == 6 ~ "IV",
      TRUE ~ NA_character_
    ),
    Histology.cat = case_when(
      Histology == 1 ~ "Small Cell",
      Histology == 2 ~ "Adenocarcinoma",
      Histology == 3 ~ "Squamous",
      Histology == 4 ~ "Other",
      TRUE ~ NA_character_
    )
  )

scrn3 <- scrn3 %>%
  mutate(
    Stage.cat = case_when(
      Stage == 1 ~ "IA",
      Stage == 2 ~ "IB",
      Stage == 3 ~ "II",
      Stage == 4 ~ "IIIA",
      Stage == 5 ~ "IIIB",
      Stage == 6 ~ "IV",
      TRUE ~ NA_character_
    ),
    Histology.cat = case_when(
      Histology == 1 ~ "Small Cell",
      Histology == 2 ~ "Adenocarcinoma",
      Histology == 3 ~ "Squamous",
      Histology == 4 ~ "Other",
      TRUE ~ NA_character_
    )
  )

# No-screening group
noscrn <- noscrn %>%
  mutate(
    Stage.cat = case_when(
      Stage == 1 ~ "IA",
      Stage == 2 ~ "IB",
      Stage == 3 ~ "II",
      Stage == 4 ~ "IIIA",
      Stage == 5 ~ "IIIB",
      Stage == 6 ~ "IV",
      TRUE ~ NA_character_
    ),
    Histology.cat = case_when(
      Histology == 1 ~ "Small Cell",
      Histology == 2 ~ "Adenocarcinoma",
      Histology == 3 ~ "Squamous",
      Histology == 4 ~ "Other",
      TRUE ~ NA_character_
    )
  )

--------------------------------------------------------------------------------
  
####### NHIS Mock Table : Part 1 #######
#### --- "noscrn" (No Screening) v.s. "scrn" (Screening) --- ###

## Table 1 # (Keep Stage 0)  
  expanded_0 <- expanded_data %>% filter(comorb_cat == "0")
  expanded_1 <- expanded_data %>% filter(comorb_cat == "1")
  expanded_2p <- expanded_data %>% filter(comorb_cat == "2+")
  
    # Refer to File "3_imputation.R" for Function "Baseline Summary for Post-Imputation Data".
    baseline_table1_expanded0 <- summarize_baseline_final(expanded_0)
    baseline_table1_expanded0$Year <- "POST-IMPUTED NHIS EXPANDED* Full Year (N_cor = 0)"
    View(baseline_table1_expanded0)
    
    baseline_table1_expanded1 <- summarize_baseline_final(expanded_1)
    baseline_table1_expanded1$Year <- "POST-IMPUTED NHIS EXPANDED* Full Year (N_cor = 1)"
    View(baseline_table1_expanded1)
    
    baseline_table1_expanded2p <- summarize_baseline_final(expanded_2p)
    baseline_table1_expanded2p$Year <- "POST-IMPUTED NHIS EXPANDED* Full Year (N_cor = 2p)"
    View(baseline_table1_expanded2p)
  

    # --- Function to summarize continuous variables ---
    summarize_continuous <- function(data, var) {
      vec <- data[[var]]
      vec <- vec[!is.na(vec)]
      n <- length(vec)
      m <- mean(vec)
      ci <- t.test(vec)$conf.int
      tibble(
        Variable = var,
        `Sample size` = n,
        `Mean/Proportion` = round(m, 2),
        `95% CI` = paste0("(", round(ci[1], 2), ", ", round(ci[2], 2), ")"),
        `Minimum` = round(min(vec), 2),
        `Maximum` = round(max(vec), 2),
        `99 Percentile` = round(quantile(vec, 0.99), 2),
        `Median` = round(median(vec), 2)
      )
    }
    
    
    # --- Function to summarize binary/categorical variables ---
    summarize_binary <- function(data, var, labels = NULL) {
      tab <- table(data[[var]])
      total <- sum(tab)
      result <- tibble()
      for (val in names(tab)) {
        p <- prop.test(tab[[val]], total)$conf.int
        label <- if (!is.null(labels) && val %in% names(labels)) labels[[val]] else paste(var, val)
        result <- bind_rows(result, tibble(
          Variable = label,
          `Sample size` = total,
          `Mean/Proportion` = round(100 * tab[[val]] / total, 1),
          `95% CI` = paste0("(", round(100 * p[1], 1), ", ", round(100 * p[2], 1), ")"),
          `Minimum` = NA, `Maximum` = NA, `99 Percentile` = NA, `Median` = NA
        ))
      }
      result
    }
    
    # --- Compute comorbidity count and category ---
    expanded_data <- expanded_data %>%
      mutate(
        comorb_count = prshist + hypertension + chd + stroke + diab + bron +
          copd + liver + heartattack + heartdisease + angina + kidney + spaceq,
        comorb_cat = case_when(
          comorb_count == 0 ~ "0",
          comorb_count == 1 ~ "1",
          comorb_count >= 2 ~ "2+"
        )
      )
    
    # --- Define labels for binary/categorical variables ---
    continuous_vars <- c("age", "pky", "smkyears", "qtyears", "avecpd", "bmi")
    
    binary_vars <- list(
      female = c("0" = "Male", "1" = "Female"),
      race = c("0" = "Non-hispanic white", "1" = "Non-hispanic black", 
               "2" = "Hispanic", "3" = "Other"),
      lung_cancer = c("1" = "Had lung cancer during follow-up", "0" = "No lung cancer")
    )
    
    comorb_labels <- c("0" = "0 comorbidities", "1" = "1 comorbidity", "2+" = "2+ comorbidities")
    
    # --- Generate summary table ---
    summary_table <- bind_rows(
      lapply(continuous_vars, function(v) summarize_continuous(expanded_data, v)),
      bind_rows(lapply(names(binary_vars), function(v) summarize_binary(expanded_data, v, binary_vars[[v]]))),
      summarize_binary(expanded_data, "comorb_cat", comorb_labels)
    )
    
    summary_table0 <- bind_rows(
      lapply(continuous_vars, function(v) summarize_continuous(expanded_0, v)),
      bind_rows(lapply(names(binary_vars), function(v) summarize_binary(expanded_0, v, binary_vars[[v]]))),
      summarize_binary(expanded_0, "comorb_cat", comorb_labels)
    )
    
    summary_table1 <- bind_rows(
      lapply(continuous_vars, function(v) summarize_continuous(expanded_1, v)),
      bind_rows(lapply(names(binary_vars), function(v) summarize_binary(expanded_1, v, binary_vars[[v]]))),
      summarize_binary(expanded_1, "comorb_cat", comorb_labels)
    )
    
    summary_table2p <- bind_rows(
      lapply(continuous_vars, function(v) summarize_continuous(expanded_2p, v)),
      bind_rows(lapply(names(binary_vars), function(v) summarize_binary(expanded_2p, v, binary_vars[[v]]))),
      summarize_binary(expanded_2p, "comorb_cat", comorb_labels)
    )

    # List of 13 comorbidity variables
    comorb_vars <- c("prshist", "hypertension", "chd", "stroke", "diab",
                     "bron", "copd", "liver", "heartattack", "heartdisease",
                     "angina", "kidney", "spaceq")
    
    # Function to summarize binary comorbidity variables
    summarize_comorb <- function(data, var) {
      tab <- table(data[[var]])
      total <- sum(tab)
      pos <- ifelse("1" %in% names(tab), tab[["1"]], 0)
      ci <- prop.test(pos, total)$conf.int
      tibble(
        Variable = var,
        Count = pos,
        `Total Sample` = total,
        `Mean/Proportion (%)` = round(100 * pos / total, 1),
        `95% CI` = paste0("(", round(100 * ci[1], 1), ", ", round(100 * ci[2], 1), ")")
      )
    }
    
    # Apply the summary function to all comorbidity variables
    comorb_summary <- bind_rows(lapply(comorb_vars, function(v) summarize_comorb(expanded_data, v)))
    #comorb_summary0 <- bind_rows(lapply(comorb_vars, function(v) summarize_comorb(expanded_0, v))) #comorbidities should be 0
    comorb_summary1 <- bind_rows(lapply(comorb_vars, function(v) summarize_comorb(expanded_1, v)))
    comorb_summary2p <- bind_rows(lapply(comorb_vars, function(v) summarize_comorb(expanded_2p, v)))
  
  # Had lung cancer during follow-up (<5 years) !!!!!!
    scrn <- scrn %>%
      mutate(
        lc_5yr = ifelse(!is.na(`Lung Cancer Age`) & (`Lung Cancer Age` - `Entry Age`) <= 5, 1, 0)
      )
    
    scrn3 <- scrn3 %>%
      mutate(
        lc_5yr = ifelse(!is.na(`Lung Cancer Age`) & (`Lung Cancer Age` - `Entry Age`) <= 5, 1, 0)
      )
    
    scrn %>%
      group_by(comorb_cat) %>%
      summarise(
        `Total N` = n(),
        `LC cases (<5 years)` = sum(lc_5yr == 1, na.rm = TRUE),
        `No LC cases` = sum(lc_5yr == 0, na.rm = TRUE),
        `LC 5yr Proportion (%)` = round(100 * sum(lc_5yr == 1, na.rm = TRUE) / n(), 2)
      )
    
    scrn3 %>%
      group_by(comorb_cat) %>%
      summarise(
        `Total N` = n(),
        `LC cases (<5 years)` = sum(lc_5yr == 1, na.rm = TRUE),
        `No LC cases` = sum(lc_5yr == 0, na.rm = TRUE),
        `LC 5yr Proportion (%)` = round(100 * sum(lc_5yr == 1, na.rm = TRUE) / n(), 2)
      )
  
    # ---- P-values ----
    
    # Function to run ANOVA for continuous variables
    run_anova <- function(var) {
      tryCatch({
        model <- aov(reformulate("comorb_cat", response = var), data = expanded_data)
        summary(model)[[1]]$`Pr(>F)`[1]
      }, error = function(e) NA)
    }
    
    # Function to run Chi-square test for categorical variables
    run_chi_test <- function(var) {
      tryCatch({
        tbl <- table(expanded_data[[var]], expanded_data$comorb_cat, useNA = "ifany")
        chisq.test(tbl)$p.value
      }, error = function(e) NA)
    }
    
    # Function to run Chi-square test from a different dataset
    run_chi_test_external <- function(data, var) {
      tryCatch({
        tbl <- table(data[[var]], data$comorb_cat, useNA = "ifany")
        chisq.test(tbl)$p.value
      }, error = function(e) NA)
    }
    
    # Variables
    anova_vars <- c("age", "smkyears", "qtyears", "avecpd", "bmi")
    chi_vars <- c("female", "race", "copd", "prshist", "hypertension", 
                  "chd", "angina", "heartattack", "heartdisease", "stroke", 
                  "diab", "bron", "kidney", "spaceq", "liver", "smokestatus")
    
    # Run tests
    anova_p <- sapply(anova_vars, run_anova)
    chi_p <- sapply(chi_vars, run_chi_test)
    p_lc_scrn <- run_chi_test_external(scrn, "lc_5yr")
    p_lc_scrn3 <- run_chi_test_external(scrn3, "lc_5yr")
    
    # Combine
    p_values_table <- data.frame(
      Variable = c("Age", "Smoking years", "Years quit", "CPD", "BMI",
                   "Sex", "Race", "COPD", "Prior Cancer", "Hypertension",
                   "CHD", "Angina", "Heart Attack", "Other Heart Disease", "Stroke",
                   "Diabetes", "Chronic Bronchitis", "Kidney", "Functional Aid", 
                   "Liver", "Smoking Status",
                   "Lung cancer (5y) – scrn", "Lung cancer (5y) – scrn3"),
      Test = c(rep("ANOVA", 5), rep("Chi-square", 16), rep("Chi-square", 2)),
      P_value = c(anova_p, chi_p, p_lc_scrn, p_lc_scrn3)
    )
    
    # Format for display
    p_values_table$Formatted_P <- ifelse(
      is.na(p_values_table$P_value), NA,
      ifelse(p_values_table$P_value < 0.001, "<0.001", round(p_values_table$P_value, 3))
    )
    
    # Display
    print(p_values_table)
    
    
    
--------------------------------------------------------------------------------  

## Table 2 # (Exclude Stage.cat == NA)   
  table2_noscrn <- noscrn %>%
    filter(!is.na(Stage.cat), !is.na(comorb_cat)) %>% # Stage == 0 is Stage.cat == NA
    count(Stage.cat, comorb_cat) %>%
    group_by(comorb_cat) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      formatted = paste0(n, " (", pct, "%)")
    ) %>%
    ungroup() %>%
    select(Stage.cat, comorb_cat, formatted) %>%
    tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)") # No Screening
  
  table2_scrn <- scrn %>%
    filter(
      !is.na(Stage.cat),
      !(Overdiagnosis == 1 & Detected == 0),
      !is.na(comorb_cat)
    ) %>%
    count(Stage.cat, comorb_cat) %>%
    group_by(comorb_cat) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      formatted = paste0(n, " (", pct, "%)")
    ) %>%
    ungroup() %>%
    select(Stage.cat, comorb_cat, formatted) %>%
    tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)") # Screening (lifetime)

  table2_scrn3 <- scrn3 %>%
    filter(
      !is.na(Stage.cat),
      !(Overdiagnosis == 1 & Detected == 0),
      !is.na(comorb_cat)
    ) %>%
    count(Stage.cat, comorb_cat) %>%
    group_by(comorb_cat) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      formatted = paste0(n, " (", pct, "%)")
    ) %>%
    ungroup() %>%
    select(Stage.cat, comorb_cat, formatted) %>%
    tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)") # Screening (3x)
  
      # Chi-square test
      chisq2_noscrn <- noscrn %>%
        filter(!is.na(Stage.cat), !is.na(comorb_cat)) %>%
        {chisq.test(table(.$Stage.cat, .$comorb_cat))}
      
      chisq2_scrn <- scrn %>%
        filter(!is.na(Stage.cat), !is.na(comorb_cat), !(Overdiagnosis == 1 & Detected == 0)) %>%
        {chisq.test(table(.$Stage.cat, .$comorb_cat))}
      
      chisq2_scrn3 <- scrn3 %>%
        filter(!is.na(Stage.cat), !is.na(comorb_cat), !(Overdiagnosis == 1 & Detected == 0)) %>%
        {chisq.test(table(.$Stage.cat, .$comorb_cat))}
      
      # Output
      print(table2_noscrn)
      cat("Chi-square p-value (no-screening):", 
          format.pval(chisq2_noscrn$p.value, digits = 3, eps = .Machine$double.eps), "\n")  
      print(table2_scrn)
      cat("Chi-square p-value (screening):", 
          format.pval(chisq2_scrn$p.value, digits = 3, eps = .Machine$double.eps), "\n")
      print(table2_scrn3)
      cat("Chi-square p-value (screening):", 
          format.pval(chisq2_scrn3$p.value, digits = 3, eps = .Machine$double.eps), "\n")

--------------------------------------------------------------------------------  

## Table 3 # All-Cause Mortality (lifetime vs 5 yr followup)
  
# --------------------------------------
# Table 3A: Entire Population
# --------------------------------------        
      # Step 0: Exclude overdiagnosed but undetected cases
      scrn$exclude_flag <- ifelse(scrn$Overdiagnosis == 1 & scrn$Detected == 0, 1, 0)
      
      # Step 1: Merge exclusion flag + survival variables into expanded_data
      surv_info <- scrn[, c("row_id", "Overall Death Age", "Entry Age", "exclude_flag")]
      expanded_data <- merge(expanded_data, surv_info, by = "row_id", all.x = TRUE)
      
      # Step 2: Filter out excluded cases
      expanded_data <- expanded_data[is.na(expanded_data$exclude_flag) | expanded_data$exclude_flag == 0, ]
      
      # Step 3: Calculate survival time and death status
      expanded_data$survtime_all <- expanded_data$`Overall Death Age` - expanded_data$`Entry Age`
      expanded_data$alldth_all <- ifelse(!is.na(expanded_data$`Overall Death Age`), 1, 0)
      
      # Step 4: Create 5-year follow-up variables
      expanded_data$time_5yr_all <- pmin(expanded_data$survtime_all, 5)
      expanded_data$death_5yr_all <- ifelse(expanded_data$survtime_all <= 5 & expanded_data$alldth_all == 1, 1, 0)
      
      # Step 5: Relevel comorbidity group
      expanded_data$comorb_cat <- relevel(factor(expanded_data$comorb_cat), ref = "0")
      
      # Step 6: Run Cox models
      ## Table 3A I: Lifetime follow-up
      cox_3a_lifetime <- coxph(Surv(survtime_all, alldth_all) ~ 
                                 factor(comorb_cat) + 
                                 age + 
                                 factor(female) + 
                                 factor(race) + 
                                 factor(smokestatus),
                               data = expanded_data)
      
      ## Table 3A II: 5-year follow-up
      cox_3a_5yr <- coxph(Surv(time_5yr_all, death_5yr_all) ~ 
                            factor(comorb_cat) + 
                            age + 
                            factor(female) + 
                            factor(race) + 
                            factor(smokestatus),
                          data = expanded_data)
      
      results_3a_lifetime <- tidy(cox_3a_lifetime, conf.int = TRUE, exponentiate = TRUE)
      results_3a_5yr <- tidy(cox_3a_5yr, conf.int = TRUE, exponentiate = TRUE)
      print(results_3a_lifetime)
      print(results_3a_5yr)
      
      #expanded_data$time_10yr_all <- pmin(expanded_data$survtime_all, 10)
      #expanded_data$death_10yr_all <- ifelse(expanded_data$survtime_all <= 10 & expanded_data$alldth_all == 1, 1, 0)
      #table(expanded_data$death_10yr_all)
      #cox_3a_10yr <- coxph(Surv(time_10yr_all, death_10yr_all) ~ 
      factor(comorb_cat) + 
        age + 
        factor(female) + 
        factor(race) + 
        factor(smokestatus),
        data = expanded_data)

      #results_3a_10yr <- tidy(cox_3a_10yr, conf.int = TRUE, exponentiate = TRUE)
      #print(results_3a_10yr)
      
# --------------------------------------
# Table 3B: Lung Cancer Patients Only
# --------------------------------------
      # Step 0: Exclude overdiagnosed but not detected cases
      scrn$exclude_flag <- ifelse(scrn$Overdiagnosis == 1 & scrn$Detected == 0, 1, 0)
      scrn <- scrn[scrn$exclude_flag == 0, ]
      
      # Step 1: Filter lung cancer patients with valid stage & histology
      scrn_lung <- scrn[!is.na(scrn$Stage.cat) & !is.na(scrn$Histology.cat), ]
      
      # Step 2: Keep only necessary variables and calculate survival time correctly
      scrn_lung <- scrn_lung %>%
        dplyr::select(row_id, `Lung Cancer Age`, `Overall Death Age`, Stage.cat, Histology.cat) %>%
        dplyr::rename(dx_age = `Lung Cancer Age`, death_age = `Overall Death Age`) %>%
        dplyr::mutate(
          survtime = death_age - dx_age,
          alldth = ifelse(!is.na(death_age), 1, 0),
          time_5yr = pmin(survtime, 5),
          death_5yr = ifelse(survtime <= 5 & alldth == 1, 1, 0)
        )
      
      # Step 3: Merge in covariates from expanded_data
      scrn_lung <- merge(
        scrn_lung,
        expanded_data[, c("row_id", "comorb_cat", "age", "female", "race", "smokestatus")],
        by = "row_id",
        all.x = TRUE
      )
      
      # Step 4: Drop any remaining NAs in key covariates
      scrn_lung <- scrn_lung %>%
        dplyr::filter(!is.na(comorb_cat) & !is.na(age) & !is.na(female) &
                        !is.na(race) & !is.na(smokestatus))
      
      # Step 5: Relevel comorbidity group
      scrn_lung$comorb_cat <- factor(scrn_lung$comorb_cat)
      if ("0" %in% levels(scrn_lung$comorb_cat)) {
        scrn_lung$comorb_cat <- relevel(scrn_lung$comorb_cat, ref = "0")
      } else {
        stop("Reference level '0' not found in comorb_cat")
      }
      
      # Step 6A: Cox model — Table 3B I (Lifetime follow-up)
      cox_3b_lifetime <- coxph(Surv(survtime, alldth) ~ 
                                 factor(comorb_cat) + 
                                 age + 
                                 factor(female) + 
                                 factor(race) + 
                                 factor(smokestatus) +
                                 factor(Stage.cat) + 
                                 factor(Histology.cat),
                               data = scrn_lung)
      
      # Step 6B: Cox model — Table 3B II (5-year follow-up)
      cox_3b_5yr <- coxph(Surv(time_5yr, death_5yr) ~ 
                            factor(comorb_cat) + 
                            age + 
                            factor(female) + 
                            factor(race) + 
                            factor(smokestatus) +
                            factor(Stage.cat) + 
                            factor(Histology.cat),
                          data = scrn_lung)
      
      results_3b_5yr <- broom::tidy(cox_3b_5yr, conf.int = TRUE, exponentiate = TRUE)
      results_3b_lifetime <- broom::tidy(cox_3b_lifetime, conf.int = TRUE, exponentiate = TRUE)
      print(results_3b_lifetime)
      print(results_3b_5yr)

      
--------------------------------------------------------------------------------

## Table 4 # Fine and Gray Model (lifetime vs 5 yr followup)
      # -----------------------------
      # Step 0: Exclude overdiagnosed but undetected cases
      # -----------------------------
      scrn$exclude_flag <- ifelse(scrn$Overdiagnosis == 1 & scrn$Detected == 0, 1, 0)
      scrn_clean <- scrn[scrn$exclude_flag == 0 | is.na(scrn$exclude_flag), ]
      
      # -----------------------------
      # Step 1: Merge Lung Cancer Age into expanded_data
      # -----------------------------
      scrn_small <- scrn_clean[, c("row_id", "Lung Cancer Age")]
      expanded_data <- merge(expanded_data, scrn_small, by = "row_id", all.x = TRUE)
      
      # -----------------------------
      # Step 2: Define time and status for Fine & Gray
      # -----------------------------
      expanded_data$lungtime_all <- with(expanded_data,
                                         ifelse(!is.na(`Lung Cancer Age`), `Lung Cancer Age` - `Entry Age`,
                                                ifelse(!is.na(survtime), survtime, NA))
      )
      
      expanded_data$lungstatus_all <- with(expanded_data,
                                           ifelse(!is.na(`Lung Cancer Age`), 1,
                                                  ifelse(alldth == 1, 2, 0))
      )
      
      # 5-year version
      expanded_data$lungtime_5yr <- pmin(expanded_data$lungtime_all, 5)
      expanded_data$lungstatus_5yr <- with(expanded_data,
                                           ifelse(lungstatus_all == 1 & lungtime_all <= 5, 1,
                                                  ifelse(lungstatus_all == 2 & lungtime_all <= 5, 2, 0))
      )
      
      # -----------------------------
      # Step 3: Relevel comorbidity
      # -----------------------------
      expanded_data$comorb_cat <- factor(expanded_data$comorb_cat)
      expanded_data$comorb_cat <- relevel(expanded_data$comorb_cat, ref = "0")
      
      # -----------------------------
      # Step 4: Random sample 50,000 observations
      # -----------------------------
      set.seed(2025)  # Reproducible results
      sample_ids <- sample(nrow(expanded_data), 50000)
      sample_data <- expanded_data[sample_ids, ]
      
      # Covariate matrix for sample
      covariates_sample <- model.matrix(~ comorb_cat + age + factor(female) +
                                          factor(race) + factor(smokestatus),
                                        data = sample_data)[, -1]
      
      # -----------------------------
      # Step 5A: Fine and Gray model — Lifetime
      # -----------------------------
      fg_model_4_lifetime <- crr(ftime = sample_data$lungtime_all,
                                 fstatus = sample_data$lungstatus_all,
                                 cov1 = covariates_sample,
                                 failcode = 1,
                                 cencode = 0)
      
      # -----------------------------
      # Step 5B: Fine and Gray model — 5-Year
      # -----------------------------
      fg_model_4_5yr <- crr(ftime = sample_data$lungtime_5yr,
                            fstatus = sample_data$lungstatus_5yr,
                            cov1 = covariates_sample,
                            failcode = 1,
                            cencode = 0)
      
      # -----------------------------
      # Step 6: Tidy + Print
      # -----------------------------
      results_4a_lifetime <- tidy(fg_model_4_lifetime, conf.int = TRUE, exponentiate = TRUE)
      results_4a_5yr <- tidy(fg_model_4_5yr, conf.int = TRUE, exponentiate = TRUE)
      
      print(results_4a_lifetime)
      print(results_4a_5yr)
      
  
--------------------------------------------------------------------------------

## Table 5 # (Exclude Histology.cat == NA)   
table5_noscrn <- noscrn %>%
  filter(!is.na(Histology.cat), !is.na(comorb_cat)) %>% # Histology == 0 is Histology.cat == NA
  count(Histology.cat, comorb_cat) %>%
  group_by(comorb_cat) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    formatted = paste0(n, " (", pct, "%)")
  ) %>%
  ungroup() %>%
  select(Histology.cat, comorb_cat, formatted) %>%
  tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)") # No Screening

table5_scrn <- scrn %>%
  filter(
    !is.na(Histology.cat), !is.na(comorb_cat),
    !(Overdiagnosis == 1 & Detected == 0)
  ) %>%
  count(Histology.cat, comorb_cat) %>%
  group_by(comorb_cat) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    formatted = paste0(n, " (", pct, "%)")
  ) %>%
  ungroup() %>%
  select(Histology.cat, comorb_cat, formatted) %>%
  tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)") # Screening (lifetime)  

table5_scrn3 <- scrn3 %>%
  filter(
    !is.na(Histology.cat), !is.na(comorb_cat),
    !(Overdiagnosis == 1 & Detected == 0)
  ) %>%
  count(Histology.cat, comorb_cat) %>%
  group_by(comorb_cat) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    formatted = paste0(n, " (", pct, "%)")
  ) %>%
  ungroup() %>%
  select(Histology.cat, comorb_cat, formatted) %>%
  tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)") # Screening (3x)

    # Chi-square test
    chisq5_noscrn <- noscrn %>%
      filter(!is.na(Histology.cat), !is.na(comorb_cat)) %>%
      {chisq.test(table(.$Histology.cat, .$comorb_cat))}
    
    chisq5_scrn <- scrn %>%
      filter(
        !is.na(Histology.cat), !is.na(comorb_cat),
        !(Overdiagnosis == 1 & Detected == 0)
      ) %>%
      {chisq.test(table(.$Histology.cat, .$comorb_cat))}
    
    chisq5_scrn3 <- scrn3 %>%
      filter(
        !is.na(Histology.cat), !is.na(comorb_cat),
        !(Overdiagnosis == 1 & Detected == 0)
      ) %>%
      {chisq.test(table(.$Histology.cat, .$comorb_cat))}
    
    # Output
    print(table5_noscrn)
    cat("Chi-square p-value (no-screening):", 
        format.pval(chisq5_noscrn$p.value, digits = 3, eps = .Machine$double.eps), "\n") 
    print(table5_scrn)
    cat("Chi-square p-value (screening):", 
        format.pval(chisq5_scrn$p.value, digits = 3, eps = .Machine$double.eps), "\n")
    print(table5_scrn3)
    cat("Chi-square p-value (screening):", 
        format.pval(chisq5_scrn3$p.value, digits = 3, eps = .Machine$double.eps), "\n") 
    

--------------------------------------------------------------------------------
      
## Table 6 # (Exclude Stage.cat == NA & Histology.cat == NA)   
  
  # Lifetime Screening
      table6_scrn_base <- scrn %>%
    filter(
      !is.na(Stage.cat), # Stage == 0 is Stage.cat == NA
      !is.na(Histology.cat), # Histology == 0 is Histology.cat == NA
      !(Overdiagnosis == 1 & Detected == 0),
      !is.na(comorb_cat)
    )
  
  generate_table6_scrn <- function(data, comorb_label) {
    tab <- data %>%
      filter(comorb_cat == comorb_label) %>%
      count(Stage.cat, Histology.cat) %>%
      pivot_wider(names_from = Histology.cat, values_from = n, values_fill = 0) %>%
      rowwise() %>%
      mutate(
        TOTAL = sum(c_across(c("Small Cell", "Adenocarcinoma", "Squamous", "Other")), na.rm = TRUE),
        across(c("Small Cell", "Adenocarcinoma", "Squamous", "Other"), 
               ~ paste0(.x, " (", round(.x / TOTAL * 100, 1), "%)"))
      ) %>%
      ungroup()
  
    # Chi-square test for "combined Table 6"
    chisq_result <- data %>%
        filter(comorb_cat == comorb_label) %>%
        select(Stage.cat, Histology.cat) %>%
        table() %>%
        chisq.test()
      
      list(table = tab, chisq = chisq_result)
    }
      
    # Stratify by comorbidity categories 
    table6_scrn_0 <- generate_table6_scrn(table6_scrn_base, "0")
    table6_scrn_1 <- generate_table6_scrn(table6_scrn_base, "1")
    table6_scrn_2plus <- generate_table6_scrn(table6_scrn_base, "2+")

    # Output
    cat("\n=== Table 6 – Screening – N_cor = 0 ===\n")
      print(table6_scrn_0$table)
    cat("\nChi-square p-value (N_cor = 0):", format.pval(table6_scrn_0$chisq$p.value, digits = 3), "\n")
      
    cat("\n=== Table 6 – Screening – N_cor = 1 ===\n")
      print(table6_scrn_1$table)
    cat("Chi-square p-value (N_cor = 1):", format.pval(table6_scrn_1$chisq$p.value, digits = 3), "\n")
      
    cat("\n=== Table 6 – Screening – N_cor = 2+ ===\n")
      print(table6_scrn_2plus$table)
    cat("Chi-square p-value (N_cor = 2+):", format.pval(table6_scrn_2plus$chisq$p.value, digits = 3), "\n")

  # 3x Screening
    table6_scrn3_base <- scrn3 %>%
      filter(
        !is.na(Stage.cat),  # Stage == 0 is Stage.cat == NA
        !is.na(Histology.cat),  # Histology == 0 is Histology.cat == NA
        !(Overdiagnosis == 1 & Detected == 0),
        !is.na(comorb_cat)
      )
    
    generate_table6_scrn3 <- function(data, comorb_label) {
      tab <- data %>%
        filter(comorb_cat == comorb_label) %>%
        count(Stage.cat, Histology.cat) %>%
        pivot_wider(names_from = Histology.cat, values_from = n, values_fill = 0) %>%
        rowwise() %>%
        mutate(
          TOTAL = sum(c_across(c("Small Cell", "Adenocarcinoma", "Squamous", "Other")), na.rm = TRUE),
          across(c("Small Cell", "Adenocarcinoma", "Squamous", "Other"), 
                 ~ paste0(.x, " (", round(.x / TOTAL * 100, 1), "%)"))
        ) %>%
        ungroup()
      
      chisq_result <- data %>%
        filter(comorb_cat == comorb_label) %>%
        select(Stage.cat, Histology.cat) %>%
        table() %>%
        chisq.test()
      
      list(table = tab, chisq = chisq_result)
    }
    
    table6_scrn3_0 <- generate_table6_scrn3(table6_scrn3_base, "0")
    table6_scrn3_1 <- generate_table6_scrn3(table6_scrn3_base, "1")
    table6_scrn3_2plus <- generate_table6_scrn3(table6_scrn3_base, "2+")
    
    # Output
    cat("\n=== Table 6 – Screening (3x) – N_cor = 0 ===\n")
    print(table6_scrn3_0$table)
    cat("Chi-square p-value (N_cor = 0):", format.pval(table6_scrn3_0$chisq$p.value, digits = 3), "\n")
    
    cat("\n=== Table 6 – Screening (3x) – N_cor = 1 ===\n")
    print(table6_scrn3_1$table)
    cat("Chi-square p-value (N_cor = 1):", format.pval(table6_scrn3_1$chisq$p.value, digits = 3), "\n")
    
    cat("\n=== Table 6 – Screening (3x) – N_cor = 2+ ===\n")
    print(table6_scrn3_2plus$table)
    cat("Chi-square p-value (N_cor = 2+):", format.pval(table6_scrn3_2plus$chisq$p.value, digits = 3), "\n")
    

      