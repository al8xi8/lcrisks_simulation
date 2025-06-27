
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
#noscrn <- noscrn[, !is.na(names(noscrn)) & names(noscrn) != ""]

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
#scrn <- scrn[, !is.na(names(scrn)) & names(scrn) != ""]

# Assign row_id
scrn$row_id <- 1:nrow(scrn)



# --- Join comorbidity info to each simulation dataset ---
noscrn <- noscrn %>%
  left_join(expanded_data %>% select(row_id, all_of(comorb_cols)), by = "row_id")

scrn <- scrn %>%
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
# Compute comorbidity count and category ---
scrn$comorb_count <- rowSums(scrn[, comorb_cols], na.rm = TRUE)
scrn$comorb_cat <- cut(scrn$comorb_count, breaks = c(-1, 0, 1, Inf), labels = c("0", "1", "2+"))

noscrn$comorb_count <- rowSums(noscrn[, comorb_cols], na.rm = TRUE)
noscrn$comorb_cat <- cut(noscrn$comorb_count, breaks = c(-1, 0, 1, Inf), labels = c("0", "1", "2+"))

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


####### NHIS Mock Table #######
#### --- "noscrn" (No Screening) v.s. "scrn" (Screening) --- ###

## Table 1 #  keep stage 0  


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
  tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)") # Screening

# Chi-square test
chisq2_noscrn <- noscrn %>%
  filter(!is.na(Stage.cat), !is.na(comorb_cat)) %>%
  {chisq.test(table(.$Stage.cat, .$comorb_cat))}

chisq2_scrn <- scrn %>%
  filter(!is.na(Stage.cat), !is.na(comorb_cat), !(Overdiagnosis == 1 & Detected == 0)) %>%
  {chisq.test(table(.$Stage.cat, .$comorb_cat))}

# Output
print(table2_noscrn)
cat("Chi-square p-value (no-screening):", 
    format.pval(chisq2_noscrn$p.value, digits = 3, eps = .Machine$double.eps), "\n")  
print(table2_scrn)
cat("Chi-square p-value (screening):", 
    format.pval(chisq2_scrn$p.value, digits = 3, eps = .Machine$double.eps), "\n")


--------------------------------------------------------------------------------  
  ## Table 3 # all cost mortality keep stage 0
  
--------------------------------------------------------------------------------
  ## Table 4 # lung cancer diagnosis keep stage 0
  
  
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
  tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)") # Screening  

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

# Output
print(table5_noscrn)
cat("Chi-square p-value (no-screening):", 
    format.pval(chisq5_noscrn$p.value, digits = 3, eps = .Machine$double.eps), "\n") 
print(table5_scrn)
cat("Chi-square p-value (screening):", 
    format.pval(chisq5_scrn$p.value, digits = 3, eps = .Machine$double.eps), "\n") 

--------------------------------------------------------------------------------  
  ## Table 6 # (Exclude Stage.cat == NA & Histology.cat == NA)   
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
  
  