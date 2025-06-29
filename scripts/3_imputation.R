
# --- Load Libraries ---
library(readr)
library(dplyr)
library(tidyr)
library(mice)
library(splitstackshape)

# --- Step 1: Filter Initial Imputation Pool ---
lcrisks_imp <- nhis_lcrisks %>%
  filter(
    year >= 2010 & year <= 2023,
    age >= 50 & age <= 80,
    SMOKESTATUS2 %in% c(10, 11, 12, 13, 20)  # current or former smokers
  )

# --- Step 2: Separate by Smoking Status ---
current_smokers <- lcrisks_imp %>% filter(SMOKESTATUS2 %in% c(10, 11, 12, 13))
former_smokers  <- lcrisks_imp %>% filter(SMOKESTATUS2 == 20)

# --- Step 3: Define Final 24 Variables for LC Risk Model ---
vars_final <- c(
  "SAMPWEIGHT", "age", "female", "smkyears", "qtyears", "avecpd", "race", "copd",
  "fmhist", "bmi", "edu", "prshist", "hypertension", "chd", "angina",
  "heartattack", "heartdisease", "stroke", "diab", "bron", "kidney", "liver",
  "spaceq", "year"
)

# --- Step 4: Define Imputation Method ---
impute_method <- c(
  "",         # SAMPWEIGHT
  "",         # age
  "logreg",   # female
  "pmm",      # smkyears
  "pmm",      # qtyears
  "pmm",      # avecpd
  "polyreg",  # race
  "logreg",   # copd
  "pmm",      # fmhist
  "pmm",      # bmi
  "polr",     # edu
  "logreg",   # prshist
  "logreg",   # hypertension
  "logreg",   # chd
  "logreg",   # angina
  "logreg",   # heartattack
  "logreg",   # heartdisease
  "logreg",   # stroke
  "logreg",   # diab
  "logreg",   # bron
  "logreg",   # kidney
  "logreg",   # liver
  "logreg",   # spaceq
  ""          # year
)

# --- Step 5: Imputation Function ---
prepare_and_impute <- function(df, type) {
  df <- df[, vars_final]
  
  # Format types
  df <- df %>%
    mutate(
      female       = factor(female),
      race         = factor(race),
      copd         = factor(copd),
      fmhist       = as.numeric(fmhist),
      bmi          = as.numeric(bmi),
      edu          = ordered(edu),
      prshist      = factor(prshist),
      hypertension = factor(hypertension),
      chd          = factor(chd),
      angina       = factor(angina),
      heartattack  = factor(heartattack),
      heartdisease = factor(heartdisease),
      stroke       = factor(stroke),
      diab         = factor(diab),
      bron         = factor(bron),
      kidney       = factor(kidney),
      liver        = factor(liver),
      spaceq       = factor(spaceq)
    )
  
  # Impute
  imp <- mice(df, m = 5, method = impute_method, seed = 2025)
  
  complete(imp) %>%
    mutate(SAMPWEIGHT = round(SAMPWEIGHT / 100),
           smoker_type = type)
}

# --- Step 6: Run Imputation for Current and Former Smokers ---
current_complete <- prepare_and_impute(current_smokers, "current")
former_complete  <- prepare_and_impute(former_smokers, "former")

# --- Step 7: Combine, Calculate Pack-years, and Apply Logical Filters ---
lcrisks_final <- bind_rows(current_complete, former_complete) %>%
  mutate(
    pky = avecpd * smkyears / 20,
    valid_logic = case_when(
      smoker_type == "current" ~ (age - smkyears) >= 8,
      smoker_type == "former"  ~ (age - (smkyears + qtyears)) >= 8,
      TRUE ~ FALSE
    )
  ) %>%
  filter(
    valid_logic,
    pky >= 20,
    smoker_type == "current" | (smoker_type == "former" & qtyears <= 15)
  ) %>%
  select(-valid_logic, -smoker_type)

# --- Step 8: Expand by Sample Weight ---
expanded_data <- expandRows(lcrisks_final, "SAMPWEIGHT")

# --- Step 9: Convert Factor Variables to Numeric ---
factor_cols <- sapply(expanded_data, is.factor)
expanded_data[factor_cols] <- lapply(expanded_data[factor_cols], function(x) as.numeric(as.character(x)))

summary(expanded_data)
View(expanded_data)

write.csv(expanded_data, "expanded_data.csv", row.names = FALSE)

--------------------------------------------------------------------------------

# --- Step 10: Baseline Summary for Post-Imputation Data ---
  
  summarize_baseline_final <- function(data) {
    # Age group
    age_group <- cut(data$age, breaks = c(-Inf, 54, 59, 64, 69, 74, Inf),
                     labels = c("<55", "55–59", "60–64", "65–69", "70–74", "≥75"))
    age <- as.data.frame(table(age_group)) %>%
      rename(Category = age_group, Count = Freq) %>%
      mutate(
        Percent = round(100 * Count / sum(Count), 1),
        Variable = "Age",
        Count = as.character(Count),
        Percent = as.character(Percent)
      )
    
    # Sex
    sex <- data %>%
      mutate(Category = factor(female, levels = c(0, 1), labels = c("Male", "Female"))) %>%
      count(Category, name = "Count") %>%
      mutate(
        Percent = round(100 * Count / sum(Count), 1),
        Variable = "Sex",
        Count = as.character(Count),
        Percent = as.character(Percent)
      )
    
    # Race/Ethnicity
    race_eth <- data %>%
      mutate(Category = case_when(
        race == 0 ~ "Non-Hispanic White",
        race == 1 ~ "Non-Hispanic Black",
        race == 2 ~ "Hispanic",
        race == 3 ~ "Other Non-Hispanic",
        TRUE ~ "Missing"
      )) %>%
      count(Category, name = "Count") %>%
      mutate(
        Percent = round(100 * Count / sum(Count), 1),
        Variable = "Race/Ethnicity",
        Count = as.character(Count),
        Percent = as.character(Percent)
      )
    
    # Smoking Status
    smoking <- data %>%
      mutate(Category = case_when(
        qtyears == 0 ~ "Current",
        qtyears > 0 & qtyears <= 15 ~ "Former",
        TRUE ~ "Missing"
      )) %>%
      count(Category, name = "Count") %>%
      mutate(
        Percent = round(100 * Count / sum(Count), 1),
        Variable = "Smoking Status",
        Count = as.character(Count),
        Percent = as.character(Percent)
      )
    
    # Smoking Pack-Years
    pky_med <- median(data$pky, na.rm = TRUE)
    pky_q1 <- quantile(data$pky, 0.25, na.rm = TRUE)
    pky_q3 <- quantile(data$pky, 0.75, na.rm = TRUE)
    pky_missing <- sum(is.na(data$pky))
    smoking_pky <- tibble(
      Variable = "Smoking Pack-Years",
      Category = c("Median (IQR)", "Missing"),
      Count = c(
        paste0(round(pky_med, 1), " (", round(pky_q1, 1), "–", round(pky_q3, 1), ")"),
        as.character(pky_missing)
      ),
      Percent = as.character(c(NA, round(100 * pky_missing / nrow(data), 1)))
    )
    
    # BMI
    bmi_cat <- cut(data$bmi, breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
                   labels = c("Underweight", "Normal", "Overweight", "Obese"))
    bmi <- table(bmi_cat, useNA = "ifany")
    bmi_df <- data.frame(Category = names(bmi), Count = as.vector(bmi)) %>%
      mutate(
        Percent = round(100 * Count / sum(Count), 1),
        Category = ifelse(is.na(Category), "Missing", as.character(Category)),
        Variable = "BMI",
        Count = as.character(Count),
        Percent = as.character(Percent)
      )
    
    # Comorbidities
    comorbidity_summary <- function(var, label) {
      data %>%
        mutate(Category = case_when(
          .data[[var]] == 1 ~ "Yes",
          .data[[var]] == 0 ~ "No",
          TRUE ~ "Missing"
        )) %>%
        count(Category, name = "Count") %>%
        mutate(
          Percent = round(100 * Count / sum(Count), 1),
          Variable = label,
          Count = as.character(Count),
          Percent = as.character(Percent)
        )
    }
    
    comorbidities <- bind_rows(
      comorbidity_summary("copd", "COPD/emphysema"),
      comorbidity_summary("bron", "Chronic bronchitis"),
      comorbidity_summary("diab", "Diabetes"),
      comorbidity_summary("heartdisease", "Heart disease or myocardial infarction"),
      comorbidity_summary("stroke", "Stroke")
    )
    
    # Cancer history (fix for factor issue)
    cancer_summary <- data %>%
      mutate(prshist_num = as.numeric(as.character(prshist))) %>%
      mutate(Category = case_when(
        prshist_num >= 1 ~ "Yes",
        prshist_num == 0 ~ "No",
        TRUE ~ "Missing"
      )) %>%
      count(Category, name = "Count") %>%
      mutate(
        Percent = round(100 * Count / sum(Count), 1),
        Variable = "Cancer (≥1)",
        Count = as.character(Count),
        Percent = as.character(Percent)
      )
    
    # Combine everything
    bind_rows(
      age, sex, race_eth, smoking, smoking_pky,
      bmi_df, comorbidities, cancer_summary
    )
  }

lcrisks_plusperiod <- lcrisks_final %>%
  filter(year >= 2016 & year <= 2021)

baseline_table1 <- summarize_baseline_final(lcrisks_final)
baseline_table1$Year <- "Post-Imputation (2010–2023)"
View(baseline_table1)

baseline_table1_plusperiod <- summarize_baseline_final(lcrisks_plusperiod)
baseline_table1_plusperiod$Year <- "2016–2021 (PLuS Alignment)"
View(baseline_table1_plusperiod)

baseline_table1_expanded <- summarize_baseline_final(expanded_data)
baseline_table1_expanded$Year <- "Expanded Post-Imputation (2010–2023)"
View(baseline_table1_expanded)
