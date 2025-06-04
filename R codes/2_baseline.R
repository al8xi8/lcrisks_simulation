
# --- Eligible Participants ---
# Full Year
lcrisks_base <- nhis_lcrisks %>%
  filter(
    year >= 2010 & year <= 2023,                 # Restrict to years in your dataset
    age >= 50 & age <= 80,                       # Age criterion
    pky >= 20,                                   # Pack-year threshold
    (SMOKESTATUS2 %in% c(10, 11, 12, 13) |       # Current smokers (detailed codes)
       (SMOKESTATUS2 == 20 & qtyears <= 15)))    # Former smokers who quit ≤15 years ago

# Primary Year: 2010 only
lcrisks_2010 <- lcrisks_base %>%
  filter(year == 2010)

# Primary Year: 2015 only
lcrisks_2015 <- lcrisks_base %>%
  filter(year == 2015)

# Primary Year: 2020 only
lcrisks_2020 <- lcrisks_base %>%
  filter(year == 2020)

# Primary Year: 2010, 2015, and 2020 combined
lcrisks_3core <- lcrisks_base %>%
  filter(year %in% c(2010, 2015, 2020))


# --- Summary Function ---
summarize_baseline <- function(data, label) {
  # Age
  age_group <- cut(data$age, breaks = c(-Inf, 54, 59, 64, 69, 74, Inf),
                   labels = c("<55", "55–59", "60–64", "65–69", "70–74", "≥75"))
  age <- as.data.frame(table(age_group)) %>%
    rename(Category = age_group, Count = Freq) %>%
    mutate(Percent = round(100 * Count / sum(Count), 1),
           Variable = "Age",
           Count = as.character(Count),
           Percent = as.character(Percent))
  
  # Sex
  sex <- data %>%
    mutate(Category = factor(female, levels = c(0, 1), labels = c("Male", "Female"))) %>%
    count(Category, name = "Count") %>%
    mutate(Percent = round(100 * Count / sum(Count), 1),
           Variable = "Sex",
           Count = as.character(Count),
           Percent = as.character(Percent))
  
  # Race/Ethnicity
  race_eth <- data %>%
    mutate(Category = case_when(
      RACENEW == 300 ~ "American Indian or Alaska Native",
      RACENEW == 400 ~ "Asian",
      RACENEW == 200 ~ "Black",
      RACENEW %in% c(500, 510, 520, 542) ~ "Native Hawaiian or other Pacific Islander",
      RACENEW == 100 ~ "White",
      RACENEW %in% c(540, 541) ~ "More than 1 racial and ethnic group",
      RACENEW %in% c(530, 997, 998, 999) ~ "Unknown/missing",
      TRUE ~ NA_character_
    )) %>%
    count(Category, name = "Count") %>%
    mutate(Percent = round(100 * Count / sum(Count), 1),
           Variable = "Race/Ethnicity",
           Count = as.character(Count),
           Percent = as.character(Percent))
  
  # Hispanic/Latino
  hisp_lat <- data %>%
    mutate(Category = case_when(
      HISPYN == 2 ~ "Hispanic/Latino",
      HISPYN == 1 ~ "Neither",
      HISPYN %in% c(7, 8, 9) ~ "Unknown/missing",
      TRUE ~ NA_character_
    )) %>%
    count(Category, name = "Count") %>%
    mutate(Percent = round(100 * Count / sum(Count), 1),
           Variable = "Hispanic/Latino",
           Count = as.character(Count),
           Percent = as.character(Percent))
  
  # Smoking status
  smoking <- data %>%
    mutate(Category = case_when(
      SMOKESTATUS2 %in% c(10, 11, 12, 13) ~ "Current",
      SMOKESTATUS2 == 20 ~ "Former",
      SMOKESTATUS2 == 30 ~ "Never",
      SMOKESTATUS2 %in% c(40, 90) ~ "Missing",
      TRUE ~ NA_character_
    )) %>%
    count(Category, name = "Count") %>%
    mutate(Percent = round(100 * Count / sum(Count), 1),
           Variable = "Smoking Status",
           Count = as.character(Count),
           Percent = as.character(Percent))
  
  # Smoking Pack-years
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
    Percent = c(NA, as.character(round(100 * pky_missing / nrow(data), 1)))
  )
  
  # BMI
  bmi_cat <- cut(data$bmi, breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
                 labels = c("Underweight", "Normal", "Overweight", "Obese"))
  bmi <- table(bmi_cat, useNA = "ifany")
  bmi_df <- data.frame(Category = names(bmi), Count = as.vector(bmi)) %>%
    mutate(Percent = round(100 * Count / sum(Count), 1),
           Category = ifelse(is.na(Category), "Missing", as.character(Category)),
           Variable = "BMI",
           Count = as.character(Count),
           Percent = as.character(Percent))
  
  # Comorbidities
  comorbidity_summary <- function(var, label) {
    data %>%
      mutate(Category = case_when(
        .data[[var]] == 1 ~ "Yes",
        .data[[var]] == 0 ~ "No",
        TRUE ~ "Missing"
      )) %>%
      count(Category, name = "Count") %>%
      mutate(Percent = round(100 * Count / sum(Count), 1),
             Variable = label,
             Count = as.character(Count),
             Percent = as.character(Percent))
  }
  
  comorbidities <- bind_rows(
    comorbidity_summary("copd", "COPD/chronic bronchitis/emphysema"),
    comorbidity_summary("diab", "Diabetes"),
    comorbidity_summary("heartdisease", "Heart disease or myocardial infarction"),
    comorbidity_summary("stroke", "Stroke")
  )
  
  # Cancer (≥1)
  cancer_summary <- data %>%
    mutate(Category = case_when(
      prshist >= 1 ~ "Yes",
      prshist == 0 ~ "No",
      TRUE ~ "Missing"
    )) %>%
    count(Category, name = "Count") %>%
    mutate(Percent = round(100 * Count / sum(Count), 1),
           Variable = "Cancer (≥1)",
           Count = as.character(Count),
           Percent = as.character(Percent))
  
  # Bind all
  summary_all <- bind_rows(
    age, sex, race_eth, hisp_lat, smoking,
    smoking_pky, bmi_df, comorbidities, cancer_summary
  )
  
  return(summary_all)
}

# --- Create Summary Table ---
baseline_nhis <- summarize_baseline(lcrisks_base)
baseline_2010   <- summarize_baseline(lcrisks_2010)
baseline_2015   <- summarize_baseline(lcrisks_2015)
baseline_2020   <- summarize_baseline(lcrisks_2020)
baseline_3core  <- summarize_baseline(lcrisks_3core)

View(baseline_nhis)
View(baseline_2010)
View(baseline_2015)
View(baseline_2020)
View(baseline_3core)

baseline_nhis$Year <- "2010–2023"
baseline_2010$Year <- "2010"
baseline_2015$Year <- "2015"
baseline_2020$Year <- "2020"
baseline_3core$Year <- "2010/2015/2020"

baseline_combined_long <- bind_rows(
  baseline_nhis,
  baseline_2010,
  baseline_2015,
  baseline_2020,
  baseline_3core)

baseline_wide <- baseline_combined_long %>%
  pivot_wider(
    id_cols = c(Variable, Category),
    names_from = Year,
    values_from = c(Count, Percent),
    names_glue = "{.value}_{Year}")

View(baseline_wide)
