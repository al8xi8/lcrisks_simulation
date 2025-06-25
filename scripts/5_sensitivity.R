
####### NHIS Mock Table #######

# Count of all overdiagnosed cases
od1_total <- sum(scrn$Overdiagnosis == 1, na.rm = TRUE)

# Count of screen-detected overdiagnosed cases (OD = 1 & Detected = 1)
od1_dec1 <- sum(scrn$Overdiagnosis == 1 & scrn$Detected == 1, na.rm = TRUE)

# Count of non-screen-detected overdiagnosed cases (OD = 1 & Detected = 0)
od1_dec0 <- sum(scrn$Overdiagnosis == 1 & scrn$Detected == 0, na.rm = TRUE)

# Output
cat("OD = 1 (total overdiagnosed):", od1_total, "\n")
cat("OD = 1 & Detected = 1 (screen-detected OD):", od1_dec1, "\n")
cat("OD = 1 & Detected = 0 (non-screen-detected OD):", od1_dec0, "\n")



#### --- Sensitivity Analysis for "scrn" (Screening) --- ###
--## Table 1 #    
  
  
--------------------------------------------------------------------------------  
## Table 2 #

### (Sensitivity) — Exclude OD=1 & Detected=1
  table2_scrn_exOD <- scrn %>%
    filter(
      !is.na(Stage.cat),
      !is.na(comorb_cat),
      !(Overdiagnosis == 1 & Detected == 1)  # Exclude confirmed overdiagnosed detected cases
    ) %>%
    count(Stage.cat, comorb_cat) %>%
    group_by(comorb_cat) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      formatted = paste0(n, " (", pct, "%)")
    ) %>%
    ungroup() %>%
    select(Stage.cat, comorb_cat, formatted) %>%
    tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)")
  
  ### Chi-square test (Sensitivity)
  chisq2_scrn_exOD <- scrn %>%
    filter(
      !is.na(Stage.cat),
      !is.na(comorb_cat),
      !(Overdiagnosis == 1 & Detected == 1)
    ) %>%
    {chisq.test(table(.$Stage.cat, .$comorb_cat))}
  
  ### Output (Sensitivity)
  print(table2_scrn_exOD)
  cat("Chi-square p-value (screening, exclude screen detected overdiagnosis):",
      format.pval(chisq2_scrn_exOD$p.value, digits = 3, eps = .Machine$double.eps), "\n")
  
  
### (TRUE Sensitivity) — Exclude ALL Overdiagnosed Cases (OD = 1)
  table2_scrn_trueSens <- scrn %>%
    filter(
      !is.na(Stage.cat),
      !is.na(comorb_cat),
      Overdiagnosis != 1  # Exclude ALL overdiagnosed cases
    ) %>%
    count(Stage.cat, comorb_cat) %>%
    group_by(comorb_cat) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      formatted = paste0(n, " (", pct, "%)")
    ) %>%
    ungroup() %>%
    select(Stage.cat, comorb_cat, formatted) %>%
    tidyr::pivot_wider(
      names_from = comorb_cat,
      values_from = formatted,
      values_fill = "0 (0%)"
    )
  
  ### Chi-square test (TRUE Sensitivity)
  chisq2_scrn_trueSens <- scrn %>%
    filter(
      !is.na(Stage.cat),
      !is.na(comorb_cat),
      Overdiagnosis != 1
    ) %>%
    {chisq.test(table(.$Stage.cat, .$comorb_cat))}
  
  ### Output (TRUE Sensitivity)
  print(table2_scrn_trueSens)
  cat("Chi-square p-value (screening, exclude ALL overdiagnosed cases):",
      format.pval(chisq2_scrn_trueSens$p.value, digits = 3, eps = .Machine$double.eps), "\n")
  
--------------------------------------------------------------------------------  
## Table 3 # all cost mortality keep stage 0
    
    
--------------------------------------------------------------------------------
## Table 4 # lung cancer diagnosis keep stage 0
    
    
--------------------------------------------------------------------------------
## Table 5 #
    
### Table 5 (Sensitivity) — Exclude OD=1 & Detected=1
  table5_scrn_exOD <- scrn %>%
    filter(
      !is.na(Histology.cat),
      !is.na(comorb_cat),
      !(Overdiagnosis == 1 & Detected == 1)  # Exclude confirmed overdiagnosed detected cases
    ) %>%
    count(Histology.cat, comorb_cat) %>%
    group_by(comorb_cat) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      formatted = paste0(n, " (", pct, "%)")
    ) %>%
    ungroup() %>%
    select(Histology.cat, comorb_cat, formatted) %>%
    tidyr::pivot_wider(names_from = comorb_cat, values_from = formatted, values_fill = "0 (0%)")
  
  ### Chi-square test (Sensitivity)
  chisq5_scrn_exOD <- scrn %>%
    filter(
      !is.na(Histology.cat),
      !is.na(comorb_cat),
      !(Overdiagnosis == 1 & Detected == 1)
    ) %>%
    {chisq.test(table(.$Histology.cat, .$comorb_cat))}
  
  ### Output (Sensitivity)
  print(table5_scrn_exOD)
  cat("Chi-square p-value (screening, screen detected overdiagnosis):", 
      format.pval(chisq5_scrn_exOD$p.value, digits = 3, eps = .Machine$double.eps), "\n")


### (TRUE Sensitivity) — Exclude ALL Overdiagnosed Cases (OD = 1)
  table5_scrn_trueSens <- scrn %>%
    filter(
      !is.na(Histology.cat),
      !is.na(comorb_cat),
      Overdiagnosis != 1  # Exclude ALL overdiagnosed cases
    ) %>%
    count(Histology.cat, comorb_cat) %>%
    group_by(comorb_cat) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      formatted = paste0(n, " (", pct, "%)")
    ) %>%
    ungroup() %>%
    select(Histology.cat, comorb_cat, formatted) %>%
    tidyr::pivot_wider(
      names_from = comorb_cat,
      values_from = formatted,
      values_fill = "0 (0%)"
    )
  
  ### Chi-square test (TRUE Sensitivity)
  chisq5_scrn_trueSens <- scrn %>%
    filter(
      !is.na(Histology.cat),
      !is.na(comorb_cat),
      Overdiagnosis != 1
    ) %>%
    {chisq.test(table(.$Histology.cat, .$comorb_cat))}
  
  ### Output (TRUE Sensitivity)
  print(table5_scrn_trueSens)
  cat("Chi-square p-value (screening, exclude ALL overdiagnosed cases):",
      format.pval(chisq5_scrn_trueSens$p.value, digits = 3, eps = .Machine$double.eps), "\n")
  
  
--------------------------------------------------------------------------------  
## Table 6 #    
    
    
    
    