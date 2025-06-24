
####### NHIS Mock Table #######
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
  cat("Chi-square p-value (screening, exclude all OD):",
      format.pval(chisq2_scrn_exOD$p.value, digits = 3, eps = .Machine$double.eps), "\n")
  
  
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
  cat("Chi-square p-value (screening, exOD):", 
      format.pval(chisq5_scrn_exOD$p.value, digits = 3, eps = .Machine$double.eps), "\n")
  
  
--------------------------------------------------------------------------------  
## Table 6 #    
    
    
    
    