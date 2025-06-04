
rm(list = ls())

# --- Load Libraries and Packages ---
library(readr)
library(dplyr)
library(tidyr)
install.packages("here")
library(here)


# --- Load Data ---
getwd()
setwd("/Users/al8xi8/Documents/GitHub/lcrisks_simulation")

# GitHub
zip_path <- "/Users/al8xi8/Documents/GitHub/lcrisks_simulation/datasets/nhis_lcrisks.csv.zip"
csv_path <- "/Users/al8xi8/Documents/GitHub/lcrisks_simulation/datasets/nhis_lcrisks.csv"
if (!file.exists(csv_path)) {
  unzip(zip_path, exdir = "/Users/al8xi8/Documents/GitHub/lcrisks_simulation/datasets")
}
library(readr)
nhis_lcrisks <- read_csv(csv_path)

# optional: download and unzip data manually on Desktop
nhis_lcrisks <- read_csv("~/Desktop/nhis_lcrisks.csv")



# --- Recode and Rename Variables ---
# column 1: age
nhis_lcrisks$age <- nhis_lcrisks$AGE
nhis_lcrisks$age[nhis_lcrisks$age %in% c(997, 998, 999)] <- NA

# column 2: female
nhis_lcrisks$female <- ifelse(nhis_lcrisks$SEX %in% c(7, 8, 9), NA,
                              ifelse(nhis_lcrisks$SEX == 2, 1,
                                     ifelse(nhis_lcrisks$SEX == 1, 0, NA)))

# column 3: smkyears
current <- which(!is.na(nhis_lcrisks$SMOKESTATUS2) & nhis_lcrisks$SMOKESTATUS2 %in% c(10, 11, 12, 13))
former  <- which(!is.na(nhis_lcrisks$SMOKESTATUS2) & nhis_lcrisks$SMOKESTATUS2 == 20)
never   <- which(!is.na(nhis_lcrisks$SMOKESTATUS2) & nhis_lcrisks$SMOKESTATUS2 == 30)
nhis_lcrisks$smkyears <- NA
# Current smokers
nhis_lcrisks$smkyears[current] <- nhis_lcrisks$age[current] - nhis_lcrisks$SMOKAGEREG[current]
# Former smokers
nhis_lcrisks$smkyears[former] <- nhis_lcrisks$age[former] - nhis_lcrisks$SMOKAGEREG[former] - nhis_lcrisks$QUITYRS[former]
# Never smokers
nhis_lcrisks$smkyears[never] <- 0
# Set negative values to NA
nhis_lcrisks$smkyears[nhis_lcrisks$smkyears < 0] <- NA

# column 4: quityrs
nhis_lcrisks$qtyears <- NA
currentyearsquit <- which(!is.na(nhis_lcrisks$QUITYRS) & nhis_lcrisks$QUITYRS >= 0 & nhis_lcrisks$QUITYRS <= 70)
nhis_lcrisks$qtyears[currentyearsquit] <- nhis_lcrisks$QUITYRS[currentyearsquit]
current <- which(!is.na(nhis_lcrisks$SMOKESTATUS2) & nhis_lcrisks$SMOKESTATUS2 %in% c(10, 11, 12, 13))
nhis_lcrisks$qtyears[current] <- 0

# column 5: avecpd
nhis_lcrisks$avecpd <- NA
# Current smokers
current <- nhis_lcrisks$SMOKESTATUS2 %in% c(10, 11, 12, 13)
nhis_lcrisks$avecpd[current] <- ifelse(
  nhis_lcrisks$CIGSDAY1[current] >= 1 & nhis_lcrisks$CIGSDAY1[current] <= 95,
  nhis_lcrisks$CIGSDAY1[current],
  ifelse(
    nhis_lcrisks$CIGSDAY2[current] >= 1 & nhis_lcrisks$CIGSDAY2[current] <= 95,
    nhis_lcrisks$CIGSDAY2[current],
    NA))
# Former smokers
former <- nhis_lcrisks$SMOKESTATUS2 == 20
nhis_lcrisks$avecpd[former] <- ifelse(
  nhis_lcrisks$CIGSDAYFS[former] >= 1 & nhis_lcrisks$CIGSDAYFS[former] <= 94,
  nhis_lcrisks$CIGSDAYFS[former],
  NA)
# Never smokers
never <- nhis_lcrisks$SMOKESTATUS2 == 30
nhis_lcrisks$avecpd[never] <- 0

# column 6: race
nhis_lcrisks$race <- NA
nhis_lcrisks$race[nhis_lcrisks$HISPYN == 1 & nhis_lcrisks$RACENEW == 100] <- 0  # NH White
nhis_lcrisks$race[nhis_lcrisks$HISPYN == 1 & nhis_lcrisks$RACENEW == 200] <- 1  # NH Black
nhis_lcrisks$race[nhis_lcrisks$HISPYN == 2] <- 2                                # Hispanic
nhis_lcrisks$race[nhis_lcrisks$HISPYN == 1 & nhis_lcrisks$RACENEW %in% c(300, 400, 500, 510, 520, 540, 541, 542)] <- 3  # Other NH
nhis_lcrisks$race[nhis_lcrisks$HISPYN %in% c(7, 8, 9) | nhis_lcrisks$RACENEW %in% c(530, 997, 998, 999)] <- NA

# column 7: copd
nhis_lcrisks$copd <- NA
copdev <- nhis_lcrisks$COPDEV
emphyev <- nhis_lcrisks$EMPHYSEMEV
copdyears1 <- which(nhis_lcrisks$YEAR >= 2010 & nhis_lcrisks$YEAR <= 2018)
copdyears2 <- which(nhis_lcrisks$YEAR >= 2019 & nhis_lcrisks$YEAR <= 2023)
# COPDEV for 2019–2023
nhis_lcrisks$copd[copdyears2[copdev[copdyears2] == 2]] <- 1
nhis_lcrisks$copd[copdyears2[copdev[copdyears2] == 1]] <- 0
nhis_lcrisks$copd[copdyears2[copdev[copdyears2] %in% c(0, 7, 8, 9)]] <- NA
# EMPHYSEMEV + COPDEV for 2010–2018
nhis_lcrisks$copd[copdyears1[emphyev[copdyears1] == 2 | copdev[copdyears1] == 2]] <- 1
nhis_lcrisks$copd[copdyears1[emphyev[copdyears1] == 1 & copdev[copdyears1] == 1 ]] <- 0
nhis_lcrisks$copd[copdyears1[emphyev[copdyears1] %in% c(0, 7, 8, 9) & copdev[copdyears1] %in% c(0, 7, 8, 9) ]] <- NA

# column 8: fmhist
nhis_lcrisks$fmhist <- NA
parents <- nhis_lcrisks[, c("BFLGCAN", "BMLGCAN")]
parents[parents %in% c(0, 7, 8, 9)] <- NA
parent_positive <- rowSums(parents == 2, na.rm = TRUE)
parent_missing <- rowSums(is.na(parents))
nhis_lcrisks$fmhist <- ifelse(parent_missing == 2, NA, parent_positive)

# column 9: bmi
nhis_lcrisks$bmi <- nhis_lcrisks$BMICALC
nhis_lcrisks$BMICALC[nhis_lcrisks$BMICALC %in% c(0, 996.0)] <- NA
nhis_lcrisks$bmi[nhis_lcrisks$bmi %in% c(0.0, 996.0)] <- NA

# column 10: edu
nhis_lcrisks$edu <- NA
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(000, 504, 996, 997, 998, 999)] <- NA
# Graduate school
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(500, 501, 502, 503, 505)] <- 6
# Bachelor's degree
nhis_lcrisks$edu[nhis_lcrisks$EDUC == 400] <- 5
# Associate's degree/some college
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(300, 301)] <- 4
# Post HS, no college
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(302, 303)] <- 3
# HS graduate
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(200, 201, 202)] <- 2
# <12 grade
nhis_lcrisks$edu[nhis_lcrisks$EDUC %in% c(100:116)] <- 1

# column 11-22
comorbidity_vars <- list(
  CANCEREV     = "prshist",       # column: 11
  HYPERTENEV   = "hypertension",  # column: 12
  CHEARTDIEV   = "chd",           # column: 13
  ANGIPECEV    = "angina",        # column: 14
  HEARTATTEV   = "heartattack",   # column: 15
  HEARTCONEV   = "heartdisease",  # column: 16
  STROKEV      = "stroke",        # column: 17
  DIABETICEV   = "diab",          # column: 18*
  CRONBRONYR   = "bron",          # column: 19
  KIDNEYWKYR   = "kidney",        # column: 20
  LIVERCONYR   = "liver",         # column: 21
  EQUIPMENT    = "spaceq"         # column: 22
)

for (orig_var in names(comorbidity_vars)) {
  new_var <- comorbidity_vars[[orig_var]]
  nhis_lcrisks[[new_var]] <- NA
  
  if (orig_var == "DIABETICEV") {
    nhis_lcrisks[[new_var]][nhis_lcrisks[[orig_var]] %in% c(1, 3)] <- 0  # 1 = No, 3 = Borderline → No
    nhis_lcrisks[[new_var]][nhis_lcrisks[[orig_var]] == 2] <- 1          # 2 = Yes
  } else {
    nhis_lcrisks[[new_var]][nhis_lcrisks[[orig_var]] == 2] <- 1
    nhis_lcrisks[[new_var]][nhis_lcrisks[[orig_var]] == 1] <- 0}}

# column 23: year
nhis_lcrisks$year <- nhis_lcrisks$YEAR

# Additional: pack-years
nhis_lcrisks$pky <- NA
valid_pky <- !is.na(nhis_lcrisks$avecpd) & !is.na(nhis_lcrisks$smkyears)
nhis_lcrisks$pky[valid_pky] <- (nhis_lcrisks$avecpd[valid_pky] * nhis_lcrisks$smkyears[valid_pky]) / 20
nhis_lcrisks$pky[nhis_lcrisks$pky < 0] <- NA
