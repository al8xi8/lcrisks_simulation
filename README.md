# Comorbidity_Project

**Incorporating Comorbidity Data into Lung Cancer Risk Modeling Using NHIS: A Microsimulation-Based Approach**

This project integrates individual-level comorbidity data into the LCrisks microsimulation framework to improve the realism and policy relevance of lung cancer screening evaluations. Using harmonized data from the Integrated Public Use Microdata Series (IPUMS) National Health Interview Survey (NHIS), spanning 2010–2023, the project simulates a representative U.S. cohort reflecting diverse smoking behavior, demographics, and chronic disease burden.

The core objective is to evaluate lung cancer incidence, detection, and mortality under screening versus no-screening scenarios, while accounting for competing mortality risks from conditions like COPD, diabetes, and cardiovascular disease. By incorporating comorbidities directly into the model, the project enables nuanced analysis of screening effectiveness, equity implications, and subgroup differences.

---

## 📦 Part 1: Dataset Preparation and Variable Recoding

- Extracted and harmonized NHIS variables relevant to lung cancer and comorbidity modeling from the 2010, 2015, and 2020 samples.
- Recoded 23 input variables used in the LCrisks model (e.g., age, sex, smoking status, COPD, diabetes).
- Created 24 derived variables, including:
  - Pack-years
  - Smoking duration
  - Eligibility flags
  - Clean indicators for 12 comorbidities: `prshist`, `hypertension`, `chd`, `angina`, `heartattack`, `heartdisease`, `stroke`, `diab`, `bron`, `kidney`, `liver`, `spaceq`
- Applied eligibility criteria: age 50–80, ≥20 pack-years, quit ≤15 years.
- Generated the final cleaned dataset `lcrisks_final.csv` with 72 columns.
- Performed weight expansion to create a synthetic U.S. population.
- Split the final expanded data into 192 subsets (`nhis_000.csv` to `nhis_191.csv`) for scalable simulation.

---

## 🧮 Part 2: Microsimulation Using the LCrisks Framework

- Adapted the LCrisks simulation engine to accept the NHIS-based synthetic population as input.
- Confirmed all model functions (`lifetable_LCRAT.R`, `screening.R`, `followup_new.R`, `treatment.R`, `stage_pst.R`, etc.) were correctly configured.
- Verified that all 24 input columns are numeric and consistent with model requirements.
- Ran the model on all 192 data chunks for both:
  - **No screening**
  - **Lifetime screening** (i.e., individuals are screened every year starting at eligibility age)
- Each individual’s cancer history, diagnosis, stage, treatment, and survival outcomes were simulated using comorbidity-adjusted natural history and mortality functions.
- Output files (`no_scrn_XXX.csv`, `scrn_XXX.csv`) were successfully generated and stored in the appropriate output folders.

---

## 📊 Part 3: Policy Evaluation and Output Table Generation

- Simulated outcomes are being used to build final tables for publication:
  - **Table 1**: Baseline characteristics by comorbidity burden
  - **Table 2**: Lung cancer incidence and mortality under lifetime vs. 3-round screening (in development)
  - **Table 5**: Cancer stage distribution comparison under different screening frequencies
  - **Table 6**: Histology types by stage and comorbidity levels
- Current results reflect **lifetime screening**. Additional simulations will evaluate **3-round screening**, where eligible individuals receive screening only at 3 pre-specified ages.
- Comparison between lifetime and 3-round screening will highlight the trade-offs in benefit and burden under resource-constrained or targeted screening policies.

---

## 📁 Key Project Files

- `nhis_lcrisks.csv`: Raw NHIS dataset after harmonization.
- `lcrisks_final.csv`: Final cleaned and recoded input dataset with 72 variables.
- `expanded_data.csv`: Weight-expanded version of `lcrisks_final.csv`.
- `split_nhis/nhis_000.csv` to `nhis_191.csv`: Subsets of the expanded dataset for batch simulation.
- `screening.R`, `lifetable_LCRAT.R`, `LCSpecMortality.R`, etc.: Core scripts powering the LCrisks microsimulation model.
- `output_noscrn/`: Simulation results for the **no-screening** scenario.
- `output_scrn_lifetime/`: Outputs from **lifetime screening** simulation (continuous annual screening from eligibility onward).
- `output_scrn_3scrns/`: Outputs from **3-round screening** simulation (screening occurs at fixed intervals, e.g., ages 55, 60, 65).

These outputs will be compared across Table 2, Table 5, and Table 6 to assess differences in outcomes between lifetime and limited-round screening strategies.


---

## ✅ Summary

This project builds a robust, population-based microsimulation to examine the real-world impact of lung cancer screening while accounting for comorbidity-related competing risks. By modeling both lifetime and limited screening strategies, it aims to inform screening policy decisions that are both effective and equitable.
