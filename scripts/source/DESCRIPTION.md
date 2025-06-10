# LCrisks Model Scripts

This folder contains modular R scripts and supporting data files for the LCrisks lung cancer microsimulation model. These components define the logic for histology assignment, disease progression, screening policies, treatment, and mortality.

---

## Script Descriptions

- **`LCSpecMortality.R`**  
  Defines lung cancer–specific mortality functions by histology and stage, using survival curves derived from U.S. population data.

- **`histology.R`**  
  Assigns histologic subtypes (e.g., adenocarcinoma, squamous cell, small cell) based on individual risk profiles and random sampling. Used for stratifying downstream treatment and survival outcomes.

- **`followup_new.R`**  
  Simulates clinical follow-up outcomes after lung cancer detection, including timing of diagnosis, stage progression, and transitions between detection states.

- **`treatment.R`**  
  Models treatment-related survival effects based on histology and stage at diagnosis. Incorporates differential mortality based on whether individuals receive surgery or systemic therapy.

- **`lifetable_LCRAT.R`**  
  Provides modified life table functions for estimating all-cause mortality using age, sex, and comorbidity-adjusted demographic data. Used to simulate competing risks of death unrelated to lung cancer.

- **`screening_core.R`**  
  Implements the core lung cancer screening logic, including eligibility determination, screening frequency, detection probability, and downstream outcomes under the screening arm.

- **`screening_newages.R`**  
  A flexible variant of the screening module supporting alternative age-based screening policies or guidelines. Allows testing of custom age inclusion rules.

- **`screening.R`**  
  Integrates PLCO risk-based eligibility, smoking history, and behavioral updates into a comprehensive screening simulation. Supports more granular modeling of personalized screening schedules. Replaces or supplements previous screening scripts.

- **`stage_pst.R`**  
  Assigns clinical stage at diagnosis for screen-detected and non-screen-detected cancers. Accounts for lead-time bias and stage-shift effects in the presence of screening.

- **`global_lungrads.R`**  
  Stores global variables, thresholds, and simulation control flags used across LCrisks modules (e.g., Lung-RADS cutoff values, simulation constants).

---

## Supporting Data Files

- **`hist_mod_sm.csv`**  
  Contains estimated coefficients from a multinomial logistic regression predicting histology subtype. Used in `histology.R` to probabilistically assign histology during simulation.  
  Columns:
  - `Variable`: Predictor name (e.g., `female`, `copd`, `age`)
  - `Response`: Histology category
  - `Estimate`: Coefficient
  - `StdErr`, `WaldChiSq`, `ProbChiSq`: Statistical measures for each predictor

- **`lung cancer specific mortality.csv`**  
  Contains survival coefficients or parameters by histology and stage.  
  - Used in: `LCSpecMortality.R`, referenced from `global_lungrads.R`
  - Role: Defines lung cancer–specific mortality curves for microsimulation.

---

For questions or edits, contact the model developers or refer to the full documentation in the project root.
