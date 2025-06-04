# LCrisks Model Scripts

This folder contains modular R scripts that define the key components of the LCrisks lung cancer microsimulation model.

## File Descriptions

- **`LCSpecMortality.R`**  
  Defines lung cancer–specific mortality functions by histology and stage, using survival curves derived from U.S. population data.

- **`histology.R`**  
  Assigns histologic subtypes (e.g., adenocarcinoma, squamous cell, small cell) based on individual risk profiles and random sampling. Used for stratifying downstream treatment and survival outcomes.

- **`followup_new.R`**  
  Simulates clinical follow-up outcomes after lung cancer detection, including timing of diagnosis, stage progression, and transitions between detection states.

- **`treatment.R`**  
  Models treatment-related survival effects based on histology and stage at diagnosis. Incorporates differential mortality based on whether individuals receive surgery or systemic therapy.

- **`lifetable_LCRAT.R`**  
  Provides modified life table functions for estimating all-cause mortality using age, sex, and comorbidity-adjusted demographic data. Used to simulate competing risk of non-lung cancer death.

- **`screening_core.R`**  
  Implements the core lung cancer screening logic, including eligibility determination, screening frequency, detection probability, and downstream outcomes under the screening arm.

- **`screening_newages.R`**  
  A flexible variant of the screening module supporting alternative age-based screening policies or guidelines. Allows testing of custom age inclusion rules.

- **`stage_pst.R`**  
  Assigns clinical stage at diagnosis for screen-detected and non-screen-detected cancers. Accounts for lead-time bias and stage-shift effects in the presence of screening.

- **`global_lungrads.R`**  
  Stores shared global variables and parameter settings for the LCrisks model, including Lung-RADS thresholds, simulation constants, and control switches used across scripts.
