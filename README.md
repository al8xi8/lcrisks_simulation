# Comorbidity_Project

🧠 **Incorporating Comorbidity Data into Lung Cancer Risk Modeling Using NHIS: A Microsimulation-Based Approach**

This project aims to improve the realism and precision of lung cancer screening evaluations by integrating individual-level comorbidity data into a validated microsimulation model. Using survey data from the Integrated Public Use Microdata Series (IPUMS) National Health Interview Survey (NHIS) spanning 2010–2023, the project constructs a synthetic U.S. population that reflects diverse health profiles, including smoking history, demographic characteristics, and chronic disease burden.

The core objective is to simulate lung cancer incidence and outcomes under different screening policies—such as USPSTF guidelines—while accounting for competing risks of death from comorbid conditions like COPD, heart disease, and diabetes. By incorporating these comorbidities into the LCrisks lung cancer microsimulation framework, the project evaluates how real-world health complexities influence screening effectiveness, survival benefit, and potential harms.

This work contributes to more equitable and context-aware policy modeling by aligning simulated screening strategies with population-level health realities. It provides a foundation for tailoring lung cancer screening recommendations based on individuals’ health risk profiles, especially in aging and comorbidity-heavy populations.

---

📦 **Part 1: Dataset Preparation and Variable Recoding**

- Extracted relevant variables from NHIS datasets and harmonized inputs across 2010, 2015, and 2020 survey years.
- Recoded 23 LCrisks model input variables (e.g., smoking history, BMI, comorbidities) from raw NHIS data.
- Derived an additional 24 variables including pack-years, model eligibility flags, and logical filters.
- Generated a final analytic dataset (`lcrisks_final.csv`) and performed weight-based expansion to simulate a U.S. population cohort.

---

🧮 **Part 2: Microsimulation Using LCrisks Framework**

- Implemented the LCrisks lung cancer microsimulation model with comorbidity-adjusted input data.
- Simulated lung cancer incidence, histology, stage, screening detection, and mortality outcomes under screening and no-screening scenarios.
- Used batch-split datasets (`nhis_000.csv` to `nhis_191.csv`) for efficient simulation at scale.

---

📊 **Part 3: Policy Simulation and Subgroup Analysis**

- Evaluated lung cancer screening policies using expanded NHIS populations with and without comorbidities.
- Analyzed variation in outcomes by age, sex, smoking status, and comorbidity burden.
- Assessed potential harms and benefits of screening in high-risk subgroups using simulation outputs.

