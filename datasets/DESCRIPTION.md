# Datasets Folder

This folder contains input data for the LCrisks microsimulation model, including raw NHIS data, recoded inputs, and expanded population files.

## Compressed Files

To use: unzip these files manually or with `unzip` or `gunzip` in your terminal.

- **`nhis_lcrisks.csv.zip`**  
  Compressed raw NHIS dataset (2010–2023) containing unprocessed variables from IPUMS NHIS. This file includes sample weights, demographics, smoking history, and comorbidities. Unzip before preprocessing or recoding.

- **`expanded_data.csv.zip`**  
  Compressed version of the final weight-expanded dataset derived from `lcrisks_final.csv`. Each individual is duplicated according to their survey weight to simulate a nationally representative cohort. Used as the primary input to the LCrisks simulation model.

## Split NHIS Files

- **`nhis_000.csv` to `nhis_191.csv`**  
  These 192 files are partitioned subsets of the full `expanded_data.csv` dataset. Each file contains a segment of the expanded, imputed NHIS population to enable:
  - Parallel processing
  - Reduced memory usage
  - Scalable simulation workflows

  All files follow the same variable structure and column order defined in `column_order_reference.csv`, ensuring compatibility with LCrisks model functions.
