# t-14-3-09.R
#   CDISC Pilot Table 14-3.09

library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(huxtable)
library(pharmaRTF)

library(safetyData)

# source('./programs/config.R')
source('~/Clinical-Tables-in-R-with-gt/data_workflow/funcs.R')

the_date <- as.character(Sys.Date())

# Read in the ADLB datasets ----
# read_xpt(glue("{adam_lib}/adadas.xpt"))
adas <- adam_adqsadas %>%
  filter(EFFFL == "Y" & PARAMCD == 'ACTOT' & ANL01FL == 'Y' & SEX == "F")

# Calculate the header Ns ----
header_n <- adas %>%
  distinct(USUBJID, TRTP, TRTPN) %>%
  get_header_n(TRTP, TRTPN)

column_headers <- header_n %>%
  select(-N) %>%
  pivot_wider(names_from = TRTPN, values_from=labels) %>%
  mutate(rowlbl1 = '')

# Run each group
summary_portion <- bind_rows(summary_data(adas, AVAL, 0,  'Baseline'),
                             summary_data(adas, AVAL, 24, 'Week 24'),
                             summary_data(adas, CHG,  24, 'Change from Baseline')) %>%
  pad_row()

# Gather the model data
model_portion <- efficacy_models(adas, 'CHG', 24)

final <- bind_rows(column_headers, summary_portion, model_portion) %>%
  select(rowlbl1, `0`, `54`, `81`)

write_csv(final, "~/Clinical-Tables-in-R-with-gt/data/final_14309.csv")




