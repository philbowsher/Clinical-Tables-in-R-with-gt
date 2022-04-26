# t-14-5-01.R
#   CDISC Pilot Table 14-5.01

library(glue)
library(tidyverse)
# library(haven)
library(pharmaRTF)

library(safetyData)

# source('./programs/config.R')
source('~/Clinical-Tables-in-R-with-gt/data_workflow/funcs.R')

the_date <- as.character(Sys.Date())


# Read in ADSL
# read_xpt(glue("{adam_lib}/adae.xpt"))
adae <- adam_adae %>%
  filter(SAFFL == 'Y' & TRTEMFL == 'Y')

# read_xpt(glue("{adam_lib}/adsl.xpt"))
adsl <- adam_adsl 

# Header N ----
header_n <- adsl %>%
  get_header_n()

# Overall counts
overall <- ae_counts(adae, N_counts = header_n) %>%
  mutate(AETERM = 'ANY BODY SYSTEM', AEBODSYS = 'ANY BODY SYSTEM', ord1=1, ord2=1)

# System Organ Class counts
bodsys <- ae_counts(adae, AEBODSYS, N_counts = header_n) %>%
  mutate(AETERM = AEBODSYS, ord1=2, ord2=1) %>%
  arrange(AEBODSYS)

pad <- bodsys %>%
  select(AEBODSYS, ord1, ord2) %>%
  mutate(ord3=999)

# Individual term counts
term <- ae_counts(adae, AEBODSYS, AETERM, sort=TRUE, N_counts = header_n) %>%
  mutate(AETERM = paste0('  ', AETERM), ord1=2, ord2=2)

# Bring the data together
combined <- bind_rows(overall, bodsys, pad, term) %>%
  arrange(ord1, AEBODSYS, ord2, desc(ord3), AETERM)

# Build and attach column headers
column_headers <- header_n %>%
  select(-N) %>%
  pivot_wider(names_from = TRT01PN, values_from=labels) %>%
  select(npct_0=`0`, npct_54=`54`, npct_81=`81`) %>%
  mutate(cAEs_0 = '',
         cAEs_54 = '',
         cAEs_81 = '',
         AETERM = '',
         p_low = "Fisher's Exact\\line p-values",
         p_high = '')

# Insert second row of header
column_headers <- bind_rows(column_headers, tibble(
  AETERM = 'System Organ Class/\\line Preferred Term',
  npct_0 = 'n(%)',
  cAEs_0 = '[AEs]',
  npct_54 = 'n(%)',
  cAEs_54 = '[AEs]',
  npct_81 = 'n(%)',
  cAEs_81 = '[AEs]',
  p_low = 'Placebo\\line vs.\\line Low Dose',
  p_high = 'Placebo\\line vs.\\line High Dose'
))

# Attach to final
final <- bind_rows(column_headers, combined) %>%
  select(AETERM, npct_0, cAEs_0, npct_54, cAEs_54, npct_81, cAEs_81, p_low, p_high)


write_csv(final, "~/Clinical-Tables-in-R-with-gt/data/final_14501.csv")




