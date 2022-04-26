# t-14-4-01.R
#   CDISC Pilot Table 14-4.01

library(glue)
library(tidyverse)
library(haven)
library(assertthat)
library(pharmaRTF)

library(safetyData)

# source('./programs/config.R')
source('~/Clinical-Tables-in-R-with-gt/data_workflow/funcs.R')

the_date <- as.character(Sys.Date())


# Read in ADSL
# read_xpt(glue("{adam_lib}/adsl.xpt"))
adsl <- adam_adsl

# Subset for completers
adsl_complt <- adsl %>%
  filter(COMP24FL == 'Y') %>%
  select(TRT01P, TRT01PN, AVGDD, CUMDOSE) %>%
  mutate(cat = 1, TRTPCD = paste(TRT01PN, '_C', sep=''))

# Subset for safety
adsl_safety <- adsl %>%
  filter(SAFFL == 'Y') %>%
  select(TRT01P, TRT01PN, AVGDD, CUMDOSE) %>%
  mutate(cat = 2, TRTPCD = paste(TRT01PN, '_S', sep=''))

# Stack the two together
adsl_ = bind_rows(adsl_safety, adsl_complt)
rm(adsl_safety, adsl_complt) # Clean-up

# Header N counts and column headers
header <- adsl_ %>%
  group_by(TRTPCD, TRT01P, TRT01PN, cat) %>%
  summarize(N = n()) %>%
  mutate(
    labels = str_replace_all(str_wrap(glue('{TRT01P} (N={N})'), width=10), "\n", function(x) "\\line ")
  ) %>%
  ungroup() %>%
  arrange(cat, TRT01PN) %>%
  select(TRTPCD, labels) %>%
  pivot_wider(names_from=TRTPCD, values_from=labels)

# Calculate average daily dose summary stats
avgdd <- adsl_ %>% desc_stats(AVGDD, group=TRTPCD, int_len=5) %>%
  mutate(rowlbl1 = 'Average daily dose (mg)')

# Calculate cumulative dose at end of study
cumdose <- adsl_ %>% desc_stats(CUMDOSE, group=TRTPCD, int_len=5) %>%
  mutate(rowlbl1 = 'Cumulative dose at end of study [2]')

# Spanner - want this to be the top left cell of the cells that will merge
spanner <- tibble(`0_C` = 'Completers at Week 24', `0_S` = 'Safety Population [1]')

# Join it all together, order columns, clean grouped cells
final <- bind_rows(spanner, header, avgdd, cumdose) %>%
  select(rowlbl1, rowlbl2, `0_C`, `54_C`, `81_C`, `0_S`, `54_S`, `81_S`) %>%
  group_by(rowlbl1) %>%
  mutate(ord1 = row_number()) %>%
  ungroup() %>%
  mutate(rowlbl1 = ifelse(ord1 == 1, rowlbl1, "")) %>%
  select(-ord1)

write_csv(final, "~/Clinical-Tables-in-R-with-gt/data/final_14401.csv")

