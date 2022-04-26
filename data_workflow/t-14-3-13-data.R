# t-14-3-13.R
#   CDISC Pilot Table 14-3.13

library(glue)
library(tidyverse)
# library(haven)
library(assertthat)

library(safetyData)

# source('./programs/config.R')
source('~/Clinical-Tables-in-R-with-gt/data_workflow/funcs.R')

the_date <- as.character(Sys.Date())

# Data frame for ordering and to fill 0s
ord <- tibble(
  AVALC = rep(c('n',
           'Marked improvement',
           'Moderate improvement',
           'Minimal improvement',
           'No Change',
           'Minimal worsening',
           'Moderate worsening',
           'Marked worsening', ''),3),
  ord = rep(c(0:8),3),
  AVISITN = c(rep(8, 9), rep(16,9), rep(24,9)),
  AVISIT = c(rep('Week 8', 9), rep('Week 16', 9), rep('Week 24', 9))
)

# Read in the CBIC dataset ----

# read_xpt(glue("{adam_lib}/adcibc.xpt")) %>%
cbic <- adam_adqscibc %>%
  filter(EFFFL == 'Y' & ITTFL == 'Y', AVISITN %in% c(8, 16, 24) & ANL01FL=='Y') %>%
  # Create a character version of AVAL for display
  mutate(
    AVALC = ord[2:8, ]$AVALC[AVAL], # The codelist is already in this dataframe so using that
  )

# Calculate the header Ns ----
header_n <- cbic %>%
  distinct(USUBJID, TRTP, TRTPN) %>%
  get_header_n(TRTP, TRTPN)

column_headers <- header_n %>%
  select(-N) %>%
  pivot_wider(names_from = TRTPN, values_from=labels) %>%
  mutate(AVISIT = '',
         AVALC = 'Assessment',
         p = 'p-value\\line [1]')

# Get the summary N counts for each group
ns <- cbic %>%
  group_by(TRTPN, AVISITN, AVISIT) %>%
  summarize(N = n()) %>%
  ungroup()

counts <- cbic %>%
  # Summarize the categorical counts
  group_by(TRTPN, AVISITN, AVISIT, AVALC) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  # Merge in the group N's for summary
  merge(ns, by=c('TRTPN', 'AVISITN', 'AVISIT', 'AVISIT')) %>%
  rowwise() %>%
  # Format the n (%)
  mutate(npct=n_pct(n, N, n_width=2)) %>%
  select(-n, -N) %>%
  # Transpose out by treatment group
  pivot_wider(names_from = TRTPN, values_from=npct) %>%
  # Bind with the N rows
  bind_rows(
    # Need for tranpose and format
    ns %>%
      rowwise() %>%
      # Format the N counts and add the row label
      mutate(
        AVALC = 'n',
        Nc = num_fmt(N, size=9, int_len=2)
      ) %>%
      select(-N) %>%
      # Transpose out by group
      pivot_wider(names_from = TRTPN, values_from=Nc)
  ) %>%
  # Join to add 0's
  full_join(
    ord, by=c('AVISITN', 'AVISIT', 'AVALC')
  ) %>%
  # Fill the 0s
  ## There is a bug here that causes vctrs to fail.
  replace_na(list(`0`=' 0       ', `54` = ' 0       ', `81`=' 0       ')) %>%
  # Clean up the rows that should be blank
  mutate(
    `0` = ifelse(AVALC=='', '', `0`),
    `54` = ifelse(AVALC=='', '', `54`),
    `81` = ifelse(AVALC=='', '', `81`),
    AVISIT = ifelse(ord==0, AVISIT, '')
  ) %>%
  # Sort
  arrange(AVISITN, ord)


## P-values ----
# !!! NOTE: To obtain the same p-value used in SAS for this display, a modification had to be made to the vcdExtra library.
#           Please refer to this github issue: https://github.com/friendly/vcdExtra/issues/3
#           And you can access our fork of the library here: https://github.com/mstackhouse/vcdExtra
counts['p'] <- character(nrow(counts))

counts[(counts$AVISITN==8 & counts$ord==0),'p'] <- cbic %>%
  filter(AVISITN == 8) %>%
  cmh_p(AVAL ~ TRTP | SITEGR1) %>%
  num_fmt(digits=4, size=5, int_len=1)

counts[(counts$AVISITN==16 & counts$ord==0),'p']  <- cbic %>%
  filter(AVISITN == 16) %>%
  cmh_p(AVAL ~ TRTP | SITEGR1) %>%
  num_fmt(digits=4, size=5, int_len=1)

counts[(counts$AVISITN==24 & counts$ord==0),'p'] <- cbic %>%
  filter(AVISITN == 24) %>%
  cmh_p(AVAL ~ TRTP | SITEGR1) %>%
  num_fmt(digits=4, size=5, int_len=1)

final <- bind_rows(column_headers, counts) %>%
  select(AVISIT, AVALC, `0`,`54`,`81`, p)

write_csv(final, "~/Clinical-Tables-in-R-with-gt/data/final_14313.csv")





