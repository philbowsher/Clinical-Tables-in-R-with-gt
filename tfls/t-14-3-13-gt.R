library(gt)
library(readr)

final_14313 <- read_csv("~/Clinical-Tables-in-R-with-gt/data/final_14313.csv")

final_14313 %>%
  gt(groupname_col="block") 


# use gt to do the reporting
tab_html <- final_14313 %>%
  gt(groupname_col="block") %>%
  
  tab_header(
    title = "Table 14.3.13",
    subtitle = "CIBIC+ - Categorical Analysis - LOCF"
  ) %>%
  
  tab_source_note(
    source_note = "[1]: [1] Overall comparison of treatments using CMH test (Pearson Chi-Square), controlling for site group."
  ) %>%
  
  
  tab_source_note(
    source_note = paste('Program Source: 14-2.01.R Executed:
(Draft)', the_date)) %>%
  
  
  # cols_label(
  # catlabel= " ",
  # GroupA = paste0("Group A (N=", bign[1], ")"),
  # GroupB = paste0("Group B (N=", bign[2], ")"),
  # GroupC = paste0("Group C (N=", bign[3], ")")) %>%
  
  tab_options(
    table.border.top.color = "white",
    heading.border.bottom.color = "black",
    table.border.bottom.color = "white",
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "white",
    row_group.border.bottom.color = "white",
    row_group.border.top.color = "white",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",) %>%
  
  cols_align(
    align = "left")
# output the HTML table
tab_html %>%
  gtsave("14-3.13.html", path = "~/Clinical-Tables-in-R-with-gt/tfls/" )
