library(gt)
library(readr)

final_14501 <- read_csv("~/Clinical-Tables-in-R-with-gt/data/final_14501.csv")

final_14501 %>%
  gt(groupname_col="block") 


# use gt to do the reporting
tab_html <- final_14501 %>%
  gt(groupname_col="block") %>%
  
  tab_header(
    title = "t-14-5-01",
    subtitle = "Summary of Demographic and Baseline Characteristics"
  ) %>%
  
  tab_source_note(
    source_note = "[1]: P-values are results of ANOVA treatment group comparison for continuous variable and Pearson's chisquare test for categorical variables."
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
  gtsave("14-5-01.html", path = "~/Clinical-Tables-in-R-with-gt/tfls/" )


