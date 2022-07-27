library(gt)

pff <- c(34, 20, 15, 91, 9, 96, 7, 6, 12, 8)

final2$pff <- pff  

library(gtExtras)

final2 %>% gt() %>%cols_label(pff = 'PFF Rank',
                              pos = 'Position',
                              player = 'Name') %>%
  cols_align(c(2,3), align = "center") %>%
  gt::tab_header(title = 'Predicting the NFL Draft') %>% gt_theme_538() %>%
  tab_source_note('Data from Pro-Football-Reference') %>%
  data_color(
    columns = c(pff),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#40798c", "#cfd7c7", "#70a9a1"),
      domain = NULL
    )
  )
