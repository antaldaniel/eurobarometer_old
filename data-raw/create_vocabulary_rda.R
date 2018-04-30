library (eurobarometer)
library (tidyverse)

values_factor_binary <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                           sheet = "factor_binary",
                                           col_names = TRUE) %>%
  mutate ( context = "factor_binary") %>%
  gather ( names, values, !! 2:(ncol(.)-1))

values_factor_pos_neg <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                           sheet = "factor_pos_neg" ) %>%
  mutate ( context = "factor_pos_neg") %>%
  gather ( names, values, !! 2:(ncol(.)-1))



values_factor_yes_no_4 <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                            sheet = "yes_no_4" ) %>%
  mutate ( context = "yes_no_4") %>%
  gather ( names, values, !! 2:(ncol(.)-1))



values_factor_3 <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                             sheet = "factor_3" ) %>%
  mutate ( context = "factor_3") %>%
  gather ( names, values, !! 2:(ncol(.)-1))



values_factor_4 <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                      sheet = "factor_4" ) %>%
  mutate ( context = "factor_4") %>%
  gather ( names, values, !! 2:(ncol(.)-1))



values_factor_5 <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                      sheet = "factor_5" ) %>%
  mutate ( context = "factor_5") %>%
  gather ( names, values, !! 2:(ncol(.)-1))

values_frequency_factor <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                              sheet = "factor_frequency" ) %>%
  mutate ( context = "factor_frequency") %>%
  gather ( names, values, !! 2:(ncol(.)-1))

vocabulary <- rbind ( values_factor_binary, values_factor_pos_neg,
               values_factor_yes_no_4, values_factor_3,
               values_factor_4, values_factor_5,
               values_frequency_factor) %>%
  mutate ( names = forcats::fct_relevel(names, c(
    "neg_2", "neg_1", "neutral", "pos_1", "pos_2", "pos_3",
    "36",  "12", "3", "0")))

vocabulary <- vocabulary %>%
  spread ( names, values )

devtools::use_data(vocabulary,
                   overwrite = TRUE)

#for checking
#load("R/sysdata.rda")
