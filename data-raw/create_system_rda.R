library (eurobarometer)
library (tidyverse)

values_factor_binary <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                           sheet = "factor_binary" )
values_factor_pos_neg <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                           sheet = "factor_pos_neg" )

values_factor_yes_no_4 <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                            sheet = "yes_no_4" )

values_factor_3 <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                             sheet = "factor_3" )

values_factor_4 <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                      sheet = "factor_4" )

values_factor_5 <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                      sheet = "factor_5" )

keep_numeric_vars <- readxl::read_excel(path = "data-raw/Vocabulary.xlsx",
                                        sheet = "keep_numeric_names" )

keep_numeric_vars <- unlist ( keep_numeric_vars )

devtools::use_data(values_factor_binary,
                   values_factor_3,
                   values_factor_pos_neg,
                   values_factor_yes_no_4,
                   values_factor_4,
                   values_factor_5,
                   keep_numeric_vars,
                   internal = TRUE,
                   overwrite = TRUE)
