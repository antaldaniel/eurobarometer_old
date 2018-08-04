library(dplyr)

nuts_2_matching <- function (x) {
  x <- tolower (x)
  x <- gsub ("prov.", "", x)
  x <- gsub( "\\(", "[", x)
  x <- gsub( "\\)", "]", x)
  x <- ifelse ( grepl( "brussels hoofdstedelijk", x),
                yes = "région de bruxelles-capitale / brussels hoofdstedelijk gewest",
                no= x)
  x <- gsub("vlaams brabant", "vlaams-brabant", x)
  x <- gsub("li\u0232ge", "liege",  x)
  x <- gsub ( "oesterreich", "österreich", x)
  x <- gsub ( "kaernten", "kärnten", x )
  x <- stringr::str_trim ( x, side = "both")
  x
}


nuts1013 <- read.csv("C:/Users/Daniel Antal/OneDrive - Visegrad Investments/_package/eurobarometer/data-raw/NUTS 2010 - NUTS 2013.csv",
                            skip = 1,
                            encoding = "UTF-8")%>%
  select ( 1:12) %>%
  purrr::set_names(.,
                   c("row", "code2010", "code2013", "country",
                     "nuts1", "nuts2", "nuts3", "change", "nuts_level",
                     "countries", "sorting_order_2010", "sorting_order_2013"
                   )) %>%
  select( row, code2010, code2013, nuts2, nuts3 ) %>%
  filter ( nchar(as.character(nuts2)) > 0) %>%
  mutate ( nuts_2_name = nuts_2_matching(nuts2)) %>%
  mutate ( country_code = stringr::str_sub(code2010, 1, 2)) %>%
  mutate ( country_code == ifelse ( country_code == "EL", "GR", country_code))


vocabulary_nuts2 <- readxl::read_excel("data-raw/code_regions.xlsx",
                                       sheet = "NUTS2") %>%
  select ( country_code, region_nuts_codes, code2010 ) %>%
  mutate_all ( as.character ) %>%
  left_join ( nuts1013, by = c("code2010", "country_code")) %>%
  select ( country_code, region_nuts_codes, code2010) %>%
  mutate ( code2013 = as.character(code2013)) %>%
  filter ( !is.na(country_code)) %>%
  mutate ( code2013 = ifelse ( code2010 == "UKN0", code2010, code2013)) %>%
  mutate ( region_nuts_codes = ifelse ( code2010 == "UKN0",
                                        "Northern Ireland", region_nuts_codes))

vocabulary_nuts1 <- readxl::read_excel("data-raw/code_regions.xlsx",
                                       sheet = "NUTS1") %>%
  select ( country_code, region_nuts_codes, code2010, code2013 ) %>%
  mutate_all ( as.character )
devtools::use_data(vocabulary_nuts1, overwrite = TRUE)

devtools::use_data(vocabulary_nuts2, overwrite = TRUE)


imputation <- readxl::read_excel("data-raw/NUTS2_2010_imputation.xlsx") %>%
  purrr::set_names(.,c("country_code", "row", "code2010", "code2013", "region_nuts_codes",
                       "nuts1", "nuts2", "nuts3", "change", "nuts_level",
                       "countries", "sorting_order_2010", "sorting_order_2013"
  ) ) %>%
  filter ( !is.na(nuts2))

nuts2_imputation <- readxl::read_excel("data-raw/NUTS2_2010_imputation.xlsx") %>%
  purrr::set_names(.,c("country_code", "row", "code2010", "code2013",
                       "region_nuts_codes",
                       "nuts1", "nuts2", "nuts3", "change", "nuts_level",
                       "countries", "sorting_order_2010", "sorting_order_2013"
  ) ) %>%
  filter ( !is.na(nuts2)) %>%
  select ( country_code, region_nuts_codes, code2010, code2013)
devtools::use_data(nuts2_imputation, overwrite = TRUE)

