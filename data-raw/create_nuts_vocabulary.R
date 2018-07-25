library(dplyr)
za5929 <-  za5688
names ( za5929 )[1:400]

gesis_sample <- za5929 %>%
  select (uniqid, gesis_archive_version_date,
          country_code_iso_3166, country_code,
          life_satisfaction,
          expectations_life_in_general, expectations_personal_job_situation,
          future_human_impact_fighting_climate_change,
          future_human_impact_job_creation,
          future_human_impact_protecting_personal_data,
          gender, age_exact, w1) %>%
  mutate ( gesis_archive_version_date  = "Demo sample from 3.0.0 (2018-02-02)")

?devtools::use_data
devtools::use_data(gesis_sample)

unique ( gesis_sample$gesis_archive_version_date)

table(gesis_sample$future_human_impact_fighting_climate_change)

labelled_to_numeric ( c("A positive view", "No impact", "A negative view"),
                      c("male", "female"),
                      c(0,1))



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


regions <- za5929 %>%
  select ( country_code, starts_with("region")) %>%
  distinct ( country_code, region_nuts_codes, region_nuts_level ) %>%
  filter ( region_nuts_level == "NUTS level 2") %>%
  mutate ( nuts_2_name =  nuts_2_matching(region_nuts_codes )) %>%
  full_join (.,  nuts1013 , by  = c("country_code", "nuts_2_name"))

regions <- za5929 %>%
  select ( country_code, starts_with("region")) %>%
  distinct ( country_code, region_nuts_codes, region_nuts_level ) %>%
  filter ( country_code %in% c("MT", "LU", "CY"))
write.csv(regions, "regions.csv", row.names = F)


vocabulary_nuts2 <- readxl::read_excel("data-raw/code_regions.xlsx",
                                       sheet = "NUTS2") %>%
  select ( country_code, region_nuts_codes, code2010 ) %>%
  mutate_all ( as.character ) %>%
  left_join ( nuts1013, by = c("code2010", "country_code")) %>%
  select ( country_code, region_nuts_codes, code2010, code2013) %>%
  mutate ( code2013 = as.character(code2013)) %>%
  filter ( !is.na(country_code))

devtools::use_data(vocabulary_nuts2, overwrite = TRUE)
