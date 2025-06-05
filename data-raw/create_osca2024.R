# Reading and cleaning OSCA 2024 structure

library(tidyverse)
library(glue)
devtools::load_all()

# include factor variants or nah?
include_factor_variants <- FALSE

# fun for captial -> title case
to_title <- function(x) str_to_title(x) %>% tools::toTitleCase()

# Set up
osca_url <- "https://www.abs.gov.au/statistics/classifications/osca-occupation-standard-classification-australia/2024-version-1-0/data-downloads/OSCA%20structure.xlsx"

temp_dir <- tempdir()
temp_path <- file.path(temp_dir, "osca.xlsx")

download.file(osca_url, temp_path, mode = "wb")

# Read
raw <- readxl::read_excel(temp_path,
  sheet = 6,
  range = "A6:G1754",
  col_names = FALSE
) %>%
  janitor::clean_names()


# Extract each level:
osca1 <- raw %>%
  filter(!is.na(x1)) %>%
  select(
    osca1_code = 1,
    osca1 = 2
  ) %>%
  mutate(osca1_code = as.character(osca1_code))

osca2 <- raw %>%
  anti_join(osca1, by = c("x2" = "osca1")) %>%
  filter(!is.na(x2)) %>%
  select(
    osca2_code = 2,
    osca2 = 3
  ) %>%
  mutate(osca1_code = substr(osca2_code, 1, 1))

osca3 <- raw %>%
  anti_join(osca1, by = c("x2" = "osca1")) %>%
  anti_join(osca2, by = c("x3" = "osca2")) %>%
  filter(!is.na(x3)) %>%
  select(
    osca3_code = 3,
    osca3 = 4
  ) %>%
  mutate(
    osca2_code = substr(osca3_code, 1, 2),
    osca1_code = substr(osca2_code, 1, 1)
  )

osca4 <- raw %>%
  anti_join(osca1, by = c("x2" = "osca1")) %>%
  anti_join(osca2, by = c("x3" = "osca2")) %>%
  anti_join(osca3, by = c("x4" = "osca3")) %>%
  filter(!is.na(x4)) %>%
  select(
    osca4_code = 4,
    osca4 = 5
  ) %>%
  mutate(
    osca3_code = substr(osca4_code, 1, 3),
    osca2_code = substr(osca3_code, 1, 2),
    osca1_code = substr(osca2_code, 1, 1)
  )

osca6 <- raw %>%
  anti_join(osca1, by = c("x2" = "osca1")) %>%
  anti_join(osca2, by = c("x3" = "osca2")) %>%
  anti_join(osca3, by = c("x4" = "osca3")) %>%
  anti_join(osca4, by = c("x5" = "osca4")) %>%
  filter(!is.na(x5)) %>%
  select(
    osca6_code = 5,
    osca6 = 6,
    skill_level = 7
  ) %>%
  mutate(
    osca4_code = substr(osca6_code, 1, 4),
    osca3_code = substr(osca4_code, 1, 3),
    osca2_code = substr(osca3_code, 1, 2),
    osca1_code = substr(osca2_code, 1, 1)
  )

# Join into wide osca occupation list
comb <- osca1 %>%
  left_join(osca2) %>%
  left_join(osca3) %>%
  left_join(osca4) %>%
  left_join(osca6) %>%
  mutate(osca1 = to_title(osca1))


# Generate ', nfd' variants, which are used in eg Census
nfd1 <- comb %>%
  select(osca1_code, osca1) %>%
  distinct() %>%
  mutate(
    osca2 = glue("{osca1}, nfd"),
    osca2_code = glue("{osca1_code}0"),
    osca3 = glue("{osca1}, nfd"),
    osca3_code = glue("{osca1_code}00"),
    osca4 = glue("{osca1}, nfd"),
    osca4_code = glue("{osca1_code}000"),
    osca6 = glue("{osca1}, nfd"),
    osca6_code = glue("{osca1_code}00000")
  )

nfd2 <- comb %>%
  select(osca1_code, osca1, osca2_code, osca2) %>%
  distinct() %>%
  mutate(
    osca3 = glue("{osca2}, nfd"),
    osca3_code = glue("{osca2_code}0"),
    osca4 = glue("{osca2}, nfd"),
    osca4_code = glue("{osca2_code}00"),
    osca6 = glue("{osca2}, nfd"),
    osca6_code = glue("{osca2_code}0000")
  )


nfd3 <- comb %>%
  select(osca1_code, osca1, osca2_code, osca2, osca3_code, osca3) %>%
  distinct() %>%
  mutate(
    osca4 = glue("{osca3}, nfd"),
    osca4_code = glue("{osca3_code}0"),
    osca6 = glue("{osca3}, nfd"),
    osca6_code = glue("{osca3_code}000")
  )

osca2024 <- comb %>%
  bind_rows(nfd1, nfd2, nfd3) %>%
  arrange(
    osca1_code, osca2_code, osca3_code,
    osca4_code, osca6_code
  ) %>%
  mutate(across(everything(),
    .fns = as.character
  )) %>%
  arrange(osca6_code)

if (include_factor_variants) {
  osca2024 <- osca2024 %>%
    mutate(
      osca1_f = as_factor(osca1),
      osca2_f = as_factor(osca2),
      osca3_f = as_factor(osca3),
      osca4_f = as_factor(osca4),
      osca6_f = as_factor(osca6),
      skill_level = as_factor(skill_level)
    ) %>%
    # order
    select(
      osca1_code, osca1, osca1_f,
      osca2_code, osca2, osca2_f,
      osca3_code, osca3, osca3_f,
      osca4_code, osca4, osca4_f,
      osca6_code, osca6, osca6_f,
      skill_level
    )
}

# Rename using new conventions: https://github.com/runapp-aus/strayr/issues/17
osca2024 <- osca2024 %>%
  rename(
    osca_major = osca1,
    osca_major_code = osca1_code,
    osca_submajor = osca2,
    osca_submajor_code = osca2_code,
    osca_minor = osca3,
    osca_minor_code = osca3_code,
    osca_unit = osca4,
    osca_unit_code = osca4_code,
    osca_occupation = osca6,
    osca_occupation_code = osca6_code
  )

osca_dictionary <- make_dictionary(osca2024)

# Export
usethis::use_data(osca2024, overwrite = TRUE)
