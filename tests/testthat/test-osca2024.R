test_that("osca2024 dataset is loaded and has correct basic structure", {
  # Test if the dataset exists after loading the package
  expect_true(exists("osca2024"), "Dataset 'osca2024' should be loaded.")

  # Test if tibble / data.frame
  expect_s3_class(osca2024, "tbl_df")
  expect_s3_class(osca2024, "data.frame")

  # Test for correct number of variables
  expect_equal(ncol(osca2024), 11)

  # Test for actual data / rows
  expect_equal(nrow(osca2024), 1328)

  # Test for correct column names and order
  expected_colnames <- c(
    "osca_major_code", "osca_major",
    "osca_submajor_code", "osca_submajor",
    "osca_minor_code", "osca_minor",
    "osca_unit_code", "osca_unit",
    "osca_occupation_code", "osca_occupation",
    "skill_level"
  )
  expect_named(osca2024, expected_colnames, ignore.order = FALSE)
})

test_that("osca2024 column types are correct", {
  # Code and name variables should be character
  character_cols <- c(
    "osca_major_code", "osca_major",
    "osca_submajor_code", "osca_submajor",
    "osca_minor_code", "osca_minor",
    "osca_unit_code", "osca_unit",
    "osca_occupation_code", "osca_occupation"
  )
  for (col in character_cols) {
    expect_type(osca2024[[col]], "character")
  }

  # Skill level is between 1 being the highest and 5 being the lowest".
  expect_type(osca2024$skill_level, "character")
})

test_that("osca2024 has no missing values in columns, excluding skill_level", {
  # Because of the wide table format, we expect no missing values (NA values)
  # in these hierarchical columns.
  columns_to_check <- setdiff(colnames(osca2024), "skill_level")

  for (col_name in columns_to_check) {
    expect_false(
      any(is.na(osca2024[[col_name]])),
      info = paste0("Column '", col_name, "' should not contain any NA values.")
    )
  }
})

test_that("each row is unique", {
  expect_equal(length(unique(osca2024$osca_occupation_code)), nrow(osca2024))
})

test_that("specific known values in osca2024 are correct", {
  bricklayer_row <- osca2024[osca2024$osca_occupation_code == "371131", ]

  expect_equal(bricklayer_row$osca_occupation, "Bricklayer")
  expect_equal(bricklayer_row$osca_unit, "Bricklayers and Stonemasons")
  expect_equal(bricklayer_row$osca_minor, "Bricklayers, Stonemasons and Concreters")
  expect_equal(bricklayer_row$osca_submajor, "Building Structural Trades Workers")
  expect_equal(bricklayer_row$osca_major, "Technicians and Trades Workers")
  expect_equal(bricklayer_row$osca_major_code, "3")
  })
