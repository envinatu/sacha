testthat::test_that("shared export paths are normalized consistently", {
  root <- sacha:::.sacha_exports_root(tempdir())
  testthat::expect_identical(
    root,
    file.path(normalizePath(tempdir(), winslash = "/", mustWork = FALSE), "sacha_exports")
  )
  testthat::expect_identical(sacha:::.sacha_path_component("Asteraceae"), "Asteraceae")
  testthat::expect_identical(
    sacha:::.sacha_taxon_directory_name("Asteraceae", "Bidens andicola"),
    "Asteraceae_Bidens_andicola"
  )
  testthat::expect_identical(
    sacha:::.sacha_taxon_export_directory(
      tempdir(), "Asteraceae", "Bidens andicola", "inat"
    ),
    file.path(root, "Asteraceae_Bidens_andicola", "inat")
  )
  testthat::expect_error(
    sacha:::.sacha_taxon_directory_name("Asteraceae", "Bidens"),
    "genus and a specific epithet"
  )
})

testthat::test_that("scientific-name validators normalize whitespace and reject incomplete names", {
  normalized <- "Bidens andicola"
  testthat::expect_identical(
    sacha:::.sacha_global_validate_species("  Bidens   andicola "), normalized
  )
  testthat::expect_identical(
    sacha:::.sacha_spatial_validate_species("  Bidens   andicola "), normalized
  )
  testthat::expect_identical(
    sacha:::.inat_normalize_species("  Bidens   andicola "), normalized
  )
  testthat::expect_error(sacha:::.sacha_global_validate_species("Bidens"))
  testthat::expect_error(sacha:::.sacha_spatial_validate_species(NA_character_))
  testthat::expect_error(sacha:::.inat_normalize_species(c("Bidens andicola", "Other species")))
})

testthat::test_that("taxonomic export names handle missing family values safely", {
  testthat::expect_identical(
    sacha:::.sacha_taxon_directory_name(NA_character_, "Bidens andicola"),
    "Unknown_family_Bidens_andicola"
  )
  testthat::expect_identical(
    sacha:::.sacha_path_component("Family with punctuation!"),
    "Family_with_punctuation"
  )
})
