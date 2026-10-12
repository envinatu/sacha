testthat::test_that("ChecklistBank report sections are emitted in the documented order", {
  .sacha_test_skip_without_namespace_mocks()
  .sacha_test_with_state({
    testthat::local_mocked_bindings(
      sc_full_name_md = function(...) cat("accepted name"),
      first_publicacion_md = function(...) cat("first publication"),
      indexations_md = function(...) cat("indexations"),
      common_names_md = function(...) cat("common names"),
      classification_md = function(...) cat("classification"),
      syn_and_comb_md = function(...) cat("synonyms"),
      locations_md = function(...) cat("locations"),
      references_sp_md = function(...) cat("references"),
      .package = "sacha"
    )

    report <- sacha::sacha_clb_report(.sacha_test_fixture_species)
    testthat::expect_identical(
      report,
      paste(
        "accepted name", "first publication", "indexations", "common names",
        "classification", "synonyms", "locations", "references",
        sep = "\n\n"
      )
    )
  })
})

testthat::test_that("species report rejects invalid sources and unsupported forwarded arguments", {
  .sacha_test_with_state({
    testthat::expect_error(
      sacha::sacha_report(.sacha_test_fixture_species, source = "unknown")
    )
    testthat::expect_error(
      sacha::sacha_report(.sacha_test_fixture_species, unsupported_option = TRUE),
      "Unsupported argument"
    )
  })
})

testthat::test_that("ChecklistBank archive writer validates unsafe inputs before network access", {
  testthat::expect_error(sacha::save_clb(dataset = -1), "non-negative whole number")
  testthat::expect_error(sacha::save_clb(dataset = 1.5), "non-negative whole number")
  testthat::expect_error(
    sacha::save_clb(dataset = "316441", overwrite = NA),
    "overwrite must be TRUE or FALSE"
  )
})
