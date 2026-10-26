testthat::test_that("ChecklistBank record normalization feeds stable accessors and caches one fetch", {
  .sacha_test_with_state({
    calls <- 0L
    sacha::sacha_configure(clb = list(fetcher = function(species, config) {
      calls <<- calls + 1L
      .sacha_test_global_record(species)
    }))

    testthat::expect_identical(
      as.character(sacha::first_publicacion(.sacha_test_fixture_species)),
      "Kunth. 1818. Nova Genera et Species Plantarum."
    )
    testthat::expect_identical(
      as.character(sacha::classification(.sacha_test_fixture_species, rank = "family")),
      "Asteraceae"
    )
    testthat::expect_identical(
      as.character(sacha::common_names(.sacha_test_fixture_species)),
      "Nombre local; Common name (en)"
    )
    testthat::expect_identical(
      as.character(sacha::references_sp(.sacha_test_fixture_species)),
      "Alpha, B. 2019. Source.\nZulu, A. 2020. Source."
    )

    urls <- sacha::indexations(.sacha_test_fixture_species)
    testthat::expect_named(urls, c("Catalogue of Life", "GBIF", "iNaturalist"))
    testthat::expect_match(urls[["GBIF"]], "www.gbif.org/species/456")
    testthat::expect_match(urls[["iNaturalist"]], "www.inaturalist.org/taxa/789")
    testthat::expect_identical(calls, 1L)
  })
})

testthat::test_that("record normalization adds a species leaf and removes duplicate source entries", {
  .sacha_test_with_state({
    record <- sacha:::.sacha_global_normalise_record(
      .sacha_test_global_record(), .sacha_test_fixture_species
    )
    testthat::expect_identical(tail(record$classification, 1L)[[1L]]$rank, "species")
    testthat::expect_identical(tail(record$classification, 1L)[[1L]]$name,
                               .sacha_test_fixture_species)
    testthat::expect_length(record$references, 2L)
    testthat::expect_length(record$synonyms$homotypic, 1L)
    testthat::expect_named(record$synonyms,
                           c("homotypic", "heterotypic", "misapplied"))
    testthat::expect_named(record$identifiers, c("col", "gbif", "inat"))
  })
})

testthat::test_that("custom ChecklistBank fetchers must return records as lists", {
  testthat::expect_error(
    sacha:::.sacha_global_normalise_record("not a record", .sacha_test_fixture_species),
    "must return a list"
  )
})
