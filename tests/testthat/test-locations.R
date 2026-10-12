testthat::test_that("Catalogue of Life locations are formatted and cached as a shared record", {
  .sacha_test_skip_without_namespace_mocks()
  .sacha_test_with_state({
    labels <- c(
      locations_title = "Distribution",
      locations_suffix = "has been recorded in the following geographic areas",
      no_locations = "No documented geographic areas for",
      communication_error = "Connection error"
    )
    calls <- character()
    testthat::local_mocked_bindings(
      .sacha_spatial_language = function(language = NULL) {
        if (is.null(language)) "en" else language
      },
      .sacha_spatial_label = function(key, language = NULL) unname(labels[[key]]),
      .sacha_spatial_http_json = function(url, query = list()) {
        calls <<- c(calls, url)
        if (grepl("/match/nameusage$", url)) {
          sacha:::.sacha_spatial_response(
            "ok",
            data = list(usage = list(
              id = "taxon-123",
              name = list(scientificName = .sacha_test_fixture_species,
                          authorship = "Kunth")
            ))
          )
        } else {
          sacha:::.sacha_spatial_response(
            "ok",
            data = list(
              usage = list(name = list(
                scientificName = .sacha_test_fixture_species,
                authorship = "Kunth"
              )),
              distributions = list(
                list(area = list(name = "Ecuador")),
                list(area = list(name = "Peru")),
                list(area = list(name = "Ecuador"))
              )
            )
          )
        }
      },
      .package = "sacha"
    )

    markdown <- sacha::locations_md(.sacha_test_fixture_species)
    testthat::expect_match(markdown, "\\*Bidens andicola\\* Kunth")
    testthat::expect_match(markdown, "Ecuador; Peru")
    testthat::expect_identical(length(calls), 2L)

    plain <- sacha::locations(.sacha_test_fixture_species)
    testthat::expect_match(plain, "Bidens andicola Kunth")
    testthat::expect_identical(length(calls), 2L)
  })
})

testthat::test_that("plain locations report transport failures while Markdown output remains renderable", {
  .sacha_test_skip_without_namespace_mocks()
  .sacha_test_with_state({
    labels <- c(
      locations_title = "Distribution",
      locations_suffix = "has been recorded in the following geographic areas",
      no_locations = "No documented geographic areas for",
      communication_error = "Connection error"
    )
    testthat::local_mocked_bindings(
      .sacha_spatial_language = function(language = NULL) {
        if (is.null(language)) "en" else language
      },
      .sacha_spatial_label = function(key, language = NULL) unname(labels[[key]]),
      .sacha_spatial_http_json = function(url, query = list()) {
        sacha:::.sacha_spatial_response("error", message = "service unavailable")
      },
      .package = "sacha"
    )

    testthat::expect_error(
      sacha::locations(.sacha_test_fixture_species),
      "service unavailable"
    )
    markdown <- sacha::locations_md(.sacha_test_fixture_species)
    testthat::expect_match(markdown, "No documented geographic areas")
    testthat::expect_match(markdown, "\\*Bidens andicola\\*")
  })
})
