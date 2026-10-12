testthat::test_that("iNaturalist image defaults use medium resolution", {
  namespace <- asNamespace("sacha")
  defaults <- get(".sacha_inat_defaults", envir = namespace)
  testthat::expect_identical(defaults$default_image_size, "medium")
  testthat::expect_identical(defaults$default_save_image_size, "medium")
  testthat::expect_identical(defaults$inat_api, "https://api.inaturalist.org/v2")
})

testthat::test_that("iNaturalist photos retain only supported reusable photo licences", {
  photos <- sacha:::.inat_photo_records(.sacha_test_inat_observation())
  testthat::expect_length(photos, 2L)
  testthat::expect_identical(vapply(photos, `[[`, character(1), "photo_id"),
                             c("11", "13"))
  testthat::expect_identical(vapply(photos, `[[`, character(1), "license_code"),
                             c("cc-by", "cc-by-nc-nd"))
  testthat::expect_length(
    sacha:::.inat_photo_records(list(photos = list(list(license_code = "all-rights-reserved")))),
    0L
  )
})

testthat::test_that("iNaturalist observation searches reject missing taxa and invalid sample sizes", {
  testthat::expect_error(
    sacha:::.inat_web_query(
      species = NULL,
      user_id = NULL,
      number_observations = 1L,
      quality_grade = "research",
      place_id = NULL
    ),
    "Provide 'species'"
  )
  testthat::expect_error(
    sacha:::.inat_web_query(
      species = .sacha_test_fixture_species,
      user_id = NULL,
      number_observations = 0L,
      quality_grade = "research",
      place_id = NULL
    ),
    "positive integer"
  )
})

testthat::test_that("public iNaturalist accessors validate image-save flags before querying data", {
  .sacha_test_with_state({
    testthat::expect_error(
      sacha::inat_images(
        .sacha_test_fixture_species,
        language = "en",
        save_images = NA
      ),
      "save_images.*TRUE or FALSE"
    )
    testthat::expect_error(
      sacha::inat_images_md(
        .sacha_test_fixture_species,
        language = "en",
        export_comp = NA
      ),
      "export_comp.*TRUE or FALSE"
    )
  })
})

testthat::test_that("observation captions format dates and link to the observation record", {
  .sacha_test_skip_without_namespace_mocks()
  .sacha_test_with_state({
    dictionary <- c(
      observed = "observed", by = "by", licence = "Licence",
      unknown_place = "unknown place", unknown_observer = "unknown observer",
      all_rights_reserved = "All rights reserved"
    )
    testthat::local_mocked_bindings(
      .inat_text = function(key, language) {
        if (key %in% names(dictionary)) unname(dictionary[[key]]) else key
      },
      .package = "sacha"
    )

    observation <- .sacha_test_inat_observation()
    taxon <- list(id = 123L, scientific_name = .sacha_test_fixture_species,
                  family = "Asteraceae")
    record <- sacha:::.inat_observation_record(
      observation, taxon, selection_index = 1L, language = "en"
    )
    caption <- sacha:::.inat_subcaption(
      record, observer_name = NULL, language = "en"
    )

    testthat::expect_match(caption, "\\*Bidens andicola\\*")
    testthat::expect_match(caption, "on 10/10/2026")
    testthat::expect_match(caption, "Photographer One")
    testthat::expect_match(caption, "CC BY 4.0")
    testthat::expect_match(
      caption,
      "\\[https://www.inaturalist.org/observations/1001\\]\\(https://www.inaturalist.org/observations/1001\\)"
    )
    testthat::expect_false(grepl("photo-11-square.jpg", caption, fixed = TRUE))
  })
})

testthat::test_that("web observation search uses iNaturalist API v2 photo and place filters", {
  .sacha_test_skip_without_namespace_mocks()
  .sacha_test_with_state({
    newest <- .sacha_test_inat_observation(id = 1002L, observed_on = "2026-10-11")
    oldest <- .sacha_test_inat_observation(id = 1001L, observed_on = "2026-10-09")
    request <- new.env(parent = emptyenv())
    testthat::local_mocked_bindings(
      .inat_resolve_taxon = function(species, refresh = FALSE) {
        list(id = 123L, scientific_name = species, family = "Asteraceae")
      },
      .inat_request_json = function(path, query = list(), fields = NULL) {
        request$path <- path
        request$query <- query
        request$fields <- fields
        list(results = list(oldest, newest))
      },
      .inat_text = function(key, language) key,
      .package = "sacha"
    )

    result <- sacha:::.inat_web_query(
      species = .sacha_test_fixture_species,
      user_id = NULL,
      number_observations = 1L,
      quality_grade = "research",
      place_id = "7512",
      language = "en"
    )

    testthat::expect_identical(request$path, "observations")
    testthat::expect_identical(request$query$taxon_id, 123L)
    testthat::expect_identical(request$query$place_id, "7512")
    testthat::expect_identical(request$query$photos, "true")
    testthat::expect_match(request$query$photo_license, "cc-by")
    testthat::expect_identical(request$query$order_by, "observed_on")
    testthat::expect_identical(request$query$order, "desc")
    testthat::expect_identical(request$query$page, 1L)
    testthat::expect_identical(result$records[[1L]]$observation_id, "1002")
    testthat::expect_identical(result$records[[1L]]$selection_index, 1L)
  })
})

testthat::test_that("large iNaturalist result sets continue with an ascending ID cursor", {
  .sacha_test_skip_without_namespace_mocks()
  .sacha_test_with_state({
    namespace <- asNamespace("sacha")
    defaults <- get(".sacha_inat_defaults", envir = namespace)
    defaults$request_delay <- 0
    make_observation <- function(id, observed_on = "2026-10-10", photos = list()) {
      list(
        id = as.integer(id),
        taxon = list(id = 123L, name = .sacha_test_fixture_species),
        user = list(name = "Observer One", login = "observer_one"),
        place_guess = "Locality",
        observed_on = observed_on,
        photos = photos
      )
    }
    pages <- lapply(seq_len(50L), function(page) {
      first_id <- (page - 1L) * 200L + 1L
      lapply(first_id:(first_id + 199L), make_observation)
    })
    last_observation <- make_observation(
      10001L,
      observed_on = "2026-10-11",
      photos = list(list(
        id = 100011L,
        url = "https://example.org/photo-10001-square.jpg",
        license_code = "cc-by",
        user = list(name = "Photographer One", login = "photo_one")
      ))
    )
    requests <- new.env(parent = emptyenv())
    requests$queries <- list()
    testthat::local_mocked_bindings(
      .inat_resolve_taxon = function(species, refresh = FALSE) {
        list(id = 123L, scientific_name = species, family = "Asteraceae")
      },
      .inat_request_json = function(path, query = list(), fields = NULL) {
        requests$queries[[length(requests$queries) + 1L]] <- query
        if (!is.null(query$id_above)) {
          list(results = list(last_observation))
        } else {
          list(results = pages[[query$page]])
        }
      },
      .inat_text = function(key, language) key,
      .package = "sacha"
    )

    result <- sacha:::.inat_web_query(
      species = .sacha_test_fixture_species,
      user_id = NULL,
      number_observations = 10001L,
      quality_grade = "research",
      place_id = NULL,
      language = "en"
    )

    testthat::expect_length(requests$queries, 51L)
    testthat::expect_identical(
      vapply(requests$queries[seq_len(50L)], `[[`, integer(1), "page"),
      seq_len(50L)
    )
    testthat::expect_identical(requests$queries[[1L]]$order_by, "id")
    testthat::expect_identical(requests$queries[[1L]]$order, "asc")
    testthat::expect_null(requests$queries[[1L]]$id_above)
    testthat::expect_identical(requests$queries[[51L]]$id_above, 10000L)
    testthat::expect_identical(requests$queries[[51L]]$page, 1L)
    testthat::expect_identical(result$records[[1L]]$observation_id, "10001")
    testthat::expect_identical(result$records[[1L]]$selection_index, 1L)
  })
})

testthat::test_that("media-volume guard rejects a request that would exceed the configured ceiling", {
  .sacha_test_with_state({
    defaults <- get(".sacha_inat_defaults", envir = asNamespace("sacha"))
    defaults$hourly_media_limit_gb <- 0.000001
    sacha:::.inat_media_budget(bytes = 600)
    usage_after_first <- get(".sacha_inat_state", envir = asNamespace("sacha"))$media_usage
    testthat::expect_equal(usage_after_first$hour_bytes, 600)
    testthat::expect_error(sacha:::.inat_media_budget(bytes = 600), "safety threshold")
    usage_after_rejection <- get(".sacha_inat_state", envir = asNamespace("sacha"))$media_usage
    testthat::expect_equal(usage_after_rejection$hour_bytes, 600)
  })
})
