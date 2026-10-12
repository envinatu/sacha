testthat::test_that("spatial validators enforce documented input ranges", {
  testthat::expect_identical(sacha:::.sacha_spatial_validate_limit(500L), 500L)
  testthat::expect_error(sacha:::.sacha_spatial_validate_limit(0L))
  testthat::expect_error(sacha:::.sacha_spatial_validate_limit(100001L))
  testthat::expect_identical(sacha:::.sacha_spatial_validate_colour("#A1b2C3"), "#A1b2C3")
  testthat::expect_error(sacha:::.sacha_spatial_validate_colour("red"))
  testthat::expect_identical(
    sacha:::.sacha_spatial_validate_logical(FALSE, "refresh"), FALSE
  )
  testthat::expect_error(sacha:::.sacha_spatial_validate_logical(NA, "refresh"))
})

testthat::test_that("crop-box parsing validates geographic bounds and preserves an optional label", {
  parsed <- sacha:::.sacha_spatial_crop_box_parse(
    c(-82.5, -5.5, -75, 1.6, "Ecuador continental")
  )
  testthat::expect_identical(unname(parsed$extent), c(-82.5, -5.5, -75, 1.6))
  testthat::expect_identical(names(parsed$extent), c("xmin", "ymin", "xmax", "ymax"))
  testthat::expect_identical(parsed$label, "Ecuador continental")
  testthat::expect_null(sacha:::.sacha_spatial_crop_box_parse(NULL))
  testthat::expect_error(sacha:::.sacha_spatial_crop_box_parse(c(1, 2, 0, 4)))
  testthat::expect_error(sacha:::.sacha_spatial_crop_box_parse(c(-181, -5, 10, 5)))
  testthat::expect_error(sacha:::.sacha_spatial_crop_box_parse(c(1, 2, 3)))
})

testthat::test_that("spatial response values have a stable status and payload structure", {
  response <- sacha:::.sacha_spatial_response(
    status = "empty", data = data.frame(), message = "No records"
  )
  testthat::expect_s3_class(response, "sacha_spatial_response")
  testthat::expect_identical(response$status, "empty")
  testthat::expect_identical(response$message, "No records")
  testthat::expect_true(is.data.frame(response$data))
  testthat::expect_error(sacha:::.sacha_spatial_response(status = "unknown"))
})

testthat::test_that("spatial crop filtering retains points inside the configured extent", {
  testthat::skip_if_not_installed("sf")
  occurrences <- data.frame(
    occurrenceID = c("inside-a", "inside-b", "outside-a"),
    decimalLongitude = c(-1, 0, 1.5),
    decimalLatitude = c(-1, 0, 0),
    stringsAsFactors = FALSE
  )
  crop <- list(
    window = c(xmin = -1, ymin = -1, xmax = 1, ymax = 1),
    boundary = NULL
  )
  filtered <- sacha:::.sacha_spatial_filter_crop(occurrences, crop)
  testthat::expect_identical(filtered$occurrenceID, c("inside-a", "inside-b"))
})

testthat::test_that("spatial cache clearing removes the selected species only", {
  .sacha_test_with_state({
    namespace <- asNamespace("sacha")
    spatial_cache <- get(".sacha_spatial_cache", envir = namespace)
    global_cache <- get(".sacha_global_cache", envir = namespace)
    selected <- "Bidens andicola"
    other <- "Other species"
    selected_key <- sacha:::.sacha_spatial_cache_key(selected)
    other_key <- sacha:::.sacha_spatial_cache_key(other)
    selected_location <- paste0("spatial_locations::", tolower(selected), "::en")
    other_location <- paste0("spatial_locations::", tolower(other), "::en")
    assign(selected_key, TRUE, envir = spatial_cache)
    assign(other_key, TRUE, envir = spatial_cache)
    assign(selected_location, TRUE, envir = global_cache)
    assign(other_location, TRUE, envir = global_cache)

    sacha:::.sacha_spatial_clear_cache(selected, reload_languages = FALSE)
    testthat::expect_false(exists(selected_key, envir = spatial_cache, inherits = FALSE))
    testthat::expect_true(exists(other_key, envir = spatial_cache, inherits = FALSE))
    testthat::expect_false(exists(selected_location, envir = global_cache, inherits = FALSE))
    testthat::expect_true(exists(other_location, envir = global_cache, inherits = FALSE))
  })
})

testthat::test_that("spatial retry repeats transient errors and propagates permanent errors", {
  calls <- 0L
  value <- sacha:::.sacha_spatial_with_retry(
    function() {
      calls <<- calls + 1L
      if (calls < 3L) stop("HTTP 503 temporarily unavailable")
      "available"
    },
    max_tries = 3L,
    base_delay = 0
  )
  testthat::expect_identical(value, "available")
  testthat::expect_identical(calls, 3L)
  testthat::expect_true(
    sacha:::.sacha_spatial_is_transient_condition(simpleError("HTTP 429"))
  )
  testthat::expect_false(
    sacha:::.sacha_spatial_is_transient_condition(simpleError("invalid query"))
  )

  permanent_calls <- 0L
  testthat::expect_error(
    sacha:::.sacha_spatial_with_retry(
      function() {
        permanent_calls <<- permanent_calls + 1L
        stop("invalid query")
      },
      max_tries = 3L,
      base_delay = 0
    ),
    "invalid query"
  )
  testthat::expect_identical(permanent_calls, 1L)
})
