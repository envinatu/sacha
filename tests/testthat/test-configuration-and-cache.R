testthat::test_that("module configuration is independent and validates groups before updating", {
  .sacha_test_with_state({
    visibility <- withVisible(sacha::sacha_configure(
      global = list(dataset = "test-dataset"),
      inat = list(default_image_size = "large"),
      spatial = list(default_limit = 500L)
    ))
    testthat::expect_false(visibility$visible)

    current <- sacha::sacha_configure()
    testthat::expect_identical(current$global$dataset, "test-dataset")
    testthat::expect_identical(current$inat$default_image_size, "large")
    testthat::expect_identical(current$spatial$default_limit, 500L)
    testthat::expect_identical(current$inat$language, "en")
    testthat::expect_identical(current$spatial$language, "en")

    testthat::expect_error(
      sacha::sacha_configure(global = list(dataset = "must-not-be-applied"), inat = "large"),
      "fully named list"
    )
    testthat::expect_identical(sacha::sacha_configure()$global$dataset, "test-dataset")
    testthat::expect_error(sacha::sacha_configure(spatial = list(default_limit = 0L)))
  })
})

testthat::test_that("shared argument defaults are queryable, removable, and subordinate to explicit values", {
  .sacha_test_with_state({
    testthat::expect_invisible(
      sacha::sacha_arg_configure(
        species = "Bidens andicola", source = "local", number_observations = 2L
      )
    )
    configured <- sacha::sacha_arg_configure()
    testthat::expect_identical(configured$species, "Bidens andicola")
    testthat::expect_identical(configured$source, "local")
    testthat::expect_identical(configured$number_observations, 2L)

    frame <- new.env(parent = emptyenv())
    frame$source <- "function default"
    frame$species <- "explicit species"
    sacha:::.sacha_apply_arg_defaults(
      frame,
      quote(accessor(species = "explicit species")),
      c("species", "source")
    )
    testthat::expect_identical(frame$source, "local")
    testthat::expect_identical(frame$species, "explicit species")

    merged <- sacha:::.sacha_merge_arg_defaults(
      list(source = "web"), c("species", "source", "number_observations")
    )
    testthat::expect_identical(merged$source, "web")
    testthat::expect_identical(merged$species, "Bidens andicola")
    testthat::expect_identical(merged$number_observations, 2L)

    sacha::sacha_arg_configure(source = NULL)
    testthat::expect_null(sacha::sacha_arg_configure()$source)
    testthat::expect_error(do.call(sacha::sacha_arg_configure, list("Bidens andicola")))
    duplicate_names <- structure(list("Bidens andicola", "Other species"),
                                 names = c("species", "species"))
    testthat::expect_error(do.call(sacha::sacha_arg_configure, duplicate_names))
    testthat::expect_error(sacha::sacha_arg_configure(not_a_package_argument = TRUE))
  })
})

testthat::test_that("clearing caches restores defaults and preserves iNaturalist download safeguards", {
  .sacha_test_skip_without_namespace_mocks()
  .sacha_test_with_state({
    testthat::local_mocked_bindings(
      .sacha_spatial_load_languages = function(force = FALSE) invisible(NULL),
      .package = "sacha"
    )

    sacha::sacha_configure(
      global = list(dataset = "test-dataset"),
      inat = list(default_image_size = "large"),
      spatial = list(default_limit = 500L)
    )
    sacha::sacha_arg_configure(species = "Bidens andicola")

    namespace <- asNamespace("sacha")
    global_cache <- get(".sacha_global_cache", envir = namespace)
    spatial_cache <- get(".sacha_spatial_cache", envir = namespace)
    inat_state <- get(".sacha_inat_state", envir = namespace)
    assign("global sentinel", TRUE, envir = global_cache)
    assign("spatial sentinel", TRUE, envir = spatial_cache)
    assign("media sentinel", TRUE, envir = inat_state$cache)
    usage <- list(hour = "2026-10-11 12", day = "2026-10-11",
                  hour_bytes = 1234, day_bytes = 5678)
    last_request <- as.POSIXct("2026-10-11 12:00:00", tz = "UTC")
    inat_state$media_usage <- usage
    inat_state$last_media_request <- last_request

    testthat::expect_identical(sacha::sacha_clear_cache(), TRUE)
    current <- sacha::sacha_configure()
    testthat::expect_identical(current$global$dataset, "3LXR")
    testthat::expect_identical(current$inat$default_image_size, "medium")
    testthat::expect_identical(current$inat$default_save_image_size, "medium")
    testthat::expect_identical(current$spatial$default_limit, 2000L)
    testthat::expect_length(sacha::sacha_arg_configure(), 0L)
    testthat::expect_false(exists("global sentinel", envir = global_cache, inherits = FALSE))
    testthat::expect_false(exists("spatial sentinel", envir = spatial_cache, inherits = FALSE))
    testthat::expect_false(exists("media sentinel", envir = inat_state$cache, inherits = FALSE))
    testthat::expect_identical(inat_state$media_usage, usage)
    testthat::expect_identical(inat_state$last_media_request, last_request)
  })
})

testthat::test_that("iNaturalist record-cache lookup and eviction follow the configured capacity", {
  .sacha_test_with_state({
    namespace <- asNamespace("sacha")
    defaults <- get(".sacha_inat_defaults", envir = namespace)
    defaults$cache_max_entries <- 2L
    calls <- 0L
    loader <- function() {
      calls <<- calls + 1L
      calls
    }

    testthat::expect_identical(sacha:::.inat_cached("first", loader = loader), 1L)
    testthat::expect_identical(sacha:::.inat_cached("first", loader = loader), 1L)
    testthat::expect_identical(calls, 1L)
    sacha:::.inat_cached("second", loader = loader)
    sacha:::.inat_cached("third", loader = loader)
    cache <- get(".sacha_inat_state", envir = namespace)$cache
    testthat::expect_false(exists("first", envir = cache, inherits = FALSE))
    testthat::expect_true(exists("second", envir = cache, inherits = FALSE))
    testthat::expect_true(exists("third", envir = cache, inherits = FALSE))
    testthat::expect_identical(get(".sacha_inat_state", envir = namespace)$cache_order,
                               c("second", "third"))
  })
})
