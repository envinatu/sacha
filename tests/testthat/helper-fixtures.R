.sacha_test_fixture_species <- "Bidens andicola"

.sacha_test_global_record <- function(species = .sacha_test_fixture_species) {
  list(
    scientific_name = species,
    authorship = "Kunth",
    first_publication = "Kunth. 1818. Nova Genera et Species Plantarum.",
    classification = list(
      list(rank = "kingdom", name = "Plantae", authorship = NA_character_, id = "P"),
      list(rank = "family", name = "Asteraceae", authorship = "Martinov", id = "A")
    ),
    common_names = list(es = c("Nombre local", "Nombre local"), en = "Common name"),
    synonyms = list(
      homotypic = list(
        list(name = list(scientificName = "Bidens synonym", authorship = "Author"))
      ),
      heterotypic = list(),
      misapplied = list()
    ),
    references = list(
      list(id = "reference-z", citation = "Zulu, A. 2020. Source."),
      list(id = "reference-a", citation = "Alpha, B. 2019. Source."),
      list(id = "reference-duplicate", citation = "Alpha, B.   2019. Source.")
    ),
    identifiers = c(col = "COL123", gbif = "456", inat = "789"),
    links = character()
  )
}

.sacha_test_inat_observation <- function(
    id = 1001L,
    observed_on = "2026-10-10T12:30:00Z",
    species = .sacha_test_fixture_species
) {
  list(
    id = id,
    taxon = list(id = 123L, name = species),
    user = list(name = "Observer One", login = "observer_one"),
    place_guess = "Saquisili, Ecuador",
    observed_on = observed_on,
    photos = list(
      list(
        id = 11L,
        url = "https://example.org/photo-11-square.jpg",
        license_code = "cc-by",
        user = list(name = "Photographer One", login = "photo_one")
      ),
      list(
        id = 12L,
        url = "https://example.org/photo-12-square.jpg",
        license_code = "all-rights-reserved",
        user = list(name = "Photographer Two", login = "photo_two")
      ),
      list(
        id = 13L,
        url = "https://example.org/photo-13-square.jpg",
        license_code = "cc-by-nc-nd",
        user = list(name = "Photographer Three", login = "photo_three")
      )
    )
  )
}

.sacha_test_state_environments <- function() {
  namespace <- asNamespace("sacha")
  names <- c(
    ".sacha_global_config", ".sacha_global_cache", ".sacha_global_tracker",
    ".sacha_global_lang_cache", ".sacha_spatial_config",
    ".sacha_spatial_cache", ".sacha_spatial_i18n", ".sacha_inat_defaults",
    ".sacha_inat_state", ".sacha_arg_defaults"
  )
  environments <- lapply(names, get, envir = namespace, inherits = FALSE)
  names(environments) <- names
  environments$inat_cache <- get(".sacha_inat_state", envir = namespace)$cache
  environments
}

.sacha_test_snapshot_environment <- function(environment) {
  as.list(environment, all.names = TRUE)
}

.sacha_test_restore_environment <- function(environment, snapshot) {
  current <- ls(envir = environment, all.names = TRUE)
  if (length(current)) {
    rm(list = current, envir = environment)
  }
  if (length(snapshot)) {
    list2env(snapshot, envir = environment)
  }
  invisible(NULL)
}

.sacha_test_with_state <- function(code) {
  environments <- .sacha_test_state_environments()
  snapshots <- lapply(environments, .sacha_test_snapshot_environment)
  on.exit({
    for (name in rev(names(environments))) {
      .sacha_test_restore_environment(environments[[name]], snapshots[[name]])
    }
  }, add = TRUE)
  force(code)
}

.sacha_test_skip_without_namespace_mocks <- function() {
  if (!("local_mocked_bindings" %in% getNamespaceExports("testthat"))) {
    testthat::skip("This test requires testthat::local_mocked_bindings().")
  }
}
