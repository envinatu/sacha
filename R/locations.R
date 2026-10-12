.sacha_spatial_col_get <- function(...) {
  segments <- c("dataset", .sacha_global_config$dataset, ...)
  encoded <- utils::URLencode(segments, reserved = TRUE)
  url <- paste(c(.sacha_global_config$base_url, encoded), collapse = "/")
  .sacha_spatial_http_json(url)
}

.sacha_spatial_col_record <- function(species) {
  species <- .sacha_spatial_validate_species(species)
  key <- paste(
    "spatial_locations",
    tolower(species),
    .sacha_global_config$dataset,
    sep = "::"
  )
  cached <- if (exists(key, envir = .sacha_global_cache, inherits = FALSE)) {
    get(key, envir = .sacha_global_cache, inherits = FALSE)
  } else {
    NULL
  }
  if (!is.null(cached) && !is.null(cached$catalogue_of_life) &&
      .sacha_spatial_cache_fresh(cached$catalogue_of_life$fetched_at)) {
    return(cached$catalogue_of_life)
  }
  cached <- cached %spatial||% list(species = species)

  match_response <- .sacha_spatial_http_json(
    paste0(.sacha_global_config$base_url, "/dataset/",
           utils::URLencode(.sacha_global_config$dataset, reserved = TRUE),
           "/match/nameusage"),
    query = list(q = species, verbose = "false")
  )

  if (identical(match_response$status, "error")) {
    result <- list(requested_species = species, usage = NULL, info = NULL,
                   status = "error", message = match_response$message,
                   fetched_at = Sys.time())
    cached$catalogue_of_life <- result
    assign(key, cached, envir = .sacha_global_cache)
    return(result)
  }

  match <- match_response$data
  usage <- match$usage %spatial||% NULL
  if (identical(match_response$status, "empty") || is.null(usage) || !is.list(usage)) {
    result <- list(requested_species = species, usage = NULL, info = NULL,
                   status = "empty", message = NULL, fetched_at = Sys.time())
    cached$catalogue_of_life <- result
    assign(key, cached, envir = .sacha_global_cache)
    return(result)
  }

  id <- .sacha_spatial_field(usage, c("id"))
  info <- NULL
  info_had_error <- FALSE
  if (!is.na(id)) {
    info_response <- .sacha_spatial_col_get("taxon", id, "info")
    if (identical(info_response$status, "ok")) {
      info <- info_response$data
    } else if (identical(info_response$status, "error")) {
      info_had_error <- TRUE
    }
  }

  result <- list(
    requested_species = species,
    usage = usage,
    info = info,
    status = if (info_had_error) "partial" else "ok",
    message = if (info_had_error) "taxon info request failed" else NULL,
    fetched_at = Sys.time()
  )
  cached$catalogue_of_life <- result
  assign(key, cached, envir = .sacha_global_cache)
  result
}

.sacha_spatial_name_parts <- function(species) {
  record <- .sacha_spatial_col_record(species)
  name_object <- record$info$usage$name %spatial||% NULL
  scientific_name <- if (is.list(name_object)) {
    .sacha_spatial_field(name_object, c("scientificName", "name"), default = species)
  } else {
    .sacha_spatial_field(record$usage, c("name", "scientificName"), default = species)
  }
  authorship <- if (is.list(name_object)) {
    .sacha_spatial_field(name_object, c("authorship", "author"), default = NA_character_)
  } else {
    .sacha_spatial_field(record$usage, c("authorship", "author"), default = NA_character_)
  }

  if (!is.na(scientific_name) && !is.na(authorship)) {
    return(list(scientific_name = scientific_name, authorship = authorship))
  }

  label <- .sacha_spatial_field(record$info$usage, c("label"), default = NA_character_)
  if (is.na(label)) label <- .sacha_spatial_field(record$usage, c("label"), default = species)
  words <- strsplit(trimws(label), "\\s+")[[1]]
  author_start <- which(grepl("^[A-Z(]", words))[-1]
  if (!length(author_start)) {
    list(scientific_name = scientific_name %spatial||% species, authorship = "")
  } else {
    index <- author_start[1]
    list(
      scientific_name = paste(words[seq_len(index - 1L)], collapse = " "),
      authorship = paste(words[index:length(words)], collapse = " ")
    )
  }
}

.sacha_spatial_display_name <- function(species, markdown = FALSE) {
  parts <- .sacha_spatial_name_parts(species)
  name <- parts$scientific_name %spatial||% species
  authorship <- parts$authorship %spatial||% ""
  if (markdown) name <- paste0("*", name, "*")
  trimws(paste(name, authorship))
}

.sacha_spatial_col_areas <- function(species) {
  record <- .sacha_spatial_col_record(species)
  distributions <- record$info$distributions %spatial||% list()
  if (!length(distributions)) return(character())

  areas <- vapply(distributions, function(distribution) {
    area <- distribution$area %spatial||% NULL
    if (is.list(area)) .sacha_spatial_field(area, c("name")) else .sacha_spatial_scalar(area)
  }, character(1))
  unique(areas[!is.na(areas) & nzchar(areas)])
}

.sacha_spatial_locations_text <- function(species, language, markdown, strict) {
  species <- .sacha_spatial_validate_species(species)
  language <- .sacha_spatial_language(language)
  record <- .sacha_spatial_col_record(species)
  name <- .sacha_spatial_display_name(species, markdown = markdown)

  if (identical(record$status, "error")) {
    if (strict) {
      stop(sprintf("%s %s (%s)",
                   .sacha_spatial_label("communication_error", language), name,
                   record$message %spatial||% "unknown error"),
           call. = FALSE)
    }
    areas <- character()
  } else {
    areas <- .sacha_spatial_col_areas(species)
  }

  if (!length(areas)) {
    return(paste0(.sacha_spatial_label("no_locations", language), " ", name, "."))
  }

  paste0(
    name, " ", .sacha_spatial_label("locations_suffix", language), ":\n\n",
    paste(areas, collapse = "; "), "."
  )
}

#' Retrieve Catalogue of Life geographic locations for a species
#'
#' `locations()` prints plain text to the console and raises a communication
#' error when ChecklistBank is unavailable. `locations_md()` prints
#' markdown-formatted text and converts an unavailable service into the same
#' no-data wording used when no geographic areas are documented, so a report
#' render is not stopped.
#'
#' @param species A binomial scientific name in the form `"Genus species"`,
#'   e.g. `"Bidens andicola"`, `"Cinchona officinalis"`, or `"Puma
#'   concolor"`.
#' @param language Display language: `"es"` (default) or `"en"`, or any
#'   other alias defined in the package's language files.
#' @return Invisibly returns the emitted string: plain text for `locations()`
#'   and markdown for `locations_md()`. The text contains the scientific name,
#'   authorship when available, and unique documented geographic areas.
#' @examples
#' \donttest{
#' locations("Bidens andicola")
#' locations_md("Bidens andicola")
#' }
#' @export
locations <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  text <- .sacha_spatial_locations_text(species, language, markdown = FALSE, strict = TRUE)
  cat(text, "\n", sep = "")
  invisible(text)
}

#' @rdname locations
#' @export
locations_md <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  language <- .sacha_spatial_language(language)
  text <- .sacha_spatial_locations_text(species, language, markdown = TRUE, strict = FALSE)
  text <- paste0("**", .sacha_spatial_label("locations_title", language), "**\n\n", text)
  cat(text, "\n", sep = "")
  invisible(text)
}
