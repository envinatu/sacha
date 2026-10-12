#' Plain-text and Markdown accessors
#'
#' Functions that retrieve data for a single taxon are provided in two
#' variants: a plain-text accessor, intended for interactive inspection in the
#' console, and a Markdown accessor with the suffix `_md`, intended for
#' document rendering (Quarto or R Markdown). Both variants read the same
#' cached record and differ only in output format and in the handling of
#' missing data and connection failures.
#'
#' @section Function pairs:
#' \itemize{
#'   \item `first_publicacion()` and `first_publicacion_md()`
#'   \item `classification()` and `classification_md()`
#'   \item `common_names()` and `common_names_md()`
#'   \item `syn_and_comb()` and `syn_and_comb_md()`
#'   \item `references_sp()` and `references_sp_md()`
#'   \item `indexations()` and `indexations_md()`
#'   \item `sc_full_name()` and `sc_full_name_md()`
#' }
#' The platform shortcuts documented in [indexations()] (for example
#' `gbif_url()`) follow the plain-text behavior described below.
#'
#' @section Plain-text accessors:
#' Plain-text accessors return a character string of class `sacha_text`.
#' When printed, the text is shown as written, with one line per line break,
#' without quotation marks, element indices or escape sequences. The object
#' behaves as an ordinary character string in `paste()`,
#' `as.character()` and comparisons. For missing data and failures:
#' \itemize{
#'   \item When the source returns no data, a localized no-data message is
#'         returned.
#'   \item When a connection failure prevents retrieval, an R
#'         \code{warning()} is raised and a localized connection message is
#'         returned.
#' }
#' `indexations()` and the platform shortcuts are the exception to the
#' return type: they return a named character vector of URLs, which is empty
#' when no URL is available, and raise a warning when that absence is caused by
#' a connection failure. `sc_full_name()` never warns: if the name cannot be
#' resolved, it returns `species` as given.
#'
#' @section Markdown accessors:
#' Markdown accessors write the Markdown string to the console with
#' \code{cat()} so that it is inserted as-is in chunks with
#' \code{results = "asis"}, and return the same string invisibly. For missing
#' data and failures:
#' \itemize{
#'   \item An empty string (\code{""}) is returned invisibly, and nothing is
#'         written, when no data are available. Sections without data are
#'         therefore omitted from the document.
#'   \item No R \code{warning()} is raised, including on connection failures.
#' }
#' Scientific names are written in italics, special Markdown characters are
#' escaped, and grouped content (synonyms, references) is written as
#' bulleted lists.
#'
#' @section Shared behavior:
#' Both variants use the same per-species cache for the duration of the R
#' session (see [sacha_clear_cache()]) and the same configuration (see
#' [sacha_configure()]). The `language` argument affects only labels
#' and messages, never the data. Invalid arguments (a `species` that is not a
#' binomial, an unsupported `language`, an unknown `rank` or `identifier`, or
#' a non-logical `save_bib`) raise an error in both variants. The
#' `sc_full_name()` pair has an independent cache and always queries the
#' Catalogue of Life dataset `"3LXR"`.
#'
#' @section Shared export directory:
#' Files explicitly saved by package functions are organised under
#' `sacha_exports` inside the relevant base directory, which defaults to the
#' current working directory. Taxon-specific outputs use
#' `sacha_exports/Family_Genus_species`; GBIF crop exports add a place
#' subdirectory, while iNaturalist photographs, metadata and compositions are
#' stored in `sacha_exports/Family_Genus_species/inat`. Taxon-level BibTeX
#' references and GBIF occurrence data and maps remain in the taxon directory.
#' A complete ChecklistBank dataset is not taxon-specific, so its archive and
#' download metadata are stored in `sacha_exports/checklistbank_<dataset>`.
#'
#' In functions with a `directory`, `images_directory`, or `bib_dir` argument,
#' that value is the base directory under which `sacha_exports` is created or
#' searched. The argument documentation identifies the resulting path for
#' each function. Existing spatial occurrence exports in `spatial_exports` and
#' iNaturalist archives in `images` remain readable through the local-source
#' functions; new package exports use `sacha_exports`.
#'
#' Figures created only to render a Markdown report are written to the
#' format-specific paths managed by Quarto or R Markdown. These intermediate
#' assets are kept separate so that simultaneous rendering to multiple output
#' formats does not cause one format to overwrite another. Persistent map PNGs
#' are saved under `sacha_exports/Family_Genus_species`; iNaturalist
#' compositions are saved under `sacha_exports/Family_Genus_species/inat`.
#'
#' @section Choosing between them:
#' Use the plain-text accessors to inspect a taxon interactively or to obtain
#' a value for further computation. Use the Markdown accessors to assemble
#' reports of one or several taxa, where unavailable data must be omitted
#' without interrupting rendering.
#'
#' @name accessors-sacha
NULL

.sacha_exports_root <- function(directory = getwd()) {
  directory <- normalizePath(path.expand(directory), winslash = "/", mustWork = FALSE)
  file.path(directory, "sacha_exports")
}

.sacha_path_component <- function(x, fallback = "Unknown") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]]) ||
      !nzchar(trimws(as.character(x[[1L]])))) {
    return(fallback)
  }
  value <- iconv(trimws(as.character(x[[1L]])), from = "", to = "ASCII//TRANSLIT")
  value <- gsub("[^A-Za-z0-9]+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) fallback else value
}

.sacha_taxon_directory_name <- function(family, species) {
  species <- gsub("\\s+", " ", trimws(as.character(species)[1L]))
  words <- strsplit(species, " ", fixed = TRUE)[[1L]]
  if (length(words) < 2L || anyNA(words[1:2]) || any(!nzchar(words[1:2]))) {
    stop("`species` must contain a genus and a specific epithet.", call. = FALSE)
  }
  paste(
    .sacha_path_component(family, "Unknown_family"),
    .sacha_path_component(words[[1L]]),
    .sacha_path_component(words[[2L]]),
    sep = "_"
  )
}

.sacha_taxon_export_directory <- function(directory, family, species,
                                           subdirectory = NULL) {
  taxon_directory <- file.path(
    .sacha_exports_root(directory),
    .sacha_taxon_directory_name(family, species)
  )
  if (is.null(subdirectory)) {
    return(taxon_directory)
  }
  file.path(taxon_directory, .sacha_path_component(subdirectory))
}
