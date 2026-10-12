# R/sacha-package.R
#
# Package-level documentation and shared conventions. This file has no
# executable logic of its own -- it exists so that `?sacha` and the
# pkgdown reference index have a landing page, and so that documentation
# conventions used across every module (coldp, spatial, inaturalist,
# core) are written down once instead of being re-decided per file.

#' sacha: Ecuadorian Taxonomic and Ethnobotanical Data Toolkit
#'
#' Tools to resolve scientific names, retrieve taxonomic classification,
#' conservation status, common names, sustainable-use records, and
#' cross-platform indexation URLs from local ColDP archives and web
#' services (ChecklistBank / Catalogue of Life, GBIF, iNaturalist); to
#' build occurrence maps; and to emit all of the above as
#' markdown-formatted text ready for inclusion in a report.
#'
#' @section Package modules:
#' \itemize{
#'   \item \strong{coldp} -- accessors reading a local ColDP archive
#'     (\code{sacha_coldp_configure()}, \code{sc_full_name_coldp()}, etc.).
#'   \item \strong{core} -- web-backed accessors querying ChecklistBank,
#'     GBIF, and iNaturalist directly (\code{sacha_configure()},
#'     \code{sc_full_name()}, \code{classification()}, etc.).
#'   \item \strong{spatial} -- GBIF occurrence maps and Catalogue of Life
#'     geographic locations (\code{gbif_map()}, \code{col_locations()}).
#'   \item \strong{inaturalist} -- iNaturalist observation images, local
#'     archiving, and markdown-formatted figure panels
#'     (\code{inat_images()}, \code{inat_save_images()}).
#' }
#'
#' @section Documenting plain / `_md` twin functions:
#' Many accessors ship in pairs: a plain-text version and a `_md`
#' (Markdown) version intended for use inside a rendered report. Whether
#' a pair shares one help page or gets two follows a single rule:
#'
#' \itemize{
#'   \item If the `_md` twin has an \strong{identical signature} to its
#'     plain counterpart and differs \strong{only} in output format,
#'     document both together with a shared \code{@rdname} (see the
#'     \code{coldp} module for the canonical example).
#'   \item If the `_md` twin introduces \strong{its own parameters}
#'     (e.g. `fig_width`, `fig_height` in \code{gbif_map_md()}) and/or
#'     \strong{diverges in error-handling contract} (e.g. a
#'     communication failure is silenced in the `_md` variant but raised
#'     as an error in the plain one, as in \code{col_locations()} vs.
#'     \code{col_locations_md()}), document them on \strong{separate}
#'     pages instead.
#' }
#'
#' Never force a shared `@rdname` onto a pair that fails either check --
#' a page whose `Usage` section lists two mismatched signatures, or whose
#' `Value` section has to describe two different error contracts, is
#' harder to read than two short separate pages.
#'
#' @section Documenting one-line wrapper families:
#' A family of thin wrappers around one underlying function (e.g. the
#' `*_url` accessors -- `ipni_url()`, `gbif_url()`, `col_url()`, etc. --
#' all delegating to \code{indexations()}) should share a single
#' `@rdname` page with one `@param`/`@return`/`@examples` block and a
#' `@details` table mapping each function name to what it fixes (e.g.
#' its `identifier`). Each wrapper keeps its own `@export`; only the
#' documentation is consolidated.
#'
#' @section Avoiding duplicated `@param` blocks:
#' When two \strong{non-twin} functions share most of their parameters
#' but return different things or have different side effects (e.g.
#' \code{inat_images()}, which returns an object, vs.
#' \code{inat_save_images()}, which writes files to disk), do not repeat
#' the shared `@param` entries. Use `@inheritParams <reference_function>`
#' and document only the parameters that are genuinely specific to the
#' function at hand.
#'
#' @section Language resolution:
#' Every module resolves its `language` argument dynamically against the
#' `.yml` files shipped under \code{inst/lang/}: any file
#' \code{inst/lang/<code>.yml} (plus any `aliases:` it declares) becomes
#' a valid `language` value without changing any R code. Do not hardcode
#' a closed set such as \code{language = c("es", "en")} with
#' \code{match.arg()} in new code -- that silently breaks this
#' extensibility contract. If a module currently does this, it is a known
#' inconsistency to be fixed, not a pattern to copy.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL

# -----------------------------------------------------------------------
# Global variables referenced via non-standard evaluation (ggplot2 aes(),
# dplyr .data pronouns handled separately above). Add here only symbols
# that trigger an R CMD check NOTE and have no other fix.
# -----------------------------------------------------------------------
utils::globalVariables(c("decimalLongitude", "decimalLatitude"))

#' Configure package modules
#'
#' Inspect or update the configuration of the ChecklistBank-backed accessors,
#' iNaturalist image functions, and spatial functions. Each module has its own
#' settings and validation. Pass settings in named lists so that identically
#' named options, such as `language`, retain a clear module-specific scope.
#' With no arguments, the function visibly returns the active configuration
#' grouped by module. When one or more groups are supplied, only their listed
#' settings are changed and the resulting configuration is returned invisibly.
#'
#' The `global` group configures ChecklistBank accessors and taxonomic
#' indexation links. Its available fields are `dataset`, `base_url`,
#' `gbif_base`, `inat_base`, `source_name`, `taxon_url_template`, `language`,
#' `fetcher`, and `labels`. `dataset` defaults to `"3LXR"`; URL fields must
#' use HTTP or HTTPS. `language` accepts an installed language file or one of
#' its declared aliases. `taxon_url_template` may be `NULL` to omit taxon
#' links, and `fetcher` may be `NULL` to restore the built-in ChecklistBank
#' adapter. `labels` is a named list of language-specific label overrides.
#' This configuration is shared by `sc_full_name()` and the Catalogue of Life
#' lookups in `locations()`.
#'
#' The `inat` group configures iNaturalist requests and image handling. It
#' accepts `inat_api`, `checklistbank_api`, `catalogue_dataset`, `user_agent`,
#' `language`, `default_quality_grade`, `default_images_directory`,
#' `default_image_size`, `default_save_image_size`, `cache_max_entries`,
#' `request_delay`, `small_download_delay`, `large_download_delay`,
#' `hourly_media_limit_gb`, and `daily_media_limit_gb`. `default_quality_grade`
#' accepts `"research"`, `"needs_id"`, or `"casual"`; image sizes accept
#' `"small"`, `"square"`, `"medium"`, `"large"`, or `"original"`. Both
#' `default_image_size` and `default_save_image_size` default to `"medium"`.
#' The cache capacity must be a positive whole number or `Inf`. Search and
#' larger-media delays must be at least 1 second; smaller-media delays must
#' be at least 0.5 seconds. Media ceilings must be positive and no greater
#' than 5 GB per hour or 24 GB per day. Defaults are 1.02 seconds,
#' 0.55 seconds, 4.5 GiB per hour, and 21.5 GiB per day. The language must
#' identify an installed
#' language file, endpoint URLs must use HTTP or HTTPS, and `user_agent` must
#' be non-empty.
#'
#' The `spatial` group accepts `language`, `default_limit`, `http_timeout`,
#' `http_max_tries`, and `cache_ttl`. The defaults are English, 2,000 records,
#' a 30-second HTTP timeout, three attempts, and an unlimited cache age.
#' `default_limit` must be between 1 and 100,000; `http_timeout` and
#' `cache_ttl` must be positive, with `Inf` accepted for `cache_ttl`; and
#' `http_max_tries` must be at least one.
#'
#' Settings in different groups remain independent even when they have similar
#' names. For example, `language` in `global`, `inat`, and `spatial` changes
#' only that module's default. The global ChecklistBank dataset and API are
#' shared by `sacha_clb.R` and `locations.R`; the iNaturalist
#' attribution dataset remains independently configurable.
#'
#' @references
#' ChecklistBank API: <https://api.checklistbank.org/>.
#' GBIF API documentation: <https://techdocs.gbif.org/en/openapi/>.
#' iNaturalist API v2: <https://api.inaturalist.org/v2/docs/>.
#' iNaturalist API recommended practices:
#' <https://www.inaturalist.org/pages/api+recommended+practices>.
#'
#' @param global Optional named list of settings for the ChecklistBank and
#'   taxonomic-indexation module.
#' @param inat Optional named list of settings for iNaturalist image queries,
#'   downloads, and cache safeguards.
#' @param spatial Optional named list of settings for GBIF maps and spatial
#'   accessors.
#' @return The nested active configuration. It is returned visibly for a
#'   query and invisibly after an update.
#' @seealso [sacha_clear_cache()], [sacha_arg_configure()]
#' @examples
#' \donttest{
#' sacha_configure()
#' sacha_configure(
#'   global = list(language = "es", dataset = "COL25"),
#'   inat = list(language = "en", default_image_size = "large"),
#'   spatial = list(default_limit = 500)
#' )
#' }
#' @export
sacha_configure <- function(global = NULL, inat = NULL, spatial = NULL) {
  groups <- list(global = global, inat = inat, spatial = spatial)
  invalid <- names(groups)[!vapply(groups, function(x) {
    is.null(x) || (is.list(x) && (length(x) == 0L || (
      !is.null(names(x)) &&
        length(names(x)) == length(x) &&
        all(!is.na(names(x)) & nzchar(names(x))) &&
        !anyDuplicated(names(x))
    )))
  }, logical(1))]
  if (length(invalid)) {
    stop("Each configuration group must be NULL or a fully named list.",
         call. = FALSE)
  }
  if (all(vapply(groups, is.null, logical(1)))) {
    return(list(
      global = .sacha_global_configure(),
      inat = .sacha_inat_configure(),
      spatial = .sacha_spatial_configure()
    ))
  }

  old_dataset <- .sacha_global_config$dataset
  old_base_url <- .sacha_global_config$base_url
  if (!is.null(global)) do.call(.sacha_global_configure, global)
  if (!is.null(inat)) do.call(.sacha_inat_configure, inat)
  if (!is.null(spatial)) do.call(.sacha_spatial_configure, spatial)

  if (!identical(old_dataset, .sacha_global_config$dataset) ||
      !identical(old_base_url, .sacha_global_config$base_url)) {
    .sacha_spatial_clear_cache()
  }

  invisible(list(
    global = .sacha_global_configure(),
    inat = .sacha_inat_configure(),
    spatial = .sacha_spatial_configure()
  ))
}

.sacha_arg_defaults <- new.env(parent = emptyenv())

.sacha_arg_supported_functions <- c(
  "sc_full_name", "sc_full_name_md", "first_publicacion",
  "first_publicacion_md", "classification", "classification_md",
  "common_names", "common_names_md", "syn_and_comb", "syn_and_comb_md",
  "references_sp", "references_sp_md", "indexations", "indexations_md",
  "ipni_url", "gbif_url", "inat_url", "col_url", "wfo_url", "tpl_url",
  "itis_url", "ncbi_url", "irmng_url", "worms_url", "eol_url",
  "locations", "locations_md", "gbif_map", "gbif_map_md",
  "gbif_save_occurrences", "gbif_map_crop", "gbif_map_crop_md",
  "inat_images", "inat_images_md", "inat_save_images", "sacha_clb_report",
  "sacha_spatial_report", "sacha_report", "save_clb"
)

.sacha_arg_supported <- function() {
  available <- .sacha_arg_supported_functions[vapply(
    .sacha_arg_supported_functions,
    exists,
    logical(1),
    envir = environment(.sacha_arg_supported),
    mode = "function",
    inherits = TRUE
  )]
  formal_names <- unlist(lapply(available, function(name) {
    setdiff(
      names(formals(get(name, envir = environment(.sacha_arg_supported)))),
      "..."
    )
  }), use.names = FALSE)
  report_arguments <- c(
    "user_id", "number_observations", "observation_id", "image_size",
    "verify_local", "images_directory", "quality_grade", "place_id",
    "catalogue_dataset", "observer_name", "save_images", "export_comp",
    "refresh", "ncol", "word_subfigure_width", "latex_subfigure_width",
    "identifier", "rank", "save_bib", "bib_dir", "kml", "crop_area",
    "crop_box", "limit", "mode", "save_occurrences", "export_image",
    "directory", "colour", "dpi", "ppp", "overwrite", "distribution",
    "fig_width", "fig_height", "local_export"
  )
  unique(c(formal_names, report_arguments))
}

#' Set shared defaults for function arguments
#'
#' Store argument values that package functions will use when the corresponding
#' argument is omitted. An explicitly supplied value in a function call always
#' takes precedence. Defaults are shared by name across the functions that
#' accept that argument; for example, `species`, `language`, and `source` can
#' be set once for a report workflow. A configured value is validated by the
#' receiving function when it is used. Passing `NULL` for a configured name
#' removes that override and restores the function's own default.
#'
#' The function accepts the argument names supported by the package's
#' taxonomic, spatial, iNaturalist, and report accessors. For report functions,
#' defaults for arguments forwarded through `...` are also applied unless the
#' report call or its forwarded arguments provide a value explicitly. Repeated
#' names are rejected to avoid ambiguous settings. The meaning and accepted
#' values remain function-specific: for example, `source = "local"` applies
#' to both GBIF and iNaturalist functions, while `"web"` is accepted by
#' iNaturalist and reports but a direct GBIF query uses `"gbif"` for its web
#' source.
#'
#' Supported argument names are `bib_dir`, `catalogue_dataset`, `colour`,
#' `crop_area`, `crop_box`, `dataset`, `directory`, `distribution`, `dpi`,
#' `export_comp`, `export_image`, `fig_height`, `fig_width`, `identifier`,
#' `image_size`, `images_directory`, `include_auxiliary`, `kml`, `language`,
#' `latex_subfigure_width`, `limit`, `local_export`, `mode`, `ncol`,
#' `number_observations`, `observation_id`, `observer_name`, `overwrite`,
#' `place_id`, `ppp`, `quality_grade`, `rank`, `refresh`, `save_bib`,
#' `save_images`, `save_occurrences`, `source`, `species`, `user_id`,
#' `verify_local`, `word_subfigure_width`, and `zoom_static`.
#'
#' Calling with no arguments visibly returns the active overrides. Updating
#' one or more values changes only those entries and returns the complete set
#' invisibly. [sacha_clear_cache()] removes all overrides.
#'
#' @param ... Named argument values to use as shared defaults. Names must
#'   correspond to arguments accepted by the package's supported functions.
#' @return The active named defaults, visible for a query and invisible after
#'   an update.
#' @seealso [sacha_configure()], [sacha_clear_cache()]
#' @examples
#' \donttest{
#' inat_images(species = "Bidens andicola", source = "local", number_observations = 1)
#' sacha_arg_configure(
#'   species = "Bidens andicola", source = "local",
#'   number_observations = 2, image_size = "medium"
#' )
#' inat_images()
#' sacha_arg_configure(species = "Bidens andicola", rank = "family")
#' sacha_arg_configure()
#' sacha_arg_configure(source = NULL)
#' sacha_clear_cache()
#' }
#' @export
sacha_arg_configure <- function(...) {
  values <- list(...)
  if (!length(values)) return(as.list(.sacha_arg_defaults))
  if (is.null(names(values)) || any(!nzchar(names(values)))) {
    stop("All configured arguments must be named.", call. = FALSE)
  }
  if (anyDuplicated(names(values))) {
    stop("Each shared argument may be configured only once per call.",
         call. = FALSE)
  }
  unknown <- setdiff(names(values), .sacha_arg_supported())
  if (length(unknown)) {
    stop("Unsupported shared argument(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  for (name in names(values)) {
    value <- values[[name]]
    if (is.null(value)) {
      if (exists(name, envir = .sacha_arg_defaults, inherits = FALSE)) {
        rm(list = name, envir = .sacha_arg_defaults)
      }
    } else {
      assign(name, value, envir = .sacha_arg_defaults)
    }
  }
  invisible(as.list(.sacha_arg_defaults))
}

.sacha_apply_arg_defaults <- function(frame, call, formal_names) {
  configured <- ls(envir = .sacha_arg_defaults, all.names = TRUE)
  if (!length(configured)) return(invisible(NULL))
  supplied <- names(as.list(call)[-1L])
  formal_names <- setdiff(formal_names, "...")
  for (name in intersect(configured, formal_names)) {
    if (!name %in% supplied) {
      assign(name, get(name, envir = .sacha_arg_defaults, inherits = FALSE),
             envir = frame)
    }
  }
  invisible(NULL)
}

.sacha_merge_arg_defaults <- function(supplied, supported) {
  configured <- as.list(.sacha_arg_defaults)
  configured <- configured[intersect(names(configured), supported)]
  configured <- configured[setdiff(names(configured), names(supplied))]
  c(configured, supplied)
}

.sacha_reset_configuration <- function() {
  .sacha_global_config$dataset <- "3LXR"
  .sacha_global_config$base_url <- "https://api.checklistbank.org"
  .sacha_global_config$gbif_base <- "https://api.gbif.org/v1"
  .sacha_global_config$inat_base <- "https://api.inaturalist.org/v2"
  .sacha_global_config$source_name <- "Catalogue of Life"
  .sacha_global_config$taxon_url_template <- "https://www.catalogueoflife.org/data/taxon/%s"
  .sacha_global_config$language <- "en"
  .sacha_global_config$fetcher <- NULL
  .sacha_global_config$label_overrides <- list()

  .sacha_spatial_config$language <- "en"
  .sacha_spatial_config$gbif_checklist_key <- "7ddf754f-d193-4cc9-b351-99906754a03b"
  .sacha_spatial_config$default_limit <- 2000L
  .sacha_spatial_config$http_timeout <- 30
  .sacha_spatial_config$http_max_tries <- 3L
  .sacha_spatial_config$cache_ttl <- Inf

  .sacha_inat_defaults$inat_api <- "https://api.inaturalist.org/v2"
  .sacha_inat_defaults$checklistbank_api <- "https://api.checklistbank.org"
  .sacha_inat_defaults$catalogue_dataset <- "3LXR"
  .sacha_inat_defaults$user_agent <- "sacha-r-package/1.0 (iNaturalist image client)"
  .sacha_inat_defaults$language <- "en"
  .sacha_inat_defaults$default_quality_grade <- "research"
  .sacha_inat_defaults$default_images_directory <- "."
  .sacha_inat_defaults$default_image_size <- "medium"
  .sacha_inat_defaults$default_save_image_size <- "medium"
  .sacha_inat_defaults$cache_max_entries <- 200L
  .sacha_inat_defaults$request_delay <- 1.02
  .sacha_inat_defaults$small_download_delay <- 0.55
  .sacha_inat_defaults$large_download_delay <- 1.02
  .sacha_inat_defaults$hourly_media_limit_gb <- 4.5
  .sacha_inat_defaults$daily_media_limit_gb <- 21.5

  configured <- ls(envir = .sacha_arg_defaults, all.names = TRUE)
  if (length(configured)) rm(list = configured, envir = .sacha_arg_defaults)
  invisible(NULL)
}

#' Clear package caches and restore defaults
#'
#' Removes cached ChecklistBank, GBIF, and iNaturalist records, including the
#' cached full scientific-name records and Catalogue of Life locations. It
#' also restores the configuration of all modules and the shared argument
#' defaults set with [sacha_arg_configure()] to their package defaults, then
#' reloads the installed language dictionaries.
#' This function takes no arguments.
#'
#' The iNaturalist media-volume counters and inter-download timer are
#' deliberately preserved. They track downloads made during the current R
#' session and are safety controls rather than cached query results; clearing
#' data or restoring defaults must not reset them.
#'
#' @return Invisibly returns `TRUE`.
#' @seealso [sacha_configure()], [sacha_arg_configure()]
#' @examples
#' \donttest{
#' sacha_configure(
#'   global = list(language = "es"),
#'   inat = list(default_image_size = "large")
#' )
#' sacha_arg_configure(species = "Bidens andicola", source = "local")
#' sacha_clear_cache()
#' }
#' @export
sacha_clear_cache <- function() {
  .sacha_reset_configuration()
  .sacha_global_clear_cache()
  .sacha_inat_clear_cache(reset_download_state = FALSE)
  .sacha_spatial_clear_cache(reload_languages = TRUE)
  global_languages <- ls(envir = .sacha_global_lang_cache, all.names = TRUE)
  if (length(global_languages)) {
    rm(list = global_languages, envir = .sacha_global_lang_cache)
  }
  invisible(TRUE)
}
