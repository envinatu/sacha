.sacha_inat_defaults <- new.env(parent = emptyenv())
.sacha_inat_defaults$inat_api <- "https://api.inaturalist.org/v2"
.sacha_inat_defaults$checklistbank_api <- "https://api.checklistbank.org"
.sacha_inat_defaults$catalogue_dataset <- "3LXR"
.sacha_inat_defaults$user_agent <- "sacha-r-package/1.0 (iNaturalist image client)"
.sacha_inat_defaults$language <- "en"
.sacha_inat_defaults$default_quality_grade <- "research"
.sacha_inat_defaults$default_image_size <- "medium"
.sacha_inat_defaults$default_save_image_size <- "medium"
.sacha_inat_defaults$cache_max_entries <- 200L
.sacha_inat_defaults$request_delay <- 1.02
.sacha_inat_defaults$small_download_delay <- 0.55
.sacha_inat_defaults$large_download_delay <- 1.02
.sacha_inat_defaults$hourly_media_limit_gb <- 4.5
.sacha_inat_defaults$daily_media_limit_gb <- 21.5


.inat_fields_taxon_detail <- "(id:!t,name:!t,rank:!t,ancestors:(id:!t,name:!t,rank:!t))"
.inat_fields_taxon_search <- "(id:!t,name:!t,rank:!t)"
.inat_fields_observation <- paste0(
  "(id:!t,taxon:(id:!t,name:!t),user:(name:!t,login:!t),",
  "place_guess:!t,observed_on:!t,license_code:!t,",
  "photos:(id:!t,url:!t,license_code:!t,user:(name:!t,login:!t)))"
)

.sacha_inat_state <- new.env(parent = emptyenv())
.sacha_inat_state$cache <- new.env(parent = emptyenv())
.sacha_inat_state$cache_order <- character(0)
.sacha_inat_state$media_usage <- list(hour = "", day = "", hour_bytes = 0, day_bytes = 0)
.sacha_inat_state$last_media_request <- NULL

`%inat||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }

  if (is.list(x)) {
    if (length(x) == 0L) {
      return(y)
    }
    return(x)
  }

  if (length(x) == 0L) {
    return(y)
  }

  if (length(x) == 1L && (is.na(x) || identical(x, ""))) {
    return(y)
  }

  x
}

.inat_lang_package <- "sacha"

.inat_lang_dir <- function() {
  installed_dir <- tryCatch(
    system.file("lang", package = .inat_lang_package),
    error = function(error) ""
  )

  if (nzchar(installed_dir) && dir.exists(installed_dir)) {
    return(installed_dir)
  }

  development_candidates <- c(
    file.path("inst", "lang"),
    file.path("..", "inst", "lang"),
    file.path(".", "lang")
  )

  for (candidate in development_candidates) {
    if (dir.exists(candidate)) {
      return(candidate)
    }
  }

  stop(
    "The package language directory could not be located. Expected an ",
    "'inst/lang' directory containing one '<language>.yml' file per ",
    "language (e.g. 'es.yml', 'en.yml').",
    call. = FALSE
  )
}

.inat_available_languages <- function(refresh = FALSE) {
  .inat_cached("available-languages", refresh, function() {
    lang_dir <- .inat_lang_dir()
    lang_files <- list.files(lang_dir, pattern = "\\.ya?ml$")
    languages <- sort(tools::file_path_sans_ext(lang_files))

    if (length(languages) == 0L) {
      stop(
        "No language files were found in '", lang_dir, "'. Add at least ",
        "one '<language>.yml' file (e.g. 'es.yml') to enable messages.",
        call. = FALSE
      )
    }

    languages
  })
}

.inat_language_choices <- function(preferred = "en") {
  languages <- .inat_available_languages()

  if (preferred %in% languages) {
    return(c(preferred, setdiff(languages, preferred)))
  }

  languages
}

.inat_argument_defaults <- function(save_image_size = FALSE) {
  image_size_options <- if (isTRUE(save_image_size)) {
    c("original", "large", "medium", "small", "square")
  } else {
    c("medium", "large", "original", "small", "square")
  }
  default_image_size <- if (isTRUE(save_image_size)) {
    .sacha_inat_defaults$default_save_image_size
  } else {
    .sacha_inat_defaults$default_image_size
  }
  language_choices <- .inat_language_choices(
    preferred = .sacha_inat_defaults$language
  )

  list(
    image_size = default_image_size,
    image_size_choices = c(
      default_image_size,
      setdiff(image_size_options, default_image_size)
    ),
    quality_grade = .sacha_inat_defaults$default_quality_grade,
    catalogue_dataset = .sacha_inat_defaults$catalogue_dataset,
    language = language_choices[[1L]],
    language_choices = language_choices
  )
}

.inat_load_language <- function(language, refresh = FALSE) {
  key <- .inat_cache_key("language-dictionary", language)

  .inat_cached(key, refresh, function() {
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop(
        "The 'yaml' package is required to load language files. Install ",
        "it with install.packages(\"yaml\").",
        call. = FALSE
      )
    }

    lang_dir <- .inat_lang_dir()
    lang_file <- file.path(lang_dir, paste0(language, ".yml"))

    if (!file.exists(lang_file)) {
      lang_file <- file.path(lang_dir, paste0(language, ".yaml"))
    }

    if (!file.exists(lang_file)) {
      stop(
        "No language file was found for '", language, "' in '", lang_dir, "'.",
        call. = FALSE
      )
    }

    dictionary <- yaml::read_yaml(lang_file)

    if (!is.list(dictionary) || is.null(names(dictionary)) ||
        any(!nzchar(names(dictionary)))) {
      stop(
        "Language file '", lang_file, "' must contain a flat 'key: value' mapping.",
        call. = FALSE
      )
    }

    dictionary
  })
}

.inat_text <- function(key, language = .inat_available_languages()) {
  language <- match.arg(language)

  dictionary <- .inat_load_language(language)

  if (!key %in% names(dictionary)) {
    stop(
      "Text key '", key, "' was not found in the '", language, "' language file.",
      call. = FALSE
    )
  }

  dictionary[[key]]
}

.inat_scalar <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }

  value <- x[[1L]]

  if (is.null(value) || length(value) == 0L) {
    return(default)
  }

  if (is.atomic(value) && length(value) == 1L && is.na(value)) {
    return(default)
  }

  value <- as.character(value)

  if (!nzchar(trimws(value))) {
    return(default)
  }

  value
}

.inat_normalize_species <- function(species, allow_null = FALSE) {
  if (is.null(species) && isTRUE(allow_null)) {
    return(NULL)
  }

  if (!is.character(species) || length(species) != 1L || is.na(species)) {
    stop(
      "'species' must be one scientific name written as 'Genus species'.",
      call. = FALSE
    )
  }

  species <- trimws(gsub("\\s+", " ", species))

  if (length(strsplit(species, " ", fixed = TRUE)[[1L]]) < 2L) {
    stop(
      "'species' must contain at least a genus and a specific epithet.",
      call. = FALSE
    )
  }

  species
}

.inat_normalize_taxon_id <- function(
    taxon_id,
    allow_null = FALSE,
    language = .sacha_inat_defaults$language
) {
  if (is.null(taxon_id) && isTRUE(allow_null)) {
    return(NULL)
  }

  if (is.numeric(taxon_id) && length(taxon_id) == 1L && !is.na(taxon_id) &&
      is.finite(taxon_id) && taxon_id > 0 && taxon_id == floor(taxon_id)) {
    taxon_id <- format(taxon_id, scientific = FALSE, trim = TRUE)
  } else if (is.character(taxon_id) && length(taxon_id) == 1L && !is.na(taxon_id)) {
    taxon_id <- trimws(taxon_id)
  } else {
    stop(.inat_text("taxon_id_invalid", language), call. = FALSE)
  }

  if (!grepl("^[1-9][0-9]*$", taxon_id)) {
    stop(.inat_text("taxon_id_invalid", language), call. = FALSE)
  }

  taxon_id
}

.inat_safe_name <- function(x) {
  x <- trimws(x)
  x <- gsub("[^[:alnum:]_-]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}





.inat_media_budget <- function(bytes = 0) {
  usage <- .sacha_inat_state$media_usage
  current_hour <- format(Sys.time(), "%Y-%m-%d %H", tz = "UTC")
  current_day <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")
  if (!identical(usage$hour, current_hour)) {
    usage$hour <- current_hour
    usage$hour_bytes <- 0
  }
  if (!identical(usage$day, current_day)) {
    usage$day <- current_day
    usage$day_bytes <- 0
  }
  hourly_guard <- .sacha_inat_defaults$hourly_media_limit_gb * 1024^3
  daily_guard <- .sacha_inat_defaults$daily_media_limit_gb * 1024^3
  if (usage$hour_bytes + bytes > hourly_guard ||
      usage$day_bytes + bytes > daily_guard) {
    stop(
      "The session media-download guard has reached its hourly or daily safety threshold.",
      call. = FALSE
    )
  }
  usage$hour_bytes <- usage$hour_bytes + bytes
  usage$day_bytes <- usage$day_bytes + bytes
  .sacha_inat_state$media_usage <- usage
  invisible(usage)
}

.inat_wait_media_request <- function(delay) {
  last_request <- .sacha_inat_state$last_media_request
  if (!is.null(last_request)) {
    elapsed <- as.numeric(difftime(Sys.time(), last_request, units = "secs"))
    if (is.finite(elapsed) && elapsed < delay) {
      Sys.sleep(delay - elapsed)
    }
  }
  .sacha_inat_state$last_media_request <- Sys.time()
  invisible(NULL)
}

.inat_cache_key <- function(...) {
  values <- list(...)

  paste(
    vapply(
      values,
      function(value) paste(as.character(value), collapse = ","),
      character(1)
    ),
    collapse = "\034"
  )
}

.inat_touch_cache_key <- function(key) {
  order <- .sacha_inat_state$cache_order
  order <- order[order != key]
  .sacha_inat_state$cache_order <- c(order, key)
  invisible(NULL)
}

.inat_prune_cache <- function() {
  max_entries <- .sacha_inat_defaults$cache_max_entries %inat||% Inf

  order <- .sacha_inat_state$cache_order

  if (length(order) <= max_entries) {
    return(invisible(NULL))
  }

  excess <- length(order) - max_entries
  oldest_keys <- order[seq_len(excess)]

  existing <- oldest_keys[
    oldest_keys %in% ls(envir = .sacha_inat_state$cache, all.names = TRUE)
  ]

  if (length(existing) > 0L) {
    rm(list = existing, envir = .sacha_inat_state$cache)
  }

  .sacha_inat_state$cache_order <- order[-seq_len(excess)]

  invisible(NULL)
}

.inat_cached <- function(key, refresh = FALSE, loader) {
  if (!isTRUE(refresh) &&
      exists(key, envir = .sacha_inat_state$cache, inherits = FALSE)) {
    .inat_touch_cache_key(key)
    return(get(key, envir = .sacha_inat_state$cache, inherits = FALSE))
  }

  value <- loader()
  assign(key, value, envir = .sacha_inat_state$cache)
  .inat_touch_cache_key(key)
  .inat_prune_cache()

  value
}

.inat_require_packages <- function() {
  required <- c("httr2", "jsonlite")
  unavailable <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

  if (length(unavailable) > 0L) {
    stop(
      "The following package dependencies are required: ",
      paste(unavailable, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.sacha_inat_clear_cache <- function(reset_download_state = FALSE) {
  cache_names <- ls(envir = .sacha_inat_state$cache, all.names = TRUE)

  if (length(cache_names) > 0L) {
    rm(list = cache_names, envir = .sacha_inat_state$cache)
  }

  .sacha_inat_state$cache_order <- character(0)
  if (isTRUE(reset_download_state)) {
    .sacha_inat_state$media_usage <- list(hour = "", day = "", hour_bytes = 0, day_bytes = 0)
    .sacha_inat_state$last_media_request <- NULL
  }

  invisible(TRUE)
}

.sacha_inat_configure <- function(
    inat_api = NULL,
    checklistbank_api = NULL,
    catalogue_dataset = NULL,
    user_agent = NULL,
    language = NULL,
    default_quality_grade = NULL,
    default_image_size = NULL,
    default_save_image_size = NULL,
    cache_max_entries = NULL,
    request_delay = NULL,
    small_download_delay = NULL,
    large_download_delay = NULL,
    hourly_media_limit_gb = NULL,
    daily_media_limit_gb = NULL
) {
  arguments <- list(
    inat_api = inat_api,
    checklistbank_api = checklistbank_api,
    catalogue_dataset = catalogue_dataset,
    user_agent = user_agent,
    language = language,
    default_quality_grade = default_quality_grade,
    default_image_size = default_image_size,
    default_save_image_size = default_save_image_size,
    cache_max_entries = cache_max_entries,
    request_delay = request_delay,
    small_download_delay = small_download_delay,
    large_download_delay = large_download_delay,
    hourly_media_limit_gb = hourly_media_limit_gb,
    daily_media_limit_gb = daily_media_limit_gb
  )
  is_query <- all(vapply(arguments, is.null, logical(1)))
  clear_cache <- FALSE

  for (field in c("inat_api", "checklistbank_api")) {
    value <- arguments[[field]]
    if (is.null(value)) next
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(trimws(value))) {
      stop(sprintf("`%s` must be one non-empty URL.", field), call. = FALSE)
    }
    value <- sub("/+$", "", trimws(value))
    if (!grepl("^https?://", value, ignore.case = TRUE)) {
      stop(sprintf("`%s` must start with http:// or https://.", field), call. = FALSE)
    }
    clear_cache <- clear_cache || !identical(.sacha_inat_defaults[[field]], value)
    .sacha_inat_defaults[[field]] <- value
  }

  if (!is.null(catalogue_dataset)) {
    if (!is.character(catalogue_dataset) || length(catalogue_dataset) != 1L ||
        is.na(catalogue_dataset) || !nzchar(trimws(catalogue_dataset))) {
      stop("`catalogue_dataset` must be one non-empty dataset key.", call. = FALSE)
    }
    catalogue_dataset <- trimws(catalogue_dataset)
    clear_cache <- clear_cache || !identical(
      .sacha_inat_defaults$catalogue_dataset,
      catalogue_dataset
    )
    .sacha_inat_defaults$catalogue_dataset <- catalogue_dataset
  }

  if (!is.null(user_agent)) {
    if (!is.character(user_agent) || length(user_agent) != 1L ||
        is.na(user_agent) || !nzchar(trimws(user_agent))) {
      stop("`user_agent` must be one non-empty string.", call. = FALSE)
    }
    .sacha_inat_defaults$user_agent <- trimws(user_agent)
  }

  if (!is.null(language)) {
    if (!is.character(language) || length(language) != 1L || is.na(language)) {
      stop("`language` must be one language code.", call. = FALSE)
    }
    language <- tolower(trimws(language))
    if (!language %in% .inat_available_languages()) {
      stop(sprintf("No language file is installed for '%s'.", language), call. = FALSE)
    }
    .sacha_inat_defaults$language <- language
  }

  if (!is.null(default_quality_grade)) {
    if (!is.character(default_quality_grade) ||
        length(default_quality_grade) != 1L || is.na(default_quality_grade) ||
        !default_quality_grade %in% c("research", "needs_id", "casual")) {
      stop("`default_quality_grade` must be 'research', 'needs_id', or 'casual'.",
           call. = FALSE)
    }
    .sacha_inat_defaults$default_quality_grade <- default_quality_grade
  }

  image_sizes <- c("medium", "large", "original", "small", "square")
  if (!is.null(default_image_size)) {
    .sacha_inat_defaults$default_image_size <- match.arg(
      default_image_size,
      image_sizes
    )
  }
  if (!is.null(default_save_image_size)) {
    .sacha_inat_defaults$default_save_image_size <- match.arg(
      default_save_image_size,
      c("original", "large", "medium", "small", "square")
    )
  }

  if (!is.null(cache_max_entries)) {
    cache_max_entries <- suppressWarnings(as.numeric(cache_max_entries)[1L])
    if (is.na(cache_max_entries) || cache_max_entries <= 0 ||
        (!is.infinite(cache_max_entries) &&
         cache_max_entries != floor(cache_max_entries))) {
      stop("`cache_max_entries` must be a positive whole number or Inf.", call. = FALSE)
    }
    .sacha_inat_defaults$cache_max_entries <- cache_max_entries
  }

  delays <- list(
    request_delay = list(value = request_delay, minimum = 1),
    small_download_delay = list(value = small_download_delay, minimum = 0.5),
    large_download_delay = list(value = large_download_delay, minimum = 1)
  )
  for (field in names(delays)) {
    value <- delays[[field]]$value
    if (is.null(value)) next
    value <- suppressWarnings(as.numeric(value)[1L])
    if (is.na(value) || !is.finite(value) || value < delays[[field]]$minimum) {
      stop(sprintf("`%s` must be at least %s seconds.", field,
                   delays[[field]]$minimum), call. = FALSE)
    }
    .sacha_inat_defaults[[field]] <- value
  }

  limits <- list(
    hourly_media_limit_gb = list(
      value = hourly_media_limit_gb,
      maximum = 5e9 / 1024^3
    ),
    daily_media_limit_gb = list(
      value = daily_media_limit_gb,
      maximum = 24e9 / 1024^3
    )
  )
  for (field in names(limits)) {
    value <- limits[[field]]$value
    if (is.null(value)) next
    value <- suppressWarnings(as.numeric(value)[1L])
    if (is.na(value) || !is.finite(value) || value <= 0 ||
        value > limits[[field]]$maximum) {
      stop(sprintf("`%s` must be greater than zero and no more than %s GB.",
                   field, limits[[field]]$maximum), call. = FALSE)
    }
    .sacha_inat_defaults[[field]] <- value
  }

  if (!is_query) {
    if (clear_cache) {
      cached <- ls(envir = .sacha_inat_state$cache, all.names = TRUE)
      if (length(cached)) rm(list = cached, envir = .sacha_inat_state$cache)
      .sacha_inat_state$cache_order <- character(0)
    }
    .inat_prune_cache()
    return(invisible(as.list(.sacha_inat_defaults)))
  }

  as.list(.sacha_inat_defaults)
}

.inat_request_json <- function(path, query = list(), fields = NULL) {
  .inat_require_packages()

  path <- sub("^/+", "", path)
  url <- paste0(.sacha_inat_defaults$inat_api, "/", path)

  request <- httr2::request(url)
  request <- httr2::req_user_agent(request, .sacha_inat_defaults$user_agent)
  request <- httr2::req_timeout(request, seconds = 45)
  request <- httr2::req_retry(request, max_tries = 3)

  if (!is.null(fields)) {
    query$fields <- fields
  }

  query <- query[!vapply(query, is.null, logical(1))]

  if (length(query) > 0L) {
    request <- do.call(httr2::req_url_query, c(list(request), query))
  }

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(error) {
      stop(
        "iNaturalist request failed for '", url, "': ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  if (httr2::resp_status(response) >= 300L) {
    stop(
      "iNaturalist returned HTTP status ", httr2::resp_status(response),
      " for '", url, "'.",
      call. = FALSE
    )
  }

  httr2::resp_body_json(response, simplifyVector = FALSE)
}

.inat_taxon_family <- function(taxon_detail) {
  ancestors <- taxon_detail$ancestors %inat||% list()
  nodes <- c(ancestors, list(taxon_detail))

  ranks <- vapply(
    nodes,
    function(node) tolower(.inat_scalar(node$rank, "")),
    character(1)
  )

  family_position <- which(ranks == "family")

  if (length(family_position) == 0L) {
    return(NULL)
  }

  .inat_scalar(nodes[[family_position[[1L]]]]$name, NULL)
}

.inat_taxon_by_id <- function(taxon_id, refresh = FALSE) {
  taxon_id <- .inat_normalize_taxon_id(taxon_id)
  key <- .inat_cache_key("taxon-id", taxon_id)

  .inat_cached(key, refresh, function() {
    response <- .inat_request_json(
      paste0("taxa/", taxon_id),
      fields = .inat_fields_taxon_detail
    )
    detail <- response$results[[1L]]

    if (is.null(detail)) {
      stop("The requested iNaturalist taxon could not be resolved.", call. = FALSE)
    }

    list(
      id = .inat_scalar(detail$id),
      scientific_name = .inat_scalar(detail$name),
      rank = .inat_scalar(detail$rank),
      family = .inat_taxon_family(detail),
      raw = detail
    )
  })
}

.inat_resolve_taxon <- function(species, refresh = FALSE, language = "en") {
  species <- .inat_normalize_species(species)
  key <- .inat_cache_key("taxon-name", tolower(species))

  .inat_cached(key, refresh, function() {
    response <- .inat_request_json(
      "taxa",
      query = list(q = species, per_page = 30L),
      fields = .inat_fields_taxon_search
    )

    candidates <- response$results %inat||% list()

    if (length(candidates) == 0L) {
      return(NULL)
    }

    candidate_names <- vapply(
      candidates,
      function(candidate) tolower(.inat_scalar(candidate$name, "")),
      character(1)
    )

    candidate_ranks <- vapply(
      candidates,
      function(candidate) tolower(.inat_scalar(candidate$rank, "")),
      character(1)
    )
    exact_match <- which(
      candidate_names == tolower(species) & candidate_ranks == "species"
    )

    if (length(exact_match) == 0L) {
      return(NULL)
    }

    candidate_ids <- unique(vapply(
      candidates[exact_match],
      function(candidate) .inat_scalar(candidate$id, ""),
      character(1)
    ))
    candidate_ids <- candidate_ids[nzchar(candidate_ids)]
    if (length(candidate_ids) == 0L) {
      return(NULL)
    }
    if (length(candidate_ids) > 1L) {
      stop(
        .inat_text("taxon_name_ambiguous", language),
        call. = FALSE
      )
    }

    .inat_taxon_by_id(
      candidate_ids[[1L]],
      refresh = refresh
    )
  })
}

.inat_checklistbank_json <- function(path, query = list()) {
  .inat_require_packages()

  path <- sub("^/+", "", path)
  url <- paste0(.sacha_inat_defaults$checklistbank_api, "/", path)

  request <- httr2::request(url)
  request <- httr2::req_user_agent(request, .sacha_inat_defaults$user_agent)
  request <- httr2::req_timeout(request, seconds = 45)
  request <- httr2::req_retry(request, max_tries = 3)

  query <- query[!vapply(query, is.null, logical(1))]

  if (length(query) > 0L) {
    request <- do.call(httr2::req_url_query, c(list(request), query))
  }

  response <- httr2::req_perform(request)

  if (httr2::resp_status(response) >= 300L) {
    stop("ChecklistBank returned an unsuccessful HTTP response.", call. = FALSE)
  }

  httr2::resp_body_json(response, simplifyVector = FALSE)
}

.inat_taxonomic_authorship <- function(
    species,
    catalogue_dataset = .sacha_inat_defaults$catalogue_dataset,
    language = .inat_language_choices(preferred = .sacha_inat_defaults$language),
    refresh = FALSE
) {
  language <- match.arg(language)
  key <- .inat_cache_key("catalogue-of-life", catalogue_dataset, tolower(species))

  .inat_cached(key, refresh, function() {
    tryCatch(
      {
        match <- .inat_checklistbank_json(
          paste0("dataset/", catalogue_dataset, "/match/nameusage"),
          query = list(q = species, verbose = "false")
        )

        usage_id <- .inat_scalar(match$usage$id)

        if (is.null(usage_id)) {
          return("")
        }

        information <- .inat_checklistbank_json(
          paste0("dataset/", catalogue_dataset, "/taxon/", usage_id, "/info")
        )

        scientific_name <- information$usage$name

        if (is.list(scientific_name)) {
          return(.inat_scalar(scientific_name$authorship, ""))
        }

        label <- .inat_scalar(information$usage$label, "")
        trimws(sub(species, "", label, fixed = TRUE))
      },
      error = function(error) {
        warning(
          sprintf(.inat_text("col_failed", language), species),
          call. = FALSE
        )
        ""
      }
    )
  })
}

.inat_license_table <- list(
  "cc0"         = list(label = "CC0",          version = "1.0", translate = FALSE),
  "cc-by"       = list(label = "CC BY",        version = "4.0", translate = FALSE),
  "cc-by-sa"    = list(label = "CC BY-SA",     version = "4.0", translate = FALSE),
  "cc-by-nc"    = list(label = "CC BY-NC",     version = "4.0", translate = FALSE),
  "cc-by-nd"    = list(label = "CC BY-ND",     version = "4.0", translate = FALSE),
  "cc-by-nc-sa" = list(label = "CC BY-NC-SA",  version = "4.0", translate = FALSE),
  "cc-by-nc-nd" = list(label = "CC BY-NC-ND",  version = "4.0", translate = FALSE),
  "gfdl"        = list(label = "GFDL",         version = "1.3", translate = FALSE),
  "pd"          = list(
    label = "public_domain_label", version = NULL, translate = TRUE
  )
)

.inat_license_label <- function(
    license_code,
    language = "en",
    use_placeholders = TRUE
) {
  code <- tolower(trimws(.inat_scalar(license_code, "")))

  if (!nzchar(code)) {
    if (!isTRUE(use_placeholders)) {
      return(NA_character_)
    }
    return(.inat_text("all_rights_reserved", language))
  }

  version_match <- regmatches(
    code,
    regexec("^(.*)-([0-9]+(?:\\.[0-9]+)?)$", code)
  )[[1L]]

  if (length(version_match) == 3L) {
    base_code <- version_match[[2L]]
    detected_version <- version_match[[3L]]
  } else {
    base_code <- code
    detected_version <- NULL
  }

  entry <- .inat_license_table[[base_code]]

  if (is.null(entry)) {
    entry <- .inat_license_table[[code]]
  }

  if (is.null(entry)) {
    if (!isTRUE(use_placeholders)) {
      return(NA_character_)
    }

    warning(
      sprintf(.inat_text("license_unmapped", language), code),
      call. = FALSE
    )

    return(trimws(toupper(gsub("-", " ", code, fixed = TRUE))))
  }

  label <- if (isTRUE(entry$translate)) {
    .inat_text(entry$label, language)
  } else {
    entry$label
  }

  version <- detected_version %inat||% entry$version

  if (is.null(version) || !nzchar(version)) {
    return(label)
  }

  paste(label, version)
}

.inat_photo_records <- function(observation) {
  photos <- observation$photos %inat||% list()
  accepted <- c(
    "cc0", "cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa",
    "cc-by-nd", "cc-by-nc-nd"
  )
  photos <- Filter(
    function(photo) tolower(.inat_scalar(photo$license_code, "")) %in% accepted,
    photos
  )
  if (length(photos) == 0L) {
    return(list())
  }
  lapply(seq_along(photos), function(index) {
    photo <- photos[[index]]
    photo_user <- photo$user %inat||% list()
    list(
      position = index,
      photo_id = .inat_scalar(photo$id, as.character(index)),
      url_square = .inat_scalar(photo$url),
      license_code = .inat_scalar(photo$license_code),
      observer = .inat_scalar(photo_user$name, .inat_scalar(photo_user$login))
    )
  })
}

.inat_observation_record <- function(
    observation,
    taxon,
    selection_index,
    language = "en",
    use_placeholders = TRUE
) {
  user <- observation$user %inat||% list()
  photos <- .inat_photo_records(observation)

  license_code <- if (length(photos) > 0L) photos[[1L]]$license_code else NULL

  list(
    observation_id = .inat_scalar(observation$id),
    observation_url = paste0(
      "https://www.inaturalist.org/observations/",
      .inat_scalar(observation$id)
    ),
    scientific_name = .inat_scalar(observation$taxon$name, taxon$scientific_name),
    taxon_id = .inat_scalar(observation$taxon$id, taxon$id),
    family = taxon$family,
    observer = .inat_scalar(
      user$name,
      .inat_scalar(user$login, "persona observadora no identificada")
    ),
    place = .inat_scalar(observation$place_guess, ""),
    observed_on = .inat_scalar(observation$observed_on),
    license_code = license_code,
    license_label = .inat_license_label(
      license_code,
      language = language,
      use_placeholders = use_placeholders
    ),
    photos = photos,
    taxonomic_author = "",
    selection_index = selection_index
  )
}

.inat_observation_by_id <- function(observation_id, refresh = FALSE) {
  key <- .inat_cache_key("observation-id", observation_id)

  .inat_cached(key, refresh, function() {
    response <- .inat_request_json(
      paste0("observations/", observation_id),
      fields = .inat_fields_observation
    )
    observation <- response$results[[1L]]

    if (is.null(observation)) {
      stop(
        "No iNaturalist observation was found for ID ", observation_id, ".",
        call. = FALSE
      )
    }

    observation
  })
}

.inat_web_query <- function(
    species,
    user_id,
    number_observations,
    observation_id = NULL,
    quality_grade,
    place_id,
    language = "en",
    use_placeholders = TRUE,
    refresh = FALSE,
    taxon_id = NULL
) {
  species <- .inat_normalize_species(species, allow_null = TRUE)
  taxon_id <- .inat_normalize_taxon_id(
    taxon_id, allow_null = TRUE, language = language
  )

  if (!is.null(species) && !is.null(taxon_id)) {
    stop(.inat_text("taxon_selector_conflict", language), call. = FALSE)
  }

  if (is.null(species) && is.null(taxon_id) && is.null(observation_id)) {
    stop(
      .inat_text("taxon_selector_required", language),
      call. = FALSE
    )
  }

  if (length(number_observations) != 1L ||
      is.na(number_observations) ||
      !is.finite(number_observations) ||
      number_observations < 1L ||
      number_observations != as.integer(number_observations)) {
    stop("'number_observations' must be a positive integer.", call. = FALSE)
  }

  number_observations <- as.integer(number_observations)
  requested_taxon <- if (!is.null(taxon_id)) {
    .inat_taxon_by_id(taxon_id, refresh = refresh)
  } else if (!is.null(species)) {
    .inat_resolve_taxon(species, refresh = refresh, language = language)
  } else {
    NULL
  }
  if (!is.null(taxon_id) &&
      !identical(tolower(.inat_scalar(requested_taxon$rank, "")), "species")) {
    stop(.inat_text("taxon_id_rank_invalid", language), call. = FALSE)
  }

  if (!is.null(observation_id)) {
    observation_ids <- unique(as.character(observation_id))

    observations <- lapply(
      observation_ids,
      .inat_observation_by_id,
      refresh = refresh
    )

    if (!is.null(requested_taxon)) {
      observation_taxon_id <- as.character(requested_taxon$id)
      observed_taxon_ids <- vapply(
        observations,
        function(observation) .inat_scalar(observation$taxon$id, ""),
        character(1)
      )

      mismatched <- observation_ids[
        observed_taxon_ids != as.character(observation_taxon_id)
      ]

      if (length(mismatched) > 0L) {
        stop(
          sprintf(.inat_text("observation_taxon_mismatch", language),
            observation_ids[[1L]], paste(mismatched, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    } else if (!is.null(species)) {
      observed_names <- vapply(
        observations,
        function(observation) .inat_scalar(observation$taxon$name, ""),
        character(1)
      )

      invalid <- tolower(observed_names) != tolower(species)

      if (any(invalid)) {
        stop(
          "At least one 'observation_id' does not correspond exactly to the requested species '",
          species, "'.",
          call. = FALSE
        )
      }

      observation_taxon_id <- .inat_scalar(observations[[1L]]$taxon$id)
      if (is.null(observation_taxon_id)) {
        stop(
          "The selected observation has no resolvable iNaturalist taxon.",
          call. = FALSE
        )
      }
      requested_taxon <- .inat_taxon_by_id(
        observation_taxon_id,
        refresh = refresh
      )
      if (!identical(tolower(.inat_scalar(requested_taxon$rank, "")), "species")) {
        stop(.inat_text("species_taxon_rank_invalid", language), call. = FALSE)
      }
    } else {
      observation_taxon_id <- .inat_scalar(observations[[1L]]$taxon$id)
      if (is.null(observation_taxon_id)) {
        stop(
          "The selected observation has no resolvable iNaturalist taxon.",
          call. = FALSE
        )
      }
      requested_taxon <- .inat_taxon_by_id(
        observation_taxon_id,
        refresh = refresh
      )
      observed_taxon_ids <- vapply(
        observations,
        function(observation) .inat_scalar(observation$taxon$id, ""),
        character(1)
      )
      if (any(observed_taxon_ids != as.character(observation_taxon_id))) {
        stop(
          .inat_text("observation_ids_taxon_mismatch", language),
          call. = FALSE
        )
      }
    }
  } else {
    if (is.null(requested_taxon)) {
      return(list(
        source = "web",
        taxon = NULL,
        requested_species = species,
        records = list()
      ))
    }

    fetch_per_page <- 200L

    key <- .inat_cache_key(
      "observations",
      requested_taxon$id,
      user_id,
      number_observations,
      fetch_per_page,
      quality_grade,
      place_id
    )

    observations <- .inat_cached(key, refresh, function() {
      fetched <- list()
      cursor <- 0L
      repeat {
        batch_target <- min(number_observations - length(fetched), 10000L)
        pages <- ceiling(batch_target / 200L)
        batch_cursor <- cursor
        batch_max_id <- cursor
        if (pages < 1L) {
          break
        }
        for (page_index in seq_len(pages)) {
          query <- list(
            taxon_id = requested_taxon$id,
            user_id = user_id,
            quality_grade = quality_grade,
            photos = "true",
            photo_license = "cc0,cc-by,cc-by-sa,cc-by-nc,cc-by-nd,cc-by-nc-sa,cc-by-nc-nd",
            per_page = 200L,
            place_id = place_id
          )
          if (number_observations <= 10000L) {
            query$order_by <- "observed_on"
            query$order <- "desc"
          } else {
            query$order_by <- "id"
            query$order <- "asc"
          }
          query$page <- page_index
          if (batch_cursor > 0L) {
            query$id_above <- batch_cursor
          }
          if (length(fetched) > 0L) {
            Sys.sleep(.sacha_inat_defaults$request_delay)
          }
          response <- .inat_request_json(
            "observations",
            query = query,
            fields = .inat_fields_observation
          )
          batch <- response$results %inat||% list()
          if (length(batch) == 0L) {
            break
          }
          fetched <- c(fetched, batch)
          ids <- vapply(
            batch,
            function(item) suppressWarnings(as.integer(.inat_scalar(item$id, "0"))),
            integer(1)
          )
          batch_max_id <- max(c(batch_max_id, ids), na.rm = TRUE)
          if (length(batch) < 200L) {
            break
          }
          if (number_observations > 10000L &&
              length(fetched) >= 10000L &&
              page_index == pages) {
            break
          }
        }
        cursor <- batch_max_id
        if (length(fetched) >= number_observations ||
            number_observations <= 10000L ||
            length(batch) == 0L) {
          break
        }
      }
      fetched
    })

    observations <- Filter(function(observation) {
      identical(
        .inat_scalar(observation$taxon$id, ""),
        as.character(requested_taxon$id)
      )
    }, observations)

  }

  if (length(observations) > 1L) {
    observed_dates <- vapply(
      observations,
      function(observation) .inat_scalar(observation$observed_on, ""),
      character(1)
    )
    parsed_dates <- suppressWarnings(as.Date(observed_dates))
    observations <- observations[order(parsed_dates, decreasing = TRUE, na.last = TRUE)]
  }
  if (is.null(observation_id) && length(observations) > number_observations) {
    observations <- observations[seq_len(number_observations)]
  }

  requested_taxon$family <- requested_taxon$family %inat||%
    .inat_text("unknown_family", language)

  records <- lapply(
    seq_along(observations),
    function(index) {
      .inat_observation_record(
        observation = observations[[index]],
        taxon = requested_taxon,
        selection_index = index,
        language = language,
        use_placeholders = use_placeholders
      )
    }
  )

  records <- Filter(function(record) length(record$photos) > 0L, records)

  list(
    source = "web",
    taxon = requested_taxon,
    requested_species = requested_taxon$scientific_name,
    records = records
  )
}

.inat_attach_authorship <- function(
    query,
    catalogue_dataset,
    language,
    refresh = FALSE
) {
  if (length(query$records) == 0L) {
    return(query)
  }

  query$records <- lapply(query$records, function(record) {
    record$taxonomic_author <- .inat_taxonomic_authorship(
      species = record$scientific_name,
      catalogue_dataset = catalogue_dataset,
      language = language,
      refresh = refresh
    )

    record
  })

  query
}

.inat_taxon_directory <- function(directory, family, species) {
  .sacha_taxon_export_directory(
    directory,
    family,
    species,
    subdirectory = "inat"
  )
}

.inat_image_url <- function(photo, image_size) {
  image_size <- match.arg(
    image_size,
    c("square", "small", "medium", "large", "original")
  )

  square_url <- .inat_scalar(photo$url_square)

  if (is.null(square_url)) {
    return(NULL)
  }

  sub("square", image_size, square_url, fixed = TRUE)
}

.inat_file_extension <- function(url) {
  extension <- tools::file_ext(sub("\\?.*$", "", url))

  if (!nzchar(extension)) {
    extension <- "jpg"
  }

  tolower(extension)
}

.inat_save_record_images <- function(
    record,
    directory,
    image_size,
    include_auxiliary,
    overwrite,
    requested_species,
    query_parameters,
    language
) {
  directory <- .inat_taxon_directory(
    directory = directory,
    family = record$family,
    species = record$scientific_name
  )

  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  }

  photos <- record$photos

  if (length(photos) == 0L) {
    warning(
      sprintf(.inat_text("no_photos", language), record$observation_id),
      call. = FALSE
    )

    return(list(
      observation_id = record$observation_id,
      directory = directory,
      metadata_file = NA_character_,
      saved_images = 0L
    ))
  }

  if (!isTRUE(include_auxiliary)) {
    photos <- photos[[1L]]
    photos <- list(photos)
  }

  image_metadata <- lapply(photos, function(photo) {
    image_url <- .inat_image_url(photo, image_size)

    if (is.null(image_url)) {
      return(NULL)
    }

    file_name <- paste0(
      record$observation_id,
      "-",
      photo$position,
      "-",
      image_size,
      ".",
      .inat_file_extension(image_url)
    )

    destination <- file.path(directory, file_name)

    if (isTRUE(overwrite) || !file.exists(destination)) {
      .inat_media_budget()
      download_delay <- if (length(photos) > 10L ||
                           suppressWarnings(as.integer(.inat_scalar(
                             query_parameters$number_observations,
                             "1"
                           ))) > 10L) {
        .sacha_inat_defaults$large_download_delay
      } else {
        .sacha_inat_defaults$small_download_delay
      }
      .inat_wait_media_request(download_delay)
      completed <- tryCatch(
        {
          utils::download.file(
            url = image_url,
            destfile = destination,
            mode = "wb",
            quiet = TRUE
          )
          file.exists(destination)
        },
        error = function(error) FALSE
      )
      if (isTRUE(completed)) {
        downloaded_bytes <- suppressWarnings(as.numeric(file.info(destination)$size))
        if (is.finite(downloaded_bytes) && downloaded_bytes > 0) {
          .inat_media_budget(downloaded_bytes)
        }
      }

      if (!isTRUE(completed)) {
        warning(
          sprintf(.inat_text("download_failed", language), record$observation_id),
          call. = FALSE
        )
      }
    }

    list(
      position = photo$position,
      photo_id = photo$photo_id,
      license_code = photo$license_code,
      file = file_name,
      image_size = image_size,
      source_url = image_url,
      is_primary = identical(photo$position, 1L),
      available = file.exists(destination)
    )
  })

  image_metadata <- Filter(Negate(is.null), image_metadata)

  record_to_store <- record
  record_to_store$image_location <- NULL
  record_to_store$image_source <- NULL
  record_to_store$local_files <- NULL

  metadata <- list(
    schema_version = "1.0",
    saved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    requested_species = requested_species,
    query_parameters = query_parameters,
    record = record_to_store,
    images = image_metadata
  )

  metadata_file <- file.path(
    directory,
    paste0(record$observation_id, "-metadata.json")
  )

  jsonlite::write_json(
    metadata,
    path = metadata_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  list(
    observation_id = record$observation_id,
    directory = directory,
    metadata_file = metadata_file,
    saved_images = sum(
      vapply(image_metadata, function(image) isTRUE(image$available), logical(1))
    )
  )
}

.inat_local_query <- function(
    species,
    number_observations,
    observation_id = NULL,
    directory = getwd(),
    user_id = NULL,
    quality_grade = NULL,
    place_id = NULL,
    taxon_id = NULL,
    language = "en"
) {
  species <- .inat_normalize_species(species, allow_null = TRUE)
  taxon_id <- .inat_normalize_taxon_id(
    taxon_id, allow_null = TRUE, language = language
  )

  images_root <- .sacha_exports_root(directory)
  legacy_root <- if (identical(directory, ".") ||
                     identical(directory, getwd())) {
    file.path(directory, "images")
  } else {
    directory
  }
  search_roots <- unique(c(images_root, legacy_root))
  search_roots <- search_roots[dir.exists(search_roots)]

  if (length(search_roots) == 0L) {
    return(NULL)
  }

  metadata_files <- unlist(lapply(search_roots, function(path) {
    list.files(
      path = path,
      pattern = "-metadata\\.json$",
      recursive = TRUE,
      full.names = TRUE
    )
  }), use.names = FALSE)

  if (length(metadata_files) == 0L) {
    return(NULL)
  }

  requested_ids <- if (is.null(observation_id)) {
    NULL
  } else {
    unique(as.character(observation_id))
  }

  records <- list()

  for (metadata_file in metadata_files) {
    metadata <- tryCatch(
      jsonlite::read_json(metadata_file, simplifyVector = FALSE),
      error = function(error) NULL
    )

    if (is.null(metadata) || is.null(metadata$record)) {
      next
    }

    record <- metadata$record
    record_species <- .inat_scalar(record$scientific_name)
    saved_query <- metadata$query_parameters %inat||% list()
    if (!is.null(user_id) &&
        !identical(.inat_scalar(saved_query$user_id, ""), as.character(user_id))) {
      next
    }
    if (!is.null(quality_grade) &&
        !identical(.inat_scalar(saved_query$quality_grade, ""), as.character(quality_grade))) {
      next
    }
    if (!is.null(place_id) &&
        !identical(.inat_scalar(saved_query$place_id, ""), as.character(place_id))) {
      next
    }

    if (!is.null(species) &&
        !identical(tolower(record_species), tolower(species))) {
      next
    }

    if (!is.null(taxon_id) && !identical(
      .inat_scalar(record$taxon_id, ""), taxon_id
    )) {
      next
    }

    if (!is.null(requested_ids) &&
        !(.inat_scalar(record$observation_id) %in% requested_ids)) {
      next
    }

    image_entries <- metadata$images %inat||% list()
    allowed_licenses <- c(
      "cc0", "cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa",
      "cc-by-nd", "cc-by-nc-nd"
    )
    image_entries <- Filter(function(entry) {
      license <- .inat_scalar(entry$license_code)
      if (is.null(license)) {
        match_photo <- which(vapply(
          record$photos %inat||% list(),
          function(photo) identical(
            .inat_scalar(photo$photo_id, ""),
            .inat_scalar(entry$photo_id, "")
          ),
          logical(1)
        ))
        if (length(match_photo) > 0L) {
          license <- .inat_scalar(record$photos[[match_photo[[1L]]]]$license_code)
        }
      }
      tolower(.inat_scalar(license, "")) %in% allowed_licenses
    }, image_entries)

    if (length(image_entries) == 0L) {
      next
    }

    primary <- vapply(
      image_entries,
      function(image) isTRUE(image$is_primary),
      logical(1)
    )

    ordered_entries <- c(
      image_entries[which(primary)],
      image_entries[which(!primary)]
    )

    available_entries <- list()

    for (entry in ordered_entries) {
      file_name <- .inat_scalar(entry$file)

      if (is.null(file_name)) {
        next
      }

      image_path <- file.path(dirname(metadata_file), file_name)

      if (file.exists(image_path)) {
        entry$path <- image_path
        available_entries[[length(available_entries) + 1L]] <- entry
      }
    }

    if (length(available_entries) == 0L) {
      next
    }

    record$image_location <- available_entries[[1L]]$path
    record$image_source <- "local"
    record$local_files <- vapply(
      available_entries,
      function(entry) entry$path,
      character(1)
    )

    records[[length(records) + 1L]] <- record
  }

  if (length(records) == 0L) {
    return(NULL)
  }

  observation_ids <- vapply(
    records,
    function(record) .inat_scalar(record$observation_id, ""),
    character(1)
  )

  records <- records[!duplicated(observation_ids)]

  observed_dates <- vapply(
    records,
    function(record) .inat_scalar(record$observed_on, ""),
    character(1)
  )
  parsed_dates <- suppressWarnings(as.Date(observed_dates))
  records <- records[order(parsed_dates, decreasing = TRUE, na.last = TRUE)]
  records <- records[seq_len(min(length(records), number_observations))]

  list(
    source = "local",
    taxon = list(
      id = .inat_scalar(records[[1L]]$taxon_id),
      scientific_name = .inat_scalar(records[[1L]]$scientific_name),
      family = .inat_scalar(records[[1L]]$family)
    ),
    requested_species = .inat_scalar(records[[1L]]$scientific_name),
    records = records
  )
}

.inat_local_query_is_sufficient <- function(
    local_query,
    number_observations,
    observation_id = NULL
) {
  if (is.null(local_query) || length(local_query$records) == 0L) {
    return(FALSE)
  }

  required <- if (is.null(observation_id)) {
    number_observations
  } else {
    length(unique(as.character(observation_id)))
  }

  length(local_query$records) >= required
}

.inat_retrieve <- function(
    species,
    user_id,
    number_observations,
    observation_id,
    quality_grade,
    place_id,
    source,
    verify_local,
    directory,
    catalogue_dataset,
    language,
    refresh,
    use_placeholders = TRUE,
    taxon_id = NULL
) {
  source <- match.arg(source, c("web", "local"))
  species <- .inat_normalize_species(species, allow_null = TRUE)
  taxon_id <- .inat_normalize_taxon_id(
    taxon_id, allow_null = TRUE, language = language
  )
  if (!is.null(species) && !is.null(taxon_id)) {
    stop(.inat_text("taxon_selector_conflict", language), call. = FALSE)
  }

  local_query <- NULL

  if (identical(source, "local") || isTRUE(verify_local)) {
    local_query <- .inat_local_query(
      species = species,
      number_observations = number_observations,
      observation_id = observation_id,
      directory = directory,
      user_id = user_id,
      quality_grade = quality_grade,
      place_id = place_id,
      taxon_id = taxon_id,
      language = language
    )

    if (.inat_local_query_is_sufficient(
      local_query,
      number_observations = number_observations,
      observation_id = observation_id
    )) {
      return(local_query)
    }

    if (identical(source, "local")) {
      stop(.inat_text("no_local_images", language), call. = FALSE)
    }
  }

  query <- .inat_web_query(
    species = species,
    user_id = user_id,
    number_observations = number_observations,
    observation_id = observation_id,
    quality_grade = quality_grade,
    place_id = place_id,
    language = language,
    use_placeholders = use_placeholders,
    refresh = refresh,
    taxon_id = taxon_id
  )

  .inat_attach_authorship(
    query = query,
    catalogue_dataset = catalogue_dataset,
    language = language,
    refresh = refresh
  )
}

.inat_primary_image_location <- function(record, image_size) {
  if (identical(record$image_source, "local") &&
      !is.null(record$image_location) &&
      file.exists(record$image_location)) {
    return(gsub("\\\\", "/", record$image_location))
  }

  if (length(record$photos) == 0L) {
    return(NULL)
  }

  .inat_image_url(record$photos[[1L]], image_size)
}







#' Save iNaturalist images and reproducible local metadata
#'
#' Downloads the primary photograph and, when requested, all auxiliary photographs
#' associated with selected iNaturalist observations. Each observation receives
#' a JSON metadata file containing the standardised taxonomic, licensing,
#' locality, observer, and source-URL information required to reproduce the
#' corresponding scientific figure without querying the web again.
#' The iNaturalist API is intended for small or medium requests; use observation
#' exports for bulk extraction. See
#' <https://www.inaturalist.org/pages/api+recommended+practices>.
#'
#' Images and their metadata are organised under
#' `sacha_exports/Family_Genus_species/inat/` within `directory`,
#' preserving the package's taxonomic directory convention while separating
#' iNaturalist files from exports produced by other package modules.
#'
#' @inheritParams inat_images
#' @param observation_id Optional iNaturalist observation identifier or vector
#'   of identifiers. When supplied with `species` or `taxon_id`, the taxonomic
#'   identity is verified; when supplied alone, all identifiers are
#'   cross-checked against a single common taxon.
#' @param include_auxiliary Logical; save all photographs of each observation
#'   when `TRUE`, or only the primary photograph when `FALSE` (default).
#' @param overwrite Logical; overwrite existing local image files.
#' @details
#' When omitted, `image_size`, `quality_grade`, `catalogue_dataset`, and
#' `language` use the active iNaturalist settings from
#' `sacha_configure(inat = ...)`. For this function, `image_size` follows
#' `default_save_image_size` (initially `"medium"`). An explicit function
#' argument takes precedence over `sacha_arg_configure()`; a shared argument
#' default takes precedence over the module setting.
#' @usage
#' inat_save_images(
#'   species = NULL,
#'   user_id = NULL,
#'   number_observations = 1L,
#'   observation_id = NULL,
#'   image_size = NULL,
#'   include_auxiliary = FALSE,
#'   directory = getwd(),
#'   quality_grade = NULL,
#'   place_id = NULL,
#'   catalogue_dataset = NULL,
#'   language = NULL,
#'   overwrite = FALSE,
#'   refresh = FALSE,
#'   taxon_id = NULL
#' )
#'
#' @return A data frame documenting saved observations, folders, metadata files,
#'   and the number of successfully available images.
#' @export
#'
#' @examples
#' \dontrun{
#' inat_save_images("Bidens andicola", user_id = "marceloamores")
#' inat_images("Bidens andicola", user_id = "marceloamores",
#'             source = "local")
#' }
inat_save_images <- function(
    species = NULL,
    user_id = NULL,
    number_observations = 1L,
    observation_id = NULL,
    image_size = NULL,
    include_auxiliary = FALSE,
    directory = getwd(),
    quality_grade = NULL,
    place_id = NULL,
    catalogue_dataset = NULL,
    language = NULL,
    overwrite = FALSE,
    refresh = FALSE,
    taxon_id = NULL
) {
  missing_image_size <- missing(image_size)
  missing_quality_grade <- missing(quality_grade)
  missing_catalogue_dataset <- missing(catalogue_dataset)
  missing_language <- missing(language)
  call <- match.call(expand.dots = FALSE)
  defaults <- .inat_argument_defaults(save_image_size = TRUE)

  if (missing_image_size) image_size <- defaults$image_size
  if (missing_quality_grade) quality_grade <- defaults$quality_grade
  if (missing_catalogue_dataset) catalogue_dataset <- defaults$catalogue_dataset
  if (missing_language) language <- defaults$language

  .sacha_apply_arg_defaults(environment(), call, names(formals(sys.function())))
  language <- match.arg(language, choices = defaults$language_choices)
  image_size <- match.arg(image_size, choices = defaults$image_size_choices)

  query <- .inat_retrieve(
    species = species,
    user_id = user_id,
    number_observations = number_observations,
    observation_id = observation_id,
    quality_grade = quality_grade,
    place_id = place_id,
    source = "web",
    verify_local = FALSE,
    directory = directory,
    catalogue_dataset = catalogue_dataset,
    language = language,
    refresh = refresh,
    use_placeholders = TRUE,
    taxon_id = taxon_id
  )

  if (length(query$records) == 0L) {
    message(.inat_text("no_observations", language))

    return(data.frame(
      observation_id = character(),
      directory = character(),
      metadata_file = character(),
      saved_images = integer(),
      stringsAsFactors = FALSE
    ))
  }

  query_parameters <- list(
    user_id = user_id,
    number_observations = number_observations,
    observation_id = observation_id,
    taxon_id = query$taxon$id,
    quality_grade = quality_grade,
    place_id = place_id,
    catalogue_dataset = catalogue_dataset
  )

  saved <- lapply(query$records, function(record) {
    .inat_save_record_images(
      record = record,
      directory = directory,
      image_size = image_size,
      include_auxiliary = include_auxiliary,
      overwrite = overwrite,
      requested_species = query$requested_species,
      query_parameters = query_parameters,
      language = language
    )
  })

  data.frame(
    observation_id = vapply(saved, `[[`, character(1), "observation_id"),
    directory = vapply(saved, `[[`, character(1), "directory"),
    metadata_file = vapply(saved, `[[`, character(1), "metadata_file"),
    saved_images = vapply(saved, `[[`, integer(1), "saved_images"),
    stringsAsFactors = FALSE
  )
}

.inat_build_sacha_images <- function(
    species,
    user_id,
    number_observations,
    observation_id,
    image_size,
    source,
    verify_local,
    directory,
    quality_grade,
    place_id,
    catalogue_dataset,
    language,
    observer_name,
    save_images = FALSE,
    include_auxiliary = FALSE,
    overwrite = FALSE,
    refresh,
    use_placeholders,
    taxon_id = NULL
) {
  if (length(number_observations) != 1L ||
      is.na(number_observations) ||
      !is.finite(number_observations) ||
      number_observations < 1L ||
      number_observations != as.integer(number_observations)) {
    stop("'number_observations' must be a positive integer.", call. = FALSE)
  }
  query <- .inat_retrieve(
    species = species,
    user_id = user_id,
    number_observations = number_observations,
    observation_id = observation_id,
    quality_grade = quality_grade,
    place_id = place_id,
    source = source,
    verify_local = verify_local,
    directory = directory,
    catalogue_dataset = catalogue_dataset,
    language = language,
    refresh = refresh,
    use_placeholders = use_placeholders,
    taxon_id = taxon_id
  )

  saved_files <- NULL
  if (isTRUE(save_images)) {
    saved_files <- lapply(query$records, function(record) {
      .inat_save_record_images(
        record = record,
        directory = directory,
        image_size = image_size,
        include_auxiliary = include_auxiliary,
        overwrite = overwrite,
        requested_species = query$requested_species,
        query_parameters = list(
          user_id = user_id,
          taxon_id = query$taxon$id,
          number_observations = number_observations,
          observation_id = observation_id,
          quality_grade = quality_grade,
          place_id = place_id
        ),
        language = language
      )
    })
    for (index in seq_along(query$records)) {
      record <- query$records[[index]]
      taxon_directory <- .inat_taxon_directory(
        directory, record$family, record$scientific_name
      )
      matches <- list.files(
        taxon_directory,
        pattern = paste0("^", record$observation_id, "-1-", image_size, "\\."),
        full.names = TRUE
      )
      if (length(matches) > 0L) {
        query$records[[index]]$image_location <- matches[[1L]]
        query$records[[index]]$image_source <- "local"
      }
    }
  }

  records <- lapply(query$records, function(record) {
    record$image_location <- if (identical(record$image_source, "local")) {
      gsub("\\\\", "/", record$image_location)
    } else {
      .inat_primary_image_location(record, image_size)
    }
    record$caption <- .inat_subcaption(
      record,
      observer_name = observer_name,
      language = language,
      use_placeholders = use_placeholders
    )
    record
  })

  structure(
    list(
      source = query$source,
      taxon = query$taxon,
      records = records,
      saved_files = saved_files,
      language = language,
      image_size = image_size
    ),
    class = "sacha_inat_images"
  )
}

#' Retrieve iNaturalist photographs
#'
#' Finds observations by scientific name, iNaturalist taxon ID, or observation
#' identifier and returns their licensed photographs with taxonomic, observer, locality,
#' source, and caption metadata. Results are ordered by observed date, newest
#' first; records without an observed date follow dated records. The default
#' search uses Research Grade
#' observations that contain photographs and retains photos with Creative
#' Commons licenses that permit attribution. Captions cite the photo license
#' and link to the observation page.
#'
#' Photo and observation licenses are separate iNaturalist fields. This
#' function filters and reports the license attached to each photo; it does
#' not infer a photo's reuse terms from the observation license. The default
#' accepted photo licenses are CC0, CC BY, CC BY-SA, CC BY-NC, CC BY-NC-SA,
#' CC BY-ND, and CC BY-NC-ND. Follow all license conditions, including
#' attribution, non-commercial use, share-alike, and no-derivatives terms.
#'
#' Searches use the iNaturalist API v2 and request observations with photos.
#' Results are paged in batches of at most 200. For requests exceeding 10,000
#' observations, ascending ID order and the id_above cursor avoid the API
#' page limit. Requests in extended searches are separated by at least 1.02
#' seconds. iNaturalist recommends about one request per second and warns
#' that media downloads above 5 GB per hour or 24 GB per day may result in a
#' permanent block. The default session thresholds are 4.5 GB per hour and
#' 21.5 GB per day; they can be adjusted within the API's stated limits with
#' [sacha_configure()]. Accounting is per R session and cannot include
#' downloads from other processes or machines using the same network. The API
#' is intended for small or medium requests; use iNaturalist observation
#' exports for bulk extraction. See
#' <https://www.inaturalist.org/pages/api+recommended+practices> and
#' <https://api.inaturalist.org/v2/docs/>.
#'
#' @param species Optional scientific name, normally written as "Genus species".
#'   It is matched exactly (case-insensitive) to a species-rank taxon. Supply
#'   either `species` or `taxon_id`; `observation_id` may be used alone or with
#'   either selector.
#' @param taxon_id Optional positive iNaturalist taxon ID (numeric or numeric
#'   text) at species rank. It queries that taxon directly without resolving a
#'   name. Observations are checked against the exact taxon ID. Local searches
#'   use the saved taxon ID and retain the existing directory structure.
#' @param user_id Optional iNaturalist login used to restrict a species search.
#'   NULL includes observations from all users.
#' @param number_observations Positive integer giving the maximum number of
#'   observations to retrieve; defaults to 1.
#' @param observation_id Optional numeric observation identifier or vector.
#'   When supplied with `species` or `taxon_id`, each observation is checked
#'   against that taxon. When supplied alone, all IDs must refer to one taxon.
#' @param image_size Image variant: `"medium"`, `"large"`, `"original"`,
#'   `"small"`, or `"square"`. When omitted, the function uses the configured
#'   image size; for `inat_images()` and `inat_images_md()` this is
#'   `default_image_size`, initially `"medium"`. `inat_save_images()` uses
#'   `default_save_image_size`, also initially `"medium"`.
#' @param source "web" queries the API; "local" uses only images and metadata
#'   previously saved under
#'   `directory/sacha_exports/Family_Genus_species/inat`.
#' @param verify_local If TRUE, use a complete local archive first and query
#'   the API only when the archive cannot satisfy the request.
#' @param directory Base directory under which the package creates or
#'   searches `sacha_exports/Family_Genus_species/inat`. Defaults to the
#'   working directory. Existing archives in the former `images` directory
#'   and earlier taxon-directory layout remain readable when using the default
#'   path.
#' @param quality_grade iNaturalist observation quality filter. When omitted,
#'   the configured `default_quality_grade` is used (initially `"research"`).
#'   Valid API values are `"research"`, `"needs_id"`, and `"casual"`.
#' @param place_id Optional numeric iNaturalist place identifier.
#' @param catalogue_dataset ChecklistBank dataset identifier used to obtain
#'   taxonomic authorship. When omitted, the configured dataset is used
#'   (initially `"3LXR"`).
#' @param language Caption language. When omitted, the configured iNaturalist
#'   language is used (initially English). Spanish and other installed
#'   `inst/lang` language files are also accepted.
#' @param observer_name Optional replacement for the iNaturalist observer name
#'   in captions.
#' @param save_images If TRUE, download and save the selected photographs and
#'   JSON metadata to `sacha_exports/Family_Genus_species/inat`, using the same
#'   layout as [inat_save_images()]. Saved images are immediately available to
#'   `source = "local"` and `verify_local = TRUE`.
#' @param include_auxiliary If TRUE, save all photographs associated with each
#'   selected observation when `save_images = TRUE`; otherwise, save only the
#'   primary photograph (default).
#' @param export_comp If TRUE, save only the PNG composition to
#'   `sacha_exports/Family_Genus_species/inat`. This does not save source
#'   photographs or their metadata.
#'   The optional magick package is also needed for the composition preview.
#' @param refresh If TRUE, bypass cached API results.
#' @param ncol Number of columns in a Markdown panel and exported composition;
#'   default 2.
#' @param word_subfigure_width Width of DOCX subfigures; default "3in".
#' @param latex_subfigure_width Width of PDF/LaTeX subfigures; default "86%".
#' @details
#' When omitted, `image_size`, `quality_grade`, `catalogue_dataset`, and
#' `language` use the active iNaturalist settings from
#' `sacha_configure(inat = ...)`. For `inat_images()` and `inat_images_md()`,
#' `image_size` follows `default_image_size`. An explicit function argument
#' takes precedence over `sacha_arg_configure()`; a shared argument default
#' takes precedence over the module setting.
#' @usage
#' inat_images(
#'   species = NULL,
#'   user_id = NULL,
#'   number_observations = 1L,
#'   observation_id = NULL,
#'   image_size = NULL,
#'   source = c("web", "local"),
#'   verify_local = FALSE,
#'   directory = getwd(),
#'   quality_grade = NULL,
#'   place_id = NULL,
#'   catalogue_dataset = NULL,
#'   language = NULL,
#'   observer_name = NULL,
#'   save_images = FALSE,
#'   include_auxiliary = FALSE,
#'   export_comp = FALSE,
#'   refresh = FALSE,
#'   taxon_id = NULL
#' )
#'
#' @return inat_images() returns the selected observations and their image metadata.
#'   Printing the result shows a concise observation summary. inat_images_md()
#'   invisibly returns the same metadata object after emitting figure markup for
#'   a Markdown document.
#'
#' @references
#' iNaturalist API recommended practices:
#' <https://www.inaturalist.org/pages/api+recommended+practices>.
#' iNaturalist API v2 reference: <https://api.inaturalist.org/v2/docs/>.
#' iNaturalist search filters and photo licenses:
#' <https://www.inaturalist.org/pages/search+urls>.
#' iNaturalist guidance on photo use and attribution:
#' <https://help.inaturalist.org/en/support/solutions/articles/151000169918>.
#'
#' Captions identify the scientific name, locality, observation date, observer,
#' photo licence, and observation URL. The licence applies to the photograph;
#' the linked URL identifies the observation. In Markdown output, one figure
#' groups the selected observations for a taxon and each observation has its
#' own subfigure caption. The Quarto figure ID is
#' `fig-taxon-<taxon_id>`; each subfigure ID is
#' `fig-sub-<observation_id>`. The taxon ID is resolved from the observations
#' and is taken from the returned observations. For example,
#' after rendering `Bidens andicola`, use `@fig-taxon-<resolved_taxon_id>` to
#' refer to its panel, or `@fig-sub-<observation_id>` to refer to one
#' observation. The generated caption is language-dependent; its English form
#' follows the pattern `Bidens andicola ... observed in <locality> on
#' dd/mm/yyyy, by <observer> (Licence: <photo licence>). <observation URL>`.
#' In Quarto or R Markdown, set `results: asis` on the chunk that calls
#' `inat_images_md()` so the figure markup is rendered and its IDs can be
#' referenced from the document.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inat_images("Bidens andicola", user_id = "marceloamores")
#'
#' inat_images("Bidens andicola", user_id = "marceloamores",
#'             save_images = TRUE)
#' inat_images("Bidens andicola", user_id = "marceloamores",
#'             source = "local")
#' }
inat_images <- function(
    species = NULL,
    user_id = NULL,
    number_observations = 1L,
    observation_id = NULL,
    image_size = NULL,
    source = c("web", "local"),
    verify_local = FALSE,
    directory = getwd(),
    quality_grade = NULL,
    place_id = NULL,
    catalogue_dataset = NULL,
    language = NULL,
    observer_name = NULL,
    save_images = FALSE,
    include_auxiliary = FALSE,
    export_comp = FALSE,
    refresh = FALSE,
    taxon_id = NULL
) {
  missing_image_size <- missing(image_size)
  missing_quality_grade <- missing(quality_grade)
  missing_catalogue_dataset <- missing(catalogue_dataset)
  missing_language <- missing(language)
  call <- match.call(expand.dots = FALSE)
  defaults <- .inat_argument_defaults()

  if (missing_image_size) image_size <- defaults$image_size
  if (missing_quality_grade) quality_grade <- defaults$quality_grade
  if (missing_catalogue_dataset) catalogue_dataset <- defaults$catalogue_dataset
  if (missing_language) language <- defaults$language

  .sacha_apply_arg_defaults(environment(), call, names(formals(sys.function())))
  language <- match.arg(language, choices = defaults$language_choices)
  image_size <- match.arg(image_size, choices = defaults$image_size_choices)
  source <- match.arg(source)
  if (!is.logical(save_images) || length(save_images) != 1L || is.na(save_images)) {
    stop("'save_images' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(export_comp) || length(export_comp) != 1L || is.na(export_comp)) {
    stop("'export_comp' must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(export_comp) && !requireNamespace("magick", quietly = TRUE)) {
    stop("Install the optional 'magick' package to export image compositions.", call. = FALSE)
  }
  images <- tryCatch(
    .inat_build_sacha_images(
      species = species,
      user_id = user_id,
      number_observations = number_observations,
      observation_id = observation_id,
      image_size = image_size,
      source = source,
      verify_local = verify_local,
      directory = directory,
      quality_grade = quality_grade,
      place_id = place_id,
      catalogue_dataset = catalogue_dataset,
      language = language,
      observer_name = observer_name,
      save_images = save_images,
      include_auxiliary = include_auxiliary,
      refresh = refresh,
      use_placeholders = TRUE,
      taxon_id = taxon_id
    ),
    error = function(error) {
      message("iNaturalist image query failed: ", conditionMessage(error))
      NULL
    }
  )
  if (is.null(images)) {
    return(invisible(NULL))
  }
  .inat_print_image_metadata(images, image_size)
  if (isTRUE(save_images) && length(images$saved_files) > 0L) {
    saved_count <- sum(vapply(
      images$saved_files,
      function(item) as.integer(item$saved_images),
      integer(1)
    ))
    cat("\nSaved ", saved_count, " image file(s) and observation metadata:\n", sep = "")
    for (item in images$saved_files) {
      cat("  ", item$observation_id, ": ", item$metadata_file, "\n", sep = "")
    }
  }
  composition_path <- .inat_show_image_composition(
    images,
    directory,
    export_comp = export_comp,
    ncol = 2L
  )
  if (isTRUE(export_comp) && !is.null(composition_path)) {
    cat("\nComposition exported to: ", composition_path, "\n", sep = "")
  }
  images
}







#' @rdname inat_images
#' @export
#' @usage
#' inat_images_md(
#'   species = NULL,
#'   user_id = NULL,
#'   number_observations = 1L,
#'   observation_id = NULL,
#'   image_size = NULL,
#'   source = c("web", "local"),
#'   verify_local = FALSE,
#'   directory = getwd(),
#'   quality_grade = NULL,
#'   place_id = NULL,
#'   catalogue_dataset = NULL,
#'   language = NULL,
#'   observer_name = NULL,
#'   save_images = FALSE,
#'   include_auxiliary = FALSE,
#'   export_comp = FALSE,
#'   refresh = FALSE,
#'   ncol = 2L,
#'   word_subfigure_width = "3in",
#'   latex_subfigure_width = "86\%",
#'   taxon_id = NULL
#' )
#'
#' @examples
#' \dontrun{
#' inat_images_md("Bidens andicola", user_id = "marceloamores")
#' }
inat_images_md <- function(
    species = NULL,
    user_id = NULL,
    number_observations = 1L,
    observation_id = NULL,
    image_size = NULL,
    source = c("web", "local"),
    verify_local = FALSE,
    directory = getwd(),
    quality_grade = NULL,
    place_id = NULL,
    catalogue_dataset = NULL,
    language = NULL,
    observer_name = NULL,
    save_images = FALSE,
    include_auxiliary = FALSE,
    export_comp = FALSE,
    refresh = FALSE,
    ncol = 2L,
    word_subfigure_width = "3in",
    latex_subfigure_width = "86%",
    taxon_id = NULL
) {
  missing_image_size <- missing(image_size)
  missing_quality_grade <- missing(quality_grade)
  missing_catalogue_dataset <- missing(catalogue_dataset)
  missing_language <- missing(language)
  call <- match.call(expand.dots = FALSE)
  defaults <- .inat_argument_defaults()

  if (missing_image_size) image_size <- defaults$image_size
  if (missing_quality_grade) quality_grade <- defaults$quality_grade
  if (missing_catalogue_dataset) catalogue_dataset <- defaults$catalogue_dataset
  if (missing_language) language <- defaults$language

  .sacha_apply_arg_defaults(environment(), call, names(formals(sys.function())))
  language <- match.arg(language, choices = defaults$language_choices)
  image_size <- match.arg(image_size, choices = defaults$image_size_choices)
  source <- match.arg(source)
  if (!is.logical(save_images) || length(save_images) != 1L || is.na(save_images)) {
    stop("'save_images' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(export_comp) || length(export_comp) != 1L || is.na(export_comp)) {
    stop("'export_comp' must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(export_comp) && !requireNamespace("magick", quietly = TRUE)) {
    return(invisible(NULL))
  }

  images <- suppressWarnings(tryCatch(.inat_build_sacha_images(
    species = species,
    user_id = user_id,
    number_observations = number_observations,
    observation_id = observation_id,
    image_size = image_size,
    source = source,
    verify_local = verify_local,
    directory = directory,
    quality_grade = quality_grade,
    place_id = place_id,
    catalogue_dataset = catalogue_dataset,
    language = language,
    observer_name = observer_name,
    save_images = save_images,
    include_auxiliary = include_auxiliary,
    refresh = refresh,
    use_placeholders = FALSE,
    taxon_id = taxon_id
  ), error = function(error) NULL))
  if (is.null(images)) {
    return(invisible(NULL))
  }

  .inat_render_images_md(
    images = images,
    directory = directory,
    observer_name = observer_name,
    export_comp = export_comp,
    ncol = ncol,
    word_subfigure_width = word_subfigure_width,
    latex_subfigure_width = latex_subfigure_width
  )
}







