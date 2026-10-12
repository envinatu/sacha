.sacha_spatial_config <- new.env(parent = emptyenv())
.sacha_spatial_config$language <- "en"
.sacha_spatial_config$gbif_checklist_key <- "7ddf754f-d193-4cc9-b351-99906754a03b"
.sacha_spatial_config$default_limit <- 2000L
.sacha_spatial_config$http_timeout <- 30
.sacha_spatial_config$http_max_tries <- 3L
.sacha_spatial_config$cache_ttl <- Inf

.sacha_spatial_cache <- new.env(parent = emptyenv())
.sacha_spatial_i18n <- new.env(parent = emptyenv())

.sacha_spatial_configure <- function(language = NULL,
                                    default_limit = NULL,
                                    http_timeout = NULL,
                                    http_max_tries = NULL,
                                    cache_ttl = NULL) {
  is_query <- is.null(language) &&
    is.null(default_limit) && is.null(http_timeout) &&
    is.null(http_max_tries) && is.null(cache_ttl)

  if (!is.null(language)) {
    .sacha_spatial_config$language <- .sacha_spatial_language(language)
  }

  if (!is.null(default_limit)) {
    default_limit <- .sacha_spatial_validate_limit(default_limit)
    .sacha_spatial_config$default_limit <- default_limit
  }

  if (!is.null(http_timeout)) {
    http_timeout <- suppressWarnings(as.numeric(http_timeout)[1])
    if (is.na(http_timeout) || http_timeout <= 0) {
      stop("`http_timeout` must be a positive number of seconds.", call. = FALSE)
    }
    .sacha_spatial_config$http_timeout <- http_timeout
  }

  if (!is.null(http_max_tries)) {
    http_max_tries <- suppressWarnings(as.integer(http_max_tries)[1])
    if (is.na(http_max_tries) || http_max_tries < 1L) {
      stop("`http_max_tries` must be an integer >= 1.", call. = FALSE)
    }
    .sacha_spatial_config$http_max_tries <- http_max_tries
  }

  if (!is.null(cache_ttl)) {
    cache_ttl <- suppressWarnings(as.numeric(cache_ttl)[1])
    if (is.na(cache_ttl) || cache_ttl <= 0) {
      stop("`cache_ttl` must be a positive number of seconds, or Inf.", call. = FALSE)
    }
    .sacha_spatial_config$cache_ttl <- cache_ttl
  }

  result <- as.list(.sacha_spatial_config)
  if (is_query) result else invisible(result)
}

.sacha_spatial_clear_cache <- function(species = NULL, reload_languages = FALSE) {
  reload_languages <- .sacha_spatial_validate_logical(reload_languages, "reload_languages")

  if (is.null(species)) {
    keys <- ls(envir = .sacha_spatial_cache, all.names = TRUE)
    if (length(keys)) rm(list = keys, envir = .sacha_spatial_cache)
  } else {
    species <- .sacha_spatial_validate_species(species)
    key <- .sacha_spatial_cache_key(species)
    if (exists(key, envir = .sacha_spatial_cache, inherits = FALSE)) {
      rm(list = key, envir = .sacha_spatial_cache)
    }
    location_prefix <- paste0(
      "spatial_locations::",
      tolower(species),
      "::"
    )
    location_keys <- ls(envir = .sacha_global_cache, all.names = TRUE)
    location_keys <- location_keys[startsWith(location_keys, location_prefix)]
    if (length(location_keys)) {
      rm(list = location_keys, envir = .sacha_global_cache)
    }
  }

  if (isTRUE(reload_languages)) {
    .sacha_spatial_load_languages(force = TRUE)
  }

  invisible(NULL)
}

.sacha_spatial_null_coalesce <- function(x, y) {
  if (is.null(x) || !length(x) || (length(x) == 1 && !is.list(x) && (is.na(x) || identical(x, "")))) {
    y
  } else {
    x
  }
}

`%spatial||%` <- .sacha_spatial_null_coalesce

.sacha_spatial_response <- function(status = c("ok", "empty", "error"),
                                    data = NULL, message = NULL) {
  status <- match.arg(status)
  structure(list(status = status, data = data, message = message),
            class = "sacha_spatial_response")
}

.sacha_spatial_lang_dir <- function() {
  root <- system.file("lang", package = "sacha")
  if (nzchar(root)) return(root)
  candidate <- file.path(getwd(), "inst", "lang")
  if (dir.exists(candidate)) return(candidate)
  stop("Could not locate the 'sacha' package's language files. Load the ",
       "package first, e.g. with library(sacha) or devtools::load_all().",
       call. = FALSE)
}

.sacha_spatial_lang_files <- function() {
  directory <- .sacha_spatial_lang_dir()
  files <- list.files(directory, pattern = "\\.ya?ml$", full.names = TRUE)
  if (!length(files)) {
    stop(sprintf("No language files (*.yml) were found in '%s'.", directory), call. = FALSE)
  }
  files
}

.sacha_spatial_load_languages <- function(force = FALSE) {
  if (!isTRUE(force) &&
      exists("labels_geo", envir = .sacha_spatial_i18n, inherits = FALSE)) {
    return(invisible(NULL))
  }
  .sacha_spatial_require("yaml")

  labels_geo <- list()
  aliases <- character()
  reference_keys <- NULL
  reference_file <- NULL

  for (file in .sacha_spatial_lang_files()) {
    code <- tolower(tools::file_path_sans_ext(basename(file)))
    content <- tryCatch(
      yaml::read_yaml(file),
      error = function(e) {
        stop(sprintf("Could not read language file '%s': %s.", file, conditionMessage(e)),
             call. = FALSE)
      }
    )

    entries <- content$labels_geo
    if (!is.list(entries) || is.null(names(entries)) || !all(nzchar(names(entries)))) {
      stop(sprintf("Language file '%s' must define a named `labels_geo:` map.", file),
           call. = FALSE)
    }

    keys <- sort(names(entries))
    if (is.null(reference_keys)) {
      reference_keys <- keys
      reference_file <- file
    } else if (!identical(keys, reference_keys)) {
      stop(sprintf(
        "Language file '%s' has different label keys than '%s'.\nMissing: %s\nExtra: %s",
        file, reference_file,
        paste(setdiff(reference_keys, keys), collapse = ", ") %spatial||% "(none)",
        paste(setdiff(keys, reference_keys), collapse = ", ") %spatial||% "(none)"
      ), call. = FALSE)
    }

    labels_geo[[code]] <- vapply(entries, function(x) as.character(x)[1], character(1))

    own_aliases <- tolower(trimws(as.character(content$aliases %spatial||% character())))
    own_aliases <- own_aliases[nzchar(own_aliases)]
    codes <- unique(c(code, own_aliases))
    mapping <- rep(code, length(codes))
    names(mapping) <- codes
    aliases <- c(aliases, mapping)
  }

  aliases <- aliases[!duplicated(names(aliases))]

  assign("labels_geo", labels_geo, envir = .sacha_spatial_i18n)
  assign("aliases", aliases, envir = .sacha_spatial_i18n)
  invisible(NULL)
}

.sacha_spatial_language <- function(language = NULL) {
  if (is.null(language)) language <- .sacha_spatial_config$language
  language <- tolower(trimws(as.character(language)[1]))
  .sacha_spatial_load_languages()
  aliases <- get("aliases", envir = .sacha_spatial_i18n, inherits = FALSE)
  if (is.na(language) || !(language %in% names(aliases))) {
    stop(sprintf("`language` must be one of: %s.", paste(sort(unique(aliases)), collapse = ", ")),
         call. = FALSE)
  }
  unname(aliases[[language]])
}

.sacha_spatial_label <- function(key, language = NULL) {
  language <- .sacha_spatial_language(language)
  labels_geo <- get("labels_geo", envir = .sacha_spatial_i18n, inherits = FALSE)
  labels_geo[[language]][[key]]
}

.sacha_spatial_validate_species <- function(species) {
  if (!is.character(species) || length(species) != 1L || is.na(species)) {
    stop("`species` must be one non-missing scientific name, e.g. 'Bidens andicola'.",
         call. = FALSE)
  }
  species <- gsub("\\s+", " ", trimws(species))
  if (!nzchar(species) || length(strsplit(species, " ", fixed = TRUE)[[1]]) < 2L) {
    stop("`species` must contain at least a genus and a specific epithet.", call. = FALSE)
  }
  species
}

.sacha_spatial_validate_limit <- function(limit) {
  limit <- suppressWarnings(as.integer(limit)[1])
  if (is.na(limit) || limit < 1L || limit > 100000L) {
    stop("`limit` must be an integer between 1 and 100000.", call. = FALSE)
  }
  limit
}

.sacha_spatial_validate_colour <- function(colour) {
  if (!is.character(colour) || length(colour) != 1L || is.na(colour) ||
      !grepl("^#(?:[0-9A-Fa-f]{3}|[0-9A-Fa-f]{4}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$", colour)) {
    stop("`colour` must be a hexadecimal colour string, e.g. '#E52472'.", call. = FALSE)
  }
  colour
}

.sacha_spatial_validate_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
  x
}

.sacha_spatial_cache_key <- function(species) {
  paste(tolower(species), .sacha_global_config$dataset, sep = "|")
}

.sacha_spatial_cache_get <- function(key) {
  if (exists(key, envir = .sacha_spatial_cache, inherits = FALSE)) {
    get(key, envir = .sacha_spatial_cache, inherits = FALSE)
  } else {
    NULL
  }
}

.sacha_spatial_cache_set <- function(key, value) {
  assign(key, value, envir = .sacha_spatial_cache)
  invisible(value)
}

.sacha_spatial_cache_fresh <- function(fetched_at) {
  if (is.null(fetched_at)) return(FALSE)
  ttl <- .sacha_spatial_config$cache_ttl
  if (is.null(ttl) || !is.finite(ttl)) return(TRUE)
  as.numeric(difftime(Sys.time(), fetched_at, units = "secs")) < ttl
}

.sacha_spatial_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x)) return(default)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x)) & tolower(trimws(x)) != "null"]
  if (!length(x)) default else x[1]
}

.sacha_spatial_field <- function(x, names_to_try, default = NA_character_) {
  if (is.null(x) || !is.list(x) || is.null(names(x))) return(default)
  for (field in names_to_try) {
    index <- which(tolower(names(x)) == tolower(field))[1]
    if (!is.na(index)) {
      value <- .sacha_spatial_scalar(x[[index]], default = NA_character_)
      if (!is.na(value)) return(value)
    }
  }
  default
}

.sacha_spatial_require <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      sprintf("Missing required package(s): %s. Install them before using this function.",
              paste(sprintf("'%s'", missing), collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.sacha_spatial_slug <- function(x) {
  x <- tolower(trimws(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", "_", x)
  sub("^_+|_+$", "", x)
}

.sacha_spatial_path <- function(species, extension, directory, prefix = "sacha") {
  directory <- normalizePath(directory, winslash = "/", mustWork = FALSE)
  if (!dir.exists(directory)) {
    stop("`directory` does not exist. Create it before writing output files.", call. = FALSE)
  }
  file.path(directory, paste0(prefix, "_", .sacha_spatial_slug(species), ".", extension))
}

.sacha_spatial_unique_path <- function(path, overwrite) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop(sprintf("Output already exists: %s. Set `overwrite = TRUE` to replace it.", path),
         call. = FALSE)
  }
  path
}

.sacha_spatial_is_transient_condition <- function(err) {
  msg <- tryCatch(conditionMessage(err), error = function(e) "")
  grepl("(^|[^0-9])(429|500|502|503|504)([^0-9]|$)|time.?out|connection reset|
        |could not resolve|temporarily unavailable|service unavailable",
        msg, ignore.case = TRUE, perl = TRUE)
}

.sacha_spatial_with_retry <- function(fn, max_tries = .sacha_spatial_config$http_max_tries,
                                      base_delay = 1,
                                      is_transient = .sacha_spatial_is_transient_condition) {
  max_tries <- max(1L, as.integer(max_tries))
  attempt <- 1L
  repeat {
    outcome <- tryCatch(list(ok = TRUE, value = fn()),
                        error = function(e) list(ok = FALSE, error = e))
    if (isTRUE(outcome$ok)) return(outcome$value)
    if (attempt >= max_tries || !isTRUE(is_transient(outcome$error))) {
      stop(outcome$error)
    }
    Sys.sleep(base_delay * 2^(attempt - 1L))
    attempt <- attempt + 1L
  }
}

.sacha_spatial_http_json <- function(url, query = list()) {
  .sacha_spatial_require("httr2")

  request <- httr2::request(url)
  request <- httr2::req_headers(request, Accept = "application/json")
  request <- httr2::req_user_agent(
    request,
    "sacha R package (https://github.com/envinatu)"
  )
  request <- httr2::req_timeout(request, seconds = .sacha_spatial_config$http_timeout)
  request <- httr2::req_error(request, is_error = function(response) FALSE)
  request <- httr2::req_retry(
    request,
    max_tries = .sacha_spatial_config$http_max_tries,
    is_transient = function(response) httr2::resp_status(response) %in%
      c(429L, 500L, 502L, 503L, 504L)
  )

  query <- query[!vapply(query, is.null, logical(1))]
  if (length(query)) request <- do.call(httr2::req_url_query, c(list(request), query))

  response <- tryCatch(httr2::req_perform(request), error = function(e) e)
  if (inherits(response, "error") || inherits(response, "condition")) {
    message <- conditionMessage(response)
    warning(sprintf(.sacha_spatial_label("warn_unreachable"), url, message), call. = FALSE)
    return(.sacha_spatial_response("error", message = message))
  }

  status_code <- httr2::resp_status(response)
  if (status_code < 200L || status_code >= 300L) {
    message <- sprintf("HTTP %s", status_code)
    warning(sprintf(.sacha_spatial_label("warn_http_status"), message, url), call. = FALSE)
    return(.sacha_spatial_response("error", message = message))
  }

  body <- tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(e) e
  )
  if (inherits(body, "error") || inherits(body, "condition")) {
    message <- conditionMessage(body)
    warning(sprintf(.sacha_spatial_label("warn_malformed_json"), url, message),
            call. = FALSE)
    return(.sacha_spatial_response("error", message = message))
  }

  if (is.null(body) || (is.list(body) && !length(body))) {
    return(.sacha_spatial_response("empty"))
  }

  .sacha_spatial_response("ok", data = body)
}
