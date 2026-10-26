.sacha_spatial_gbif_checklist_key <- function() {
  key <- .sacha_spatial_config$gbif_checklist_key
  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(trimws(key))) {
    "7ddf754f-d193-4cc9-b351-99906754a03b"
  } else {
    trimws(key)
  }
}

.sacha_spatial_validate_taxon_id <- function(taxon_id) {
  if (!is.character(taxon_id) || length(taxon_id) != 1L || is.na(taxon_id)) {
    stop("`taxon_id` must be one non-missing COL XR alphanumeric taxon key, e.g. 'Q2M4'.",
         call. = FALSE)
  }
  taxon_id <- trimws(taxon_id)
  if (!grepl("^[[:alnum:]]+$", taxon_id) || !grepl("[[:alpha:]]", taxon_id)) {
    stop("`taxon_id` must be an alphanumeric COL XR taxon key, e.g. 'Q2M4'.",
         call. = FALSE)
  }
  taxon_id
}

.sacha_spatial_taxon_input <- function(species = NULL, taxon_id = NULL) {
  if (is.null(species) == is.null(taxon_id)) {
    stop("Supply exactly one of `species` or `taxon_id`.", call. = FALSE)
  }
  if (!is.null(species)) {
    return(list(species = .sacha_spatial_validate_species(species), taxon_id = NULL))
  }
  list(species = NULL, taxon_id = .sacha_spatial_validate_taxon_id(taxon_id))
}

.sacha_spatial_apply_taxon_arg_defaults <- function(frame, call, formal_names) {
  supplied <- names(as.list(call)[-1L])
  .sacha_apply_arg_defaults(frame, call, formal_names)
  if ("taxon_id" %in% supplied && !("species" %in% supplied)) {
    assign("species", NULL, envir = frame)
  }
  if ("species" %in% supplied && !("taxon_id" %in% supplied)) {
    assign("taxon_id", NULL, envir = frame)
  }
  invisible(NULL)
}

.sacha_spatial_validate_date <- function(date) {
  if (is.null(date)) return(NULL)
  if (!is.character(date) || length(date) != 1L || is.na(date)) {
    stop("`date` must be one ISO date or an ISO date range, e.g. '2020-01-01,2021-01-01'.",
         call. = FALSE)
  }
  date <- trimws(date)
  comma_count <- lengths(regmatches(date, gregexpr(",", date, fixed = TRUE)))
  if (comma_count > 1L || grepl("^,|,$", date)) {
    stop("`date` must be one ISO date or an ordered pair of ISO dates separated by a comma.",
         call. = FALSE)
  }
  values <- trimws(strsplit(date, ",", fixed = TRUE)[[1L]])
  valid_date <- function(value) {
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value) &&
      !is.na(suppressWarnings(as.Date(value, format = "%Y-%m-%d")))
  }
  if (!(length(values) %in% c(1L, 2L)) || !all(vapply(values, valid_date, logical(1)))) {
    stop("`date` must be an ISO date or an ordered pair of ISO dates separated by a comma.",
         call. = FALSE)
  }
  if (length(values) == 2L && as.Date(values[1L]) > as.Date(values[2L])) {
    stop("The start date in `date` must not be later than its end date.", call. = FALSE)
  }
  paste(values, collapse = ",")
}

.sacha_spatial_gbif_dataset_keys <- function(date) {
  if (is.null(date)) return(NULL)
  .sacha_spatial_require("rgbif")
  datasets <- tryCatch(
    .sacha_spatial_with_retry(
      function() rgbif::dataset_export(type = "OCCURRENCE", createdDate = date),
      max_tries = .sacha_spatial_config$http_max_tries
    ),
    error = function(e) e
  )
  if (inherits(datasets, "error") || inherits(datasets, "condition")) {
    stop(sprintf("Could not find GBIF occurrence datasets created within `%s`: %s.",
                 date, conditionMessage(datasets)), call. = FALSE)
  }
  datasets <- as.data.frame(datasets)
  if (!nrow(datasets)) return(character())
  if (!("key" %in% names(datasets))) {
    stop("GBIF dataset search did not return dataset keys for the `date` filter.",
         call. = FALSE)
  }
  keys <- unique(as.character(datasets$key))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (length(keys) > 100000L) {
    stop("The `date` filter matched more than 100000 GBIF occurrence datasets; narrow the date range.",
         call. = FALSE)
  }
  keys
}

.sacha_spatial_filter_local_dataset_date <- function(occurrences, metadata, date) {
  if (is.null(date)) return(occurrences)
  dataset_filter <- metadata$dataset_filter %spatial||% NULL
  if (is.null(dataset_filter) || !identical(dataset_filter$createdDate, date) ||
      is.null(dataset_filter$datasetKeys)) {
    stop("This local export was not saved with the same `date` filter. Create a GBIF export with that date range first.",
         call. = FALSE)
  }
  if (!("datasetKey" %in% names(occurrences))) {
    stop("The local export does not include `datasetKey`, so its `date` filter cannot be applied.",
         call. = FALSE)
  }
  occurrences[as.character(occurrences$datasetKey) %in% unlist(dataset_filter$datasetKeys),
              , drop = FALSE]
}

.sacha_spatial_gbif_taxon <- function(species = NULL, taxon_id = NULL) {
  .sacha_spatial_require("rgbif")
  input <- .sacha_spatial_taxon_input(species, taxon_id)
  species <- input$species
  taxon_id <- input$taxon_id
  checklist_key <- .sacha_spatial_gbif_checklist_key()
  key <- if (is.null(taxon_id)) {
    .sacha_spatial_cache_key(species)
  } else {
    paste("taxon_id", taxon_id, checklist_key, sep = "|")
  }
  cached <- .sacha_spatial_cache_get(key)
  if (!is.null(cached) && !is.null(cached$taxon) &&
      identical(cached$taxon$status, "ok") &&
      identical(cached$taxon$checklist_key, checklist_key) &&
      .sacha_spatial_cache_fresh(cached$taxon$fetched_at)) {
    cached_taxon <- cached$taxon
    cached_taxon$input_name <- species %spatial||% cached_taxon$canonical_name
    cached_taxon$input_taxon_id <- taxon_id
    if (!is.null(taxon_id)) cached_taxon$match_type <- "EXACT"
    return(cached_taxon)
  }

  response <- tryCatch(
    .sacha_spatial_with_retry(
      function() {
        if (is.null(taxon_id)) {
          rgbif::name_backbone(
            name = species, rank = "species", strict = TRUE,
            checklistKey = checklist_key
          )
        } else {
          rgbif::name_backbone(usageKey = taxon_id, checklistKey = checklist_key)
        }
      },
      max_tries = .sacha_spatial_config$http_max_tries
    ),
    error = function(e) e
  )
  if (inherits(response, "error") || inherits(response, "condition")) {
    return(list(
      status = "error", message = conditionMessage(response),
      fetched_at = Sys.time(), error_type = "communication"
    ))
  }

  field <- function(name) {
    if (!is.data.frame(response) || !nrow(response) || !(name %in% names(response))) {
      return(NA_character_)
    }
    value <- as.character(response[[name]][1])
    if (is.na(value) || !nzchar(trimws(value))) NA_character_ else value
  }
  taxon_key <- field("usageKey")
  match_type <- toupper(field("matchType"))
  rank <- toupper(field("rank"))
  taxon_status <- toupper(field("status"))
  accepted_taxon <- identical(taxon_status, "ACCEPTED")
  exact_name <- is.null(taxon_id) && identical(match_type, "EXACT")
  exact_key <- !is.null(taxon_id) && !is.na(taxon_key) && identical(taxon_key, taxon_id)
  if (is.na(taxon_key) || !(exact_name || exact_key) || !identical(rank, "SPECIES") ||
      !accepted_taxon) {
    return(list(
      status = "error",
      message = sprintf(
        "GBIF could not resolve %s as one exact accepted species-level taxon in COL Extended Release (match type: %s; rank: %s; status: %s).",
        if (is.null(taxon_id)) sQuote(species) else sprintf("taxon_id %s", sQuote(taxon_id)),
        ifelse(is.na(match_type), "unavailable", match_type),
        ifelse(is.na(rank), "unavailable", rank),
        ifelse(is.na(taxon_status), "unavailable", taxon_status)
      ),
      fetched_at = Sys.time(), error_type = "taxonomy"
    ))
  }
  canonical_name <- field("canonicalName")
  accepted_name <- if (!is.na(canonical_name)) canonical_name else field("scientificName")
  if (is.na(accepted_name)) accepted_name <- species
  if (is.null(accepted_name) || is.na(accepted_name)) {
    return(list(
      status = "error",
      message = "GBIF did not return a canonical species name for the exact COL XR taxon.",
      fetched_at = Sys.time(), error_type = "taxonomy"
    ))
  }
  if (is.na(canonical_name)) canonical_name <- accepted_name
  result <- list(
    status = "ok", key = taxon_key, input_name = species %spatial||% accepted_name,
    input_taxon_id = taxon_id,
    accepted_name = accepted_name %spatial||% species,
    canonical_name = canonical_name,
    match_type = match_type %spatial||% if (!is.null(taxon_id)) "EXACT" else NULL,
    rank = rank,
    checklist_key = checklist_key,
    fetched_at = Sys.time()
  )
  cached <- cached %spatial||% list(species = result$canonical_name)
  cached$taxon <- result
  .sacha_spatial_cache_set(key, cached)
  species_key <- .sacha_spatial_cache_key(result$canonical_name)
  species_cached <- .sacha_spatial_cache_get(species_key) %spatial||% list(species = result$canonical_name)
  species_cached$taxon <- result
  .sacha_spatial_cache_set(species_key, species_cached)
  result
}

.sacha_spatial_resolve_taxon_input <- function(species = NULL, taxon_id = NULL) {
  input <- .sacha_spatial_taxon_input(species, taxon_id)
  taxon <- .sacha_spatial_gbif_taxon(input$species, input$taxon_id)
  if (!identical(taxon$status, "ok")) {
    input_label <- input$species %spatial||%
      sprintf("taxon_id %s", input$taxon_id)
    .sacha_spatial_stop_gbif_error(taxon, input_label)
  }
  list(
    species = .sacha_spatial_validate_species(taxon$canonical_name),
    taxon_id = input$taxon_id,
    taxon = taxon
  )
}

.sacha_spatial_gbif_occurrence_search_by_datasets <- function(taxon, dataset_keys,
                                                             limit = NULL) {
  if (!length(dataset_keys)) return(list(data = data.frame()))
  dataset_groups <- split(dataset_keys, ceiling(seq_along(dataset_keys) / 50L))
  pages <- list()
  retrieved <- 0L
  for (dataset_group in dataset_groups) {
    if (!is.null(limit) && retrieved >= limit) break
    dataset_key <- paste(dataset_group, collapse = ";")
    if (!is.null(limit)) {
      page <- .sacha_spatial_with_retry(
        function() rgbif::occ_search(
          taxonKey = taxon$key, checklistKey = taxon$checklist_key,
          hasCoordinate = TRUE, datasetKey = dataset_key,
          limit = limit - retrieved
        ),
        max_tries = .sacha_spatial_config$http_max_tries
      )
      page_data <- page$data %spatial||% data.frame()
      if (nrow(page_data)) {
        pages[[length(pages) + 1L]] <- page_data
        retrieved <- retrieved + nrow(page_data)
      }
    } else {
      page_size <- 300L
      start <- 0L
      repeat {
        page <- .sacha_spatial_with_retry(
          function() rgbif::occ_search(
            taxonKey = taxon$key, checklistKey = taxon$checklist_key,
            hasCoordinate = TRUE, datasetKey = dataset_key,
            limit = page_size, start = start
          ),
          max_tries = .sacha_spatial_config$http_max_tries
        )
        page_data <- page$data %spatial||% data.frame()
        if (nrow(page_data)) {
          pages[[length(pages) + 1L]] <- page_data
          retrieved <- retrieved + nrow(page_data)
        }
        total <- suppressWarnings(as.integer(page$meta$count)[1L])
        if (!nrow(page_data) || nrow(page_data) < page_size ||
            (!is.na(total) && start + nrow(page_data) >= total)) break
        start <- start + nrow(page_data)
      }
    }
  }
  if (!length(pages)) return(list(data = data.frame()))
  columns <- unique(unlist(lapply(pages, names), use.names = FALSE))
  pages <- lapply(pages, function(page_data) {
    missing_columns <- setdiff(columns, names(page_data))
    for (column in missing_columns) page_data[[column]] <- NA
    page_data[, columns, drop = FALSE]
  })
  list(data = do.call(rbind, pages))
}

.sacha_spatial_gbif_occurrences <- function(species, limit = NULL, date = NULL,
                                            dataset_keys = NULL, taxon = NULL,
                                            crop_data = NULL) {
  .sacha_spatial_require("rgbif")
  if (utils::packageVersion("rgbif") < "3.9.0") {
    stop("`rgbif` >= 3.9.0 is required so that the taxonomy used for GBIF occurrence searches (COL Extended Release) is explicit and reproducible.",
         call. = FALSE)
  }
  species <- .sacha_spatial_validate_species(species)
  if (!is.null(limit)) limit <- .sacha_spatial_validate_limit(limit)
  date <- .sacha_spatial_validate_date(date)
  spatial_query <- .sacha_spatial_gbif_crop_query(crop_data)
  has_spatial_query <- !is.null(spatial_query$country) ||
    !is.null(spatial_query$geometry)
  key <- .sacha_spatial_cache_key(species)
  cacheable <- is.null(date) && !has_spatial_query
  cached <- if (cacheable) .sacha_spatial_cache_get(key) else NULL
  taxon <- taxon %spatial||% .sacha_spatial_gbif_taxon(species)
  if (!identical(taxon$status, "ok")) {
    return(list(
      occurrences = data.frame(), requested_limit = limit %spatial||% Inf,
      status = "error", message = taxon$message, fetched_at = Sys.time(),
      error_type = taxon$error_type, taxon = taxon
    ))
  }
  if (!is.null(date) && is.null(dataset_keys)) {
    dataset_keys <- .sacha_spatial_gbif_dataset_keys(date)
  }
  if (!is.null(date) && !length(dataset_keys)) {
    return(list(
      occurrences = data.frame(), requested_limit = limit %spatial||% Inf,
      status = "empty", message = NULL, fetched_at = Sys.time(), taxon = taxon,
      date = date, dataset_keys = character(), spatial_query = spatial_query
    ))
  }

  if (cacheable && !is.null(limit) && !is.null(cached) && !is.null(cached$gbif) &&
      cached$gbif$requested_limit >= limit &&
      identical(cached$gbif$status, "ok") &&
      !is.null(cached$gbif$taxon$key) &&
      identical(cached$gbif$taxon$key, taxon$key) &&
      .sacha_spatial_cache_fresh(cached$gbif$fetched_at)) {
    result <- cached$gbif
    result$occurrences <- utils::head(result$occurrences, limit)
    return(result)
  }
  cached <- cached %spatial||% list(species = species)

  response <- tryCatch({
    if (has_spatial_query) {
      .sacha_spatial_gbif_filtered_search(
        taxon, limit, dataset_keys, spatial_query, crop_data
      )
    } else if (!is.null(date)) {
      .sacha_spatial_gbif_occurrence_search_by_datasets(taxon, dataset_keys, limit)
    } else if (!is.null(limit)) {
      .sacha_spatial_with_retry(
        function() rgbif::occ_search(
          taxonKey = taxon$key, checklistKey = taxon$checklist_key,
          hasCoordinate = TRUE, limit = limit
        ),
        max_tries = .sacha_spatial_config$http_max_tries
      )
    } else {
      page_size <- 300L
      start <- 0L
      pages <- list()
      total <- Inf
      repeat {
        page <- .sacha_spatial_with_retry(
          function() rgbif::occ_search(
            taxonKey = taxon$key, checklistKey = taxon$checklist_key,
            hasCoordinate = TRUE,
            limit = page_size, start = start
          ),
          max_tries = .sacha_spatial_config$http_max_tries
        )
        page_data <- page$data %spatial||% data.frame()
        pages[[length(pages) + 1L]] <- page_data
        total <- suppressWarnings(as.integer(page$meta$count)[1])
        if (!nrow(page_data) || nrow(page_data) < page_size ||
            (!is.na(total) && start + nrow(page_data) >= total)) break
        start <- start + nrow(page_data)
      }
      if (length(pages)) {
        columns <- unique(unlist(lapply(pages, names), use.names = FALSE))
        pages <- lapply(pages, function(page_data) {
          missing_columns <- setdiff(columns, names(page_data))
          for (column in missing_columns) page_data[[column]] <- NA
          page_data[, columns, drop = FALSE]
        })
        list(data = do.call(rbind, pages))
      } else {
        list(data = data.frame())
      }
    }
  }, error = function(e) e)

  if (inherits(response, "error") || inherits(response, "condition")) {
    message <- conditionMessage(response)
    warning(sprintf(.sacha_spatial_label("warn_gbif_failed"), species, message),
            call. = FALSE)
    result <- list(occurrences = data.frame(), requested_limit = limit %spatial||% Inf,
                   status = "error", message = message, fetched_at = Sys.time(),
                   error_type = "communication", taxon = taxon)
    if (cacheable && !is.null(limit)) {
      cached$gbif <- result
      .sacha_spatial_cache_set(key, cached)
    }
    return(result)
  }

  data <- response$data %spatial||% data.frame()
  required <- c("decimalLongitude", "decimalLatitude", "scientificName", "year", "stateProvince", "recordedBy")
  for (column in required) if (!(column %in% names(data))) data[[column]] <- NA

  occurrences <- data
  occurrences$decimalLongitude <- suppressWarnings(as.numeric(occurrences$decimalLongitude))
  occurrences$decimalLatitude <- suppressWarnings(as.numeric(occurrences$decimalLatitude))
  occurrences <- occurrences[
    !is.na(occurrences$decimalLongitude) & !is.na(occurrences$decimalLatitude) &
      occurrences$decimalLongitude >= -180 & occurrences$decimalLongitude <= 180 &
      occurrences$decimalLatitude >= -90 & occurrences$decimalLatitude <= 90,
    , drop = FALSE
  ]
  rownames(occurrences) <- NULL

  result <- list(
    occurrences = occurrences,
    requested_limit = limit %spatial||% Inf,
    status = if (nrow(occurrences)) "ok" else "empty",
    message = NULL,
    fetched_at = Sys.time(),
    taxon = taxon, date = date, dataset_keys = dataset_keys,
    spatial_query = if (has_spatial_query) spatial_query else NULL
  )
  if (cacheable && !is.null(limit)) {
    cached$gbif <- result
    .sacha_spatial_cache_set(key, cached)
  }
  result
}

.sacha_spatial_gbif_occurrence_download <- function(taxon, date = NULL,
                                                  dataset_keys = NULL,
                                                  crop_data = NULL) {
  .sacha_spatial_require("rgbif")
  date <- .sacha_spatial_validate_date(date)
  spatial_query <- .sacha_spatial_gbif_crop_query(crop_data)
  if (!is.null(date) && is.null(dataset_keys)) {
    dataset_keys <- .sacha_spatial_gbif_dataset_keys(date)
  }
  if (!is.null(date) && !length(dataset_keys)) {
    return(list(
      occurrences = data.frame(), status = "empty", taxon = taxon,
      download = NULL, date = date, dataset_keys = character(),
      fetched_at = Sys.time(), spatial_query = spatial_query
    ))
  }
  if (utils::packageVersion("rgbif") < "3.9.0") {
    stop("`rgbif` >= 3.9.0 is required to create GBIF downloads with COL Extended Release taxonomy.",
         call. = FALSE)
  }
  predicates <- list(
    rgbif::pred("taxonKey", taxon$key),
    rgbif::pred("hasCoordinate", TRUE)
  )
  if (!is.null(spatial_query$country)) {
    predicates[[length(predicates) + 1L]] <- rgbif::pred("country", spatial_query$country)
  }
  if (!is.null(spatial_query$geometry)) {
    predicates[[length(predicates) + 1L]] <- rgbif::pred_within(spatial_query$geometry)
  }
  if (!is.null(date)) {
    predicates[[length(predicates) + 1L]] <- rgbif::pred_in("datasetKey", dataset_keys)
  }
  request <- tryCatch(
    do.call(
      rgbif::occ_download,
      c(predicates, list(format = "SIMPLE_CSV", checklistKey = taxon$checklist_key))
    ),
    error = function(e) e
  )
  if (inherits(request, "error") || inherits(request, "condition")) {
    stop(
      sprintf(
        .sacha_spatial_label("gbif_download_auth_error"),
        conditionMessage(request)
      ),
      call. = FALSE
    )
  }

  download_meta <- tryCatch(
    rgbif::occ_download_wait(request, quiet = TRUE),
    error = function(e) e
  )
  if (inherits(download_meta, "error") || inherits(download_meta, "condition")) {
    stop(sprintf("Could not complete the GBIF occurrence download: %s",
                 conditionMessage(download_meta)), call. = FALSE)
  }
  status <- if (!is.null(download_meta$status)) {
    toupper(as.character(download_meta$status)[1L])
  } else {
    NA_character_
  }
  if (!identical(status, "SUCCEEDED")) {
    stop(sprintf("The GBIF occurrence download did not succeed (status: %s).",
                 status %spatial||% "unavailable"), call. = FALSE)
  }
  download_key <- if (!is.null(download_meta$key)) {
    as.character(download_meta$key)[1L]
  } else {
    as.character(request)[1L]
  }
  doi_value <- download_meta$doi %spatial||% attr(request, "doi")
  doi <- if (!is.null(doi_value)) as.character(doi_value)[1L] else NA_character_
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  if (is.na(download_key) || !nzchar(download_key) || is.na(doi) || !nzchar(doi)) {
    stop("GBIF completed the occurrence download but did not return its download key and DOI.",
         call. = FALSE)
  }

  download_dir <- tempfile("sacha-gbif-download-")
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(download_dir, recursive = TRUE, force = TRUE), add = TRUE)
  download_file <- tryCatch(
    suppressMessages(rgbif::occ_download_get(
      key = download_key, path = download_dir, overwrite = TRUE
    )),
    error = function(e) e
  )
  if (inherits(download_file, "error") || inherits(download_file, "condition")) {
    stop(sprintf("Could not retrieve GBIF download '%s': %s.", download_key,
                 conditionMessage(download_file)), call. = FALSE)
  }
  occurrences <- tryCatch(
    rgbif::occ_download_import(download_file, path = download_dir),
    error = function(e) e
  )
  if (inherits(occurrences, "error") || inherits(occurrences, "condition")) {
    stop(sprintf("Could not import GBIF download '%s': %s.", download_key,
                 conditionMessage(occurrences)), call. = FALSE)
  }
  occurrences <- as.data.frame(occurrences)
  required <- c("decimalLongitude", "decimalLatitude", "scientificName", "year",
                "stateProvince", "recordedBy")
  for (column in required) if (!(column %in% names(occurrences))) occurrences[[column]] <- NA
  occurrences$decimalLongitude <- suppressWarnings(as.numeric(occurrences$decimalLongitude))
  occurrences$decimalLatitude <- suppressWarnings(as.numeric(occurrences$decimalLatitude))
  occurrences <- occurrences[
    !is.na(occurrences$decimalLongitude) & !is.na(occurrences$decimalLatitude) &
      occurrences$decimalLongitude >= -180 & occurrences$decimalLongitude <= 180 &
      occurrences$decimalLatitude >= -90 & occurrences$decimalLatitude <= 90,
    , drop = FALSE
  ]
  rownames(occurrences) <- NULL

  list(
    occurrences = occurrences,
    status = if (nrow(occurrences)) "ok" else "empty",
    taxon = taxon,
    download = list(
      key = download_key,
      doi = doi,
      doi_url = paste0("https://doi.org/", doi),
      page_url = paste0("https://www.gbif.org/occurrence/download/", download_key),
      format = "SIMPLE_CSV",
      status = status,
      created = if (!is.null(download_meta$created)) {
        as.character(download_meta$created)[1L]
      } else {
        NULL
      },
      record_count = nrow(occurrences),
      created_date_filter = date,
      dataset_key_count = if (is.null(date)) NULL else length(dataset_keys)
    ),
    fetched_at = Sys.time(), date = date, dataset_keys = dataset_keys,
    spatial_query = if (!is.null(spatial_query$country) ||
                        !is.null(spatial_query$geometry)) spatial_query else NULL
  )
}

.sacha_spatial_source <- function(source) {
  match.arg(source, c("web", "local"))
}

.sacha_spatial_local_export <- function(local_export) {
  match.arg(local_export, c("species", "crop"))
}

.sacha_spatial_path_component <- function(x) {
  .sacha_path_component(x)
}

.sacha_spatial_taxon_directory <- function(species, occurrences, directory) {
  .sacha_taxon_export_directory(
    directory,
    family = if ("family" %in% names(occurrences)) {
      values <- as.character(occurrences$family)
      values <- values[!is.na(values) & nzchar(trimws(values))]
      if (length(values)) values[[1L]] else "Unknown_family"
    } else {
      "Unknown_family"
    },
    species = species
  )
}

.sacha_spatial_metadata <- function(species, occurrences, crop = NULL, parent = NULL,
                                    taxon = NULL, download = NULL, date = NULL,
                                    dataset_keys = NULL, spatial_query = NULL) {
  species <- .sacha_spatial_validate_species(species)
  metadata <- list(
    schema_version = 4L,
    source = if (is.null(download)) "GBIF occurrence search API" else "GBIF occurrence download",
    source_url = if (is.null(download)) {
      "https://api.gbif.org/v1/occurrence/search"
    } else {
      download$page_url
    },
    species = species,
    taxonomic_reference = list(
      name = "Catalogue of Life (COL) Extended Release",
      checklistKey = .sacha_spatial_gbif_checklist_key()
    ),
    query = list(
      scientificName = species,
      checklistKey = .sacha_spatial_gbif_checklist_key(),
      taxonKey = if (is.null(taxon)) NULL else taxon$key,
      nameMatch = if (is.null(taxon)) NULL else list(
        inputName = taxon$input_name,
        inputTaxonId = taxon$input_taxon_id,
        acceptedName = taxon$accepted_name,
        matchType = taxon$match_type,
        rank = taxon$rank
      ),
      hasCoordinate = TRUE,
      country = spatial_query$country %spatial||% NULL,
      geometry = spatial_query$geometry %spatial||% NULL,
      geometryIsBoundingBox = if (is.null(spatial_query)) NULL else
        isTRUE(spatial_query$geometry_is_bbox)
    ),
    retrieved_at = format(Sys.time(), tz = "", usetz = TRUE),
    coordinate_reference_system = "EPSG:4326",
    record_count = nrow(occurrences),
    columns = names(occurrences),
    gbif_download = download,
    dataset_filter = if (is.null(date)) NULL else list(
      createdDate = date,
      datasetCount = length(dataset_keys),
      datasetKeys = dataset_keys
    ),
    crop = crop
  )
  if (!is.null(parent)) {
    metadata$source <- parent$source %spatial||% metadata$source
    metadata$source_url <- parent$source_url %spatial||% metadata$source_url
    metadata$query$checklistKey <- parent$query$checklistKey %spatial||%
      metadata$query$checklistKey
    metadata$query$taxonKey <- parent$query$taxonKey %spatial||% metadata$query$taxonKey
    metadata$query$nameMatch <- parent$query$nameMatch %spatial||% metadata$query$nameMatch
    metadata$retrieved_at <- parent$retrieved_at %spatial||% metadata$retrieved_at
    metadata$taxonomic_reference <- parent$taxonomic_reference %spatial||%
      metadata$taxonomic_reference
    metadata$gbif_download <- parent$gbif_download %spatial||% metadata$gbif_download
    metadata$dataset_filter <- parent$dataset_filter %spatial||% metadata$dataset_filter
    metadata$derived_from <- list(
      file = "occurrences.gpkg",
      record_count = parent$record_count,
      retrieved_at = parent$retrieved_at
    )
    if (!is.null(parent$local_coverage)) {
      metadata$derived_from$local_coverage <- parent$local_coverage
    }
    metadata$processed_at <- format(Sys.time(), tz = "", usetz = TRUE)
  }
  if (!is.null(download) &&
      (!is.null(crop) || !identical(as.integer(download$record_count), as.integer(nrow(occurrences))))) {
    metadata$derived_from <- list(
      download_key = download$key,
      doi = download$doi,
      record_count = download$record_count,
      saved_record_count = nrow(occurrences),
      crop = crop
    )
  }
  metadata
}

.sacha_spatial_save_local_occurrences <- function(species, occurrences, directory,
                                                  overwrite, crop = NULL,
                                                  boundary = NULL,
                                                  taxon_directory = NULL,
                                                  parent = NULL, taxon = NULL,
                                                  download = NULL, date = NULL,
                                                  dataset_keys = NULL,
                                                  spatial_query = NULL) {
  .sacha_spatial_require(c("sf", "jsonlite"))
  if (is.null(taxon_directory)) {
    taxon_directory <- .sacha_spatial_taxon_directory(species, occurrences, directory)
  }
  target <- if (is.null(crop)) taxon_directory else file.path(
    taxon_directory, .sacha_spatial_path_component(crop$place)
  )
  if (dir.exists(target) && !isTRUE(overwrite)) {
    stop(sprintf("Local occurrence export already exists: %s. Set `overwrite = TRUE` to replace it.",
                 target), call. = FALSE)
  }
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  data_path <- file.path(target, "occurrences.gpkg")
  metadata_path <- file.path(target, "metadata.json")
  spatial_points <- sf::st_as_sf(
    occurrences,
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326,
    remove = FALSE
  )
  sf::st_write(
    spatial_points, data_path, driver = "GPKG", quiet = TRUE,
    append = FALSE, delete_dsn = file.exists(data_path)
  )
  jsonlite::write_json(
    .sacha_spatial_metadata(
      species, occurrences, crop, parent, taxon, download, date,
      dataset_keys, spatial_query
    ), metadata_path,
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  paths <- c(occurrences = data_path, metadata = metadata_path)
  if (!is.null(boundary)) {
    boundary_path <- file.path(target, "boundary.gpkg")
    sf::st_write(
      boundary, boundary_path, driver = "GPKG", quiet = TRUE,
      append = FALSE, delete_dsn = file.exists(boundary_path)
    )
    paths["boundary"] <- boundary_path
  }
  paths
}

.sacha_spatial_local_taxon_directories <- function(species = NULL, directory,
                                                  taxon_id = NULL) {
  .sacha_spatial_require("jsonlite")
  input <- .sacha_spatial_taxon_input(species, taxon_id)
  species <- input$species
  taxon_id <- input$taxon_id
  matches_taxon <- function(metadata) {
    if (is.null(metadata)) return(FALSE)
    if (!is.null(taxon_id)) {
      return(identical(as.character(metadata$query$taxonKey %spatial||% ""), taxon_id))
    }
    identical(metadata$species, species)
  }
  locate <- function(root) {
    candidates <- list.dirs(root, recursive = FALSE, full.names = TRUE)
    if (length(candidates) == 0L) return(character())
    matches <- vapply(candidates, function(candidate) {
      metadata_path <- file.path(candidate, "metadata.json")
      data_path <- file.path(candidate, "occurrences.gpkg")
      metadata <- if (file.exists(data_path) && file.exists(metadata_path)) {
        tryCatch(jsonlite::read_json(metadata_path, simplifyVector = FALSE), error = function(e) NULL)
      } else {
        NULL
      }
      if (matches_taxon(metadata)) return(TRUE)
      crop_directories <- list.dirs(candidate, recursive = FALSE, full.names = TRUE)
      crop_metadata <- file.path(crop_directories, "metadata.json")
      crop_metadata <- crop_metadata[
        file.exists(crop_metadata) &
          file.exists(file.path(crop_directories, "occurrences.gpkg"))
      ]
      any(vapply(crop_metadata, function(path) {
        candidate_metadata <- tryCatch(
          jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) NULL
        )
        matches_taxon(candidate_metadata) &&
          !is.null(candidate_metadata$crop)
      }, logical(1)))
    }, logical(1))
    candidates[matches]
  }
  locate(.sacha_exports_root(directory))
}

.sacha_spatial_local_export_entries <- function(species = NULL, directory,
                                                taxon_id = NULL, date = NULL) {
  .sacha_spatial_require("jsonlite")
  input <- .sacha_spatial_taxon_input(species, taxon_id)
  species <- input$species
  taxon_id <- input$taxon_id
  root <- .sacha_exports_root(directory)
  if (!dir.exists(root)) return(list())
  data_paths <- list.files(
    root, pattern = "^occurrences\\.gpkg$", recursive = TRUE,
    full.names = TRUE, include.dirs = FALSE
  )
  entries <- lapply(data_paths, function(data_path) {
    metadata_path <- file.path(dirname(data_path), "metadata.json")
    if (!file.exists(metadata_path)) return(NULL)
    metadata <- tryCatch(
      jsonlite::read_json(metadata_path, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(metadata)) return(NULL)
    metadata_species <- .sacha_spatial_scalar(metadata$species)
    metadata_taxon_id <- .sacha_spatial_scalar(metadata$query$taxonKey)
    if (!is.null(taxon_id)) {
      if (is.na(metadata_taxon_id) || !identical(metadata_taxon_id, taxon_id)) {
        return(NULL)
      }
    } else if (is.na(metadata_species) || !identical(metadata_species, species)) {
      return(NULL)
    }
    checklist_key <- .sacha_spatial_scalar(
      metadata$taxonomic_reference$checklistKey
    )
    if (!is.na(checklist_key) &&
        !identical(checklist_key, .sacha_spatial_gbif_checklist_key())) {
      return(NULL)
    }
    if (!is.null(date)) {
      dataset_filter <- metadata$dataset_filter %spatial||% NULL
      if (is.null(dataset_filter) ||
          !identical(.sacha_spatial_scalar(dataset_filter$createdDate), date) ||
          !length(unlist(dataset_filter$datasetKeys, use.names = FALSE))) {
        return(NULL)
      }
    }
    list(
      data_path = data_path,
      metadata_path = metadata_path,
      metadata = metadata
    )
  })
  Filter(Negate(is.null), entries)
}

.sacha_spatial_read_local_occurrence_export <- function(data_path,
                                                        metadata_path = NULL,
                                                        metadata = NULL) {
  .sacha_spatial_require(c("sf", "jsonlite"))
  if (is.null(metadata_path)) metadata_path <- file.path(dirname(data_path), "metadata.json")
  occurrences <- tryCatch(sf::st_read(data_path, quiet = TRUE), error = function(e) {
    stop(sprintf("Could not read local occurrences from '%s': %s.",
                 data_path, conditionMessage(e)), call. = FALSE)
  })
  if (is.null(metadata)) {
    metadata <- tryCatch(
      jsonlite::read_json(metadata_path, simplifyVector = FALSE),
      error = function(e) {
        stop(sprintf("Could not read local metadata from '%s': %s.",
                     metadata_path, conditionMessage(e)), call. = FALSE)
      }
    )
  }
  required <- c("decimalLongitude", "decimalLatitude", "scientificName", "year", "stateProvince")
  if (!is.data.frame(occurrences) || !all(required %in% names(occurrences))) {
    stop(sprintf("Local occurrence export in '%s' is not compatible with the map functions.",
                 dirname(data_path)), call. = FALSE)
  }
  occurrences <- as.data.frame(sf::st_drop_geometry(occurrences))
  if (!("recordedBy" %in% names(occurrences))) occurrences$recordedBy <- NA_character_
  list(occurrences = occurrences, metadata = metadata)
}

.sacha_spatial_local_occurrence_key <- function(occurrences) {
  keys <- rep(NA_character_, nrow(occurrences))
  if ("gbifID" %in% names(occurrences)) {
    gbif_id <- trimws(as.character(occurrences$gbifID))
    has_gbif_id <- !is.na(gbif_id) & nzchar(gbif_id)
    keys[has_gbif_id] <- paste0("gbif:", gbif_id[has_gbif_id])
  }
  if (all(c("occurrenceID", "datasetKey") %in% names(occurrences))) {
    occurrence_id <- trimws(as.character(occurrences$occurrenceID))
    dataset_key <- trimws(as.character(occurrences$datasetKey))
    use_occurrence_id <- is.na(keys) & !is.na(occurrence_id) &
      nzchar(occurrence_id) & !is.na(dataset_key) & nzchar(dataset_key)
    keys[use_occurrence_id] <- paste0(
      "occurrence:", dataset_key[use_occurrence_id], ":",
      occurrence_id[use_occurrence_id]
    )
  }
  keys
}

.sacha_spatial_load_local_available_occurrences <- function(species = NULL,
                                                            directory,
                                                            taxon_id = NULL,
                                                            date = NULL) {
  entries <- .sacha_spatial_local_export_entries(
    species, directory, taxon_id, date
  )
  taxon_label <- species %spatial||% sprintf("taxon_id %s", taxon_id)
  if (!length(entries)) {
    stop(sprintf("No local occurrence export was found for '%s' in '%s'. Create it first with `gbif_save_occurrences()`.",
                 taxon_label, .sacha_exports_root(directory)),
         call. = FALSE)
  }
  is_crop <- vapply(entries, function(entry) !is.null(entry$metadata$crop), logical(1))
  complete_entries <- entries[!is_crop]
  if (length(complete_entries)) {
    retrieved <- vapply(complete_entries, function(entry) {
      value <- .sacha_spatial_scalar(entry$metadata$retrieved_at)
      timestamp <- if (is.na(value)) NA_real_ else {
        suppressWarnings(as.numeric(as.POSIXct(value, tz = "UTC")))
      }
      file_time <- as.numeric(file.info(entry$data_path)$mtime)
      if (is.finite(timestamp)) timestamp else file_time
    }, numeric(1))
    if (all(!is.finite(retrieved))) retrieved <- seq_along(complete_entries)
    retrieved[!is.finite(retrieved)] <- -Inf
    selected <- complete_entries[[which.max(retrieved)]]
    loaded <- .sacha_spatial_read_local_occurrence_export(
      selected$data_path, selected$metadata_path, selected$metadata
    )
    if (is.null(species)) {
      species <- .sacha_spatial_validate_species(loaded$metadata$species)
    }
    loaded$metadata$local_coverage <- list(
      type = "full_extent_export",
      export_count = 1L,
      record_count = nrow(loaded$occurrences)
    )
    return(c(
      loaded,
      list(directory = dirname(selected$data_path),
           coverage = loaded$metadata$local_coverage)
    ))
  }

  taxon_keys <- unique(Filter(Negate(is.na), lapply(entries, function(entry) {
    .sacha_spatial_scalar(entry$metadata$query$taxonKey, default = NA_character_)
  })))
  if (length(taxon_keys) > 1L) {
    stop(sprintf(
      "Local cropped exports for '%s' resolve to different COL XR taxon keys and cannot be combined.",
      taxon_label
    ), call. = FALSE)
  }
  loaded <- lapply(entries, function(entry) {
    .sacha_spatial_read_local_occurrence_export(
      entry$data_path, entry$metadata_path, entry$metadata
    )
  })
  if (!is.null(date)) {
    loaded <- lapply(loaded, function(export) {
      export$occurrences <- .sacha_spatial_filter_local_dataset_date(
        export$occurrences, export$metadata, date
      )
      export
    })
  }
  columns <- unique(unlist(lapply(loaded, function(export) {
    names(export$occurrences)
  }), use.names = FALSE))
  data <- lapply(loaded, function(export) {
    occurrences <- export$occurrences
    missing_columns <- setdiff(columns, names(occurrences))
    for (column in missing_columns) occurrences[[column]] <- NA
    occurrences[, columns, drop = FALSE]
  })
  occurrences <- do.call(rbind, data)
  record_count_before_deduplication <- nrow(occurrences)
  record_keys <- .sacha_spatial_local_occurrence_key(occurrences)
  duplicated_records <- !is.na(record_keys) & duplicated(record_keys)
  occurrences <- occurrences[!duplicated_records, , drop = FALSE]
  rownames(occurrences) <- NULL
  metadata <- loaded[[1L]]$metadata
  retrieved_at_values <- vapply(loaded, function(export) {
    .sacha_spatial_scalar(export$metadata$retrieved_at)
  }, character(1))
  retrieved_days <- vapply(retrieved_at_values, function(value) {
    retrieved_date <- .sacha_spatial_map_style_retrieval_date(value)
    if (is.na(retrieved_date)) NA_real_ else as.numeric(retrieved_date)
  }, numeric(1))
  caption_retrieved_at <- if (any(is.finite(retrieved_days))) {
    retrieved_at_values[[which.max(retrieved_days)]]
  } else {
    NULL
  }
  metadata$record_count <- nrow(occurrences)
  metadata$local_coverage <- list(
    type = if (length(loaded) == 1L) {
      "single_cropped_export"
    } else {
      "union_of_cropped_exports"
    },
    export_count = length(loaded),
    record_count_before_deduplication = record_count_before_deduplication,
    deduplicated_record_count = nrow(occurrences),
    source_exports = vapply(entries, function(entry) entry$data_path, character(1))
  )
  if (!is.null(date)) {
    dataset_keys <- unique(unlist(lapply(loaded, function(export) {
      export$metadata$dataset_filter$datasetKeys
    }), use.names = FALSE))
    metadata$dataset_filter <- list(
      createdDate = date,
      datasetCount = length(dataset_keys),
      datasetKeys = dataset_keys
    )
  }
  list(
    occurrences = occurrences,
    metadata = metadata,
    directory = dirname(entries[[1L]]$data_path),
    coverage = metadata$local_coverage,
    caption_retrieved_at = caption_retrieved_at
  )
}

.sacha_spatial_load_local_occurrences <- function(species = NULL, directory,
                                                  crop_place = NULL,
                                                  crop_only = FALSE,
                                                  taxon_id = NULL,
                                                  prefer_available = FALSE,
                                                  date = NULL) {
  input <- .sacha_spatial_taxon_input(species, taxon_id)
  species <- input$species
  taxon_id <- input$taxon_id
  if (isTRUE(prefer_available)) {
    return(.sacha_spatial_load_local_available_occurrences(
      species, directory, taxon_id, date
    ))
  }
  taxon_directories <- .sacha_spatial_local_taxon_directories(
    species, directory, taxon_id
  )
  taxon_label <- species %spatial||% sprintf("taxon_id %s", taxon_id)
  if (length(taxon_directories) != 1L) {
    stop(sprintf("Could not identify one local occurrence export for '%s' in '%s'.",
                 taxon_label, .sacha_exports_root(directory)), call. = FALSE)
  }
  target <- taxon_directories
  if (isTRUE(crop_only) && is.null(crop_place)) {
    crop_directories <- list.dirs(target, recursive = FALSE, full.names = TRUE)
    crop_directories <- crop_directories[file.exists(file.path(crop_directories, "occurrences.gpkg"))]
    if (length(crop_directories) != 1L) {
      stop(sprintf("Specify `crop_vector`, `crop_area`, or `crop_box` to identify one local cropped export for '%s'.", taxon_label),
           call. = FALSE)
    }
    target <- crop_directories
  } else if (!is.null(crop_place)) {
    target <- file.path(target, .sacha_spatial_path_component(crop_place))
  }
  .sacha_spatial_require(c("sf", "jsonlite"))
  data_path <- file.path(target, "occurrences.gpkg")
  metadata_path <- file.path(target, "metadata.json")
  if (!file.exists(data_path) || !file.exists(metadata_path)) {
    stop(sprintf("No local occurrence export was found for '%s' in '%s'. Create it first with `gbif_save_occurrences()`.",
                 taxon_label, target),
         call. = FALSE)
  }
  occurrences <- tryCatch(sf::st_read(data_path, quiet = TRUE), error = function(e) {
    stop(sprintf("Could not read local occurrences from '%s': %s.", data_path, conditionMessage(e)),
         call. = FALSE)
  })
  metadata <- tryCatch(jsonlite::read_json(metadata_path, simplifyVector = FALSE), error = function(e) {
    stop(sprintf("Could not read local metadata from '%s': %s.", metadata_path, conditionMessage(e)),
         call. = FALSE)
  })
  required <- c("decimalLongitude", "decimalLatitude", "scientificName", "year", "stateProvince")
  if (!is.data.frame(occurrences) || !all(required %in% names(occurrences))) {
    stop(sprintf("Local occurrence export in '%s' is not compatible with the map functions.", target),
         call. = FALSE)
  }
  occurrences <- as.data.frame(sf::st_drop_geometry(occurrences))
  if (!("recordedBy" %in% names(occurrences))) occurrences$recordedBy <- NA_character_
  if (is.null(species)) species <- .sacha_spatial_validate_species(metadata$species)
  list(occurrences = occurrences, metadata = metadata, directory = target)
}

.sacha_spatial_stop_gbif_error <- function(result, species, language) {
  if (identical(result$error_type, "taxonomy")) {
    stop(result$message, call. = FALSE)
  }
  stop(sprintf("%s %s (%s)",
               .sacha_spatial_label("communication_error", language), species,
               result$message %spatial||% "unknown error"),
       call. = FALSE)
}

.sacha_spatial_filter_occurrences <- function(occurrences, boundary) {
  .sacha_spatial_require("sf")
  if (!nrow(occurrences)) return(occurrences)
  points <- sf::st_as_sf(occurrences,
                         coords = c("decimalLongitude", "decimalLatitude"),
                         crs = 4326, remove = FALSE)
  inside <- lengths(sf::st_intersects(points, sf::st_union(boundary))) > 0L
  occurrences[inside, , drop = FALSE]
}

.sacha_spatial_crop_box_parse <- function(crop_box) {
  if (is.null(crop_box)) return(NULL)
  values <- unlist(crop_box, use.names = FALSE)
  if (!length(values) %in% c(4L, 5L) || (length(values) == 5L && is.numeric(values))) {
    stop("`crop_box` must be `c(xmin, ymin, xmax, ymax)`, optionally followed by a character label, e.g. `c(-82.5, -5.5, -75.0, 1.6, \"Ecuador continental\")`.",
         call. = FALSE)
  }
  extent <- suppressWarnings(as.numeric(values[1:4]))
  if (anyNA(extent) || !all(is.finite(extent))) {
    stop("The first four elements of `crop_box` must be finite numeric coordinates: xmin, ymin, xmax, ymax.",
         call. = FALSE)
  }
  names(extent) <- c("xmin", "ymin", "xmax", "ymax")
  if (extent[["xmin"]] >= extent[["xmax"]] || extent[["ymin"]] >= extent[["ymax"]]) {
    stop("`crop_box` must satisfy xmin < xmax and ymin < ymax.", call. = FALSE)
  }
  if (extent[["xmin"]] < -180 || extent[["xmax"]] > 180 ||
      extent[["ymin"]] < -90 || extent[["ymax"]] > 90) {
    stop("`crop_box` coordinates must be WGS 84 decimal degrees: longitude in [-180, 180] and latitude in [-90, 90].",
         call. = FALSE)
  }
  label <- NULL
  if (length(values) == 5L) {
    label <- trimws(as.character(values[5]))
    if (is.na(label) || !nzchar(label)) label <- NULL
  }
  list(extent = extent, label = label)
}

.sacha_spatial_crop_box_label <- function(extent) {
  format_coordinate <- function(value, positive, negative) {
    paste0(formatC(abs(value), format = "fg", digits = 6), "\u00b0",
           if (value < 0) negative else positive)
  }
  paste(
    format_coordinate(extent[["xmin"]], "E", "W"),
    format_coordinate(extent[["ymin"]], "N", "S"),
    format_coordinate(extent[["xmax"]], "E", "W"),
    format_coordinate(extent[["ymax"]], "N", "S"),
    sep = ", "
  )
}

.sacha_spatial_apply_crop_box <- function(crop_data, crop_box) {
  .sacha_spatial_require("sf")
  extent <- crop_box$extent
  window <- sf::st_as_sfc(sf::st_bbox(extent, crs = sf::st_crs(4326)))
  box_record <- c(as.list(extent), list(label = crop_box$label))
  if (is.null(crop_data)) {
    return(list(
      boundary = NULL,
      region = sf::st_sf(geometry = window),
      bbox = sf::st_bbox(window),
      place = crop_box$label %spatial||% .sacha_spatial_crop_box_label(extent),
      crop_vector = NULL,
      crop_area = NULL,
      country_code = NULL,
      crop_box = box_record,
      window = extent
    ))
  }
  place_attribute <- attr(crop_data$boundary, "sacha_place")
  planar_boundary <- sf::st_make_valid(
    sf::st_set_crs(sf::st_geometry(crop_data$boundary), NA)
  )
  planar_window <- sf::st_set_crs(window, NA)
  clipped <- sf::st_intersection(planar_boundary, planar_window)
  if (any(sf::st_geometry_type(clipped) == "GEOMETRYCOLLECTION")) {
    clipped <- sf::st_collection_extract(clipped, "POLYGON")
  }
  clipped <- clipped[!sf::st_is_empty(clipped)]
  if (!length(clipped)) {
    stop("`crop_box` does not overlap the geographic boundary defined by `crop_vector` or `crop_area`.",
         call. = FALSE)
  }
  clipped <- sf::st_sf(geometry = sf::st_set_crs(clipped, 4326))
  if (!is.null(crop_box$label)) crop_data$place <- crop_box$label
  if (!is.null(place_attribute)) attr(clipped, "sacha_place") <- crop_data$place
  crop_data$boundary <- clipped
  crop_data$region <- clipped
  crop_data$bbox <- sf::st_bbox(window)
  crop_data$crop_box <- box_record
  crop_data$window <- extent
  crop_data
}

.sacha_spatial_filter_crop <- function(occurrences, crop_data) {
  extent <- crop_data$window
  if (is.null(extent)) {
    return(.sacha_spatial_filter_occurrences(occurrences, crop_data$boundary))
  }
  if (!nrow(occurrences)) return(occurrences)
  .sacha_spatial_require("sf")
  inside <- occurrences$decimalLongitude >= extent[["xmin"]] &
    occurrences$decimalLongitude <= extent[["xmax"]] &
    occurrences$decimalLatitude >= extent[["ymin"]] &
    occurrences$decimalLatitude <= extent[["ymax"]]
  occurrences <- occurrences[!is.na(inside) & inside, , drop = FALSE]
  if (is.null(crop_data$boundary) || !nrow(occurrences)) return(occurrences)
  points <- sf::st_as_sf(occurrences,
                         coords = c("decimalLongitude", "decimalLatitude"),
                         crs = sf::NA_crs_, remove = FALSE)
  planar_region <- sf::st_union(sf::st_set_crs(sf::st_geometry(crop_data$boundary), NA))
  inside <- lengths(sf::st_intersects(points, planar_region)) > 0L
  occurrences[inside, , drop = FALSE]
}

.sacha_spatial_gbif_orient_ring <- function(ring, counterclockwise) {
  coordinates <- as.matrix(ring)
  if (nrow(coordinates) < 4L || ncol(coordinates) < 2L) return(coordinates)
  xy <- coordinates[, 1:2, drop = FALSE]
  signed_area <- sum(
    xy[-nrow(xy), 1L] * xy[-1L, 2L] -
      xy[-1L, 1L] * xy[-nrow(xy), 2L]
  )
  if (is.finite(signed_area) && signed_area != 0 &&
      ((signed_area > 0) != isTRUE(counterclockwise))) {
    coordinates <- coordinates[nrow(coordinates):1L, , drop = FALSE]
  }
  coordinates
}

.sacha_spatial_gbif_orient_polygon <- function(rings) {
  lapply(seq_along(rings), function(index) {
    .sacha_spatial_gbif_orient_ring(rings[[index]], counterclockwise = index == 1L)
  })
}

.sacha_spatial_gbif_orient_geometry <- function(geometry) {
  geometry_class <- class(geometry)
  if ("POLYGON" %in% geometry_class) {
    return(structure(
      .sacha_spatial_gbif_orient_polygon(unclass(geometry)),
      class = geometry_class
    ))
  }
  if ("MULTIPOLYGON" %in% geometry_class) {
    polygons <- lapply(unclass(geometry), .sacha_spatial_gbif_orient_polygon)
    return(structure(polygons, class = geometry_class))
  }
  if ("GEOMETRYCOLLECTION" %in% geometry_class) {
    geometries <- lapply(unclass(geometry), .sacha_spatial_gbif_orient_geometry)
    return(structure(geometries, class = geometry_class))
  }
  geometry
}

.sacha_spatial_gbif_bbox_polygon <- function(geometry) {
  extent <- sf::st_bbox(geometry)
  values <- as.numeric(extent)
  names(values) <- names(extent)
  epsilon <- 1e-6
  if (values[["xmin"]] >= values[["xmax"]]) {
    values[["xmin"]] <- max(-180, values[["xmin"]] - epsilon)
    values[["xmax"]] <- min(180, values[["xmax"]] + epsilon)
  }
  if (values[["ymin"]] >= values[["ymax"]]) {
    values[["ymin"]] <- max(-90, values[["ymin"]] - epsilon)
    values[["ymax"]] <- min(90, values[["ymax"]] + epsilon)
  }
  sf::st_as_sfc(sf::st_bbox(values, crs = sf::st_crs(4326)))
}

.sacha_spatial_gbif_geometry_wkt <- function(crop_data, max_chars = 8000L) {
  .sacha_spatial_require("sf")
  region <- crop_data$region %spatial||% crop_data$boundary
  if (is.null(region) || !inherits(region, "sf")) return(NULL)
  geometry <- sf::st_geometry(region)
  if (is.na(sf::st_crs(geometry))) {
    sf::st_crs(geometry) <- 4326
  } else if (!isTRUE(sf::st_crs(geometry) == sf::st_crs(4326))) {
    geometry <- sf::st_transform(geometry, 4326)
  }
  geometry <- sf::st_make_valid(sf::st_union(geometry))
  geometry_types <- unique(as.character(sf::st_geometry_type(geometry)))
  exact_polygon <- length(geometry_types) > 0L &&
    all(geometry_types %in% c("POLYGON", "MULTIPOLYGON"))
  if (!exact_polygon) {
    geometry <- .sacha_spatial_gbif_bbox_polygon(geometry)
  }
  geometry <- sf::st_sfc(
    lapply(geometry, .sacha_spatial_gbif_orient_geometry),
    crs = sf::st_crs(4326)
  )
  wkt <- as.character(sf::st_as_text(geometry, digits = 15L)[1L])
  if (!nzchar(wkt) || is.na(wkt)) return(NULL)
  is_bbox <- !exact_polygon
  if (nchar(wkt, type = "bytes") > max_chars) {
    geometry <- .sacha_spatial_gbif_bbox_polygon(geometry)
    geometry <- sf::st_sfc(
      lapply(geometry, .sacha_spatial_gbif_orient_geometry),
      crs = sf::st_crs(4326)
    )
    wkt <- as.character(sf::st_as_text(geometry, digits = 15L)[1L])
    is_bbox <- TRUE
  }
  list(wkt = wkt, is_bbox = is_bbox)
}

.sacha_spatial_gbif_crop_query <- function(crop_data = NULL) {
  if (is.null(crop_data)) return(NULL)
  country <- crop_data$country_code %spatial||% NULL
  if (!is.null(country) &&
      (!is.character(country) || length(country) != 1L || is.na(country) ||
       !grepl("^[A-Z]{2}$", country))) {
    country <- NULL
  }
  geometry <- NULL
  if (is.null(country) || !is.null(crop_data$window)) {
    geometry <- .sacha_spatial_gbif_geometry_wkt(crop_data)
  }
  list(
    country = country,
    geometry = geometry$wkt %spatial||% NULL,
    geometry_is_bbox = isTRUE(geometry$is_bbox)
  )
}

.sacha_spatial_gbif_search_page <- function(taxon, limit, start = 0L,
                                            dataset_keys = NULL,
                                            spatial_query = NULL) {
  arguments <- list(
    taxonKey = taxon$key,
    checklistKey = taxon$checklist_key,
    hasCoordinate = TRUE,
    limit = limit,
    start = start
  )
  if (!is.null(dataset_keys) && length(dataset_keys)) {
    arguments$datasetKey <- paste(dataset_keys, collapse = ";")
  }
  if (!is.null(spatial_query$country)) arguments$country <- spatial_query$country
  if (!is.null(spatial_query$geometry)) {
    arguments$geometry <- spatial_query$geometry
    arguments$geom_big <- "asis"
  }
  .sacha_spatial_with_retry(
    function() do.call(rgbif::occ_search, arguments),
    max_tries = .sacha_spatial_config$http_max_tries
  )
}

.sacha_spatial_gbif_filtered_search <- function(taxon, limit, dataset_keys,
                                                spatial_query, crop_data) {
  dataset_groups <- if (is.null(dataset_keys)) {
    list(NULL)
  } else {
    split(dataset_keys, ceiling(seq_along(dataset_keys) / 50L))
  }
  results <- list()
  selected_count <- 0L
  for (dataset_group in dataset_groups) {
    start <- 0L
    repeat {
      remaining <- if (is.null(limit)) 300L else limit - selected_count
      if (remaining <= 0L) break
      page_size <- min(300L, remaining, 100000L - start)
      if (page_size <= 0L) break
      page <- .sacha_spatial_gbif_search_page(
        taxon, page_size, start, dataset_group, spatial_query
      )
      page_data <- page$data %spatial||% data.frame()
      if (nrow(page_data)) {
        for (column in c("decimalLongitude", "decimalLatitude")) {
          if (!(column %in% names(page_data))) page_data[[column]] <- NA_real_
          page_data[[column]] <- suppressWarnings(as.numeric(page_data[[column]]))
        }
        valid <- is.finite(page_data$decimalLongitude) &
          is.finite(page_data$decimalLatitude) &
          page_data$decimalLongitude >= -180 & page_data$decimalLongitude <= 180 &
          page_data$decimalLatitude >= -90 & page_data$decimalLatitude <= 90
        page_data <- page_data[valid, , drop = FALSE]
        if (nrow(page_data) && !is.null(crop_data)) {
          page_data <- .sacha_spatial_filter_crop(page_data, crop_data)
        }
        if (nrow(page_data)) {
          results[[length(results) + 1L]] <- page_data
          selected_count <- selected_count + nrow(page_data)
        }
      }
      total <- suppressWarnings(as.integer(page$meta$count)[1L])
      retrieved <- nrow(page$data %spatial||% data.frame())
      if (!retrieved || retrieved < page_size || start + retrieved >= 100000L ||
          (!is.na(total) && start + retrieved >= total)) {
        break
      }
      start <- start + retrieved
    }
    if (!is.null(limit) && selected_count >= limit) break
  }
  if (!length(results)) return(list(data = data.frame()))
  columns <- unique(unlist(lapply(results, names), use.names = FALSE))
  results <- lapply(results, function(result) {
    missing_columns <- setdiff(columns, names(result))
    for (column in missing_columns) result[[column]] <- NA
    result[, columns, drop = FALSE]
  })
  data <- do.call(rbind, results)
  if (!is.null(limit)) data <- utils::head(data, limit)
  rownames(data) <- NULL
  list(data = data)
}

.sacha_spatial_map_mode <- function(mode) {
  match.arg(mode, c("interactive", "static"))
}

.sacha_spatial_resolve_report_mode <- function(mode = NULL) {
  if (!is.null(mode)) {
    return(.sacha_spatial_map_mode(mode))
  }
  if (requireNamespace("knitr", quietly = TRUE)) {
    to <- tryCatch(knitr::pandoc_to(), error = function(e) NULL)
    if (!is.null(to)) {
      html_formats <- c("html", "html4", "html5", "slidy", "revealjs", "s5", "slideous", "dzslides")
      return(if (to %in% html_formats) "interactive" else "static")
    }
  }
  "interactive"
}

.sacha_spatial_export_image <- function(occurrences, species, output_directory,
                                        colour, zoom, dpi, overwrite,
                                        boundary = NULL, bbox = NULL,
                                        file_prefix = "gbif_map", legend_place = NULL,
                                        static_legend_position = "bottomleft") {
  .sacha_spatial_require("ggplot2")
  static_map <- .sacha_spatial_static_map(
    occurrences, colour, zoom, boundary = boundary, bbox = bbox,
    legend_place = legend_place,
    static_legend_position = static_legend_position
  )
  export_size <- .sacha_spatial_map_style_export_size(attr(static_map, "sacha_map_frame"))
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  path <- .sacha_spatial_unique_path(
    .sacha_spatial_path(species, "png", output_directory, prefix = file_prefix), overwrite
  )
  ggplot2::ggsave(path, plot = static_map, width = export_size[["width"]],
                  height = export_size[["height"]], units = "in", dpi = dpi)
  c(image = path)
}

.sacha_spatial_crop_vector_place_name <- function(crop_vector) {
  name <- tools::file_path_sans_ext(basename(crop_vector))
  name <- gsub("[_\\-]+", " ", name)
  trimws(name)
}

.sacha_spatial_crop_vector_boundary <- function(crop_vector) {
  .sacha_spatial_require("sf")
  if (!is.character(crop_vector) || length(crop_vector) != 1L || is.na(crop_vector) ||
      !nzchar(trimws(crop_vector))) {
    stop("`crop_vector` must be one path to a .kml or .shp file.", call. = FALSE)
  }
  if (!file.exists(crop_vector)) {
    stop(sprintf("`crop_vector` file was not found: %s", crop_vector), call. = FALSE)
  }
  extension <- tolower(tools::file_ext(crop_vector))
  if (!(extension %in% c("kml", "shp"))) {
    stop("`crop_vector` must point to a .kml or .shp file.", call. = FALSE)
  }

  boundary <- tryCatch(
    sf::st_read(crop_vector, quiet = TRUE),
    error = function(e) {
      stop(sprintf("Could not read crop vector '%s': %s.", crop_vector, conditionMessage(e)),
           call. = FALSE)
    }
  )
  if (!nrow(boundary)) {
    stop(sprintf("Crop vector '%s' does not contain any geometry.", crop_vector), call. = FALSE)
  }
  if (is.na(sf::st_crs(boundary))) {
    warning_message <- if (identical(extension, "kml")) {
      sprintf(.sacha_spatial_label("warn_kml_no_crs"), crop_vector)
    } else {
      sprintf("Crop vector '%s' has no coordinate reference system; its coordinates are treated as WGS 84 (EPSG:4326).",
              crop_vector)
    }
    warning(warning_message, call. = FALSE)
    sf::st_crs(boundary) <- 4326
  } else {
    boundary <- sf::st_transform(boundary, 4326)
  }

  extent <- sf::st_bbox(boundary)
  list(
    boundary = boundary,
    bbox = extent,
    place = .sacha_spatial_crop_vector_place_name(crop_vector)
  )
}

# Punto fijo de salida para Quarto/knitr. Quarto procesa `fig-cap` antes de
# evaluar el chunk, por lo que un caption dinámico no puede inyectarse de
# forma fiable mediante `opts_current`. Las figuras estáticas se escriben como
# un div QMD de referencia cruzada; así Quarto crea el caption, la lista de
# figuras y el caption corto de LaTeX a partir de `fig-scap`.
.sacha_spatial_qmd_escape_attribute <- function(x) {
  gsub('"', "&quot;", as.character(x), fixed = TRUE)
}

.sacha_spatial_relative_path <- function(path, directory) {
  path_parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  directory_parts <- strsplit(directory, "/", fixed = TRUE)[[1L]]
  compare <- if (identical(.Platform$OS.type, "windows")) tolower else identity
  common <- 0L
  max_common <- min(length(path_parts), length(directory_parts))
  while (common < max_common &&
         identical(compare(path_parts[[common + 1L]]),
                   compare(directory_parts[[common + 1L]]))) {
    common <- common + 1L
  }
  if (common == 0L) return(path)
  path_remainder <- if (common < length(path_parts)) {
    path_parts[seq.int(common + 1L, length(path_parts))]
  } else {
    character()
  }
  relative_parts <- c(
    rep("..", length(directory_parts) - common),
    path_remainder
  )
  if (length(relative_parts) == 0L) "." else paste(relative_parts, collapse = "/")
}

.sacha_spatial_emit_static_qmd_figure <- function(map, report) {
  path_seed <- knitr::fig_path(suffix = "png")
  base_dir <- dirname(path_seed)

  # Sin renombrar la carpeta: dejamos que Quarto use su carpeta propia por
  # formato (figure-html, figure-pdf, figure-docx, figure-epub, ...), que es
  # justamente lo que le permite renderizar varios formatos a la vez sin que
  # uno borre las imágenes que otro todavía necesita.
  path <- file.path(base_dir, paste0(report$id, ".png"))
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)

  ggplot2::ggsave(
    filename = path, plot = map, width = report$width, height = report$height,
    units = "in", dpi = .sacha_spatial_map_style$defaults$dpi
  )

  target <- tryCatch(knitr::pandoc_to(), error = function(error) NULL)
  image_path <- if (identical(target, "typst")) {
    input_path <- tryCatch(knitr::current_input(), error = function(error) "")
    if (length(input_path) == 1L && !is.na(input_path) && nzchar(input_path)) {
      input_path <- normalizePath(input_path, winslash = "/", mustWork = TRUE)
      typst_directory <- dirname(input_path)
      source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      destination_path <- file.path(typst_directory, basename(path))
      normalized_destination <- normalizePath(
        destination_path, winslash = "/", mustWork = FALSE
      )
      if (!identical(source_path, normalized_destination) &&
          !isTRUE(file.copy(source_path, destination_path, overwrite = TRUE))) {
        stop(
          "Could not place the GBIF map where the Typst document resolves chapter images.",
          call. = FALSE
        )
      }
    }
    basename(path)
  } else if (identical(target, "docx") || identical(target, "epub")) {
    input_path <- tryCatch(knitr::current_input(), error = function(error) "")
    input_directory <- if (
      length(input_path) == 1L && !is.na(input_path) && nzchar(input_path)
    ) {
      dirname(normalizePath(input_path, winslash = "/", mustWork = TRUE))
    } else {
      getwd()
    }
    absolute_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    input_directory <- normalizePath(
      input_directory, winslash = "/", mustWork = TRUE
    )
    if (identical(target, "docx")) {
      .sacha_spatial_relative_path(absolute_path, input_directory)
    } else {
      tryCatch(
        gsub(
          "\\\\", "/",
          xfun::relative_path(absolute_path, input_directory, error = TRUE)
        ),
        error = function(error) gsub("\\\\", "/", path)
      )
    }
  } else {
    gsub("\\\\", "/", path)
  }

  markdown <- paste0(
    "![", report$full, "](<", image_path, ">){#", report$id,
    " fig-scap=\"", .sacha_spatial_qmd_escape_attribute(report$short), "\"}\n"
  )

  knitr::asis_output(markdown)
}

.sacha_spatial_emit_interactive_qmd_figure <- function(map, report) {
  widget_output <- knitr::knit_print(map)
  widget_markdown <- paste(as.character(widget_output), collapse = "\n")
  markdown <- paste0(
    "::: {#", report$id, "}\n\n",
    widget_markdown, "\n\n",
    report$full, "\n\n:::\n"
  )
  knitr::asis_output(markdown, meta = attr(widget_output, "knit_meta"))
}

.sacha_spatial_emit_report_map <- function(map, report) {
  attr(map, "sacha_caption_full") <- report$full
  attr(map, "sacha_caption_short") <- report$short
  if (!requireNamespace("knitr", quietly = TRUE)) return(map)

  target <- tryCatch(knitr::pandoc_to(), error = function(e) NULL)
  if (is.null(target) || !nzchar(target)) return(map)
  if (inherits(map, "ggplot")) {
    return(.sacha_spatial_emit_static_qmd_figure(map, report))
  }
  html_formats <- c("html", "html4", "html5", "slidy", "revealjs", "s5", "slideous", "dzslides")
  if (inherits(map, "htmlwidget") && target %in% html_formats) {
    return(.sacha_spatial_emit_interactive_qmd_figure(map, report))
  }
  .sacha_spatial_map_style_render_caption(map, report)
}

.sacha_spatial_emit_md_map <- function(map, variant, mode, frame, species,
                                       language, place, fig_width = NULL,
                                       fig_height = NULL) {
  report <- .sacha_spatial_map_style_report(
    variant = variant, mode = mode, frame = frame, species = species,
    language = language, place = place,
    retrieved_at = attr(map, "sacha_retrieved_at", exact = TRUE)
  )
  if (identical(mode, "static")) {
    if (!is.null(fig_width)) report$width <- fig_width
    if (!is.null(fig_height)) report$height <- fig_height
  }
  .sacha_spatial_emit_report_map(map, report)
}

#' Generate a full-extent GBIF occurrence map
#'
#' `gbif_map()` creates a standalone map of the georeferenced occurrences of a
#' species, and `gbif_map_md()` creates the same map for a markdown-formatted
#' report (Quarto or R Markdown).
#'
#' With `source = "web"` (default), supply exactly one of `species` or
#' `taxon_id`. GBIF resolves the binomial as an exact, species-level name or
#' validates the COL XR usage key as an exact species-level taxon. The
#' resulting taxon key and the same
#' checklist key are used to retrieve records with coordinates. This prevents
#' approximate name matches and keeps records tied to one accepted GBIF
#' taxonomic concept. With
#' `source = "local"`, records are read from an export previously written by
#' `save_occurrences = TRUE` or by [gbif_save_occurrences()], and GBIF is not
#' contacted. Local exports are stored under
#' `sacha_exports/Family_Genus_species` inside `directory`.
#'
#' Setting `save_occurrences = TRUE` stores up to `limit` georeferenced
#' records as a GeoPackage in WGS 84 (EPSG:4326), together with a JSON file
#' describing the exact taxon match, query, date filter, retrieval date,
#' coordinate reference system, and record count. By default,
#' `generate_doi = FALSE` uses the fast GBIF occurrence search and does not wait
#' for a registered download or DOI. Set `generate_doi = TRUE` to create and wait
#' for a registered download; its DOI is recorded in `metadata.json` and in the
#' map's `sacha_gbif_download` attribute. This requires `GBIF_USER`,
#' `GBIF_PWD`, and `GBIF_EMAIL` credentials. With `source = "local"`,
#' `generate_doi` does not contact GBIF and no new export is written.
#'
#' Configure the GBIF account credentials in the current R session with
#' `Sys.setenv(GBIF_USER = "your_username", GBIF_PWD = "your_password",
#' GBIF_EMAIL = "your_account_email")`. To keep them available in future
#' sessions, add the same three variables to your personal `~/.Renviron` file
#' and restart R. Do not share or publish that file.
#'
#' The default `limit` of 50,000 is intended to keep routine queries
#' manageable. For species with larger occurrence sets, such as
#' `Amaranthus hybridus`, reuse a local export or restrict the records with
#' `date`. For a spatially filtered retrieval, use [gbif_map_crop()] or
#' [gbif_save_occurrences()]: country areas and geometric boundaries are sent
#' to GBIF before records are returned.
#'
#' Interactive maps show the scientific name, year, locality, and observer
#' (`recordedBy`) reported by GBIF for each occurrence. Missing values are
#' displayed as unavailable.
#'
#' Setting `export_image = TRUE` writes the static map as a PNG file, whatever
#' the value of `mode`, to the same directory as the saved occurrences
#' (`sacha_exports/Family_Genus_species`).
#'
#' If GBIF cannot be reached (a transient network or server problem), an
#' error explicitly worded as a communication failure is raised. It is
#' distinct from the error raised when GBIF responds successfully but holds no
#' georeferenced records for the species.
#'
#' `date` selects occurrence datasets by their GBIF registry creation date
#' (`createdDate`) and then filters records to those datasets; it is not the
#' date when an individual occurrence was observed. This registry lookup is
#' separate from the occurrence search. GBIF uses the Catalogue of Life
#' Extended Release (COL XR) as the taxonomic reference for occurrence
#' downloads. When `generate_doi = TRUE`, the export's `metadata.json` records
#' its official download DOI and link. By downloading the data, you acknowledge GBIF's
#' [data-use agreement](https://www.gbif.org/terms/data-user). When citing or
#' referring to registered downloads (`generate_doi = TRUE`), follow GBIF's
#' [citation guidelines](https://www.gbif.org/citation-guidelines#occDataDownload)
#' and include the DOI assigned to that specific download in every publication.
#'
#' In Markdown reports, `gbif_map_md()` assigns a stable Quarto figure ID from
#' the map type and species name. For `"Bidens andicola"`, the full-extent map
#' is `fig-gbif-map-bidens_andicola`. Use that ID in a cross-reference
#' such as `@fig-gbif-map-bidens_andicola`. The caption identifies the
#' accepted species and the date on which the map was produced. In Quarto or
#' R Markdown, use `results: asis` for the chunk that calls an `_md` function so
#' that the generated figure markup is interpreted by the document renderer.
#'
#' @param species A binomial scientific name in the form `"Genus species"`,
#'   e.g. `"Bidens andicola"`, `"Cinchona officinalis"`, or `"Puma
#'   concolor"`. GBIF must resolve it as an exact species-level name; spelling
#'   variants and higher-rank matches are rejected. Supply exactly one of
#'   `species` or `taxon_id`.
#' @param taxon_id A COL XR alphanumeric species usage key, such as `"Q2M4"`.
#'   GBIF resolves the key against the configured checklist and verifies that
#'   it identifies one species-level taxon. Supply exactly one of `taxon_id`
#'   or `species`.
#' @param limit Maximum number of georeferenced occurrences used to draw and
#'   save, an integer between `1` and `100000`, e.g. `100`, `500`, or `2000`.
#'   With a local source, the first `limit` records of the saved export are
#'   used. Defaults to `50000`.
#' @param mode Map type: `"interactive"` (default; a Leaflet widget) or
#'   `"static"` (a ggplot2 map). In `gbif_map_md()`, `NULL` selects an
#'   interactive map for HTML output and a static map for other formats.
#' @param save_occurrences Logical. If `TRUE`, up to `limit` georeferenced
#'   records are saved as `occurrences.gpkg` under
#'   `sacha_exports/Family_Genus_species`, with `metadata.json` describing the
#'   query. `generate_doi = TRUE` adds the official DOI for a registered GBIF
#'   download. Defaults to `FALSE`.
#' @param export_image Logical. If `TRUE`, the static map is written as a PNG
#'   file to `sacha_exports/Family_Genus_species`. Defaults to `FALSE`.
#' @param directory Base directory in which `sacha_exports` is created or,
#'   for a local source, searched. Occurrences and exported images are saved
#'   under `sacha_exports/Family_Genus_species`. Defaults to [getwd()].
#' @param colour Colour of the occurrence points as a hexadecimal code with 3,
#'   4, 6, or 8 digits, e.g. `"#1B9E77"`. When `NULL`, the package default
#'   colour is used.
#' @param static_legend_position Position of the static legend as one composed
#'   unit with its scale bar and north arrow. Applies to static maps and PNGs
#'   written with `export_image = TRUE`, including when `mode` is
#'   `"interactive"`: `"bottomleft"` (default), `"bottom"`,
#'   `"bottomright"`, `"right"`, `"topright"`, `"top"`, `"topleft"`, or
#'   `"left"`. It does not change the interactive map widget.
#' @param zoom_static Logical. Should static maps be framed on the extent of
#'   the occurrences? It applies to static maps and exported images. When
#'   `NULL`, the package default is used.
#' @param dpi Resolution of the exported PNG in dots per inch. When `NULL`,
#'   the package default resolution is used.
#' @param overwrite Logical. Should existing output files be replaced? If
#'   `FALSE`, an existing local occurrence export raises an error. Defaults to
#'   `TRUE`.
#' @param source Occurrence source: `"web"` (default) retrieves records from
#'   GBIF; `"local"` reads a saved export from `sacha_exports` under
#'   `directory`.
#' @param generate_doi Logical. If `TRUE` and `source = "web"`, create and wait
#'   for a registered GBIF download, retrieve its DOI, and use at most `limit`
#'   records. This requires GBIF account credentials. If `FALSE` (default),
#'   use the faster occurrence-search API without waiting for a DOI. For a
#'   local source, no GBIF request is made.
#' @param date Optional ISO date or range such as
#'   `"2020-01-01,2021-01-01"`. It filters GBIF occurrence datasets by their
#'   registry `createdDate`, then filters records by the matching dataset keys.
#'   It does not filter the date on which an occurrence was observed. Local
#'   exports can use only the same date filter recorded in their metadata.
#' @param fig_width,fig_height Dimensions in inches for static maps produced
#'   by `gbif_map_md()`. When `NULL`, the static map dimensions follow its
#'   aspect ratio. Interactive Leaflet maps use the widget and output
#'   container's default sizing.
#' @return A Leaflet widget or a ggplot2 object. Paths of the written files
#'   are stored in the `"sacha_files"` attribute, the plotted records in
#'   `"sacha_occurrences"`, and the species in `"sacha_species"`.
#'   `gbif_map_md()` returns the map formatted for the report output.
#' @examples
#' \donttest{
#' map <- gbif_map("Bidens andicola", limit = 100)
#' map_by_key <- gbif_map(taxon_id = "Q2M4", limit = 100)
#' map
#' }
#' \dontrun{
#' gbif_map("Bidens andicola", save_occurrences = TRUE, export_image = TRUE)
#' gbif_map("Bidens andicola", source = "local", mode = "static")
#'
#' gbif_map_md("Bidens andicola", source = "web", mode = "interactive")
#' gbif_map_md("Bidens andicola", source = "web", mode = "static",
#'             limit = 500, export_image = TRUE, save_occurrences = TRUE)
#' gbif_map_md("Bidens andicola", source = "local", mode = "static",
#'             limit = 500, directory = getwd(), export_image = TRUE,
#'             colour = "#1B9E77", zoom_static = TRUE, dpi = 300,
#'             overwrite = TRUE, fig_width = 8, fig_height = 6)
#' }
#' @export
gbif_map <- function(species = NULL,
                     limit = 50000L,
                     mode = c("interactive", "static"),
                     save_occurrences = FALSE,
                     export_image = FALSE,
                     directory = getwd(),
                     colour = NULL,
                     zoom_static = NULL,
                     dpi = NULL,
                     overwrite = TRUE,
                     source = c("web", "local"),
                     taxon_id = NULL,
                     generate_doi = FALSE,
                     date = NULL,
                     static_legend_position = "bottomleft") {
  .sacha_spatial_apply_taxon_arg_defaults(
    environment(), match.call(expand.dots = FALSE), names(formals(sys.function()))
  )
  input <- .sacha_spatial_taxon_input(species, taxon_id)
  species <- input$species
  taxon_id <- input$taxon_id
  language <- .sacha_spatial_language()
  limit <- .sacha_spatial_validate_limit(limit)
  generate_doi <- .sacha_spatial_validate_logical(generate_doi, "generate_doi")
  date <- .sacha_spatial_validate_date(date)
  map_inputs <- .sacha_spatial_map_style_inputs(colour, dpi, zoom_static)
  colour <- map_inputs$colour
  dpi <- map_inputs$dpi
  zoom_static <- map_inputs$zoom_static
  dpi <- .sacha_spatial_validate_limit(dpi)
  colour <- .sacha_spatial_validate_colour(colour)
  zoom_static <- .sacha_spatial_validate_logical(zoom_static, "zoom_static")
  overwrite <- .sacha_spatial_validate_logical(overwrite, "overwrite")
  save_occurrences <- .sacha_spatial_validate_logical(save_occurrences, "save_occurrences")
  export_image <- .sacha_spatial_validate_logical(export_image, "export_image")
  mode <- .sacha_spatial_map_mode(mode)
  source <- .sacha_spatial_source(source)

  local_species <- NULL
  complete_result <- NULL
  occ_result <- NULL
  if (identical(source, "web")) {
    resolved_taxon <- .sacha_spatial_resolve_taxon_input(species, taxon_id)
    species <- resolved_taxon$species
    if (isTRUE(generate_doi)) {
      complete_result <- .sacha_spatial_gbif_occurrence_download(
        resolved_taxon$taxon, date = date
      )
      occurrences <- utils::head(complete_result$occurrences, limit)
    } else {
      occ_result <- .sacha_spatial_gbif_occurrences(
        species, limit, date = date, taxon = resolved_taxon$taxon
      )
      if (identical(occ_result$status, "error")) {
        .sacha_spatial_stop_gbif_error(occ_result, species, language)
      }
      occurrences <- occ_result$occurrences
    }
  } else {
    local_species <- .sacha_spatial_load_local_occurrences(
      species, directory, taxon_id = taxon_id
    )
    species <- .sacha_spatial_validate_species(local_species$metadata$species)
    local_occurrences <- .sacha_spatial_filter_local_dataset_date(
      local_species$occurrences, local_species$metadata, date
    )
    occurrences <- utils::head(local_occurrences, limit)
  }
  if (!nrow(occurrences)) {
    stop(paste(.sacha_spatial_label("no_occurrences", language), species), call. = FALSE)
  }

  taxon_directory <- .sacha_spatial_taxon_directory(species, occurrences, directory)
  local_files <- character()
  if (isTRUE(save_occurrences)) {
    if (identical(source, "web")) {
      local_files <- .sacha_spatial_save_local_occurrences(
        species, occurrences, directory, overwrite,
        taxon_directory = taxon_directory, taxon = resolved_taxon$taxon,
        download = if (is.null(complete_result)) NULL else complete_result$download,
        date = date,
        dataset_keys = if (is.null(complete_result)) {
          occ_result$dataset_keys
        } else {
          complete_result$dataset_keys
        }
      )
    } else {
      warning("`save_occurrences` has no effect: the occurrences were read from a saved local export.",
              call. = FALSE)
    }
  }

  image_files <- if (isTRUE(export_image)) {
    .sacha_spatial_export_image(
      occurrences, species, taxon_directory, colour, zoom_static, dpi, overwrite,
      file_prefix = "gbif_map",
      static_legend_position = static_legend_position
    )
  } else {
    character()
  }

  map <- if (identical(mode, "interactive")) {
    .sacha_spatial_interactive_map(
      occurrences, colour, language,
      boundary = NULL, bbox = NULL
    )
  } else {
    .sacha_spatial_static_map(
      occurrences, colour, zoom_static,
      boundary = NULL, bbox = NULL,
      static_legend_position = static_legend_position
    )
  }
  attr(map, "sacha_occurrences") <- occurrences
  attr(map, "sacha_files") <- c(image_files, local_files)
  attr(map, "sacha_species") <- species
  attr(map, "sacha_map_place") <- NULL
  attr(map, "sacha_gbif_download") <- if (is.null(complete_result)) {
    NULL
  } else {
    complete_result$download
  }
  attr(map, "sacha_retrieved_at") <- if (is.null(local_species)) {
    NULL
  } else {
    local_species$metadata$retrieved_at
  }
  map
}

#' @rdname gbif_map
#' @export
gbif_map_md <- function(species = NULL,
                        limit = 50000L,
                        mode = NULL,
                        save_occurrences = FALSE,
                        export_image = FALSE,
                        directory = getwd(),
                        colour = NULL,
                        zoom_static = NULL,
                        dpi = NULL,
                        overwrite = TRUE,
                        fig_width = NULL,
                        fig_height = NULL,
                        source = c("web", "local"),
                        taxon_id = NULL,
                        generate_doi = FALSE,
                        date = NULL,
                        static_legend_position = "bottomleft") {
  .sacha_spatial_apply_taxon_arg_defaults(
    environment(), match.call(expand.dots = FALSE), names(formals(sys.function()))
  )
  input <- .sacha_spatial_taxon_input(species, taxon_id)
  language <- .sacha_spatial_language()
  mode <- .sacha_spatial_resolve_report_mode(mode)
  map <- gbif_map(
    species = input$species,
    taxon_id = input$taxon_id,
    limit = limit,
    mode = mode,
    save_occurrences = save_occurrences,
    export_image = export_image,
    directory = directory,
    colour = colour,
    static_legend_position = static_legend_position,
    zoom_static = zoom_static,
    dpi = dpi,
    overwrite = overwrite,
    source = source,
    generate_doi = generate_doi,
    date = date
  )
  .sacha_spatial_emit_md_map(
    map = map, variant = attr(map, "sacha_map_frame")$variant,
    mode = mode, frame = attr(map, "sacha_map_frame"),
    species = attr(map, "sacha_species"),
    language = language, place = attr(map, "sacha_map_place"),
    fig_width = fig_width, fig_height = fig_height
  )
}

#' Save traceable GBIF occurrences for local mapping
#'
#' Supply exactly one of `species` or `taxon_id`. GBIF resolves a supplied name
#' as an exact species-level name or validates a COL XR usage key as an exact
#' species-level taxon, then retrieves georeferenced records using that taxon
#' key and the same checklist key. By default, the fast occurrence-search API
#' retrieves up to 50,000 records without waiting for a registered download.
#' Set `generate_doi = TRUE` to wait for a registered download and include its
#' official DOI in `metadata.json`; this requires `GBIF_USER`, `GBIF_PWD`, and
#' `GBIF_EMAIL` credentials. With `generate_doi = TRUE`, set `limit = NULL` to save
#' all available records; otherwise increase the finite limit up to 100,000.
#' For high-volume species such as `Amaranthus hybridus`, prefer a local export
#' or narrow the query with `date` or spatial filters. Country areas are queried
#' with GBIF's standardized country predicate; `crop_vector` and `crop_box` are
#' converted to a common WGS 84 geometry and sent as a spatial predicate. The
#' exact package boundary is applied to returned records before `limit`, so a
#' crop receives up to the requested number of matching occurrences. If a
#' vector geometry exceeds GBIF's query-size limit, its bounding box is used as
#' a candidate filter and the exact geometry is applied locally. The function can optionally restrict
#' records to a KML or ESRI Shapefile boundary, a named geographic area, or a
#' WGS 84 bounding box. It saves the returned GBIF fields and machine-readable
#' metadata including the exact taxon match and COL XR taxon key, and, for a
#' crop, the corresponding WGS 84 boundary. Full-extent exports can be used offline with
#' `source = "local"` in [gbif_map()] or [gbif_map_crop()]. A cropped export can
#' be used with [gbif_map_crop()] by selecting `source = "local"`,
#' `local_export = "crop"`, and the same crop arguments used when saving it.
#' When both functions use the same taxon, occurrence set, and crop selectors,
#' their saved occurrence records are interchangeable as local input. The
#' default limit is 50,000; `limit = NULL` requires `generate_doi = TRUE` to save
#' the complete available occurrence set. The default fast search saves the returned
#' records without waiting for a registered GBIF download. Set `generate_doi =
#' TRUE` to wait for that download and record its DOI in the metadata.
#' In either form, the accepted COL XR species name determines the output
#' directory, so supplying a taxon by name or by `taxon_id` uses the same path.
#'
#' Full-extent records are saved to
#' `sacha_exports/Family_Genus_species`; cropped records are saved to
#' `sacha_exports/Family_Genus_species/Place`, where `Place` is derived from
#' the crop-vector file name, the resolved geographic area, or the crop-box label (or
#' its coordinates when no label is supplied).
#'
#' GBIF uses the Catalogue of Life Extended Release (COL XR) as the taxonomic
#' reference for occurrence downloads. By downloading the data, you
#' acknowledge GBIF's [data-use agreement](https://www.gbif.org/terms/data-user).
#' When citing or referring to registered downloads (`generate_doi = TRUE`), follow GBIF's
#' [citation guidelines](https://www.gbif.org/citation-guidelines#occDataDownload)
#' and include the DOI assigned to that specific download in every publication.
#'
#' Configure the GBIF account credentials in the current R session with
#' `Sys.setenv(GBIF_USER = "your_username", GBIF_PWD = "your_password",
#' GBIF_EMAIL = "your_account_email")`. To keep them available in future
#' sessions, add the same three variables to your personal `~/.Renviron` file
#' and restart R. Do not share or publish that file.
#'
#' @param species A binomial scientific name, for example `"Bidens andicola"`.
#'   GBIF must resolve it as an exact species-level name; approximate or
#'   higher-rank matches are not used. Supply exactly one of `species` or
#'   `taxon_id`.
#' @param taxon_id A COL XR alphanumeric species usage key, such as `"Q2M4"`.
#'   GBIF verifies that the key identifies one species-level taxon. Supply
#'   exactly one of `taxon_id` or `species`.
#' @param crop_vector Optional path to a `.kml` or `.shp` file used to restrict
#'   occurrences to a geographic area. The file name provides the place name
#'   used for the caption and export directory. It cannot be combined with
#'   `crop_area`.
#' @param crop_area,crop_box Geographic crop selectors shared with
#'   [gbif_map_crop()]. They use the same area resolution, WGS 84 coordinate
#'   order, label, and overlap rules. Here they filter the records being saved
#'   and determine the `Place` subdirectory. A country area uses GBIF's
#'   standardized `country` predicate when its two-letter code is available;
#'   `crop_box` is sent as a WGS 84 spatial predicate. `crop_area` cannot be
#'   combined with `crop_vector`; `crop_box` can be used alone or with either
#'   boundary input.
#' @param limit Maximum number of georeferenced records retained after the
#'   spatial filter, as a positive integer from `1` to `100000`. Defaults to
#'   `50000`, as in [gbif_map_crop()]. Set to `NULL` with `generate_doi = TRUE` to
#'   retrieve all available matching records. For high-volume species, use a
#'   local export or narrow the query with `date` or a spatial selector.
#' @param generate_doi Logical. If `TRUE`, request and wait for a registered GBIF
#'   occurrence download, and record its DOI in `metadata.json`. This requires
#'   GBIF account credentials. If `FALSE` (default), retrieve records with the
#'   faster occurrence-search API and save the returned records without waiting
#'   for a DOI.
#' @param date Optional ISO date or date range such as
#'   `"2020-01-01,2021-01-01"`. It filters occurrence datasets by their GBIF
#'   registry creation date (`createdDate`), then selects records from those
#'   datasets. It does not filter the date an occurrence was observed.
#' @param directory Base output directory. As in [gbif_map_crop()], the
#'   function creates `sacha_exports/Family_Genus_species`; cropped exports
#'   are placed in its `Place` subdirectory. Defaults to [getwd()].
#' @param overwrite Logical. If `TRUE` (default), existing occurrence,
#'   metadata, and boundary files for this export may be replaced; `FALSE`
#'   prevents replacement.
#' @return A named character vector with paths to the saved occurrence,
#'   metadata, and, for cropped exports, boundary files.
#' @examples
#' \dontrun{
#' gbif_save_occurrences("Bidens andicola")
#' gbif_save_occurrences("Bidens andicola", crop_vector = "Cotopaxi.kml")
#' gbif_save_occurrences(taxon_id = "Q2M4")
#' gbif_save_occurrences(
#'   "Bidens andicola",
#'   crop_box = c(-75, -10, -70, -5, "study area"),
#'   limit = 5000
#' )
#' gbif_map("Bidens andicola", source = "local")
#' }
#' @export
gbif_save_occurrences <- function(species = NULL,
                                  crop_vector = NULL,
                                  crop_area = NULL,
                                  crop_box = NULL,
                                  limit = 50000L,
                                  directory = getwd(),
                                  overwrite = TRUE,
                                  taxon_id = NULL,
                                  generate_doi = FALSE,
                                  date = NULL) {
  .sacha_spatial_apply_taxon_arg_defaults(
    environment(), match.call(expand.dots = FALSE), names(formals(sys.function()))
  )
  resolved_taxon <- .sacha_spatial_resolve_taxon_input(species, taxon_id)
  species <- resolved_taxon$species
  taxon <- resolved_taxon$taxon
  language <- .sacha_spatial_language()
  limit <- if (is.null(limit)) NULL else .sacha_spatial_validate_limit(limit)
  generate_doi <- .sacha_spatial_validate_logical(generate_doi, "generate_doi")
  if (is.null(limit) && !isTRUE(generate_doi)) {
    stop("`limit = NULL` requires `generate_doi = TRUE` so the complete GBIF export is retrieved.",
         call. = FALSE)
  }
  date <- .sacha_spatial_validate_date(date)
  overwrite <- .sacha_spatial_validate_logical(overwrite, "overwrite")
  crop_window <- .sacha_spatial_crop_box_parse(crop_box)
  box_only <- !is.null(crop_window) && is.null(crop_vector) && is.null(crop_area)
  crop_data <- if (box_only) {
    .sacha_spatial_apply_crop_box(NULL, crop_window)
  } else if (!is.null(crop_vector) || !is.null(crop_area)) {
    boundary <- .sacha_spatial_map_crop_boundary(
      crop_vector = crop_vector, crop_area = crop_area, language = language
    )
    boundary$region <- boundary$boundary
    if (!is.null(crop_window)) {
      .sacha_spatial_apply_crop_box(boundary, crop_window)
    } else {
      boundary
    }
  } else {
    NULL
  }
  spatial_query <- .sacha_spatial_gbif_crop_query(crop_data)
  result <- if (isTRUE(generate_doi)) {
    .sacha_spatial_gbif_occurrence_download(
      taxon, date = date, crop_data = crop_data
    )
  } else {
    .sacha_spatial_gbif_occurrences(
      species, limit, date = date, taxon = taxon, crop_data = crop_data
    )
  }
  if (identical(result$status, "error")) {
    .sacha_spatial_stop_gbif_error(result, species, language)
  }
  occurrences <- result$occurrences
  if (!is.null(crop_data)) {
    occurrences <- .sacha_spatial_filter_crop(occurrences, crop_data)
  }
  if (!is.null(limit)) occurrences <- utils::head(occurrences, limit)
  if (!nrow(occurrences)) {
    stop(paste(.sacha_spatial_label("no_occurrences", language), species), call. = FALSE)
  }
  crop_metadata <- if (is.null(crop_data)) {
    NULL
  } else {
    metadata <- list(place = crop_data$place)
    if (!is.null(crop_data$crop_vector)) metadata$crop_vector <- crop_data$crop_vector
    if (!is.null(crop_data$crop_area)) metadata$crop_area <- crop_data$crop_area
    if (!is.null(crop_data$country_code)) metadata$country_code <- crop_data$country_code
    if (!is.null(crop_data$crop_box)) metadata$crop_box <- crop_data$crop_box
    metadata
  }
  .sacha_spatial_save_local_occurrences(
    species = species,
    occurrences = occurrences,
    directory = directory,
    overwrite = overwrite,
    crop = crop_metadata,
    boundary = if (is.null(crop_data)) NULL else crop_data$region,
    taxon = result$taxon,
    download = result$download,
    date = date,
    dataset_keys = result$dataset_keys,
    spatial_query = spatial_query
  )
}

#' @rdname gbif_save_occurrences
#' @export
gbif_save_ocurrences <- gbif_save_occurrences

#' Generate a GBIF occurrence map cropped to a geographic boundary
#'
#' `gbif_map_crop()` creates a standalone map of the georeferenced occurrences
#' of a species within a geographic boundary, and `gbif_map_crop_md()` creates
#' the same map for a markdown-formatted report (Quarto or R Markdown).
#'
#' The area to map is defined with `crop_vector`, `crop_area`, or `crop_box`.
#' `crop_vector` and `crop_area` are mutually exclusive and define a boundary
#' that is drawn as the map outline; `crop_box` defines a rectangle and can be
#' used alone or together with either of them. With `distribution = "local"` (default), only
#' records inside the area are mapped. With `distribution = "global"`, all
#' available georeferenced records are mapped and the boundary, when there is
#' one, is kept as a reference outline.
#'
#' When `crop_box` is combined with `crop_vector` or `crop_area`, the boundary is
#' clipped to the rectangle and the map is framed on the rectangle, so that
#' only the part of the area that falls inside it is shown; the rectangle
#' itself is not drawn. For example, `crop_area = "EC"` maps Ecuador including
#' the Galapagos Islands, whereas adding
#' `crop_box = c(-82.5, -5.5, -75.0, 1.6, "Ecuador continental")` maps only the
#' continental part of the country. When `crop_box` is used alone, the map is
#' framed on the rectangle and no outline is drawn. Clipping is performed in
#' planar longitude/latitude coordinates, so the edges of the rectangle follow
#' the meridians and parallels given.
#'
#' For a GBIF source with `distribution = "local"`, `crop_area` is sent to GBIF
#' as its standardized two-letter `country` filter when available. A vector or
#' rectangle is sent as a WGS 84 spatial predicate in both fast occurrence
#' searches and registered downloads. The returned points are then checked
#' against the exact package boundary. If a complex vector exceeds GBIF's query
#' size limit, its bounding box is used to retrieve candidates before exact
#' local filtering. A `distribution = "global"` map keeps its global query;
#' when `save_occurrences = TRUE`, its saved crop is retrieved separately with
#' the spatial filter.
#'
#' With `source = "web"` (default), supply exactly one of `species` or
#' `taxon_id`. GBIF resolves the binomial as an exact, species-level name or
#' validates the COL XR usage key as an exact species-level taxon. Its taxon
#' key and that same checklist key are then used for the occurrence query,
#' preventing approximate or higher-rank name matches. The accepted name
#' determines the output directory, so either input form selects the same
#' species export. With `source = "local"`, GBIF is not contacted and
#' records are read from a saved export under `sacha_exports` in
#' `directory`, in one of two ways set by `local_export`:
#' \itemize{
#'   \item `"species"` (default): the full-extent export of the species
#'     (`sacha_exports/Family_Genus_species`), produced by
#'     `save_occurrences = TRUE` in [gbif_map()] or by
#'     [gbif_save_occurrences()]. The selected area is applied to these
#'     records as it is applied to records retrieved from GBIF.
#'   \item `"crop"`: an export previously saved for the same area
#'     (`sacha_exports/Family_Genus_species/Place`), which contains the
#'     filtered records and the stored boundary. It requires
#'     `distribution = "local"`. When the export was saved with a `crop_box`,
#'     the same `crop_box` (and therefore the same label) must be supplied to
#'     identify it.
#' }
#' With `distribution = "global"`, a full-extent species export is used when
#' available. If it is absent, compatible cropped exports for the same COL XR
#' taxon are found recursively and combined; duplicate records with the same
#' GBIF or dataset-occurrence identifier are removed. A single cropped export
#' is also accepted. The map's `sacha_local_coverage` attribute reports whether
#' it used a full-extent export or partial local crops. In this mode,
#' `local_export` does not prevent fallback to the widest available local data.
#'
#' Interactive maps show the scientific name, year, locality, and observer
#' (`recordedBy`) reported by GBIF for each occurrence. Missing values are
#' displayed as unavailable.
#'
#' Setting `save_occurrences = TRUE` stores the records located inside the
#' area and its boundary, whatever the value of `distribution`, in
#' `sacha_exports/Family_Genus_species/Place` within `directory`, where
#' `Place` is the name of the area or, when `crop_box` is supplied, its label.
#' If `crop_box` is supplied, the stored records and boundary are those inside
#' the rectangle; when `crop_box` is used alone, the stored boundary is the
#' rectangle itself. The spatial filter is applied before `limit`. For a global
#' GBIF map, saving a crop performs a separate spatially filtered retrieval;
#' its `metadata.json` records the query and, when requested, the DOI for that
#' filtered download.
#' Three files are written: `occurrences.gpkg` (records in WGS 84, EPSG:4326),
#' `boundary.gpkg`, and `metadata.json` (query, retrieval date, coordinate
#' reference system, record count, and, when `generate_doi = TRUE`, the official
#' GBIF download key and DOI). Registered downloads require `GBIF_USER`,
#' `GBIF_PWD`, and `GBIF_EMAIL` credentials. With `distribution = "global"` the map
#' still shows its global record set, but only those inside the area are saved; if
#' none falls inside, nothing is saved and a warning is issued. When the
#' records come from the full-extent local export, the metadata retain its
#' query, provenance, and any DOI it contains, and add a `derived_from` entry
#' and the processing date, so that a crop produced offline has the same traceability as one
#' produced from GBIF. When `source = "local"`, `local_export = "crop"`, and
#' `distribution = "local"`, the records are already saved and nothing is written.
#'
#' Configure the GBIF account credentials in the current R session with
#' `Sys.setenv(GBIF_USER = "your_username", GBIF_PWD = "your_password",
#' GBIF_EMAIL = "your_account_email")`. To keep them available in future
#' sessions, add the same three variables to your personal `~/.Renviron` file
#' and restart R. Do not share or publish that file.
#'
#' GBIF uses the Catalogue of Life Extended Release (COL XR) as the taxonomic
#' reference for occurrence downloads. By downloading the data, you
#' acknowledge GBIF's [data-use agreement](https://www.gbif.org/terms/data-user).
#' When citing or referring to registered downloads (`generate_doi = TRUE`), follow GBIF's
#' [citation guidelines](https://www.gbif.org/citation-guidelines#occDataDownload)
#' and include the DOI assigned to that specific download in every publication.
#'
#' Setting `export_image = TRUE` writes the static map as a PNG file, whatever
#' the value of `mode`, to the same directory as the saved files
#' (`sacha_exports/Family_Genus_species/Place`). With
#' `distribution = "global"` the file prefix is `gbif_map_crop_global`, so that
#' the image does not replace that of the cropped map.
#'
#' In Markdown reports, the crop figure ID has the form
#' `fig-gbif-map-crop-bidens_andicola-<place>`, where `<place>` is the
#' lower-case, ASCII-normalised area name or crop label. For example,
#' `crop_area = "Ecuador"` yields
#' `fig-gbif-map-crop-bidens_andicola-ecuador`; the label
#' `"Ecuador continental"` yields
#' `fig-gbif-map-crop-bidens_andicola-ecuador_continental`. A crop-vector map uses its
#' resolved place name. Use the resulting ID as `@fig-gbif-map-crop-...` in
#' report prose; `distribution`, `source`, and `mode` do not change the ID.
#' For generated Markdown figures, set `results: asis` on the Quarto or
#' R Markdown chunk.
#'
#' @param species A binomial scientific name in the form `"Genus species"`,
#'   e.g. `"Bidens andicola"`. GBIF must resolve it as an exact species-level
#'   name; approximate and higher-rank matches are rejected. Supply exactly
#'   one of `species` or `taxon_id`.
#' @param taxon_id A COL XR alphanumeric species usage key, such as `"Q2M4"`.
#'   GBIF verifies that the key identifies one species-level taxon. Supply
#'   exactly one of `taxon_id` or `species`.
#' @param crop_vector Path to a `.kml` or `.shp` file whose geometry defines
#'   the crop extent and is drawn as the map outline. The file name provides
#'   the place name used in captions and in the export directory. It cannot be
#'   combined with `crop_area`.
#' @param crop_area Geographic area name or code resolved against Natural
#'   Earth's administrative gazetteer, such as `"Ecuador"` or `"EC"`. Its
#'   complete administrative name is used in captions and legends, unless
#'   `crop_box` provides a label. It cannot be combined with `crop_vector`.
#' @param crop_box Rectangular crop in WGS 84 decimal degrees, given as
#'   `c(xmin, ymin, xmax, ymax)`: west and east longitude, south and north
#'   latitude, e.g. `c(-82.5, -5.5, -75.0, 1.6)`. A fifth element, a character
#'   string, sets the crop label, e.g. `c(-82.5, -5.5, -75.0, 1.6, "Ecuador
#'   continental")`. Every rectangular crop adds an area item to the map
#'   legend: it uses this label when present and otherwise uses the formatted
#'   rectangle coordinates, e.g. `"82.5°W, 5.5°S, 75°W, 1.6°N"`. A supplied
#'   label also replaces the area name in captions and the export directory.
#'   It can be used alone or together with `crop_vector` or `crop_area`; in the latter
#'   case it must overlap the boundary. The map is framed on the rectangle,
#'   which is not drawn. With `distribution = "global"` all records are still
#'   mapped, but only those inside the frame are visible. Defaults to `NULL`
#'   (no rectangular crop).
#' @param limit Maximum number of georeferenced occurrences used to draw the
#'   map, an integer between `1` and `100000`. For local distribution, spatial
#'   filters are applied before this limit. Defaults to `50000`. For
#'   high-volume species such as `Amaranthus hybridus`, prefer a local export or
#'   narrow the query with `date`, `crop_vector`, `crop_area`, or `crop_box`.
#' @param mode Map type: `"interactive"` (default; a Leaflet widget) or
#'   `"static"` (a ggplot2 map). In `gbif_map_crop_md()`, `NULL` selects an
#'   interactive map for HTML output and a static map for other formats.
#' @param save_occurrences Logical. If `TRUE`, records located within the area,
#'   the boundary, and a provenance file are saved under
#'   `sacha_exports/Family_Genus_species/Place`. The metadata includes the
#'   validated COL XR taxon key and exact taxon match. A registered download DOI
#'   is included when `generate_doi = TRUE`. Defaults to `FALSE`.
#' @param export_image Logical. If `TRUE`, the static map is written as a PNG
#'   file to `sacha_exports/Family_Genus_species/Place`. Defaults to `FALSE`.
#' @param directory Base directory in which `sacha_exports` is created or,
#'   for a local source, searched. Occurrences and exported images are saved
#'   under `sacha_exports/Family_Genus_species/Place`. Defaults to [getwd()].
#' @param colour Colour of the occurrence points as a hexadecimal code with 3,
#'   4, 6, or 8 digits, e.g. `"#1B9E77"`. When `NULL`, the package default
#'   colour is used.
#' @param static_legend_position Position of the static legend as one composed
#'   unit with its scale bar and north arrow. Applies to static maps and PNGs
#'   written with `export_image = TRUE`, including when `mode` is
#'   `"interactive"`: `"bottomleft"` (default), `"bottom"`,
#'   `"bottomright"`, `"right"`, `"topright"`, `"top"`, `"topleft"`, or
#'   `"left"`. It does not change the interactive map widget.
#' @param dpi Resolution of the exported PNG in dots per inch. When `NULL`,
#'   the package default resolution is used.
#' @param overwrite Logical. Should existing output files be replaced? If
#'   `FALSE`, an existing local occurrence export raises an error. Defaults to
#'   `TRUE`.
#' @param distribution Geographic distribution of the mapped records:
#'   `"local"` (default) retrieves and maps records within the area;
#'   `"global"` maps the available global record set in the map frame. For a
#'   local source, the widest compatible saved extent is used and may be
#'   partial.
#' @param source Occurrence source: `"web"` (default) retrieves records from
#'   GBIF; `"local"` reads a saved export from `sacha_exports` under
#'   `directory`.
#' @param local_export Saved export used when `source = "local"` and
#'   `distribution = "local"`: `"species"` (default) reads the full-extent
#'   export and applies the area; `"crop"` reads the export saved for the area.
#'   For `distribution = "global"`, the full-extent export is preferred and
#'   compatible cropped exports are combined if no full-extent export exists.
#'   It is ignored when `source = "web"`.
#' @param generate_doi Logical. If `TRUE` and `source = "web"`, create and wait
#'   for a registered GBIF download, and record its DOI in `metadata.json`.
#'   This requires GBIF account credentials. If `FALSE` (default), use the
#'   faster occurrence-search API and save the returned records without waiting
#'   for a DOI. For a local source, no GBIF request is made.
#' @param date Optional ISO date or range such as
#'   `"2020-01-01,2021-01-01"`. It filters occurrence datasets by their GBIF
#'   registry creation date (`createdDate`), then selects records from those
#'   datasets. It does not filter the date an occurrence was observed. Local
#'   exports can use only the same date filter recorded in their metadata.
#' @param fig_width,fig_height Dimensions in inches for static maps produced
#'   by `gbif_map_crop_md()`. When `NULL`, the static map dimensions follow its
#'   aspect ratio. Interactive Leaflet maps use the widget and output
#'   container's default sizing.
#' @return A Leaflet widget or a ggplot2 object. Paths of the written files
#'   are stored in the `"sacha_files"` attribute, the plotted records in
#'   `"sacha_occurrences"`, the species in `"sacha_species"`, the resolved
#'   place name in `"sacha_crop_place"`, and local coverage details in
#'   `"sacha_local_coverage"` for global local-source maps.
#'   `gbif_map_crop_md()` returns the map
#'   formatted for the report output.
#' @examples
#' \dontrun{
#' map <- gbif_map_crop("Bidens andicola", crop_vector = "Ecuador.kml", limit = 100)
#' map_by_key <- gbif_map_crop(taxon_id = "Q2M4", crop_area = "Ecuador")
#' map
#'
#' gbif_map_crop("Bidens andicola", crop_area = "EC",
#'               crop_box = c(-82.5, -5.5, -75.0, 1.6, "Ecuador continental"))
#'
#' gbif_map_crop("Bidens andicola",
#'               crop_box = c(-82.5, -5.5, -75.0, 1.6))
#'
#' gbif_save_occurrences("Bidens andicola")
#' gbif_map_crop("Bidens andicola", crop_area = "Ecuador", source = "local",
#'               save_occurrences = TRUE, export_image = TRUE)
#' gbif_map_crop("Bidens andicola", crop_area = "Ecuador", source = "local",
#'               local_export = "crop")
#'
#' gbif_map_crop_md("Bidens andicola", crop_area = "Ecuador",
#'                   source = "web", distribution = "local")
#' gbif_map_crop_md("Bidens andicola", crop_area = "Ecuador",
#'                   crop_box = c(-82.5, -5.5, -75.0, 1.6,
#'                                "Ecuador continental"),
#'                   source = "web", distribution = "global", mode = "static",
#'                   save_occurrences = TRUE, export_image = TRUE,
#'                   colour = "#1B9E77", dpi = 300, overwrite = TRUE,
#'                   fig_width = 8, fig_height = 6)
#' gbif_map_crop_md("Bidens andicola",
#'                   crop_box = c(-82.5, -5.5, -75.0, 1.6,
#'                                "Ecuador continental"),
#'                   source = "web", distribution = "local")
#' gbif_map_crop_md("Bidens andicola", crop_vector = "study_boundary.kml",
#'                   source = "web", distribution = "local", mode = "static",
#'                   export_image = TRUE)
#' gbif_map_crop_md("Bidens andicola", crop_vector = "study_boundary.shp",
#'                   crop_box = c(-82.5, -5.5, -75.0, 1.6,
#'                                "study area detail"),
#'                   source = "web", distribution = "global")
#' gbif_map_crop_md("Bidens andicola", crop_area = "Ecuador",
#'                   source = "local", local_export = "species",
#'                   distribution = "global")
#' gbif_map_crop_md("Bidens andicola", crop_area = "Ecuador",
#'                   source = "local", local_export = "crop",
#'                   distribution = "local")
#' gbif_map_crop_md("Bidens andicola", crop_area = "Ecuador",
#'                   crop_box = c(-82.5, -5.5, -75.0, 1.6,
#'                                "Ecuador continental"),
#'                   source = "local", local_export = "crop",
#'                   distribution = "local")
#' }
#' @export
gbif_map_crop <- function(species = NULL,
                          crop_vector = NULL,
                          crop_area = NULL,
                          crop_box = NULL,
                          limit = 50000L,
                          mode = c("interactive", "static"),
                          save_occurrences = FALSE,
                          export_image = FALSE,
                          directory = getwd(),
                          colour = NULL,
                          dpi = NULL,
                          overwrite = TRUE,
                          distribution = c("local", "global"),
                          source = c("web", "local"),
                          local_export = c("species", "crop"),
                          taxon_id = NULL,
                          generate_doi = FALSE,
                          date = NULL,
                          static_legend_position = "bottomleft") {
  .sacha_spatial_apply_taxon_arg_defaults(
    environment(), match.call(expand.dots = FALSE), names(formals(sys.function()))
  )
  input <- .sacha_spatial_taxon_input(species, taxon_id)
  species <- input$species
  taxon_id <- input$taxon_id
  language <- .sacha_spatial_language()
  limit <- .sacha_spatial_validate_limit(limit)
  generate_doi <- .sacha_spatial_validate_logical(generate_doi, "generate_doi")
  date <- .sacha_spatial_validate_date(date)
  map_inputs <- .sacha_spatial_map_style_inputs(colour, dpi)
  colour <- map_inputs$colour
  dpi <- map_inputs$dpi
  dpi <- .sacha_spatial_validate_limit(dpi)
  colour <- .sacha_spatial_validate_colour(colour)
  overwrite <- .sacha_spatial_validate_logical(overwrite, "overwrite")
  save_occurrences <- .sacha_spatial_validate_logical(save_occurrences, "save_occurrences")
  export_image <- .sacha_spatial_validate_logical(export_image, "export_image")
  mode <- .sacha_spatial_map_mode(mode)
  distribution <- match.arg(distribution, c("local", "global"))
  source <- .sacha_spatial_source(source)
  local_export <- .sacha_spatial_local_export(local_export)
  if (identical(source, "web")) {
    resolved_taxon <- .sacha_spatial_resolve_taxon_input(species, taxon_id)
    species <- resolved_taxon$species
  }
  crop_window <- .sacha_spatial_crop_box_parse(crop_box)
  box_only <- !is.null(crop_window) && is.null(crop_vector) && is.null(crop_area)

  use_crop_export <- identical(source, "local") && identical(distribution, "local") &&
    identical(local_export, "crop")
  local_retrieved_at <- NULL

  if (use_crop_export) {
    crop_selector <- if (box_only) {
      list(
        place = crop_window$label %spatial||%
          .sacha_spatial_crop_box_label(crop_window$extent),
        crop_vector = NULL,
        crop_area = NULL,
        crop_box = NULL
      )
    } else {
      selector <- .sacha_spatial_map_crop_boundary(
        crop_vector = crop_vector, crop_area = crop_area, language = language
      )
      if (!is.null(crop_window$label)) selector$place <- crop_window$label
      selector
    }
    crop_place <- crop_selector$place
    local_crop <- .sacha_spatial_load_local_occurrences(
      species, directory, crop_place, crop_only = TRUE, taxon_id = taxon_id
    )
    species <- .sacha_spatial_validate_species(local_crop$metadata$species)
    local_retrieved_at <- local_crop$metadata$retrieved_at
    boundary_path <- file.path(local_crop$directory, "boundary.gpkg")
    if (!file.exists(boundary_path)) {
      stop(sprintf("Local cropped export for '%s' does not contain a saved boundary.", species),
           call. = FALSE)
    }
    boundary <- tryCatch(sf::st_read(boundary_path, quiet = TRUE), error = function(e) {
      stop(sprintf("Could not read local boundary from '%s': %s.", boundary_path,
                   conditionMessage(e)), call. = FALSE)
    })
    crop_data <- list(
      boundary = if (box_only) NULL else boundary,
      region = boundary,
      bbox = if (is.null(crop_window)) {
        sf::st_bbox(boundary)
      } else {
        sf::st_bbox(crop_window$extent, crs = sf::st_crs(4326))
      },
      place = local_crop$metadata$crop$place %spatial||% crop_place,
      crop_vector = crop_selector$crop_vector,
      crop_area = crop_selector$crop_area,
      crop_box = crop_selector$crop_box,
      window = crop_window$extent
    )
    if (!is.null(crop_data$boundary)) {
      attr(crop_data$boundary, "sacha_place") <- crop_data$place
    }
  } else if (box_only) {
    crop_data <- .sacha_spatial_apply_crop_box(NULL, crop_window)
  } else {
    crop_data <- .sacha_spatial_map_crop_boundary(
      crop_vector = crop_vector, crop_area = crop_area, language = language
    )
    crop_data$region <- crop_data$boundary
    if (!is.null(crop_window)) {
      crop_data <- .sacha_spatial_apply_crop_box(crop_data, crop_window)
    }
  }

  local_species <- NULL
  cropped_occurrences <- NULL
  if (identical(source, "web")) {
    occ_result <- if (isTRUE(generate_doi)) {
      .sacha_spatial_gbif_occurrence_download(
        resolved_taxon$taxon, date = date,
        crop_data = if (identical(distribution, "local")) crop_data else NULL
      )
    } else {
      .sacha_spatial_gbif_occurrences(
        species, limit, date = date, taxon = resolved_taxon$taxon,
        crop_data = if (identical(distribution, "local")) crop_data else NULL
      )
    }
    if (identical(occ_result$status, "error")) {
      .sacha_spatial_stop_gbif_error(occ_result, species, language)
    }
    occ_result$taxon <- resolved_taxon$taxon
    retrieved_occurrences <- occ_result$occurrences
    if (identical(distribution, "local")) {
      cropped_occurrences <- .sacha_spatial_filter_crop(retrieved_occurrences, crop_data)
      if (!is.null(limit)) cropped_occurrences <- utils::head(cropped_occurrences, limit)
      occurrences <- cropped_occurrences
    } else {
      if (!is.null(limit)) retrieved_occurrences <- utils::head(retrieved_occurrences, limit)
      cropped_occurrences <- .sacha_spatial_filter_crop(retrieved_occurrences, crop_data)
      occurrences <- retrieved_occurrences
    }
  } else if (use_crop_export) {
    occurrences <- .sacha_spatial_filter_local_dataset_date(
      local_crop$occurrences, local_crop$metadata, date
    )
    if (!is.null(limit)) occurrences <- utils::head(occurrences, limit)
    cropped_occurrences <- occurrences
  } else {
    local_species <- .sacha_spatial_load_local_occurrences(
      species, directory, taxon_id = taxon_id,
      prefer_available = identical(distribution, "global"), date = date
    )
    local_retrieved_at <- local_species$caption_retrieved_at
    if (is.null(local_retrieved_at)) {
      local_retrieved_at <- local_species$metadata$retrieved_at
    }
    species <- .sacha_spatial_validate_species(local_species$metadata$species)
    local_occurrences <- .sacha_spatial_filter_local_dataset_date(
      local_species$occurrences, local_species$metadata, date
    )
    if (identical(distribution, "local")) {
      cropped_occurrences <- .sacha_spatial_filter_crop(local_occurrences, crop_data)
      if (!is.null(limit)) cropped_occurrences <- utils::head(cropped_occurrences, limit)
      occurrences <- cropped_occurrences
    } else {
      occurrences <- if (!is.null(limit)) {
        utils::head(local_occurrences, limit)
      } else {
        local_occurrences
      }
      cropped_occurrences <- .sacha_spatial_filter_crop(local_occurrences, crop_data)
      if (!is.null(limit)) cropped_occurrences <- utils::head(cropped_occurrences, limit)
    }
  }
  if (!nrow(occurrences)) {
    stop(paste(.sacha_spatial_label("no_occurrences", language), species), call. = FALSE)
  }
  map_bbox <- crop_data$bbox
  crop_legend_place <- if (is.null(crop_window)) {
    NULL
  } else {
    crop_window$label %spatial||% .sacha_spatial_crop_box_label(crop_window$extent)
  }

  taxon_directory <- .sacha_spatial_taxon_directory(species, occurrences, directory)
  output_directory <- file.path(
    taxon_directory, .sacha_spatial_path_component(crop_data$place)
  )

  local_files <- character()
  if (isTRUE(save_occurrences)) {
    if (use_crop_export) {
      warning("`save_occurrences` has no effect: the occurrences were read from a saved local export that already contains them.",
              call. = FALSE)
    } else {
      save_result <- NULL
      if (identical(source, "web")) {
        save_result <- if (identical(distribution, "global")) {
          if (isTRUE(generate_doi)) {
            .sacha_spatial_gbif_occurrence_download(
              resolved_taxon$taxon, date = date, crop_data = crop_data
            )
          } else {
            .sacha_spatial_gbif_occurrences(
              species, limit, date = date, taxon = resolved_taxon$taxon,
              crop_data = crop_data
            )
          }
        } else {
          occ_result
        }
        if (identical(save_result$status, "error")) {
          .sacha_spatial_stop_gbif_error(save_result, species, language)
        }
        cropped_occurrences <- .sacha_spatial_filter_crop(
          save_result$occurrences, crop_data
        )
        if (!is.null(limit)) cropped_occurrences <- utils::head(cropped_occurrences, limit)
      }
      if (is.null(cropped_occurrences) || !nrow(cropped_occurrences)) {
        warning("`save_occurrences` saved nothing: no occurrences fall within the crop boundary.",
                call. = FALSE)
      } else {
        crop_metadata <- list(
          place = crop_data$place,
          crop_vector = crop_data$crop_vector,
          crop_area = crop_data$crop_area,
          crop_box = crop_data$crop_box,
          country_code = crop_data$country_code
        )
        local_files <- .sacha_spatial_save_local_occurrences(
          species, cropped_occurrences, directory, overwrite,
          crop = crop_metadata,
          boundary = crop_data$region,
          taxon_directory = taxon_directory,
          parent = if (!is.null(local_species)) local_species$metadata,
          taxon = if (identical(source, "web")) save_result$taxon else NULL,
          download = if (identical(source, "web")) save_result$download else NULL,
          date = date,
          dataset_keys = if (identical(source, "web")) {
            save_result$dataset_keys
          } else if (is.null(date)) {
            NULL
          } else {
            local_species$metadata$dataset_filter$datasetKeys
          },
          spatial_query = if (identical(source, "web")) {
            save_result$spatial_query
          } else {
            NULL
          }
        )
      }
    }
  }

  image_files <- if (isTRUE(export_image)) {
    .sacha_spatial_export_image(
      occurrences, species, output_directory, colour,
      zoom = .sacha_spatial_map_style_crop_zoom(), dpi, overwrite,
      boundary = crop_data$boundary, bbox = map_bbox,
      file_prefix = if (identical(distribution, "global")) "gbif_map_crop_global" else "gbif_map_crop",
      legend_place = crop_legend_place,
      static_legend_position = static_legend_position
    )
  } else {
    character()
  }

  map <- if (identical(mode, "interactive")) {
    .sacha_spatial_interactive_map(
      occurrences, colour, language, boundary = crop_data$boundary,
      bbox = map_bbox, legend_place = crop_legend_place
    )
  } else {
    .sacha_spatial_static_map(
      occurrences, colour, zoom = .sacha_spatial_map_style_crop_zoom(),
      boundary = crop_data$boundary,
      bbox = map_bbox, legend_place = crop_legend_place,
      static_legend_position = static_legend_position
    )
  }
  attr(map, "sacha_occurrences") <- occurrences
  attr(map, "sacha_files") <- c(image_files, local_files)
  attr(map, "sacha_species") <- species
  attr(map, "sacha_crop_place") <- crop_data$place
  attr(map, "sacha_gbif_download") <- if (identical(source, "web")) {
    occ_result$download %spatial||% NULL
  } else {
    NULL
  }
  attr(map, "sacha_retrieved_at") <- local_retrieved_at
  attr(map, "sacha_local_coverage") <- if (
    identical(source, "local") && identical(distribution, "global")
  ) {
    local_species$metadata$local_coverage %spatial||% NULL
  } else {
    NULL
  }
  map
}

#' @rdname gbif_map_crop
#' @export
gbif_map_crop_md <- function(species = NULL,
                             crop_vector = NULL,
                             crop_area = NULL,
                             crop_box = NULL,
                             limit = 50000L,
                             mode = NULL,
                             save_occurrences = FALSE,
                             export_image = FALSE,
                             directory = getwd(),
                             colour = NULL,
                             dpi = NULL,
                             overwrite = TRUE,
                             distribution = c("local", "global"),
                             fig_width = NULL,
                             fig_height = NULL,
                             source = c("web", "local"),
                             local_export = c("species", "crop"),
                             taxon_id = NULL,
                             generate_doi = FALSE,
                             date = NULL,
                             static_legend_position = "bottomleft") {
  .sacha_spatial_apply_taxon_arg_defaults(
    environment(), match.call(expand.dots = FALSE), names(formals(sys.function()))
  )
  input <- .sacha_spatial_taxon_input(species, taxon_id)
  language <- .sacha_spatial_language()
  mode <- .sacha_spatial_resolve_report_mode(mode)
  map <- gbif_map_crop(
    species = input$species,
    crop_vector = crop_vector,
    taxon_id = input$taxon_id,
    crop_area = crop_area,
    crop_box = crop_box,
    limit = limit,
    mode = mode,
    save_occurrences = save_occurrences,
    export_image = export_image,
    directory = directory,
    colour = colour,
    static_legend_position = static_legend_position,
    dpi = dpi,
    overwrite = overwrite,
    distribution = distribution,
    source = source,
    local_export = local_export,
    generate_doi = generate_doi,
    date = date
  )
  .sacha_spatial_emit_md_map(
    map = map, variant = "crop", mode = mode,
    frame = attr(map, "sacha_map_frame"), species = attr(map, "sacha_species"),
    language = language, place = attr(map, "sacha_crop_place"),
    fig_width = fig_width, fig_height = fig_height
  )
}
