.sacha_global_config <- local({
  cfg <- new.env(parent = emptyenv())
  cfg$dataset <- "3LXR"
  cfg$base_url <- "https://api.checklistbank.org"
  cfg$gbif_base <- "https://api.gbif.org/v1"
  cfg$inat_base <- "https://api.inaturalist.org/v2"
  cfg$source_name <- "Catalogue of Life"
  cfg$taxon_url_template <- "https://www.catalogueoflife.org/data/taxon/%s"
  cfg$language <- "en"
  cfg$fetcher <- NULL
  cfg$label_overrides <- list()
  cfg
})

.sacha_global_cache <- new.env(parent = emptyenv())
.sacha_global_tracker <- new.env(parent = emptyenv())
.sacha_global_tracker$current <- NULL
.sacha_global_lang_cache <- new.env(parent = emptyenv())


.sacha_global_registry <- list(
  col = list(platform = "Catalogue of Life", url = "https://www.catalogueoflife.org/data/taxon/%s"),
  gbif = list(platform = "GBIF", url = "https://www.gbif.org/species/%s"),
  inat = list(platform = "iNaturalist", url = "https://www.inaturalist.org/taxa/%s"),
  ipni = list(platform = "IPNI", url = "https://www.ipni.org/n/%s"),
  wfo = list(platform = "World Flora Online", url = "https://wfoplantlist.org/taxon/%s"),
  tpl = list(platform = "The Plant List", url = "http://www.theplantlist.org/tpl1.1/record/%s"),
  itis = list(platform = "ITIS", url = "https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=%s"),
  ncbi = list(platform = "NCBI Taxonomy", url = "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=%s"),
  irmng = list(platform = "IRMNG", url = "https://www.irmng.org/aphia.php?p=taxdetails&id=%s"),
  worms = list(platform = "WoRMS", url = "https://www.marinespecies.org/aphia.php?p=taxdetails&id=%s"),
  eol = list(platform = "Encyclopedia of Life", url = "https://eol.org/pages/%s")
)

.sacha_global_synonym_groups <- c("homotypic", "heterotypic", "misapplied")

.sacha_global_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || !length(x)) return(default)
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x) & tolower(x) != "null"]
  if (length(x)) x[[1L]] else default
}

.sacha_global_text <- function(x, argument, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(NULL)
  x <- .sacha_global_scalar(x, NA_character_)
  if (is.na(x)) stop(sprintf("`%s` must be a non-empty character string.", argument), call. = FALSE)
  x
}

.sacha_global_validate_species <- function(species) {
  if (!is.character(species) || length(species) != 1L || is.na(species)) {
    stop("`species` must be one non-missing scientific name.", call. = FALSE)
  }
  species <- gsub("\\s+", " ", trimws(species))
  if (!nzchar(species) || length(strsplit(species, " ", fixed = TRUE)[[1L]]) < 2L) {
    stop("`species` must include at least a genus and a specific epithet.", call. = FALSE)
  }
  species
}

.sacha_global_lang_dir <- function() {
  directory <- system.file("lang", package = "sacha")
  if (!nzchar(directory)) {
    stop("Could not find the 'lang' directory shipped with the sacha package.", call. = FALSE)
  }
  directory
}

.sacha_global_lang_files <- function() {
  key <- ".files"
  if (!exists(key, .sacha_global_lang_cache, inherits = FALSE)) {
    files <- list.files(.sacha_global_lang_dir(), pattern = "\\.ya?ml$",
                        full.names = TRUE, ignore.case = TRUE)
    if (!length(files)) {
      stop("No .yml language files were found in the configured language directory.", call. = FALSE)
    }
    names(files) <- tolower(sub("\\.ya?ml$", "", basename(files), ignore.case = TRUE))
    assign(key, files, .sacha_global_lang_cache)
  }
  get(key, .sacha_global_lang_cache, inherits = FALSE)
}

.sacha_global_lang_data <- function(language) {
  key <- paste0("lang:", language)
  if (!exists(key, .sacha_global_lang_cache, inherits = FALSE)) {
    files <- .sacha_global_lang_files()
    if (!(language %in% names(files))) {
      stop(sprintf("No language dictionary exists for '%s'.", language), call. = FALSE)
    }
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("Package 'yaml' is required to read inst/lang/*.yml.", call. = FALSE)
    }
    data <- tryCatch(yaml::read_yaml(files[[language]]), error = function(e) NULL)
    if (is.null(data) || !is.list(data)) {
      stop(sprintf("Could not parse language dictionary '%s'.", files[[language]]), call. = FALSE)
    }
    assign(key, data, .sacha_global_lang_cache)
  }
  get(key, .sacha_global_lang_cache, inherits = FALSE)
}

.global_lang_aliases <- function() {
  key <- ".aliases"
  if (!exists(key, .sacha_global_lang_cache, inherits = FALSE)) {
    aliases <- character()
    for (code in names(.sacha_global_lang_files())) {
      candidates <- .sacha_global_lang_data(code)$aliases
      if (is.null(candidates)) candidates <- character()
      candidates <- unique(c(tolower(trimws(as.character(unlist(candidates, use.names = FALSE)))), code))
      candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
      for (candidate in candidates) if (!(candidate %in% names(aliases))) aliases[candidate] <- code
    }
    builtin <- c(spanish = "es", english = "en")
    for (candidate in names(builtin)) {
      if (builtin[[candidate]] %in% names(.sacha_global_lang_files()) && !(candidate %in% names(aliases))) {
        aliases[candidate] <- builtin[[candidate]]
      }
    }
    assign(key, aliases, .sacha_global_lang_cache)
  }
  get(key, .sacha_global_lang_cache, inherits = FALSE)
}

#' Resolve a language code or alias
#'
#' Internal helper. With `NULL`, returns the active language code. Otherwise
#' resolves a YAML basename (for example `"es"` for `inst/lang/es.yml`) or an
#' alias declared in the `aliases` entry of that file. It does not modify the
#' configuration.
#'
#' @param language `NULL`, a YAML basename, or a declared alias.
#' @return The canonical YAML basename.
#' @noRd
global_lang <- function(language = NULL) {
  if (is.null(language)) return(.sacha_global_config$language)
  code <- tolower(.sacha_global_text(language, "language"))
  aliases <- .global_lang_aliases()
  if (!(code %in% names(aliases))) stop(sprintf("Unsupported `language`: '%s'.", code), call. = FALSE)
  unname(aliases[[code]])
}

.sacha_global_label <- function(key, language = NULL) {
  language <- global_lang(language)
  value <- .sacha_global_scalar(.sacha_global_lang_data(language)$labels_data[key], NA_character_)
  overrides <- .sacha_global_config$label_overrides[[language]]
  if (!is.null(overrides)) {
    override <- .sacha_global_scalar(overrides[key], NA_character_)
    if (!is.na(override)) value <- override
  }
  if (is.na(value)) {
    stop(sprintf("The `labels_data.%s` entry is missing in the '%s' language dictionary.", key, language), call. = FALSE)
  }
  value
}

.sacha_global_set_labels <- function(labels) {
  if (!is.list(labels) || is.null(names(labels)) || any(!nzchar(names(labels)))) {
    stop("`labels` must be a named list of named character vectors.", call. = FALSE)
  }
  for (language in names(labels)) {
    values <- labels[[language]]
    if (is.list(values)) values <- unlist(values, recursive = TRUE, use.names = TRUE)
    if (!is.character(values) || is.null(names(values)) || any(!nzchar(names(values)))) {
      stop("Every `labels` entry must be a named character vector.", call. = FALSE)
    }
    language <- tolower(trimws(language))
    previous <- .sacha_global_config$label_overrides[[language]]
    if (is.null(previous)) previous <- character()
    previous[names(values)] <- as.character(values)
    .sacha_global_config$label_overrides[[language]] <- previous
  }
}

.sacha_global_configure <- function(dataset = NULL, base_url = NULL,
                                   gbif_base = NULL, inat_base = NULL,
                                   source_name = NULL, taxon_url_template = NULL,
                                   language = NULL, fetcher = NULL, labels = NULL) {
  queried_only <- missing(dataset) && missing(base_url) && missing(gbif_base) &&
    missing(inat_base) && missing(source_name) && missing(taxon_url_template) &&
    missing(language) && missing(fetcher) && missing(labels)
  if (queried_only) return(as.list(.sacha_global_config))

  changed <- FALSE
  if (!is.null(dataset)) {
    dataset <- .sacha_global_text(dataset, "dataset")
    changed <- changed || !identical(.sacha_global_config$dataset, dataset)
    .sacha_global_config$dataset <- dataset
  }
  if (!is.null(base_url)) {
    base_url <- sub("/+$", "", .sacha_global_text(base_url, "base_url"))
    if (!grepl("^https?://", base_url, ignore.case = TRUE)) {
      stop("`base_url` must start with http:// or https://.", call. = FALSE)
    }
    changed <- changed || !identical(.sacha_global_config$base_url, base_url)
    .sacha_global_config$base_url <- base_url
  }
  for (field in c("gbif_base", "inat_base")) {
    item <- if (identical(field, "gbif_base")) gbif_base else inat_base
    if (is.null(item)) next
    value <- sub("/+$", "", .sacha_global_text(item, field))
    if (!grepl("^https?://", value, ignore.case = TRUE)) {
      stop(sprintf("`%s` must start with http:// or https://.", field), call. = FALSE)
    }
    changed <- changed || !identical(.sacha_global_config[[field]], value)
    .sacha_global_config[[field]] <- value
  }
  if (!is.null(source_name)) .sacha_global_config$source_name <- .sacha_global_text(source_name, "source_name")
  if (!missing(taxon_url_template)) {
    if (!is.null(taxon_url_template)) {
      taxon_url_template <- .sacha_global_text(taxon_url_template, "taxon_url_template")
      if (!grepl("%s", taxon_url_template, fixed = TRUE)) {
        stop("`taxon_url_template` must contain one '%s' placeholder.", call. = FALSE)
      }
    }
    .sacha_global_config$taxon_url_template <- taxon_url_template
  }
  if (!is.null(labels)) .sacha_global_set_labels(labels)
  if (!is.null(language)) {
    language <- global_lang(language)
    changed <- changed || !identical(.sacha_global_config$language, language)
    .sacha_global_config$language <- language
  }
  if (!missing(fetcher)) {
    if (!is.null(fetcher) && !is.function(fetcher)) {
      stop("`fetcher` must be NULL or a function with arguments `(species, config)`.", call. = FALSE)
    }
    changed <- changed || !identical(.sacha_global_config$fetcher, fetcher)
    .sacha_global_config$fetcher <- fetcher
  }
  if (changed) .sacha_global_clear_cache()
  invisible(as.list(.sacha_global_config))
}

.sacha_global_clear_cache <- function(species = NULL) {
  if (is.null(species)) {
    keys <- ls(.sacha_global_cache, all.names = TRUE)
    if (length(keys)) rm(list = keys, envir = .sacha_global_cache)
  } else {
    key <- tolower(.sacha_global_validate_species(species))
    if (exists(key, .sacha_global_cache, inherits = FALSE)) rm(list = key, envir = .sacha_global_cache)
    full_name_key <- paste0("sc_full_name::", key)
    if (exists(full_name_key, .sacha_global_cache, inherits = FALSE)) {
      rm(list = full_name_key, envir = .sacha_global_cache)
    }
    location_prefix <- paste0("spatial_locations::", key, "::")
    location_keys <- ls(envir = .sacha_global_cache, all.names = TRUE)
    location_keys <- location_keys[startsWith(location_keys, location_prefix)]
    if (length(location_keys)) {
      rm(list = location_keys, envir = .sacha_global_cache)
    }
  }
  invisible(NULL)
}

.sacha_global_with_tracker <- function(fn) {
  state <- new.env(parent = emptyenv())
  state$failed <- FALSE
  state$messages <- character()
  previous <- .sacha_global_tracker$current
  .sacha_global_tracker$current <- state
  on.exit(.sacha_global_tracker$current <- previous, add = TRUE)
  list(value = fn(), failed = state$failed, messages = state$messages)
}

.sacha_global_track_error <- function(message) {
  state <- .sacha_global_tracker$current
  if (!is.null(state)) {
    state$failed <- TRUE
    state$messages <- unique(c(state$messages, as.character(message)[[1L]]))
  }
  invisible(NULL)
}

.sacha_global_dataframe_rows <- function(x) {
  lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
}

.sacha_global_rows <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (is.data.frame(x)) return(.sacha_global_dataframe_rows(x))
  if (!is.list(x)) return(list())
  if (!is.null(x$result)) x <- x$result
  if (is.null(x) || !length(x)) return(list())
  if (is.data.frame(x)) return(.sacha_global_dataframe_rows(x))
  if (!is.list(x)) return(list())
  fields <- c("id", "name", "scientificName", "rank", "usage", "property", "value", "language")
  if (!is.null(names(x)) && any(names(x) %in% fields)) return(list(x))
  if (is.null(names(x)) || all(grepl("^[0-9]+$", names(x)))) return(x)
  list(x)
}

.sacha_global_first_field <- function(x, fields, default = NA_character_) {
  if (is.data.frame(x)) x <- as.list(x[1L, , drop = FALSE])
  if (!is.list(x) || is.null(names(x))) return(default)
  lower_names <- tolower(names(x))
  for (field in fields) {
    index <- which(lower_names == tolower(field))[1L]
    if (!is.na(index)) {
      value <- .sacha_global_scalar(x[[index]], NA_character_)
      if (!is.na(value)) return(value)
    }
  }
  default
}

.sacha_global_url_encode <- function(x) utils::URLencode(as.character(x), reserved = TRUE)

.sacha_global_http_json <- function(url, query = list()) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required by the default ChecklistBank adapter.", call. = FALSE)
  }
  request <- httr2::request(url)
  request <- httr2::req_headers(request, Accept = "application/json")
  request <- httr2::req_user_agent(request, "sacha R package")
  request <- httr2::req_timeout(request, seconds = 30)
  request <- httr2::req_error(request, is_error = function(response) FALSE)
  request <- httr2::req_retry(request, max_tries = 3, is_transient = function(response) {
    httr2::resp_status(response) %in% c(429L, 500L, 502L, 503L, 504L)
  })
  query <- query[!vapply(query, is.null, logical(1))]
  if (length(query)) request <- do.call(httr2::req_url_query, c(list(request), query))
  response <- tryCatch(httr2::req_perform(request), error = function(e) {
    .sacha_global_track_error(conditionMessage(e)); NULL
  })
  if (is.null(response)) return(NULL)
  status <- httr2::resp_status(response)
  if (identical(status, 404L)) return(NULL)
  if (status < 200L || status >= 300L) {
    .sacha_global_track_error(sprintf("HTTP %s from %s", status, url))
    return(NULL)
  }
  tryCatch(httr2::resp_body_json(response, simplifyVector = FALSE), error = function(e) {
    .sacha_global_track_error(conditionMessage(e)); NULL
  })
}

.sacha_global_checklist_get <- function(dataset, ..., query = list()) {
  path <- vapply(c("dataset", dataset, ...), .sacha_global_url_encode, character(1))
  .sacha_global_http_json(paste(c(.sacha_global_config$base_url, path), collapse = "/"), query)
}

.sacha_global_gbif_match <- function(species, base_url) {
  .sacha_global_http_json(
    paste0(sub("/+$", "", base_url), "/species/match"),
    query = list(name = species, rank = "SPECIES")
  )
}

.sacha_global_inat_match <- function(species, base_url) {
  response <- .sacha_global_http_json(
    paste0(sub("/+$", "", base_url), "/taxa"),
    query = list(q = species, per_page = 30, fields = "id,name")
  )
  results <- if (is.list(response)) response$results else NULL
  if (is.null(results) || !length(results)) return(NULL)
  names_found <- vapply(results, .sacha_global_first_field, character(1), fields = "name")
  exact <- which(tolower(names_found) == tolower(species))
  if (length(exact)) results[[exact[[1L]]]] else results[[1L]]
}

.sacha_global_search_usage <- function(species, dataset) {
  candidates <- .sacha_global_rows(.sacha_global_checklist_get(
    dataset, "nameusage", "search", query = list(q = species, limit = 100)
  ))
  if (!length(candidates)) return(NULL)
  scientific <- vapply(candidates, .sacha_global_first_field, character(1),
                       fields = c("scientificName", "name", "label"))
  status <- vapply(candidates, .sacha_global_first_field, character(1), fields = "status")
  exact <- which(tolower(scientific) == tolower(species))
  accepted_exact <- exact[tolower(status[exact]) == "accepted"]
  selected <- if (length(accepted_exact)) accepted_exact[1L] else if (length(exact)) {
    exact[1L]
  } else {
    accepted <- which(tolower(status) == "accepted")
    if (length(accepted)) accepted[1L] else 1L
  }
  candidates[[selected]]
}

.sacha_global_resolve_usage <- function(species, dataset) {
  match <- .sacha_global_checklist_get(dataset, "match", "nameusage",
                                       query = list(q = species, verbose = "false"))
  usage <- if (is.list(match)) match$usage else NULL
  if (is.null(usage) || !is.list(usage)) usage <- .sacha_global_search_usage(species, dataset)
  if (is.null(usage)) return(NULL)
  id <- .sacha_global_first_field(usage, c("id", "usage.id"))
  if (is.na(id)) return(NULL)
  list(id = id,
       name = .sacha_global_first_field(usage, c("scientificName", "name", "label")),
       authorship = .sacha_global_first_field(usage, c("authorship", "author")),
       usage = usage)
}

.sacha_global_bundle <- function(resolution, dataset) {
  if (is.null(resolution) || is.na(resolution$id)) return(list())
  id <- resolution$id
  list(resolution = resolution,
       taxon = .sacha_global_checklist_get(dataset, "taxon", id),
       info = .sacha_global_checklist_get(dataset, "taxon", id, "info"),
       classification = .sacha_global_checklist_get(dataset, "taxon", id, "classification"),
       vernacular = .sacha_global_checklist_get(dataset, "taxon", id, "vernacular"))
}

.sacha_global_values_named <- function(x, key_pattern) {
  values <- character()
  visit <- function(node) {
    if (!is.list(node)) return(invisible(NULL))
    node_names <- names(node)
    if (!is.null(node_names)) {
      for (i in seq_along(node)) {
        value <- node[[i]]
        if (grepl(key_pattern, node_names[[i]], ignore.case = TRUE, perl = TRUE)) {
          values <<- c(values, as.character(unlist(value, recursive = TRUE, use.names = FALSE)))
        }
        if (is.list(value)) visit(value)
      }
    } else for (value in node) if (is.list(value)) visit(value)
    invisible(NULL)
  }
  visit(x)
  values <- trimws(values)
  unique(values[!is.na(values) & nzchar(values) & tolower(values) != "null"])
}

.sacha_global_all_strings <- function(x) {
  if (is.null(x)) return(character())
  values <- trimws(as.character(unlist(x, recursive = TRUE, use.names = FALSE)))
  unique(values[!is.na(values) & nzchar(values) & tolower(values) != "null"])
}

.sacha_global_reference_citation <- function(bundle, dataset) {
  reference_ids <- .sacha_global_values_named(bundle, "^(referenceId|publishedInId|sourceId)$")
  for (reference_id in reference_ids) {
    reference <- .sacha_global_checklist_get(dataset, "reference", reference_id)
    citation <- .sacha_global_first_field(reference, "citation")
    if (!is.na(citation)) return(citation)
  }
  NA_character_
}

.sacha_global_normalise_language <- function(language, country = NULL, output_language = NULL) {
  language <- tolower(.sacha_global_scalar(language, ""))
  country <- tolower(.sacha_global_scalar(country, ""))
  if (!nzchar(language)) return(".unspecified")
  if (grepl("castellano-kichwa|vacio", language) ||
      (grepl("spa|es", language) && grepl("kichwa|qvi", language))) return("castellano-kichwa")
  if (grepl("kichwa|quechua|qvi|quz|que", language)) return("kichwa")
  if (grepl("spa|es|castellano|spanish", language)) return("es")
  if (nzchar(country)) paste0(language, "-", country) else language
}

.sacha_global_normalise_common_names <- function(x, language = NULL) {
  if (is.null(x) || !length(x)) return(list())
  if (is.list(x) && !is.null(x$result)) x <- x$result
  if (is.character(x) && !is.null(names(x))) return(lapply(split(unname(x), names(x)), unique))
  if (is.data.frame(x) || (is.list(x) && (is.null(names(x)) || all(grepl("^[0-9]+$", names(x)))))) {
    x <- .sacha_global_rows(x)
  } else if (is.list(x) && !is.null(names(x)) &&
             any(tolower(names(x)) %in% c("name", "vernacularname", "commonname"))) {
    x <- list(x)
  } else if (is.list(x) && !is.null(names(x))) {
    output <- lapply(x, function(values) unique(trimws(as.character(unlist(values, use.names = FALSE)))))
    output <- output[vapply(output, length, integer(1)) > 0L]
    return(output)
  }
  output <- list()
  for (row in x) {
    name <- .sacha_global_first_field(row, c("name", "vernacularName", "commonName"))
    if (is.na(name)) next
    lang <- .sacha_global_normalise_language(.sacha_global_first_field(row, "language"),
                                             .sacha_global_first_field(row, c("country", "area")), language)
    output[[lang]] <- unique(c(output[[lang]], name))
  }
  output
}

.sacha_global_normalise_classification <- function(x, species, authorship = NA_character_) {
  rows <- .sacha_global_rows(x)
  output <- lapply(rows, function(row) list(
    rank = tolower(.sacha_global_first_field(row, "rank")),
    name = .sacha_global_first_field(row, c("name", "scientificName")),
    authorship = .sacha_global_first_field(row, c("authorship", "author")),
    id = .sacha_global_first_field(row, c("id", "taxonId"))
  ))
  output <- Filter(function(row) !is.na(row$rank) && !is.na(row$name), output)
  if (!any(vapply(output, function(row) identical(row$rank, "species"), logical(1)))) {
    output[[length(output) + 1L]] <- list(rank = "species", name = species,
                                          authorship = authorship, id = NA_character_)
  }
  keys <- vapply(output, function(row) paste(row$rank, row$name, sep = "|"), character(1))
  output[!duplicated(keys)]
}

.sacha_global_is_synonym_node <- function(node) {
  is.list(node) && !is.null(names(node)) &&
    (is.list(node[["name"]]) || is.character(node[["name"]]) ||
       !is.na(.sacha_global_first_field(node, c("scientificName", "label"))))
}

.sacha_global_synonym_entry <- function(node) {
  name_node <- node[["name"]]
  inner <- if (is.list(name_node)) name_node else node
  name <- .sacha_global_first_field(inner, c("scientificName", "name", "label"))
  authorship <- .sacha_global_first_field(inner, c("authorship", "author"))
  if (is.na(authorship)) authorship <- .sacha_global_first_field(node, c("authorship", "author"))
  according <- .sacha_global_first_field(node, c("accordingTo", "according_to"))
  list(name = name, authorship = authorship, according_to = according)
}

.sacha_global_collect_synonyms <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (is.data.frame(x)) x <- .sacha_global_dataframe_rows(x)
  if (!is.list(x)) return(list())
  if (.sacha_global_is_synonym_node(x)) return(list(.sacha_global_synonym_entry(x)))
  output <- list()
  for (item in x) output <- c(output, .sacha_global_collect_synonyms(item))
  output
}

.sacha_global_normalise_synonyms <- function(x) {
  output <- list()
  for (group in .sacha_global_synonym_groups) {
    entries <- if (is.list(x) && !is.data.frame(x)) .sacha_global_collect_synonyms(x[[group]]) else list()
    entries <- Filter(function(entry) !is.na(entry$name), entries)
    keys <- vapply(entries, function(entry) paste(entry$name, entry$authorship, entry$according_to), character(1))
    output[[group]] <- entries[!duplicated(keys)]
  }
  output
}

.sacha_global_normalise_references <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  rows <- list()
  if (is.data.frame(x)) {
    rows <- .sacha_global_dataframe_rows(x)
  } else if (is.list(x)) {
    x_names <- names(x)
    if (!is.null(x_names) && "citation" %in% tolower(x_names)) {
      rows <- list(x)
    } else if (!is.null(x_names) && !all(grepl("^[0-9]+$", x_names)) &&
               all(vapply(x, is.list, logical(1)))) {
      rows <- lapply(seq_along(x), function(i) {
        row <- x[[i]]
        if (is.na(.sacha_global_first_field(row, "id"))) row[["id"]] <- x_names[[i]]
        row
      })
    } else {
      rows <- x
    }
  }
  output <- lapply(rows, function(row) {
    csl <- row[["csl"]]
    if (is.list(csl) && length(csl) == 1L && is.null(names(csl)) && is.list(csl[[1L]])) csl <- csl[[1L]]
    if (!is.list(csl) || is.null(names(csl))) csl <- NULL
    citation <- .sacha_global_first_field(row, "citation")
    if (!is.na(citation)) citation <- gsub("\\s+", " ", citation)
    list(id = .sacha_global_first_field(row, "id"), citation = citation, csl = csl)
  })
  output <- Filter(function(entry) !is.na(entry$citation), output)
  keys <- vapply(output, function(entry) entry$citation, character(1))
  output[!duplicated(keys)]
}

.sacha_global_identifier_map <- function(bundle, resolution, gbif = NULL, inat = NULL) {
  output <- character()
  if (!is.null(resolution) && !is.na(resolution$id)) output["col"] <- resolution$id
  gbif_id <- .sacha_global_first_field(gbif, c("usageKey", "speciesKey", "key"))
  if (!is.na(gbif_id)) output["gbif"] <- gbif_id
  inat_id <- .sacha_global_first_field(inat, "id")
  if (!is.na(inat_id)) output["inat"] <- inat_id
  values <- .sacha_global_values_named(bundle, "^(identifier|identifiers)$")
  for (value in values) {
    if (!grepl(":", value, fixed = TRUE)) next
    key <- tolower(sub(":.*$", "", value))
    id <- sub("^[^:]+:", "", value)
    if (key %in% names(.sacha_global_registry) && nzchar(id) && !(key %in% names(output))) output[key] <- id
  }
  ipni_urls <- .sacha_global_all_strings(bundle)
  ipni_urls <- ipni_urls[grepl("ipni\\.org", ipni_urls, ignore.case = TRUE)]
  for (url in ipni_urls) {
    id <- sub(".*(?:names:|/n/)([0-9-]+).*$", "\\1", url, perl = TRUE)
    if (grepl("^[0-9-]+$", id) && !("ipni" %in% names(output))) output["ipni"] <- id
  }
  output
}

.sacha_global_checklistbank_fetcher <- function(species, config) {
  resolution <- .sacha_global_resolve_usage(species, config$dataset)
  bundle <- .sacha_global_bundle(resolution, config$dataset)
  gbif <- .sacha_global_gbif_match(species, config$gbif_base)
  inat <- .sacha_global_inat_match(species, config$inat_base)
  name <- if (is.null(resolution)) species else resolution$name
  if (is.na(name)) name <- species
  authorship <- if (is.null(resolution)) NA_character_ else resolution$authorship
  if (is.na(authorship)) authorship <- .sacha_global_first_field(bundle$taxon, c("authorship", "author"))
  classification <- bundle$info$classification
  if (is.null(classification)) classification <- bundle$taxon$classification
  if (is.null(classification)) classification <- bundle$classification
  list(scientific_name = name,
       authorship = authorship,
       first_publication = .sacha_global_reference_citation(bundle, config$dataset),
       classification = .sacha_global_normalise_classification(classification, name, authorship),
       common_names = .sacha_global_normalise_common_names(bundle$vernacular, config$language),
       synonyms = bundle$info$synonyms,
       references = bundle$info$references,
       identifiers = .sacha_global_identifier_map(bundle, resolution, gbif, inat),
       links = character())
}

.sacha_global_named_character <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = TRUE)
  original_names <- names(x)
  x <- trimws(as.character(x))
  if (!is.null(original_names)) names(x) <- original_names
  keep <- !is.na(x) & nzchar(x)
  x <- x[keep]
  if (!length(x)) return(character())
  if (is.null(names(x))) names(x) <- rep("", length(x))
  x
}

.sacha_global_normalise_record <- function(record, requested_species) {
  if (is.null(record)) record <- list()
  if (!is.list(record)) stop("A data-source `fetcher` must return a list.", call. = FALSE)
  name <- .sacha_global_scalar(record$scientific_name, requested_species)
  authorship <- .sacha_global_scalar(record$authorship, NA_character_)
  identifiers <- .sacha_global_named_character(record$identifiers)
  identifiers <- identifiers[nzchar(names(identifiers))]
  if (length(identifiers)) names(identifiers) <- tolower(names(identifiers))
  links <- .sacha_global_named_character(record$links)
  links <- links[nzchar(names(links))]
  messages <- record$transport_messages
  if (is.null(messages)) messages <- character()
  list(requested_species = requested_species,
       scientific_name = name,
       authorship = authorship,
       first_publication = .sacha_global_scalar(record$first_publication, NA_character_),
       classification = .sacha_global_normalise_classification(record$classification, name, authorship),
       common_names = .sacha_global_normalise_common_names(record$common_names, .sacha_global_config$language),
       synonyms = .sacha_global_normalise_synonyms(record$synonyms),
       references = .sacha_global_normalise_references(record$references),
       identifiers = identifiers,
       links = links,
       transport_error = isTRUE(record$transport_error),
       transport_messages = as.character(messages),
       retrieved_at = Sys.time())
}

.sacha_global_fetch_record <- function(species) {
  result <- .sacha_global_with_tracker(function() {
    fetcher <- .sacha_global_config$fetcher
    if (is.null(fetcher)) fetcher <- .sacha_global_checklistbank_fetcher
    tryCatch(fetcher(species, .sacha_global_configure()), error = function(e) {
      .sacha_global_track_error(conditionMessage(e)); NULL
    })
  })
  record <- .sacha_global_normalise_record(result$value, species)
  record$transport_error <- record$transport_error || result$failed
  record$transport_messages <- unique(c(record$transport_messages, result$messages))
  record
}

.sacha_global_record <- function(species, refresh = FALSE) {
  species <- .sacha_global_validate_species(species)
  key <- tolower(species)
  if (isTRUE(refresh) || !exists(key, .sacha_global_cache, inherits = FALSE)) {
    assign(key, .sacha_global_fetch_record(species), .sacha_global_cache)
  }
  get(key, .sacha_global_cache, inherits = FALSE)
}

.sacha_global_text_out <- function(x) {
  structure(as.character(x), class = c("sacha_text", "character"))
}

#' @export
#' @noRd
print.sacha_text <- function(x, ...) {
  cat(paste0(unclass(x), "\n"), sep = "")
  invisible(x)
}

.sacha_global_no_data_or_warn <- function(record, language = NULL) {
  if (isTRUE(record$transport_error)) {
    warning(sprintf("sacha global (%s): %s", .sacha_global_config$source_name,
                    .sacha_global_label("connection_error", language)), call. = FALSE)
    return(.sacha_global_text_out(.sacha_global_label("connection_error", language)))
  }
  .sacha_global_text_out(.sacha_global_label("no_data", language))
}

.sacha_global_rank_label <- function(rank, language = NULL) {
  code <- global_lang(language)
  rank <- tolower(trimws(rank))
  dictionary <- .sacha_global_lang_data(code)$ranks
  label <- .sacha_global_scalar(dictionary[rank], NA_character_)
  if (!length(label) || is.na(label) || !nzchar(label)) {
    paste0(toupper(substr(rank, 1L, 1L)), substr(rank, 2L, nchar(rank)))
  } else {
    label
  }
}

.sacha_global_markdown_escape <- function(x) {
  gsub("([\\\\`*_{}\\[\\]()<>#+.!|-])", "\\\\\\1", as.character(x), perl = TRUE)
}

.sacha_global_emit_md <- function(markdown) {
  if (!is.null(markdown) && length(markdown) && nzchar(markdown)) cat(markdown, "\n", sep = "")
  invisible(if (is.null(markdown)) "" else markdown)
}

.sacha_global_trim_period <- function(x) sub("[[:space:]]*[.]+$", "", trimws(x))

.sacha_fn_http_json <- function(url, query = list()) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required by the sacha module's ChecklistBank adapter.", call. = FALSE)
  }
  request <- httr2::request(url)
  request <- httr2::req_headers(request, Accept = "application/json")
  request <- httr2::req_user_agent(request, "sacha R module")
  request <- httr2::req_timeout(request, seconds = 30)
  request <- httr2::req_error(request, is_error = function(response) FALSE)
  request <- httr2::req_retry(request, max_tries = 3, is_transient = function(response) {
    httr2::resp_status(response) %in% c(429L, 500L, 502L, 503L, 504L)
  })
  query <- query[!vapply(query, is.null, logical(1))]
  if (length(query)) request <- do.call(httr2::req_url_query, c(list(request), query))
  response <- tryCatch(httr2::req_perform(request), error = function(e) NULL)
  if (is.null(response)) return(NULL)
  status <- httr2::resp_status(response)
  if (identical(status, 404L)) return(NULL)
  if (status < 200L || status >= 300L) return(NULL)
  tryCatch(httr2::resp_body_json(response, simplifyVector = FALSE), error = function(e) NULL)
}

.sacha_fn_checklist_get <- function(..., query = list()) {
  path <- vapply(c("dataset", .sacha_global_config$dataset, ...), .sacha_global_url_encode, character(1))
  .sacha_fn_http_json(paste(c(.sacha_global_config$base_url, path), collapse = "/"), query)
}

.sacha_fn_rows <- function(x) {
  if (is.null(x) || !length(x)) return(list())
  if (!is.list(x)) return(list())
  if (!is.null(x$result)) x <- x$result
  if (is.null(x) || !length(x)) return(list())
  if (!is.list(x)) return(list())
  fields <- c("id", "name", "scientificName", "rank", "usage", "property", "value", "language")
  if (!is.null(names(x)) && any(names(x) %in% fields)) return(list(x))
  if (is.null(names(x)) || all(grepl("^[0-9]+$", names(x)))) return(x)
  list(x)
}

.sacha_fn_first_field <- function(x, fields, default = NA_character_) {
  if (!is.list(x) || is.null(names(x))) return(default)
  lower_names <- tolower(names(x))
  for (field in fields) {
    index <- which(lower_names == tolower(field))[1L]
    if (!is.na(index)) {
      value <- .sacha_global_scalar(x[[index]], NA_character_)
      if (!is.na(value)) return(value)
    }
  }
  default
}

.sacha_fn_search_usage <- function(species) {
  candidates <- .sacha_fn_rows(.sacha_fn_checklist_get("nameusage", "search", query = list(q = species, limit = 100)))
  if (!length(candidates)) return(NULL)
  scientific <- vapply(candidates, .sacha_fn_first_field, character(1),
                       fields = c("scientificName", "name", "label"))
  status <- vapply(candidates, .sacha_fn_first_field, character(1), fields = "status")
  exact <- which(tolower(scientific) == tolower(species))
  accepted_exact <- exact[tolower(status[exact]) == "accepted"]
  selected <- if (length(accepted_exact)) accepted_exact[1L] else if (length(exact)) {
    exact[1L]
  } else {
    accepted <- which(tolower(status) == "accepted")
    if (length(accepted)) accepted[1L] else 1L
  }
  candidates[[selected]]
}

.sacha_fn_resolve_usage <- function(species) {
  match <- .sacha_fn_checklist_get("match", "nameusage", query = list(q = species, verbose = "false"))
  usage <- if (is.list(match)) match$usage else NULL
  if (is.null(usage) || !is.list(usage)) usage <- .sacha_fn_search_usage(species)
  if (is.null(usage)) return(NULL)
  id <- .sacha_fn_first_field(usage, c("id", "usage.id"))
  if (is.na(id)) return(NULL)
  list(
    id = id,
    name = .sacha_fn_first_field(usage, c("scientificName", "name", "label")),
    authorship = .sacha_fn_first_field(usage, c("authorship", "author"))
  )
}

.sacha_fn_fetch_record <- function(species) {
  resolution <- tryCatch(.sacha_fn_resolve_usage(species), error = function(e) NULL)
  if (is.null(resolution)) {
    return(list(scientific_name = species, authorship = NA_character_, retrieved_at = Sys.time()))
  }
  list(
    scientific_name = .sacha_global_scalar(resolution$name, species),
    authorship = .sacha_global_scalar(resolution$authorship, NA_character_),
    retrieved_at = Sys.time()
  )
}

.sacha_fn_record <- function(species, refresh = FALSE) {
  species <- .sacha_global_validate_species(species)
  key <- paste0("sc_full_name::", tolower(species))
  if (isTRUE(refresh) || !exists(key, envir = .sacha_global_cache, inherits = FALSE)) {
    assign(key, .sacha_fn_fetch_record(species), envir = .sacha_global_cache)
  }
  get(key, envir = .sacha_global_cache, inherits = FALSE)
}

.sacha_fn_markdown_escape_text <- function(x) {
  if (is.na(x)) return(x)
  escaped <- .sacha_global_markdown_escape(x)
  gsub("\\\\([()])", "\\1", escaped)
}

#' Full scientific name of a species
#'
#' Resolves `species` against the configured Catalogue of Life dataset in
#' ChecklistBank through `match/nameusage`, and falls back to
#' `nameusage/search` (`limit = 100`) when no direct match is returned. Among
#' the search candidates, the selected record is, in order of preference: an
#' exact case-insensitive name match with status `"accepted"`; any exact name
#' match; the first `"accepted"` candidate; the first candidate returned.
#' The resolution uses the ChecklistBank dataset and API configured with
#' [sacha_configure()].
#'
#' Results are cached in the same ChecklistBank session cache as the other
#' accessors and are cleared by [sacha_clear_cache()].
#'
#' Resolution failures never raise an error. Network or HTTP errors, timeouts,
#' malformed JSON, absence of a match, or a missing httr2 dependency
#' return `species` as given, without authorship. An error is raised only when
#' `species` is structurally invalid.
#'
#' @param species Character scalar with a scientific name of at least a genus
#'   and a specific epithet, for example `"Bidens andicola"`. Surrounding and
#'   repeated whitespace is removed. Vectors of length greater than one, `NA`
#'   and single-word names are rejected with an error.
#'
#' @return
#' - `sc_full_name()`: a character string with the scientific name followed by
#'   its authorship when available, for example `"Bidens andicola Kunth"`.
#' - `sc_full_name_md()`: invisibly, a level-two Markdown heading with the
#'   name in italics and the authorship in roman type, for example
#'   `"## *Bidens andicola* Kunth"`. Markdown-sensitive characters are escaped.
#'   The heading is also written to the console.
#'
#' @seealso [accessors-sacha]
#' @examples
#' \dontrun{
#' sc_full_name("Bidens andicola")
#'
#' sc_full_name_md("Bidens andicola")
#' }
#'
#' @export
sc_full_name <- function(species) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  record <- .sacha_fn_record(species)
  text <- if (is.na(record$authorship)) {
    record$scientific_name
  } else {
    paste(record$scientific_name, record$authorship)
  }
  .sacha_global_text_out(text)
}

#' @rdname sc_full_name
#' @export
sc_full_name_md <- function(species) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  record <- .sacha_fn_record(species)
  markdown <- paste0(
    "## *", .sacha_fn_markdown_escape_text(record$scientific_name), "*",
    if (!is.na(record$authorship)) paste0(" ", .sacha_fn_markdown_escape_text(record$authorship)) else ""
  )
  .sacha_global_emit_md(markdown)
}

#' First publication of a taxon
#'
#' Retrieves the bibliographic citation of the first publication reported by
#' the configured source for a species.
#'
#' @param species Character scalar with a scientific name of at least a genus
#'   and a specific epithet, for example `"Bidens andicola"`. Surrounding and
#'   repeated whitespace is removed. Vectors of length greater than one, `NA`
#'   and single-word names are rejected with an error.
#' @param language Language of labels and messages: a YAML basename in
#'   `inst/lang/` (for example `"en"` or `"es"`) or a declared alias (for
#'   example `"english"` or `"spanish"`). `NULL` (default) uses the language
#'   set with [sacha_configure()]. It does not translate the citation.
#'   Unsupported values raise an error.
#'
#' @return
#' - `first_publicacion()`: the citation as a character string, or a localized
#'   no-data or connection message.
#' - `first_publicacion_md()`: invisibly, a Markdown line with a localized
#'   "first published" label followed by the citation, or `""` when no
#'   citation is available. The line is also written to the console.
#'
#' @seealso [sacha_configure()], [accessors-sacha]
#' @examples
#' \dontrun{
#' first_publicacion("Bidens andicola")
#'
#' first_publicacion("Bidens andicola", language = "es")
#'
#' first_publicacion_md("Bidens andicola")
#' }
#' @export
first_publicacion <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  record <- .sacha_global_record(species)
  if (is.na(record$first_publication)) {
    .sacha_global_no_data_or_warn(record, language)
  } else {
    .sacha_global_text_out(record$first_publication)
  }
}

#' @rdname first_publicacion
#' @export
first_publicacion_md <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  citation <- .sacha_global_record(species)$first_publication
  if (is.na(citation)) return(.sacha_global_emit_md(""))
  .sacha_global_emit_md(paste0(.sacha_global_label("first_published", language), " ",
                               .sacha_global_markdown_escape(.sacha_global_trim_period(citation)), ".  \n"))
}

#' Taxonomic classification
#'
#' Retrieves the higher classification of a species from the configured
#' source, either as the complete lineage or as the name at a single rank. The
#' lineage follows the order provided by the source, and rank names are shown
#' with the labels of the selected language.
#'
#' @inheritParams first_publicacion
#' @param rank Optional taxonomic rank, in lowercase English as reported by the
#'   source. Typical values are `"domain"`, `"kingdom"`, `"phylum"`,
#'   `"class"`, `"order"`, `"family"`, `"genus"` and `"species"`, plus any
#'   intermediate rank present in the source (for example `"subfamily"`,
#'   `"tribe"` or `"subgenus"`). Matching is case-insensitive. `NULL`
#'   (default) uses the whole lineage.
#'
#' @return
#' - `classification()`: the lineage as `"Rank: Name > Rank: Name"`; with `rank`,
#'   only the name at that rank, without authorship. When no entry matches, a
#'   localized no-data or connection message.
#' - `classification_md()`: invisibly, the lineage in Markdown, or `""` when no
#'   entry matches. Each entry includes its authorship when known and is
#'   linked through `taxon_url_template` (see [sacha_configure()]) when
#'   the source provides a taxon identifier. With `rank`, only the entry at
#'   that rank is written. The output is also written to the console.
#'
#' @seealso [sacha_configure()], [accessors-sacha]
#' @examples
#' \dontrun{
#' classification("Bidens andicola")
#'
#' classification("Bidens andicola", rank = "family")
#'
#' classification("Bidens andicola", language = "es")
#'
#' classification_md("Bidens andicola")
#'
#' classification_md("Bidens andicola", rank = "genus")
#' }
#' @export
classification <- function(species, rank = NULL, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  record <- .sacha_global_record(species)
  entries <- record$classification
  if (!is.null(rank)) {
    rank <- tolower(.sacha_global_text(rank, "rank"))
    entries <- Filter(function(entry) identical(entry$rank, rank), entries)
    if (!length(entries)) return(.sacha_global_no_data_or_warn(record, language))
    return(.sacha_global_text_out(entries[[1L]]$name))
  }
  if (!length(entries)) return(.sacha_global_no_data_or_warn(record, language))
  .sacha_global_text_out(paste(vapply(entries, function(entry) {
    paste0(.sacha_global_rank_label(entry$rank, language), ": ", entry$name)
  }, character(1)), collapse = " > "))
}

.sacha_global_taxon_url <- function(id) {
  template <- .sacha_global_config$taxon_url_template
  if (is.null(template) || is.na(id)) return(NA_character_)
  sprintf(template, .sacha_global_url_encode(id))
}

#' @rdname classification
#' @export
classification_md <- function(species, rank = NULL, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  entries <- .sacha_global_record(species)$classification
  if (!is.null(rank)) {
    rank <- tolower(.sacha_global_text(rank, "rank"))
    entries <- Filter(function(entry) identical(entry$rank, rank), entries)
  }
  if (!length(entries)) return(.sacha_global_emit_md(""))
  output <- vapply(entries, function(entry) {
    name <- if (is.na(entry$authorship)) entry$name else paste(entry$name, entry$authorship)
    name <- .sacha_global_markdown_escape(name)
    url <- .sacha_global_taxon_url(entry$id)
    if (!is.na(url)) name <- paste0("[", name, "](", url, ")")
    paste0(.sacha_global_rank_label(entry$rank, language), ": ", name)
  }, character(1))
  .sacha_global_emit_md(paste0(
    "**", .sacha_global_label("classification_title", language), "**\n\n",
    paste(output, collapse = " > "), "  \n"
  ))
}

.sacha_global_common_names_text <- function(common_names, language = NULL) {
  if (!length(common_names)) return(character())
  vapply(names(common_names), function(name_language) {
    values <- paste(unique(common_names[[name_language]]), collapse = ", ")
    if (identical(name_language, "es")) values else {
      label <- if (identical(name_language, ".unspecified")) {
        .sacha_global_label("unspecified_language", language)
      } else {
        name_language
      }
      sprintf("%s (%s)", values, label)
    }
  }, character(1))
}

#' Common names
#'
#' Retrieves the vernacular names of a species from the configured source,
#' grouped by language. Names in Spanish are listed without a language label.
#' Names in other languages are followed by the language in parentheses, and
#' names without a declared language are marked with a localized
#' "unspecified" label. Language codes reported by the source are normalized
#' for Spanish (`"es"`); other
#' codes are kept as reported, with the country appended when available.
#' Groups are separated by `"; "`.
#'
#' @inheritParams first_publicacion
#'
#' @return
#' - `common_names()`: the names as a character string, or a localized no-data
#'   or connection message.
#' - `common_names_md()`: invisibly, a Markdown line with a bold localized
#'   "common names" label followed by the names, or `""` when no names are
#'   available. The line is also written to the console.
#'
#' @seealso [sacha_configure()], [accessors-sacha]
#' @examples
#' \dontrun{
#' common_names("Bidens andicola")
#'
#' common_names("Bidens andicola", language = "es")
#'
#' common_names_md("Bidens andicola")
#' }
#' @export
common_names <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  record <- .sacha_global_record(species)
  names_found <- .sacha_global_common_names_text(record$common_names, language)
  if (!length(names_found)) {
    .sacha_global_no_data_or_warn(record, language)
  } else {
    .sacha_global_text_out(paste(names_found, collapse = "; "))
  }
}

#' @rdname common_names
#' @export
common_names_md <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  names_found <- .sacha_global_common_names_text(.sacha_global_record(species)$common_names, language)
  if (!length(names_found)) return(.sacha_global_emit_md(""))
  .sacha_global_emit_md(paste0("**", .sacha_global_label("common_names", language), ":** ",
                               .sacha_global_markdown_escape(paste(names_found, collapse = "; ")), ".  \n"))
}

.sacha_global_italic_name <- function(name) {
  tokens <- strsplit(trimws(name), "\\s+")[[1L]]
  markers <- c("var.", "subsp.", "ssp.", "f.", "fo.", "subvar.", "subf.", "forma", "varietas",
               "subspecies", "\u00d7", "x", "nothosubsp.", "nothovar.", "agg.", "cv.")
  is_marker <- tolower(tokens) %in% markers
  pieces <- character()
  buffer <- character()
  flush <- function() {
    if (length(buffer)) {
      pieces <<- c(pieces, paste0("*", paste(.sacha_global_markdown_escape(buffer), collapse = " "), "*"))
      buffer <<- character()
    }
  }
  for (i in seq_along(tokens)) {
    if (is_marker[[i]]) {
      flush()
      pieces <- c(pieces, .sacha_global_markdown_escape(tokens[[i]]))
    } else {
      buffer <- c(buffer, tokens[[i]])
    }
  }
  flush()
  paste(pieces, collapse = " ")
}

.sacha_global_synonym_entry_text <- function(entry, group, markdown = FALSE) {
  name <- if (markdown) .sacha_global_italic_name(entry$name) else entry$name
  authorship <- if (is.na(entry$authorship)) "" else if (markdown) {
    .sacha_global_markdown_escape(entry$authorship)
  } else {
    entry$authorship
  }
  text <- trimws(paste(name, authorship))
  if (identical(group, "misapplied") && !is.na(entry$according_to)) {
    according <- if (markdown) .sacha_global_markdown_escape(entry$according_to) else entry$according_to
    text <- paste0(text, " sensu ", according)
  }
  text
}

.sacha_global_synonym_blocks <- function(synonyms, language = NULL, markdown = FALSE) {
  blocks <- list()
  for (group in .sacha_global_synonym_groups) {
    entries <- synonyms[[group]]
    if (!length(entries)) next
    plain <- vapply(entries, .sacha_global_synonym_entry_text, character(1),
                    group = group, markdown = FALSE)
    entries <- entries[order(tolower(plain), method = "radix")]
    items <- vapply(entries, .sacha_global_synonym_entry_text, character(1),
                    group = group, markdown = markdown)
    blocks[[group]] <- list(label = .sacha_global_label(paste0("syn_", group), language),
                            items = items)
  }
  blocks
}

#' Synonyms and combinations of a taxon
#'
#' Retrieves the nomenclatural synonyms and combinations associated with an
#' accepted species in the configured source, organized in the three groups of
#' the Catalogue of Life data model. Groups without entries are omitted.
#' Within each group, names are sorted alphabetically and duplicates are
#' removed. Group labels are taken from the language dictionaries (keys
#' `syn_homotypic`, `syn_heterotypic` and `syn_misapplied`).
#'
#' @section Groups:
#' - Homotypic synonyms (including combinations): names based on the same
#'   type specimen as the accepted name, such as the basionym, subsequent
#'   combinations in other genera and replacement names. They are objective
#'   synonyms.
#' - Heterotypic synonyms: names based on a different type, treated as the
#'   same taxon by the source. They are subjective (taxonomic) synonyms.
#' - Misapplied names: names that an author applied to the taxon but whose
#'   original type belongs to another taxon. When the source provides the
#'   author of the misapplication, it is appended after "sensu".
#'
#' @inheritParams first_publicacion
#'
#' @return
#' - `syn_and_comb()`: a character string with one block per group. Each block
#'   starts with the group label and lists one name per line, indented. Blocks
#'   are separated by a blank line. When no names are available, a localized
#'   no-data or connection message.
#' - `syn_and_comb_md()`: invisibly, a Markdown string with a bold localized
#'   heading, followed by one bulleted list per group. Binomials and
#'   infraspecific epithets are in italics; rank markers (for example `var.`
#'   or `subsp.`) and authorship are in roman type; Markdown special
#'   characters are escaped. `""` when no names are available. The string is
#'   also written to the console.
#'
#' @seealso [references_sp()], [sacha_configure()], [accessors-sacha]
#' @examples
#' \dontrun{
#' syn_and_comb("Bidens andicola")
#'
#' syn_and_comb("Bidens andicola", language = "es")
#'
#' syn_and_comb_md("Bidens andicola")
#' }
#' @export
syn_and_comb <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  record <- .sacha_global_record(species)
  blocks <- .sacha_global_synonym_blocks(record$synonyms, language, markdown = FALSE)
  if (!length(blocks)) return(.sacha_global_no_data_or_warn(record, language))
  text <- vapply(blocks, function(block) {
    paste(c(paste0(block$label, ":"), paste0("  ", block$items)), collapse = "\n")
  }, character(1))
  .sacha_global_text_out(paste(text, collapse = "\n\n"))
}

#' @rdname syn_and_comb
#' @export
syn_and_comb_md <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  blocks <- .sacha_global_synonym_blocks(.sacha_global_record(species)$synonyms, language, markdown = TRUE)
  if (!length(blocks)) return(.sacha_global_emit_md(""))
  text <- vapply(blocks, function(block) {
    paste0(block$label, ":\n\n", paste0("- ", block$items, collapse = "\n"))
  }, character(1))
  .sacha_global_emit_md(paste0("**", .sacha_global_label("syn_and_comb", language), ":**\n\n",
                               paste(text, collapse = "\n\n"), "\n"))
}

.sacha_global_bib_escape <- function(x) gsub("([&%$#_])", "\\\\\\1", x)

.sacha_global_csl_people <- function(people) {
  if (!is.list(people) || !length(people)) return(NA_character_)
  output <- vapply(people, function(person) {
    family <- .sacha_global_first_field(person, "family")
    given <- .sacha_global_first_field(person, "given")
    literal <- .sacha_global_first_field(person, "literal")
    if (!is.na(family) && !is.na(given)) paste0(family, ", ", given)
    else if (!is.na(family)) family
    else if (!is.na(literal)) paste0("{", literal, "}")
    else NA_character_
  }, character(1))
  output <- output[!is.na(output)]
  if (length(output)) paste(output, collapse = " and ") else NA_character_
}

.sacha_global_csl_year <- function(csl) {
  values <- .sacha_global_all_strings(csl[["issued"]])
  hits <- regmatches(values, regexpr("[0-9]{4}", values))
  if (length(hits)) hits[[1L]] else NA_character_
}

.sacha_global_bib_key <- function(reference) {
  csl <- reference$csl
  family <- NA_character_
  year <- NA_character_
  if (is.list(csl) && length(csl)) {
    authors <- csl[["author"]]
    if (is.list(authors) && length(authors)) {
      family <- .sacha_global_first_field(authors[[1L]], c("family", "literal"))
    }
    year <- .sacha_global_csl_year(csl)
  }
  base <- if (!is.na(family)) {
    paste0(family, if (!is.na(year)) year else "")
  } else if (!is.na(reference$id)) {
    reference$id
  } else {
    "reference"
  }
  base <- gsub("[^[:alnum:]]+", "", iconv(base, to = "ASCII//TRANSLIT"))
  if (is.na(base) || !nzchar(base)) "reference" else base
}

.sacha_global_bibtex_entry <- function(reference, key) {
  csl <- reference$csl
  has_csl <- is.list(csl) && length(csl) > 0L && !is.null(names(csl))
  type <- "misc"
  fields <- character()
  add <- function(field, value, escape = TRUE) {
    if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(value)) return(invisible(NULL))
    if (escape) value <- .sacha_global_bib_escape(value)
    fields <<- c(fields, sprintf("  %s = {%s}", field, value))
    invisible(NULL)
  }
  if (has_csl) {
    map <- c("article-journal" = "article", "article-magazine" = "article",
             "article-newspaper" = "article", "book" = "book", "chapter" = "incollection",
             "paper-conference" = "inproceedings", "report" = "techreport")
    csl_type <- tolower(.sacha_global_first_field(csl, "type"))
    if (!is.na(csl_type) && csl_type %in% names(map)) type <- map[[csl_type]]
    title <- .sacha_global_first_field(csl, "title")
    container <- .sacha_global_first_field(csl, "container-title")
    add("author", .sacha_global_csl_people(csl[["author"]]))
    add("editor", .sacha_global_csl_people(csl[["editor"]]))
    add("title", title)
    if (identical(type, "article")) add("journal", container)
    if (type %in% c("incollection", "inproceedings")) add("booktitle", container)
    add("year", .sacha_global_csl_year(csl))
    add("volume", .sacha_global_first_field(csl, "volume"))
    add("number", .sacha_global_first_field(csl, "issue"))
    add("pages", gsub("-+", "--", .sacha_global_first_field(csl, "page")))
    add("publisher", .sacha_global_first_field(csl, "publisher"))
    add("isbn", .sacha_global_first_field(csl, "ISBN"))
    add("issn", .sacha_global_first_field(csl, "ISSN"))
    add("doi", .sacha_global_first_field(csl, "DOI"), escape = FALSE)
    add("url", .sacha_global_first_field(csl, "URL"), escape = FALSE)
    if (is.na(title)) add("note", reference$citation)
  } else {
    year <- regmatches(reference$citation, regexpr("(1[5-9]|20)[0-9]{2}", reference$citation))
    add("year", if (length(year)) year[[1L]] else NA_character_)
    add("note", reference$citation)
  }
  if (!length(fields)) add("note", reference$citation)
  sprintf("@%s{%s,\n%s\n}", type, key, paste(fields, collapse = ",\n"))
}

.sacha_global_bibtex <- function(references) {
  keys <- make.unique(vapply(references, .sacha_global_bib_key, character(1)), sep = "_")
  entries <- vapply(seq_along(references), function(i) {
    .sacha_global_bibtex_entry(references[[i]], keys[[i]])
  }, character(1))
  paste(entries, collapse = "\n\n")
}

.sacha_global_bib_name <- function(record) {
  family <- NA_character_
  for (entry in record$classification) {
    if (identical(entry$rank, "family")) {
      family <- entry$name
      break
    }
  }
  tokens <- strsplit(record$scientific_name, " ", fixed = TRUE)[[1L]]
  if (length(tokens) < 2L) tokens <- strsplit(record$requested_species, " ", fixed = TRUE)[[1L]]
  if (is.na(family)) family <- "Unknown"
  parts <- c(family, tokens[[1L]], tokens[[2L]])
  paste(gsub("[^[:alnum:]]+", "_", parts), collapse = "_")
}

.sacha_global_save_bib <- function(record, directory, language = NULL) {
  if (!length(record$references)) return(invisible(NULL))
  name <- .sacha_global_bib_name(record)
  target <- file.path(.sacha_exports_root(directory), name)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(target)) stop(sprintf("Could not create the directory '%s'.", target), call. = FALSE)
  file <- file.path(target, paste0(name, ".bib"))
  writeLines(enc2utf8(c(.sacha_global_bibtex(record$references), "")), file, useBytes = TRUE)
  message(sprintf(.sacha_global_label("bib_saved", language),
                  normalizePath(file, winslash = "/", mustWork = FALSE)))
  invisible(file)
}

.sacha_global_references_record <- function(species, save_bib, directory, language) {
  if (!is.logical(save_bib) || length(save_bib) != 1L || is.na(save_bib)) {
    stop("`save_bib` must be TRUE or FALSE.", call. = FALSE)
  }
  record <- .sacha_global_record(species)
  if (save_bib) .sacha_global_save_bib(record, .sacha_global_text(directory, "directory"), language)
  record
}

.sacha_global_reference_citations <- function(record) {
  citations <- vapply(record$references, function(entry) entry$citation, character(1))
  citations[order(tolower(citations), method = "radix")]
}

#' References associated with a taxon
#'
#' Retrieves the bibliographic references that the configured source links to
#' a species, for example the sources of its taxonomic treatment, synonymy and
#' distribution. Duplicated citations are removed, whitespace inside each
#' citation is normalized, and citations are sorted alphabetically. The
#' references can also be exported to a BibTeX file.
#'
#' @section Saving references in BibTeX:
#' With `save_bib = TRUE`, the references are also written to a `.bib` file
#' inside a directory named after the species with the pattern
#' `Family_Genus_species`, for example `Asteraceae_Bidens_andicola`. The
#' directory is created under `directory` if it does not exist, and the file
#' has the same name as the directory plus the `.bib` extension:
#'
#' `<directory>/sacha_exports/Asteraceae_Bidens_andicola/Asteraceae_Bidens_andicola.bib`
#'
#' The family is taken from the classification returned by the source; if no
#' family is available, `Unknown` is used. Characters other than letters and
#' digits are replaced by underscores. An existing file with the same name is
#' overwritten.
#'
#' When the source provides structured bibliographic metadata (CSL-JSON), each
#' reference becomes a typed BibTeX entry (`@article`, `@book`,
#' `@incollection`, `@inproceedings`, `@techreport` or `@misc`) with authors,
#' title, container, year, volume, issue, pages, publisher, DOI and URL when
#' present. References without structured metadata are written as `@misc`
#' entries that keep the full citation in the `note` field. Citation keys are
#' built from the family name of the first author and the year, and are made
#' unique within the file. Nothing is written when the source has no
#' references.
#'
#' @inheritParams first_publicacion
#' @param save_bib Logical. `FALSE` (default) only returns or prints the
#'   references. `TRUE` also saves them in BibTeX format as described in
#'   *Saving references in BibTeX*. Any other value raises an error.
#' @param directory Common base directory for saved package files.
#'   `sacha_exports/Family_Genus_species` is created under this path when
#'   `save_bib = TRUE`. Defaults to the current working directory. It is
#'   ignored when `save_bib = FALSE`.
#'
#' @return
#' - `references_sp()`: a character string with one citation per line, or a
#'   localized no-data or connection message.
#' - `references_sp_md()`: invisibly, a Markdown string with a bold localized
#'   heading followed by a bulleted list with one citation per item, or `""`
#'   when no references are available. The string is also written to the
#'   console.
#'
#' @seealso [syn_and_comb()], [first_publicacion()], [sacha_configure()],
#'   [accessors-sacha]
#' @examples
#' \dontrun{
#' references_sp("Bidens andicola")
#'
#' references_sp("Bidens andicola", language = "es")
#'
#' references_sp("Bidens andicola", save_bib = TRUE)
#'
#' references_sp("Bidens andicola", save_bib = TRUE, directory = "bibliografia")
#'
#' references_sp_md("Bidens andicola")
#'
#' references_sp_md("Bidens andicola", save_bib = TRUE, directory = "bibliografia")
#' }
#' @export
references_sp <- function(species, language = NULL, save_bib = FALSE, directory = getwd()) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  record <- .sacha_global_references_record(species, save_bib, directory, language)
  if (!length(record$references)) return(.sacha_global_no_data_or_warn(record, language))
  .sacha_global_text_out(paste(.sacha_global_reference_citations(record), collapse = "\n"))
}

#' @rdname references_sp
#' @export
references_sp_md <- function(species, language = NULL, save_bib = FALSE, directory = getwd()) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  record <- .sacha_global_references_record(species, save_bib, directory, language)
  if (!length(record$references)) return(.sacha_global_emit_md(""))
  items <- paste0("- ", .sacha_global_markdown_escape(.sacha_global_reference_citations(record)))
  .sacha_global_emit_md(paste0("**", .sacha_global_label("references", language), ":**\n\n",
                               paste(items, collapse = "\n"), "\n"))
}

.sacha_global_registry_alias <- function(identifier, available = character()) {
  identifier <- tolower(.sacha_global_text(identifier, "identifier"))
  aliases <- c("catalogue of life" = "col", catalogue_of_life = "col",
               inaturalist = "inat", "i naturalist" = "inat",
               world_flora_online = "wfo", the_plant_list = "tpl")
  if (identifier %in% names(aliases)) identifier <- aliases[[identifier]]
  if (!(identifier %in% c(names(.sacha_global_registry), available))) {
    stop("Unknown `identifier` for the configured source.", call. = FALSE)
  }
  identifier
}

.sacha_global_index_table <- function(record) {
  rows <- list()
  if (length(record$links)) for (key in names(record$links)) {
    rows[[length(rows) + 1L]] <- data.frame(identifier = tolower(key), platform = key,
                                            url = record$links[[key]], stringsAsFactors = FALSE)
  }
  if (length(record$identifiers)) for (key in names(record$identifiers)) {
    spec <- .sacha_global_registry[[key]]
    if (is.null(spec)) next
    rows[[length(rows) + 1L]] <- data.frame(
      identifier = key, platform = spec$platform,
      url = sprintf(spec$url, .sacha_global_url_encode(record$identifiers[[key]])), stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(data.frame(identifier = character(), platform = character(), url = character()))
  output <- do.call(rbind, rows)
  output <- output[!duplicated(output$url), , drop = FALSE]
  rownames(output) <- NULL
  output
}

.sacha_global_index_urls <- function(record, identifier = NULL) {
  table <- .sacha_global_index_table(record)
  if (!is.null(identifier)) {
    identifier <- .sacha_global_registry_alias(identifier, table$identifier)
    table <- table[table$identifier == identifier, , drop = FALSE]
  }
  output <- table$url
  names(output) <- table$platform
  output
}

#' Indexation URLs
#'
#' Retrieves the URLs of the pages that describe a species in external
#' taxonomic platforms. URLs are built from the identifiers returned by the
#' configured source and from the links supplied by a custom `fetcher` (see
#' [sacha_configure()]). The default adapter provides identifiers for
#' Catalogue of Life, GBIF and iNaturalist, and for other platforms when the
#' source records them. The platform shortcuts (`ipni_url()`, `gbif_url()`,
#' `inat_url()`, `col_url()`, `wfo_url()`, `tpl_url()`, `itis_url()`,
#' `ncbi_url()`, `irmng_url()`, `worms_url()` and `eol_url()`) are documented
#' on this page.
#'
#' @section Registry codes:
#' The values accepted by `identifier` are:
#' - `"col"`: Catalogue of Life.
#' - `"gbif"`: GBIF.
#' - `"inat"`: iNaturalist.
#' - `"ipni"`: International Plant Names Index.
#' - `"wfo"`: World Flora Online.
#' - `"tpl"`: The Plant List.
#' - `"itis"`: Integrated Taxonomic Information System.
#' - `"ncbi"`: NCBI Taxonomy.
#' - `"irmng"`: Interim Register of Marine and Nonmarine Genera.
#' - `"worms"`: World Register of Marine Species.
#' - `"eol"`: Encyclopedia of Life.
#'
#' The aliases `"catalogue of life"`, `"catalogue_of_life"`, `"inaturalist"`,
#' `"i naturalist"`, `"world_flora_online"` and `"the_plant_list"` are also
#' accepted. Matching is case-insensitive.
#'
#' @section Platform shortcuts:
#' Each shortcut is equivalent to calling `indexations()` with the
#' corresponding `identifier` and accepts only `species` and `language`:
#' `ipni_url()` (IPNI), `gbif_url()` (GBIF), `inat_url()` (iNaturalist),
#' `col_url()` (Catalogue of Life), `wfo_url()` (World Flora Online),
#' `tpl_url()` (The Plant List), `itis_url()` (ITIS), `ncbi_url()` (NCBI
#' Taxonomy), `irmng_url()` (IRMNG), `worms_url()` (WoRMS) and `eol_url()`
#' (Encyclopedia of Life).
#'
#' @inheritParams first_publicacion
#' @param identifier Optional platform to keep: one of the registry codes
#'   listed in *Registry codes*, one of the accepted aliases, or the name of a
#'   link supplied by a custom `fetcher`. `NULL` (default) returns every
#'   available platform. Unknown values raise an error.
#'
#' @return
#' - `indexations()` and the platform shortcuts: a named character vector
#'   of URLs, with platform names as names; empty when no URL is available.
#' - `indexations_md()`: invisibly, a Markdown line with a bold localized
#'   "indexed in" label followed by one link per platform, or `""` when no URL
#'   is available. The line is also written to the console.
#'
#' @seealso [sacha_configure()], [accessors-sacha]
#' @examples
#' \dontrun{
#' indexations("Bidens andicola")
#'
#' indexations("Bidens andicola", identifier = "gbif")
#'
#' indexations("Bidens andicola", identifier = "catalogue of life")
#'
#' indexations_md("Bidens andicola")
#'
#' indexations_md("Bidens andicola", identifier = "inat", language = "es")
#'
#' gbif_url("Bidens andicola")
#'
#' ipni_url("Bidens andicola")
#' }
#' @export
indexations <- function(species, identifier = NULL, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  record <- .sacha_global_record(species)
  output <- .sacha_global_index_urls(record, identifier)
  if (!length(output) && isTRUE(record$transport_error)) .sacha_global_no_data_or_warn(record, language)
  output
}

#' @rdname indexations
#' @export
indexations_md <- function(species, identifier = NULL, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  urls <- .sacha_global_index_urls(.sacha_global_record(species), identifier)
  if (!length(urls)) return(.sacha_global_emit_md(""))
  links <- paste0("[", .sacha_global_markdown_escape(names(urls)), "](", urls, ")")
  .sacha_global_emit_md(paste0("**", .sacha_global_label("indexed_in", language), ":** ",
                               paste(links, collapse = ", "), ".  \n"))
}

#' @rdname indexations
#' @export
ipni_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "ipni", language)
}
#' @rdname indexations
#' @export
gbif_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "gbif", language)
}
#' @rdname indexations
#' @export
inat_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "inat", language)
}
#' @rdname indexations
#' @export
col_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "col", language)
}
#' @rdname indexations
#' @export
wfo_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "wfo", language)
}
#' @rdname indexations
#' @export
tpl_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "tpl", language)
}
#' @rdname indexations
#' @export
itis_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "itis", language)
}
#' @rdname indexations
#' @export
ncbi_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "ncbi", language)
}
#' @rdname indexations
#' @export
irmng_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "irmng", language)
}
#' @rdname indexations
#' @export
worms_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "worms", language)
}
#' @rdname indexations
#' @export
eol_url <- function(species, language = NULL) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  indexations(species, "eol", language)
}
