.sacha_spatial_gbif_checklist_key <- function() {
  key <- .sacha_spatial_config$gbif_checklist_key
  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(trimws(key))) {
    "7ddf754f-d193-4cc9-b351-99906754a03b"
  } else {
    trimws(key)
  }
}

.sacha_spatial_gbif_taxon <- function(species) {
  .sacha_spatial_require("rgbif")
  species <- .sacha_spatial_validate_species(species)
  checklist_key <- .sacha_spatial_gbif_checklist_key()
  key <- .sacha_spatial_cache_key(species)
  cached <- .sacha_spatial_cache_get(key)
  if (!is.null(cached) && !is.null(cached$taxon) &&
      identical(cached$taxon$status, "ok") &&
      identical(cached$taxon$checklist_key, checklist_key) &&
      .sacha_spatial_cache_fresh(cached$taxon$fetched_at)) {
    return(cached$taxon)
  }

  response <- tryCatch(
    .sacha_spatial_with_retry(
      function() rgbif::name_backbone(
        name = species, rank = "species", strict = TRUE,
        checklistKey = checklist_key
      ),
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
  if (is.na(taxon_key) || !identical(match_type, "EXACT") || !identical(rank, "SPECIES")) {
    return(list(
      status = "error",
      message = sprintf(
        "GBIF could not resolve '%s' as an exact species-level name (match type: %s; rank: %s).",
        species, ifelse(is.na(match_type), "unavailable", match_type),
        ifelse(is.na(rank), "unavailable", rank)
      ),
      fetched_at = Sys.time(), error_type = "taxonomy"
    ))
  }
  accepted_name <- field("scientificName")
  if (is.na(accepted_name)) accepted_name <- field("canonicalName")
  result <- list(
    status = "ok", key = taxon_key, input_name = species,
    accepted_name = accepted_name %spatial||% species,
    match_type = match_type, rank = rank,
    checklist_key = checklist_key,
    fetched_at = Sys.time()
  )
  cached <- cached %spatial||% list(species = species)
  cached$taxon <- result
  .sacha_spatial_cache_set(key, cached)
  result
}

.sacha_spatial_gbif_occurrences <- function(species, limit = NULL) {
  .sacha_spatial_require("rgbif")
  if (utils::packageVersion("rgbif") < "3.9.0") {
    stop("`rgbif` >= 3.9.0 is required so that the taxonomy used for GBIF occurrence searches (COL Extended Release) is explicit and reproducible.",
         call. = FALSE)
  }
  species <- .sacha_spatial_validate_species(species)
  if (!is.null(limit)) limit <- .sacha_spatial_validate_limit(limit)
  key <- .sacha_spatial_cache_key(species)
  cached <- .sacha_spatial_cache_get(key)
  taxon <- .sacha_spatial_gbif_taxon(species)
  if (!identical(taxon$status, "ok")) {
    return(list(
      occurrences = data.frame(), requested_limit = limit %spatial||% Inf,
      status = "error", message = taxon$message, fetched_at = Sys.time(),
      error_type = taxon$error_type, taxon = taxon
    ))
  }

  if (!is.null(limit) && !is.null(cached) && !is.null(cached$gbif) &&
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
    if (!is.null(limit)) {
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
    if (!is.null(limit)) {
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
    taxon = taxon
  )
  if (!is.null(limit)) {
    cached$gbif <- result
    .sacha_spatial_cache_set(key, cached)
  }
  result
}

.sacha_spatial_source <- function(source) {
  match.arg(source, c("gbif", "local"))
}

.sacha_spatial_local_export <- function(local_export) {
  match.arg(local_export, c("species", "crop"))
}

.sacha_spatial_path_component <- function(x) {
  .sacha_path_component(x)
}

.sacha_spatial_exports_root <- function(directory) {
  .sacha_exports_root(directory)
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
                                    taxon = NULL) {
  species <- .sacha_spatial_validate_species(species)
  metadata <- list(
    schema_version = 2L,
    source = "GBIF occurrence search API",
    source_url = "https://api.gbif.org/v1/occurrence/search",
    species = species,
    query = list(
      scientificName = species,
      checklistKey = .sacha_spatial_gbif_checklist_key(),
      taxonKey = if (is.null(taxon)) NULL else taxon$key,
      nameMatch = if (is.null(taxon)) NULL else list(
        inputName = taxon$input_name,
        acceptedName = taxon$accepted_name,
        matchType = taxon$match_type,
        rank = taxon$rank
      ),
      hasCoordinate = TRUE,
      geometry = if (is.null(crop)) NULL else crop$place
    ),
    retrieved_at = format(Sys.time(), tz = "", usetz = TRUE),
    coordinate_reference_system = "EPSG:4326",
    record_count = nrow(occurrences),
    columns = names(occurrences),
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
    metadata$derived_from <- list(
      file = "occurrences.gpkg",
      record_count = parent$record_count,
      retrieved_at = parent$retrieved_at
    )
    metadata$processed_at <- format(Sys.time(), tz = "", usetz = TRUE)
  }
  metadata
}

.sacha_spatial_save_local_occurrences <- function(species, occurrences, directory,
                                                  overwrite, crop = NULL,
                                                  boundary = NULL,
                                                  taxon_directory = NULL,
                                                  parent = NULL, taxon = NULL) {
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
    .sacha_spatial_metadata(species, occurrences, crop, parent, taxon), metadata_path,
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

.sacha_spatial_local_taxon_directories <- function(species, directory) {
  .sacha_spatial_require("jsonlite")
  species <- .sacha_spatial_validate_species(species)
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
      if (identical(metadata$species, species)) return(TRUE)
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
        identical(candidate_metadata$species, species) &&
          !is.null(candidate_metadata$crop)
      }, logical(1)))
    }, logical(1))
    candidates[matches]
  }
  matches <- locate(.sacha_spatial_exports_root(directory))
  if (length(matches) > 0L) return(matches)
  locate(file.path(normalizePath(directory, winslash = "/", mustWork = FALSE), "spatial_exports"))
}

.sacha_spatial_load_local_occurrences <- function(species, directory, crop_place = NULL,
                                                  crop_only = FALSE) {
  taxon_directories <- .sacha_spatial_local_taxon_directories(species, directory)
  if (length(taxon_directories) != 1L) {
    stop(sprintf("Could not identify one local occurrence export for '%s' in '%s'.",
                 species, .sacha_spatial_exports_root(directory)), call. = FALSE)
  }
  target <- taxon_directories
  if (isTRUE(crop_only) && is.null(crop_place)) {
    crop_directories <- list.dirs(target, recursive = FALSE, full.names = TRUE)
    crop_directories <- crop_directories[file.exists(file.path(crop_directories, "occurrences.gpkg"))]
    if (length(crop_directories) != 1L) {
      stop(sprintf("Specify `kml` to identify one local cropped export for '%s'.", species),
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
                 species, target),
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
      kml = NULL,
      crop_area = NULL,
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
    stop("`crop_box` does not overlap the geographic boundary defined by `kml` or `crop_area`.",
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
                                        file_prefix = "gbif_map", legend_place = NULL) {
  .sacha_spatial_require("ggplot2")
  static_map <- .sacha_spatial_static_map(
    occurrences, colour, zoom, boundary = boundary, bbox = bbox,
    legend_place = legend_place
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

.sacha_spatial_kml_place_name <- function(kml_path) {
  name <- tools::file_path_sans_ext(basename(kml_path))
  name <- gsub("[_\\-]+", " ", name)
  trimws(name)
}

.sacha_spatial_kml_boundary <- function(kml_path) {
  .sacha_spatial_require("sf")
  if (!is.character(kml_path) || length(kml_path) != 1L || is.na(kml_path) ||
      !nzchar(trimws(kml_path))) {
    stop("`kml` must be a single path to a .kml file.", call. = FALSE)
  }
  if (!file.exists(kml_path)) {
    stop(sprintf("`kml` file was not found: %s", kml_path), call. = FALSE)
  }

  boundary <- tryCatch(
    sf::st_read(kml_path, quiet = TRUE),
    error = function(e) {
      stop(sprintf("Could not read KML file '%s': %s.", kml_path, conditionMessage(e)),
           call. = FALSE)
    }
  )
  if (!nrow(boundary)) {
    stop(sprintf("KML file '%s' does not contain any geometry.", kml_path), call. = FALSE)
  }
  if (is.na(sf::st_crs(boundary))) {
    warning(sprintf(
      .sacha_spatial_label("warn_kml_no_crs"), kml_path
    ), call. = FALSE)
    sf::st_crs(boundary) <- 4326
  } else {
    boundary <- sf::st_transform(boundary, 4326)
  }

  extent <- sf::st_bbox(boundary)
  list(
    boundary = boundary,
    bbox = extent,
    place = .sacha_spatial_kml_place_name(kml_path)
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
    language = language, place = place
  )
  if (is.null(fig_width)) fig_width <- report$width
  if (is.null(fig_height)) fig_height <- report$height
  report$width <- fig_width
  report$height <- fig_height
  .sacha_spatial_emit_report_map(map, report)
}

#' Generate a full-extent GBIF occurrence map
#'
#' `gbif_map()` creates a standalone map of the georeferenced occurrences of a
#' species, and `gbif_map_md()` creates the same map for a markdown-formatted
#' report (Quarto or R Markdown).
#'
#' With `source = "gbif"` (default), the supplied binomial is first resolved
#' by GBIF as an exact, species-level name in the configured checklist (COL
#' Extended Release by default). The resulting taxon key and the same
#' checklist key are used to retrieve records with coordinates. This prevents
#' approximate name matches and keeps records tied to one accepted GBIF
#' taxonomic concept. With
#' `source = "local"`, records are read from an export previously written by
#' `save_occurrences = TRUE` or by [gbif_save_occurrences()], and GBIF is not
#' contacted. Local exports are stored under
#' `sacha_exports/Family_Genus_species` inside `directory`.
#'
#' Setting `save_occurrences = TRUE` retrieves every available georeferenced
#' record (independently of `limit`) and stores it as a GeoPackage in WGS 84
#' (EPSG:4326) with all GBIF fields, together with a JSON file describing the
#' exact name match, taxon key, query, retrieval date, coordinate reference
#' system, and record count. When
#' `source = "local"`, the records already reside in a saved export and
#' nothing is written.
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
#'   variants and higher-rank matches are rejected.
#' @param limit Maximum number of georeferenced occurrences used to draw the
#'   map, an integer between `1` and `100000`, e.g. `100`, `500`, or `2000`.
#'   With a local source, the first `limit` records of the saved export are
#'   used. Defaults to `100000`.
#' @param mode Map type: `"interactive"` (default; a Leaflet widget) or
#'   `"static"` (a ggplot2 map). In `gbif_map_md()`, `NULL` selects an
#'   interactive map for HTML output and a static map for other formats.
#' @param save_occurrences Logical. If `TRUE`, the complete set of
#'   georeferenced records for the same validated GBIF taxon key is saved as
#'   `occurrences.gpkg` with its provenance file `metadata.json` under
#'   `sacha_exports/Family_Genus_species`. Defaults to `FALSE`.
#' @param export_image Logical. If `TRUE`, the static map is written as a PNG
#'   file to `sacha_exports/Family_Genus_species`. Defaults to `FALSE`.
#' @param directory Base directory in which `sacha_exports` is created or,
#'   for a local source, searched. Occurrences and exported images are saved
#'   under `sacha_exports/Family_Genus_species`. Defaults to [getwd()].
#' @param colour Colour of the occurrence points as a hexadecimal code with 3,
#'   4, 6, or 8 digits, e.g. `"#1B9E77"`. When `NULL`, the package default
#'   colour is used.
#' @param zoom_static Logical. Should static maps be framed on the extent of
#'   the occurrences? It applies to static maps and exported images. When
#'   `NULL`, the package default is used.
#' @param dpi Resolution of the exported PNG in dots per inch. When `NULL`,
#'   the package default resolution is used.
#' @param ppp Spanish-language synonym of `dpi` (puntos por pulgada). If both
#'   are supplied, `ppp` takes precedence.
#' @param overwrite Logical. Should existing output files be replaced? If
#'   `FALSE`, an existing local occurrence export raises an error. Defaults to
#'   `TRUE`.
#' @param source Occurrence source: `"gbif"` (default) retrieves records from
#'   GBIF; `"local"` reads a saved export from `sacha_exports` under
#'   `directory`.
#' @param fig_width,fig_height Figure dimensions in inches, used only by
#'   `gbif_map_md()`. When `NULL`, dimensions are chosen from the aspect ratio
#'   of the map.
#' @return A Leaflet widget or a ggplot2 object. Paths of the written files
#'   are stored in the `"sacha_files"` attribute, the plotted records in
#'   `"sacha_occurrences"`, and the species in `"sacha_species"`.
#'   `gbif_map_md()` returns the map formatted for the report output.
#' @examples
#' \donttest{
#' map <- gbif_map("Bidens andicola", limit = 100)
#' map
#' }
#' \dontrun{
#' gbif_map("Bidens andicola", save_occurrences = TRUE, export_image = TRUE)
#' gbif_map("Bidens andicola", source = "local", mode = "static")
#'
#' gbif_map_md("Bidens andicola", source = "gbif", mode = "interactive")
#' gbif_map_md("Bidens andicola", source = "gbif", mode = "static",
#'             limit = 500, export_image = TRUE, save_occurrences = TRUE)
#' gbif_map_md("Bidens andicola", source = "local", mode = "static",
#'             limit = 500, directory = getwd(), export_image = TRUE,
#'             colour = "#1B9E77", zoom_static = TRUE, dpi = 300,
#'             overwrite = TRUE, fig_width = 8, fig_height = 6)
#' }
#' @export
gbif_map <- function(species,
                     limit = 100000L,
                     mode = c("interactive", "static"),
                     save_occurrences = FALSE,
                     export_image = FALSE,
                     directory = getwd(),
                     colour = NULL,
                     zoom_static = NULL,
                     dpi = NULL,
                     ppp = NULL,
                     overwrite = TRUE,
                     source = c("gbif", "local")) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  species <- .sacha_spatial_validate_species(species)
  language <- .sacha_spatial_language()
  limit <- .sacha_spatial_validate_limit(limit)
  if (!is.null(ppp)) dpi <- ppp
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
  if (identical(source, "gbif")) {
    occ_result <- .sacha_spatial_gbif_occurrences(species, limit)
    if (identical(occ_result$status, "error")) {
      .sacha_spatial_stop_gbif_error(occ_result, species, language)
    }
    occurrences <- occ_result$occurrences
  } else {
    local_species <- .sacha_spatial_load_local_occurrences(species, directory)
    occurrences <- utils::head(local_species$occurrences, limit)
  }
  if (!nrow(occurrences)) {
    stop(paste(.sacha_spatial_label("no_occurrences", language), species), call. = FALSE)
  }

  taxon_directory <- .sacha_spatial_taxon_directory(species, occurrences, directory)
  local_files <- character()
  if (isTRUE(save_occurrences)) {
    if (identical(source, "gbif")) {
      complete_result <- .sacha_spatial_gbif_occurrences(species)
      if (identical(complete_result$status, "error")) {
        .sacha_spatial_stop_gbif_error(complete_result, species, language)
      }
      local_files <- .sacha_spatial_save_local_occurrences(
        species, complete_result$occurrences, directory, overwrite,
        taxon_directory = taxon_directory, taxon = complete_result$taxon
      )
    } else {
      warning("`save_occurrences` has no effect: the occurrences were read from a saved local export.",
              call. = FALSE)
    }
  }

  image_files <- if (isTRUE(export_image)) {
    .sacha_spatial_export_image(
      occurrences, species, taxon_directory, colour, zoom_static, dpi, overwrite,
      file_prefix = "gbif_map"
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
      boundary = NULL, bbox = NULL
    )
  }
  attr(map, "sacha_occurrences") <- occurrences
  attr(map, "sacha_files") <- c(image_files, local_files)
  attr(map, "sacha_species") <- species
  attr(map, "sacha_map_place") <- NULL
  map
}

#' @rdname gbif_map
#' @export
gbif_map_md <- function(species,
                        limit = 100000L,
                        mode = NULL,
                        save_occurrences = FALSE,
                        export_image = FALSE,
                        directory = getwd(),
                        colour = NULL,
                        zoom_static = NULL,
                        dpi = NULL,
                        ppp = NULL,
                        overwrite = TRUE,
                        fig_width = NULL,
                        fig_height = NULL,
                        source = c("gbif", "local")) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  species <- .sacha_spatial_validate_species(species)
  language <- .sacha_spatial_language()
  mode <- .sacha_spatial_resolve_report_mode(mode)
  map <- gbif_map(
    species = species,
    limit = limit,
    mode = mode,
    save_occurrences = save_occurrences,
    export_image = export_image,
    directory = directory,
    colour = colour,
    zoom_static = zoom_static,
    dpi = dpi,
    ppp = ppp,
    overwrite = overwrite,
    source = source
  )
  .sacha_spatial_emit_md_map(
    map = map, variant = attr(map, "sacha_map_frame")$variant,
    mode = mode, frame = attr(map, "sacha_map_frame"), species = species,
    language = language, place = attr(map, "sacha_map_place"),
    fig_width = fig_width, fig_height = fig_height
  )
}

#' Save traceable GBIF occurrences for local mapping
#'
#' Resolves `species` as an exact, species-level GBIF name in the configured
#' checklist (COL Extended Release by default) and retrieves georeferenced
#' records using the resulting taxon key and the same checklist key. By
#' default, up to 100,000 records are retrieved. Set `limit = NULL` to retrieve
#' all available records. A finite limit is applied before any geographic
#' crop. The function
#' can optionally restrict records to a KML boundary, a named geographic area,
#' or a WGS 84 bounding box. It saves the returned GBIF fields, machine-readable
#' metadata including the exact name match and taxon key, and, for a crop, the
#' corresponding WGS 84 boundary. Full-extent exports can be used offline with
#' `source = "local"` in [gbif_map()] or [gbif_map_crop()]. A cropped export can
#' be used with [gbif_map_crop()] by selecting `source = "local"`,
#' `local_export = "crop"`, and the same crop arguments used when saving it.
#' When both functions use the same taxon, occurrence set, and crop selectors,
#' their saved occurrence records are interchangeable as local input. The
#' default limit here is 100,000; use `limit = NULL` when the complete available
#' occurrence set is required.
#'
#' Full-extent records are saved to
#' `sacha_exports/Family_Genus_species`; cropped records are saved to
#' `sacha_exports/Family_Genus_species/Place`, where `Place` is derived from
#' the KML file name, the resolved geographic area, or the crop-box label (or
#' its coordinates when no label is supplied).
#'
#' @param species A binomial scientific name, for example `"Bidens andicola"`.
#'   GBIF must resolve it as an exact species-level name; approximate or
#'   higher-rank matches are not used.
#' @param kml Optional path to a `.kml` file used to restrict occurrences to a
#'   geographic area. It cannot be combined with `crop_area`.
#' @param crop_area,crop_box Geographic crop selectors shared with
#'   [gbif_map_crop()]. They use the same area resolution, WGS 84 coordinate
#'   order, label, and overlap rules. Here they filter the records being saved
#'   and determine the `Place` subdirectory. `crop_area` cannot be combined
#'   with `kml`; `crop_box` can be used alone or with either boundary input.
#' @param limit Maximum number of georeferenced records requested from GBIF
#'   before spatial filtering, as a positive integer from `1` to `100000`.
#'   Defaults to `100000`, as in [gbif_map_crop()]. Set to `NULL` to retrieve
#'   all available records. A saved crop may contain fewer records than a
#'   finite `limit` because the limit is applied before spatial filtering.
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
#' gbif_save_occurrences("Bidens andicola", kml = "Cotopaxi.kml")
#' gbif_save_occurrences(
#'   "Bidens andicola",
#'   crop_box = c(-75, -10, -70, -5, "study area"),
#'   limit = 5000
#' )
#' gbif_map("Bidens andicola", source = "local")
#' }
#' @export
gbif_save_occurrences <- function(species,
                                  kml = NULL,
                                  crop_area = NULL,
                                  crop_box = NULL,
                                  limit = 100000L,
                                  directory = getwd(),
                                  overwrite = TRUE) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  species <- .sacha_spatial_validate_species(species)
  language <- .sacha_spatial_language()
  limit <- if (is.null(limit)) NULL else .sacha_spatial_validate_limit(limit)
  overwrite <- .sacha_spatial_validate_logical(overwrite, "overwrite")
  crop_window <- .sacha_spatial_crop_box_parse(crop_box)
  box_only <- !is.null(crop_window) && is.null(kml) && is.null(crop_area)
  crop_data <- if (box_only) {
    .sacha_spatial_apply_crop_box(NULL, crop_window)
  } else if (!is.null(kml) || !is.null(crop_area)) {
    boundary <- .sacha_spatial_map_crop_boundary(
      kml = kml, crop_area = crop_area, language = language
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
  result <- .sacha_spatial_gbif_occurrences(species, limit = limit)
  if (identical(result$status, "error")) {
    .sacha_spatial_stop_gbif_error(result, species, language)
  }
  occurrences <- result$occurrences
  if (!is.null(crop_data)) {
    occurrences <- .sacha_spatial_filter_crop(occurrences, crop_data)
  }
  if (!nrow(occurrences)) {
    stop(paste(.sacha_spatial_label("no_occurrences", language), species), call. = FALSE)
  }
  crop_metadata <- if (is.null(crop_data)) {
    NULL
  } else {
    metadata <- list(place = crop_data$place)
    if (!is.null(crop_data$kml)) metadata$kml <- crop_data$kml
    if (!is.null(crop_data$crop_area)) metadata$crop_area <- crop_data$crop_area
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
    taxon = result$taxon
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
#' The area to map is defined with `kml`, `crop_area`, or `crop_box`. `kml` and
#' `crop_area` are mutually exclusive and define a boundary that is drawn as
#' the map outline; `crop_box` defines a rectangle and can be used alone or
#' together with either of them. With `distribution = "local"` (default), only
#' records inside the area are mapped. With `distribution = "global"`, all
#' available georeferenced records are mapped and the boundary, when there is
#' one, is kept as a reference outline.
#'
#' When `crop_box` is combined with `kml` or `crop_area`, the boundary is
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
#' With `source = "gbif"` (default), the supplied binomial is first resolved
#' as an exact, species-level name in the configured GBIF checklist (COL
#' Extended Release by default). Its taxon key and that same checklist key are
#' then used for the occurrence query, preventing approximate or higher-rank
#' name matches. With `source = "local"`, GBIF is not contacted and
#' records are read from a saved export under `sacha_exports` in
#' `directory`, in one of two ways set by `local_export`:
#' \itemize{
#'   \item `"species"` (default): the full-extent export of the species
#'     (`sacha_exports/Family_Genus_species`), produced by
#'     `save_occurrences = TRUE` in [gbif_map()] or by
#'     [gbif_save_occurrences()]. The area is applied to these records
#'     exactly as it is applied to records retrieved from GBIF. This is the
#'     only option compatible with `distribution = "global"`.
#'   \item `"crop"`: an export previously saved for the same area
#'     (`sacha_exports/Family_Genus_species/Place`), which contains the
#'     filtered records and the stored boundary. It requires
#'     `distribution = "local"`. When the export was saved with a `crop_box`,
#'     the same `crop_box` (and therefore the same label) must be supplied to
#'     identify it.
#' }
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
#' rectangle itself. Records are not truncated by `limit`.
#' Three files are written: `occurrences.gpkg` (records in WGS 84, EPSG:4326),
#' `boundary.gpkg`, and `metadata.json` (query, retrieval date, coordinate
#' reference system, and record count). With `distribution = "global"` the map
#' still shows all records, but only those inside the area are saved; if
#' none falls inside, nothing is saved and a warning is issued. When the
#' records come from the full-extent local export, the metadata retain the
#' original GBIF provenance and add a `derived_from` entry and the processing
#' date, so that a crop produced offline has the same traceability as one
#' produced from GBIF. If the records were read from a cropped local export,
#' they are already saved and nothing is written.
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
#' `fig-gbif-map-crop-bidens_andicola-ecuador_continental`. A KML map uses its
#' resolved place name. Use the resulting ID as `@fig-gbif-map-crop-...` in
#' report prose; `distribution`, `source`, and `mode` do not change the ID.
#' For generated Markdown figures, set `results: asis` on the Quarto or
#' R Markdown chunk.
#'
#' @param species A binomial scientific name in the form `"Genus species"`,
#'   e.g. `"Bidens andicola"`. GBIF must resolve it as an exact species-level
#'   name; approximate and higher-rank matches are rejected.
#' @param kml Path to a `.kml` file whose geometry defines the crop extent and
#'   is drawn as the map outline. The file name provides the place name used
#'   in captions and in the export directory. It cannot be combined with
#'   `crop_area`.
#' @param crop_area Geographic area name or code resolved against Natural
#'   Earth's administrative gazetteer, such as `"Ecuador"` or `"EC"`. Its
#'   complete administrative name is used in captions and legends, unless
#'   `crop_box` provides a label. It cannot be combined with `kml`.
#' @param crop_box Rectangular crop in WGS 84 decimal degrees, given as
#'   `c(xmin, ymin, xmax, ymax)`: west and east longitude, south and north
#'   latitude, e.g. `c(-82.5, -5.5, -75.0, 1.6)`. A fifth element, a character
#'   string, sets the crop label, e.g. `c(-82.5, -5.5, -75.0, 1.6, "Ecuador
#'   continental")`. Every rectangular crop adds an area item to the map
#'   legend: it uses this label when present and otherwise uses the formatted
#'   rectangle coordinates, e.g. `"82.5°W, 5.5°S, 75°W, 1.6°N"`. A supplied
#'   label also replaces the area name in captions and the export directory.
#'   It can be used alone or together with `kml` or `crop_area`; in the latter
#'   case it must overlap the boundary. The map is framed on the rectangle,
#'   which is not drawn. With `distribution = "global"` all records are still
#'   mapped, but only those inside the frame are visible. Defaults to `NULL`
#'   (no rectangular crop).
#' @param limit Maximum number of georeferenced occurrences used to draw the
#'   map, an integer between `1` and `100000`. Defaults to `100000`.
#' @param mode Map type: `"interactive"` (default; a Leaflet widget) or
#'   `"static"` (a ggplot2 map). In `gbif_map_crop_md()`, `NULL` selects an
#'   interactive map for HTML output and a static map for other formats.
#' @param save_occurrences Logical. If `TRUE`, the records located within the
#'   area, its boundary, and a provenance file containing the validated GBIF
#'   taxon key and exact name match are saved under
#'   `sacha_exports/Family_Genus_species/Place`. Defaults to `FALSE`.
#' @param export_image Logical. If `TRUE`, the static map is written as a PNG
#'   file to `sacha_exports/Family_Genus_species/Place`. Defaults to `FALSE`.
#' @param directory Base directory in which `sacha_exports` is created or,
#'   for a local source, searched. Occurrences and exported images are saved
#'   under `sacha_exports/Family_Genus_species/Place`. Defaults to [getwd()].
#' @param colour Colour of the occurrence points as a hexadecimal code with 3,
#'   4, 6, or 8 digits, e.g. `"#1B9E77"`. When `NULL`, the package default
#'   colour is used.
#' @param dpi Resolution of the exported PNG in dots per inch. When `NULL`,
#'   the package default resolution is used.
#' @param ppp Spanish-language synonym of `dpi` (puntos por pulgada). If both
#'   are supplied, `ppp` takes precedence.
#' @param overwrite Logical. Should existing output files be replaced? If
#'   `FALSE`, an existing local occurrence export raises an error. Defaults to
#'   `TRUE`.
#' @param distribution Geographic distribution of the mapped records:
#'   `"local"` (default) restricts them to the area; `"global"` includes
#'   all records of the species.
#' @param source Occurrence source: `"gbif"` (default) retrieves records from
#'   GBIF; `"local"` reads a saved export from `sacha_exports` under
#'   `directory`.
#' @param local_export Saved export used when `source = "local"`:
#'   `"species"` (default) reads the full-extent export of the species and
#'   applies the area; `"crop"` reads the export saved for the area.
#'   It is ignored when `source = "gbif"`.
#' @param fig_width,fig_height Figure dimensions in inches, used only by
#'   `gbif_map_crop_md()`. When `NULL`, dimensions are chosen from the aspect
#'   ratio of the map.
#' @return A Leaflet widget or a ggplot2 object. Paths of the written files
#'   are stored in the `"sacha_files"` attribute, the plotted records in
#'   `"sacha_occurrences"`, the species in `"sacha_species"`, and the resolved
#'   place name in `"sacha_crop_place"`. `gbif_map_crop_md()` returns the map
#'   formatted for the report output.
#' @examples
#' \dontrun{
#' map <- gbif_map_crop("Bidens andicola", kml = "Ecuador.kml", limit = 100)
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
#'                   source = "gbif", distribution = "local")
#' gbif_map_crop_md("Bidens andicola", crop_area = "Ecuador",
#'                   crop_box = c(-82.5, -5.5, -75.0, 1.6,
#'                                "Ecuador continental"),
#'                   source = "gbif", distribution = "global", mode = "static",
#'                   save_occurrences = TRUE, export_image = TRUE,
#'                   colour = "#1B9E77", ppp = 300, overwrite = TRUE,
#'                   fig_width = 8, fig_height = 6)
#' gbif_map_crop_md("Bidens andicola",
#'                   crop_box = c(-82.5, -5.5, -75.0, 1.6,
#'                                "Ecuador continental"),
#'                   source = "gbif", distribution = "local")
#' gbif_map_crop_md("Bidens andicola", kml = "study_boundary.kml",
#'                   source = "gbif", distribution = "local", mode = "static",
#'                   export_image = TRUE)
#' gbif_map_crop_md("Bidens andicola", kml = "study_boundary.kml",
#'                   crop_box = c(-82.5, -5.5, -75.0, 1.6,
#'                                "study area detail"),
#'                   source = "gbif", distribution = "global")
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
gbif_map_crop <- function(species,
                          kml = NULL,
                          crop_area = NULL,
                          crop_box = NULL,
                          limit = 100000L,
                          mode = c("interactive", "static"),
                          save_occurrences = FALSE,
                          export_image = FALSE,
                          directory = getwd(),
                          colour = NULL,
                          dpi = NULL,
                          ppp = NULL,
                          overwrite = TRUE,
                          distribution = c("local", "global"),
                          source = c("gbif", "local"),
                          local_export = c("species", "crop")) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  species <- .sacha_spatial_validate_species(species)
  language <- .sacha_spatial_language()
  limit <- .sacha_spatial_validate_limit(limit)
  if (!is.null(ppp)) dpi <- ppp
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
  crop_window <- .sacha_spatial_crop_box_parse(crop_box)
  box_only <- !is.null(crop_window) && is.null(kml) && is.null(crop_area)

  if (identical(source, "local") && identical(distribution, "global") &&
      identical(local_export, "crop")) {
    stop("`local_export = \"crop\"` requires `distribution = \"local\"`: a cropped export does not contain records outside its boundary.",
         call. = FALSE)
  }
  use_crop_export <- identical(source, "local") && identical(distribution, "local") &&
    identical(local_export, "crop")

  if (use_crop_export) {
    crop_selector <- if (box_only) {
      list(
        place = crop_window$label %spatial||%
          .sacha_spatial_crop_box_label(crop_window$extent),
        kml = NULL,
        crop_area = NULL,
        crop_box = NULL
      )
    } else {
      selector <- .sacha_spatial_map_crop_boundary(
        kml = kml, crop_area = crop_area, language = language
      )
      if (!is.null(crop_window$label)) selector$place <- crop_window$label
      selector
    }
    crop_place <- crop_selector$place
    local_crop <- .sacha_spatial_load_local_occurrences(
      species, directory, crop_place, crop_only = TRUE
    )
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
      kml = crop_selector$kml,
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
      kml = kml, crop_area = crop_area, language = language
    )
    crop_data$region <- crop_data$boundary
    if (!is.null(crop_window)) {
      crop_data <- .sacha_spatial_apply_crop_box(crop_data, crop_window)
    }
  }

  local_species <- NULL
  cropped_occurrences <- NULL
  if (identical(source, "gbif")) {
    occ_result <- .sacha_spatial_gbif_occurrences(species, limit)
    if (identical(occ_result$status, "error")) {
      .sacha_spatial_stop_gbif_error(occ_result, species, language)
    }
    occurrences <- occ_result$occurrences
    if (identical(distribution, "local")) {
      occurrences <- .sacha_spatial_filter_crop(occurrences, crop_data)
    }
  } else if (use_crop_export) {
    occurrences <- utils::head(local_crop$occurrences, limit)
  } else {
    local_species <- .sacha_spatial_load_local_occurrences(species, directory)
    cropped_occurrences <- .sacha_spatial_filter_crop(
      local_species$occurrences, crop_data
    )
    occurrences <- utils::head(
      if (identical(distribution, "local")) cropped_occurrences else local_species$occurrences,
      limit
    )
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
      if (identical(source, "gbif")) {
        complete_result <- .sacha_spatial_gbif_occurrences(species)
        if (identical(complete_result$status, "error")) {
          .sacha_spatial_stop_gbif_error(complete_result, species, language)
        }
        cropped_occurrences <- .sacha_spatial_filter_crop(
          complete_result$occurrences, crop_data
        )
      }
      if (!nrow(cropped_occurrences)) {
        warning("`save_occurrences` saved nothing: no occurrences fall within the crop boundary.",
                call. = FALSE)
      } else {
        local_files <- .sacha_spatial_save_local_occurrences(
          species, cropped_occurrences, directory, overwrite,
          crop = list(
            place = crop_data$place,
            kml = crop_data$kml,
            crop_area = crop_data$crop_area,
            crop_box = crop_data$crop_box
          ),
          boundary = crop_data$region,
          taxon_directory = taxon_directory,
          parent = if (!is.null(local_species)) local_species$metadata,
          taxon = if (identical(source, "gbif")) complete_result$taxon else NULL
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
      legend_place = crop_legend_place
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
      bbox = map_bbox, legend_place = crop_legend_place
    )
  }
  attr(map, "sacha_occurrences") <- occurrences
  attr(map, "sacha_files") <- c(image_files, local_files)
  attr(map, "sacha_species") <- species
  attr(map, "sacha_crop_place") <- crop_data$place
  map
}

#' @rdname gbif_map_crop
#' @export
gbif_map_crop_md <- function(species,
                             kml = NULL,
                             crop_area = NULL,
                             crop_box = NULL,
                             limit = 100000L,
                             mode = NULL,
                             save_occurrences = FALSE,
                             export_image = FALSE,
                             directory = getwd(),
                             colour = NULL,
                             dpi = NULL,
                             ppp = NULL,
                             overwrite = TRUE,
                             distribution = c("local", "global"),
                             fig_width = NULL,
                             fig_height = NULL,
                             source = c("gbif", "local"),
                             local_export = c("species", "crop")) {
  .sacha_apply_arg_defaults(environment(), match.call(expand.dots = FALSE), names(formals(sys.function())))
  species <- .sacha_spatial_validate_species(species)
  language <- .sacha_spatial_language()
  mode <- .sacha_spatial_resolve_report_mode(mode)
  map <- gbif_map_crop(
    species = species,
    kml = kml,
    crop_area = crop_area,
    crop_box = crop_box,
    limit = limit,
    mode = mode,
    save_occurrences = save_occurrences,
    export_image = export_image,
    directory = directory,
    colour = colour,
    dpi = dpi,
    ppp = ppp,
    overwrite = overwrite,
    distribution = distribution,
    source = source,
    local_export = local_export
  )
  .sacha_spatial_emit_md_map(
    map = map, variant = "crop", mode = mode,
    frame = attr(map, "sacha_map_frame"), species = species,
    language = language, place = attr(map, "sacha_crop_place"),
    fig_width = fig_width, fig_height = fig_height
  )
}
