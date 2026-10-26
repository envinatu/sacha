#' Download a complete ChecklistBank dataset as a ColDP archive
#'
#' Downloads the complete dataset selected by dataset from ChecklistBank and
#' saves the ColDP ZIP archive under `sacha_exports/checklistbank_<dataset>`
#' within directory. The archive includes its own metadata.yaml file. A
#' neighbouring JSON file records the dataset key, download URL, response
#' status, retrieval time, archive size and checksum.
#'
#' The default dataset is configured with `sacha_configure(clb = ...)`.
#' Supply another ChecklistBank dataset key to download a different dataset
#' without changing the package configuration.
#'
#' ChecklistBank does not publish a general numeric request or bandwidth quota
#' for this export endpoint. The function therefore keeps retries bounded: it
#' makes at most three attempts for HTTP 429 and selected temporary server
#' errors, follows a `Retry-After` delay up to five minutes, and otherwise waits
#' 15 seconds and then 60 seconds. Longer server-requested delays stop the
#' download so it can be retried later. Network and transfer errors are not
#' retried because repeating a large archive transfer could add unnecessary
#' load. If the API export route returns 404, the function also tries the
#' ChecklistBank download route. Only a ZIP response is accepted; HTML pages
#' and other responses are rejected with their HTTP status and content type.
#'
#' @param dataset ChecklistBank dataset key or alias, supplied as a character
#'   string or a whole number. When omitted, uses the dataset configured with
#'   `sacha_configure(clb = ...)`. A value supplied directly takes precedence
#'   over a shared default from `sacha_arg_configure()`.
#' @param directory Base directory under which `sacha_exports` is created for
#'   the archive and its download metadata. Defaults to the current working
#'   directory; the required directories are created when necessary.
#' @param overwrite Logical; replace an existing archive and its metadata file.
#'   Defaults to TRUE.
#'
#' @return Invisibly, the normalized path to the downloaded ColDP ZIP archive.
#'
#' @references
#' ChecklistBank API guide: <https://www.catalogueoflife.org/tools/api>.
#' ChecklistBank dataset export endpoint: `/dataset/{key}/export.zip`, relative
#' to <https://api.checklistbank.org/>.
#' Catalogue of Life Data Package (ColDP) specification:
#' <https://catalogueoflife.github.io/coldp/>.
#'
#' @seealso sacha_configure()
#'
#' @examples
#' \dontrun{
#' save_clb()
#' save_clb(dataset = "316441")
#' save_clb(dataset = 313479)
#' }
#' @export
save_clb <- function(
    dataset = NULL,
    directory = getwd(),
    overwrite = TRUE
) {
  dataset_missing <- missing(dataset)
  call <- match.call(expand.dots = FALSE)
  if (dataset_missing) dataset <- .sacha_global_config$dataset
  .sacha_apply_arg_defaults(environment(), call, names(formals(sys.function())))
  if (is.numeric(dataset) && length(dataset) == 1L && !is.na(dataset)) {
    if (!is.finite(dataset) || dataset < 0 || dataset != trunc(dataset)) {
      stop("A numeric dataset key must be a non-negative whole number.", call. = FALSE)
    }
    dataset <- format(dataset, scientific = FALSE, trim = TRUE, digits = 22L)
  } else {
    dataset <- .sacha_global_text(dataset, "dataset")
  }
  directory <- .sacha_global_text(directory, "directory")
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("overwrite must be TRUE or FALSE.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to write ChecklistBank download metadata.", call. = FALSE)
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required to download ChecklistBank data.", call. = FALSE)
  }

  dataset_slug <- gsub("[^[:alnum:]_-]+", "_", dataset)
  directory <- file.path(
    .sacha_exports_root(directory),
    paste0("checklistbank_", dataset_slug)
  )
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    stop(sprintf("Could not create the download directory '%s'.", directory), call. = FALSE)
  }
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  filename <- paste0("checklistbank_", dataset_slug, "_coldp.zip")
  destination <- file.path(directory, filename)
  metadata_destination <- file.path(
    directory,
    paste0("checklistbank_", dataset_slug, "_download_metadata.json")
  )
  if (!isTRUE(overwrite) &&
      (file.exists(destination) || file.exists(metadata_destination))) {
    stop(
      sprintf(
        "A ChecklistBank archive or metadata file already exists for dataset '%s'. Set overwrite = TRUE to replace it.",
        dataset
      ),
      call. = FALSE
    )
  }

  base_url <- sub("/+$", "", .sacha_global_config$base_url)
  encoded_dataset <- .sacha_global_url_encode(dataset)
  url <- paste0(base_url, "/dataset/", encoded_dataset, "/export.zip")
  download_url <- url
  temporary <- tempfile(
    pattern = ".checklistbank-",
    tmpdir = directory,
    fileext = ".zip"
  )
  metadata_temporary <- tempfile(
    pattern = ".checklistbank-metadata-",
    tmpdir = directory,
    fileext = ".json"
  )
  on.exit(unlink(c(temporary, metadata_temporary)), add = TRUE)

  make_request <- function(request_url) {
    request <- httr2::request(request_url)
    request <- httr2::req_headers(request, Accept = "application/zip")
    request <- httr2::req_user_agent(request, "sacha R package")
    request <- httr2::req_timeout(request, seconds = 1800)
    request <- httr2::req_error(request, is_error = function(response) FALSE)
    httr2::req_url_query(request, format = "COLDP", extended = "true")
  }
  max_attempts <- 3L
  attempt <- 0L
  started_at <- Sys.time()
  repeat {
    attempt <- attempt + 1L
    if (file.exists(temporary)) {
      unlink(temporary)
    }
    response <- tryCatch(
      httr2::req_perform(make_request(download_url), path = temporary),
      error = function(error) {
        stop(
          "ChecklistBank full dataset export failed. The request was not retried to avoid repeating a potentially large transfer: ",
          conditionMessage(error),
          call. = FALSE
        )
      }
    )
    status <- httr2::resp_status(response)
    if (status == 404L && identical(download_url, url)) {
      download_url <- paste0(
        "https://www.checklistbank.org/dataset/",
        encoded_dataset,
        "/download"
      )
      next
    }
    if (!(status %in% c(429L, 500L, 502L, 503L, 504L)) || attempt >= max_attempts) {
      break
    }
    retry_after <- httr2::resp_header(response, "retry-after")
    wait_seconds <- NA_real_
    if (!is.null(retry_after) && length(retry_after) == 1L && !is.na(retry_after)) {
      numeric_retry <- suppressWarnings(as.numeric(retry_after))
      if (is.finite(numeric_retry)) {
        wait_seconds <- max(0, numeric_retry)
      } else {
        retry_time <- suppressWarnings(as.POSIXct(
          retry_after,
          tz = "UTC",
          tryFormats = c(
            "%a, %d %b %Y %H:%M:%S",
            "%A, %d-%b-%y %H:%M:%S",
            "%a %b %d %H:%M:%S %Y"
          )
        ))
        if (!is.na(retry_time)) {
          wait_seconds <- max(0, as.numeric(difftime(retry_time, Sys.time(), units = "secs")))
        }
      }
    }
    if (!is.finite(wait_seconds)) {
      wait_seconds <- c(15, 60)[[attempt]]
    }
    if (wait_seconds > 300) {
      stop(
        sprintf(
          "ChecklistBank returned HTTP status %s and requested a retry delay of %.0f seconds. No automatic retry was made; try again later.",
          status,
          wait_seconds
        ),
        call. = FALSE
      )
    }
    if (wait_seconds > 0) {
      Sys.sleep(wait_seconds)
    }
  }
  elapsed_seconds <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
  if (status < 200L || status >= 300L) {
    stop(
      sprintf(
        "ChecklistBank returned HTTP status %s for dataset key '%s'. Check that the key identifies an accessible dataset and that its complete ColDP download is available.",
        status,
        dataset
      ),
      call. = FALSE
    )
  }
  if (!file.exists(temporary) || is.na(file.info(temporary)$size) ||
      file.info(temporary)$size < 4L) {
    stop("ChecklistBank returned an empty or incomplete ColDP archive.", call. = FALSE)
  }

  signature <- readBin(temporary, what = "raw", n = 4L)
  if (length(signature) < 4L ||
      !identical(signature[1:2], as.raw(c(0x50, 0x4B)))) {
    content_type <- httr2::resp_header(response, "content-type")
    if (is.null(content_type) || !length(content_type) || is.na(content_type)) {
      content_type <- "not provided"
    }
    stop(
      sprintf(
        "ChecklistBank returned HTTP status %s with content type '%s' instead of a ColDP ZIP archive for dataset key '%s' (URL: %s). The key may not be downloadable through the public complete-export endpoint.",
        status,
        content_type,
        dataset,
        download_url
      ),
      call. = FALSE
    )
  }
  archive_files <- tryCatch(
    utils::unzip(temporary, list = TRUE),
    error = function(error) NULL
  )
  if (is.null(archive_files) || !"Name" %in% names(archive_files)) {
    stop("The downloaded file is not a readable ZIP archive.", call. = FALSE)
  }
  package_metadata <- grep(
    "(^|/)metadata\\.ya?ml$",
    archive_files$Name,
    value = TRUE,
    ignore.case = TRUE
  )
  if (!length(package_metadata)) {
    stop(
      "The ZIP archive does not contain the metadata.yaml file required for ColDP.",
      call. = FALSE
    )
  }
  has_coldp_data <- any(grepl(
    "(^|/)(nameusage|taxon|name)\\.(csv|tsv|tab|txt)$",
    archive_files$Name,
    ignore.case = TRUE
  ))
  if (!has_coldp_data) {
    stop(
      "The ZIP archive contains metadata but no recognized ColDP taxonomic data table.",
      call. = FALSE
    )
  }

  archive_size <- file.info(temporary)$size
  archive_md5 <- unname(tools::md5sum(temporary))
  download_record <- list(
    dataset_key = dataset,
    format = "ColDP",
    download_url = download_url,
    downloaded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    http_status = status,
    attempts = attempt,
    elapsed_seconds = elapsed_seconds,
    archive_filename = filename,
    archive_bytes = unname(as.numeric(archive_size)),
    archive_md5 = archive_md5,
    coldp_package_metadata = package_metadata[[1L]]
  )
  jsonlite::write_json(
    download_record,
    path = metadata_temporary,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )

  if (!file.copy(temporary, destination, overwrite = isTRUE(overwrite), copy.date = TRUE)) {
    stop(sprintf("Could not save the ColDP archive to '%s'.", destination), call. = FALSE)
  }
  if (!file.copy(
    metadata_temporary,
    metadata_destination,
    overwrite = isTRUE(overwrite)
  )) {
    stop(
      sprintf("Could not save the download metadata to '%s'.", metadata_destination),
      call. = FALSE
    )
  }

  destination <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  metadata_destination <- normalizePath(
    metadata_destination,
    winslash = "/",
    mustWork = TRUE
  )
  message("ChecklistBank ColDP archive saved to: ", destination)
  message("Download metadata saved to: ", metadata_destination)
  invisible(destination)
}
