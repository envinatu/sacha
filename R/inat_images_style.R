.inat_escape_attribute <- function(text) {
  text <- gsub("\"", "\u201D", text, fixed = TRUE)
  text <- gsub("'", "\u2019", text, fixed = TRUE)
  trimws(text)
}

.inat_sanitize_text <- function(text) {
  if (is.null(text)) {
    return(text)
  }

  gsub("|", "\uFF5C", text, fixed = TRUE)
}

.inat_scientific_label <- function(record) {
  authorship <- .inat_scalar(record$taxonomic_author, "")
  paste0(
    "*", record$scientific_name, "*",
    if (nzchar(authorship)) paste0(" ", authorship) else ""
  )
}

.inat_observed_date_label <- function(record, language) {
  observed_on <- .inat_scalar(record$observed_on, "")
  if (!nzchar(observed_on)) {
    return("")
  }
  observed_date <- suppressWarnings(as.Date(
    substr(observed_on, 1L, 10L),
    tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d")
  ))
  if (is.na(observed_date)) {
    return("")
  }
  prefix <- tryCatch(
    .inat_text("on", language),
    error = function(error) if (identical(language, "es")) "el" else "on"
  )
  paste0(", ", trimws(prefix), " ", format(observed_date, "%d/%m/%Y"))
}

.inat_subcaption <- function(
    record,
    observer_name,
    language,
    use_placeholders = TRUE
) {
  photo_observer <- if (length(record$photos) > 0L) record$photos[[1L]]$observer else NULL
  observer <- if (!is.null(observer_name) && nzchar(trimws(observer_name))) {
    observer_name
  } else {
    .inat_scalar(
      photo_observer,
      .inat_scalar(record$observer, .inat_text("unknown_observer", language))
    )
  }

  locality <- .inat_scalar(record$place, .inat_text("unknown_place", language))
  date_label <- .inat_observed_date_label(record, language)

  license_text <- if (!is.null(record$license_label) &&
                      nzchar(record$license_label %inat||% "")) {
    record$license_label
  } else if (isTRUE(use_placeholders)) {
    .inat_text("all_rights_reserved", language)
  } else {
    ""
  }

  sprintf(
    "%s, %s %s%s, %s %s (%s: %s). [%s](%s)",
    .inat_sanitize_text(.inat_scientific_label(record)),
    .inat_text("observed", language),
    .inat_sanitize_text(locality),
    date_label,
    .inat_text("by", language),
    .inat_sanitize_text(observer),
    .inat_text("licence", language),
    .inat_sanitize_text(license_text),
    record$observation_url,
    record$observation_url
  )
}

#' @export
#' @noRd

print.sacha_inat_images <- function(x, ...) {
  if (length(x$records) == 0L) {
    cat(.inat_text("no_observations", x$language), "\n")
    return(invisible(x))
  }

  source_label <- if (identical(x$source, "local")) {
    .inat_text("local_source", x$language)
  } else {
    .inat_text("web_source", x$language)
  }

  cat(source_label, "\n\n")

  summary_table <- data.frame(
    observation_id = vapply(
      x$records,
      function(record) record$observation_id,
      character(1)
    ),
    scientific_name = vapply(
      x$records,
      function(record) record$scientific_name,
      character(1)
    ),
    image = vapply(
      x$records,
      function(record) record$image_location %inat||% NA_character_,
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  print(summary_table, row.names = FALSE)

  invisible(x)
}

.inat_is_output <- function(format) {
  requireNamespace("knitr", quietly = TRUE) &&
    isTRUE(tryCatch(knitr::pandoc_to(format), error = function(error) FALSE))
}

.inat_qmd_grid_table <- function(image_rows, caption_rows, ncol) {
  column_width <- max(nchar(image_rows), nchar(caption_rows)) + 2L

  pad <- function(x) {
    sprintf(paste0("%-", column_width, "s"), x)
  }

  border <- paste0(
    "+",
    paste(rep(strrep("-", column_width + 2L), ncol), collapse = "+"),
    "+"
  )

  blank_row <- paste0(
    "|",
    paste(rep(strrep(" ", column_width + 2L), ncol), collapse = "|"),
    "|"
  )

  make_row <- function(values) {
    missing_cells <- ncol - length(values)

    if (missing_cells > 0L) {
      values <- c(values, rep("", missing_cells))
    }

    paste0("| ", paste(pad(values), collapse = " | "), " |")
  }

  number_of_rows <- ceiling(length(image_rows) / ncol)
  blocks <- character(number_of_rows)

  for (row_index in seq_len(number_of_rows)) {
    first <- (row_index - 1L) * ncol + 1L
    last <- min(row_index * ncol, length(image_rows))
    selected <- first:last

    blocks[[row_index]] <- paste(
      c(
        make_row(image_rows[selected]),
        blank_row,
        make_row(caption_rows[selected])
      ),
      collapse = "\n"
    )
  }

  paste(
    c(
      border,
      paste(blocks, collapse = paste0("\n", border, "\n")),
      border
    ),
    collapse = "\n"
  )
}

.inat_print_image_metadata <- function(images, image_size) {
  language <- images$language
  labels <- c(
    source = .inat_text("image_metadata_source", language),
    taxon = .inat_text("image_metadata_taxon", language),
    family = .inat_text("image_metadata_family", language),
    date = .inat_text("image_metadata_date", language),
    place = .inat_text("image_metadata_place", language),
    observer = .inat_text("image_metadata_observer", language),
    photo_author = .inat_text("image_metadata_photo_author", language),
    observation_url = .inat_text("image_metadata_observation_url", language),
    photo = .inat_text("image_metadata_photo", language),
    photo_license = .inat_text("image_metadata_photo_license", language),
    image_url = .inat_text("image_metadata_image_url", language)
  )
  source_label <- if (identical(images$source, "local")) {
    .inat_text("image_metadata_source_local", language)
  } else {
    .inat_text("image_metadata_source_web", language)
  }
  cat(source_label, "\n")
  for (record_index in seq_along(images$records)) {
    record <- images$records[[record_index]]
    cat("\n", .inat_text("image_metadata_observation", language),
        " ", record_index, " (", record$observation_id, ")\n", sep = "")
    cat("  ", labels[["taxon"]], ": ", record$scientific_name, "\n", sep = "")
    authorship <- .inat_scalar(record$taxonomic_author, "")
    if (nzchar(authorship)) {
      cat("  ", .inat_text("image_metadata_taxonomic_authorship", language),
          ": ", authorship, "\n", sep = "")
    }
    cat("  ", labels[["family"]], ": ",
        .inat_scalar(record$family, .inat_text("image_metadata_not_recorded", language)),
        "\n", sep = "")
    cat("  ", labels[["date"]], ": ",
        .inat_scalar(record$observed_on, .inat_text("image_metadata_not_recorded", language)),
        "\n", sep = "")
    cat("  ", labels[["place"]], ": ",
        .inat_scalar(record$place, .inat_text("image_metadata_not_recorded", language)),
        "\n", sep = "")
    cat("  ", labels[["observer"]], ": ",
        .inat_scalar(record$observer, .inat_text("image_metadata_not_identified", language)),
        "\n", sep = "")
    cat("  ", labels[["observation_url"]], ": ",
        record$observation_url, "\n", sep = "")
    for (index in seq_along(record$photos)) {
      photo <- record$photos[[index]]
      cat("  ", labels[["photo"]], " ", index, "\n", sep = "")
      photo_author <- .inat_scalar(photo$observer, "")
      if (nzchar(photo_author)) {
        cat("    ", labels[["photo_author"]], ": ", photo_author, "\n", sep = "")
      }
      cat("    ", labels[["photo_license"]], ": ",
          .inat_license_label(photo$license_code, language = images$language),
          "\n", sep = "")
      cat("    ", labels[["image_url"]], ": ",
          .inat_image_url(photo, image_size), "\n", sep = "")
    }
    if (identical(record$image_source, "local") &&
        !is.null(record$image_location)) {
      cat("  ", .inat_text("image_metadata_local_file", language), ": ",
          record$image_location, "\n", sep = "")
    }
  }
  invisible(NULL)
}

.inat_wrap_caption <- function(text, width) {
  text <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", "\\2", text, perl = TRUE)
  text <- gsub("*", "", text, fixed = TRUE)
  words <- unlist(strsplit(trimws(text), "\\s+"), use.names = FALSE)
  if (length(words) == 0L || !nzchar(words[[1L]])) {
    return("")
  }
  split_word <- function(word) {
    if (nchar(word, type = "width") <= width) {
      return(word)
    }
    starts <- seq.int(1L, nchar(word, type = "chars"), by = width)
    ends <- pmin(starts + width - 1L, nchar(word, type = "chars"))
    substring(word, starts, ends)
  }
  words <- unlist(lapply(words, split_word), use.names = FALSE)
  lines <- character()
  current <- ""
  for (word in words) {
    candidate <- if (nzchar(current)) paste(current, word) else word
    if (nchar(candidate, type = "width") > width && nzchar(current)) {
      lines <- c(lines, current)
      current <- word
    } else {
      current <- candidate
    }
  }
  c(lines, current)
}

.inat_show_image_composition <- function(
    images,
    images_directory,
    export_comp = FALSE,
    ncol = 2L,
    show = TRUE
) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    if (isTRUE(show) || isTRUE(export_comp)) {
      message("Install the optional 'magick' package to preview or export image compositions.")
    }
    return(invisible(NULL))
  }
  records <- Filter(
    function(record) !is.null(record$image_location) &&
      nzchar(record$image_location),
    images$records
  )
  if (length(records) == 0L) {
    return(invisible(NULL))
  }
  if (length(ncol) != 1L || is.na(ncol) || ncol < 1L) {
    stop("'ncol' must be a positive integer.", call. = FALSE)
  }
  ncol <- min(as.integer(ncol), length(records))
  temporary_files <- character()
  on.exit(unlink(temporary_files), add = TRUE)
  loaded <- lapply(records, function(record) {
    tryCatch({
      path <- record$image_location
      if (grepl("^https?://", path)) {
        destination <- tempfile(fileext = paste0(".", .inat_file_extension(path)))
        .inat_media_budget()
        .inat_wait_media_request(
          if (length(records) > 10L) {
            .sacha_inat_defaults$large_download_delay
          } else {
            .sacha_inat_defaults$small_download_delay
          }
        )
        utils::download.file(path, destination, mode = "wb", quiet = TRUE)
        downloaded_bytes <- suppressWarnings(as.numeric(file.info(destination)$size))
        if (is.finite(downloaded_bytes) && downloaded_bytes > 0) {
          .inat_media_budget(downloaded_bytes)
        }
        temporary_files <<- c(temporary_files, destination)
        path <- destination
      }
      magick::image_read(path)
    }, error = function(error) NULL)
  })
  keep <- !vapply(loaded, is.null, logical(1))
  loaded <- loaded[keep]
  records <- records[keep]
  if (length(loaded) == 0L) {
    return(invisible(NULL))
  }

  caption_text <- vapply(records, function(record) {
    caption <- .inat_scalar(record$caption, "")
    if (!nzchar(caption)) {
      caption <- paste(
        .inat_scientific_label(record),
        .inat_scalar(record$taxonomic_author, ""),
        sep = " "
      )
    }
    caption
  }, character(1), USE.NAMES = FALSE)
  rows <- ceiling(length(records) / ncol)

  draw_composition <- function() {
    device_size <- grDevices::dev.size("in")
    device_width <- device_size[[1L]]
    device_height <- device_size[[2L]]
    info <- lapply(loaded, magick::image_info)
    aspects <- vapply(info, function(item) item$width[[1L]] / item$height[[1L]], numeric(1))
    column_caption_width <- device_width / ncol * 0.94
    caption_widths <- rep(column_caption_width, length(records))
    captions <- lapply(caption_text, .inat_wrap_caption, width = max(12L, floor(column_caption_width * 16L)))
    caption_fraction <- rep(0.35, rows)
    for (iteration in seq_len(4L)) {
      row_max_lines <- vapply(seq_len(rows), function(row) {
        indices <- seq.int((row - 1L) * ncol + 1L, min(row * ncol, length(records)))
        max(vapply(captions[indices], length, integer(1)))
      }, integer(1))
      caption_fraction <- pmin(0.58, pmax(0.28, row_max_lines * 0.06))
      for (index in seq_along(records)) {
        row <- ceiling(index / ncol)
        image_area_height <- device_height / rows * 0.96 * 0.96 * (1 - caption_fraction[[row]])
        image_width <- min(
          device_width / ncol * 0.96 * 0.96 * 0.96,
          image_area_height * 0.96 * aspects[[index]]
        )
        caption_widths[[index]] <- if (length(records) == 1L) image_width else column_caption_width
        captions[[index]] <- .inat_wrap_caption(
          caption_text[[index]],
          max(12L, floor(caption_widths[[index]] * 16L))
        )
      }
    }
    row_max_lines <- vapply(seq_len(rows), function(row) {
      indices <- seq.int((row - 1L) * ncol + 1L, min(row * ncol, length(records)))
      max(vapply(captions[indices], length, integer(1)))
    }, integer(1))
    caption_fraction <- pmin(0.58, pmax(0.28, row_max_lines * 0.06))
    grid::grid.newpage()


    grid::pushViewport(grid::viewport(
      layout = grid::grid.layout(
        nrow = rows,
        ncol = ncol,
        widths = grid::unit(rep(1, ncol), "null"),
        heights = grid::unit(rep(1, rows), "null")
      )
    ))
    for (index in seq_along(records)) {
      row <- ceiling(index / ncol)
      column <- index - (row - 1L) * ncol
      row_caption_fraction <- caption_fraction[[row]]
      grid::pushViewport(grid::viewport(
        layout.pos.row = row,
        layout.pos.col = column
      ))
      grid::pushViewport(grid::viewport(
        x = 0.5,
        y = 0.5,
        width = 0.96,
        height = 0.96,
        layout = grid::grid.layout(
          nrow = 2L,
          ncol = 1L,
          heights = grid::unit(c(1 - row_caption_fraction, row_caption_fraction), "null")
        )
      ))
      grid::pushViewport(grid::viewport(layout.pos.row = 1L))
      info <- magick::image_info(loaded[[index]])
      area_width <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
      area_height <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
      aspect <- info$width[[1L]] / info$height[[1L]]
      raster_width <- min(area_width * 0.96, area_height * 0.96 * aspect)
      raster_height <- raster_width / aspect
      grid::grid.raster(
        grDevices::as.raster(loaded[[index]]),
        x = 0.5,
        y = grid::unit(0.02, "npc") + grid::unit(raster_height / 2, "inches"),
        width = grid::unit(raster_width, "inches"),
        height = grid::unit(raster_height, "inches"),
        interpolate = TRUE
      )
      grid::upViewport()
      grid::pushViewport(grid::viewport(layout.pos.row = 2L))
      caption_viewport_width <- if (length(records) == 1L) {
        grid::unit(caption_widths[[index]], "inches")
      } else {
        grid::unit(0.9791667, "npc")
      }
      grid::pushViewport(grid::viewport(
        x = 0.5,
        width = caption_viewport_width,
        height = 1
      ))
      caption_lines <- captions[[index]]
      taxon_name <- records[[index]]$scientific_name
      first_line <- caption_lines[[1L]]
      first_label <- if (startsWith(first_line, taxon_name)) {
        remainder <- trimws(substring(first_line, nchar(taxon_name, type = "chars") + 1L))
        if (nzchar(remainder)) {
          bquote(italic(.(taxon_name)) ~ .(remainder))
        } else {
          bquote(italic(.(taxon_name)))
        }
      } else {
        first_line
      }
      for (line_index in seq_along(caption_lines)) {
        caption_label <- if (line_index == 1L) first_label else caption_lines[[line_index]]
        grid::grid.text(
          caption_label,
          x = 0.5,
          y = grid::unit(0.98, "npc") - grid::unit((line_index - 1L) * 1.15, "lines"),
          just = c("centre", "top"),
          gp = grid::gpar(
            fontfamily = "sans",
            fontsize = 9.5,
            lineheight = 1.15,
            col = "#243447"
          )
        )
      }
      grid::upViewport()
      grid::upViewport(3L)


    }
    grid::upViewport()
    invisible(NULL)
  }

  if (isTRUE(show)) {
    draw_composition()
  }
  destination <- NULL
  if (isTRUE(export_comp)) {
    record <- records[[1L]]
    directory <- .inat_taxon_directory(
      images_directory,
      record$family,
      record$scientific_name
    )
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    }
    image_count <- length(records)
    count_label <- if (image_count == 1L) "image" else "images"
    destination <- file.path(
      directory,
      sprintf("inat_images_composition_%d_%s.png", image_count, count_label)
    )
    output_height <- max(1000L, rows * 1050L)
    output_width <- ncol * 900L
    grDevices::png(
      filename = destination,
      width = output_width,
      height = output_height,
      res = 180,
      bg = "white"
    )
    tryCatch(draw_composition(), finally = grDevices::dev.off())
  }
  invisible(destination %inat||% NULL)
}

.inat_render_images_md <- function(
    images,
    images_directory,
    observer_name,
    export_comp,
    ncol,
    word_subfigure_width,
    latex_subfigure_width
) {
  if (isTRUE(export_comp)) {
    suppressMessages(tryCatch(
      suppressWarnings(.inat_show_image_composition(
        images,
        images_directory,
        export_comp = TRUE,
        ncol = ncol,
        show = FALSE
      )),
      error = function(error) NULL
    ))
  }
  records <- Filter(
    function(record) !is.null(record$image_location) &&
      nzchar(record$image_location),
    images$records
  )
  if (length(records) == 0L) {
    return(invisible(images))
  }

  target <- tryCatch(knitr::pandoc_to(), error = function(error) NULL)
  is_rendering <- length(target) == 1L && !is.na(target) && nzchar(target)
  input_directory <- NULL
  if (is_rendering) {
    input_path <- tryCatch(knitr::current_input(), error = function(error) "")
    if (length(input_path) != 1L || is.na(input_path) || !nzchar(input_path)) {
      stop("Could not resolve the Quarto input file for image paths.", call. = FALSE)
    }
    input_path <- normalizePath(input_path, winslash = "/", mustWork = TRUE)
    input_directory <- dirname(input_path)
  }

  relative_image_path <- function(path, directory) {
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

  records <- lapply(records, function(record) {
    path <- record$image_location
    if (grepl("^https?://", path) || !is_rendering) {
      record$render_image_location <- path
    } else {
      source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      if (identical(target, "typst")) {
        destination_path <- file.path(input_directory, basename(source_path))
        normalized_destination <- normalizePath(
          destination_path, winslash = "/", mustWork = FALSE
        )
        if (!identical(source_path, normalized_destination) &&
            !isTRUE(file.copy(source_path, destination_path, overwrite = TRUE))) {
          stop(
            "Could not place the iNaturalist image where the Typst document resolves chapter images.",
            call. = FALSE
          )
        }
        record$render_image_location <- basename(source_path)
      } else {
        record$render_image_location <- relative_image_path(
          source_path, input_directory
        )
      }
    }
    record
  })

  if (length(records) == 1L) {
    record <- records[[1L]]
    figure_caption <- .inat_subcaption(
      record = record,
      observer_name = observer_name,
      language = images$language,
      use_placeholders = FALSE
    )

    short_caption <- paste(
      .inat_text("record", images$language),
      .inat_scientific_label(record)
    )

    cat(
      sprintf(
        "\n![%s](%s){#fig-obs-%s fig-scap=\"%s\" fig-align=\"center\" width=90%%}\n\n",
        figure_caption,
        record$render_image_location,
        record$observation_id,
        .inat_escape_attribute(short_caption)
      )
    )

    return(invisible(images))
  }

  first_record <- records[[1L]]
  panel_caption <- paste(
    .inat_text("records", images$language),
    .inat_scientific_label(first_record)
  )

  short_caption <- paste(
    .inat_text("record", images$language),
    .inat_scientific_label(first_record)
  )

  taxon_id <- .inat_safe_name(first_record$taxon_id)
  is_docx <- .inat_is_output("docx")
  is_latex <- .inat_is_output("latex")

  if (is_docx) {
    letters_for_records <- letters[seq_along(records)]

    image_rows <- vapply(
      records,
      function(record) {
        sprintf(
          "![](%s){width=%s}",
          record$render_image_location,
          word_subfigure_width
        )
      },
      character(1)
    )

    caption_rows <- vapply(
      seq_along(records),
      function(index) {
        sprintf(
          "(%s) %s",
          letters_for_records[[index]],
          .inat_subcaption(
            records[[index]],
            observer_name = observer_name,
            language = images$language,
            use_placeholders = FALSE
          )
        )
      },
      character(1)
    )

    cat(
      sprintf(
        "\n::: {#fig-taxon-%s fig-cap=\"%s\" fig-scap=\"%s\"}\n\n",
        taxon_id,
        .inat_escape_attribute(panel_caption),
        .inat_escape_attribute(short_caption)
      )
    )

    cat(.inat_qmd_grid_table(image_rows, caption_rows, ncol = ncol))
    cat("\n\n:::\n\n")

    return(invisible(images))
  }

  fig_attrs <- if (is_latex) {
    "fig-pos=\"tp\" fig-env=\"figure*\""
  } else {
    "fig-pos=\"H\""
  }

  cat(
    sprintf(
      "\n::: {#fig-taxon-%s layout-ncol=%s layout-valign=\"center\" %s fig-cap=\"%s\" fig-scap=\"%s\"}\n\n",
      taxon_id,
      ncol,
      fig_attrs,
      .inat_escape_attribute(panel_caption),
      .inat_escape_attribute(short_caption)
    )
  )

  for (record in records) {
    image_attributes <- if (is_latex) {
      sprintf(
        "#fig-sub-%s width=\"%s\"",
        record$observation_id,
        latex_subfigure_width
      )
    } else {
      sprintf("#fig-sub-%s", record$observation_id)
    }

    cat(
      sprintf(
        "![%s](%s){%s}\n\n",
        .inat_subcaption(
          record,
          observer_name = observer_name,
          language = images$language,
          use_placeholders = FALSE
        ),
        record$render_image_location,
        image_attributes
      )
    )
  }

  cat(":::\n\n")

  invisible(images)
}
