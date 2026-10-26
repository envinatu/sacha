.sacha_spatial_map_style <- list(
  defaults = list(
    point_colour = "#0A8CE6",
    dpi = 300,
    zoom_static = TRUE,
    crop_zoom_static = TRUE,
    crop_kml = NULL
  ),
  frame = list(
    common = list(
      square_aspect = 1,
      horizontal_aspect = 4 / 3,
      vertical_aspect = 3 / 4,
      elongation_threshold = 1.3,
      padding_fraction = 0.06,
      minimum_longitude_span = 1,
      minimum_latitude_span = 1,
      minimum_cosine = 0.15,
      latitude_limit = 85,
      world_bbox = c(xmin = -180, ymin = -85, xmax = 180, ymax = 85)
    ),
    complete = list(),
    crop = list(
      padding_fraction = 0.01,
      minimum_longitude_span = 0.1,
      minimum_latitude_span = 0.1
    )
  ),
  country_boundary = list(
    source = "Natural Earth",
    scale = "medium",
    matching_fields = c("admin", "name_long", "name", "sovereignt", "iso_a2", "iso_a3"),
    place_field = "admin"
  ),
  static = list(
    common = list(
      land_fill = "#EAEAEA",
      land_border = "#FFFFFF",
      land_border_width = 0.3,
      panel_background = "#FFFFFF",
      point_alpha = 0.75,
      point_size = 1.8,
      boundary_fill = "#4D4D4D",
      boundary_alpha = 0.08,
      boundary_border = "#4D4D4D",
      boundary_border_width = 0.6,
      plot_margin = 10,
      crs = NULL,
      aspect_ratio = NULL,
      legend_font_size = 10,
      legend_title_font_size = 10,
      legend_text_colour = "#333333",
      legend_key_size_cm = 0.32,
      legend_row_height_cm = 0.5,
      legend_pad_x_cm = 0.3,
      legend_text_gap_cm = 0.18,
      legend_pad_y_gap_cm = 0.22,
      legend_background_alpha = 0.45,
      legend_min_width_cm = 2.2,
      legend_margin_cm = 0.3,
      legend_corner_radius_cm = 0.1,
      legend_border_alpha = 0.12,
      legend_border_width = 0.5,
      legend_shadow_alpha = 0.1,
      legend_shadow_rings = 4,
      legend_shadow_step_cm = 0.025,
      legend_shadow_offset_cm = 0.02,
      north_scale_pad_x_cm = 0.3,
      north_scale_pad_y_cm = 0.3,
      north_scale_gap_cm = 0.18,
      compass_height_cm = 1.1,
      compass_width_cm = 1.1,
      compass_text_cex = 0.55,
      scalebar_height_cm = 0.35,
      scalebar_text_cex = 0.55,
      scalebar_max_width_cm = 3,
      scalebar_nice_steps_km = c(
        0.01, 0.02, 0.05, 0.1, 0.2, 0.5,
        1, 2, 5, 10, 20, 50,
        100, 200, 500, 1000, 2000, 5000
      ),
      grid_show = TRUE,
      grid_target_divisions = 4,
      grid_nice_steps_deg = sort(unique(c(
        as.vector(outer(c(1, 1.5, 2, 3, 4, 5, 6, 8), 10^(-3:1))),
        15, 20, 30, 45, 60, 90
      ))),
      grid_line_colour = "#B3B3B3",
      grid_line_width = 0.25,
      grid_text_size = 10,
      grid_text_colour = "#4D4D4D"
    ),
    complete = list(
      coord_expand = FALSE
    ),
    crop = list(
      coord_expand = FALSE
    ),
    export = list(
      complete = list(
        square = c(width = 7, height = 7),
        horizontal = c(width = 8, height = 6),
        vertical = c(width = 6, height = 8)
      ),
      crop = list(
        square = c(width = 7, height = 7),
        horizontal = c(width = 8, height = 6),
        vertical = c(width = 6, height = 8)
      )
    ),
    markdown = list(
      complete = list(
        square = c(width = 7, height = 7),
        horizontal = c(width = 8, height = 6),
        vertical = c(width = 6, height = 8)
      ),
      crop = list(
        square = c(width = 7, height = 7),
        horizontal = c(width = 8, height = 6),
        vertical = c(width = 6, height = 8)
      )
    ),
    adornments = list(common = list(), complete = list(), crop = list())
  ),
  interactive = list(
    common = list(
      tile_providers = c("Esri.WorldGrayCanvas", "Esri.WorldImagery", "Esri.WorldTopoMap", "OpenStreetMap.Mapnik"),
      marker_radius = 4,
      marker_fill_opacity = 0.7,
      boundary_fill = "#4D4D4D",
      boundary_fill_opacity = 0.08,
      boundary_border = "#4D4D4D",
      boundary_border_weight = 2,
      boundary_border_opacity = 0.9,
      legend_position = "bottomleft",
      legend_opacity = 0.8,
      legend_margin_px = 10,
      layers_control_position = "topright",
      fullscreen_control_position = "topright",
      zoom_control = TRUE,
      attribution_control = TRUE,
      layers_collapsed = TRUE,
      popup_font_family = "sans-serif",
      tile_no_wrap = FALSE,
      world_copy_jump = TRUE,
      minimum_zoom = 0,
      container_background = "#A9D8E8",
      fit_padding_px = c(0, 0)
    ),
    complete = list(),
    crop = list(),
    adornments = list(common = list(), complete = list(), crop = list())
  )
)

.sacha_spatial_map_style_variant <- function(engine, boundary = NULL) {
  variant <- if (is.null(boundary)) "complete" else "crop"
  engine_style <- .sacha_spatial_map_style[[engine]]
  utils::modifyList(engine_style$common, engine_style[[variant]])
}

.sacha_spatial_map_style_frame_variant <- function(boundary = NULL) {
  variant <- if (is.null(boundary)) "complete" else "crop"
  utils::modifyList(.sacha_spatial_map_style$frame$common,
                    .sacha_spatial_map_style$frame[[variant]])
}

.sacha_spatial_map_style_inputs <- function(colour = NULL, dpi = NULL, zoom_static = NULL) {
  defaults <- .sacha_spatial_map_style$defaults
  list(
    colour = if (is.null(colour)) defaults$point_colour else colour,
    dpi = if (is.null(dpi)) defaults$dpi else dpi,
    zoom_static = if (is.null(zoom_static)) defaults$zoom_static else zoom_static
  )
}

.sacha_spatial_map_style_crop_zoom <- function() {
  .sacha_spatial_map_style$defaults$crop_zoom_static
}

.sacha_spatial_map_style_crop_kml <- function(kml = NULL) {
  if (is.null(kml)) .sacha_spatial_map_style$defaults$crop_kml else kml
}

.sacha_spatial_map_style_retrieval_date <- function(retrieved_at) {
  value <- .sacha_spatial_scalar(retrieved_at, default = NA_character_)
  if (is.na(value)) return(as.Date(NA_character_))
  date_text <- substr(value, 1L, 10L)
  date <- suppressWarnings(as.Date(date_text, format = "%Y-%m-%d"))
  if (is.na(date) || !identical(format(date, "%Y-%m-%d"), date_text)) {
    return(as.Date(NA_character_))
  }
  date
}

.sacha_spatial_map_style_report <- function(variant, mode, frame, species,
                                            language, place = NULL,
                                            retrieved_at = NULL) {
  if (!variant %in% c("complete", "crop")) {
    stop("`variant` must be 'complete' or 'crop'.", call. = FALSE)
  }
  display_name <- .sacha_spatial_display_name(species, markdown = TRUE)
  caption_date <- .sacha_spatial_map_style_retrieval_date(retrieved_at)
  if (is.na(caption_date)) caption_date <- Sys.Date()
  date <- format(caption_date, "%d-%m-%Y")
  captions <- if (identical(variant, "crop")) {
    list(
      full = sprintf(.sacha_spatial_label("map_caption_crop", language),
                     display_name, place, date),
      short = sprintf(.sacha_spatial_label("map_short_caption_crop", language),
                      display_name, place)
    )
  } else {
    list(
      full = sprintf(.sacha_spatial_label("map_caption", language), display_name, date),
      short = sprintf(.sacha_spatial_label("map_short_caption", language), display_name)
    )
  }
  id_parts <- c("fig", "gbif", "map")
  if (identical(variant, "crop")) id_parts <- c(id_parts, variant)
  id_parts <- c(id_parts, .sacha_spatial_slug(species))
  if (!is.null(place) && nzchar(place)) id_parts <- c(id_parts, .sacha_spatial_slug(place))
  report <- c(captions, list(id = paste(id_parts[nzchar(id_parts)], collapse = "-")))
  if (identical(mode, "static")) {
    size <- .sacha_spatial_map_style_markdown_size(mode, frame)
    report$width <- size[["width"]]
    report$height <- size[["height"]]
  }
  report
}

.sacha_spatial_map_style_render_caption <- function(map, report) {
  if (inherits(map, "htmlwidget") && requireNamespace("htmltools", quietly = TRUE)) {
    caption_html <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", report$full)
    return(htmltools::browsable(htmltools::tags$figure(
      class = "sacha-map-figure",
      map,
      htmltools::tags$figcaption(htmltools::HTML(caption_html))
    )))
  }
  map
}

.sacha_spatial_map_style_country_key <- function(x) {
  x <- iconv(tolower(trimws(as.character(x))), from = "", to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9]+", "", x)
}

.sacha_spatial_map_style_crop_area_boundary <- function(crop_area) {
  if (!is.character(crop_area) || length(crop_area) != 1L || is.na(crop_area) ||
      !nzchar(trimws(crop_area))) {
    stop("`crop_area` must be one non-empty geographic area name or code.", call. = FALSE)
  }
  .sacha_spatial_require(c("rnaturalearth", "sf"))
  settings <- .sacha_spatial_map_style$country_boundary
  world <- rnaturalearth::ne_countries(scale = settings$scale, returnclass = "sf")
  fields <- intersect(settings$matching_fields, names(world))
  target <- .sacha_spatial_map_style_country_key(crop_area)
  matched <- rep(FALSE, nrow(world))
  for (field in fields) {
    matched <- matched | (.sacha_spatial_map_style_country_key(world[[field]]) == target)
  }
  boundary <- world[matched, , drop = FALSE]
  if (!nrow(boundary)) {
    stop(sprintf(
      "No public %s boundary matched `crop_area = %s`. Use an accepted area name, code, `crop_box`, or `crop_vector`.",
      settings$source, sQuote(crop_area)
    ), call. = FALSE)
  }
  place_field <- settings$place_field
  place <- if (place_field %in% names(boundary)) as.character(boundary[[place_field]][1]) else crop_area
  country_code <- if ("iso_a2" %in% names(boundary)) {
    toupper(trimws(as.character(boundary$iso_a2[1L])))
  } else {
    NA_character_
  }
  if (is.na(country_code) || !grepl("^[A-Z]{2}$", country_code)) {
    country_code <- NULL
  }
  list(
    boundary = boundary,
    bbox = sf::st_bbox(boundary),
    place = place,
    country_code = country_code
  )
}

.sacha_spatial_map_crop_boundary <- function(crop_vector = NULL, crop_area = NULL,
                                             language = NULL) {
  selectors <- c(crop_vector = !is.null(crop_vector), crop_area = !is.null(crop_area))
  if (sum(selectors) > 1L) {
    stop("Specify only one of `crop_vector` or `crop_area`.", call. = FALSE)
  }
  if (!any(selectors)) {
    crop_vector <- .sacha_spatial_map_style_crop_kml()
    if (is.null(crop_vector)) {
      stop("Specify `crop_vector`, `crop_area`, or `crop_box`.", call. = FALSE)
    }
  }
  result <- if (!is.null(crop_vector)) {
    .sacha_spatial_crop_vector_boundary(crop_vector)
  } else {
    .sacha_spatial_map_style_crop_area_boundary(crop_area)
  }
  place <- result$place
  attr(result$boundary, "sacha_place") <- place
  result$crop_vector <- crop_vector
  result$crop_area <- crop_area
  result
}

.sacha_spatial_map_style_interval <- function(centre, span, lower, upper) {
  span <- min(span, upper - lower)
  from <- max(lower, centre - span / 2)
  to <- min(upper, centre + span / 2)
  if ((to - from) < span) {
    if (from <= lower) to <- min(upper, lower + span)
    if (to >= upper) from <- max(lower, upper - span)
  }
  c(from, to)
}

.sacha_spatial_map_style_frame <- function(occurrences, boundary = NULL, bbox = NULL,
                                           use_world = FALSE) {
  style <- .sacha_spatial_map_style_frame_variant(boundary)
  raw_bbox <- if (isTRUE(use_world)) {
    style$world_bbox
  } else if (!is.null(bbox)) {
    bbox
  } else {
    c(
      xmin = min(occurrences$decimalLongitude),
      ymin = min(occurrences$decimalLatitude),
      xmax = max(occurrences$decimalLongitude),
      ymax = max(occurrences$decimalLatitude)
    )
  }
  raw_bbox <- as.numeric(raw_bbox[c("xmin", "ymin", "xmax", "ymax")])
  names(raw_bbox) <- c("xmin", "ymin", "xmax", "ymax")
  if (any(!is.finite(raw_bbox)) || raw_bbox[["xmax"]] < raw_bbox[["xmin"]] ||
      raw_bbox[["ymax"]] < raw_bbox[["ymin"]]) {
    stop("The map frame requires finite geographic coordinates.", call. = FALSE)
  }
  lon_span <- max(raw_bbox[["xmax"]] - raw_bbox[["xmin"]], style$minimum_longitude_span)
  lat_span <- max(raw_bbox[["ymax"]] - raw_bbox[["ymin"]], style$minimum_latitude_span)
  lon_span <- lon_span * (1 + 2 * style$padding_fraction)
  lat_span <- lat_span * (1 + 2 * style$padding_fraction)
  lon_centre <- mean(raw_bbox[c("xmin", "xmax")])
  lat_centre <- mean(raw_bbox[c("ymin", "ymax")])
  cosine <- max(cos(lat_centre * pi / 180), style$minimum_cosine)
  physical_ratio <- lon_span * cosine / lat_span
  orientation <- if (physical_ratio > style$elongation_threshold) {
    "horizontal"
  } else if (physical_ratio < 1 / style$elongation_threshold) {
    "vertical"
  } else {
    "square"
  }
  aspect <- switch(
    orientation,
    horizontal = style$horizontal_aspect,
    vertical = style$vertical_aspect,
    square = style$square_aspect
  )
  target_lon_span <- aspect * lat_span / cosine
  if (lon_span < target_lon_span) {
    lon_span <- target_lon_span
  } else {
    lat_span <- lon_span * cosine / aspect
  }
  longitude <- .sacha_spatial_map_style_interval(lon_centre, lon_span, -180, 180)
  latitude <- .sacha_spatial_map_style_interval(
    lat_centre, lat_span, -style$latitude_limit, style$latitude_limit
  )
  list(
    bbox = c(xmin = longitude[1], ymin = latitude[1], xmax = longitude[2], ymax = latitude[2]),
    orientation = orientation,
    aspect = aspect,
    variant = if (is.null(boundary)) "complete" else "crop"
  )
}

.sacha_spatial_map_style_apply_adornments <- function(map, engine, context) {
  adornment_sets <- .sacha_spatial_map_style[[engine]]$adornments
  adornments <- c(adornment_sets$common, adornment_sets[[context$variant]])
  if (!length(adornments)) return(map)
  for (adornment in adornments) {
    if (!is.function(adornment)) {
      stop("Every map-style adornment must be a function.", call. = FALSE)
    }
    map <- adornment(map, context)
  }
  map
}

.sacha_spatial_map_style_export_size <- function(frame) {
  .sacha_spatial_map_style$static$export[[frame$variant]][[frame$orientation]]
}

.sacha_spatial_map_style_markdown_size <- function(mode, frame) {
  .sacha_spatial_map_style[[mode]]$markdown[[frame$variant]][[frame$orientation]]
}

.sacha_spatial_static_map_physical_width_cm <- function(frame) {
  size <- .sacha_spatial_map_style_export_size(frame)
  unname(size[["width"]])
}

.sacha_spatial_legend_species_name <- function(occurrences, language = NULL) {
  names_raw <- if ("scientificName" %in% names(occurrences)) {
    trimws(as.character(occurrences$scientificName))
  } else {
    character(0)
  }
  names_raw <- names_raw[!is.na(names_raw) & nzchar(names_raw)]
  if (!length(names_raw)) {
    language <- if (is.null(language)) "es" else language
    return(list(text = .sacha_spatial_label("map_legend", language), italic = FALSE))
  }
  dominant <- names(sort(table(names_raw), decreasing = TRUE))[1]
  words <- strsplit(dominant, "\\s+")[[1]]
  list(text = paste(words[seq_len(min(2L, length(words)))], collapse = " "), italic = TRUE)
}

.sacha_spatial_legend_boundary_label <- function(place, language) {
  if (!is.null(place) && nzchar(place)) return(place)
  language <- if (is.null(language)) "es" else language
  fallback <- if (identical(language, "en")) "Boundary" else "L\u00edmite"
  translated <- tryCatch(.sacha_spatial_label("map_legend_boundary", language),
                         error = function(e) NA_character_)
  if (is.null(translated) || length(translated) != 1L || is.na(translated) || !nzchar(translated)) {
    fallback
  } else {
    translated
  }
}

.sacha_spatial_static_legend_entries <- function(colour, boundary, place, style,
                                                 species_label, language) {
  entries <- list(list(
    label = species_label$text,
    fontface = if (isTRUE(species_label$italic)) "italic" else "plain",
    colour = colour,
    fill = colour,
    type = "point"
  ))
  if (!is.null(boundary) || (!is.null(place) && nzchar(place))) {
    entries <- c(entries, list(list(
      label = .sacha_spatial_legend_boundary_label(place, language),
      fontface = "plain",
      colour = style$boundary_border,
      fill = style$boundary_fill,
      type = "polygon"
    )))
  }
  entries
}

.sacha_spatial_static_north_scale_block_height <- function(style) {
  style$scalebar_height_cm + style$north_scale_gap_cm + style$compass_height_cm
}

.sacha_spatial_static_nice_value <- function(max_value, nice_steps) {
  candidates <- nice_steps[nice_steps <= max_value]
  if (length(candidates)) max(candidates) else min(nice_steps)
}

.sacha_spatial_static_scale_grob <- function(frame, style, offset = NULL) {
  bbox <- frame$bbox
  lon_span_deg <- unname(bbox["xmax"] - bbox["xmin"])
  lat_centre <- mean(bbox[c("ymin", "ymax")])
  km_per_degree <- 111.32 * cos(lat_centre * pi / 180)
  total_km <- max(lon_span_deg * km_per_degree, 1e-6)
  panel_width_cm <- .sacha_spatial_static_map_physical_width_cm(frame)
  cm_per_km <- panel_width_cm / total_km
  max_bar_km <- style$scalebar_max_width_cm / cm_per_km
  nice_km <- .sacha_spatial_static_nice_value(max_bar_km, style$scalebar_nice_steps_km)
  bar_width_cm <- nice_km * cm_per_km
  label <- if (nice_km >= 1) {
    sprintf("%g km", nice_km)
  } else {
    sprintf("%g m", nice_km * 1000)
  }

  offset_x <- if (is.null(offset)) grid::unit(0, "cm") else offset$x
  offset_y <- if (is.null(offset)) grid::unit(0, "cm") else offset$y
  x0 <- grid::unit(style$legend_margin_cm + style$north_scale_pad_x_cm, "cm") + offset_x
  y0 <- grid::unit(style$legend_margin_cm + style$north_scale_pad_y_cm, "cm") + offset_y
  half <- grid::unit(bar_width_cm / 2, "cm")
  height <- grid::unit(style$scalebar_height_cm, "cm")

  label_grob <- grid::textGrob(
    label, x = x0 + half * 2 + grid::unit(0.12, "cm"), y = y0 + height / 2,
    just = c("left", "center"),
    gp = grid::gpar(fontsize = style$scalebar_text_cex * 10, col = style$legend_text_colour)
  )

  grob <- grid::gTree(children = grid::gList(
    grid::rectGrob(
      x = x0, y = y0, width = half, height = height, just = c("left", "bottom"),
      gp = grid::gpar(fill = style$legend_text_colour, col = style$legend_text_colour, lwd = 0.6)
    ),
    grid::rectGrob(
      x = x0 + half, y = y0, width = half, height = height, just = c("left", "bottom"),
      gp = grid::gpar(fill = "white", col = style$legend_text_colour, lwd = 0.6)
    ),
    label_grob
  ))
  list(grob = grob, label_grob = label_grob, width_cm = bar_width_cm, label = label)
}

.sacha_spatial_static_legend_grob <- function(entries, style, scale_info = NULL,
                                              offset = NULL) {
  key <- grid::unit(style$legend_key_size_cm, "cm")
  row_h <- grid::unit(style$legend_row_height_cm, "cm")
  pad_x <- grid::unit(style$legend_pad_x_cm, "cm")
  text_x0 <- pad_x + key + grid::unit(style$legend_text_gap_cm, "cm")
  offset_x <- if (is.null(offset)) grid::unit(0, "cm") else offset$x
  offset_y <- if (is.null(offset)) grid::unit(0, "cm") else offset$y
  ox <- grid::unit(style$legend_margin_cm, "cm") + offset_x
  oy <- grid::unit(style$legend_margin_cm, "cm") + offset_y
  radius <- style$legend_corner_radius_cm

  north_scale_block <- .sacha_spatial_static_north_scale_block_height(style)
  base_y <- oy + grid::unit(style$north_scale_pad_y_cm, "cm") +
    grid::unit(north_scale_block, "cm") +
    grid::unit(style$legend_pad_y_gap_cm, "cm")

  n <- length(entries)
  top <- base_y + row_h * n + grid::unit(style$legend_pad_y_gap_cm, "cm")
  box_height <- top - oy

  label_gp <- function(entry) {
    grid::gpar(fontsize = style$legend_font_size, fontface = entry$fontface,
               col = style$legend_text_colour)
  }

  label_widths <- do.call(
    grid::unit.c,
    lapply(entries, function(entry) {
      grid::unit(1, "grobwidth", grid::textGrob(entry$label, gp = label_gp(entry)))
    })
  )
  content_width <- text_x0 + max(label_widths) + pad_x

  scale_width <- if (!is.null(scale_info)) {
    grid::unit(style$north_scale_pad_x_cm, "cm") +
      grid::unit(scale_info$width_cm, "cm") +
      grid::unit(0.12, "cm") +
      grid::unit(1, "grobwidth", scale_info$label_grob) +
      pad_x
  } else {
    grid::unit(0, "cm")
  }

  box_width <- max(
    grid::unit(style$legend_min_width_cm, "cm"), content_width, scale_width
  )

  children <- list()

  rings <- style$legend_shadow_rings
  for (k in rev(seq_len(rings))) {
    d <- k * style$legend_shadow_step_cm
    ring_alpha <- style$legend_shadow_alpha * (1 - (k - 1) / rings)
    children[[length(children) + 1]] <- grid::roundrectGrob(
      x = ox - grid::unit(d, "cm"),
      y = oy - grid::unit(d + style$legend_shadow_offset_cm, "cm"),
      width = box_width + grid::unit(2 * d, "cm"),
      height = box_height + grid::unit(2 * d, "cm"),
      just = c("left", "bottom"),
      r = grid::unit(radius + d, "cm"),
      gp = grid::gpar(
        fill = NA,
        col = grDevices::adjustcolor("black", alpha.f = ring_alpha),
        lwd = 1
      )
    )
  }

  children[[length(children) + 1]] <- grid::roundrectGrob(
    x = ox, y = oy,
    width = box_width, height = box_height,
    just = c("left", "bottom"),
    r = grid::unit(radius, "cm"),
    gp = grid::gpar(
      fill = grDevices::adjustcolor("white", alpha.f = style$legend_background_alpha),
      col = grDevices::adjustcolor("black", alpha.f = style$legend_border_alpha),
      lwd = style$legend_border_width
    )
  )

  point_stroke <- 0.5
  for (i in seq_len(n)) {
    entry <- entries[[i]]
    y <- base_y + row_h * (i - 1) + row_h * 0.5
    key_grob <- if (identical(entry$type, "polygon")) {
      grid::rectGrob(
        x = ox + pad_x, y = y, width = key, height = key, just = c("left", "center"),
        gp = grid::gpar(
          fill = grDevices::adjustcolor(entry$fill, alpha.f = style$boundary_alpha),
          col = entry$colour, lwd = style$boundary_border_width * ggplot2::.pt
        )
      )
    } else {
      grid::pointsGrob(
        x = ox + pad_x + key * 0.5, y = y, pch = 19,
        gp = grid::gpar(
          col = grDevices::adjustcolor(entry$colour, alpha.f = style$point_alpha),
          fontsize = style$point_size * ggplot2::.pt + point_stroke * ggplot2::.stroke / 2,
          lwd = point_stroke * ggplot2::.stroke / 2
        )
      )
    }
    text_grob <- grid::textGrob(
      entry$label, x = ox + text_x0, y = y, just = c("left", "center"), gp = label_gp(entry)
    )
    children[[length(children) + 1]] <- key_grob
    children[[length(children) + 1]] <- text_grob
  }

  grob <- grid::gTree(
    children = do.call(grid::gList, children),
    gp = grid::gpar(fontsize = style$legend_font_size)
  )
  list(grob = grob, width = box_width, height = box_height)
}

.sacha_spatial_static_legend_position <- function(position) {
  choices <- c(
    "bottomleft", "bottom", "bottomright", "right",
    "topright", "top", "topleft", "left"
  )
  if (is.null(position)) return(choices[[1L]])
  if (is.character(position) && length(position) == 1L && !is.na(position) &&
      position %in% choices) {
    return(position)
  }
  match.arg(
    if (is.character(position) && length(position) == 1L && !is.na(position)) {
      position
    } else {
      ""
    },
    choices
  )
}

.sacha_spatial_static_legend_offset <- function(position, legend_size, style) {
  shadow <- style$legend_shadow_rings * style$legend_shadow_step_cm
  margin <- style$legend_margin_cm
  shadow_offset <- style$legend_shadow_offset_cm
  width <- legend_size$width
  height <- legend_size$height
  horizontal <- if (grepl("left$", position)) {
    "left"
  } else if (grepl("right$", position)) {
    "right"
  } else {
    "center"
  }
  vertical <- if (startsWith(position, "top")) {
    "top"
  } else if (startsWith(position, "bottom")) {
    "bottom"
  } else {
    "center"
  }
  x <- switch(
    horizontal,
    left = grid::unit(shadow, "cm"),
    center = (grid::unit(1, "npc") - width) / 2 - grid::unit(margin, "cm"),
    right = grid::unit(1, "npc") - width - grid::unit(2 * margin + shadow, "cm")
  )
  y <- switch(
    vertical,
    bottom = grid::unit(shadow + shadow_offset, "cm"),
    center = (grid::unit(1, "npc") - height) / 2 - grid::unit(margin - shadow_offset, "cm"),
    top = grid::unit(1, "npc") - height -
      grid::unit(2 * margin + shadow - shadow_offset, "cm")
  )
  list(x = x, y = y)
}

.sacha_spatial_static_scale_compass_legend <- function(map, context) {
  style <- .sacha_spatial_map_style$static$common
  language <- if (is.null(context$language)) "es" else context$language

  species_label <- .sacha_spatial_legend_species_name(context$occurrences, language)
  entries <- .sacha_spatial_static_legend_entries(
    context$colour, context$boundary, context$place, style, species_label, language
  )
  scale_info <- .sacha_spatial_static_scale_grob(context$frame, style)
  legend_info <- .sacha_spatial_static_legend_grob(entries, style, scale_info)
  position <- context$static_legend_position %spatial||% "bottomleft"
  offset <- NULL
  if (!identical(position, "bottomleft")) {
    offset <- .sacha_spatial_static_legend_offset(position, legend_info, style)
    scale_info <- .sacha_spatial_static_scale_grob(context$frame, style, offset)
    legend_info <- .sacha_spatial_static_legend_grob(
      entries, style, scale_info, offset
    )
  }
  children <- list(legend_info$grob, scale_info$grob)
  if (requireNamespace("ggspatial", quietly = TRUE)) {
    compass <- ggspatial::GeomNorthArrow[["draw_panel"]](
      data = data.frame(which_north = "true", location = "bl"),
      panel_params = list(
        x_range = context$frame$bbox[c("xmin", "xmax")],
        y_range = context$frame$bbox[c("ymin", "ymax")],
        crs = sf::st_crs(style$crs %spatial||% 4326)
      ),
      coordinates = map$coordinates,
      height = grid::unit(style$compass_height_cm, "cm"),
      width = grid::unit(style$compass_width_cm, "cm"),
      pad_x = grid::unit(
        style$legend_margin_cm + style$north_scale_pad_x_cm, "cm"
      ),
      pad_y = grid::unit(
        style$legend_margin_cm + style$north_scale_pad_y_cm +
          style$scalebar_height_cm + style$north_scale_gap_cm,
        "cm"
      ),
      rotation = NULL,
      style = ggspatial::north_arrow_fancy_orienteering(
        line_col = style$legend_text_colour,
        text_col = style$legend_text_colour,
        text_size = 6
      )
    )
    offset_x <- if (is.null(offset)) grid::unit(0, "cm") else offset$x
    offset_y <- if (is.null(offset)) grid::unit(0, "cm") else offset$y
    compass$vp$x <- grid::unit(
      style$legend_margin_cm + style$north_scale_pad_x_cm +
        style$compass_width_cm / 2,
      "cm"
    ) + offset_x
    compass$vp$y <- grid::unit(
      style$legend_margin_cm + style$north_scale_pad_y_cm +
        style$scalebar_height_cm + style$north_scale_gap_cm +
        style$compass_height_cm / 2,
      "cm"
    ) + offset_y
    children[[length(children) + 1L]] <- compass
  }
  composition <- grid::gTree(children = do.call(grid::gList, children))
  map <- map + ggplot2::annotation_custom(
    composition, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )
  map
}

.sacha_spatial_static_dynamic_grid <- function(map, context) {
  style <- .sacha_spatial_map_style$static$common
  if (!isTRUE(style$grid_show)) return(map)

  bbox <- context$bbox
  span_x <- unname(bbox["xmax"] - bbox["xmin"])
  span_y <- unname(bbox["ymax"] - bbox["ymin"])
  nice_steps <- style$grid_nice_steps_deg

  reference_span <- (span_x + span_y) / 2
  divisions_per_step <- reference_span / nice_steps
  step <- nice_steps[which.min(abs(divisions_per_step - style$grid_target_divisions))]

  digits_val <- 0L
  while (digits_val < 6L && !isTRUE(all.equal(step, round(step, digits_val)))) {
    digits_val <- digits_val + 1L
  }

  x_breaks <- seq(
    floor(bbox[["xmin"]] / step) * step,
    ceiling(bbox[["xmax"]] / step) * step,
    by = step
  )
  y_breaks <- seq(
    floor(bbox[["ymin"]] / step) * step,
    ceiling(bbox[["ymax"]] / step) * step,
    by = step
  )
  label_fmt <- function(x) formatC(x, format = "f", digits = digits_val)

  map +
    ggplot2::scale_x_continuous(breaks = x_breaks, labels = label_fmt, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = y_breaks, labels = label_fmt, expand = c(0, 0)) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = style$grid_text_size, colour = style$grid_text_colour),
      axis.ticks = ggplot2::element_line(colour = style$grid_line_colour, linewidth = style$grid_line_width),
      axis.ticks.length = grid::unit(0.1, "cm"),
      axis.title = ggplot2::element_blank()
    )
}

.sacha_spatial_map_style$static$adornments$common <- list(
  .sacha_spatial_static_dynamic_grid,
  .sacha_spatial_static_scale_compass_legend
)

.sacha_spatial_static_map <- function(occurrences, colour, zoom, boundary = NULL, bbox = NULL,
                                      language = NULL, legend_place = NULL,
                                      static_legend_position = "bottomleft") {
  .sacha_spatial_require(c("ggplot2", "rnaturalearth", "sf"))
  static_legend_position <- .sacha_spatial_static_legend_position(
    static_legend_position
  )
  language <- if (is.null(language)) "es" else language
  style <- .sacha_spatial_map_style_variant("static", boundary)
  frame <- .sacha_spatial_map_style_frame(
    occurrences, boundary, bbox, use_world = !isTRUE(zoom) && is.null(boundary)
  )
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world, fill = style$land_fill, color = style$land_border,
                     linewidth = style$land_border_width)
  if (!is.null(boundary)) {
    map <- map + ggplot2::geom_sf(
      data = boundary, fill = style$boundary_fill, alpha = style$boundary_alpha,
      color = style$boundary_border, linewidth = style$boundary_border_width, linetype = "solid"
    )
  }
  map <- map +
    ggplot2::geom_point(
      data = occurrences,
      ggplot2::aes(x = decimalLongitude, y = decimalLatitude),
      color = colour, alpha = style$point_alpha, size = style$point_size
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = style$panel_background, color = NA),
      plot.margin = ggplot2::margin(style$plot_margin, style$plot_margin,
                                    style$plot_margin, style$plot_margin),
      aspect.ratio = style$aspect_ratio
    )
  coord_args <- list(expand = style$coord_expand, crs = style$crs)
  coord_args$xlim <- c(frame$bbox["xmin"], frame$bbox["xmax"])
  coord_args$ylim <- c(frame$bbox["ymin"], frame$bbox["ymax"])
  map <- map + do.call(ggplot2::coord_sf, coord_args)
  boundary_place <- if (!is.null(boundary)) attr(boundary, "sacha_place") else NULL
  place <- legend_place %spatial||% boundary_place
  map <- .sacha_spatial_map_style_apply_adornments(
    map, "static",
    list(occurrences = occurrences, colour = colour, boundary = boundary, bbox = frame$bbox,
         frame = frame, variant = frame$variant, place = place, language = language,
         static_legend_position = static_legend_position)
  )
  attr(map, "sacha_map_frame") <- frame
  map
}

.sacha_spatial_interactive_legend_html <- function(colour, boundary, place, style,
                                                   species_label, language) {
  escape <- function(x) htmltools::htmlEscape(x)

  species_text <- if (isTRUE(species_label$italic)) {
    sprintf("<em>%s</em>", escape(species_label$text))
  } else {
    escape(species_label$text)
  }
  marker_diameter <- 2 * style$marker_radius
  marker_css <- grDevices::adjustcolor(colour, alpha.f = style$marker_fill_opacity)
  point_row <- sprintf(
    "<div style='display:flex;align-items:center;'>
       <span style='display:inline-block;box-sizing:border-box;width:%spx;height:%spx;margin:0 %spx 0 2px;background:%s;border-radius:50%%;'></span>%s
     </div>",
    marker_diameter, marker_diameter, 18 - 2 - marker_diameter, marker_css, species_text
  )
  rows <- point_row
  if (!is.null(boundary) || (!is.null(place) && nzchar(place))) {
    boundary_fill_css <- grDevices::adjustcolor(style$boundary_fill, alpha.f = style$boundary_fill_opacity)
    boundary_border_css <- grDevices::adjustcolor(style$boundary_border, alpha.f = style$boundary_border_opacity)
    polygon_row <- sprintf(
      "<div style='display:flex;align-items:center;margin-bottom:3px;'>
         <span style='display:inline-block;box-sizing:border-box;width:12px;height:12px;margin-right:6px;background:%s; border:%spx solid %s;'></span>%s
       </div>",
      boundary_fill_css, style$boundary_border_weight, boundary_border_css,
      escape(.sacha_spatial_legend_boundary_label(place, language))
    )
    rows <- paste0(polygon_row, point_row)
  }
  sprintf(
    paste0(
      "<div class='sacha-legend-box' style='background:rgba(255,255,255,%s); ",
      "padding:6px 10px; border-radius:4px; box-shadow:0 1px 4px rgba(0,0,0,0.25); ",
      "margin:%spx; font-size:12px; line-height:1.3;'>%s</div>"
    ),
    style$legend_opacity, style$legend_margin_px, rows
  )
}

.sacha_spatial_interactive_map <- function(occurrences, colour, language,
                                           boundary = NULL, bbox = NULL,
                                           legend_place = NULL) {
  .sacha_spatial_require(c("leaflet", "htmlwidgets", "htmltools"))
  language <- if (is.null(language)) "es" else language
  style <- .sacha_spatial_map_style_variant("interactive", boundary)
  frame <- .sacha_spatial_map_style_frame(occurrences, boundary, bbox)
  unavailable <- .sacha_spatial_label("unavailable", language)
  popup_value <- function(value) {
    value <- trimws(as.character(value))
    value[is.na(value) | !nzchar(value)] <- unavailable
    htmltools::htmlEscape(value)
  }
  popup <- paste0(
    "<div style='font-family: ", style$popup_font_family, ";'>", "<b>",
    popup_value(occurrences$scientificName), "</b><br>",
    "<b>", .sacha_spatial_label("popup_year", language), ":</b> ",
    popup_value(occurrences$year), "<br>",
    "<b>", .sacha_spatial_label("popup_locality", language), ":</b> ",
    popup_value(occurrences$stateProvince), "<br>",
    "<b>", .sacha_spatial_label("popup_observer", language), ":</b> ",
    popup_value(occurrences$recordedBy), "</div>"
  )

  map <- leaflet::leaflet(
    data = occurrences,
    options = leaflet::leafletOptions(zoomControl = style$zoom_control,
                                      attributionControl = style$attribution_control,
                                      minZoom = style$minimum_zoom,
                                      worldCopyJump = style$world_copy_jump,
                                      zoomSnap = 0)
  )

  map <- leaflet::setMaxBounds(map, lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90)

  for (provider in style$tile_providers) {
    map <- leaflet::addProviderTiles(
      map, provider, group = provider,
      options = leaflet::providerTileOptions(noWrap = style$tile_no_wrap)
    )
  }
  map <- leaflet::addLayersControl(
    map, baseGroups = style$tile_providers,
    options = leaflet::layersControlOptions(collapsed = style$layers_collapsed,
                                            position = style$layers_control_position)
  )
  if (!is.null(boundary)) {
    map <- leaflet::addPolygons(
      map, data = boundary, fillColor = style$boundary_fill,
      fillOpacity = style$boundary_fill_opacity, color = style$boundary_border,
      weight = style$boundary_border_weight, opacity = style$boundary_border_opacity
    )
  }
  map <- leaflet::addCircleMarkers(
    map, lng = ~decimalLongitude, lat = ~decimalLatitude, radius = style$marker_radius,
    color = colour, fillColor = colour, fillOpacity = style$marker_fill_opacity,
    stroke = FALSE, popup = popup
  )

  boundary_place <- if (!is.null(boundary)) attr(boundary, "sacha_place") else NULL
  place <- legend_place %spatial||% boundary_place
  species_label <- .sacha_spatial_legend_species_name(occurrences, language)
  map <- leaflet::addControl(
    map,
    html = .sacha_spatial_interactive_legend_html(
      colour, boundary, place, style, species_label, language
    ),
    position = style$legend_position,
    className = "sacha-legend-control"
  )

  map <- leaflet::fitBounds(
    map, lng1 = unname(frame$bbox["xmin"]), lat1 = unname(frame$bbox["ymin"]),
    lng2 = unname(frame$bbox["xmax"]), lat2 = unname(frame$bbox["ymax"]),
    options = list(padding = as.integer(style$fit_padding_px), animate = FALSE)
  )

  if (requireNamespace("leaflet.extras", quietly = TRUE)) {
    map <- leaflet.extras::addFullscreenControl(map, position = style$fullscreen_control_position)
  }

  map <- .sacha_spatial_map_style_apply_adornments(
    map, "interactive",
    list(occurrences = occurrences, colour = colour, boundary = boundary, bbox = frame$bbox,
         frame = frame, language = language, variant = frame$variant, place = place)
  )
  attr(map, "sacha_map_frame") <- frame
  map
}
