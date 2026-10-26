library(hexSticker)
library(ggplot2)

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

# ---- Paletas ---------------------------------------------------------------

paletas <- list(
  logo = list(
    bg = "#FFFFFA", borde = "#0B7BD0", titulo = "#0B3C6E", url = "#57606A",
    ink = "#24292F", green = "#1A7431", blue = "#0969DA", amber = "#D4A017",
    earth = "#8B5E3C", teal = "#1B9E9E", lima = "#1AA64A"
  ),
  logo_dark = list(
    bg = "#0D1117", borde = "#2AA3E0", titulo = "#F0F6FC", url = "#79C0FF",
    ink = "#8B949E", green = "#3FB950", blue = "#58A6FF", amber = "#F2CC60",
    earth = "#C9A27A", teal = "#56D4DD", lima = "#3DDC7A"
  )
)

# ---- Geometría base ---------------------------------------------------------

rad <- function(a) a * pi / 180

elipse <- function(cx, cy, a, b, n = 120) {
  t <- seq(0, 2 * pi, length.out = n)
  data.frame(x = cx + a * cos(t), y = cy + b * sin(t))
}

circulo <- function(cx, cy, r) elipse(cx, cy, r, r)

hoja_arco <- function(cx, cy, r, a0, a1, w, n = 60) {
  t <- seq(0, 1, length.out = n)
  a <- rad(a0 + (a1 - a0) * t)
  g <- w * sin(pi * t)
  data.frame(
    x = c(cx + (r + g) * cos(a), cx + rev((r - g) * cos(a))),
    y = c(cy + (r + g) * sin(a), cy + rev((r - g) * sin(a)))
  )
}

lienzo <- function() {
  ggplot() +
    coord_fixed(xlim = c(-10, 10), ylim = c(-10, 10), expand = FALSE) +
    scale_fill_identity() +
    scale_colour_identity() +
    theme_void() +
    theme_transparent()
}

poli <- function(d, fill, col = NA, alpha = 1, lw = 0.4) {
  geom_polygon(
    data = d, aes(x = x, y = y), fill = fill, colour = col,
    alpha = alpha, linewidth = lw
  )
}

ruta <- function(d, col, lw = 1, alpha = 1) {
  geom_path(
    data = d, aes(x = x, y = y),
    colour = col, linewidth = lw, alpha = alpha, lineend = "round"
  )
}

seg <- function(d, col, lw = 1, alpha = 1) {
  geom_segment(
    data = d, aes(x = x, y = y, xend = xend, yend = yend),
    colour = col, linewidth = lw, alpha = alpha, lineend = "round"
  )
}

pts <- function(d, col, size = 3) {
  geom_point(data = d, aes(x = x, y = y), colour = col, size = size, shape = 16)
}

# ---- Símbolos ---------------------------------------------------------------

# Libro abierto (páginas azules, líneas de texto con el color de fondo)
libro <- function(k, s = 0.85, dy = -2.0) {
  izq <- data.frame(x = c(-7, -0.35, -0.35, -7), y = c(-4.2, -5.4, 0.8, 2))
  der <- data.frame(x = -izq$x, y = izq$y)
  d <- c(1.4, 2.6, 3.8)
  li <- data.frame(
    x = -6, y = 2 - (-6 + 7) * 1.2 / 6.65 - d,
    xend = -1.4, yend = 2 - (-1.4 + 7) * 1.2 / 6.65 - d
  )
  ld <- data.frame(x = 6, y = li$y, xend = 1.4, yend = li$yend)
  tr <- function(p) {
    p[intersect(names(p), c("x", "xend"))] <- p[intersect(names(p), c("x", "xend"))] * s
    p[intersect(names(p), c("y", "yend"))] <- p[intersect(names(p), c("y", "yend"))] * s + dy
    p
  }
  list(
    poli(tr(izq), k$blue), poli(tr(der), k$blue, alpha = 0.85),
    seg(tr(li), k$bg, 0.8), seg(tr(ld), k$bg, 0.8)
  )
}

# "S" de dos arcos-hoja con contorno fino
ese <- function(k, cx, cy, s, c1, c2, borde, tam) {
  p <- data.frame(
    x = cx + 2.5 * s * c(cos(rad(20)), cos(rad(-160))),
    y = cy + s * c(2.5 + 2.5 * sin(rad(20)), -2.5 + 2.5 * sin(rad(-160)))
  )
  list(
    poli(hoja_arco(cx, cy + 2.5 * s, 2.5 * s, 20, 270, 0.95 * s), c1, col = borde, lw = 0.15),
    poli(hoja_arco(cx, cy - 2.5 * s, 2.5 * s, -160, 90, 0.95 * s), c2, col = borde, lw = 0.15),
    pts(p, k$amber, tam)
  )
}

# Globo: disco azul con retícula clara y "S", con aro de separación
globo_s <- function(k, cx, cy, r) {
  ret <- function(d) ruta(d, k$bg, 0.45, 0.7)
  c(
    list(poli(circulo(cx, cy, r * 1.08), k$bg), poli(circulo(cx, cy, r), k$blue)),
    lapply(c(0.4, 0.75), function(f) ret(elipse(cx, cy, r * f, r))),
    list(ret(data.frame(x = cx, y = cy + c(-r, r)))),
    lapply(c(-0.55, 0, 0.55), function(f) {
      ret(data.frame(x = cx + c(-1, 1) * sqrt(r^2 - (r * f)^2), y = cy + r * f))
    }),
    ese(k, cx, cy, 0.145 * r, k$bg, k$lima, k$blue, max(1.6, 0.7 * r))
  )
}

# Pez
pez <- function(k, cx, cy, s) {
  cx <- cx + 0.3 * s
  list(
    poli(data.frame(x = cx + c(-0.8, -1.6, -1.6) * s, y = cy + c(0, 0.6, -0.6) * s), k$teal),
    poli(elipse(cx, cy, s, 0.55 * s), k$blue),
    pts(data.frame(x = cx + 0.5 * s, y = cy + 0.12 * s), k$bg, 1.5)
  )
}

# Rapaz genérica y minimalista con las alas abiertas (vista frontal, cabeza de
# perfil): cobertoras azules, remeras celestes con puntas en "dedos", cola verde
# y pico amarillo.
rapaz <- function(k, cx, cy, s) {
  P <- function(x, y) data.frame(x = cx + x * s, y = cy + y * s)
  ala <- data.frame(
    x = c(0.2, 0.8, 1.5, 2.2, 2.7, 3.05, 2.75, 2.95, 2.55, 2.6, 2.25, 2.15, 1.8, 1.2, 0.6, 0.2),
    y = c(0.35, 0.8, 1.0, 0.95, 0.65, 0.15, 0.2, -0.35, -0.1, -0.65, -0.25, -0.8, -0.3, -0.55, -0.6, -0.5)
  )
  cob <- data.frame(
    x = c(0.2, 0.8, 1.5, 2.2, 2.7, 2.5, 1.6, 0.9, 0.2),
    y = c(0.35, 0.8, 1.0, 0.95, 0.65, 0.35, 0.3, 0.2, 0.05)
  )
  lados <- lapply(c(-1, 1), function(sg) {
    list(poli(P(sg * ala$x, ala$y), k$teal), poli(P(sg * cob$x, cob$y), k$blue))
  })
  c(
    list(poli(P(c(-0.28, 0.28, 0.55, 0.3, 0, -0.3, -0.55),
                c(-0.5, -0.5, -1.3, -1.45, -1.5, -1.45, -1.3)), k$lima)),
    lados,
    list(
      poli(elipse(cx, cy, 0.32 * s, 0.75 * s), k$blue),
      poli(elipse(cx + 0.04 * s, cy + 0.95 * s, 0.26 * s, 0.26 * s), k$blue),
      poli(P(c(0.18, 0.6, 0.62, 0.45, 0.18), c(1.05, 0.92, 0.72, 0.8, 0.78)), k$amber)
    )
  )
}

# Árbol: tronco café y copa verde
arbol <- function(k, cx, cy, s) {
  tronco <- data.frame(x = cx + c(-0.22, 0.22, 0.1, -0.1) * s,
                       y = cy + c(-1, -1, 0.4, 0.4) * s)
  ramas <- data.frame(x = cx, y = cy + c(-0.2, -0.4) * s,
                      xend = cx + c(-0.5, 0.45) * s, yend = cy + c(0.2, 0.05) * s)
  copa <- data.frame(x = c(-0.55, 0.55, 0, 0), y = c(0.3, 0.3, 0.85, 0.35),
                     r = c(0.55, 0.55, 0.7, 0.6))
  c(
    list(poli(tronco, k$earth), seg(ramas, k$earth, 1.2)),
    lapply(seq_len(nrow(copa)), function(i) {
      poli(circulo(cx + copa$x[i] * s, cy + copa$y[i] * s, copa$r[i] * s),
           k$green, col = k$bg, lw = 0.15)
    }),
    list(poli(circulo(cx - 0.2 * s, cy + 1.05 * s, 0.3 * s), k$lima, alpha = 0.8))
  )
}

# ---- Composición ------------------------------------------------------------

subplot_logo <- function(k) {
  lienzo() +
    libro(k) +
    globo_s(k, 0, 2.1, 2.6) +
    arbol(k, -4.7, 2.4, 1.05) +
    pez(k, 4.9, 2.6, 1.0) +
    rapaz(k, 0, 5.8, 0.45)
}

# ---- Generación -------------------------------------------------------------

for (nombre in names(paletas)) {
  k <- paletas[[nombre]]
  # hexSticker pasa `type`/`antialias` a ggsave(); con ragg instalado ggplot2
  # avisa que los ignora. Es inofensivo, así que se silencia.
  suppressWarnings(suppressMessages(sticker(
    subplot = subplot_logo(k), package = "sacha",
    p_size = 28, p_color = k$titulo, p_y = 1.5,
    s_x = 1, s_y = 0.75, s_width = 1.2, s_height = 1.2,
    h_fill = k$bg, h_color = k$borde,
    url = "github.com/envinatu/sacha",
    u_color = k$url, u_size = 4.2,
    filename = file.path("man/figures", paste0(nombre, ".png"))
  )))
}
