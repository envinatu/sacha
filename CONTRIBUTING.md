# Contributing to sacha

Thanks for contributing! This document covers the conventions this
package follows so new code stays consistent with existing modules
(`coldp`, `core`, `spatial`, `inaturalist`).

## Documentation conventions (roxygen2)

The full rationale for these rules lives in `R/sacha-package.R`
(`?sacha` after loading the package) — read it once before writing
new accessors. Summary:

### Plain / `_md` twin functions

Many accessors ship as a plain-text version and a `_md`
(Markdown-formatted) version meant for use inside a rendered report.

- **Identical signature, output-format-only difference** → share one
  help page with `@rdname` (see the `coldp` module).
- **`_md` has its own parameters, or a different error-handling
  contract** (e.g. a communication failure is silenced in `_md` but
  raised as an error in the plain version) → document on **separate**
  pages (see `gbif_map()`/`gbif_map_md()`, `col_locations()`/
  `col_locations_md()`).

If you're unsure which bucket a new pair falls into, write the two
functions first — the signature and error behavior will make it
obvious.

### One-line wrapper families

A family of thin wrappers delegating to one underlying function (e.g.
`ipni_url()`, `gbif_url()`, `col_url()`, ... → `indexation_urls()`)
gets **one shared `@rdname` page**, with a `@details` table mapping
each wrapper to what it fixes. Keep `@export` on every wrapper; only
the documentation is consolidated.

### Shared parameters between non-twin functions

If two functions share most parameters but aren't a plain/`_md` pair
(e.g. `inat_images()` vs. `save_inat_images()`), don't repeat
`@param` blocks — use `@inheritParams <reference_function>` and
document only what's specific to that function.

### Language argument

Every module resolves `language` dynamically from `inst/lang/*.yml`.
**Never** hardcode a closed set like `language = c("es", "en")` with
`match.arg()` — that breaks the "drop a new `.yml` file to add a
language" contract the rest of the package relies on.

## Before opening a PR

- Run `devtools::document()` and commit the resulting `man/*.Rd`
  changes.
- Run `devtools::check()` and make sure it's clean (or that any
  existing NOTEs are unrelated to your change).
- If you added or changed a `_md` twin, confirm in your PR description
  which bucket above it falls into and why.