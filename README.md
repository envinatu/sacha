<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![CRAN status](https://www.r-pkg.org/badges/version/sacha)](https://CRAN.R-project.org/package=sacha) [![](http://cranlogs.r-pkg.org/badges/grand-total/sacha?color=green)](https://cran.r-project.org/package=sacha) [![Codecov test coverage](https://codecov.io/gh/envinatu/sacha/graph/badge.svg)](https://app.codecov.io/gh/envinatu/sacha) [![R-hub](https://github.com/envinatu/sacha/actions/workflows/rhub.yaml/badge.svg)](https://github.com/envinatu/sacha/actions/workflows/rhub.yaml) [![R-CMD-check](https://github.com/envinatu/sacha/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/envinatu/sacha/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

# sacha

<img src="man/figures/logo.png" align="right" height="139"/>

`sacha` supports species-centred biodiversity workflows by bringing together taxonomic and nomenclatural information from ChecklistBank and the Catalogue of Life, occurrence records from GBIF, and observation photographs from iNaturalist. Its functions format taxonomic names, classification, synonyms, common names, references, and documented geographic areas; create distribution maps and image compositions; and assemble Markdown reports for scientific documents.

The package separates interactive access from document output. Plain functions return or display results for use in R, while corresponding `_md()` functions produce Markdown for Quarto and R Markdown. Shared configuration and argument defaults help keep repeated analyses consistent.

## Installation

Install the development version from GitHub:

``` r
install.packages("pak")
pak::pkg_install("envinatu/sacha")
```

## Quick start

``` r
library(sacha)

sacha_configure(
  clb = list(language = "en"),
  inat = list(language = "en"),
  spatial = list(language = "en")
)

sc_full_name("Bidens andicola")
classification("Bidens andicola")
common_names("Bidens andicola")
```

These accessors query their configured ChecklistBank service. A connection to the relevant service is required when the result is not already available in the session cache.

For a static map of georeferenced records:

``` r
gbif_map(
  "Bidens andicola",
  source = "web",
  mode = "static"
)
```

To inspect iNaturalist photographs interactively:

``` r
inat_images(
  "Bidens andicola",
  source = "web",
  number_observations = 1
)
```

## Use in Quarto and R Markdown

The `_md()` functions and report functions return Markdown suitable for a chunk with `results: asis`. For example, this chunk inserts a taxonomic report into a Quarto document:

```{r}
#| label: bidens-taxonomic-report
#| results: asis
#| echo: false

sacha_clb_report("Bidens andicola", language = "en")
```

An iNaturalist image panel can be inserted in the same way:

```{r}
#| label: bidens-observation-images
#| results: asis
#| echo: false

inat_images_md(
  "Bidens andicola",
  source = "web",
  number_observations = 1
)
```

For a combined species report, `sacha_report()` joins taxonomic sections, iNaturalist images, geographic information, and a GBIF map. Its default `source = "local"` applies to the iNaturalist and GBIF sections, which requires the relevant local exports to exist. ChecklistBank-backed sections continue to use their configured web service, so this report is not fully offline even in local mode.

## Data sources and local workflows

The `source` argument uses the same choices where it is available; each module applies them as follows:

| Module | Web source | Local source |
|:---|:---|:---|
| ChecklistBank and Catalogue of Life | Accessors query the configured service. | Local source support for downloading and reading ColDP archives is currently under development. |
| GBIF | `source = "web"` queries GBIF for georeferenced occurrence records. | `source = "local"` reads occurrence exports previously saved by the package. |
| iNaturalist | `source = "web"` queries iNaturalist observations and photographs. | `source = "local"` uses images and metadata previously saved by the package. |

`gbif_save_occurrences()` saves a local occurrence export for later use by GBIF functions. The export can include many records, so consider the expected data volume before requesting it. `inat_save_images()` saves photographs and associated metadata for later local use. Both types of taxon-specific exports are stored under `sacha_exports/Family_Genus_species` in the selected base directory; iNaturalist files are kept in its `inat` subdirectory.

## Configuration and reproducibility

`sacha_configure()` sets module-specific defaults, including language and service options. `sacha_arg_configure()` stores shared argument values for subsequent function calls; values passed directly to a function take precedence. `sacha_clear_cache()` restores configuration and shared argument defaults and clears session caches. It does not remove exported files.

For example, set a shared species name once and override it in any individual call when needed:

``` r
sacha_arg_configure(species = "Bidens andicola")

sc_full_name()
classification()

sacha_clear_cache()
```

## Related application

`sacha` is being used alongside `pacha` in the Quarto book [*Plantas susceptibles de uso sostenible en la zona andina de la provincia de Cotopaxi, Ecuador*](https://envinatu.github.io/envyura/), which applies both packages to the study of plants and sustainable use in the Andean region.


## Attribution and responsible access

The package retrieves data from external services; records and media remain subject to the source's terms, licences, and citation requirements. Check the licence attached to each iNaturalist photograph before reuse. Image captions include the photograph's licence and attribution information while linking to the corresponding observation. GBIF records should be cited according to GBIF's guidance and the data publishers' terms.

The services and their records can change over time. Prefer local exports when repeating analyses, request only the records needed, and avoid unnecessary repeated or concurrent requests. iNaturalist publishes request and media download recommendations in its [API recommended practices](https://www.inaturalist.org/pages/api+recommended+practices). For service details, see the [ChecklistBank API](https://api.checklistbank.org/), [GBIF API documentation](https://techdocs.gbif.org/en/openapi/), and [iNaturalist API v2 documentation](https://api.inaturalist.org/v2/docs/).

## Contributing

Contributions are very welcome. Please see CONTRIBUTING.md for the package's documentation and development conventions. By participating, contributors agree to follow the project's Code of Conduct.
