# housereportstyle

`housereportstyle` is a reusable Quarto report style package for scientific
reports. It provides report scaffolds, validation checks, table helpers, plot
themes, and scanpath helpers in one installable package.

Install with:

```r
pak::pkg_install("mjgreen/housereportstyle")
```

The package provides:

- APA-compatible HTML table helpers built around `gt`.
- Quarto table and figure conventions, including numbers and captions owned by
  Quarto, prose cross-references, and notes in margin asides.
- Report and scanpath plot themes based on Jacob A. Long's `jtools`
  package, including `jtools::theme_apa()`
  (<https://github.com/jacob-long/jtools>).
- Scanpath method blocks for ScaSim distance audits, using Titus von der
  Malsburg's `scanpath` R package (<https://github.com/tmalsburg/scanpath>)
  and the von der Malsburg & Vasishth (2011) method, MDS dimensionality sweeps,
  Mclust selection, chi-square screens, GLM/GLMM screens, participant bridge
  summaries, and representative scanpaths.
- Geometry-aware scanpath helpers that auto-detect row/column layout bounds
  from target-native event tables and retain those bounds for downstream plots.
- Scanpath identity helpers that include `device` automatically when a study has
  participant-device-trial scanpaths.
- Report scaffolds for general scientific reports and scanpath-strategy reports.
- Shared HTML house style: floating right contents, numbered sections, labelled
  light/dark mode toggle, portable embedded resources, readable tables, and
  report-level CSS copied by `create_report()`.
- Gentle style validation checks for future reports and AI-assisted edits.

Recommended workflow:

1. Load the package with `library(housereportstyle)`.
2. Read `inst/ai/REPORTING_STYLE.md`.
3. Scaffold a report with `create_report()`.
4. Keep expensive model chunks cached and separate from display chunks.
5. Run `validate_report_style()` before final render.
