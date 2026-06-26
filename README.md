# housereportstyle

`housereportstyle` is a reusable Quarto report style package. It carries the
house style from the original foraging reporting package into a standalone
helper package for general scientific reports, while keeping the scanpath and
foraging-report helpers available for true foraging reports.

Install with:

```r
pak::pak("mjgreen/housereportstyle")
```

The package provides:

- APA-compatible HTML table helpers built around `gt`.
- Quarto figure conventions, including captions owned by Quarto and notes in
  margin asides.
- Report and scanpath plot themes based on `jtools::theme_apa()`.
- Scanpath method blocks for ScaSim distance audits, MDS dimensionality sweeps,
  Mclust selection, chi-square screens, GLM/GLMM screens, participant bridge
  summaries, and representative scanpaths.
- Geometry-aware scanpath helpers that auto-detect row/column layout bounds
  from target-native event tables and retain those bounds for downstream plots.
- Scanpath identity helpers that include `device` automatically when a study has
  participant-device-trial scanpaths.
- Report scaffolds for general scientific reports and scanpath-strategy reports.
- Shared HTML house style: floating left contents, numbered sections, light/dark
  mode toggle, portable embedded resources, readable tables, and report-level
  CSS copied by `create_report()`.
- Gentle style validation checks for future reports and AI-assisted edits.

Recommended workflow:

1. Load the package with `library(housereportstyle)`.
2. Read `inst/ai/REPORTING_STYLE.md`.
3. Scaffold a report with `create_report()`.
4. Keep expensive model chunks cached and separate from display chunks.
5. Run `validate_report_style()` before final render.
