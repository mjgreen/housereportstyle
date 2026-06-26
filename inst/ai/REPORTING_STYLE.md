# House Report Style

Read this before editing a Quarto report that uses the shared house style. The
goal is a scientific report that can be regenerated without hand-renumbering,
copied between projects, and read as a standalone HTML file.

## Non-Negotiables

- The report should answer scientific questions, not advertise itself.
- Keep provenance visible before modelling.
- Separate expensive data/model chunks from display chunks.
- Keep hidden validation checks in the report even when they are not part of the
  narrative.
- Let Quarto own all table and figure labels, titles, numbering, captions, and
  back-references.
- Follow every reader-facing table and figure with short prose that names the
  object through a Quarto cross-reference and states the takeaway.

## Quarto HTML Defaults

- Use Quarto HTML with `toc: true`, `toc-title: "Contents"`, `toc-depth: 3`,
  and `toc-location: right` so readers get a floating table of contents.
- Use `number-sections: true` and `number-depth: 3` for numbered sections and
  subsections.
- Use paired Bootstrap themes, `light: cosmo` and `dark: darkly`, with
  `respect-user-color-scheme: true` so reports have an automatic colour-mode
  toggle and can follow the reader's system preference.
- The shared stylesheet labels the colour-mode toggle and gives it a larger hit
  area.
- Use `embed-resources: true` so rendered reports are portable single-file HTML
  artifacts.
- Use `smooth-scroll: true`, `anchor-sections: true`, and
  `link-external-icon: true` for reader-friendly navigation and links.
- Use `df-print: paged` for large data-frame displays.
- Include `css: report-style.css`; scaffolds created by `create_report()` copy
  this shared stylesheet next to `report.qmd`.

## Numbering And References

- Tables use executable chunks labelled `tbl-*`, a `#| tbl-cap:` option, and
  prose references through `@tbl-*`.
- Figures use executable chunks labelled `fig-*`, a `#| fig-cap:` option, and
  prose references through `@fig-*`.
- Do not make prose depend on a hand-written rendered number or on the visual
  position of the object. The source should remain correct if tables or figures
  are moved, inserted, or removed.

## Tables

- Use `reader_table()` or another `gt`-based table for reader-facing tables.
- Put the table title in `#| tbl-cap: "*Short descriptive title*"`.
- Put notes inside the table with `note =`.
- Format p-values, confidence intervals, percentages, and counts consistently.

## Figures

- Use `#| fig-cap-location: top`.
- Do not set ggplot `title`, `subtitle`, or `caption` for reader-facing figures.
- Put figure notes in an adjacent margin aside immediately after the figure
  chunk.
- Keep plot titles and numbering in Quarto, not inside the image.

## Method And Package Attribution

- Cite CRAN packages in the normal way for the report's discipline.
- Any code, package, method implementation, or helper that is not generally
  available on CRAN needs a linked source attribution in the report or method
  note.
- When using ScaSim, cite Titus von der Malsburg's `scanpath` R package and link
  to the repository: <https://github.com/tmalsburg/scanpath>. The repository
  describes the package as an R package for analysing scanpaths and points to
  von der Malsburg & Vasishth (2011) for the method.
- When using report plot themes based on `jtools::theme_apa()`, cite Jacob A.
  Long's `jtools` package and link to the repository:
  <https://github.com/jacob-long/jtools>.

## Avoid

- `kable()` for reader-facing output.
- Baked-in ggplot `title`, `subtitle`, or `caption`.
- Hand-maintained object numbering or position-dependent object references in
  prose.
- Uncached expensive model chunks.
- Display chunks that recompute expensive objects.
- Inline-code styling in headings.

Keep headings plain text. Do not put inline code spans in headings; they render
as oversized pills in dark mode. Letter appendices manually and leave them
unnumbered, for example `## Appendix A. Metric Glossary {.unnumbered}`.
