# House Report Style

Read this before editing a Quarto report that uses the shared house style.

Core principles:

- The report should answer scientific questions, not advertise itself.
- Keep provenance visible before modelling.
- Separate expensive data/model chunks from display chunks.
- Use APA-compatible HTML conventions where practical.
- Use `gt` tables for reader-facing tables.
- Let Quarto own figure numbers and titles.
- Put figure notes in margin asides immediately after figure chunks.
- Add short prose after every table and figure explaining the takeaway.
- Keep hidden validation checks in the report even when they are not part of the narrative.

HTML house style:

- Use Quarto HTML with `toc: true`, `toc-title: "Contents"`, `toc-depth: 3`,
  and `toc-location: left` so readers get a floating table of contents.
- Use `number-sections: true` and `number-depth: 3` for numbered sections and
  subsections.
- Use paired Bootstrap themes, `light: cosmo` and `dark: darkly`, with
  `respect-user-color-scheme: true` so reports have an automatic dark-mode
  toggle and can follow the reader's system preference.
- Use `embed-resources: true` so rendered reports are portable single-file HTML
  artifacts.
- Use `smooth-scroll: true`, `anchor-sections: true`, and
  `link-external-icon: true` for reader-friendly navigation and links.
- Use `df-print: paged` for large data-frame displays.
- Include `css: report-style.css`; scaffolds created by `create_report()` copy
  this shared stylesheet next to `report.qmd`.
- Keep headings plain text. Do not put inline code spans such as `` `fg_06` ``
  in headings; they render as oversized pills in dark mode.
- Letter appendices manually and leave them unnumbered, for example
  `## Appendix A. Metric Glossary {.unnumbered}`.

Avoid:

- `kable()` for reader-facing output.
- Baked-in ggplot `title`, `subtitle`, or `caption`.
- Hard-coded prose such as `Figure 5`; use Quarto cross-references.
- Uncached expensive model chunks.
- Display chunks that recompute expensive objects.
- Inline-code styling in headings.
