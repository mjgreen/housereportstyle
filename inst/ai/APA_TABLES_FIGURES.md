# APA-Compatible Tables And Figures

Tables:

- Use `reader_table()` or another `gt`-based table.
- Label table chunks `tbl-*` and put the title in `#| tbl-cap:`.
- Use Quarto cross-references such as `@tbl-model-summary` in prose.
- Do not bake table numbers into `reader_table()` labels or prose.
- Include a note when needed.
- Format p-values, confidence intervals, percentages, and counts consistently.
- Follow every table with prose explaining what the reader should take from it.

Figures:

- Use Quarto chunk labels and `fig-cap` for title text.
- Use `#| fig-cap-location: top`.
- Use Quarto cross-references such as `@fig-model-summary` in prose.
- Do not set ggplot `title`, `subtitle`, or `caption` for reader-facing figures.
- Put the note in an adjacent margin aside.
- Follow every figure with prose explaining the takeaway.

HTML reports are not print APA documents. The goal is APA-compatible structure
where it works well in HTML, not rigid print-layout imitation.
