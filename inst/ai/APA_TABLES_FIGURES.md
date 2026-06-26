# APA-Compatible Tables And Figures

Tables:

- Use `reader_table()` or another `gt`-based table.
- Include a table number label, italic title, and note when needed.
- Format p-values, confidence intervals, percentages, and counts consistently.
- Follow every table with prose explaining what the reader should take from it.

Figures:

- Use Quarto chunk labels and `fig-cap` for title text.
- Use `#| fig-cap-location: top`.
- Do not set ggplot `title`, `subtitle`, or `caption` for reader-facing figures.
- Put the note in an adjacent margin aside.
- Follow every figure with prose explaining the takeaway.

HTML reports are not print APA documents. The goal is APA-compatible structure
where it works well in HTML, not rigid print-layout imitation.

