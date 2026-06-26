# Quarto Chunk Rules

Use stable chunk names.

- Data preparation: `*_model`
- Expensive models: `*_model`, `*_dimension_sweep_model`, `*_glmm_model`
- Tables: `tbl-*`
- Figures: `fig-*`
- Hidden checks: `internal_validation_checks`

Use caching deliberately.

- Expensive data/model chunks should include `#| cache: true`.
- Display chunks should usually not include expensive computation.
- Display chunks should include `#| dependson:` pointing to the model chunks they report.
- If helper code changes, use a cache refresh render before treating HTML as final.

Preferred table chunk shape:

````
```{r tbl-example}
#| tbl-cap: "*Short descriptive title*"
#| dependson: example_model
reader_table(example_table, note = "Explain any abbreviations or screening rules.")
```

@tbl-example explains the table's takeaway without depending on a hand-written
number.
````

Preferred figure chunk shape:

````
```{r fig-example}
#| fig-width: 8
#| fig-height: 5
#| out-width: "100%"
#| fig-cap: "*Short descriptive title*"
#| fig-cap-location: top
#| dependson: example_model
example_plot
```

::: {.aside}
_Note._ Explain how to read the figure.
:::
````
