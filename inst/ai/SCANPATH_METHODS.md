# Scanpath Methods

Use the most recent validated scanpath report as the worked example, and keep
the report-specific preprocessing choices explicit in prose.

Recommended sequence:

1. Build participant-trial scanpaths with `build_scanpaths()`. By default it
   auto-detects row/column geometry from the full input event table before
   applying any analysis filter; pass an explicit `geometry = list(row_min = ...,
   row_max = ..., col_min = ..., col_max = ...)` when a deliberately filtered or
   sparse event table should still use the full task layout. Scanpath IDs use
   `participant`, `device`, and `trial` when those columns exist; pass
   `scanpath_id_cols` only when a report needs a different unit of analysis.
2. Remove consecutive duplicate locations when analysing transitions.
3. Retain later revisits.
4. Compute inter-visit durations.
5. Compute ScaSim distances with documented normalization and geometry settings.
   Cite Titus von der Malsburg's `scanpath` package
   (<https://github.com/tmalsburg/scanpath>) and the underlying
   von der Malsburg & Vasishth (2011) method when reporting this step. The
   scanpath sequence output should contain `geometry_*` columns and the manifest
   should record the detected row/column bounds.
6. Audit the distance matrix before modelling.
7. Fit lower-dimensional maps for 2-10 dimensions using `MASS::isoMDS`.
8. Fit `Mclust` at each dimensionality over the report-specified `G` range.
9. Plot stress and BIC-selected cluster count by dimensionality.
10. Select the final dimensionality using the stated rule.
11. Run the model ladder: pooled chi-square, non-mixed GLM, participant bridge plot, participant-adjusted GLMM.
12. Plot cluster geometry using neutral scanpath lines.
13. Use coloured facet strips, not thick panel borders, for over-representation.
14. Plot real centroid-representative scanpaths when useful.

The package provides helpers for these steps, but each report must still state
its scientific choices in prose.
