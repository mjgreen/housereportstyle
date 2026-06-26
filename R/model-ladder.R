compute_cluster_group_screen <- function(
    assignments,
    group_col,
    group_levels,
    analysis_dir,
    prefix,
    palette = default_condition_palette()) {
  analysis_groups <- assignments |>
    dplyr::mutate(
      analysis_group = factor(as.character(.data[[group_col]]), levels = group_levels),
      cluster_id = factor(
        as.character(.data$cluster_id),
        levels = as.character(sort(unique(as.integer(as.character(.data$cluster_id)))))
      )
    )

  cluster_group_counts <- analysis_groups |>
    dplyr::count(.data$cluster_id, .data$analysis_group, name = "scanpaths") |>
    tidyr::complete(
      cluster_id,
      analysis_group = factor(group_levels, levels = group_levels),
      fill = list(scanpaths = 0)
    ) |>
    dplyr::group_by(.data$analysis_group) |>
    dplyr::mutate(within_group_share = .data$scanpaths / sum(.data$scanpaths)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$cluster_id) |>
    dplyr::mutate(within_cluster_share = .data$scanpaths / sum(.data$scanpaths)) |>
    dplyr::ungroup() |>
    dplyr::mutate(cluster_label = cluster_label(.data$cluster_id))

  cluster_group_display <- cluster_group_counts |>
    dplyr::mutate(cell = paste0(.data$scanpaths, " (", apa_percent(.data$within_group_share), ")")) |>
    dplyr::select(.data$analysis_group, .data$cluster_label, .data$cell) |>
    tidyr::pivot_wider(names_from = .data$cluster_label, values_from = .data$cell) |>
    dplyr::arrange(.data$analysis_group) |>
    dplyr::rename(Group = .data$analysis_group)

  table_wide <- cluster_group_counts |>
    dplyr::select(.data$analysis_group, .data$cluster_id, .data$scanpaths) |>
    tidyr::pivot_wider(names_from = .data$cluster_id, values_from = .data$scanpaths, values_fill = 0)
  table_matrix <- as.matrix(table_wide[, setdiff(names(table_wide), "analysis_group"), drop = FALSE])
  rownames(table_matrix) <- as.character(table_wide$analysis_group)

  chisq_available <- nrow(table_matrix) > 1 && ncol(table_matrix) > 1
  if (chisq_available) {
    pooled_chisq <- suppressWarnings(stats::chisq.test(table_matrix, correct = FALSE))
    residuals <- as.data.frame(as.table(pooled_chisq$stdres), stringsAsFactors = FALSE) |>
      tibble::as_tibble() |>
      dplyr::rename(analysis_group = .data$Var1, cluster_id = .data$Var2, standardized_residual = .data$Freq)

    cramer_result <- tryCatch(
      effectsize::cramers_v(table_matrix, adjust = FALSE) |> as.data.frame(),
      error = function(error) tibble::tibble(Cramers_v = NA_real_, CI_low = NA_real_, CI_high = NA_real_)
    )

    pooled_summary <- tibble::tibble(
      test = "Pearson chi-square",
      statistic = unname(pooled_chisq$statistic),
      df = unname(pooled_chisq$parameter),
      n = sum(table_matrix),
      p_value = pooled_chisq$p.value,
      cramers_v = cramer_result$Cramers_v[[1]],
      cramers_v_low = cramer_result$CI_low[[1]],
      cramers_v_high = cramer_result$CI_high[[1]]
    ) |>
      dplyr::mutate(
        apa_summary = chisq_apa_string(
          .data$statistic,
          .data$df,
          .data$n,
          .data$p_value,
          .data$cramers_v,
          .data$cramers_v_low,
          .data$cramers_v_high
        )
      )
  } else {
    residuals <- cluster_group_counts |>
      dplyr::transmute(
        analysis_group = .data$analysis_group,
        cluster_id = .data$cluster_id,
        standardized_residual = NA_real_
      )
    pooled_summary <- tibble::tibble(
      test = "Pearson chi-square",
      statistic = NA_real_,
      df = NA_real_,
      n = sum(table_matrix),
      p_value = NA_real_,
      cramers_v = NA_real_,
      cramers_v_low = NA_real_,
      cramers_v_high = NA_real_,
      apa_summary = "The pooled chi-square screen was not available because fewer than two clusters or fewer than two groups were present."
    )
  }

  residuals <- residuals |>
    dplyr::mutate(
      analysis_group = factor(as.character(.data$analysis_group), levels = group_levels),
      cluster_id = factor(as.character(.data$cluster_id), levels = levels(cluster_group_counts$cluster_id)),
      cluster_label = cluster_label(.data$cluster_id)
    ) |>
    dplyr::left_join(
      cluster_group_counts |>
        dplyr::select(.data$cluster_id, .data$analysis_group, observed = .data$scanpaths),
      by = c("cluster_id", "analysis_group")
    )

  residual_display <- residuals |>
    dplyr::mutate(standardized_residual = apa_number(.data$standardized_residual, 2)) |>
    dplyr::select(.data$cluster_label, .data$analysis_group, .data$standardized_residual) |>
    tidyr::pivot_wider(names_from = .data$analysis_group, values_from = .data$standardized_residual) |>
    dplyr::arrange(.data$cluster_label)

  overrepresentation <- residuals |>
    dplyr::group_by(.data$cluster_id, .data$cluster_label) |>
    dplyr::slice_max(.data$standardized_residual, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      overrepresentation = dplyr::if_else(
        is.finite(.data$standardized_residual) & .data$standardized_residual > 1.96,
        as.character(.data$analysis_group),
        "No clear over-representation"
      ),
      strip_encoding = dplyr::if_else(
        .data$overrepresentation == "No clear over-representation",
        "white strip",
        "condition-coloured strip"
      ),
      rule = "Cluster strip is coloured when the largest positive chi-square standardized residual is greater than 1.96; strip alpha reflects residual strength."
    ) |>
    dplyr::select(
      .data$cluster_id,
      .data$cluster_label,
      .data$overrepresentation,
      largest_standardized_residual = .data$standardized_residual,
      .data$rule,
      .data$strip_encoding
    )

  readr::write_csv(cluster_group_counts, file.path(analysis_dir, paste0(prefix, "_cluster_group_counts.csv")))
  readr::write_csv(residuals, file.path(analysis_dir, paste0(prefix, "_chisq_standardized_residuals.csv")))
  readr::write_csv(pooled_summary, file.path(analysis_dir, paste0(prefix, "_chisq_summary.csv")))
  readr::write_csv(overrepresentation, file.path(analysis_dir, paste0(prefix, "_chisq_overrepresentation.csv")))

  list(
    counts = cluster_group_counts,
    display = cluster_group_display,
    residuals = residuals,
    residual_display = residual_display,
    summary = pooled_summary,
    overrepresentation = overrepresentation,
    palette = palette,
    group_levels = group_levels
  )
}

capture_glm <- function(formula, data) {
  captured_warnings <- character()
  captured_messages <- character()
  fit <- withCallingHandlers(
    tryCatch(
      stats::glm(formula, data = data, family = stats::binomial),
      error = function(error) error
    ),
    warning = function(warning) {
      captured_warnings <<- c(captured_warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    },
    message = function(message) {
      captured_messages <<- c(captured_messages, conditionMessage(message))
      invokeRestart("muffleMessage")
    }
  )
  list(fit = fit, warnings = captured_warnings, messages = captured_messages)
}

capture_glmer <- function(formula, data) {
  captured_warnings <- character()
  captured_messages <- character()
  fit <- withCallingHandlers(
    tryCatch(
      lme4::glmer(
        formula,
        data = data,
        family = stats::binomial,
        control = lme4::glmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 2e5)
        )
      ),
      error = function(error) error
    ),
    warning = function(warning) {
      captured_warnings <<- c(captured_warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    },
    message = function(message) {
      captured_messages <<- c(captured_messages, conditionMessage(message))
      invokeRestart("muffleMessage")
    }
  )
  list(fit = fit, warnings = captured_warnings, messages = captured_messages)
}

extract_lrt <- function(null_fit, full_fit) {
  lrt <- stats::anova(null_fit, full_fit, test = "Chisq") |> as.data.frame()
  chisq_col <- intersect(c("Chisq", "Deviance"), names(lrt))[[1]]
  p_col <- names(lrt)[stringr::str_detect(names(lrt), "Pr")][[1]]
  tibble::tibble(
    chisq = lrt[[chisq_col]][[2]],
    df = lrt$Df[[2]],
    p_value = lrt[[p_col]][[2]]
  )
}

fit_one_vs_rest_models <- function(
    assignments,
    group_col,
    group_levels,
    analysis_dir,
    prefix,
    mixed = FALSE) {
  model_data_base <- assignments |>
    dplyr::mutate(
      analysis_group = factor(as.character(.data[[group_col]]), levels = group_levels),
      participant_factor = factor(.data$participant),
      cluster_id = factor(
        as.character(.data$cluster_id),
        levels = as.character(sort(unique(as.integer(as.character(.data$cluster_id)))))
      )
    )

  clusters <- levels(model_data_base$cluster_id)

  model_results <- purrr::map_dfr(clusters, function(cluster_value) {
    model_data <- model_data_base |>
      dplyr::mutate(cluster_is_target = as.integer(.data$cluster_id == cluster_value))

    response_check <- tibble::tibble(
      cluster_id = cluster_value,
      cluster_label = cluster_label(cluster_value),
      target_scanpaths = sum(model_data$cluster_is_target == 1),
      other_scanpaths = sum(model_data$cluster_is_target == 0),
      has_both_response_values = dplyr::n_distinct(model_data$cluster_is_target) == 2,
      group_levels = paste(group_levels, collapse = ", ")
    )

    if (!response_check$has_both_response_values || dplyr::n_distinct(model_data$analysis_group) < 2) {
      return(response_check |>
        dplyr::mutate(
          null_aic = NA_real_, full_aic = NA_real_,
          null_bic = NA_real_, full_bic = NA_real_,
          chisq = NA_real_, df = NA_real_, p_value = NA_real_,
          full_singular = NA, null_singular = NA,
          full_warnings = "", null_warnings = "",
          model_messages = "model not fit",
          status = "not applicable"
        ))
    }

    if (mixed) {
      full_result <- capture_glmer(cluster_is_target ~ analysis_group + (1 | participant_factor), model_data)
      null_result <- capture_glmer(cluster_is_target ~ 1 + (1 | participant_factor), model_data)
      expected_class <- "merMod"
    } else {
      full_result <- capture_glm(cluster_is_target ~ analysis_group, model_data)
      null_result <- capture_glm(cluster_is_target ~ 1, model_data)
      expected_class <- "glm"
    }

    if (!inherits(full_result$fit, expected_class) || !inherits(null_result$fit, expected_class)) {
      full_error <- if (inherits(full_result$fit, "error")) conditionMessage(full_result$fit) else ""
      null_error <- if (inherits(null_result$fit, "error")) conditionMessage(null_result$fit) else ""
      return(response_check |>
        dplyr::mutate(
          null_aic = NA_real_, full_aic = NA_real_,
          null_bic = NA_real_, full_bic = NA_real_,
          chisq = NA_real_, df = NA_real_, p_value = NA_real_,
          full_singular = NA, null_singular = NA,
          full_warnings = paste(full_result$warnings, collapse = " | "),
          null_warnings = paste(null_result$warnings, collapse = " | "),
          model_messages = paste(c(full_error, null_error), collapse = " | "),
          status = "model fit failed"
        ))
    }

    lrt <- extract_lrt(null_result$fit, full_result$fit)

    response_check |>
      dplyr::mutate(
        null_aic = stats::AIC(null_result$fit),
        full_aic = stats::AIC(full_result$fit),
        null_bic = stats::BIC(null_result$fit),
        full_bic = stats::BIC(full_result$fit),
        chisq = lrt$chisq[[1]],
        df = lrt$df[[1]],
        p_value = lrt$p_value[[1]],
        full_singular = if (mixed) lme4::isSingular(full_result$fit) else NA,
        null_singular = if (mixed) lme4::isSingular(null_result$fit) else NA,
        full_warnings = paste(full_result$warnings, collapse = " | "),
        null_warnings = paste(null_result$warnings, collapse = " | "),
        model_messages = paste(c(full_result$messages, null_result$messages), collapse = " | "),
        status = "ok"
      )
  }) |>
    dplyr::mutate(
      p_adjust_bh = stats::p.adjust(.data$p_value, method = "BH"),
      aic_delta_full_minus_null = .data$full_aic - .data$null_aic,
      bic_delta_full_minus_null = .data$full_bic - .data$null_bic,
      screen = dplyr::if_else(mixed, "participant-adjusted GLMM", "non-mixed logistic GLM")
    ) |>
    dplyr::arrange(.data$p_value, .data$cluster_id)

  readr::write_csv(
    model_results,
    file.path(analysis_dir, paste0(prefix, "_one_vs_rest_", if (mixed) "glmm" else "glm", ".csv"))
  )
  model_results
}

build_participant_bridge <- function(assignments, group_col, group_levels, chisq_screen, analysis_dir, prefix) {
  target_cluster <- chisq_screen$residuals |>
    dplyr::filter(is.finite(.data$standardized_residual)) |>
    dplyr::arrange(dplyr::desc(abs(.data$standardized_residual))) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(.data$cluster_id) |>
    as.character()

  if (length(target_cluster) == 0) {
    target_cluster <- assignments |>
      dplyr::count(.data$cluster_id, sort = TRUE) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(.data$cluster_id) |>
      as.character()
  }

  participant_cluster_composition <- assignments |>
    dplyr::mutate(
      analysis_group = factor(as.character(.data[[group_col]]), levels = group_levels),
      cluster_id = factor(
        as.character(.data$cluster_id),
        levels = as.character(sort(unique(as.integer(as.character(.data$cluster_id)))))
      ),
      cluster_label = cluster_label(.data$cluster_id)
    ) |>
    dplyr::count(.data$participant, .data$analysis_group, .data$cluster_id, .data$cluster_label, name = "scanpaths") |>
    dplyr::group_by(.data$participant, .data$analysis_group) |>
    dplyr::mutate(
      participant_scanpaths = sum(.data$scanpaths),
      participant_cluster_proportion = .data$scanpaths / .data$participant_scanpaths
    ) |>
    dplyr::ungroup()

  participant_summaries <- participant_cluster_composition |>
    dplyr::group_by(.data$participant, .data$analysis_group) |>
    dplyr::summarise(
      target_cluster = cluster_label(target_cluster),
      target_cluster_proportion = sum(
        .data$participant_cluster_proportion[as.character(.data$cluster_id) == target_cluster],
        na.rm = TRUE
      ),
      dominant_cluster_proportion = max(.data$participant_cluster_proportion, na.rm = TRUE),
      cluster_diversity = dplyr::n_distinct(.data$cluster_id),
      participant_scanpaths = dplyr::first(.data$participant_scanpaths),
      .groups = "drop"
    )

  run_bridge_test <- function(data, outcome, outcome_label) {
    model_data <- data |>
      dplyr::filter(is.finite(.data[[outcome]]))

    if (dplyr::n_distinct(model_data$analysis_group) < 2) {
      return(tibble::tibble(
        outcome = outcome,
        outcome_label = outcome_label,
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p_value = NA_real_,
        eta_squared = NA_real_,
        eta_squared_low = NA_real_,
        eta_squared_high = NA_real_
      ))
    }

    formula <- stats::as.formula(paste(outcome, "~ analysis_group"))
    welch <- stats::oneway.test(formula, data = model_data)
    aov_fit <- stats::aov(formula, data = model_data)
    eta <- tryCatch(
      effectsize::eta_squared(aov_fit, partial = FALSE) |> as.data.frame(),
      error = function(error) tibble::tibble(Eta2 = NA_real_, CI_low = NA_real_, CI_high = NA_real_)
    )

    tibble::tibble(
      outcome = outcome,
      outcome_label = outcome_label,
      statistic = unname(welch$statistic),
      df1 = unname(welch$parameter[[1]]),
      df2 = unname(welch$parameter[[2]]),
      p_value = welch$p.value,
      eta_squared = eta$Eta2[[1]],
      eta_squared_low = eta$CI_low[[1]],
      eta_squared_high = eta$CI_high[[1]]
    )
  }

  bridge_tests <- dplyr::bind_rows(
    run_bridge_test(participant_summaries, "target_cluster_proportion", paste0("Proportion in ", cluster_label(target_cluster))),
    run_bridge_test(participant_summaries, "dominant_cluster_proportion", "Dominant-cluster proportion"),
    run_bridge_test(participant_summaries, "cluster_diversity", "Cluster diversity")
  )

  bridge_plot_data <- participant_summaries |>
    dplyr::select(
      .data$participant,
      .data$analysis_group,
      .data$target_cluster_proportion,
      .data$dominant_cluster_proportion,
      .data$cluster_diversity
    ) |>
    tidyr::pivot_longer(
      cols = c("target_cluster_proportion", "dominant_cluster_proportion", "cluster_diversity"),
      names_to = "outcome",
      values_to = "value"
    ) |>
    dplyr::left_join(bridge_tests |> dplyr::select(.data$outcome, .data$outcome_label), by = "outcome") |>
    dplyr::mutate(outcome_label = factor(.data$outcome_label, levels = unique(.data$outcome_label)))

  readr::write_csv(participant_cluster_composition, file.path(analysis_dir, paste0(prefix, "_participant_cluster_composition.csv")))
  readr::write_csv(participant_summaries, file.path(analysis_dir, paste0(prefix, "_anova_compatible_participant_summaries.csv")))
  readr::write_csv(bridge_tests, file.path(analysis_dir, paste0(prefix, "_anova_compatible_tests.csv")))

  list(
    target_cluster = target_cluster,
    composition = participant_cluster_composition,
    summaries = participant_summaries,
    tests = bridge_tests,
    plot_data = bridge_plot_data
  )
}

build_cluster_representatives <- function(assignments, metadata, distance_matrix, analysis_dir, prefix) {
  representatives <- assignments |>
    dplyr::mutate(
      cluster_id = factor(
        as.character(.data$cluster_id),
        levels = as.character(sort(unique(as.integer(as.character(.data$cluster_id)))))
      )
    ) |>
    dplyr::group_by(.data$cluster_id) |>
    dplyr::group_modify(function(data, key) {
      cluster_scanpath_ids <- intersect(as.character(data$scanpath_id), rownames(distance_matrix))
      representative_scanpath_id <- if (length(cluster_scanpath_ids) == 1) {
        cluster_scanpath_ids
      } else {
        scanpath::which.centroid(distance_matrix, select = cluster_scanpath_ids)
      }
      mean_scasim_distance <- if (length(cluster_scanpath_ids) == 1) {
        0
      } else {
        mean(distance_matrix[representative_scanpath_id, cluster_scanpath_ids], na.rm = TRUE)
      }
      tibble::tibble(
        representative_scanpath_id = representative_scanpath_id,
        representative_rule = "scanpath::which.centroid",
        mean_scasim_distance = mean_scasim_distance,
        cluster_size = nrow(data)
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      metadata,
      by = c("representative_scanpath_id" = "scanpath_id")
    ) |>
    dplyr::arrange(as.integer(as.character(.data$cluster_id)))

  readr::write_csv(representatives, file.path(analysis_dir, paste0(prefix, "_cluster_representatives.csv")))
  representatives
}

