#' Run Budget Impact Analysis
#'
#' Projects total budget impact over a time horizon given a target population,
#' market uptake curve, and per-patient costs from the CEA model. Follows
#' ISPOR BIA Good Practices (Sullivan et al. 2014, Mauskopf et al. 2017).
#'
#' @param model A `cea_model` object.
#' @param params_df Parameter data.frame.
#' @param population Total eligible population size.
#' @param current_mix Named numeric vector of current market shares by strategy
#'   (must sum to 1). Names must match strategy names in the model.
#' @param new_mix Named numeric vector of market shares under the new scenario
#'   (must sum to 1), OR a list of such vectors (one per year) for time-varying uptake.
#' @param years Number of years to project (default 5).
#' @param annual_growth Annual population growth rate (default 0).
#' @param horizon_years Length in years of the model's time horizon. The
#'   model's `simulate()` returns the (discounted) cost per patient over its
#'   whole horizon; the BIA needs a cost per patient per year, so the lifetime
#'   cost is divided by this horizon (an average annual cost). Taken from
#'   `model$info()$horizon` or the `time_horizon` parameter when `NULL`.
#'   (Review finding D1: the lifetime cost used to be charged every year.)
#' @return A list with:
#'   \describe{
#'     \item{annual}{Data.frame with Year, Strategy, Patients, Cost columns for both scenarios}
#'     \item{impact}{Data.frame with Year, Cost_Current, Cost_New, Budget_Impact}
#'     \item{total_impact}{Scalar total budget impact across all years}
#'   }
#' @export
run_bia <- function(model, params_df, population, current_mix, new_mix,
                    years = 5L, annual_growth = 0, horizon_years = NULL) {
  strategies <- model$info()$strategies
  pars <- stats::setNames(as.list(params_df$base), params_df$name)
  horizon_years <- .bia_horizon(model, params_df, horizon_years)

  # Validate mixes
  .validate_mix <- function(mix, label) {
    if (!is.numeric(mix) || is.null(names(mix))) {
      stop(label, " must be a named numeric vector.", call. = FALSE)
    }
    missing_s <- setdiff(names(mix), strategies)
    if (length(missing_s)) {
      stop(label, " contains unknown strategies: ",
           paste(missing_s, collapse = ", "), call. = FALSE)
    }
    if (abs(sum(mix) - 1) > 0.01) {
      stop(label, " must sum to 1 (got ", round(sum(mix), 4), ").", call. = FALSE)
    }
  }

  .validate_mix(current_mix, "current_mix")

  # new_mix can be a single vector or a list of vectors (one per year)
  if (is.numeric(new_mix) && !is.list(new_mix)) {
    .validate_mix(new_mix, "new_mix")
    new_mix_list <- replicate(years, new_mix, simplify = FALSE)
  } else if (is.list(new_mix)) {
    if (length(new_mix) < years) {
      # Pad with last year's mix
      last <- new_mix[[length(new_mix)]]
      while (length(new_mix) < years) new_mix <- c(new_mix, list(last))
    }
    for (i in seq_len(years)) .validate_mix(new_mix[[i]], paste0("new_mix[[", i, "]]"))
    new_mix_list <- new_mix[seq_len(years)]
  } else {
    stop("new_mix must be a named numeric vector or a list of them.", call. = FALSE)
  }

  # Per-patient cost per YEAR by strategy: lifetime model cost / horizon.
  per_patient_cost <- vapply(strategies, function(s) {
    res <- safe_simulate(model, pars, s)
    if (is.null(res)) NA_real_ else res$cost / horizon_years
  }, numeric(1))

  # Build annual tables
  annual_rows <- list()
  impact_rows <- list()

  for (yr in seq_len(years)) {
    pop_yr <- population * (1 + annual_growth)^(yr - 1)
    nm <- new_mix_list[[yr]]

    cost_current <- 0
    cost_new <- 0

    for (s in strategies) {
      share_c <- if (s %in% names(current_mix)) current_mix[[s]] else 0
      share_n <- if (s %in% names(nm)) nm[[s]] else 0
      patients_c <- pop_yr * share_c
      patients_n <- pop_yr * share_n
      c_c <- patients_c * per_patient_cost[[s]]
      c_n <- patients_n * per_patient_cost[[s]]

      annual_rows[[length(annual_rows) + 1L]] <- data.frame(
        Year = yr, Scenario = "Current", Strategy = s,
        Share = share_c, Patients = round(patients_c),
        Per_Patient_Cost = per_patient_cost[[s]],
        Total_Cost = c_c, stringsAsFactors = FALSE
      )
      annual_rows[[length(annual_rows) + 1L]] <- data.frame(
        Year = yr, Scenario = "New", Strategy = s,
        Share = share_n, Patients = round(patients_n),
        Per_Patient_Cost = per_patient_cost[[s]],
        Total_Cost = c_n, stringsAsFactors = FALSE
      )

      cost_current <- cost_current + c_c
      cost_new <- cost_new + c_n
    }

    impact_rows[[yr]] <- data.frame(
      Year = yr,
      Cost_Current = cost_current,
      Cost_New = cost_new,
      Budget_Impact = cost_new - cost_current,
      stringsAsFactors = FALSE
    )
  }

  annual <- do.call(rbind, annual_rows)
  impact <- do.call(rbind, impact_rows)

  list(
    annual = annual,
    impact = impact,
    total_impact = sum(impact$Budget_Impact),
    horizon_years = horizon_years,
    cost_basis = "Average annual cost per patient: lifetime (discounted) model cost / horizon years"
  )
}

# Resolve the horizon used to annualise the model's lifetime cost.
.bia_horizon <- function(model, params_df, horizon_years = NULL) {
  h <- horizon_years
  if (is.null(h)) h <- tryCatch(model$info()$horizon, error = function(e) NULL)
  if (is.null(h) && "time_horizon" %in% params_df$name) {
    h <- params_df$base[match("time_horizon", params_df$name)]
  }
  h <- suppressWarnings(as.numeric(h))
  if (length(h) != 1L || !is.finite(h) || h <= 0) {
    stop("Budget impact needs the model's time horizon in years to turn the lifetime ",
         "cost per patient into an annual cost: pass `horizon_years`, add a `time_horizon` ",
         "parameter, or set `horizon` in model$info().", call. = FALSE)
  }
  h
}

#' Generate Linear Uptake Mix
#'
#' Creates a list of yearly market share vectors that linearly transition
#' from a starting mix to a target mix over the specified years.
#'
#' @param from Named numeric vector of starting shares.
#' @param to Named numeric vector of target shares.
#' @param years Number of years for the transition.
#' @return A list of length `years`, each element a named numeric vector.
#' @export
linear_uptake <- function(from, to, years = 5L) {
  all_strats <- union(names(from), names(to))
  lapply(seq_len(years), function(yr) {
    frac <- (yr - 1) / max(years - 1, 1)
    mix <- vapply(all_strats, function(s) {
      f <- if (s %in% names(from)) from[[s]] else 0
      t <- if (s %in% names(to)) to[[s]] else 0
      f + frac * (t - f)
    }, numeric(1))
    names(mix) <- all_strats
    mix
  })
}

#' Plot Budget Impact Over Time
#'
#' @param impact_df Data.frame from run_bia()$impact with columns
#'   Year, Cost_Current, Cost_New, Budget_Impact.
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_bia <- function(impact_df, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("cream")

  df_long <- rbind(
    data.frame(Year = impact_df$Year, Scenario = "Current",
               Cost = impact_df$Cost_Current, stringsAsFactors = FALSE),
    data.frame(Year = impact_df$Year, Scenario = "New",
               Cost = impact_df$Cost_New, stringsAsFactors = FALSE)
  )

  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = .data$Year, y = .data$Cost,
                                              fill = .data$Scenario)) +
    ggplot2::geom_col(position = "dodge", width = 0.6, alpha = 0.85) +
    ggplot2::scale_fill_manual(values = c("Current" = palette$secondary,
                                           "New" = palette$primary)) +
    ggplot2::scale_y_continuous(labels = function(x) {
      ifelse(abs(x) >= 1e6,
             paste0("$", formatC(x / 1e6, format = "f", digits = 1), "M"),
             ifelse(abs(x) >= 1e3,
                    paste0("$", formatC(x / 1e3, format = "f", digits = 0), "k"),
                    paste0("$", formatC(x, format = "f", digits = 0))))
    }) +
    ggplot2::labs(x = "Year", y = "Total Cost", fill = "Scenario",
                  title = "Budget Impact Analysis: Current vs. New Scenario") +
    theme_cea() +
    ggplot2::theme(legend.position = "bottom")

  p
}

#' Plot Annual Budget Impact (Waterfall)
#'
#' @param impact_df Data.frame from run_bia()$impact.
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_bia_waterfall <- function(impact_df, palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("cream")

  impact_df$color <- ifelse(impact_df$Budget_Impact >= 0,
                            "Additional cost", "Savings")

  ggplot2::ggplot(impact_df, ggplot2::aes(x = factor(.data$Year),
                                           y = .data$Budget_Impact,
                                           fill = .data$color)) +
    ggplot2::geom_col(width = 0.5, alpha = 0.85) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "#999999") +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(
        abs(.data$Budget_Impact) >= 1e6,
        paste0("$", formatC(.data$Budget_Impact / 1e6, format = "f", digits = 2), "M"),
        paste0("$", formatC(.data$Budget_Impact / 1e3, format = "f", digits = 0), "k")
      )),
      vjust = ifelse(impact_df$Budget_Impact >= 0, -0.5, 1.5),
      size = 3.5, color = "#333333"
    ) +
    ggplot2::scale_fill_manual(values = c("Additional cost" = palette$dominated,
                                           "Savings" = palette$primary)) +
    ggplot2::labs(x = "Year", y = "Budget Impact ($)",
                  title = "Annual Incremental Budget Impact", fill = NULL) +
    theme_cea() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major.x = ggplot2::element_blank())
}
