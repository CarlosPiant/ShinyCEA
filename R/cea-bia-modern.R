#' Define Target Population for BIA
#'
#' Creates a structured population object with prevalence/incidence,
#' growth rates, and eligibility criteria for budget impact analysis.
#' Follows ISPOR BIA Good Practices (Sullivan et al., 2014).
#'
#' @param total_population Total population base (e.g., health plan enrollment).
#' @param prevalence Disease prevalence (proportion, 0 to 1).
#' @param eligible_fraction Fraction of prevalent cases eligible for treatment
#'   (e.g., diagnosed, insured, meeting criteria). Default 1.
#' @param incident_annual Annual incident cases (absolute number). If provided,
#'   added to prevalent pool each year.
#' @param annual_growth Annual population growth rate (e.g., 0.02 for 2%). Default 0.
#' @param mortality_rate Annual disease-specific mortality rate for prevalent
#'   cases (reduces the pool each year). Default 0.
#' @param years Number of projection years. Default 5.
#' @return A `bia_population` list with `$eligible` (vector of eligible patients
#'   per year) and all input parameters.
#' @export
#' @examples
#' pop <- define_population(
#'   total_population = 1000000,
#'   prevalence = 0.005,
#'   eligible_fraction = 0.80,
#'   incident_annual = 200,
#'   annual_growth = 0.01,
#'   mortality_rate = 0.05,
#'   years = 5
#' )
#' pop$eligible
define_population <- function(total_population, prevalence,
                               eligible_fraction = 1,
                               incident_annual = 0,
                               annual_growth = 0,
                               mortality_rate = 0,
                               years = 5L) {
  prevalent <- total_population * prevalence
  eligible <- numeric(years)

  pool <- prevalent
  for (yr in seq_len(years)) {
    eligible[yr] <- round(pool * eligible_fraction)
    # Update pool: growth + incidence - mortality
    pool <- pool * (1 + annual_growth) + incident_annual - pool * mortality_rate
    pool <- max(pool, 0)
  }

  structure(
    list(
      eligible = eligible,
      total_population = total_population,
      prevalence = prevalence,
      eligible_fraction = eligible_fraction,
      incident_annual = incident_annual,
      annual_growth = annual_growth,
      mortality_rate = mortality_rate,
      years = years
    ),
    class = "bia_population"
  )
}

#' @export
print.bia_population <- function(x, ...) {
  cat("BIA Population:\n")
  cat("  Base:", x$total_population, "x", x$prevalence, "prevalence =",
      round(x$total_population * x$prevalence), "prevalent\n")
  cat("  Eligible fraction:", x$eligible_fraction, "\n")
  cat("  Eligible by year:", paste(x$eligible, collapse = ", "), "\n")
  invisible(x)
}

#' Define Market Shares for BIA
#'
#' Creates a structured market share object for current and new scenarios.
#'
#' @param current Named numeric vector of current market shares by strategy
#'   (must sum to 1).
#' @param new_target Named numeric vector of target market shares under the
#'   new scenario (must sum to 1).
#' @return A `bia_shares` list.
#' @export
#' @examples
#' shares <- define_market_shares(
#'   current = c(std = 0.60, trt_A = 0.30, trt_B = 0.10),
#'   new_target = c(std = 0.30, trt_A = 0.20, trt_B = 0.10, new_drug = 0.40)
#' )
define_market_shares <- function(current, new_target) {
  if (abs(sum(current) - 1) > 0.01) {
    stop("Current shares must sum to 1 (got ", round(sum(current), 4), ").", call. = FALSE)
  }
  if (abs(sum(new_target) - 1) > 0.01) {
    stop("New target shares must sum to 1 (got ", round(sum(new_target), 4), ").", call. = FALSE)
  }

  structure(
    list(current = current, new_target = new_target),
    class = "bia_shares"
  )
}

#' Define Uptake Curve for BIA
#'
#' Creates a time-varying market share trajectory from current to target
#' over the projection period. Supports linear, S-curve (logistic), and
#' custom trajectories.
#'
#' @param shares A `bia_shares` object from [define_market_shares()].
#' @param years Number of projection years.
#' @param shape One of "linear" (default), "s_curve" (logistic), or "immediate".
#' @param midpoint For s_curve: year at which uptake reaches 50% of target.
#'   Default is years/2.
#' @param steepness For s_curve: steepness parameter (higher = steeper). Default 2.
#' @return A list of length `years`, each element a named numeric vector of
#'   shares for the new scenario.
#' @export
#' @examples
#' shares <- define_market_shares(
#'   current = c(std = 1),
#'   new_target = c(std = 0.4, new = 0.6)
#' )
#' uptake <- define_uptake_curve(shares, years = 5, shape = "s_curve")
define_uptake_curve <- function(shares, years = 5L,
                                 shape = c("linear", "s_curve", "immediate"),
                                 midpoint = NULL, steepness = 2) {
  shape <- match.arg(shape)
  from <- shares$current
  to <- shares$new_target

  # Ensure all strategies appear in both

  all_strats <- union(names(from), names(to))
  from_full <- stats::setNames(rep(0, length(all_strats)), all_strats)
  to_full <- from_full
  from_full[names(from)] <- from
  to_full[names(to)] <- to

  if (is.null(midpoint)) midpoint <- years / 2

  lapply(seq_len(years), function(yr) {
    frac <- switch(shape,
      "linear" = (yr - 1) / max(years - 1, 1),
      # Logistic rescaled so year 1 is the current mix and the last year
      # reaches the target (review finding D11).
      "s_curve" = {
        f <- function(y) 1 / (1 + exp(-steepness * (y - midpoint)))
        if (years > 1) (f(yr) - f(1)) / (f(years) - f(1)) else 1
      },
      "immediate" = 1
    )
    mix <- from_full + frac * (to_full - from_full)
    # Renormalize to ensure sum = 1
    mix / sum(mix)
  })
}

#' Define Cost Components for BIA
#'
#' Creates a structured cost breakdown per strategy, separating drug
#' acquisition, administration, monitoring, adverse events, and other
#' costs. This enables detailed budget impact decomposition.
#'
#' @param ... Named lists, one per strategy. Each list should contain
#'   named cost components (e.g., drug = 5000, admin = 300, monitoring = 200,
#'   ae = 500, other = 100). The total per-patient cost is the sum.
#' @return A `bia_costs` list with per-strategy cost breakdowns.
#' @export
#' @examples
#' costs <- define_cost_components(
#'   std = list(drug = 500, admin = 200, ae = 100),
#'   new_drug = list(drug = 5000, admin = 300, monitoring = 333, ae = 50)
#' )
define_cost_components <- function(...) {
  components <- list(...)
  if (is.null(names(components)) || any(names(components) == "")) {
    stop("Each argument must be named by strategy.", call. = FALSE)
  }

  # Compute totals
  totals <- vapply(components, function(x) sum(unlist(x)), numeric(1))

  structure(
    list(components = components, totals = totals),
    class = "bia_costs"
  )
}

#' @export
print.bia_costs <- function(x, ...) {
  cat("BIA Cost Components:\n")
  for (nm in names(x$components)) {
    comp <- x$components[[nm]]
    cat("  ", nm, ": $", formatC(sum(unlist(comp)), big.mark = ","),
        "/patient (", paste(names(comp), "=", unlist(comp), collapse = ", "), ")\n")
  }
  invisible(x)
}

#' Run Modern Budget Impact Analysis
#'
#' Full BIA pipeline using structured population, market shares, uptake
#' curves, and cost components. Produces annual budget projections with
#' cost decomposition.
#'
#' @param population A `bia_population` object from [define_population()].
#' @param shares A `bia_shares` object from [define_market_shares()].
#' @param costs A `bia_costs` object from [define_cost_components()], OR
#'   NULL to use per-patient costs from a CEA model.
#' @param uptake A list of yearly share vectors from [define_uptake_curve()],
#'   or NULL for linear uptake.
#' @param model Optional `cea_model` object. If provided (and costs is NULL),
#'   per-patient costs are extracted from the model's simulate() output.
#' @param params_df Parameter data.frame (required if model is provided).
#' @param horizon_years Model horizon in years, used to annualise the model's
#'   lifetime cost per patient (see [run_bia()]); resolved from the model or
#'   the `time_horizon` parameter when `NULL`.
#' @return A list with:
#'   \describe{
#'     \item{annual}{Data.frame with Year, Scenario, Strategy, Patients,
#'       Per_Patient_Cost, Total_Cost}
#'     \item{impact}{Data.frame with Year, Cost_Current, Cost_New, Budget_Impact}
#'     \item{by_component}{Data.frame with Year, Scenario, Strategy, Component,
#'       Cost (if cost components provided)}
#'     \item{total_impact}{Scalar total budget impact}
#'     \item{population}{The population object used}
#'   }
#' @export
run_bia_modern <- function(population, shares, costs = NULL, uptake = NULL,
                            model = NULL, params_df = NULL, horizon_years = NULL) {
  years <- population$years

  # Build uptake trajectory
  if (is.null(uptake)) {
    uptake <- define_uptake_curve(shares, years = years, shape = "linear")
  }
  if (length(uptake) < years) {
    last <- uptake[[length(uptake)]]
    while (length(uptake) < years) uptake <- c(uptake, list(last))
  }

  # Get per-patient costs
  if (!is.null(costs) && inherits(costs, "bia_costs")) {
    cost_per_patient <- costs$totals
    cost_components <- costs$components
  } else if (!is.null(model)) {
    pars <- stats::setNames(as.list(params_df$base), params_df$name)
    strats <- model$info()$strategies
    # Annual cost per patient = lifetime model cost / horizon (finding D1).
    horizon_years <- .bia_horizon(model, params_df, horizon_years)
    cost_per_patient <- vapply(strats, function(s) {
      res <- safe_simulate(model, pars, s)
      if (is.null(res)) NA_real_ else res$cost / horizon_years
    }, numeric(1))
    names(cost_per_patient) <- strats
    cost_components <- NULL
  } else {
    stop("Provide either 'costs' (bia_costs) or 'model' + 'params_df'.", call. = FALSE)
  }

  current_shares <- shares$current
  all_strats <- union(names(current_shares), names(uptake[[1]]))

  # Fill missing strategies with 0 cost
  for (s in all_strats) {
    if (!s %in% names(cost_per_patient)) cost_per_patient[s] <- 0
  }

  annual_rows <- list()
  comp_rows <- list()
  impact_rows <- list()

  for (yr in seq_len(years)) {
    n_eligible <- population$eligible[yr]
    new_shares <- uptake[[yr]]

    cost_current <- 0
    cost_new <- 0

    for (s in all_strats) {
      sh_c <- if (s %in% names(current_shares)) current_shares[[s]] else 0
      sh_n <- if (s %in% names(new_shares)) new_shares[[s]] else 0
      cpp <- cost_per_patient[[s]]

      pat_c <- round(n_eligible * sh_c)
      pat_n <- round(n_eligible * sh_n)
      tc_c <- pat_c * cpp
      tc_n <- pat_n * cpp

      annual_rows[[length(annual_rows) + 1L]] <- data.frame(
        Year = yr, Scenario = "Current", Strategy = s,
        Patients = pat_c, Per_Patient_Cost = cpp, Total_Cost = tc_c,
        stringsAsFactors = FALSE
      )
      annual_rows[[length(annual_rows) + 1L]] <- data.frame(
        Year = yr, Scenario = "New", Strategy = s,
        Patients = pat_n, Per_Patient_Cost = cpp, Total_Cost = tc_n,
        stringsAsFactors = FALSE
      )

      cost_current <- cost_current + tc_c
      cost_new <- cost_new + tc_n

      # Cost components (if available)
      if (!is.null(cost_components) && s %in% names(cost_components)) {
        for (comp_name in names(cost_components[[s]])) {
          comp_cost <- cost_components[[s]][[comp_name]]
          comp_rows[[length(comp_rows) + 1L]] <- data.frame(
            Year = yr, Scenario = "Current", Strategy = s,
            Component = comp_name, Cost = pat_c * comp_cost,
            stringsAsFactors = FALSE
          )
          comp_rows[[length(comp_rows) + 1L]] <- data.frame(
            Year = yr, Scenario = "New", Strategy = s,
            Component = comp_name, Cost = pat_n * comp_cost,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    impact_rows[[yr]] <- data.frame(
      Year = yr, Cost_Current = cost_current, Cost_New = cost_new,
      Budget_Impact = cost_new - cost_current,
      stringsAsFactors = FALSE
    )
  }

  annual <- do.call(rbind, annual_rows)
  impact <- do.call(rbind, impact_rows)
  by_component <- if (length(comp_rows) > 0) do.call(rbind, comp_rows) else NULL

  list(
    annual = annual,
    impact = impact,
    by_component = by_component,
    total_impact = sum(impact$Budget_Impact),
    population = population
  )
}

#' Run BIA Scenario Comparison
#'
#' Runs multiple BIA scenarios and compares their budget impacts.
#' Useful for testing sensitivity to population size, uptake speed,
#' pricing, or market share assumptions.
#'
#' @param scenarios A named list of BIA configurations. Each element is a
#'   list with `population`, `shares`, `costs` (or `model` + `params_df`),
#'   and optionally `uptake`.
#' @return A data.frame with Scenario, Year, Budget_Impact, and
#'   Cumulative_Impact columns.
#' @export
scenario_bia <- function(scenarios) {
  if (!is.list(scenarios) || is.null(names(scenarios))) {
    stop("scenarios must be a named list.", call. = FALSE)
  }

  rows <- list()
  for (sc_name in names(scenarios)) {
    sc <- scenarios[[sc_name]]
    bia <- do.call(run_bia_modern, sc)

    for (i in seq_len(nrow(bia$impact))) {
      rows[[length(rows) + 1L]] <- data.frame(
        Scenario = sc_name,
        Year = bia$impact$Year[i],
        Cost_Current = bia$impact$Cost_Current[i],
        Cost_New = bia$impact$Cost_New[i],
        Budget_Impact = bia$impact$Budget_Impact[i],
        stringsAsFactors = FALSE
      )
    }
  }

  df <- do.call(rbind, rows)

  # Add cumulative impact per scenario
  df$Cumulative_Impact <- stats::ave(df$Budget_Impact, df$Scenario, FUN = cumsum)
  df
}

#' Plot Budget Impact with Cost Decomposition
#'
#' Enhanced BIA plot showing annual budget impact with optional cost
#' component breakdown.
#'
#' @param bia_result Output from [run_bia_modern()].
#' @param show_components Logical; show cost component breakdown? Only
#'   available when cost components were provided via [define_cost_components()].
#' @param palette Optional palette from [cea_palette()].
#' @return A ggplot object.
#' @export
plot_budget_impact <- function(bia_result, show_components = FALSE,
                                palette = NULL) {
  if (is.null(palette)) palette <- cea_palette("cream")

  if (show_components && !is.null(bia_result$by_component)) {
    # Stacked bar by component
    df <- bia_result$by_component

    # Compute incremental by component
    comp_current <- df[df$Scenario == "Current", ]
    comp_new <- df[df$Scenario == "New", ]

    merged <- merge(
      stats::aggregate(Cost ~ Year + Component, data = comp_new, FUN = sum),
      stats::aggregate(Cost ~ Year + Component, data = comp_current, FUN = sum),
      by = c("Year", "Component"), suffixes = c("_New", "_Current")
    )
    merged$Impact <- merged$Cost_New - merged$Cost_Current

    p <- ggplot2::ggplot(merged, ggplot2::aes(x = factor(.data$Year),
                                               y = .data$Impact,
                                               fill = .data$Component)) +
      ggplot2::geom_col(width = 0.6, alpha = 0.85) +
      scale_fill_cea(palette) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "#999999") +
      ggplot2::labs(x = "Year", y = "Incremental Budget Impact ($)",
                    title = "Budget Impact by Cost Component",
                    fill = "Component") +
      theme_cea() +
      ggplot2::theme(legend.position = "bottom")
  } else {
    # Standard waterfall
    impact <- bia_result$impact
    impact$color <- ifelse(impact$Budget_Impact >= 0, "Additional cost", "Savings")

    p <- ggplot2::ggplot(impact, ggplot2::aes(x = factor(.data$Year),
                                               y = .data$Budget_Impact,
                                               fill = .data$color)) +
      ggplot2::geom_col(width = 0.5, alpha = 0.85) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "#999999") +
      ggplot2::scale_fill_manual(values = c("Additional cost" = palette$dominated,
                                             "Savings" = palette$primary)) +
      ggplot2::labs(x = "Year", y = "Budget Impact ($)",
                    title = "Annual Budget Impact", fill = NULL) +
      theme_cea() +
      ggplot2::theme(legend.position = "bottom",
                     panel.grid.major.x = ggplot2::element_blank())
  }

  p
}

#' Generate Annual Budget Table
#'
#' Creates a formatted annual budget table suitable for reports and
#' HTA submissions. Shows current costs, new costs, incremental impact,
#' and cumulative impact by year.
#'
#' @param bia_result Output from [run_bia_modern()].
#' @return A data.frame with Year, Eligible_Patients, Cost_Current,
#'   Cost_New, Annual_Impact, Cumulative_Impact columns.
#' @export
table_annual_budget <- function(bia_result) {
  impact <- bia_result$impact
  pop <- bia_result$population

  df <- data.frame(
    Year = impact$Year,
    Eligible_Patients = pop$eligible[seq_len(nrow(impact))],
    Cost_Current = impact$Cost_Current,
    Cost_New = impact$Cost_New,
    Annual_Impact = impact$Budget_Impact,
    Cumulative_Impact = cumsum(impact$Budget_Impact),
    stringsAsFactors = FALSE
  )

  df
}
