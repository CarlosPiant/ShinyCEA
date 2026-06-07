#' Define a Parameter with Rich Metadata
#'
#' Extended version of [cea_param()] that accepts standard errors, confidence
#' intervals, source citations, and parameter groups. Automatically derives
#' distribution shape parameters from the provided evidence.
#'
#' @param name Parameter name.
#' @param mean Mean (point estimate).
#' @param lower Lower bound of 95% CI (optional if `se` given).
#' @param upper Upper bound of 95% CI (optional if `se` given).
#' @param se Standard error (optional; if provided and bounds are missing,
#'   bounds are derived as mean +/- 1.96*SE).
#' @param base Base-case value (defaults to mean).
#' @param distribution Distribution for PSA: "beta", "gamma", "lognormal",
#'   "normal", "uniform", "triangular", "pert", or "fixed".
#' @param include_psa Include in PSA? Default TRUE.
#' @param include_tornado Include in tornado? Default TRUE.
#' @param group Optional group label for organizing parameters (e.g.,
#'   "Costs", "Utilities", "Treatment effects").
#' @param source Optional source citation string.
#' @param description Optional plain-text description of the parameter.
#' @return A single-row data.frame with all standard columns plus
#'   `se`, `group`, `source`, `description`, and derived shape columns
#'   (`shape1`, `shape2`).
#' @export
#' @examples
#' define_param("p_death", 0.05, se = 0.01, distribution = "beta",
#'              group = "Probabilities", source = "Smith et al. 2023")
#'
#' define_param("c_drug", 5000, lower = 3000, upper = 8000,
#'              distribution = "gamma", group = "Costs")
#'
#' define_param("hr_os", 0.72, lower = 0.58, upper = 0.88,
#'              distribution = "lognormal", group = "Treatment effects",
#'              source = "VISION trial")
define_param <- function(name, mean, lower = NA, upper = NA, se = NA,
                         base = mean, distribution = "fixed",
                         include_psa = TRUE, include_tornado = TRUE,
                         group = NA_character_, source = NA_character_,
                         description = NA_character_) {
  # Derive bounds from SE if not provided
  if (is.finite(se) && !is.finite(lower)) lower <- mean - 1.96 * se
  if (is.finite(se) && !is.finite(upper)) upper <- mean + 1.96 * se

  # Derive SE from bounds if not provided
  if (!is.finite(se) && is.finite(lower) && is.finite(upper)) {
    se <- (upper - lower) / (2 * 1.96)
  }

  # Derive distribution shape parameters
  shapes <- derive_shapes(mean, lower, upper, se, distribution)

  data.frame(
    name = name, mean = mean, lower = lower, upper = upper,
    se = se, base = base, distribution = distribution,
    include_psa = include_psa, include_tornado = include_tornado,
    shape1 = shapes[1], shape2 = shapes[2],
    group = group, source = source, description = description,
    stringsAsFactors = FALSE
  )
}

#' Read a Parameter Table from CSV
#'
#' Loads parameters from a CSV file with standardized columns. Validates
#' the table and fills in missing fields with sensible defaults. The CSV
#' must have at least a `name` column; all other columns are optional.
#'
#' @param path Path to the CSV file.
#' @param derive_shapes Logical; if TRUE, compute distribution shape
#'   parameters for each row. Default TRUE.
#' @return A data.frame suitable for use as a model parameter table.
#' @export
#' @examples
#' \dontrun{
#' params <- read_param_csv("parameters.csv")
#' }
read_param_csv <- function(path, derive_shapes = TRUE) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)

  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  if (!"name" %in% names(df)) {
    stop("CSV must have a 'name' column.", call. = FALSE)
  }

  # Ensure all standard columns exist
  std_cols <- c("name", "mean", "lower", "upper", "se", "base",
                "distribution", "include_psa", "include_tornado",
                "group", "source", "description")
  for (col in std_cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }

  # Coerce types
  df$mean <- suppressWarnings(as.numeric(df$mean))
  df$lower <- suppressWarnings(as.numeric(df$lower))
  df$upper <- suppressWarnings(as.numeric(df$upper))
  df$se <- suppressWarnings(as.numeric(df$se))
  df$base <- suppressWarnings(as.numeric(df$base))
  df$distribution <- tolower(as.character(df$distribution))
  df$distribution[is.na(df$distribution) | df$distribution == ""] <- "fixed"
  df$include_psa <- as.logical(df$include_psa)
  df$include_psa[is.na(df$include_psa)] <- TRUE
  df$include_tornado <- as.logical(df$include_tornado)
  df$include_tornado[is.na(df$include_tornado)] <- TRUE

  # Fill base from mean
  df$base[is.na(df$base)] <- df$mean[is.na(df$base)]

  # Derive bounds from SE where missing
  has_se <- is.finite(df$se)
  no_lo <- !is.finite(df$lower)
  no_hi <- !is.finite(df$upper)
  df$lower[has_se & no_lo] <- df$mean[has_se & no_lo] - 1.96 * df$se[has_se & no_lo]
  df$upper[has_se & no_hi] <- df$mean[has_se & no_hi] + 1.96 * df$se[has_se & no_hi]

  # Derive SE from bounds where missing
  no_se <- !is.finite(df$se)
  has_bounds <- is.finite(df$lower) & is.finite(df$upper)
  df$se[no_se & has_bounds] <- (df$upper[no_se & has_bounds] -
                                 df$lower[no_se & has_bounds]) / (2 * 1.96)

  # Derive shape parameters
  if (derive_shapes) {
    shapes <- t(mapply(derive_shapes_row, df$mean, df$lower, df$upper,
                       df$se, df$distribution))
    df$shape1 <- shapes[, 1]
    df$shape2 <- shapes[, 2]
  }

  df
}

#' Validate a Parameter Table with Detailed Diagnostics
#'
#' Performs comprehensive validation beyond [sanitize_params()]: checks
#' distribution-domain consistency, PSA readiness, bound logic, and
#' returns a structured diagnostic report.
#'
#' @param df A parameter data.frame.
#' @return A data.frame with columns: parameter, check, status (PASS/WARN/FAIL),
#'   message.
#' @export
#' @examples
#' params <- rbind(
#'   define_param("p_death", 0.05, se = 0.01, distribution = "beta"),
#'   define_param("c_drug", -500, se = 100, distribution = "gamma")
#' )
#' validate_param_table(params)
validate_param_table <- function(df) {
  results <- list()
  add <- function(param, check, status, msg) {
    results[[length(results) + 1L]] <<- data.frame(
      parameter = param, check = check, status = status, message = msg,
      stringsAsFactors = FALSE
    )
  }

  # Global checks
  if (!"name" %in% names(df)) {
    add("(table)", "has name column", "FAIL", "Missing 'name' column.")
    return(do.call(rbind, results))
  }

  if (any(duplicated(df$name))) {
    dups <- df$name[duplicated(df$name)]
    add("(table)", "unique names", "WARN",
        paste("Duplicate names:", paste(unique(dups), collapse = ", ")))
  } else {
    add("(table)", "unique names", "PASS", "")
  }

  supported <- c("fixed", "none", "beta", "gamma", "lognormal", "normal",
                 "uniform", "triangular", "pert")

  for (i in seq_len(nrow(df))) {
    nm <- df$name[i]
    dist <- tolower(df$distribution[i])
    m <- df$mean[i]
    lo <- df$lower[i]
    hi <- df$upper[i]
    b <- df$base[i]

    # Distribution supported?
    if (!dist %in% supported) {
      add(nm, "distribution supported", "FAIL",
          paste0("'", dist, "' not in: ", paste(supported, collapse = ", ")))
    }

    # Domain consistency
    if (dist == "beta") {
      if (is.finite(m) && (m < 0 || m > 1)) {
        add(nm, "beta domain", "FAIL",
            paste0("Mean = ", m, " outside [0,1] for beta distribution."))
      } else {
        add(nm, "beta domain", "PASS", "")
      }
      if (is.finite(lo) && lo < 0) {
        add(nm, "beta lower bound", "WARN",
            paste0("Lower = ", lo, " < 0 for beta; will be clamped."))
      }
      if (is.finite(hi) && hi > 1) {
        add(nm, "beta upper bound", "WARN",
            paste0("Upper = ", hi, " > 1 for beta; will be clamped."))
      }
    }

    if (dist == "gamma") {
      if (is.finite(m) && m < 0) {
        add(nm, "gamma domain", "FAIL",
            paste0("Mean = ", m, " < 0 for gamma distribution."))
      } else if (dist == "gamma") {
        add(nm, "gamma domain", "PASS", "")
      }
    }

    if (dist == "lognormal") {
      if (is.finite(m) && m <= 0) {
        add(nm, "lognormal domain", "FAIL",
            paste0("Mean = ", m, " <= 0 for lognormal distribution."))
      } else {
        add(nm, "lognormal domain", "PASS", "")
      }
    }

    # Bounds order
    if (is.finite(lo) && is.finite(hi) && lo > hi) {
      add(nm, "bounds order", "WARN", paste0("Lower (", lo, ") > Upper (", hi, ")."))
    }

    # Base within bounds
    if (is.finite(b) && is.finite(lo) && b < lo) {
      add(nm, "base in bounds", "WARN", paste0("Base (", b, ") < Lower (", lo, ")."))
    }
    if (is.finite(b) && is.finite(hi) && b > hi) {
      add(nm, "base in bounds", "WARN", paste0("Base (", b, ") > Upper (", hi, ")."))
    }

    # PSA readiness
    if (isTRUE(df$include_psa[i]) && dist != "fixed") {
      if (!is.finite(lo) && !is.finite(hi) && !is.finite(df$se[i])) {
        add(nm, "PSA ready", "WARN",
            "include_psa=TRUE but no bounds or SE to define distribution.")
      }
    }
  }

  do.call(rbind, results)
}

#' Draw a Full PSA Parameter Matrix
#'
#' Generates an N x P matrix of parameter draws for probabilistic sensitivity
#' analysis. Supports optional correlation between parameters via a
#' Cholesky-transformed copula.
#'
#' @param param_df Parameter data.frame with standard columns.
#' @param n Number of PSA iterations.
#' @param correlation Optional correlation matrix or output from
#'   [add_correlation()]. If NULL, parameters are drawn independently.
#' @return A data.frame with N rows and one column per PSA-eligible parameter.
#' @export
#' @examples
#' params <- rbind(
#'   define_param("p_death", 0.05, se = 0.01, distribution = "beta"),
#'   define_param("c_drug", 5000, se = 1000, distribution = "gamma"),
#'   define_param("hr", 0.72, lower = 0.58, upper = 0.88, distribution = "lognormal")
#' )
#' draws <- sample_psa(params, n = 100)
sample_psa <- function(param_df, n = 1000L, correlation = NULL) {
  psa_idx <- which(param_df$include_psa &
                     tolower(param_df$distribution) != "fixed")
  if (length(psa_idx) == 0) {
    return(data.frame(matrix(nrow = n, ncol = 0)))
  }

  psa_df <- param_df[psa_idx, ]
  p <- nrow(psa_df)

  # Generate uniform draws (independent or correlated)
  if (!is.null(correlation)) {
    corr_mat <- if (is.list(correlation)) correlation$matrix else correlation
    # Subset correlation to matching parameters
    psa_names <- psa_df$name
    if (!is.null(rownames(corr_mat))) {
      shared <- intersect(psa_names, rownames(corr_mat))
      if (length(shared) < 2) {
        U <- matrix(stats::runif(n * p), nrow = n, ncol = p)
      } else {
        # Build full correlation matrix (identity for non-correlated)
        full_corr <- diag(p)
        rownames(full_corr) <- colnames(full_corr) <- psa_names
        for (i in seq_along(shared)) {
          for (j in seq_along(shared)) {
            full_corr[shared[i], shared[j]] <- corr_mat[shared[i], shared[j]]
          }
        }
        # Cholesky decomposition for Gaussian copula
        L <- tryCatch(chol(full_corr), error = function(e) NULL)
        if (is.null(L)) {
          warning("Correlation matrix not positive definite; using independent draws.",
                  call. = FALSE)
          U <- matrix(stats::runif(n * p), nrow = n, ncol = p)
        } else {
          Z <- matrix(stats::rnorm(n * p), nrow = n, ncol = p) %*% L
          U <- stats::pnorm(Z)
        }
      }
    } else {
      U <- matrix(stats::runif(n * p), nrow = n, ncol = p)
    }
  } else {
    U <- matrix(stats::runif(n * p), nrow = n, ncol = p)
  }

  # Transform uniform draws to parameter-specific distributions
  draws <- matrix(NA_real_, nrow = n, ncol = p)
  colnames(draws) <- psa_df$name

  for (j in seq_len(p)) {
    row <- psa_df[j, ]
    dist <- tolower(row$distribution)
    m <- as.numeric(row$mean)
    lo <- suppressWarnings(as.numeric(row$lower))
    hi <- suppressWarnings(as.numeric(row$upper))
    se_val <- suppressWarnings(as.numeric(row$se))

    if (!is.finite(lo)) lo <- m * 0.8
    if (!is.finite(hi)) hi <- m * 1.2
    if (hi < lo) { tmp <- lo; lo <- hi; hi <- tmp }

    u <- U[, j]

    draws[, j] <- switch(dist,
      "beta" = {
        m_c <- min(max(m, 1e-6), 1 - 1e-6)
        ab <- .beta_ab_from_ci(m_c, lo, hi)
        stats::qbeta(u, shape1 = ab[1], shape2 = ab[2])
      },
      "gamma" = {
        m <- max(m, 1e-9)
        gp <- .gamma_params_from_ci(max(m, 1e-9), max(lo, 1e-9), max(hi, lo + 1e-9))
        stats::qgamma(u, shape = gp[1], scale = gp[2])
      },
      "lognormal" = {
        m <- max(m, 1e-9)
        lp <- .lognorm_params_from_ci(m, max(lo, 1e-9), max(hi, lo + 1e-9))
        stats::qlnorm(u, meanlog = lp[1], sdlog = lp[2])
      },
      "normal" = {
        sd_val <- if (is.finite(se_val) && se_val > 0) se_val else (hi - lo) / (2 * 1.96)
        if (!is.finite(sd_val) || sd_val <= 0) sd_val <- abs(m) / 4 + 1e-9
        stats::qnorm(u, mean = m, sd = sd_val)
      },
      "uniform" = {
        lo + u * (hi - lo)
      },
      rep(m, n)  # fallback
    )
  }

  as.data.frame(draws, check.names = FALSE)
}

#' Add Correlation Structure to Parameters
#'
#' Creates a named correlation matrix for use with [sample_psa()]. Parameters
#' not listed are assumed independent (correlation = 0).
#'
#' @param ... Named pairs or a single matrix. If pairs, use the form
#'   `add_correlation(c("param_a", "param_b"), 0.5, c("param_b", "param_c"), -0.3)`.
#' @param pairs A list of lists, each with `params` (character vector of length 2)
#'   and `rho` (correlation coefficient).
#' @return A list with `matrix` (the correlation matrix) and `pairs` (input pairs).
#' @export
#' @examples
#' corr <- add_correlation(pairs = list(
#'   list(params = c("p_death", "p_prog"), rho = 0.4),
#'   list(params = c("c_drug", "c_admin"), rho = 0.6)
#' ))
add_correlation <- function(..., pairs = NULL) {
  if (is.null(pairs)) {
    args <- list(...)
    if (length(args) == 1 && is.matrix(args[[1]])) {
      return(list(matrix = args[[1]], pairs = list()))
    }
    # Parse alternating params/rho
    pairs <- list()
    i <- 1
    while (i <= length(args)) {
      if (is.character(args[[i]]) && length(args[[i]]) == 2 &&
          i + 1 <= length(args) && is.numeric(args[[i + 1]])) {
        pairs[[length(pairs) + 1]] <- list(params = args[[i]], rho = args[[i + 1]])
        i <- i + 2
      } else {
        i <- i + 1
      }
    }
  }

  # Collect all parameter names
  all_params <- unique(unlist(lapply(pairs, function(p) p$params)))
  n <- length(all_params)
  mat <- diag(n)
  rownames(mat) <- colnames(mat) <- all_params

  for (p in pairs) {
    a <- p$params[1]; b <- p$params[2]; rho <- p$rho
    if (a %in% all_params && b %in% all_params) {
      mat[a, b] <- rho
      mat[b, a] <- rho
    }
  }

  # Check positive definiteness
  eigvals <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
  if (any(eigvals < -1e-10)) {
    warning("Correlation matrix is not positive semi-definite. ",
            "PSA draws may fail.", call. = FALSE)
  }

  list(matrix = mat, pairs = pairs)
}

#' Derive Beta Distribution Parameters
#'
#' Computes the alpha and beta shape parameters of a Beta distribution
#' from a mean and either a 95% confidence interval or a standard error.
#'
#' @param mean Mean of the parameter (must be in (0, 1)).
#' @param lower Lower bound of 95% CI (optional).
#' @param upper Upper bound of 95% CI (optional).
#' @param se Standard error (optional; used if bounds not given).
#' @return A named numeric vector with `alpha` and `beta`.
#' @export
#' @examples
#' derive_beta(mean = 0.3, lower = 0.2, upper = 0.4)
#' derive_beta(mean = 0.3, se = 0.05)
derive_beta <- function(mean, lower = NA, upper = NA, se = NA) {
  m <- min(max(mean, 1e-6), 1 - 1e-6)

  if (is.finite(se) && se > 0) {
    s2 <- se^2
  } else if (is.finite(lower) && is.finite(upper) && upper > lower) {
    s2 <- ((upper - lower) / (2 * 1.96))^2
  } else {
    s2 <- (m * (1 - m)) / 50
  }

  s2 <- min(max(s2, 1e-9), m * (1 - m) - 1e-9)
  tmp <- m * (1 - m) / s2 - 1
  alpha <- m * tmp
  beta <- (1 - m) * tmp

  if (!is.finite(alpha) || !is.finite(beta) || alpha <= 0 || beta <= 0) {
    alpha <- m * 100; beta <- (1 - m) * 100
  }

  c(alpha = alpha, beta = beta)
}

#' Derive Gamma Distribution Parameters
#'
#' Computes the shape and rate parameters of a Gamma distribution from a
#' mean and standard error (or 95% CI).
#'
#' @param mean Mean of the parameter (must be > 0).
#' @param se Standard error (optional).
#' @param lower Lower bound of 95% CI (optional).
#' @param upper Upper bound of 95% CI (optional).
#' @return A named numeric vector with `shape` and `rate`.
#' @export
#' @examples
#' derive_gamma(mean = 5000, se = 1000)
#' derive_gamma(mean = 5000, lower = 3000, upper = 8000)
derive_gamma <- function(mean, se = NA, lower = NA, upper = NA) {
  m <- max(mean, 1e-9)

  if (is.finite(se) && se > 0) {
    s2 <- se^2
  } else if (is.finite(lower) && is.finite(upper) && upper > lower) {
    s2 <- ((upper - lower) / (2 * 1.96))^2
  } else {
    s2 <- (m^2) / 10
  }

  shape <- m^2 / s2
  rate <- m / s2

  if (!is.finite(shape) || !is.finite(rate) || shape <= 0 || rate <= 0) {
    shape <- 10; rate <- 10 / m
  }

  c(shape = shape, rate = rate)
}

#' Derive Log-Normal Parameters from HR and CI
#'
#' Computes the meanlog and sdlog parameters of a log-normal distribution
#' from a hazard ratio (or relative risk) point estimate and its 95%
#' confidence interval. This is the standard approach for sampling
#' treatment effects in PSA (Briggs et al., 2006).
#'
#' @param hr Point estimate (hazard ratio, relative risk, or odds ratio).
#' @param lower Lower bound of 95% CI.
#' @param upper Upper bound of 95% CI.
#' @param se Standard error on the log scale (optional; used if CI not given).
#' @return A named numeric vector with `meanlog` and `sdlog`.
#' @export
#' @examples
#' derive_lognormal(hr = 0.72, lower = 0.58, upper = 0.88)
#' derive_lognormal(hr = 0.62, lower = 0.50, upper = 0.75)
derive_lognormal <- function(hr, lower = NA, upper = NA, se = NA) {
  hr <- max(hr, 1e-9)

  if (is.finite(lower) && is.finite(upper) && lower > 0 && upper > lower) {
    # Standard approach: SE on log scale from CI width
    z <- stats::qnorm(c(0.025, 0.975))
    sdlog <- (log(upper) - log(lower)) / (z[2] - z[1])
    meanlog <- log(hr)
  } else if (is.finite(se) && se > 0) {
    sdlog <- se
    meanlog <- log(hr)
  } else {
    sdlog <- 0.20
    meanlog <- log(hr)
  }

  c(meanlog = meanlog, sdlog = sdlog)
}


# --- Internal helper used by define_param and read_param_csv ---

#' @keywords internal
derive_shapes <- function(mean, lower, upper, se, distribution) {
  derive_shapes_row(mean, lower, upper, se, distribution)
}

#' @keywords internal
derive_shapes_row <- function(mean, lower, upper, se, distribution) {
  dist <- tolower(distribution)
  m <- suppressWarnings(as.numeric(mean))
  lo <- suppressWarnings(as.numeric(lower))
  hi <- suppressWarnings(as.numeric(upper))
  se_val <- suppressWarnings(as.numeric(se))

  if (!is.finite(m)) return(c(NA_real_, NA_real_))

  result <- switch(dist,
    "beta" = {
      ab <- derive_beta(m, lo, hi, se_val)
      c(ab["alpha"], ab["beta"])
    },
    "gamma" = {
      gp <- derive_gamma(m, se_val, lo, hi)
      c(gp["shape"], gp["rate"])
    },
    "lognormal" = {
      lp <- derive_lognormal(m, lo, hi, se_val)
      c(lp["meanlog"], lp["sdlog"])
    },
    "normal" = {
      sd_val <- if (is.finite(se_val) && se_val > 0) se_val else {
        if (is.finite(lo) && is.finite(hi)) (hi - lo) / (2 * 1.96) else abs(m) / 4
      }
      c(m, sd_val)
    },
    c(NA_real_, NA_real_)
  )

  unname(result)
}
