#' Sanitize a Parameter Table
#'
#' Ensures required columns exist, coerces types, fixes reversed bounds,
#' fills missing base values, and de-duplicates names.
#'
#' @param df A data.frame of parameters.
#' @return A list with `df` (sanitized) and `notes` (character vector of actions taken).
#' @export
sanitize_params <- function(df) {
  notes <- character(0)

  if (is.null(df) || !nrow(df)) {
    empty <- data.frame(
      name = character(0), distribution = character(0),
      mean = numeric(0), lower = numeric(0), upper = numeric(0),
      base = numeric(0), include_psa = logical(0), include_tornado = logical(0),
      stringsAsFactors = FALSE
    )
    return(list(df = empty, notes = c(notes, "No parameters provided.")))
  }

  required_cols <- c("name", "distribution", "mean", "lower", "upper",
                     "base", "include_psa", "include_tornado")
  for (nm in required_cols) {
    if (!nm %in% names(df)) df[[nm]] <- NA
  }

  df$name <- as.character(df$name)
  df$distribution <- tolower(as.character(df$distribution))

  # Numeric and logical columns may arrive as text from a CSV ("0,05", "5%",
  # "yes", "1"); parse them explicitly and report every value that was lost
  # (review finding D3: silent NA then silently FALSE/TRUE).
  for (nm in c("mean", "lower", "upper", "base")) {
    parsed <- .parse_numeric(df[[nm]])
    lost <- !is.na(parsed$raw) & nzchar(parsed$raw) & is.na(parsed$value)
    if (any(lost)) {
      notes <- c(notes, sprintf("Column '%s': could not read %s.", nm,
                                paste(sprintf("'%s' (%s)", parsed$raw[lost], df$name[lost]), collapse = ", ")))
    }
    df[[nm]] <- parsed$value
  }
  for (nm in c("include_psa", "include_tornado")) {
    parsed <- .parse_logical(df[[nm]])
    lost <- !is.na(parsed$raw) & nzchar(parsed$raw) & is.na(parsed$value)
    if (any(lost)) {
      notes <- c(notes, sprintf("Column '%s': unreadable flag(s) %s treated as FALSE.", nm,
                                paste(unique(parsed$raw[lost]), collapse = ", ")))
    }
    df[[nm]] <- parsed$value
  }

  df$distribution[is.na(df$distribution) | df$distribution == "" |
                     df$distribution == "none"] <- "fixed"

  empty_name <- is.na(df$name) | trimws(df$name) == ""
  if (any(empty_name, na.rm = TRUE)) {
    n_drop <- sum(empty_name, na.rm = TRUE)
    df <- df[!empty_name, , drop = FALSE]
    notes <- c(notes, sprintf("Dropped %d row(s) with empty 'name'.", n_drop))
  }

  if (any(duplicated(df$name))) {
    old <- df$name
    df$name <- make.unique(df$name, sep = "_")
    if (!identical(old, df$name)) {
      notes <- c(notes, "De-duplicated parameter names with suffixes.")
    }
  }

  bad_bounds <- !is.na(df$lower) & !is.na(df$upper) & (df$lower > df$upper)
  if (any(bad_bounds, na.rm = TRUE)) {
    tmp <- df$lower[bad_bounds]
    df$lower[bad_bounds] <- df$upper[bad_bounds]
    df$upper[bad_bounds] <- tmp
    notes <- c(notes, sprintf("Swapped lower/upper for %d row(s) with reversed bounds.",
                              sum(bad_bounds, na.rm = TRUE)))
  }

  base_na <- is.na(df$base)
  if (any(base_na, na.rm = TRUE)) {
    use_mean <- !is.na(df$mean)
    df$base[base_na & use_mean] <- df$mean[base_na & use_mean]
    still_na <- is.na(df$base) & !is.na(df$lower)
    df$base[still_na] <- df$lower[still_na]
    still_na2 <- is.na(df$base) & !is.na(df$upper)
    df$base[still_na2] <- df$upper[still_na2]
  }

  fin_lower <- is.finite(df$lower)
  fin_upper <- is.finite(df$upper)
  fin_base <- is.finite(df$base)
  below <- fin_lower & fin_base & (df$base < df$lower)
  above <- fin_upper & fin_base & (df$base > df$upper)
  if (any(below, na.rm = TRUE)) {
    df$base[below] <- df$lower[below]
    notes <- c(notes, sprintf("Clamped base to lower for %d row(s).", sum(below, na.rm = TRUE)))
  }
  if (any(above, na.rm = TRUE)) {
    df$base[above] <- df$upper[above]
    notes <- c(notes, sprintf("Clamped base to upper for %d row(s).", sum(above, na.rm = TRUE)))
  }

  df$include_psa[is.na(df$include_psa)] <- FALSE
  df$include_tornado[is.na(df$include_tornado)] <- FALSE

  list(df = df, notes = notes)
}

#' Sample a Single Parameter Value from Its Distribution
#'
#' Conventions: `mean` is the centre of the distribution and `lower`/`upper`
#' its 95% interval. For `beta`, `gamma` and `normal` the interval width sets
#' the variance by moment matching; for `lognormal` (hazard and risk ratios)
#' `mean` is the point estimate, i.e. the median, and the interval is read on
#' the log scale, as in [derive_lognormal()]. Draws are restricted only to the
#' domain of the distribution (beta between 0 and 1; gamma and lognormal non-negative;
#' uniform, triangular and PERT within their bounds); parameter names play no
#' role. A row whose lower and upper bounds coincide carries no uncertainty and
#' returns `base`.
#'
#' @param row A single-row data.frame with columns: distribution, mean, lower, upper, base.
#' @return A single numeric draw.
#' @export
sample_param_row <- function(row) {
  dist <- tolower(as.character(row$distribution))
  m <- as.numeric(row$mean)
  lo <- suppressWarnings(as.numeric(row$lower))
  hi <- suppressWarnings(as.numeric(row$upper))
  base <- suppressWarnings(as.numeric(row$base))

  if (dist %in% c("fixed", "none")) return(base)
  # Equal bounds mean "no uncertainty": do not invent a variance (finding A4).
  if (is.finite(lo) && is.finite(hi) && lo == hi) return(if (is.finite(base)) base else m)

  if (!is.finite(m)) m <- base
  if (!is.finite(m)) m <- mean(c(lo, hi), na.rm = TRUE)
  if (!is.finite(lo)) lo <- m * 0.8
  if (!is.finite(hi)) hi <- m * 1.2
  if (hi < lo) {
    tmp <- lo; lo <- hi; hi <- tmp
  }

  val <- switch(dist,
    "beta" = {
      m_c <- .clamp01(m)
      pars <- .beta_ab_from_ci(m_c, lo, hi)
      .clamp01(stats::rbeta(1, shape1 = pars[1], shape2 = pars[2]))
    },
    "gamma" = {
      m <- max(m, 1e-9)
      lo <- max(lo, 1e-9)
      hi <- max(hi, lo + 1e-9)
      pars <- .gamma_params_from_ci(m, lo, hi)
      max(0, stats::rgamma(1, shape = pars[1], scale = pars[2]))
    },
    "lognormal" = {
      m <- max(m, 1e-9)
      lo <- max(lo, 1e-9)
      hi <- max(hi, lo + 1e-9)
      pars <- .lognorm_params_from_ci(m, lo, hi)
      max(0, stats::rlnorm(1, meanlog = pars[1], sdlog = pars[2]))
    },
    "normal" = {
      sd <- (hi - lo) / (2 * 1.96)
      if (!is.finite(sd) || sd <= 0) sd <- abs(m) / 4 + 1e-9
      stats::rnorm(1, mean = m, sd = sd)
    },
    "uniform" = {
      stats::runif(1, min = lo, max = hi)
    },
    "triangular" = {
      # The mode must lie inside the bounds (imputed bounds may not contain it).
      mode <- min(max(if (is.finite(base)) base else m, lo), hi)
      .rtri(1, min = lo, mode = mode, max = hi)
    },
    "pert" = {
      mode <- min(max(if (is.finite(base)) base else m, lo), hi)
      .rpert(1, min = lo, mode = mode, max = hi)
    },
    base
  )

  # Name-based clamping was removed (review finding A3): it zeroed negative
  # parameters such as disutilities or log hazard ratios whose names matched
  # "u_" or "drug". The distribution's own domain is enforced above.
  val
}

# --- Parsing helpers shared by sanitize_params() and read_param_csv() ---

# Numbers typed as text: trims blanks, accepts "5%" (as 0.05), a decimal comma
# ("0,05") and thousands separators ("1,000.5"). Returns list(raw, value).
.parse_numeric <- function(x) {
  raw <- trimws(as.character(x))
  if (is.numeric(x)) return(list(raw = raw, value = as.numeric(x)))
  s <- gsub("[[:space:]]", "", raw)
  pct <- grepl("%$", s)
  s <- sub("%$", "", s)
  both <- grepl(",", s, fixed = TRUE) & grepl(".", s, fixed = TRUE)
  s[both] <- gsub(",", "", s[both], fixed = TRUE)          # 1,000.5 -> 1000.5
  only_comma <- grepl(",", s, fixed = TRUE) & !both
  s[only_comma] <- sub(",", ".", s[only_comma], fixed = TRUE) # 0,05 -> 0.05
  v <- suppressWarnings(as.numeric(s))
  v[pct] <- v[pct] / 100
  list(raw = raw, value = v)
}

# Flags typed as text or numbers: TRUE/FALSE, T/F, yes/no, y/n, 1/0.
.parse_logical <- function(x) {
  raw <- trimws(as.character(x))
  if (is.logical(x)) return(list(raw = raw, value = x))
  key <- tolower(raw)
  v <- rep(NA, length(key))
  v[key %in% c("true", "t", "yes", "y", "1")] <- TRUE
  v[key %in% c("false", "f", "no", "n", "0")] <- FALSE
  list(raw = raw, value = v)
}

# --- Internal helpers ---

.clamp01 <- function(x) pmin(pmax(x, 0), 1)

.beta_ab_from_ci <- function(m, lo, hi) {
  m <- min(max(m, 1e-6), 1 - 1e-6)
  s2 <- if (!is.na(lo) && !is.na(hi) && hi > lo) ((hi - lo) / 4)^2 else (m * (1 - m)) / 50
  v <- min(max(s2, 1e-9), m * (1 - m) - 1e-9)
  tmp <- m * (1 - m) / v - 1
  a <- m * tmp
  b <- (1 - m) * tmp
  if (!is.finite(a) || !is.finite(b) || a <= 0 || b <= 0) {
    a <- m * 100; b <- (1 - m) * 100
  }
  c(a, b)
}

.gamma_params_from_ci <- function(m, lo, hi) {
  m <- max(m, 1e-9)
  s2 <- if (!is.na(lo) && !is.na(hi) && hi > lo && lo > 0) ((hi - lo) / 4)^2 else (m^2) / 10
  k <- m^2 / s2
  th <- s2 / m
  if (!is.finite(k) || !is.finite(th) || k <= 0 || th <= 0) {
    k <- 10; th <- m / k
  }
  c(k, th)
}

# Lognormal from a point estimate and its 95% interval. `m` is the point
# estimate (median), so meanlog = log(m): the same convention as
# derive_lognormal(), which makes the sampled interval match the stated one
# (review finding A5: the former arithmetic-mean convention shifted both).
.lognorm_params_from_ci <- function(m, lo, hi) {
  m <- max(m, 1e-9)
  if (!is.na(lo) && !is.na(hi) && lo > 0 && hi > lo) {
    z <- stats::qnorm(c(0.025, 0.975))
    sig <- (log(hi) - log(lo)) / (z[2] - z[1])
  } else {
    sig <- 0.5
  }
  c(log(m), sig)
}

.rtri <- function(n, min, mode, max) {
  u <- stats::runif(n)
  Fc <- (mode - min) / (max - min)
  ifelse(u < Fc,
         min + sqrt(u * (max - min) * (mode - min)),
         max - sqrt((1 - u) * (max - min) * (max - mode)))
}

.rpert <- function(n, min, mode, max, lambda = 4) {
  if (!is.finite(min) || !is.finite(mode) || !is.finite(max) || min >= max) {
    return(rep(NA_real_, n))
  }
  a <- 1 + lambda * (mode - min) / (max - min)
  b <- 1 + lambda * (max - mode) / (max - min)
  x <- stats::rbeta(n, shape1 = max(a, 1e-6), shape2 = max(b, 1e-6))
  min + x * (max - min)
}
