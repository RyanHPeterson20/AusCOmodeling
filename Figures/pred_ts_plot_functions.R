# ============================================================================
#  pred_ts_plot_functions.R
#
#  Functions for producing multi-panel predictor time-series figures
#  (e.g. SI_SE2019_pred_ts.png) showing lag-1:52 anomaly envelopes for
#  six SST/atmospheric predictors across a wildfire sub-season window.
#
#  Public interface
#  ----------------
#  build_season_mats()               assemble per-predictor lag matrices
#  extract_season_preds()            extract one season's anomaly vectors
#  build_season_dates()              build x-axis date vector + axis elements
#  build_group_data()                convenience wrapper for one sub-season group
#  plot_pred_ts_panels()             multi-predictor figure for one sub-season
#  plot_mode_comparison_panels()     single-mode figure across multiple sub-seasons
#
#  Internal helpers (prefix ".")
#  --------------------------------
#  .draw_pred_panel()        draw one predictor panel
#  .draw_lag_highlights()    overlay highlighted lag windows + labels
#
#  Standalone helpers (also exported for use elsewhere)
#  -----------------------------------------------------
#  make_month_ticks()
#  make_month_lines()
#  make_year_lines()
#  add_zero_crossings()
#  draw_envelope_zero()
#
#  Dependencies: lubridate, scales
# ============================================================================

suppressMessages(library(lubridate))
suppressMessages(library(scales))


# ----------------------------------------------------------------------------
#  Color palette  (modify here to restyle all figures at once)
# ----------------------------------------------------------------------------
.TS_COLORS <- list(
  pred_pos = "#F2855DFF",   # salmon   – positive anomaly fill
  pred_neg = "#68ABB8FF",   # slate    – negative anomaly fill
  lag_pos  = "tomato3",     # darker red   – highlighted lag (positive)
  lag_neg  = "skyblue4"     # darker blue  – highlighted lag (negative)
)


# ============================================================================
#  STANDALONE HELPERS
# ============================================================================

# ----------------------------------------------------------------------------
#' Monthly axis tick positions and labels
#'
#' @param xrange  Length-2 Date vector giving the x-axis display range.
#' @return Named list: \code{ticks} (Date vector), \code{labs} (character
#'   vector; January ticks formatted as "Jan\nYYYY", others as "Mon").
make_month_ticks <- function(xrange) {
  ticks <- seq(floor_date(xrange[1], "month"),
               ceiling_date(xrange[2], "month"),
               by = "1 month")
  labs  <- ifelse(month(ticks) == 1,
                  format(ticks, "%b\n%Y"),
                  format(ticks, "%b"))
  list(ticks = ticks, labs = labs)
}

# ----------------------------------------------------------------------------
#' Monthly vertical-line positions
#'
#' @inheritParams make_month_ticks
#' @return Date vector of month-boundary positions.
make_month_lines <- function(xrange) {
  seq(floor_date(xrange[1], "month"),
      ceiling_date(xrange[2], "month"),
      by = "1 month")
}

# ----------------------------------------------------------------------------
#' Year-boundary vertical-line positions
#'
#' @inheritParams make_month_ticks
#' @return Date vector of Jan-1 positions spanning \code{xrange}.
make_year_lines <- function(xrange) {
  yrs <- seq(year(xrange[1]), year(xrange[2]), by = 1)
  ymd(paste0(yrs, "-01-01"))
}

# ----------------------------------------------------------------------------
#' Insert linearly-interpolated zero-crossing points into (x, y)
#'
#' Ensures that filled envelope polygons do not bleed across y = 0.
#'
#' @param x  Date or numeric vector of x positions.
#' @param y  Numeric anomaly vector.
#' @return Named list: \code{x} (Date), \code{y} (numeric).
add_zero_crossings <- function(x, y) {
  x <- as.Date(x)
  y <- as.numeric(y)
  o <- order(x)
  x <- x[o]
  y <- y[o]

  keep_x <- x[1L]
  keep_y <- y[1L]

  for (i in seq(2L, length(x))) {
    y0 <- y[i - 1L]; y1 <- y[i]

    if (!is.na(y0) && !is.na(y1) && y0 * y1 < 0) {
      t  <- abs(y0) / (abs(y0) + abs(y1))          # fraction of interval
      xc <- as.Date(as.numeric(x[i - 1L]) +
                      t * (as.numeric(x[i]) - as.numeric(x[i - 1L])),
                    origin = "1970-01-01")
      keep_x <- c(keep_x, xc, x[i])
      keep_y <- c(keep_y, 0,  y1)
    } else {
      keep_x <- c(keep_x, x[i])
      keep_y <- c(keep_y, y1)
    }
  }
  list(x = keep_x, y = keep_y)
}

# ----------------------------------------------------------------------------
#' Filled area chart split at y = 0 (two-colour envelope)
#'
#' @param x        Date vector (x positions).
#' @param y        Numeric anomaly vector.
#' @param col_pos  Fill colour for positive (above-zero) region.
#' @param col_neg  Fill colour for negative (below-zero) region.
#' @param alpha    Opacity passed to \code{scales::alpha()}.
draw_envelope_zero <- function(x, y, col_pos, col_neg, alpha = 0.67) {
  xy   <- add_zero_crossings(x, y)
  x2   <- xy$x
  y2   <- xy$y
  n    <- length(x2)

  # positive polygon (y > 0)
  y_pos <- ifelse(y2 > 0, y2, 0)
  polygon(c(x2, rev(x2)),
          c(rep(0, n), rev(y_pos)),
          col = scales::alpha(col_pos, alpha), border = NA)

  # negative polygon (y < 0)
  y_neg <- ifelse(y2 < 0, y2, 0)
  polygon(c(x2, rev(x2)),
          c(y_neg, rep(0, n)),
          col = scales::alpha(col_neg, alpha), border = NA)
}


# ============================================================================
#  INTERNAL HELPERS
# ============================================================================

# ----------------------------------------------------------------------------
#' Overlay highlighted lag windows and "Lag N" text labels  (internal)
#'
#' Uses a two-pass approach: envelopes are drawn first, then all label
#' positions are computed, collision-resolved (greedy left-to-right vertical
#' stagger), and drawn together.  Collision distances are measured using
#' \code{strwidth()} / \code{strheight()} in plot user coordinates so the
#' threshold automatically adapts to font size and axis scale.
#'
#' @param pred_time  Date vector for the full plotting window.
#' @param y          Numeric anomaly vector (same length as \code{pred_time}).
#' @param lag_vals   Integer vector of lag values to highlight.
#' @param col_pos    Highlight fill – positive region.
#' @param col_neg    Highlight fill – negative region.
#' @param n_group    Number of time steps in the highlight window.
#' @param ylim       Length-2 numeric y-axis limits.
#' @param lag_alpha  Opacity for highlighted fill.
#' @param text_cex   Character expansion for "Lag N" labels.
.draw_lag_highlights <- function(pred_time, y, lag_vals,
                                  col_pos, col_neg,
                                  n_group   = 4L,
                                  ylim      = c(-2, 2),
                                  lag_alpha = 0.85,
                                  text_cex  = 2.5) {

  # The anchor week is the ISO week of pred_time[1] — always the sub-season's
  # first week (one year prior to the fire season).
  anchor_week    <- week(pred_time[1L])
  label_y_offset <- diff(ylim) * 0.05

  n_vals  <- length(lag_vals)
  lbl_x   <- rep(as.Date(NA), n_vals)  # Date: x centre of label
  lbl_y   <- rep(NA_real_,    n_vals)  # y base of label
  lbl_txt <- rep(NA_character_, n_vals)
  active  <- rep(FALSE, n_vals)

  # --- pass 1: draw envelopes; collect raw label positions ---
  for (k in seq_along(lag_vals)) {
    j <- lag_vals[k]

    lag_week <- anchor_week - j
    if (lag_week <= 0L) lag_week <- lag_week + 52L

    hits <- which(week(pred_time) == lag_week)
    if (length(hits) == 0L) next
    active[k] <- TRUE

    idx <- hits[1L]:min(hits[1L] + n_group - 1L, length(pred_time))

    draw_envelope_zero(pred_time[idx], y[idx], col_pos, col_neg, alpha = lag_alpha)

    lbl_x[k]   <- pred_time[idx[1L]] + days(round((length(idx) * 7L) / 2L))
    y_peak      <- max(y[idx], na.rm = TRUE)
    lbl_y[k]   <- max(y_peak, 0) + label_y_offset
    lbl_txt[k] <- paste0("Lag ", j)
  }

  keep <- which(active)
  if (length(keep) == 0L) return(invisible(NULL))

  # --- pass 2: resolve collisions ---
  # strwidth/strheight return values in x/y user coordinates (days / anomaly units),
  # so they automatically scale with font size and axis range.
  lbl_w <- strwidth( "Lag 00", cex = text_cex, units = "user")
  lbl_h <- strheight("Lag 00", cex = text_cex, units = "user") * 1.3

  # sort active labels left to right for a greedy sweep
  ord   <- keep[order(as.numeric(lbl_x[keep]))]
  x_res <- as.numeric(lbl_x[ord])   # numeric for distance arithmetic
  y_res <- lbl_y[ord]

  # greedy sweep: for each label, check all labels to its left; if any overlap,
  # bump this one up to clear the highest overlapping neighbour
  if (length(ord) > 1L) {
    for (m in seq(2L, length(ord))) {
      for (prev in seq(1L, m - 1L)) {
        x_overlap <- abs(x_res[m] - x_res[prev]) < lbl_w
        y_overlap <- abs(y_res[m] - y_res[prev]) < lbl_h
        if (x_overlap && y_overlap) {
          y_res[m] <- max(y_res[m], y_res[prev] + lbl_h)
        }
      }
    }
  }

  # --- pass 3: draw labels at resolved positions ---
  for (m in seq_along(ord)) {
    text(x      = lbl_x[ord[m]],
         y      = y_res[m],
         labels = lbl_txt[ord[m]],
         adj    = c(0.5, 0),
         col    = "grey28",
         cex    = text_cex,
         xpd    = NA)
  }

  invisible(NULL)
}

# ----------------------------------------------------------------------------
#' Draw one predictor panel  (internal)
#'
#' Renders: black time-series line, horizontal zero line, monthly/yearly grid
#' lines, two-colour fill envelope, optional lag highlights, and predictor
#' label in the top-left corner.
#'
#' @param pred_time   Date vector for the plotting window.
#' @param y           Numeric anomaly vector.
#' @param ylim        Length-2 numeric y-axis limits.
#' @param ylab        Y-axis title string.
#' @param label       Predictor label (top-left legend text).
#' @param xlim        Length-2 Date x-axis display range.
#' @param year_lines  Date vector of year-boundary positions.
#' @param month_lines Date vector of month-boundary positions.
#' @param xticks      Date vector of x-axis tick positions.
#' @param xlabs       Character vector of x-axis tick labels.
#' @param lag_vals        Integer vector of lags to highlight (NULL = none).
#' @param show_x          Logical; draw the x-axis (bottom panel only).
#' @param colors          Named list of fill colours (see \code{.TS_COLORS}).
#' @param y_tick_lab      Numeric vector of y-axis tick positions / labels.
#' @param pred_label_cex  cex for the in-panel predictor label ("Niño 3.4" etc.).
#' @param pred_label_x_offset Integer days right of \code{xlim[1]} for the
#'   predictor label.  Default 2.
#' @param pred_label_y_frac Fractional position of the predictor label on the
#'   y-axis: 0 = top (\code{ylim[2]}), 1 = bottom (\code{ylim[1]}).  Default 0.
#' @param lag_label_cex   cex for "Lag N" highlight labels.
#' @param group_label     Optional string drawn top-right (e.g. "Early Season").
#'   \code{NULL} suppresses it.
#' @param group_label_cex cex for the group label.  Defaults to \code{pred_label_cex}.
#' @param axis_cex        cex for y-axis tick labels.
#' @param xaxis_cex       cex for x-axis tick labels.
#' @param lab_cex         cex for the y-axis title.
.draw_pred_panel <- function(pred_time, y,
                              ylim, ylab, label,
                              xlim, year_lines, month_lines,
                              xticks, xlabs,
                              lag_vals            = NULL,
                              n_group             = 4L,
                              show_x              = FALSE,
                              colors              = .TS_COLORS,
                              y_tick_lab,
                              pred_label_cex      = 2.5,
                              pred_label_x_offset = 2L,
                              pred_label_y_frac   = 0,
                              lag_label_cex       = 2.5,
                              group_label         = NULL,
                              group_label_cex     = pred_label_cex,
                              axis_cex            = 2.25,
                              xaxis_cex           = 2.75,
                              lab_cex             = 2.75) {

  # --- base plot ---
  plot(pred_time, y,
       type = "l", col = "black", lwd = 2,
       xaxt = "n", xlab = "",
       yaxt = "n", ylab = ylab, col.lab = "black",
       xlim = xlim, ylim = ylim, bty = "n",
       cex.lab = lab_cex, xpd = NA)

  # --- y-axis ---
  axis(side = 2, at = y_tick_lab, cex.axis = axis_cex,
       col = NA, line = 0, col.ticks = "black", col.axis = "black", las = 1)

  # --- reference lines ---
  abline(h = 0,           lty = 1, col = "grey50", lwd = 1)
  abline(v = month_lines, lty = 3, col = "grey30", lwd = 2)   # monthly (dotted)
  abline(v = month_lines[month(month_lines) == 1],
                          lty = 2, col = "grey30", lwd = 2)   # January (dashed)
  abline(v = year_lines,  lty = 2, col = "grey30", lwd = 2)   # year boundary

  # --- fill envelope ---
  draw_envelope_zero(pred_time, y, colors$pred_pos, colors$pred_neg, alpha = 0.50)

  # --- lag highlights ---
  if (!is.null(lag_vals) && length(lag_vals) > 0L) {
    .draw_lag_highlights(pred_time, y, lag_vals,
                          colors$lag_pos, colors$lag_neg,
                          n_group   = n_group,
                          ylim      = ylim,
                          lag_alpha = 0.85, text_cex = lag_label_cex)
  }

  # --- predictor label (top-left, left-aligned) ---
  x_lbl <- xlim[1] + days(pred_label_x_offset)
  y_lbl <- ylim[2] - diff(ylim) * pred_label_y_frac
  text(x      = x_lbl,
       y      = y_lbl,
       labels = label,
       adj    = c(0, 1),
       col    = "grey30",
       cex    = pred_label_cex,
       xpd    = NA)

  # --- group label (top-right, right-aligned) ---
  if (!is.null(group_label)) {
    text(x      = xlim[2],
         y      = ylim[2],
         labels = group_label,
         adj    = c(1, 1),
         col    = "grey30",
         cex    = group_label_cex,
         xpd    = NA)
  }

  # --- x-axis (bottom panel only) ---
  if (show_x) {
    axis(1, at = xticks, labels = xlabs, las = 2,
         cex.axis = xaxis_cex, line = 1)
  }
}


# ----------------------------------------------------------------------------
#' Compute symmetric y-axis tick positions  (internal)
#'
#' Robust replacement for the inline \code{seq(y_step, y_max - y_step, ...)}
#' idiom.  \code{round()} can push \code{y_step} slightly above
#' \code{y_max / 2}, making the sequence end < start and causing
#' \code{seq()} to throw "wrong sign in 'by' argument".
#'
#' @param y_max  Positive numeric half-range.
#' @return Numeric vector of tick positions symmetric around 0, excluding
#'   \code{±y_max} itself.
.make_yticks <- function(y_max) {
  y_step <- round(y_max / 2, 1L)
  y_seq  <- seq(y_step, y_max, by = y_step)
  y_seq  <- y_seq[y_seq < y_max]          # exclude y_max; safe if empty
  c(-rev(y_seq), 0, y_seq)
}

# ----------------------------------------------------------------------------
#' Parse a coefficient vector into a lag_list for highlighting  (internal)
#'
#' Extracts main-term coefficient names matching the pattern
#' \code{<var>_lag<N>} and groups the lag numbers by predictor key.
#' Interaction (\code{:}) and quadratic (\code{I(...)}) terms are ignored.
#'
#' @param coef_vec  Named numeric vector, e.g. \code{coef(SE1.lm)}.
#' @param key_map   Named character vector mapping model variable names to
#'   internal predictor keys where they differ.  E.g.
#'   \code{c(aao = "sam")} maps \code{aao_lag<N>} terms to the \code{"sam"}
#'   panel.  Identity mappings do not need to be listed.
#' @return Named list of integer vectors, one per unique predictor key found.
.coef_to_lag_list <- function(coef_vec, key_map = c(aao = "sam")) {

  nms <- names(coef_vec)
  if (is.null(nms)) stop("coef_vec must be a named numeric vector.")

  pat      <- "^([a-z]+)_lag([0-9]+)$"
  is_main  <- grepl(pat, nms) &
              !grepl(":", nms) &          # exclude interactions
              !grepl("^I\\(", nms)        # exclude quadratics

  main_nms <- nms[is_main]
  if (length(main_nms) == 0L)
    warning(".coef_to_lag_list: no terms matching '<var>_lag<N>' found. ",
            "Check coefficient names with names(coef_vec)).")

  lag_list <- list()

  for (nm in main_nms) {
    var_raw <- sub(pat, "\\1", nm)
    lag     <- as.integer(sub(pat, "\\2", nm))

    # apply key_map alias (e.g. aao -> sam); identity if not in map
    var_key <- if (var_raw %in% names(key_map)) key_map[[var_raw]] else var_raw

    lag_list[[var_key]] <- c(lag_list[[var_key]], lag)
  }

  lag_list
}

# ----------------------------------------------------------------------------
#' Assemble per-predictor lag matrices for one sub-season
#'
#' Mirrors the pattern of \code{pred_setup()}: takes the full \code{aus.lag}
#' list and a sub-season week vector, then derives the anchor week and number
#' of extra lags automatically from the group definition — no hardcoded week
#' numbers.
#'
#' \strong{How the columns are assembled} (using peak as the worked example):
#' \itemize{
#'   \item \code{sub_season = c(51, 52, 1, 2)};
#'         \code{season.weeks = c(38:52, 1:14)}
#'   \item Chronological ordering via \code{which(season.weeks \%in\% sub_season)}
#'         gives the group in order: 51, 52, 1, 2.
#'   \item \strong{anchor week} = 51 (first); \strong{last week} = 2 (last).
#'   \item \code{n_extra = length(group) - 1 = 3}
#'         (weeks 52, 1, 2 sit between anchor and end of season).
#'   \item Per predictor: \code{cbind(last_week[lags 1:n_extra],
#'         anchor_week[lags 1:n_main_lags])} = 55 columns.
#'   \item After \code{rev()} in \code{extract_season_preds()}, columns run
#'         oldest \eqn{\to} newest.
#' }
#'
#' @param aus.lag      Named list of weekly lag matrices (e.g. \code{SEAus.lag}).
#'   List names must follow the format \code{"Week  N"} (two spaces).
#' @param season.weeks Integer vector giving the \emph{ordered} set of season
#'   weeks (e.g. \code{c(38:52, 1:14)}).
#' @param sub_season   Integer vector of weeks belonging to this sub-season
#'   (e.g. \code{c(51, 52, 1, 2)} for peak, \code{38:50} for early,
#'   \code{3:14} for late).
#' @param n_main_lags  Number of lags to extract from the anchor week (default 52).
#' @param col_spec     Named list; each element is the \emph{start column index}
#'   for that predictor in the raw week matrix (columns 1:2 are metadata;
#'   lag 1 starts at column 3 for the first predictor).
#'   Defaults match the current \code{SEAus.lag} layout.
#' @return Named list of matrices (one per predictor);
#'   \code{n_main_lags + n_extra} columns each.
#'
#' @examples
#' \dontrun{
#'   season.weeks <- c(38:52, 1:14)
#'   SE.mid       <- c(51, 52, 1, 2)
#'
#'   peak_mats <- build_season_mats(SEAus.lag, season.weeks, SE.mid)
#'   # early_mats <- build_season_mats(SEAus.lag, season.weeks, 38:50)
#'   # late_mats  <- build_season_mats(SEAus.lag, season.weeks, 3:14)
#' }
build_season_mats <- function(
    aus.lag,
    season.weeks,
    sub_season,
    n_main_lags = 52L,
    col_spec    = list(
      nino = 3L,
      wtio = 107L,
      etio = 159L,
      tsa  = 211L,
      sam  = 263L,   # AAO / SAM
      olr  = 315L
    )
) {
  # ---- derive anchor / last week from group definition (mirrors pred_setup) ----
  grp_pos     <- which(season.weeks %in% sub_season)
  grp_ordered <- season.weeks[grp_pos]          # chronological order
  anchor_week <- grp_ordered[1L]                # e.g. 51 for peak
  last_week   <- grp_ordered[length(grp_ordered)] # e.g. 2 for peak
  n_extra     <- length(grp_ordered) - 1L       # e.g. 3 for peak

  # ---- look up week matrices from aus.lag ----
  anchor_key <- paste0("Week  ", anchor_week)
  last_key   <- paste0("Week  ", last_week)

  mat_anchor <- aus.lag[[anchor_key]]
  mat_last   <- aus.lag[[last_key]]

  if (is.null(mat_anchor))
    stop("aus.lag entry not found: '", anchor_key,
         "'. Check list names with names(aus.lag).")
  if (is.null(mat_last))
    stop("aus.lag entry not found: '", last_key,
         "'. Check list names with names(aus.lag).")

  # ---- extract and combine lag columns per predictor ----
  lapply(col_spec, function(start_col) {
    main_cols  <- start_col:(start_col + n_main_lags - 1L)  # lags 1:52 from anchor
    extra_cols <- start_col:(start_col + n_extra - 1L)      # lags 1:n_extra from last
    cbind(mat_last[,   extra_cols, drop = FALSE],
          mat_anchor[, main_cols,  drop = FALSE])
  })
}

# ----------------------------------------------------------------------------
#' Extract one season's time-ordered anomaly vectors
#'
#' Selects row \code{season_i} from each lag matrix produced by
#' \code{build_season_mats()}, and reverses the column order so that the
#' result runs oldest \eqn{\to} newest (lag 52 from the anchor week first).
#'
#' @param season_i    Integer row index (1 = first season, 19 = 2019/20, …).
#' @param season_mats Named list of matrices from \code{build_season_mats()}.
#' @return Named list of numeric vectors (one per predictor).
#'
#' @examples
#' \dontrun{
#'   peak_mats <- build_season_mats(SEAus.lag, season.weeks, SE.mid)
#'   preds     <- extract_season_preds(19, peak_mats)
#' }
extract_season_preds <- function(season_i, season_mats) {
  lapply(season_mats, function(mat) {
    as.numeric(rev(mat[season_i, ]))
  })
}

# ----------------------------------------------------------------------------
#' Build the x-axis date vector and derived axis elements for one season
#'
#' The plotting window starts at the \strong{anchor week} of the sub-season
#' (the chronologically first week, e.g. week 51 for the peak group) in the
#' year preceding the fire season, and spans \code{n_main_lags + n_extra}
#' weeks forward.  The anchor week is derived from \code{sub_season} and
#' \code{season.weeks} — no week numbers are hardcoded.
#'
#' @param season_i     Integer row index into \code{season_years}.
#' @param pred_df      Data frame with columns \code{week}, \code{year},
#'                     \code{date}.
#' @param season_years Integer vector of season-start years (e.g. 2001:2020).
#' @param season.weeks Integer vector giving the ordered set of season weeks
#'   (e.g. \code{c(38:52, 1:14)}).
#' @param sub_season   Integer vector of weeks for this sub-season group
#'   (e.g. \code{c(51, 52, 1, 2)}).
#' @param n_main_lags  Number of main lags (default 52); passed through for
#'   span calculation.
#' @return Named list:
#'   \describe{
#'     \item{pred_time}{Date vector of weekly time steps in the window.}
#'     \item{xrange}{Length-2 Date display range (start trimmed +1 week).}
#'     \item{month_ticks}{Output of \code{make_month_ticks()}.}
#'     \item{month_lines}{Output of \code{make_month_lines()}.}
#'     \item{year_lines}{Output of \code{make_year_lines()}.}
#'   }
#'
#' @examples
#' \dontrun{
#'   season.weeks <- c(38:52, 1:14)
#'   dates <- build_season_dates(19, pred_df, season_years, season.weeks, SE.mid)
#' }
build_season_dates <- function(season_i, pred_df, season_years,
                                season.weeks, sub_season,
                                n_main_lags = 52L) {

  # ---- derive anchor week and span (mirrors build_season_mats) ----
  grp_pos     <- which(season.weeks %in% sub_season)
  grp_ordered <- season.weeks[grp_pos]
  anchor_week <- grp_ordered[1L]
  n_extra     <- length(grp_ordered) - 1L
  n_span      <- n_main_lags + n_extra        # total weekly time steps

  # ---- find the anchor week date in the correct calendar year ----
  # season.weeks = c(38:52, 1:14) straddles a year boundary.
  # Weeks >= 38 fall in calendar year yr-1 (e.g. week 38 of 2018 for 2019/20).
  # Weeks  < 38 fall in calendar year yr   (e.g. week  3 of 2020 for 2019/20).
  yr          <- season_years[season_i]
  anchor_year <- if (anchor_week >= 38L) yr - 1L else yr
  start_row   <- pred_df[pred_df$week == anchor_week & pred_df$year == anchor_year, ]
  date_start  <- ymd(start_row$date[1L])

  # Select exactly n_span consecutive rows from pred_df starting at date_start.
  # This replaces the original year-boundary epiweek adjustment, which only
  # worked for the peak sub-season and always fires erroneously for sub-seasons
  # whose window does not end near ISO week 1 (e.g. SE.early).
  window_all <- pred_df[pred_df$date >= date_start, ]
  window_all <- window_all[order(window_all$date), ]
  if (nrow(window_all) < n_span)
    stop("pred_df does not contain ", n_span, " rows on or after ", date_start,
         ". Check season_i, pred_df, and n_main_lags.")
  pred_time  <- as.Date(window_all$date[seq_len(n_span)])
  xrange     <- range(pred_time)
  xrange[1L] <- xrange[1L] + weeks(1L)        # trim first step for display

  list(
    pred_time   = pred_time,
    xrange      = xrange,
    n_group     = length(grp_ordered),   # group size drives highlight window width
    month_ticks = make_month_ticks(xrange),
    month_lines = make_month_lines(xrange),
    year_lines  = make_year_lines(xrange)
  )
}

# ----------------------------------------------------------------------------
#' 6-panel predictor time-series figure
#'
#' Produces the composite figure of Niño 3.4, WTIO, ETIO, TSA, SAM, and OLR
#' anomaly time series for one wildfire season.  Optionally saves to a PNG.
#'
#' @param season_i     Integer row index into \code{seasons}.
#' @param preds        Named list from \code{extract_season_preds()}.
#'   Names must include: \code{nino}, \code{wtio}, \code{etio},
#'   \code{tsa}, \code{sam}, \code{olr}.
#' @param dates        List from \code{build_season_dates()}.
#' @param seasons      Character vector of "YYYY-YYYY" season labels.
#' @param model_coef Named numeric vector of model coefficients, e.g.
#'   \code{coef(SE1.lm)}.  When provided, lag values to highlight are parsed
#'   automatically from terms matching \code{<var>_lag<N>}.  Takes priority
#'   over \code{lag_list}.  \code{NULL} falls back to \code{lag_list}.
#' @param lag_list   Named list of integer lag values to highlight per
#'   predictor, used only when \code{model_coef = NULL}.  Set a predictor's
#'   entry to \code{NULL} to suppress highlights for that panel.
#' @param key_map    Named character vector mapping model variable names to
#'   internal predictor keys where they differ (e.g. \code{c(aao = "sam")}).
#'   Only needed when \code{model_coef} is supplied.  Default covers the
#'   known \code{aao} → \code{sam} alias.
#' @param y_max        Shared y-axis half-range.  Computed from \code{preds}
#'   if \code{NULL} (rounded up to one decimal place).
#' @param outfile      Full path for PNG output.  \code{NULL} plots to the
#'   current graphics device.
#' @param png_dims     Named list with \code{width}, \code{height}, \code{res}.
#' @param pred_labels  Named character vector of panel predictor labels.
#' @param ylabs        Named character vector of y-axis titles per predictor.
#' @param colors       Named list of fill colours; defaults to
#'   \code{.TS_COLORS}.
#'
#' @return Invisibly returns \code{NULL}.  Side effect: one figure.
#'
#' @examples
#' \dontrun{
#'   season.weeks <- c(38:52, 1:14)
#'   SE.mid       <- c(51, 52, 1, 2)
#'
#'   # --- one-time setup ---
#'   peak_mats <- build_season_mats(SEAus.lag, season.weeks, SE.mid)
#'   y_max_all <- max(abs(unlist(peak_mats)), na.rm = TRUE)
#'
#'   # --- per-season loop: lags parsed from model coefficients ---
#'   for (i in c(2, 3, 5, 6, 15, 19)) {
#'     preds <- extract_season_preds(i, peak_mats)
#'     dates <- build_season_dates(i, pred_df, season_years, season.weeks, SE.mid)
#'
#'     plot_pred_ts_panels(
#'       season_i   = i,
#'       preds      = preds,
#'       dates      = dates,
#'       seasons    = seasons,
#'       model_coef = coef(SE1.lm),
#'       y_max      = y_max_all,
#'       outfile    = file.path(out_dir,
#'                              paste0("SI_SE", season_years[i], "_pred_ts.png"))
#'     )
#'   }
#' }
#' @param pred_label_cex       cex for the in-panel predictor labels.  Default 2.5.
#' @param pred_label_x_offset  Integer days right of \code{xlim[1]} for the
#'   predictor label.  Default 2.
#' @param pred_label_y_frac    Fractional position on the y-axis: 0 = top
#'   (\code{ylim[2]}), 1 = bottom (\code{ylim[1]}).  Default 0 (top-left).
#' @param lag_label_cex        cex for "Lag N" highlight labels.  Default 2.5.
plot_pred_ts_panels <- function(
    season_i,
    preds,
    dates,
    seasons,
    preds_ord           = c("nino", "wtio", "etio", "tsa", "sam", "olr"),
    model_coef          = NULL,
    lag_list            = NULL,
    key_map             = c(aao = "sam"),
    y_max               = NULL,
    outfile             = NULL,
    png_dims            = list(width = 4800L, height = 5600L, res = 275L),
    pred_labels         = c(
      nino = "Ni\u00f1o 3.4",
      wtio = "WTIO",
      etio = "ETIO",
      tsa  = "TSA",
      sam  = "SAM",
      olr  = "OLR"
    ),
    ylabs               = c(
      nino = "Anomaly [W/m^2]",
      wtio = "Anomaly [W/m^2]",
      etio = "Anomaly [W/m^2]",
      tsa  = "Anomaly [W/m^2]",
      sam  = "Anomaly",
      olr  = "Anomaly [W/m^2]"
    ),
    pred_label_cex      = 2.5,
    pred_label_x_offset = 2L,
    pred_label_y_frac   = 0,
    lag_label_cex       = 2.5,
    colors              = .TS_COLORS
) {

  # ---- resolve lag highlights ----
  # model_coef takes priority; lag_list is the manual fallback
  if (!is.null(model_coef)) {
    lag_list <- .coef_to_lag_list(model_coef, key_map = key_map)
  } else if (is.null(lag_list)) {
    warning("plot_pred_ts_panels: both model_coef and lag_list are NULL. ",
            "No lag highlights will be drawn.")
    lag_list <- list()
  }

  # ---- predictor draw order ----
  n_panels  <- length(preds_ord)

  # ---- shared y-axis ----
  if (is.null(y_max)) {
    y_max <- ceiling(max(abs(unlist(preds)), na.rm = TRUE) * 10L) / 10L
  }
  y_tick_lab <- .make_yticks(y_max)
  ylim       <- c(-y_max, y_max)

  # ---- unpack date elements ----
  pred_time   <- dates$pred_time
  xlim        <- dates$xrange
  n_group     <- dates$n_group        # sub-season group size → highlight window width
  xticks      <- dates$month_ticks$ticks
  xlabs       <- dates$month_ticks$labs
  month_lines <- dates$month_lines
  year_lines  <- dates$year_lines

  # ---- open output device ----
  if (!is.null(outfile)) {
    png(filename = outfile,
        width    = png_dims$width,
        height   = png_dims$height,
        res      = png_dims$res)
    on.exit(dev.off(), add = TRUE)
  }

  # ---- layout ----
  par(mfrow = c(n_panels, 1))
  par(oma  = c(7, 4, 5.5, 0))   # bottom: x labels; top: figure title
  par(mgp  = c(4, 0.25, 0))     # axis title, tick labels, tick marks

  # ---- panels ----
  for (k in seq_along(preds_ord)) {

    p        <- preds_ord[k]
    is_first <- k == 1L
    is_last  <- k == n_panels

    # top panel gets a small top margin for the outer title; bottom gets
    # space for the x-axis; all others are flush
    par(mar = c(if (is_last) 1L else 0L,
                5L,
                if (is_first) 1L else 0L,
                0L))

    .draw_pred_panel(
      pred_time           = pred_time,
      y                   = preds[[p]],
      ylim                = ylim,
      ylab                = ylabs[p],
      label               = pred_labels[p],
      xlim                = xlim,
      year_lines          = year_lines,
      month_lines         = month_lines,
      xticks              = xticks,
      xlabs               = xlabs,
      lag_vals            = lag_list[[p]],
      n_group             = n_group,
      show_x              = is_last,
      colors              = colors,
      y_tick_lab          = y_tick_lab,
      pred_label_cex      = pred_label_cex,
      pred_label_x_offset = pred_label_x_offset,
      pred_label_y_frac   = pred_label_y_frac,
      lag_label_cex       = lag_label_cex
    )

    # season title sits in the outer top margin, above the first panel
    if (is_first) {
      title(paste0(seasons[season_i], " Wildfire Season"),
            adj     = 0,
            cex.main = 3.0,
            xpd     = TRUE,
            outer   = TRUE)
    }
  }

  invisible(NULL)
}


# ============================================================================
#  SUB-SEASON COMPARISON FUNCTIONS
# ============================================================================

# ----------------------------------------------------------------------------
#' Build all data needed for one sub-season group and one season
#'
#' Convenience wrapper that calls \code{build_season_mats()},
#' \code{extract_season_preds()}, and \code{build_season_dates()} in one step.
#' The returned list is passed directly as one element of the \code{groups}
#' argument to \code{plot_mode_comparison_panels()}.
#'
#' @param season_i     Integer row index into \code{season_years}.
#' @param aus.lag      Named list of weekly lag matrices (e.g. \code{SEAus.lag}).
#' @param pred_df      Data frame with columns \code{week}, \code{year}, \code{date}.
#' @param season_years Integer vector of season-start years.
#' @param season.weeks Ordered integer vector of all season weeks
#'   (e.g. \code{c(38:52, 1:14)}).
#' @param sub_season   Integer vector of weeks for this sub-season group.
#' @param model_coef   Named numeric coefficient vector (e.g. \code{coef(lm)}).
#'   Takes priority over \code{lag_vals} when both are supplied.
#' @param lag_vals     Named list of integer lag vectors, used only when
#'   \code{model_coef = NULL}.
#' @param n_main_lags  Number of main lags (default 52).
#' @return Named list: \code{preds}, \code{dates}, \code{model_coef},
#'   \code{lag_vals}.
#'
#' @examples
#' \dontrun{
#'   season.weeks <- c(38:52, 1:14)
#'
#'   groups <- list(
#'     early = build_group_data(19, SEAus.lag, pred.df, season.years,
#'                              season.weeks, SE.early,
#'                              model_coef = coef(SE.early.lm)),
#'     peak  = build_group_data(19, SEAus.lag, pred.df, season.years,
#'                              season.weeks, SE.mid,
#'                              model_coef = coef(SE.peak.lm)),
#'     late  = build_group_data(19, SEAus.lag, pred.df, season.years,
#'                              season.weeks, SE.late,
#'                              model_coef = coef(SE.late.lm))
#'   )
#' }
build_group_data <- function(season_i, aus.lag, pred_df, season_years,
                              season.weeks, sub_season,
                              model_coef  = NULL,
                              lag_vals    = NULL,
                              n_main_lags = 52L) {
  mats  <- build_season_mats(aus.lag, season.weeks, sub_season,
                              n_main_lags = n_main_lags)
  preds <- extract_season_preds(season_i, mats)
  dates <- build_season_dates(season_i, pred_df, season_years,
                               season.weeks, sub_season, n_main_lags)
  list(preds      = preds,
       dates      = dates,
       model_coef = model_coef,
       lag_vals   = lag_vals)
}

# ----------------------------------------------------------------------------
#' Multi-panel sub-season comparison for a single climate mode
#'
#' Produces a stacked figure with one panel per sub-season group, all showing
#' the same climate mode's anomaly time series.  Panels have their own x-axis
#' ranges (each group's lag window) but share a common y-axis scale.  The
#' mode label appears top-left of every panel; an optional group label appears
#' top-right.  Lag highlights are parsed independently per group from each
#' group's \code{model_coef} or \code{lag_vals}.
#'
#' @param season_i     Integer row index into \code{seasons}.
#' @param mode         Single predictor key to display, e.g. \code{"nino"}.
#' @param groups       Named list of group data, each element produced by
#'   \code{build_group_data()} or built manually as
#'   \code{list(preds, dates, model_coef, lag_vals)}.
#'   List order determines panel order (top to bottom).
#' @param seasons      Character vector of \code{"YYYY-YYYY"} season labels.
#' @param group_labels Character vector of top-right panel labels, one per
#'   group.  \code{NULL} suppresses all group labels.
#'   Default uses \code{names(groups)}.
#' @param key_map      Named character vector mapping model term names to
#'   internal predictor keys (e.g. \code{c(aao = "sam")}).
#' @param y_max        Shared y-axis half-range.  \code{NULL} = auto-computed
#'   across all groups for \code{mode}.
#' @param ylab         Y-axis title applied to all panels.
#' @param outfile      Full path for PNG output.  \code{NULL} = current device.
#' @param png_dims     Named list: \code{width}, \code{height}, \code{res}.
#' @param mode_label   Override display string for the mode label.  \code{NULL}
#'   looks up from \code{pred_labels}.
#' @param pred_labels  Named character vector of display labels per mode key.
#' @param pred_label_cex      cex for the mode label.
#' @param pred_label_x_offset Integer days right of \code{xlim[1]}.
#' @param pred_label_y_frac   Fractional y position: 0 = top, 1 = bottom.
#' @param group_label_cex     cex for the group label.
#' @param lag_label_cex       cex for "Lag N" labels.
#' @param colors       Named list of fill colours; defaults to \code{.TS_COLORS}.
#'
#' @return Invisibly \code{NULL}.  Side effect: one figure.
#'
#' @examples
#' \dontrun{
#'   season.weeks <- c(38:52, 1:14)
#'
#'   groups <- list(
#'     early = build_group_data(19, SEAus.lag, pred.df, season.years,
#'                              season.weeks, SE.early,
#'                              model_coef = coef(SE.early.lm)),
#'     peak  = build_group_data(19, SEAus.lag, pred.df, season.years,
#'                              season.weeks, SE.mid,
#'                              model_coef = coef(SE.peak.lm)),
#'     late  = build_group_data(19, SEAus.lag, pred.df, season.years,
#'                              season.weeks, SE.late,
#'                              model_coef = coef(SE.late.lm))
#'   )
#'
#'   plot_mode_comparison_panels(
#'     season_i = 19,
#'     mode     = "nino",
#'     groups   = groups,
#'     seasons  = seasons,
#'     outfile  = file.path(out_dir, "fig3_SE2019_nino_comparison.png")
#'   )
#' }
plot_mode_comparison_panels <- function(
    season_i,
    mode,
    groups,
    seasons,
    group_labels        = names(groups),
    key_map             = c(aao = "sam"),
    y_max               = NULL,
    ylab                = "Anomaly [W/m^2]",
    outfile             = NULL,
    png_dims            = list(width = 4800L, height = 5600L, res = 275L),
    mode_label          = NULL,
    pred_labels         = c(
      nino = "Ni\u00f1o 3.4",
      wtio = "WTIO",
      etio = "ETIO",
      tsa  = "TSA",
      sam  = "SAM",
      olr  = "OLR"
    ),
    pred_label_cex      = 2.5,
    pred_label_x_offset = 2L,
    pred_label_y_frac   = 0,
    group_label_cex     = 2.5,
    lag_label_cex       = 2.5,
    colors              = .TS_COLORS
) {

  n_panels <- length(groups)

  # ---- resolve mode display label ----
  lbl_mode <- if (!is.null(mode_label)) mode_label else pred_labels[mode]

  # ---- resolve lag highlights per group ----
  lag_list_per_group <- lapply(groups, function(g) {
    if (!is.null(g$model_coef)) {
      ll <- .coef_to_lag_list(g$model_coef, key_map = key_map)
      ll[[mode]]
    } else if (!is.null(g$lag_vals)) {
      if (is.list(g$lag_vals)) g$lag_vals[[mode]] else g$lag_vals
    } else {
      NULL
    }
  })

  # ---- shared y-axis: max across all groups for this mode ----
  if (is.null(y_max)) {
    all_vals <- unlist(lapply(groups, function(g) g$preds[[mode]]))
    y_max    <- ceiling(max(abs(all_vals), na.rm = TRUE) * 10L) / 10L
  }
  y_step     <- round(y_max / 2, 1L)
  y_seq      <- seq(y_step, y_max - y_step, by = y_step)
  y_tick_lab <- c(-rev(y_seq), 0, y_seq)
  ylim       <- c(-y_max, y_max)

  # ---- open output device ----
  if (!is.null(outfile)) {
    png(filename = outfile,
        width    = png_dims$width,
        height   = png_dims$height,
        res      = png_dims$res)
    on.exit(dev.off(), add = TRUE)
  }

  # ---- layout ----
  par(mfrow = c(n_panels, 1))
  par(oma  = c(7, 4, 5.5, 0))
  par(mgp  = c(4, 0.25, 0))

  # ---- panels ----
  for (k in seq_len(n_panels)) {

    g        <- groups[[k]]
    is_first <- k == 1L
    is_last  <- k == n_panels

    # each panel has its own x-axis derived from its group's lag window
    pred_time   <- g$dates$pred_time
    xlim        <- g$dates$xrange
    n_group     <- g$dates$n_group
    month_lines <- g$dates$month_lines
    year_lines  <- g$dates$year_lines
    xticks      <- g$dates$month_ticks$ticks
    xlabs       <- g$dates$month_ticks$labs

    par(mar = c(if (is_last) 1L else 0L,
                5L,
                if (is_first) 1L else 0L,
                0L))

    .draw_pred_panel(
      pred_time           = pred_time,
      y                   = g$preds[[mode]],
      ylim                = ylim,
      ylab                = ylab,
      label               = lbl_mode,
      xlim                = xlim,
      year_lines          = year_lines,
      month_lines         = month_lines,
      xticks              = xticks,
      xlabs               = xlabs,
      lag_vals            = lag_list_per_group[[k]],
      n_group             = n_group,
      show_x              = is_last,
      colors              = colors,
      y_tick_lab          = y_tick_lab,
      pred_label_cex      = pred_label_cex,
      pred_label_x_offset = pred_label_x_offset,
      pred_label_y_frac   = pred_label_y_frac,
      lag_label_cex       = lag_label_cex,
      group_label         = if (!is.null(group_labels)) group_labels[k] else NULL,
      group_label_cex     = group_label_cex
    )

    if (is_first) {
      title(paste0(seasons[season_i], " Wildfire Season"),
            adj      = 0,
            cex.main = 3.0,
            xpd      = TRUE,
            outer    = TRUE)
    }
  }

  invisible(NULL)
}
