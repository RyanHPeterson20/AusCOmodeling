# ============================================================================
#  pred_ts_plot_functions.R
#
#  Functions for producing multi-panel predictor time-series figures
#  (e.g. SI_SE2019_pred_ts.png) showing lag-1:52 anomaly envelopes for
#  six SST/atmospheric predictors across a wildfire sub-season window.
#
#  Public interface
#  ----------------
#  build_peak_mats()         assemble per-predictor 55-column lag matrices
#  extract_season_preds()    extract one season's anomaly vectors
#  build_season_dates()      build x-axis date vector + axis elements
#  plot_pred_ts_panels()     draw (and optionally save) the full 6-panel figure
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
#' Each highlighted window spans the 4 time steps starting at the week
#' corresponding to lag \code{j} from the sub-season anchor. The anchor week
#' is derived automatically as \code{week(pred_time[1])}, so this function
#' works correctly for any sub-season (peak, early, late) without any extra
#' arguments.
#'
#' @param pred_time  Date vector for the full plotting window.
#' @param y          Numeric anomaly vector (same length as \code{pred_time}).
#' @param lag_vals   Integer vector of lag values to highlight.
#' @param col_pos    Highlight fill – positive region.
#' @param col_neg    Highlight fill – negative region.
#' @param lag_alpha  Opacity for highlighted fill.
#' @param text_cex   Character expansion for "Lag N" labels.
.draw_lag_highlights <- function(pred_time, y, lag_vals,
                                  col_pos, col_neg,
                                  lag_alpha = 0.85,
                                  text_cex  = 2.5) {
  # The anchor week is the ISO week of pred_time[1] — always the sub-season's
  # first week (one year prior to the fire season). This generalises correctly
  # to any sub-season: peak (51), early (38), late (3), etc.
  anchor_week <- week(pred_time[1L])

  for (j in lag_vals) {

    lag_week <- anchor_week - j
    if (lag_week <= 0L) lag_week <- lag_week + 52L

    hits <- which(week(pred_time) == lag_week)
    if (length(hits) == 0L) next

    # 4-step window starting at the first matching date
    idx <- hits[1L]:min(hits[1L] + 3L, length(pred_time))

    draw_envelope_zero(pred_time[idx], y[idx],
                       col_pos, col_neg, alpha = lag_alpha)

    # "Lag N" text: positioned just above the peak of the highlighted window
    y_peak  <- max(y[idx], na.rm = TRUE)
    y_label <- ifelse(y_peak < 0, 0, y_peak)

    legend(x = c(pred_time[idx[1L]] - days(9L),
                 pred_time[idx[1L]] - days(9L)),
           y = c(y_label + 1.5, y_label + 1.5),
           legend   = paste0("Lag ", j),
           box.col  = NA, bg = NA, xpd = NA,
           text.col = "grey28", cex = text_cex)
  }
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
#' @param lag_vals    Integer vector of lags to highlight (NULL = none).
#' @param show_x      Logical; draw the x-axis (bottom panel only).
#' @param colors      Named list of fill colours (see \code{.TS_COLORS}).
#' @param y_tick_lab  Numeric vector of y-axis tick positions / labels.
#' @param label_cex   cex for the predictor label and lag text.
#' @param axis_cex    cex for y-axis tick labels.
#' @param xaxis_cex   cex for x-axis tick labels.
#' @param lab_cex     cex for the y-axis title.
.draw_pred_panel <- function(pred_time, y,
                              ylim, ylab, label,
                              xlim, year_lines, month_lines,
                              xticks, xlabs,
                              lag_vals   = NULL,
                              show_x     = FALSE,
                              colors     = .TS_COLORS,
                              y_tick_lab,
                              label_cex  = 2.5,
                              axis_cex   = 2.25,
                              xaxis_cex  = 2.75,
                              lab_cex    = 2.75) {

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
                          lag_alpha = 0.85, text_cex = label_cex)
  }

  # --- predictor label (top-left) ---
  legend(x = c(xlim[1] + days(2L), xlim[1] + days(32L)),
         y = c(ylim[2], ylim[2]),
         legend   = label,
         box.col  = NA, bg = NA, xpd = NA,
         text.col = "grey30", cex = label_cex)

  # --- x-axis (bottom panel only) ---
  if (show_x) {
    axis(1, at = xticks, labels = xlabs, las = 2,
         cex.axis = xaxis_cex, line = 1)
  }
}


# ============================================================================
#  PUBLIC INTERFACE
# ============================================================================

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

  # ---- find the anchor week date in the year preceding the fire season ----
  yr         <- season_years[season_i]
  start_row  <- pred_df[pred_df$week == anchor_week & pred_df$year == yr - 1L, ]
  date_start <- ymd(start_row$date[1L])

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
#' @param lag_list     Named list of integer lag values to highlight per
#'   predictor.  Pass \code{NULL} for a predictor to suppress highlights.
#'   Default reproduces the peak-season highlights.
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
#'   # --- per-season loop ---
#'   for (i in c(2, 3, 5, 6, 15, 19)) {
#'     preds <- extract_season_preds(i, peak_mats)
#'     dates <- build_season_dates(i, pred_df, season_years, season.weeks, SE.mid)
#'
#'     plot_pred_ts_panels(
#'       season_i = i,
#'       preds    = preds,
#'       dates    = dates,
#'       seasons  = seasons,
#'       y_max    = y_max_all,
#'       outfile  = file.path(out_dir,
#'                            paste0("SI_SE", season_years[i], "_pred_ts.png"))
#'     )
#'   }
#' }
plot_pred_ts_panels <- function(
    season_i,
    preds,
    dates,
    seasons,
    preds_ord   = c("nino", "wtio", "etio", "tsa", "sam", "olr"),
    lag_list    = list(
      nino = 40L,
      wtio = c(14L, 46L),
      etio = c(7L,  33L),
      tsa  = 29L,
      sam  = c(9L,  21L),
      olr  = NULL
    ),
    y_max       = NULL,
    outfile     = NULL,
    png_dims    = list(width = 4800L, height = 5600L, res = 275L),
    pred_labels = c(
      nino = "Ni\u00f1o 3.4",
      wtio = "WTIO",
      etio = "ETIO",
      tsa  = "TSA",
      sam  = "SAM",
      olr  = "OLR"
    ),
    ylabs       = c(
      nino = "Anomaly [W/m^2]",
      wtio = "Anomaly [W/m^2]",
      etio = "Anomaly [W/m^2]",
      tsa  = "Anomaly [W/m^2]",
      sam  = "Anomaly",           # SAM/AAO: no unit label (matches original)
      olr  = "Anomaly [W/m^2]"
    ),
    colors      = .TS_COLORS
) {

  # ---- predictor draw order ----
  n_panels  <- length(preds_ord)

  # ---- shared y-axis ----
  if (is.null(y_max)) {
    y_max <- ceiling(max(abs(unlist(preds)), na.rm = TRUE) * 10L) / 10L
  }
  y_step    <- round(y_max / 2, 1L)
  y_seq     <- seq(y_step, y_max - y_step, by = y_step)
  y_tick_lab <- c(-rev(y_seq), 0, y_seq)
  ylim      <- c(-y_max, y_max)

  # ---- unpack date elements ----
  pred_time   <- dates$pred_time
  xlim        <- dates$xrange
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
      pred_time   = pred_time,
      y           = preds[[p]],
      ylim        = ylim,
      ylab        = ylabs[p],
      label       = pred_labels[p],
      xlim        = xlim,
      year_lines  = year_lines,
      month_lines = month_lines,
      xticks      = xticks,
      xlabs       = xlabs,
      lag_vals    = lag_list[[p]],
      show_x      = is_last,
      colors      = colors,
      y_tick_lab  = y_tick_lab
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
