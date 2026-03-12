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
#' Each highlighted window spans the 4 time steps centred on the week
#' corresponding to \code{lag_week = 51 - j} in \code{pred_time}.
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
  for (j in lag_vals) {

    lag_week <- 51L - j                             # ISO week number
    if (lag_week <= 0L) lag_week <- lag_week + 52L  # wrap for large lags

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
#' Assemble per-predictor 55-column lag matrices for the peak sub-season
#'
#' The peak window uses lags 1–3 from Week 2 prepended to lags 1–52 from
#' Week 51, yielding 55 columns per predictor (rows = seasons).
#'
#' @param week51  Wide lag matrix/data-frame for Week 51
#'               (from \code{SEAus.lag$\`Week  51\`}).
#' @param week2   Wide lag matrix/data-frame for Week 2
#'               (from \code{SEAus.lag$\`Week   2\`}).
#' @param col_spec Named list giving the \emph{first} and \emph{last} column
#'   indices for each predictor in the wide data, e.g.
#'   \code{list(nino = c(3, 54), wtio = c(107, 158), ...)}.
#'   Defaults match the current \code{SEAus.lag} layout.
#' @return Named list of matrices (one per predictor); 55 columns each.
#'
#' @examples
#' \dontrun{
#'   peak_mats <- build_peak_mats(SEAus.lag$`Week  51`, SEAus.lag$`Week   2`)
#' }
build_peak_mats <- function(
    week51,
    week2,
    col_spec = list(
      nino = c(3L,   54L),
      wtio = c(107L, 158L),
      etio = c(159L, 210L),
      tsa  = c(211L, 262L),
      sam  = c(263L, 314L),   # AAO / SAM
      olr  = c(315L, 366L)
    )
) {
  lapply(col_spec, function(rng) {
    lag52_cols <- rng[1L]:rng[2L]
    lag3_cols  <- rng[1L]:(rng[1L] + 2L)         # lags 1:3 from week 2
    cbind(week2[, lag3_cols, drop = FALSE],
          week51[, lag52_cols, drop = FALSE])
  })
}

# ----------------------------------------------------------------------------
#' Extract one season's time-ordered anomaly vectors
#'
#' Selects row \code{season_i} from each 55-column matrix, reverses the
#' column order so that the earliest date (lag 52 from week 51) is first
#' in time.
#'
#' @param season_i  Integer row index (1 = first season, 19 = 2019/20, …).
#' @param peak_mats Named list of matrices from \code{build_peak_mats()}.
#' @return Named list of numeric vectors (length 55), one per predictor.
#'
#' @examples
#' \dontrun{
#'   preds <- extract_season_preds(19, peak_mats)
#' }
extract_season_preds <- function(season_i, peak_mats) {
  lapply(peak_mats, function(mat) {
    as.numeric(rev(mat[season_i, ]))
  })
}

# ----------------------------------------------------------------------------
#' Build the x-axis date vector and derived axis elements for one season
#'
#' The plotting window runs from Week 51 of the preceding calendar year
#' (lag 52 anchor) through approximately Week 2 of the following year
#' (lag 1 anchor), spanning ~55 weekly steps.
#'
#' @param season_i    Integer row index into \code{season_years}.
#' @param pred_df     Data frame with columns \code{week}, \code{year},
#'                    \code{date}.
#' @param season_years Integer vector of season-start years (e.g. 2001:2020).
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
#'   dates <- build_season_dates(19, pred_df, season_years)
#' }
build_season_dates <- function(season_i, pred_df, season_years) {

  yr         <- season_years[season_i]
  start_row  <- pred_df[pred_df$week == 51L & pred_df$year == yr - 1L, ]
  date_start <- ymd(start_row$date[1L])

  date_end   <- date_start + weeks(54L)
  if (epiweek(date_end) != 1L) date_end <- date_end + weeks(1L)

  window     <- pred_df[pred_df$date >= date_start & pred_df$date <= date_end, ]
  pred_time  <- as.Date(window$date)
  xrange     <- range(pred_time)
  xrange[1L] <- xrange[1L] + weeks(1L)     # trim first step for display

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
#'   # --- one-time setup ---
#'   peak_mats <- build_peak_mats(SEAus.lag$`Week  51`, SEAus.lag$`Week   2`)
#'   y_max_all <- max(abs(unlist(peak_mats)), na.rm = TRUE)
#'
#'   # --- per-season loop ---
#'   for (i in c(2, 3, 5, 6, 15, 19)) {
#'     preds <- extract_season_preds(i, peak_mats)
#'     dates <- build_season_dates(i, pred_df, season_years)
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

  # ---- predictor draw order (fixed) ----
  preds_ord <- c("nino", "wtio", "etio", "tsa", "sam", "olr")
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
