
# =============================================================================
# coef_int_plot.R
#
# Exports:
#   alpha_col()            thin adjustcolor() wrapper
#   pretty_var_label()     climate index key -> display label
#   parse_term()           classify a single coefficient name
#   coef_to_df()           named coef vector -> tidy data frame
#   build_model_styles()   pre-resolve all per-model visual properties
#   plot_lagged_coef_panels()  main multi-panel figure function
#
# Coefficient naming rules (enforced by parse_term):
#   Main term    :  <var>_lag<N>                  e.g.  nino_lag45
#   Interaction  :  <var1>_lag<N>:<var2>_lag<M>   e.g.  nino_lag45:etio_lag8
#   Quadratic    :  I(<var>_lag<N>^2)             e.g.  I(nino_lag45^2)
#   Other        :  anything else (intercept …)   silently skipped
#
# Variable key -> panel label (pretty_var_label):
#   nino -> Nino 3.4 | wtio -> WTIO | etio -> ETIO
#   tsa  -> TSA      | aao  -> SAM  | olr  -> OLR
# =============================================================================


# =============================================================================
# Utilities
# =============================================================================

#' Thin wrapper around adjustcolor for consistent alpha application.
alpha_col <- function(col, a = 0.6) adjustcolor(col, alpha.f = a)


#' Map a lower-case variable key to its display label.
pretty_var_label <- function(var) {
  switch(tolower(var),
    nino = "Ni\u00f1o 3.4",
    dmi  = "DMI",
    wtio = "WTIO",
    etio = "ETIO",
    tsa  = "TSA",
    aao  = "SAM",
    olr  = "OLR",
    toupper(var)   # fallback: upper-case the key as-is
  )
}


# =============================================================================
# Term parsers
# =============================================================================

#' Classify one coefficient name and extract its structural components.
#' Returns a list with at minimum element $kind = "main"|"quad"|"interaction"|"other".
parse_term <- function(term) {

  # Quadratic: I(var_lagNN^2)
  m2  <- regexec("^I\\((.+)_lag([0-9]+)\\^2\\)$", term)
  mm2 <- regmatches(term, m2)[[1]]
  if (length(mm2) == 3) {
    return(list(
      kind      = "quad",
      term      = term,
      var       = mm2[2],
      lag       = as.integer(mm2[3]),
      base_term = paste0(mm2[2], "_lag", mm2[3])
    ))
  }

  # Interaction: var1_lagNN:var2_lagMM
  if (grepl(":", term, fixed = TRUE)) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    return(list(
      kind       = "interaction",
      term       = term,
      left_term  = parts[1],
      right_term = parts[2]
    ))
  }

  # Main: var_lagNN
  m  <- regexec("^(.+)_lag([0-9]+)$", term)
  mm <- regmatches(term, m)[[1]]
  if (length(mm) == 3) {
    return(list(
      kind = "main",
      term = term,
      var  = mm[2],
      lag  = as.integer(mm[3])
    ))
  }

  list(kind = "other", term = term)
}


#' Convert a named coefficient vector into a tidy data frame for plotting.
#' One row per term; columns include kind, var, lag, left_term, right_term, base_term.
coef_to_df <- function(coef_vec, model_name) {
  stopifnot(!is.null(names(coef_vec)))
  terms  <- names(coef_vec)
  parsed <- lapply(terms, parse_term)

  # Helper: safely extract a field that may not exist in all list elements.
  safe_chr <- function(field)
    vapply(parsed, function(z) if (!is.null(z[[field]])) z[[field]] else NA_character_, character(1))
  safe_int <- function(field)
    vapply(parsed, function(z) if (!is.null(z[[field]])) z[[field]] else NA_integer_,   integer(1))

  data.frame(
    model      = model_name,
    term       = terms,
    value      = as.numeric(coef_vec),
    kind       = vapply(parsed, `[[`, character(1), "kind"),
    var        = safe_chr("var"),
    lag        = safe_int("lag"),
    left_term  = safe_chr("left_term"),
    right_term = safe_chr("right_term"),
    base_term  = safe_chr("base_term"),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Style builder
# =============================================================================

#' Pre-resolve all per-model visual properties into a named list of sublists.
#'
#' This eliminates the repeated if (model %in% names(x)) x[[model]] else default
#' pattern throughout the drawing code.  Every drawing step calls
#'   sty <- styles[[model]]
#' and then accesses sty$col, sty$bg, sty$outline, sty$lty, sty$xoff directly.
#'
#' @param model_cols    Named character vector of primary colours (required).
#' @param model_bgs     Named character vector of fill (bg) colours.
#'                      NULL -> auto: first model at alpha 0.50, rest at 0.65.
#' @param model_outline Named character vector of outline colours.
#'                      NULL -> auto: first model "grey4", rest "black".
#' @param model_lty     Named integer vector of line types (1=solid, 2=dashed…).
#'                      NULL -> auto: all dashed (2).
#' @param x_offsets     Named numeric vector of lag-axis nudges per model.
#'                      NULL -> auto: all zero.
#' @return Named list of per-model style sublists.
build_model_styles <- function(model_cols,
                                model_bgs     = NULL,
                                model_outline = NULL,
                                model_lty     = NULL,
                                x_offsets     = NULL) {
  keys <- names(model_cols)
  n    <- length(keys)
  stopifnot(n >= 1, !is.null(keys))

  # Auto-derive backgrounds
  if (is.null(model_bgs)) {
    alphas    <- c(0.50, rep(0.65, max(n - 1L, 0L)))
    model_bgs <- setNames(
      mapply(alpha_col, unname(model_cols), alphas, SIMPLIFY = TRUE),
      keys
    )
  }

  # Auto-derive outlines
  if (is.null(model_outline))
    model_outline <- setNames(c("grey4", rep("black", max(n - 1L, 0L))), keys)

  # Auto-derive line types
  if (is.null(model_lty))
    model_lty <- setNames(rep(2L, n), keys)

  # Auto-derive x-offsets
  if (is.null(x_offsets))
    x_offsets <- setNames(rep(0, n), keys)

  # Assemble: one sublist per model, every field guaranteed to exist
  setNames(lapply(keys, function(k) list(
    col     = unname(model_cols[[k]]),
    bg      = unname(if (k %in% names(model_bgs))     model_bgs[[k]]     else alpha_col(model_cols[[k]], 0.6)),
    outline = unname(if (k %in% names(model_outline)) model_outline[[k]] else "black"),
    lty     = unname(if (k %in% names(model_lty))     model_lty[[k]]     else 2L),
    xoff    = unname(if (k %in% names(x_offsets))     x_offsets[[k]]     else 0)
  )), keys)
}


# =============================================================================
# Jitter pre-computation helpers  (internal)
# =============================================================================

# Both helpers return a plain environment used as a named lookup table.
# Keys use "|" as separator: "model|anchor_term|iterm" or "model|base_term|qterm".

# Combined y-jitter for interaction arms and quadratic lines:
#
# Replaces the former separate .int_y_offsets and .quad_y_offsets helpers.
# The key insight is that both types of line depart from the same anchor point
# in the left panel and terminate somewhere in the right panel.  When a
# quadratic and an interaction share an anchor, the two systems must be aware
# of each other to avoid overlap.
#
# Algorithm (per model, per anchor):
#   1. Collect ALL departing lines: interaction arms + quadratic terms.
#   2. Assign a "direction index" to each:
#        Interaction arm  -> panel index of the OTHER side's variable in
#                            vars_order (lower index = visually higher = more
#                            positive offset, so the line runs upward without
#                            crossing lines running downward).
#        Quadratic line   -> Inf (treated as "below all variable panels",
#                            always placed at the most negative offset).
#   3. Sort by direction index ascending, break ties by term name for stability.
#   4. Assign offsets from +(n-1)/2 down to -(n-1)/2, scaled by the
#        appropriate jitter magnitude for each type
#        (int_y_jitter for interaction arms, quad_y_jitter for quad lines).
#   5. Single total departure: zero offset (preserves prior behaviour).
#
# Returns a named list with elements $int and $quad, each a plain environment
# with the same key formats used by draw_right_panel:
#   $int  keyed  "model|anchor_term|iterm"
#   $quad keyed  "model|base_term|qterm"
#
# The x-jitter helper (.int_x_offsets) is unchanged and kept separate.
.combined_y_offsets <- function(df_int, df_quad, model_keys,
                                 int_y_jitter, quad_y_jitter, vars_order) {
  int_env  <- new.env(parent = emptyenv())
  quad_env <- new.env(parent = emptyenv())

  # Panel index of the variable in a main-term string, or Inf if unknown.
  term_panel_idx <- function(t) {
    p <- parse_term(t)
    if (!identical(p$kind, "main")) return(Inf)
    idx <- match(tolower(p$var), tolower(vars_order))
    if (is.na(idx)) Inf else as.numeric(idx)
  }

  for (mdl in model_keys) {
    sub_int  <- df_int[df_int$model   == mdl, , drop = FALSE]
    sub_quad <- df_quad[df_quad$model == mdl, , drop = FALSE]

    # All anchor terms that appear in either data frame for this model.
    all_anchors <- unique(c(
      if (nrow(sub_int)  > 0) c(sub_int$left_term,  sub_int$right_term) else character(0),
      if (nrow(sub_quad) > 0) sub_quad$base_term                        else character(0)
    ))

    for (anch in all_anchors) {

      # Interaction terms touching this anchor.
      int_here <- if (nrow(sub_int) > 0)
        unique(sub_int$term[sub_int$left_term == anch | sub_int$right_term == anch])
      else character(0)

      # Quadratic terms anchored here (base_term == anch).
      quad_here <- if (nrow(sub_quad) > 0)
        unique(sub_quad$term[sub_quad$base_term == anch])
      else character(0)

      n_int   <- length(int_here)
      n_quad  <- length(quad_here)
      n_total <- n_int + n_quad
      if (n_total == 0L) next

      # Single departure: zero offset regardless of type.
      if (n_total == 1L) {
        if (n_int  == 1L) int_env[[paste(mdl,  anch, int_here[1L],  sep = "|")]] <- 0
        if (n_quad == 1L) quad_env[[paste(mdl, anch, quad_here[1L], sep = "|")]] <- 0
        next
      }

      # Build a flat table of all departing lines with their direction indices.
      items <- data.frame(
        kind = c(rep("int", n_int), rep("quad", n_quad)),
        term = c(int_here, quad_here),
        stringsAsFactors = FALSE
      )

      items$dir_idx <- vapply(seq_len(nrow(items)), function(i) {
        if (items$kind[i] == "int") {
          # Direction = panel index of the variable on the OTHER side of the ":"
          row <- sub_int[sub_int$term == items$term[i], , drop = FALSE]
          if (nrow(row) == 0L) return(Inf)
          other_term <- if (identical(row$left_term[1L], anch)) row$right_term[1L]
                        else                                     row$left_term[1L]
          term_panel_idx(other_term)
        } else {
          # Quadratic: use the anchor's own panel index as direction key.
          #
          # Rationale: the quadratic line is horizontal and has no inherent
          # direction in the right panel.  By giving it the index of its own
          # panel we place it between arms heading above the anchor (smaller
          # index, positive offset) and arms heading below (larger index,
          # negative offset).  With a single interaction arm this means the
          # quadratic always ends up on the OPPOSITE side: if the arm goes to a
          # panel below (dir > anchor) the quad sorts earlier and gets the
          # positive offset; if the arm goes to a panel above (dir < anchor) the
          # quad sorts later and gets the negative offset.
          term_panel_idx(anch)
        }
      }, numeric(1L))

      # Stable sort: direction index ascending, then term name for ties.
      ord   <- order(items$dir_idx, items$term)
      items <- items[ord, , drop = FALSE]

      # Offset multipliers: +(n-1)/2 down to -(n-1)/2 (top-connecting gets +).
      mults <- seq((n_total - 1L) / 2, -(n_total - 1L) / 2, length.out = n_total)

      for (i in seq_len(n_total)) {
        jitter  <- if (items$kind[i] == "int") int_y_jitter else quad_y_jitter
        if (items$kind[i] == "int") {
          int_env[[paste(mdl,  anch, items$term[i], sep = "|")]] <- mults[i] * jitter
        } else {
          quad_env[[paste(mdl, anch, items$term[i], sep = "|")]] <- mults[i] * jitter
        }
      }
    }
  }

  list(int = int_env, quad = quad_env)
}


# Interaction x-jitter:
# One symmetric offset per unique interaction term; returned as a named vector.
.int_x_offsets <- function(df_int, int_x_jitter) {
  terms <- unique(df_int$term)
  n     <- length(terms)
  setNames(
    if (n > 1L) seq(-(n - 1) / 2, (n - 1) / 2, length.out = n) * int_x_jitter else 0,
    terms
  )
}


# Auto interaction-coefficient x-jitter:
# Groups (model, iterm) pairs whose interaction coefficient values are within
# `tol` of each other (union-find), then assigns symmetric x nudges to every
# group of size > 1.  Groups of size 1 get no entry (lookup -> 0).
#
# This is deliberately separate from the manual int_x_jitter: that one spreads
# ALL interaction terms by a fixed amount; this one only moves terms that are
# actually nearly coincident in coefficient value, leaving well-separated terms
# untouched.
#
# Returns an environment keyed "model|iterm" -> numeric nudge value.
.auto_int_x_offsets <- function(df_int, model_keys, tol, nudge) {
  env <- new.env(parent = emptyenv())
  if (nudge == 0) return(env)

  # Collect one row per (model, iterm): the coefficient value for that pair.
  rows_list <- lapply(model_keys, function(mdl) {
    sub <- df_int[df_int$model == mdl, , drop = FALSE]
    if (nrow(sub) == 0L) return(NULL)
    terms <- unique(sub$term)
    vals  <- vapply(terms, function(t) sub$value[sub$term == t][1L], numeric(1L))
    data.frame(model = mdl, term = terms, value = vals, stringsAsFactors = FALSE)
  })
  rows <- do.call(rbind, rows_list[!vapply(rows_list, is.null, logical(1L))])
  if (is.null(rows) || nrow(rows) < 2L) return(env)

  n     <- nrow(rows)
  group <- seq_len(n)

  # Union-find: merge any pair whose coefficient values are within tol
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      if (abs(rows$value[i] - rows$value[j]) <= tol) {
        old_g             <- group[j]
        new_g             <- group[i]
        group[group == old_g] <- new_g
      }
    }
  }

  # Assign symmetric nudges within each overlapping group
  for (g in unique(group)) {
    idx <- which(group == g)
    if (length(idx) == 1L) next
    m       <- length(idx)
    offsets <- seq(-(m - 1) / 2, (m - 1) / 2, length.out = m) * nudge
    for (k in seq_along(idx))
      env[[paste(rows$model[idx[k]], rows$term[idx[k]], sep = "|")]] <- offsets[k]
  }
  env
}


# Overlap detection and nudge pre-computation:
# For each left panel, collects every (model, term) point with its effective
# x position (lag + per-model xoff).  Pairs of points whose effective-x
# distance is <= tol_x AND whose coefficient distance is <= tol_y are placed
# in the same overlap group (transitive union-find).  Each group of size > 1
# receives a symmetric sequence of x and/or y nudges centred at 0.
#
# Returns an environment keyed "model|term" -> list(dx, dy).
# Points with no overlap get no entry (lookup returns NULL -> nudge = 0).
.overlap_nudges <- function(df_main, model_keys, styles, vars_order,
                             tol_x, tol_y, nudge_x, nudge_y) {
  env <- new.env(parent = emptyenv())
  if (nudge_x == 0 && nudge_y == 0) return(env)

  for (var in vars_order) {
    sub <- df_main[tolower(df_main$var) == tolower(var), , drop = FALSE]
    if (nrow(sub) == 0L) next

    # Build a flat table of all points for this panel across all models
    pts_list <- lapply(model_keys, function(mdl) {
      s <- sub[sub$model == mdl, , drop = FALSE]
      if (nrow(s) == 0L) return(NULL)
      xoff <- styles[[mdl]]$xoff
      data.frame(model = mdl, term = s$term,
                 ex = s$lag + xoff, ey = s$value,
                 stringsAsFactors = FALSE)
    })
    pts <- do.call(rbind, pts_list[!vapply(pts_list, is.null, logical(1L))])
    if (is.null(pts) || nrow(pts) < 2L) next

    n     <- nrow(pts)
    group <- seq_len(n)

    # Union-find: merge any pair within tolerance
    for (i in seq_len(n - 1L)) {
      for (j in (i + 1L):n) {
        if (abs(pts$ex[i] - pts$ex[j]) <= tol_x &&
            abs(pts$ey[i] - pts$ey[j]) <= tol_y) {
          old_g             <- group[j]
          new_g             <- group[i]
          group[group == old_g] <- new_g
        }
      }
    }

    # Assign symmetric nudges within each group of size > 1
    for (g in unique(group)) {
      idx <- which(group == g)
      if (length(idx) == 1L) next
      m       <- length(idx)
      offsets <- seq(-(m - 1) / 2, (m - 1) / 2, length.out = m)
      for (k in seq_along(idx))
        env[[paste(pts$model[idx[k]], pts$term[idx[k]], sep = "|")]] <-
          list(dx = offsets[k] * nudge_x, dy = offsets[k] * nudge_y)
    }
  }
  env
}


# =============================================================================
# Model-legend drawing helper  (internal)
# =============================================================================

# Draws the Model legend, supporting multi-line labels.
#
# Usage: embed "\n" in any element of legend_models to split that entry across
# two (or more) lines.  The symbol (point + line segment) is centred at the
# midpoint y of all lines belonging to that entry.  Single-line entries fall
# through to a standard legend() call with no extra overhead.
#
# Geometry is recovered from two legend(plot=FALSE) calls:
#   - one WITH the title, to obtain the full bounding box and text positions
#   - one WITHOUT the title, to isolate how much vertical space the title
#     occupies so it can be placed accurately
#
# Symbol x-position derivation:
#   With R's defaults x.intersp=1 and seg.len=2, one character-width (cw):
#     symbol_left  = rect_left + 1*cw
#     symbol_right = rect_left + 3*cw    (1 + seg.len)
#     symbol_ctr   = rect_left + 2*cw
#     text_left    = rect_left + 4*cw    (1 + seg.len + 1)
#   Therefore: symbol_ctr = (rect_left + text_left) / 2
#
.draw_model_legend <- function(pos, inset, title_str, cex,
                                labels, pch_val, lty_vec, lwd_val,
                                cols, pt_cex) {

  # ---- split labels on \n and check for multi-line entries -------------------
  split_labels <- strsplit(labels, "\n", fixed = TRUE)
  n_lines      <- vapply(split_labels, length, integer(1L))

  if (!any(n_lines > 1L)) {
    # No multi-line entries: standard legend(), nothing extra needed.
    legend(pos,
           inset  = inset,
           title  = title_str,
           cex    = cex,
           legend = labels,
           pch    = pch_val,
           lty    = lty_vec,
           lwd    = lwd_val,
           col    = cols,
           pt.cex = pt_cex)
    return(invisible(NULL))
  }

  # ---- build flat label list -------------------------------------------------
  flat_labels <- unlist(split_labels)
  entry_idx   <- rep(seq_along(labels), times = n_lines)
  n_flat      <- length(flat_labels)

  # Repeat style vectors to match flat label count (first line of each entry
  # carries the real style; subsequent lines share the same entry colour).
  flat_cols <- cols[entry_idx]

  # ---- recover geometry with legend(plot=FALSE) ------------------------------
  # Pass real pch / lty so that R allocates the full symbol column width.
  lg_with <- legend(pos, inset = inset, title = title_str, cex = cex,
                    legend = flat_labels,
                    pch    = rep(pch_val, n_flat),
                    lty    = rep(lty_vec[1L], n_flat),
                    lwd    = lwd_val,
                    col    = flat_cols,
                    pt.cex = pt_cex,
                    plot   = FALSE)

  # Without title: used only to derive vertical space the title occupies.
  lg_no_title <- legend(pos, inset = inset, title = NULL, cex = cex,
                        legend = flat_labels,
                        pch    = rep(pch_val, n_flat),
                        lty    = rep(lty_vec[1L], n_flat),
                        lwd    = lwd_val,
                        col    = flat_cols,
                        pt.cex = pt_cex,
                        plot   = FALSE)

  # Bounding box corners
  box_left   <- lg_with$rect$left
  box_top    <- lg_with$rect$top
  box_right  <- box_left + lg_with$rect$w
  box_bottom <- box_top  - lg_with$rect$h

  # Symbol centre x: midpoint of box left edge and text left edge
  sym_x <- (box_left + lg_with$text$x[1L]) / 2

  # Segment half-width: derived from text_x and box_left
  # seg_half = cw (one character width) where text_x = box_left + 4*cw
  seg_half <- (lg_with$text$x[1L] - box_left) / 4

  # Title y: centre of the vertical space the title occupies at the top
  title_space <- lg_with$rect$h - lg_no_title$rect$h
  title_y     <- box_top - title_space / 2

  # Line height between consecutive flat entries
  line_h <- if (n_flat > 1L) abs(lg_with$text$y[1L] - lg_with$text$y[2L])
            else              abs(par("cxy")[2L]) * cex

  # ---- draw ------------------------------------------------------------------
  # Box
  rect(box_left, box_bottom, box_right, box_top,
       col = "white", border = "black")

  # Title
  if (!is.null(title_str) && nzchar(title_str))
    text(x      = (box_left + box_right) / 2,
         y      = title_y,
         labels = title_str,
         cex    = cex,
         font   = 1L,
         adj    = 0.5)

  # Text lines
  for (j in seq_len(n_flat))
    text(x      = lg_with$text$x[j],
         y      = lg_with$text$y[j],
         labels = flat_labels[j],
         cex    = cex,
         adj    = 0)

  # Symbols: one per original entry, centred at mean y of its lines
  for (i in seq_along(labels)) {
    rows  <- which(entry_idx == i)
    y_sym <- mean(lg_with$text$y[rows])

    # Line segment
    segments(sym_x - seg_half, y_sym,
             sym_x + seg_half, y_sym,
             col = cols[i], lty = lty_vec[i], lwd = lwd_val)

    # Point (bg set to col to cover both filled and open pch conventions)
    points(sym_x, y_sym,
           pch    = pch_val,
           col    = cols[i],
           bg     = cols[i],
           cex    = pt_cex)
  }

  invisible(NULL)
}


# =============================================================================
# Main function
# =============================================================================

#' Multi-panel coefficient x lag figure with cross-panel interaction diagram.
#'
#' Layout: N left panels (one per climate variable) showing coefficient vs lag,
#' plus one right panel showing interaction V-connectors and quadratic lines.
#' Horizontal dashed lines span from each left-panel point to the right panel,
#' connecting each term to its interaction/quadratic coefficient value.
#'
#' @param coefs_named_list  Named list of coefficient vectors; one entry per
#'   model.  Names become the model keys used in all style parameters.
#'   e.g. list(base = coef(fit1), const = coef(fit2), vary = coef(fit3))
#'
#' @param vars_order    Character vector of variable keys defining which left
#'   panels are drawn and in what order.  Omit variables with no terms.
#'
#' @param pch_map       Named integer vector mapping variable key -> point shape.
#'   OLR is special: pch = 10 triggers two-layer rendering (filled disc +
#'   crosshair overlay) matching the canonical by-hand figures.
#'
#' @param layout_widths Numeric vector c(left, right) of relative panel widths
#'   passed to layout().
#'
#' @param oma           Outer margins c(bottom, left, top, right).
#'   NULL = auto (slightly more top margin when main_title is set).
#'
#' @param xlim_lag      x-axis range for all left panels.
#' @param coef_range    y-axis range for all left panels.
#' @param coef_range_int x-axis range for the right interaction panel.
#'   Defaults to coef_range.
#'
#' @param x_axis_at     Tick positions on the lag axis (all left panels).
#' @param y_axis_at     Tick positions on the coefficient axis.
#'   NULL = auto from coef_range via pretty(), with 0 forced in.
#' @param int_axis_at   Tick positions on the right panel's x-axis.
#'   NULL = R's default automatic ticks.
#'
#' @param main_title    Figure title drawn at adj=0 in panel 1's top margin.
#'   NULL = no title.
#' @param cex_main      cex for main_title.
#' @param title_line    line= argument for title(); panel 1's top margin is
#'   auto-set to max(2.5, title_line + 1.2) when main_title is non-NULL.
#' @param cex_axis      cex.axis for axis tick labels.
#' @param cex_lab       cex.lab for axis titles ("Lag", "Coefficients").
#' @param cex_var_label cex for the in-panel variable label ("Nino 3.4" etc.).
#'
#' @param cex_pt        Point cex in left panels.
#' @param cex_pt_int    Point cex in the right interaction panel.
#'
#' @param lwd           Line width for linking lines and V-connectors.
#' @param lty_ref       Line type for h=0 / v=0 reference lines.
#' @param lwd_ref       Line width for reference lines.
#'
#' @param x_offsets     Named numeric vector of lag-axis nudges per model.
#'   Useful when two models share a lag so their points don't fully overlap.
#'   NULL = no offset.  Missing keys default to 0.
#'
#' @param model_cols    Named character vector of primary colours per model.
#'   Names must match the keys in coefs_named_list.
#' @param model_bgs     Named character vector of fill colours.
#'   NULL = auto: first model alpha 0.50, remaining models alpha 0.65.
#' @param model_outline Named character vector of outline colours.
#'   NULL = auto: first model "grey4", remaining "black".
#' @param model_lty     Named integer vector of line types per model.
#'   NULL = all dashed (2).  Differentiate models in greyscale.
#'
#' @param quad_y_jitter y-separation of quadratic lines from the same anchor.
#'   First line at y=0 (no offset); subsequent ones alternate +j, -j, +2j, ...
#' @param int_y_jitter  y-separation of V-connector arms from the same anchor.
#'   Offset is per (model, anchor, interaction term) so it resets independently
#'   for each panel and each model.
#' @param int_x_jitter  x-shift of the vertical bar per unique interaction term.
#'   Horizontal arms always terminate at the shifted x so the T-junction stays.
#'
#' @param add_legends   Logical; draw Terms and Model legends in right panel.
#' @param legend_inset_terms  inset= for the Terms legend.
#' @param legend_inset_model  inset= for the Model legend.
#' @param legend_cex_terms    cex for Terms legend.
#' @param legend_cex_model    cex for Model legend.
#' @param legend_pt_bg        pt.bg for Terms legend symbols.
#' @param legend_terms        Character vector of term labels.
#' @param legend_terms_pch    pch vector matching legend_terms.
#' @param legend_terms_pt_cex pt.cex vector matching legend_terms.
#' @param legend_models       Character vector of model labels.
#' @param legend_model_keys   Keys into model_cols for legend colour lookup.
plot_lagged_coef_panels <- function(

  # --- Data -------------------------------------------------------------------
  coefs_named_list,

  # --- Panel variables --------------------------------------------------------
  vars_order        = c("nino", "wtio", "etio", "tsa", "aao", "olr"),
  pch_map           = c(nino = 21, wtio = 24, etio = 25,
                         tsa  = 22, aao  = 23, olr  = 10),

  # --- Layout -----------------------------------------------------------------
  layout_widths     = c(1.75, 1.25),
  oma               = NULL,

  # --- Axis ranges ------------------------------------------------------------
  # xlim_lag is c(high, low) by default so lag 1 plots on the right and
  # lag 52 on the left — matching the reversed-axis convention.
  xlim_lag          = c(52, 1),
  coef_range        = c(-5, 5),
  coef_range_int    = coef_range,

  # --- Axis ticks -------------------------------------------------------------
  # Ticks run right-to-left to match the reversed lag axis.
  x_axis_at         = c(52, 40, 30, 20, 10, 1),
  y_axis_at         = NULL,
  # las style for the left-panel y-axis tick labels only.
  # 0 = parallel to axis (default), 1 = always horizontal, 2 = perpendicular,
  # 3 = always vertical.  Most useful values are 0 and 1.
  y_axis_las        = 0,
  int_axis_at       = NULL,
  # Draw unlabelled half-ticks at midpoints between labelled ticks.
  # Each axis is controlled independently.  Half-ticks are drawn at half the
  # standard tick length (tcl = -0.25).
  half_ticks_x      = FALSE,   # lag axis on all left panels
  half_ticks_y      = FALSE,   # coefficient axis on all left panels
  half_ticks_int    = FALSE,   # x-axis on the right interaction panel

  # --- Title & text -----------------------------------------------------------
  main_title        = NULL,
  cex_main          = 1.75,
  title_line        = 1,
  cex_axis          = 1.2,
  # cex_lab sets the default for all axis labels.  Override any individual
  # label by supplying the corresponding specific parameter.
  cex_lab           = 1.4,
  cex_lab_lag       = NULL,   # x-axis label "Lag" on the bottom left panel
  cex_lab_y         = NULL,   # outer-margin y-axis label (ylab_left)
  cex_lab_int       = NULL,   # x-axis label(s) on the right interaction panel
  cex_var_label     = 1.4,
  # Horizontal position of the in-panel variable label, as a percentage of the
  # lag-axis range inset from the LEFT visual edge (xlim_lag[1]).
  # 5 = 5 % inset, which keeps the label clear of the axis line.
  # Increase if a long label clips the y-axis; decrease to push it further left.
  var_label_pos     = 5,

  # --- Axis labels ------------------------------------------------------------
  # Text for the x-axis label on the bottom-most left panel.
  xlab_lag          = "Lag",
  # Text for the x-axis label on the right interaction panel AND the shared
  # y-axis label on the left panels (outer margin mtext).
  # e.g. set to "Main Coefficient" to distinguish from interaction panel.
  xlab_coef         = "Coefficients",
  # Optional second line for the right-panel x-axis label.
  # NULL (default) = single-line label using xlab_coef.
  # Any string     = drawn on a second line below xlab_coef via mtext().
  # Useful when xlab_coef is long and clips at larger cex_lab values.
  xlab_coef2        = NULL,
  # Gap in mtext line units between xlab_coef (line 1) and xlab_coef2 (line 2).
  # Increase to add more space between the two label lines; decrease to tighten.
  xlab_coef2_line_gap = 1.2,

  # --- Points -----------------------------------------------------------------
  cex_pt            = 2.1,
  cex_pt_int        = 2.0,

  # --- Lines ------------------------------------------------------------------
  lwd               = 2,
  lty_ref           = 2,
  lwd_ref           = 1.5,

  # --- Per-model x-offsets ----------------------------------------------------
  x_offsets         = NULL,

  # --- Per-model colours & styles ---------------------------------------------
  model_cols        = c(base  = "forestgreen",
                        const = "magenta4",
                        vary  = "darkorange2"),
  model_bgs         = NULL,
  model_outline     = NULL,
  model_lty         = NULL,

  # --- Jitter -----------------------------------------------------------------
  quad_y_jitter     = 0.10,
  int_y_jitter      = 0,
  int_x_jitter      = 0,

  # --- Auto-jitter for overlapping points -------------------------------------
  # When auto_jitter = TRUE, points from different models whose lag and
  # coefficient values fall within (auto_jitter_tol_x, auto_jitter_tol_y) of
  # each other are spread apart by a symmetric sequence of nudges.
  # auto_jitter_x / auto_jitter_y control the nudge step size (in user units).
  # The nudges are also applied to the NDC anchors so right-panel connectors
  # follow the nudged positions exactly.
  # Set both nudge values to 0 to disable entirely (same as auto_jitter=FALSE).
  auto_jitter       = FALSE,
  auto_jitter_x     = 0.4,
  auto_jitter_y     = 0.0,
  auto_jitter_tol_x = 1.5,
  auto_jitter_tol_y = 0.3,

  # --- Y-axis label for left panels -------------------------------------------
  # Controls the shared outer-margin y-axis label drawn beside the left panels.
  # NULL (default) -> falls back to xlab_coef (backward-compatible behaviour).
  # ""             -> label is suppressed entirely.
  # Any string     -> used verbatim as the label.
  ylab_left         = NULL,

  # --- Legends ----------------------------------------------------------------
  add_legends           = FALSE,
  legend_inset_terms    = c(0.000, 0.00),
  legend_inset_model    = c(0.000, 0.21),
  legend_cex_terms      = 2.25,
  legend_cex_model      = 2.0,
  legend_pt_bg          = alpha_col("gray32", 0.65),
  legend_terms          = c("Ni\u00f1o 3.4", "WTIO", "ETIO",
                             "TSA", "SAM", "OLR", "Interaction"),
  legend_terms_pch      = c(21, 24, 25, 22, 23, 10, 11),
  legend_terms_pt_cex   = c(2.25, 1.8, 1.8, 2.25, 2.25, 2.25, 1.8),
  legend_models         = c("All-Data", "Fixed-Selection", "Withheld-Season"),
  legend_model_keys     = c("base", "const", "vary"),
  # Point shape used in the Model legend.  Default 15 (filled square).
  # The model's line type (from model_lty / styles) is always shown alongside
  # the point, giving a type-"b" appearance that reflects both colour and lty.
  legend_model_pch      = 15L,
  # Position keywords for each legend box.  Accepts any value that R's
  # legend() recognises: "topright", "topleft", "bottomright", "bottomleft",
  # "top", "bottom", "left", "right", "center".
  legend_pos_terms      = "topright",
  legend_pos_model      = "topright",

  # --- Auto x-jitter for overlapping interaction coefficients -----------------
  # When auto_int_x_jitter = TRUE, (model, iterm) pairs whose interaction
  # coefficient values fall within auto_int_x_tol of each other are
  # automatically spread apart along the x-axis of the right panel.
  # The nudge propagates to the vertical bar and star marker automatically
  # because they are all drawn from the same xint_j value.
  auto_int_x_jitter     = FALSE,
  auto_int_x_nudge      = 0.10,
  auto_int_x_tol        = 0.15

) {

  # ===========================================================================
  # 1.  Validate
  # ===========================================================================
  stopifnot(
    is.list(coefs_named_list),
    length(coefs_named_list) >= 1L,
    !is.null(names(coefs_named_list))
  )

  model_keys <- names(coefs_named_list)

  # Resolve per-axis cex_lab values: specific parameter wins; falls back to
  # the shared cex_lab default so existing calls need no changes.
  .cex_lag <- if (!is.null(cex_lab_lag)) cex_lab_lag else cex_lab
  .cex_y   <- if (!is.null(cex_lab_y))   cex_lab_y   else cex_lab
  .cex_int <- if (!is.null(cex_lab_int)) cex_lab_int else cex_lab

  # ===========================================================================
  # 2.  Pre-resolve all per-model styles (single lookup, used everywhere)
  # ===========================================================================
  styles <- build_model_styles(
    model_cols    = model_cols,
    model_bgs     = model_bgs,
    model_outline = model_outline,
    model_lty     = model_lty,
    x_offsets     = x_offsets
  )

  # ===========================================================================
  # 3.  Auto-compute y-axis ticks when not supplied
  # ===========================================================================
  if (is.null(y_axis_at)) {
    tks       <- pretty(coef_range, n = 3L)
    if (!0 %in% tks) tks <- sort(unique(c(0, tks)))
    y_axis_at <- tks
  }

  # ===========================================================================
  # 4.  Parse all coefficients into tidy data frames
  # ===========================================================================
  df      <- do.call(rbind, Map(coef_to_df, coefs_named_list, model_keys))
  df_main <- df[df$kind == "main"        & !is.na(df$var) & !is.na(df$lag), , drop = FALSE]
  df_int  <- df[df$kind == "interaction",                                    , drop = FALSE]
  df_quad <- df[df$kind == "quad",                                           , drop = FALSE]

  # NDC anchor store: keyed "model|term".
  # Populated by draw_left_panel(); read by draw_right_panel().
  # Using an environment (mutable reference) so both closures share one object.
  anchors <- new.env(parent = emptyenv())

  # ===========================================================================
  # 5.  Pre-compute jitter offset tables (before any drawing starts)
  # ===========================================================================
  # .combined_y_offsets handles both interaction arms and quadratic lines in a
  # single unified pass so they are offset directionally relative to each other
  # when they share the same anchor point.
  combined_y <- .combined_y_offsets(df_int, df_quad, model_keys,
                                     int_y_jitter, quad_y_jitter, vars_order)
  int_y_off  <- combined_y$int
  quad_y_off <- combined_y$quad

  int_x_off  <- if (nrow(df_int) > 0) .int_x_offsets(df_int, int_x_jitter) else NULL

  # Auto interaction x-jitter: keyed "model|iterm" -> nudge value.
  # Only populated when auto_int_x_jitter = TRUE.
  auto_int_x_off <- if (isTRUE(auto_int_x_jitter) && nrow(df_int) > 0)
    .auto_int_x_offsets(df_int, model_keys,
                        tol   = auto_int_x_tol,
                        nudge = auto_int_x_nudge)
  else
    new.env(parent = emptyenv())

  # Overlap nudge table: keyed "model|term" -> list(dx, dy).
  # Only populated when auto_jitter = TRUE and at least one nudge size is > 0.
  overlap_off <- if (isTRUE(auto_jitter))
    .overlap_nudges(df_main, model_keys, styles, vars_order,
                    tol_x   = auto_jitter_tol_x,
                    tol_y   = auto_jitter_tol_y,
                    nudge_x = auto_jitter_x,
                    nudge_y = auto_jitter_y)
  else
    new.env(parent = emptyenv())

  # ===========================================================================
  # 6.  Layout
  # ===========================================================================
  n_left <- length(vars_order)

  # Layout matrix: panels 1..n_left on left, panel n_left+1 spans right.
  layout(
    cbind(seq_len(n_left), rep(n_left + 1L, n_left)),
    widths  = layout_widths,
    heights = rep(1, n_left)
  )

  if (is.null(oma))
    oma <- c(1.05, 1.25, if (!is.null(main_title)) 0.75 else 0.5, 0.25)
  par(oma = oma)

  # ===========================================================================
  # 7.  Inner closure: draw one left panel
  #
  #     Captures from enclosing environment: styles, anchors, df_main,
  #     pch_map, n_left, plus all axis/text/point parameters.
  # ===========================================================================
  draw_left_panel <- function(var, panel_i) {

    sub      <- df_main[tolower(df_main$var) == tolower(var), , drop = FALSE]
    is_first <- panel_i == 1L
    is_last  <- panel_i == n_left
    is_olr   <- tolower(var) == "olr"

    # Top margin: accommodate title above panel 1.
    top_mar <- if (is_first && !is.null(main_title)) max(2.5, title_line + 1.2) else 1.0

    par(mar = c(if (is_last) 4.5 else 3.0, 3.75, top_mar, 1.0))

    # --- Blank canvas with manual axes (matches by-hand axes=FALSE pattern) --
    plot(NA, NA,
         xlim    = xlim_lag,
         ylim    = coef_range,
         xlab    = "",
         ylab    = "",
         axes    = FALSE,
         cex.lab = .cex_lag)

    box()
    axis(1, at = x_axis_at, cex.axis = cex_axis)
    axis(2, at = y_axis_at, cex.axis = cex_axis, las = y_axis_las)
    # Lag axis label drawn via mtext() on the bottom panel so cex and position
    # are fully independent of the plot() machinery.  line = par("mgp")[1]
    # matches the default xlab placement position.
    if (is_last && nzchar(xlab_lag))
      mtext(xlab_lag, side = 1, line = par("mgp")[1], cex = .cex_lag)
    if (isTRUE(half_ticks_x) && length(x_axis_at) >= 2L)
      axis(1, at = (x_axis_at[-length(x_axis_at)] + x_axis_at[-1L]) / 2,
           labels = FALSE, tcl = -0.25)
    if (isTRUE(half_ticks_y) && length(y_axis_at) >= 2L)
      axis(2, at = (y_axis_at[-length(y_axis_at)] + y_axis_at[-1L]) / 2,
           labels = FALSE, tcl = -0.25)
    abline(h = 0, lty = lty_ref, lwd = lwd_ref)

    # In-panel variable label, just below the top of the y-range.
    # x_label sits var_label_pos % of the lag range inside from the LEFT visual
    # edge (xlim_lag[1]), so it stays correct on both normal and reversed axes.
    # adj = 0 left-aligns the text from that anchor point, so labels of any
    # length grow rightward rather than shifting the anchor.
    x_label <- xlim_lag[1] + (xlim_lag[2] - xlim_lag[1]) * (var_label_pos / 100)
    text(x      = x_label,
         y      = coef_range[2] - diff(coef_range) * 0.20,
         labels = pretty_var_label(var),
         adj    = 0,
         col    = "gray12",
         cex    = cex_var_label)

    # Main title inside panel 1's own top margin (adj=0, matches by-hand).
    if (is_first && !is.null(main_title))
      title(main_title, adj = 0, cex.main = cex_main, line = title_line, xpd = TRUE, outer = TRUE)

    if (nrow(sub) == 0L) return(invisible(NULL))

    # --- Draw each model's points and store NDC anchors ----------------------
    for (model in model_keys) {

      s2 <- sub[sub$model == model, , drop = FALSE]
      if (nrow(s2) == 0L) next

      sty     <- styles[[model]]    # all style fields pre-resolved
      pch_val <- pch_map[[tolower(var)]]
      if (is.null(pch_val)) pch_val <- 21L

      # Per-term overlap nudges (both zero when auto_jitter is off)
      ovlp_dx <- vapply(s2$term, function(t) {
        ov <- overlap_off[[paste(model, t, sep = "|")]]
        if (!is.null(ov)) ov$dx else 0
      }, numeric(1L))
      ovlp_dy <- vapply(s2$term, function(t) {
        ov <- overlap_off[[paste(model, t, sep = "|")]]
        if (!is.null(ov)) ov$dy else 0
      }, numeric(1L))

      x_pos <- s2$lag   + sty$xoff + ovlp_dx
      y_pos <- s2$value            + ovlp_dy

      if (is_olr) {
        # OLR two-layer rendering (matches by-hand exactly):
        #   Layer 1 : pch=19 filled disc   in the model's bg colour
        #   Layer 2 : pch=10 crosshair ring in the outline colour
        # Produces a coloured filled circle with a visible black crosshair.
        points(x_pos, y_pos, pch = 19L, col = sty$bg,      cex = cex_pt)
        points(x_pos, y_pos, pch = 10L, col = sty$outline, cex = cex_pt)
      } else {
        points(x_pos, y_pos,
               pch = pch_val, col = sty$outline, bg = sty$bg, cex = cex_pt)
      }

      # Store NDC coordinates for later cross-panel drawing.
      # grconvert must be called while this panel is still active.
      # The overlap nudge is baked in so right-panel connectors follow the
      # actual drawn position, not the un-nudged model coordinate.
      for (k in seq_len(nrow(s2))) {
        key            <- paste(model, s2$term[k], sep = "|")
        anchors[[key]] <- list(
          ndc_x = grconvertX(x_pos[k], from = "user", to = "ndc"),
          ndc_y = grconvertY(y_pos[k], from = "user", to = "ndc")
        )
      }
    }

    invisible(NULL)
  }

  # ===========================================================================
  # 8.  Inner closure: draw the right interaction panel
  #
  #     Reads from anchors (populated by draw_left_panel calls).
  #     Uses pre-computed jitter tables: int_y_off, int_x_off, quad_y_off.
  # ===========================================================================
  draw_right_panel <- function() {

    par(mar = c(4.5, 1.0, 2.5, 1.0))

    # axes = FALSE + explicit axis() matches the draw_left_panel pattern and
    # ensures int_axis_at fully controls the tick positions with no automatic
    # ticks drawn underneath.
    # xlab is set to "" when a two-line label is requested so we can draw both
    # lines precisely with mtext() below.
    use_two_line_lab <- !is.null(xlab_coef2) && nzchar(xlab_coef2)
    plot(0, 0, type = "n",
         xlim     = coef_range_int,
         ylim     = c(0, 1),
         xlab     = if (use_two_line_lab) "" else xlab_coef,
         ylab     = "",
         axes     = FALSE,
         cex.axis = cex_axis,
         cex.lab  = .cex_int)

    # Two-line x-axis label: line 1 at the normal position, line 2 one step
    # further out.  mtext() line= is in units of text line heights.
    if (use_two_line_lab) {
      mtext(xlab_coef,  side = 1, line = 2.8,                        cex = .cex_int)
      mtext(xlab_coef2, side = 1, line = 2.8 + xlab_coef2_line_gap,  cex = .cex_int)
    }

    box()
    # Draw x-axis: use int_axis_at when supplied, otherwise R default ticks.
    if (!is.null(int_axis_at))
      axis(1, at = int_axis_at, cex.axis = cex_axis)
    else
      axis(1, cex.axis = cex_axis)
    # Half-ticks on the interaction coefficient axis.
    if (isTRUE(half_ticks_int)) {
      int_tks <- if (!is.null(int_axis_at)) int_axis_at else axTicks(1)
      if (length(int_tks) >= 2L)
        axis(1, at = (int_tks[-length(int_tks)] + int_tks[-1L]) / 2,
             labels = FALSE, tcl = -0.25)
    }
    # y-axis is suppressed entirely (this panel has no y meaning).

    abline(v = 0, lty = lty_ref, lwd = lwd_ref)

    # Allow segments/points to extend outside the plot region so the
    # horizontal linking lines can reach back into the left panels.
    par(xpd = NA)

    # ---- Interaction V-connectors -------------------------------------------
    if (nrow(df_int) > 0L) {

      for (r in seq_len(nrow(df_int))) {
        iterm <- df_int$term[r]
        lterm <- df_int$left_term[r]
        rterm <- df_int$right_term[r]
        xjoff <- if (!is.null(int_x_off)) int_x_off[[iterm]] else 0

        for (model in model_keys) {
          subm <- df_int[df_int$model == model & df_int$term == iterm, , drop = FALSE]
          if (nrow(subm) == 0L) next

          k1 <- paste(model, lterm, sep = "|")
          k2 <- paste(model, rterm, sep = "|")
          if (is.null(anchors[[k1]]) || is.null(anchors[[k2]])) next

          a1 <- anchors[[k1]]
          a2 <- anchors[[k2]]

          # Per-anchor y-jitter: look up independently for each arm so that
          # y1 (from the left-term panel) and y2 (from the right-term panel)
          # can carry different displacements.  No global cascade.
          get_yoff <- function(anch, it) {
            if (is.null(int_y_off)) return(0)
            ky <- paste(model, anch, it, sep = "|")
            if (!is.null(int_y_off[[ky]])) int_y_off[[ky]] else 0
          }
          y1_joff <- get_yoff(lterm, iterm)
          y2_joff <- get_yoff(rterm, iterm)

          # Convert NDC anchors to right-panel user coordinates.
          x1 <- grconvertX(a1$ndc_x, from = "ndc", to = "user")
          y1 <- grconvertY(a1$ndc_y, from = "ndc", to = "user") + y1_joff
          x2 <- grconvertX(a2$ndc_x, from = "ndc", to = "user")
          y2 <- grconvertY(a2$ndc_y, from = "ndc", to = "user") + y2_joff

          # xint_j: coefficient value + manual x-jitter + auto x-jitter.
          # All four segments and the star point are drawn from xint_j so the
          # entire V-connector (horizontal arms, vertical bar, marker) shifts
          # together with no further changes needed.
          auto_xoff <- {
            k_auto <- paste(model, iterm, sep = "|")
            v <- auto_int_x_off[[k_auto]]
            if (!is.null(v)) v else 0
          }
          xint_j <- subm$value[1] + xjoff + auto_xoff

          sty <- styles[[model]]

          # Horizontal arms: from each left-panel anchor out to the vertical bar.
          segments(x1, y1, xint_j, y1, col = sty$col, lty = sty$lty, lwd = lwd)
          segments(x2, y2, xint_j, y2, col = sty$col, lty = sty$lty, lwd = lwd)

          # Vertical bar spanning y1 to y2, meeting at their midpoint.
          ymid <- (y1 + y2) / 2
          segments(xint_j, y1,   xint_j, ymid, col = sty$col, lty = sty$lty, lwd = lwd)
          segments(xint_j, y2,   xint_j, ymid, col = sty$col, lty = sty$lty, lwd = lwd)

          # Six-pointed star at the midpoint (interaction marker, pch=11).
          points(xint_j, ymid,
                 pch = 11L,
                 col = alpha_col(sty$col, 0.99),
                 bg  = alpha_col(sty$col, 0.95),
                 cex = cex_pt_int)
        }
      }
    }

    # ---- Quadratic terms ----------------------------------------------------
    if (nrow(df_quad) > 0L) {

      for (r in seq_len(nrow(df_quad))) {
        qterm <- df_quad$term[r]
        bterm <- df_quad$base_term[r]

        for (model in model_keys) {
          subm <- df_quad[df_quad$model == model & df_quad$term == qterm, , drop = FALSE]
          if (nrow(subm) == 0L) next

          k <- paste(model, bterm, sep = "|")
          if (is.null(anchors[[k]])) next

          a      <- anchors[[k]]
          x_base <- grconvertX(a$ndc_x, from = "ndc", to = "user")
          y_base <- grconvertY(a$ndc_y, from = "ndc", to = "user")

          # y-jitter for this quad term from its (model, base_term) group.
          yoff <- if (!is.null(quad_y_off)) {
            ky <- paste(model, bterm, qterm, sep = "|")
            if (!is.null(quad_y_off[[ky]])) quad_y_off[[ky]] else 0
          } else 0

          yq <- y_base + yoff
          xq <- subm$value[1]

          sty <- styles[[model]]

          # Horizontal line from base anchor to quad coefficient.
          segments(x_base, yq, xq, yq, col = sty$col, lty = sty$lty, lwd = lwd)

          # Quadratic point: use the same pch as its parent variable so the
          # symbol matches the left panel (nino quad -> pch=21, aao -> pch=23).
          # Falls back to pch=25 if the base term isn't in df_main.
          base_var  <- df_main$var[df_main$term == bterm]
          pch_q     <- if (length(base_var) > 0L) {
            pv <- pch_map[[tolower(base_var[1])]]
            if (!is.null(pv)) pv else 25L
          } else 25L

          # OLR quadratic: two-layer rendering consistent with left panels.
          if (length(base_var) > 0L && tolower(base_var[1]) == "olr") {
            points(xq, yq, pch = 19L, col = sty$bg,      cex = cex_pt_int)
            points(xq, yq, pch = 10L, col = sty$outline, cex = cex_pt_int)
          } else {
            points(xq, yq, pch = pch_q, col = sty$outline, bg = sty$bg, cex = cex_pt_int)
          }
        }
      }
    }

    invisible(NULL)
  }

  # ===========================================================================
  # 9.  Execute: left panels (fills anchors), then right panel (reads anchors)
  # ===========================================================================
  for (i in seq_along(vars_order))
    draw_left_panel(vars_order[i], i)

  draw_right_panel()

  # ===========================================================================
  # 10.  Legends (optional, drawn inside the right panel)
  # ===========================================================================
  if (isTRUE(add_legends)) {
    par(xpd = NA)

    legend(legend_pos_terms,
           inset   = legend_inset_terms,
           title   = "Terms",
           cex     = legend_cex_terms,
           legend  = legend_terms,
           pch     = legend_terms_pch,
           col     = "grey4",
           pt.bg   = legend_pt_bg,
           pt.cex  = legend_terms_pt_cex)

    .draw_model_legend(
      pos       = legend_pos_model,
      inset     = legend_inset_model,
      title_str = "Model",
      cex       = legend_cex_model,
      labels    = legend_models,
      pch_val   = legend_model_pch,
      lty_vec   = vapply(legend_model_keys,
                         function(k) as.integer(
                           if (!is.null(styles[[k]])) styles[[k]]$lty else 2L),
                         integer(1L)),
      lwd_val   = lwd,
      cols      = unname(model_cols[legend_model_keys]),
      pt_cex    = 2.25)
  }

  # ===========================================================================
  # 11.  Shared y-axis label spanning all left panels (outer margin)
  #      ylab_left controls the text independently of xlab_coef.
  #      NULL  -> fall back to xlab_coef (backward-compatible).
  #      ""    -> suppressed entirely.
  #      other -> used verbatim.
  # ===========================================================================
  yl <- if (is.null(ylab_left)) xlab_coef else ylab_left
  if (nzchar(yl))
    mtext(yl, side = 2, outer = TRUE, line = -0.5, cex = .cex_y)

  invisible(NULL)
}
