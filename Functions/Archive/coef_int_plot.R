
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

# Interaction y-jitter:
# For each (model, anchor) pair, all interaction terms that touch that anchor
# receive a symmetric sequence centred at 0.  Resetting per anchor AND per
# model prevents cascading: lines from different panels or models are
# completely independent of each other.
.int_y_offsets <- function(df_int, model_keys, int_y_jitter) {
  env <- new.env(parent = emptyenv())
  for (mdl in model_keys) {
    sub <- df_int[df_int$model == mdl, , drop = FALSE]
    if (nrow(sub) == 0) next
    for (anch in unique(c(sub$left_term, sub$right_term))) {
      touching <- unique(sub$term[sub$left_term == anch | sub$right_term == anch])
      n        <- length(touching)
      offs     <- if (n > 1L)
        seq(-(n - 1) / 2, (n - 1) / 2, length.out = n) * int_y_jitter
      else 0
      for (j in seq_along(touching))
        env[[paste(mdl, anch, touching[j], sep = "|")]] <- offs[j]
    }
  }
  env
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

# Quadratic y-jitter:
# Per (model, base_term): first term at offset 0, subsequent terms alternate
# +j, -j, +2j, -2j, …  Resets independently per model.
.quad_y_offsets <- function(df_quad, model_keys, quad_y_jitter) {
  env <- new.env(parent = emptyenv())
  for (mdl in model_keys) {
    sub <- df_quad[df_quad$model == mdl, , drop = FALSE]
    if (nrow(sub) == 0) next
    for (bt in unique(sub$base_term)) {
      grp    <- sub[sub$base_term == bt, , drop = FALSE]
      n      <- nrow(grp)
      mults  <- numeric(n)
      # 0, +1, -1, +2, -2, …
      if (n > 1L) for (j in 2:n)
        mults[j] <- if (j %% 2 == 0) j / 2 else -((j - 1L) / 2)
      offs <- mults * quad_y_jitter
      for (j in seq_len(n))
        env[[paste(mdl, bt, grp$term[j], sep = "|")]] <- offs[j]
    }
  }
  env
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
  int_axis_at       = NULL,

  # --- Title & text -----------------------------------------------------------
  main_title        = NULL,
  cex_main          = 1.75,
  title_line        = 1,
  cex_axis          = 1.2,
  cex_lab           = 1.4,
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
  legend_model_keys     = c("base", "const", "vary")

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
  int_y_off  <- if (nrow(df_int)  > 0) .int_y_offsets(df_int,  model_keys, int_y_jitter)  else NULL
  int_x_off  <- if (nrow(df_int)  > 0) .int_x_offsets(df_int,  int_x_jitter)              else NULL
  quad_y_off <- if (nrow(df_quad) > 0) .quad_y_offsets(df_quad, model_keys, quad_y_jitter) else NULL

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
    oma <- c(1.05, 1.05, if (!is.null(main_title)) 0.75 else 0.5, 0.25)
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

    par(mar = c(if (is_last) 4.5 else 3.0, 4.75, top_mar, 1.0))

    # --- Blank canvas with manual axes (matches by-hand axes=FALSE pattern) --
    plot(NA, NA,
         xlim    = xlim_lag,
         ylim    = coef_range,
         xlab    = if (is_last) xlab_lag else "",
         ylab    = "",
         axes    = FALSE,
         cex.lab = cex_lab)

    box()
    axis(1, at = x_axis_at, cex.axis = cex_axis)
    axis(2, at = y_axis_at, cex.axis = cex_axis)
    abline(h = 0, lty = lty_ref, lwd = lwd_ref)

    # In-panel variable label, just below the top of the y-range.
    # x_label sits var_label_pos % of the lag range inside from the LEFT visual
    # edge (xlim_lag[1]), so it stays correct on both normal and reversed axes.
    # adj = 0 left-aligns the text from that anchor point, so labels of any
    # length grow rightward rather than shifting the anchor.
    x_label <- xlim_lag[1] + (xlim_lag[2] - xlim_lag[1]) * (var_label_pos / 100)
    text(x      = x_label,
         y      = coef_range[2] - diff(coef_range) * 0.12,
         labels = pretty_var_label(var),
         adj    = 0,
         col    = "gray12",
         cex    = cex_var_label)

    # Main title inside panel 1's own top margin (adj=0, matches by-hand).
    if (is_first && !is.null(main_title))
      title(main_title, adj = 0, cex.main = cex_main, line = title_line)

    if (nrow(sub) == 0L) return(invisible(NULL))

    # --- Draw each model's points and store NDC anchors ----------------------
    for (model in model_keys) {

      s2 <- sub[sub$model == model, , drop = FALSE]
      if (nrow(s2) == 0L) next

      sty     <- styles[[model]]    # all style fields pre-resolved
      pch_val <- pch_map[[tolower(var)]]
      if (is.null(pch_val)) pch_val <- 21L

      if (is_olr) {
        # OLR two-layer rendering (matches by-hand exactly):
        #   Layer 1 : pch=19 filled disc   in the model's bg colour
        #   Layer 2 : pch=10 crosshair ring in the outline colour
        # Produces a coloured filled circle with a visible black crosshair.
        points(s2$lag + sty$xoff, s2$value, pch = 19L, col = sty$bg,      cex = cex_pt)
        points(s2$lag + sty$xoff, s2$value, pch = 10L, col = sty$outline, cex = cex_pt)
      } else {
        points(s2$lag + sty$xoff, s2$value,
               pch = pch_val, col = sty$outline, bg = sty$bg, cex = cex_pt)
      }

      # Store NDC coordinates for later cross-panel drawing.
      # grconvert must be called while this panel is still active.
      for (k in seq_len(nrow(s2))) {
        key         <- paste(model, s2$term[k], sep = "|")
        anchors[[key]] <- list(
          ndc_x = grconvertX(s2$lag[k] + sty$xoff, from = "user", to = "ndc"),
          ndc_y = grconvertY(s2$value[k],            from = "user", to = "ndc")
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

    # yaxt="n" suppresses the y-axis entirely (this panel has no y meaning).
    plot(0, 0, type = "n",
         xlim     = coef_range_int,
         ylim     = c(0, 1),
         xlab     = xlab_coef,
         ylab     = "",
         yaxt     = "n",
         cex.axis = cex_axis,
         cex.lab  = cex_lab)

    if (!is.null(int_axis_at))
      axis(1, at = int_axis_at, cex.axis = cex_axis)

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

          # xint_j: coefficient value + x-jitter.
          # Horizontal arms terminate here so the T-junction remains exact.
          xint_j <- subm$value[1] + xjoff

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

    legend("topright",
           inset   = legend_inset_terms,
           title   = "Terms",
           cex     = legend_cex_terms,
           legend  = legend_terms,
           pch     = legend_terms_pch,
           col     = "grey4",
           pt.bg   = legend_pt_bg,
           pt.cex  = legend_terms_pt_cex)

    legend("topright",
           inset   = legend_inset_model,
           title   = "Model",
           cex     = legend_cex_model,
           legend  = legend_models,
           pch     = 15L,
           col     = unname(model_cols[legend_model_keys]),
           pt.cex  = 2.25)
  }

  # ===========================================================================
  # 11.  Shared y-axis label spanning all left panels (outer margin)
  # ===========================================================================
  mtext(xlab_coef, side = 2, outer = TRUE, line = -0.5, cex = 1.25)

  invisible(NULL)
}
