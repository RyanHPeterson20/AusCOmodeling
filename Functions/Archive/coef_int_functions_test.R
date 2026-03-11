
# =============================================================================
# coef_int_functions.R
# Functions for coefficient-interaction plots.
# =============================================================================


# =============================================================================
# Parsing helpers
# =============================================================================

# parse_term()
# Classifies a single coefficient name and extracts its components.
#
# Recognised patterns:
#   "main"        var_lagNN
#   "quad"        I(var_lagNN^2)
#   "interaction" var1_lagNN:var2_lagMM
#   "other"       anything else (intercept, etc.) -- silently ignored downstream
parse_term <- function(term) {

  # quadratic: I(var_lagNN^2)
  m2  <- regexec("^I\\((.+)_lag([0-9]+)\\^2\\)$", term)
  mm2 <- regmatches(term, m2)[[1]]
  if (length(mm2) == 3) {
    base_term <- paste0(mm2[2], "_lag", mm2[3])
    return(list(kind = "quad", term = term, var = mm2[2],
                lag  = as.integer(mm2[3]), base_term = base_term))
  }

  # interaction: var_lagNN:var2_lagMM
  if (grepl(":", term, fixed = TRUE)) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    return(list(kind = "interaction", term = term,
                left_term = parts[1], right_term = parts[2]))
  }

  # main: var_lagNN
  m  <- regexec("^(.+)_lag([0-9]+)$", term)
  mm <- regmatches(term, m)[[1]]
  if (length(mm) == 3)
    return(list(kind = "main", term = term,
                var  = mm[2], lag = as.integer(mm[3])))

  list(kind = "other", term = term)
}


# coef_to_df()
# Converts a named coefficient vector to a tidy data frame for plotting.
coef_to_df <- function(coef_vec, model_name) {
  stopifnot(!is.null(names(coef_vec)))
  terms  <- names(coef_vec)
  parsed <- lapply(terms, parse_term)

  data.frame(
    model      = model_name,
    term       = terms,
    value      = as.numeric(coef_vec),
    kind       = vapply(parsed, `[[`, character(1), "kind"),
    var        = vapply(parsed,
                        function(z) if (!is.null(z$var))        z$var        else NA_character_,
                        character(1)),
    lag        = vapply(parsed,
                        function(z) if (!is.null(z$lag))        z$lag        else NA_integer_,
                        integer(1)),
    left_term  = vapply(parsed,
                        function(z) if (!is.null(z$left_term))  z$left_term  else NA_character_,
                        character(1)),
    right_term = vapply(parsed,
                        function(z) if (!is.null(z$right_term)) z$right_term else NA_character_,
                        character(1)),
    base_term  = vapply(parsed,
                        function(z) if (!is.null(z$base_term))  z$base_term  else NA_character_,
                        character(1)),
    stringsAsFactors = FALSE
  )
}


# Thin wrappers used throughout
alpha_col <- function(col, a = 0.6) adjustcolor(col, alpha.f = a)

pretty_var_label <- function(var) {
  switch(tolower(var),
    nino = "Ni\u00f1o 3.4",
    dmi  = "DMI",
    wtio = "WTIO",
    etio = "ETIO",
    tsa  = "TSA",
    aao  = "SAM",
    olr  = "OLR",
    toupper(var)
  )
}


# =============================================================================
# plot_lagged_coef_panels()
#
# Produces the multi-panel coefficient x lag figure with a right-hand
# interaction panel.  All cosmetic choices are exposed as parameters so
# each fire-season figure can be tuned without touching the function body.
#
# INPUT FORMAT
# ------------
# coefs_named_list : named list of coefficient vectors, one entry per model.
#   Names become the model keys used in every style parameter.
#   e.g. list(base = coef(fit_base), const = coef(fit_const), vary = coef(fit_vary))
#
# Coefficient name conventions (enforced by parse_term):
#   Main term    : <var>_lag<N>                e.g. nino_lag45
#   Interaction  : <var1>_lag<N>:<var2>_lag<M>  e.g. nino_lag45:etio_lag8
#   Quadratic    : I(<var>_lag<N>^2)            e.g. I(nino_lag45^2)
#   Other terms (intercept, etc.) are silently skipped.
#
# Variable key -> panel label (pretty_var_label):
#   nino -> Nino 3.4 | wtio -> WTIO | etio -> ETIO
#   tsa  -> TSA      | aao  -> SAM  | olr  -> OLR
# =============================================================================
plot_lagged_coef_panels_test <- function(

  ## ---- data ----------------------------------------------------------------
  coefs_named_list,

  ## ---- panel structure -----------------------------------------------------
  # Which variables get a left panel, and in what order.
  # Omit any variable that has no terms in the current model set.
  vars_order        = c("nino", "wtio", "etio", "tsa", "aao", "olr"),

  ## ---- point shapes --------------------------------------------------------
  # Named by variable key (lower-case).  OLR is special: pch=10 triggers the
  # two-layer filled-circle + crosshair rendering used in the by-hand figures.
  pch_map           = c(nino = 21, wtio = 24, etio = 25,
                         tsa  = 22, aao  = 23, olr  = 10),

  ## ---- axis ranges ---------------------------------------------------------
  xlim_lag          = c(1, 52),
  coef_range        = c(-5, 5),   # y-range for ALL left panels
  coef_range_int    = coef_range, # x-range for the right (interaction) panel

  ## ---- axis ticks ----------------------------------------------------------
  # x_axis_at   : tick positions on the lag axis (all left panels).
  #               Default matches the canonical by-hand figures.
  # y_axis_at   : tick positions on the coefficient axis (left panels).
  #               NULL = auto-computed from coef_range via pretty().
  # int_axis_at : tick positions on the x-axis of the right panel.
  #               NULL = R default auto ticks.
  x_axis_at         = seq(10, 50, 10),
  y_axis_at         = NULL,
  int_axis_at       = NULL,

  ## ---- titles & labels -----------------------------------------------------
  main_title        = NULL,
  cex_main          = 1.75,  # title cex
  title_line        = 1,     # line= for title() in panel 1;
                              # par(mar[3]) is auto-set >= title_line + 1.2
  cex_num           = 1.2,   # axis tick label cex
  cex_label         = 1.4,   # axis title cex ("Lag", "Coefficients")
  cex_subtitle      = 1.4,   # in-panel variable label cex

  ## ---- points & lines ------------------------------------------------------
  cex_pt            = 2.1,   # point cex in left panels
  cex_pt_int        = 2.0,   # point cex in right (interaction) panel
  line_width        = 2,

  ## ---- jitter parameters ---------------------------------------------------
  # quad_y_jitter : y-separation of quadratic lines from the same anchor.
  #                 First line at y=0, subsequent ones alternate +j, -j, +2j...
  # int_y_jitter  : y-separation of V-connector arms from the same anchor.
  #                 Computed per (model, anchor, interaction-term) triple so it
  #                 resets independently for each panel and each model.
  # int_x_jitter  : x-shift of the vertical bar per unique interaction term.
  #                 Horizontal arms always terminate at the shifted position so
  #                 the T-junction geometry stays intact.
  quad_y_jitter     = 0.10,
  int_y_jitter      = 0,
  int_x_jitter      = 0,

  ## ---- per-model x offsets -------------------------------------------------
  # Small left/right nudge in lag units so co-located points do not fully
  # overlap.  Named vector keyed by model name; missing keys default to 0.
  x_offsets         = c(base = -0.25, const = 0.25, vary = 0),

  ## ---- per-model colours & styles ------------------------------------------
  # model_cols    : named vector of primary colours, one per model.
  # model_bgs     : NULL = auto-derived (first model alpha 0.50, rest 0.65).
  #                 Supply an explicit named vector to override per-model.
  # model_outline : NULL = auto-derived (first model "grey4", rest "black").
  #                 Supply an explicit named vector to override per-model.
  # model_lty     : line type per model (1=solid, 2=dashed, 3=dotted...).
  #                 Missing keys default to 2.
  model_cols        = c(base  = "forestgreen",
                        const = "magenta4",
                        vary  = "darkorange2"),
  model_bgs         = NULL,
  model_outline     = NULL,
  model_lty         = c(base = 2, const = 2, vary = 2),

  ## ---- legends -------------------------------------------------------------
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

  # ---------------------------------------------------------------------------
  # 0.  Validation
  # ---------------------------------------------------------------------------
  stopifnot(is.list(coefs_named_list), length(coefs_named_list) >= 1)
  stopifnot(!is.null(names(coefs_named_list)))

  # ---------------------------------------------------------------------------
  # 1.  Resolve per-model style vectors
  #
  #     All lookups later use names(model_cols) so any key naming convention
  #     works as long as it is consistent across all named arguments.
  # ---------------------------------------------------------------------------
  mdl_keys <- names(model_cols)
  n_mdls   <- length(mdl_keys)

  if (is.null(model_bgs)) {
    # First model at alpha 0.50 (matches by-hand "base"); rest at 0.65.
    alphas    <- c(0.50, rep(0.65, n_mdls - 1))
    model_bgs <- setNames(
      mapply(alpha_col, unname(model_cols), alphas, SIMPLIFY = TRUE),
      mdl_keys
    )
  }

  if (is.null(model_outline)) {
    # First model "grey4" (matches by-hand "base"); rest "black".
    model_outline <- setNames(
      c("grey4", rep("black", n_mdls - 1)),
      mdl_keys
    )
  }

  # ---------------------------------------------------------------------------
  # 2.  Compute y-axis ticks for left panels if not supplied
  # ---------------------------------------------------------------------------
  if (is.null(y_axis_at)) {
    tks <- pretty(coef_range, n = 3)
    if (!0 %in% tks) tks <- sort(unique(c(tks, 0L)))
    y_axis_at <- tks
  }

  # ---------------------------------------------------------------------------
  # 3.  Parse coefficients into tidy data frames
  # ---------------------------------------------------------------------------
  df      <- do.call(rbind, Map(coef_to_df, coefs_named_list, names(coefs_named_list)))
  df_main <- df[df$kind == "main"        & !is.na(df$var) & !is.na(df$lag), , drop = FALSE]
  df_int  <- df[df$kind == "interaction",                                    , drop = FALSE]
  df_quad <- df[df$kind == "quad",                                           , drop = FALSE]

  # NDC anchors for main plotted points: keyed "model|term"
  anchors <- new.env(parent = emptyenv())

  # ---------------------------------------------------------------------------
  # 4.  Layout
  # ---------------------------------------------------------------------------
  n_left <- length(vars_order)
  mat    <- cbind(seq_len(n_left), rep(n_left + 1L, n_left))
  layout(mat, widths = c(1.75, 1.25), heights = rep(1, n_left))
  par(oma = c(1.05, 1.05, if (!is.null(main_title)) 0.75 else 0.5, 0.25))

  # ---------------------------------------------------------------------------
  # 5.  Left panels -- one per variable in vars_order
  # ---------------------------------------------------------------------------
  plot_one_var <- function(var, panel_i) {

    sub      <- df_main[tolower(df_main$var) == tolower(var), , drop = FALSE]
    is_first <- panel_i == 1L
    is_last  <- panel_i == n_left

    # Top margin: large enough for the title on panel 1.
    top_mar <- if (is_first && !is.null(main_title)) max(2.5, title_line + 1.2) else 1.0

    par(mar = c(if (is_last) 4.5 else 3.0,
                4.75,
                top_mar,
                1.0))

    # Blank canvas -- axes drawn manually for precise tick control,
    # exactly matching the axes=FALSE + axis() pattern in the by-hand script.
    plot(NA, NA,
         xlim    = xlim_lag,
         ylim    = coef_range,
         xlab    = if (is_last) "Lag" else "",
         ylab    = "",
         axes    = FALSE,
         cex.lab = cex_label)

    box()
    axis(1, at = x_axis_at, cex.axis = cex_num)
    axis(2, at = y_axis_at, cex.axis = cex_num)
    abline(h = 0, lty = 2, lwd = 1.5)

    # In-panel variable label -- positioned just below the top of the range.
    text(x      = if (tolower(var) == "nino") 4 else 3,
         y      = coef_range[2] - diff(coef_range) * 0.12,
         labels = pretty_var_label(var),
         adj    = 0,
         col    = "gray12",
         cex    = cex_subtitle)

    # Main title drawn inside panel 1 so it sits in the panel's own top margin
    # (adj=0, line=title_line).  This matches the by-hand exactly, avoids the
    # outer-margin adj hack, and works correctly at any layout width ratio.
    if (is_first && !is.null(main_title))
      title(main_title, adj = 0, cex.main = cex_main, line = title_line)

    if (nrow(sub) == 0) return(invisible(NULL))

    # ---- Draw each model's points ------------------------------------------
    for (model in names(coefs_named_list)) {

      s2 <- sub[sub$model == model, , drop = FALSE]
      if (nrow(s2) == 0) next

      col_link <- if (model %in% names(model_cols))    model_cols[[model]]    else "grey40"
      bg_col   <- if (model %in% names(model_bgs))     model_bgs[[model]]     else alpha_col(col_link, 0.5)
      out_col  <- if (model %in% names(model_outline)) model_outline[[model]] else "black"
      xoff     <- if (model %in% names(x_offsets))     x_offsets[[model]]     else 0

      pch_val <- pch_map[[tolower(var)]]
      if (is.null(pch_val)) pch_val <- 21L

      is_olr  <- tolower(var) == "olr"

      if (is_olr) {
        # Two-layer OLR rendering (matches by-hand exactly):
        #   Layer 1 -- filled circle (pch=19) in the model's bg colour.
        #   Layer 2 -- crosshair ring (pch=10) in the outline colour.
        # This gives a coloured filled circle with a visible crosshair overlay,
        # which is visually distinct from a plain pch=10.
        points(s2$lag + xoff, s2$value,
               pch = 19L, col = bg_col,  cex = cex_pt)
        points(s2$lag + xoff, s2$value,
               pch = 10L, col = out_col, cex = cex_pt)
      } else {
        points(s2$lag + xoff, s2$value,
               pch = pch_val,
               col = out_col,
               bg  = bg_col,
               cex = cex_pt)
      }

      # Store NDC coordinates for the cross-panel horizontal linking lines.
      for (k in seq_len(nrow(s2))) {
        key <- paste(model, s2$term[k], sep = "|")
        anchors[[key]] <- list(
          from_x = grconvertX(s2$lag[k] + xoff, from = "user", to = "ndc"),
          from_y = grconvertY(s2$value[k],        from = "user", to = "ndc")
        )
      }
    }

    invisible(NULL)
  }

  for (i in seq_along(vars_order)) plot_one_var(vars_order[i], i)

  # ---------------------------------------------------------------------------
  # 6.  Right panel -- interaction & quadratic coefficients
  # ---------------------------------------------------------------------------
  par(mar = c(4.5, 1.0, 2.5, 1.0))

  # yaxt="n" suppresses the y-axis; x-axis drawn by R (or int_axis_at if set).
  plot(0, 0, type = "n",
       xlim     = coef_range_int,
       ylim     = c(0, 1),
       xlab     = "Coefficients",
       ylab     = "",
       yaxt     = "n",
       cex.axis = cex_num,
       cex.lab  = cex_label)

  if (!is.null(int_axis_at))
    axis(1, at = int_axis_at, cex.axis = cex_num)

  abline(v = 0, lty = 2, lwd = 1.5)

  # Allow segments / points to extend outside this panel's plot region so
  # the horizontal linking lines can reach back into the left panels.
  par(xpd = NA)

  # ---- 6a.  Interaction V-connectors ----------------------------------------
  if (nrow(df_int) > 0) {

    # Pre-compute y-jitter offsets keyed by "model|anchor_term|iterm".
    # For each (model, anchor) pair, all interaction terms that touch that
    # anchor receive an evenly-spaced sequence centred at 0.  Resetting per
    # anchor AND per model ensures lines from different panels never share a
    # global offset and models never interfere with each other.
    int_y_off <- new.env(parent = emptyenv())
    for (mdl in names(coefs_named_list)) {
      mdl_ints <- df_int[df_int$model == mdl, , drop = FALSE]
      if (nrow(mdl_ints) == 0) next
      for (anch in unique(c(mdl_ints$left_term, mdl_ints$right_term))) {
        touching <- unique(mdl_ints$term[
          mdl_ints$left_term == anch | mdl_ints$right_term == anch
        ])
        n    <- length(touching)
        offs <- if (n > 1)
          seq(-(n - 1) / 2, (n - 1) / 2, length.out = n) * int_y_jitter
        else 0
        for (j in seq_along(touching))
          int_y_off[[paste(mdl, anch, touching[j], sep = "|")]] <- offs[j]
      }
    }

    # x-jitter: one symmetric offset per unique interaction term.
    unique_int_terms <- unique(df_int$term)
    n_int <- length(unique_int_terms)
    x_jitter_seq <- setNames(
      if (n_int > 1)
        seq(-(n_int - 1) / 2, (n_int - 1) / 2, length.out = n_int) * int_x_jitter
      else 0,
      unique_int_terms
    )

    for (r in seq_len(nrow(df_int))) {
      iterm <- df_int$term[r]
      lterm <- df_int$left_term[r]
      rterm <- df_int$right_term[r]
      xjoff <- x_jitter_seq[[iterm]]

      for (model in names(coefs_named_list)) {
        subm <- df_int[df_int$model == model & df_int$term == iterm, , drop = FALSE]
        if (nrow(subm) == 0) next

        k1 <- paste(model, lterm, sep = "|")
        k2 <- paste(model, rterm, sep = "|")
        if (is.null(anchors[[k1]]) || is.null(anchors[[k2]])) next

        a1 <- anchors[[k1]]
        a2 <- anchors[[k2]]

        # y-jitter: each arm looks up its own anchor's offset independently so
        # y1 and y2 can carry different displacements (avoids global cascade).
        y1_joff <- { ky <- paste(model, lterm, iterm, sep = "|")
                     if (!is.null(int_y_off[[ky]])) int_y_off[[ky]] else 0 }
        y2_joff <- { ky <- paste(model, rterm, iterm, sep = "|")
                     if (!is.null(int_y_off[[ky]])) int_y_off[[ky]] else 0 }

        # Convert NDC anchors to right-panel user coordinates.
        x1 <- grconvertX(a1$from_x, from = "ndc", to = "user")
        y1 <- grconvertY(a1$from_y, from = "ndc", to = "user") + y1_joff
        x2 <- grconvertX(a2$from_x, from = "ndc", to = "user")
        y2 <- grconvertY(a2$from_y, from = "ndc", to = "user") + y2_joff

        # xint_j: coefficient value shifted by x-jitter.
        # Horizontal arms terminate here so the T-junction stays intact.
        xint   <- subm$value[1]
        xint_j <- xint + xjoff

        col_link <- if (model %in% names(model_cols)) model_cols[[model]] else "grey40"
        lty_link <- if (model %in% names(model_lty))  model_lty[[model]]  else 2L

        # Horizontal arms from each left-panel anchor to the vertical bar.
        segments(x1, y1, xint_j, y1, col = col_link, lty = lty_link, lwd = line_width)
        segments(x2, y2, xint_j, y2, col = col_link, lty = lty_link, lwd = line_width)

        # Vertical bar of the V-connector.
        ymid <- (y1 + y2) / 2
        segments(xint_j, y1,   xint_j, ymid, col = col_link, lty = lty_link, lwd = line_width)
        segments(xint_j, y2,   xint_j, ymid, col = col_link, lty = lty_link, lwd = line_width)

        # Interaction midpoint marker (pch=11, six-pointed star).
        points(xint_j, ymid,
               pch = 11L,
               col = alpha_col(col_link, 0.99),
               bg  = alpha_col(col_link, 0.95),
               cex = cex_pt_int)
      }
    }
  }

  # ---- 6b.  Quadratic terms -------------------------------------------------
  if (nrow(df_quad) > 0) {

    # Pre-compute y-jitter offsets keyed by "model|base_term|qterm".
    # First term at offset 0; subsequent ones alternate +j, -j, +2j, -2j, ...
    # Resets independently per model.
    quad_y_off <- new.env(parent = emptyenv())
    for (mdl in names(coefs_named_list)) {
      mdl_quads <- df_quad[df_quad$model == mdl, , drop = FALSE]
      if (nrow(mdl_quads) == 0) next
      for (bt in unique(mdl_quads$base_term)) {
        qt_grp <- mdl_quads[mdl_quads$base_term == bt, , drop = FALSE]
        n      <- nrow(qt_grp)
        # Multiplier sequence: 0, +1, -1, +2, -2, ...
        mults  <- numeric(n)
        if (n > 1) for (j in 2:n)
          mults[j] <- if (j %% 2 == 0) j / 2 else -((j - 1L) / 2)
        offs <- mults * quad_y_jitter
        for (j in seq_len(n))
          quad_y_off[[paste(mdl, bt, qt_grp$term[j], sep = "|")]] <- offs[j]
      }
    }

    for (r in seq_len(nrow(df_quad))) {
      qterm <- df_quad$term[r]
      bterm <- df_quad$base_term[r]

      for (model in names(coefs_named_list)) {
        subm <- df_quad[df_quad$model == model & df_quad$term == qterm, , drop = FALSE]
        if (nrow(subm) == 0) next

        k <- paste(model, bterm, sep = "|")
        if (is.null(anchors[[k]])) next

        a      <- anchors[[k]]
        x_base <- grconvertX(a$from_x, from = "ndc", to = "user")
        y_base <- grconvertY(a$from_y, from = "ndc", to = "user")

        yoff_key <- paste(model, bterm, qterm, sep = "|")
        yoff     <- if (!is.null(quad_y_off[[yoff_key]])) quad_y_off[[yoff_key]] else 0
        yq       <- y_base + yoff
        xq       <- subm$value[1]

        col_link <- if (model %in% names(model_cols))    model_cols[[model]]    else "grey40"
        bg_col   <- if (model %in% names(model_bgs))     model_bgs[[model]]     else alpha_col(col_link, 0.5)
        out_col  <- if (model %in% names(model_outline)) model_outline[[model]] else "black"
        lty_link <- if (model %in% names(model_lty))     model_lty[[model]]     else 2L

        segments(x_base, yq, xq, yq, col = col_link, lty = lty_link, lwd = line_width)

        # Quadratic point: use the same pch as the base variable so the symbol
        # matches its left panel (by-hand: nino quad uses pch=21, aao uses
        # pch=23, etio uses pch=25, etc.).  Falls back to pch=25 if unknown.
        var_match <- df_main$var[df_main$term == bterm]
        pch_q     <- if (length(var_match) > 0) {
          pv <- pch_map[[tolower(var_match[1])]]
          if (!is.null(pv)) pv else 25L
        } else 25L

        # OLR quadratic: two-layer rendering consistent with the left panel.
        if (length(var_match) > 0 && tolower(var_match[1]) == "olr") {
          points(xq, yq, pch = 19L, col = bg_col,  cex = cex_pt_int)
          points(xq, yq, pch = 10L, col = out_col, cex = cex_pt_int)
        } else {
          points(xq, yq, pch = pch_q, col = out_col, bg = bg_col, cex = cex_pt_int)
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 7.  Legends (optional) -- drawn inside the right panel
  # ---------------------------------------------------------------------------
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

  # ---------------------------------------------------------------------------
  # 8.  Shared y-axis label for all left panels
  # ---------------------------------------------------------------------------
  mtext("Coefficients",
        side  = 2,
        outer = TRUE,
        line  = -0.5,
        cex   = 1.25)

  invisible(NULL)
}
