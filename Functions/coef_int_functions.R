
#functions for model outputs, as table or coef-int plots

#TODO: rename this for model output function so we know that the table functions are here too. 

## model table output functions
#TODO: add in checks for the necessary libraries (maybe grid and gridExtra? double check this.)
#TODO: add in some functionality to select for model metrics, such as R^2, adj. R^2, RMSE, etc. 
#function setup
lm_card_grob <- function(fit,
                         border = "#4C78A8",
                         fill   = "#E8F1FB",
                         title  = "Est (Std. Error)",
                         digits = 1,
                         fontfamily = "mono") {
  sm <- summary(fit)
  co <- sm$coefficients
  
  # Format coefficient rows
  term <- rownames(co)
  est  <- round(co[, "Estimate"], digits)
  se   <- round(co[, "Std. Error"], digits)
  
  # Make fixed-width lines (monospace) that line up nicely
  term_w <- max(nchar(term))
  est_w  <- max(nchar(format(est, trim = TRUE)))
  lines <- sprintf(
    paste0("%-", term_w, "s  %", est_w, "s (%s)"),
    term,
    format(est, trim = TRUE),
    format(se, trim = TRUE)
  )
  
  # Footer stats
  ## temp footer: (change later)
  ar2     <- sm$adj.r.squared
  nterms  <- nrow(co)
  
  footer <- c(
    "",
    sprintf("Adjusted R-squared: %.2f", ar2),
    sprintf("Number of terms: %d", nterms)
  )
  
  # Assemble full text block
  text_block <- paste(c(title, lines, footer), collapse = "\n")
  
  grobTree(
    rectGrob(gp = gpar(col = border, fill = fill, lwd = 3)),
    textGrob(
      text_block,
      x = unit(0.04, "npc"), y = unit(0.96, "npc"),
      just = c("left", "top"),
      gp = gpar(fontfamily = fontfamily, fontsize = 11, col = "black")
    )
  )
}  


## coefficient interaction plots functions
# ---- parsing helpers ----
parse_term <- function(term) {
  
  # quadratic: I(var_lagNN^2)
  m2 <- regexec("^I\\((.+)_lag([0-9]+)\\^2\\)$", term)
  mm2 <- regmatches(term, m2)[[1]]
  if (length(mm2) == 3) {
    base_term <- paste0(mm2[2], "_lag", mm2[3])
    return(list(
      kind = "quad",
      term = term,
      var = mm2[2],
      lag = as.integer(mm2[3]),
      base_term = base_term
    ))
  }
  
  # interaction: var_lagNN:var2_lagMM
  if (grepl(":", term, fixed = TRUE)) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    return(list(kind="interaction", term=term, left_term=parts[1], right_term=parts[2]))
  }
  
  # main: var_lagNN
  m <- regexec("^(.+)_lag([0-9]+)$", term)
  mm <- regmatches(term, m)[[1]]
  if (length(mm) == 3) return(list(kind="main", term=term, var=mm[2], lag=as.integer(mm[3])))
  
  list(kind="other", term=term)
}

coef_to_df <- function(coef_vec, model_name) {
  stopifnot(!is.null(names(coef_vec)))
  terms <- names(coef_vec)
  parsed <- lapply(terms, parse_term)
  
  data.frame(
    model = model_name,
    term  = terms,
    value = as.numeric(coef_vec),
    
    kind  = vapply(parsed, `[[`, character(1), "kind"),
    var   = vapply(parsed, function(z) if (!is.null(z$var)) z$var else NA_character_, character(1)),
    lag   = vapply(parsed, function(z) if (!is.null(z$lag)) z$lag else NA_integer_, integer(1)),
    
    left_term  = vapply(parsed, function(z) if (!is.null(z$left_term)) z$left_term else NA_character_, character(1)),
    right_term = vapply(parsed, function(z) if (!is.null(z$right_term)) z$right_term else NA_character_, character(1)),
    
    base_term  = vapply(parsed, function(z) if (!is.null(z$base_term)) z$base_term else NA_character_, character(1)),
    
    stringsAsFactors = FALSE
  )
}

alpha_col <- function(col, a=0.6) adjustcolor(col, alpha.f=a)

pretty_var_label <- function(var) {
  switch(tolower(var),
         nino = "Ni\u00f1o 3.4",
         dmi = "DMI",
         wtio = "WTIO",
         etio = "ETIO",
         tsa  = "TSA",
         aao  = "SAM",
         olr  = "OLR",
         toupper(var))
}


# ---- FIXED: uses ONLY base graphics in interaction panel for perfect alignment ----
plot_lagged_coef_panels <- function(coefs_named_list,
                                    vars_order = c("nino","wtio","etio","tsa","aao","olr"),
                                    pch_map = c(nino=21, wtio=24, etio=25, tsa=22, aao=23, olr=10),
                                    xlim_lag = c(1,52),
                                    coef_range = c(-5,5),
                                    main_title = NULL,
                                    cex_num = 1.2,
                                    cex_label = 1.4,
                                    cex_subtitle = 1.4,
                                    line_width = 2,
                                    quad_y_jitter = 0.10,
                                    x_offsets = c(base = 0, const = 0, vary=0),
                                    model_cols = c(base="forestgreen", const="magenta4", vary="darkorange2"),
                                    add_legends = FALSE,
                                    legend_inset_terms = c(0.000, 0.10),
                                    legend_inset_model = c(0.00, 0.00),
                                    legend_cex_terms = 2.25,
                                    legend_cex_model = 2.0,
                                    legend_pt_bg = alpha_col("gray32", 0.65),
                                    legend_terms = c("Ni\u00f1o 3.4", "WTIO", "ETIO", "TSA", "SAM", "OLR", "Interaction"),
                                    legend_terms_pch = c(21, 24, 25, 22, 23, 10, 11),
                                    legend_terms_pt_cex = c(2.25, 1.8, 1.8, 2.25, 2.25, 2.25, 1.8),
                                    legend_models = c("All-Data", "Fixed-Selection", "Withheld-Season"),
                                    legend_model_keys = c("base", "const", "vary")) {
  
  stopifnot(is.list(coefs_named_list), length(coefs_named_list) >= 1)
  stopifnot(!is.null(names(coefs_named_list)))
  
  # styles per model
  model_bgs  <- c(base=alpha_col(model_cols[1], 0.5),
                  const=alpha_col(model_cols[2], 0.65),
                  vary=alpha_col(model_cols[3], 0.65))
  model_outline <- c(base="grey4", const="black", vary="black")
  #x_offsets <- c(base = -0.25, const = 0.25, vary=0)
  
  # pch by variable (add olr)
  #pch_map <- c(nino=21, wtio=24, etio=25, tsa=22, aao=23, olr=10)
  
  # combine into one df
  df <- do.call(rbind, Map(coef_to_df, coefs_named_list, names(coefs_named_list)))
  df_main <- df[df$kind=="main" & !is.na(df$var) & !is.na(df$lag), , drop=FALSE]
  df_int  <- df[df$kind=="interaction", , drop=FALSE]
  df_quad <- df[df$kind=="quad", , drop=FALSE]
  
  # anchors in NDC keyed by "model|term" for MAIN plotted points
  anchors <- new.env(parent=emptyenv())
  
  # layout: N left panels + right panel spanning rows
  n_left <- length(vars_order)
  mat <- cbind(seq_len(n_left), rep(n_left + 1, n_left))
  layout(mat, widths=c(1.75, 1.25), heights=rep(1, n_left))
  par(oma=c(1.25, 1.25, 1.25, 0.25))
  
  plot_one_var <- function(var, panel_i) {
    sub <- df_main[tolower(df_main$var)==tolower(var), , drop=FALSE]
    
    par(mar=c(if (panel_i==n_left) 4.5 else 3, 4.75,
              if (panel_i==1) 2.5 else 1, 1))
    
    plot(NA, NA, xlim=xlim_lag, ylim=coef_range,
         xlab=if (panel_i==n_left) "Lag" else "",
         ylab= "",
         cex.axis=cex_num, cex.lab=cex_label)
    
    abline(h=0, lty=2, lwd=1.5)
    text(x=3, y=coef_range[2]-0.75, labels=pretty_var_label(var),
         col="gray12", cex=cex_subtitle)
    
    if (panel_i==1 && !is.null(main_title)) {
      title(main_title, adj=0, cex.main=2.0, line=1)
    }
    
    if (nrow(sub)==0) return(invisible(NULL))
    
    for (model in names(coefs_named_list)) {
      s2 <- sub[sub$model==model, , drop=FALSE]
      if (nrow(s2)==0) next
      
      # default styling if user supplies other model names
      col_link <- if (model %in% names(model_cols)) model_cols[[model]] else "grey40"
      bg_col   <- if (model %in% names(model_bgs))  model_bgs[[model]]  else alpha_col(col_link, 0.5)
      out_col  <- if (model %in% names(model_outline)) model_outline[[model]] else "black"
      xoff     <- if (model %in% names(x_offsets)) x_offsets[[model]] else 0
      
      pch_val <- pch_map[[tolower(var)]]
      if (is.null(pch_val)) pch_val <- 21
      
      points(s2$lag + xoff, s2$value,
             pch=pch_val,
             col= if (tolower(var)=="olr") bg_col else out_col,
             bg=bg_col,
             cex=2.1)
      
      for (k in seq_len(nrow(s2))) {
        key <- paste(model, s2$term[k], sep="|")
        anchors[[key]] <- list(
          from_x = grconvertX(s2$lag[k] + xoff, from="user", to="ndc"),
          from_y = grconvertY(s2$value[k],      from="user", to="ndc")
        )
      }
    }
    invisible(NULL)
  }
  
  for (i in seq_along(vars_order)) plot_one_var(vars_order[i], i)
  
  # ---- right panel: interactions + quadratic terms ----
  par(mar=c(4.5, 1, 2.5, 1))
  plot(0,0,type="n", ylim=c(0,1), xlim=coef_range,
       yaxt="n", ylab="", xlab="Coefficients",
       cex.axis=cex_num, cex.lab=cex_label)
  abline(v=0, lty=2, lwd=1.5)
  
  # allow drawing beyond plot region like your example
  par(xpd = NA)
  
  # --- draw interactions (V connectors + PERFECTLY ALIGNED horizontals) ---
  if (nrow(df_int) > 0) {
    for (r in seq_len(nrow(df_int))) {
      iterm <- df_int$term[r]
      lterm <- df_int$left_term[r]
      rterm <- df_int$right_term[r]
      
      for (model in names(coefs_named_list)) {
        subm <- df_int[df_int$model==model & df_int$term==iterm, , drop=FALSE]
        if (nrow(subm)==0) next
        
        k1 <- paste(model, lterm, sep="|")
        k2 <- paste(model, rterm, sep="|")
        if (is.null(anchors[[k1]]) || is.null(anchors[[k2]])) next
        
        a1 <- anchors[[k1]]
        a2 <- anchors[[k2]]
        
        # convert anchors (ndc) into THIS panel's user coords
        x1 <- grconvertX(a1$from_x, from="ndc", to="user")
        y1 <- grconvertY(a1$from_y, from="ndc", to="user")
        x2 <- grconvertX(a2$from_x, from="ndc", to="user")
        y2 <- grconvertY(a2$from_y, from="ndc", to="user")
        
        xint <- subm$value[1]
        
        col_link <- if (model %in% names(model_cols)) model_cols[[model]] else "grey40"
        
        # horizontals that align with the V connectors
        segments(x1, y1, xint, y1, col=col_link, lty=2, lwd=line_width)
        segments(x2, y2, xint, y2, col=col_link, lty=2, lwd=line_width)
        
        # V connector
        ymid <- (y1 + y2) / 2
        segments(xint, y1, xint, ymid, col=col_link, lty=2, lwd=line_width)
        segments(xint, y2, xint, ymid, col=col_link, lty=2, lwd=line_width)
        
        # interaction point
        points(xint, ymid, pch=11,
               col=alpha_col(col_link, 0.99),
               bg =alpha_col(col_link, 0.95),
               cex=2)
      }
    }
  }
  
  # --- draw quadratic terms (single-anchor horizontal + point) ---
  if (nrow(df_quad) > 0) {
    jitter_sign <- rep(c(1, -1), length.out = nrow(df_quad))
    
    for (r in seq_len(nrow(df_quad))) {
      qterm <- df_quad$term[r]
      bterm <- df_quad$base_term[r]
      
      for (model in names(coefs_named_list)) {
        subm <- df_quad[df_quad$model==model & df_quad$term==qterm, , drop=FALSE]
        if (nrow(subm)==0) next
        
        k <- paste(model, bterm, sep="|")
        if (is.null(anchors[[k]])) next
        
        a <- anchors[[k]]
        
        x_base <- grconvertX(a$from_x, from="ndc", to="user")
        y_base <- grconvertY(a$from_y, from="ndc", to="user")
        
        yq <- y_base + jitter_sign[r] * quad_y_jitter
        xq <- subm$value[1]
        
        col_link <- if (model %in% names(model_cols)) model_cols[[model]] else "grey40"
        bg_col   <- if (model %in% names(model_bgs))  model_bgs[[model]]  else alpha_col(col_link, 0.5)
        out_col  <- if (model %in% names(model_outline)) model_outline[[model]] else "black"
        
        segments(x_base, yq, xq, yq, col=col_link, lty=2, lwd=line_width)
        points(xq, yq, pch=25, col=out_col, bg=bg_col, cex=2)
      }
    }
  }
  
  
  # ---- legends (optional), drawn in the right interaction panel ----
  if (isTRUE(add_legends)) {
    par(xpd = NA)
    
    legend("topright",
           inset = legend_inset_terms,
           title = "Terms",
           cex = legend_cex_terms,
           legend = legend_terms,
           pch = legend_terms_pch,
           col = "grey4",
           pt.bg = legend_pt_bg,
           pt.cex = legend_terms_pt_cex)
    
    # map model keys to colors from model_cols
    model_cols_for_legend <- unname(model_cols[legend_model_keys])
    
    legend("topright",
           inset = legend_inset_model,
           title = "Model",
           cex = legend_cex_model,
           legend = legend_models,
           pch = 15,
           col = model_cols_for_legend,
           pt.cex = 2.25)
  }
  
  mtext("Coefficients",
        side = 2,
        outer = TRUE,
        line = -0.5,
        cex = 1.25)
  
  invisible(NULL)
}

