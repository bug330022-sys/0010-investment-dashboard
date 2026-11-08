# ============================================================
# p10_model.R
# 0010 組合：四因子綜合分數、權重、交易成本、季度再平衡回測
# 依賴：garch_model.R、app.R 內的 cross_section_score() 與 score_return_matrix()
# ============================================================

# ------------------------------------------------------------
# 1. 四因子權重（三組 preset，讓使用者比較）
# ------------------------------------------------------------

P10_PRESETS <- list(
  A = c(growth = 0.55, risk = 0.20, stability = 0.15, garch = 0.10),
  B = c(growth = 0.40, risk = 0.25, stability = 0.20, garch = 0.15),
  C = c(growth = 0.30, risk = 0.25, stability = 0.25, garch = 0.20)
)

P10_PRESET_LABEL <- c(
  A = "A｜成長導向 55/20/15/10",
  B = "B｜均衡型 40/25/20/15",
  C = "C｜防守導向 30/25/25/20"
)

# 交易成本（台股）
P10_COST <- list(fee = 0.001425, min_fee = 20, tax = 0.003, slip = 0.001)

# ------------------------------------------------------------
# 2. 全成分股 GARCH 表
# ------------------------------------------------------------

p10_garch_table <- function(df, symbols, window = 1000, horizon = 63, progress = NULL) {
  d <- as.data.frame(df)
  d <- d[!is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
  d <- d[order(d$symbol, d$date), , drop = FALSE]
  sp <- split(d$adjusted, d$symbol)

  rows <- list()
  k <- 0
  for (s in symbols) {
    k <- k + 1
    if (!is.null(progress)) progress(k, length(symbols), s)

    x <- sp[[s]]
    if (is.null(x) || length(x) < 260) next

    a <- garch_analyse_one(as.numeric(x), window = window, horizon = horizon)
    if (is.null(a)) next

    f <- a$fit
    rows[[length(rows) + 1]] <- data.frame(
      symbol = s,
      omega = f$omega, alpha = f$alpha, beta = f$beta,
      persistence = f$persistence,
      half_life = f$half_life,
      forecast_vol = a$summary$forecast_vol,
      real_vol = a$real_vol,
      ewma = a$ewma_vol,
      ratio = a$ratio,
      var95 = a$summary$var95,
      cvar95 = a$summary$cvar95,
      aic = f$aic, bic = f$bic,
      lb_p = f$lb_p,
      converged = f$converged,
      regime = a$regime,
      stringsAsFactors = FALSE
    )
  }

  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 3. 四因子綜合分數
# ------------------------------------------------------------

p10_score_table <- function(scored, garch_tbl, preset = "A") {
  w <- P10_PRESETS[[preset]]
  if (is.null(w)) w <- P10_PRESETS$A

  t <- as.data.frame(scored)
  t <- t[t$symbol != "0050", , drop = FALSE]

  g <- garch_tbl
  if (!is.null(g) && nrow(g)) {
    t <- merge(t, g[, c("symbol", "ratio", "forecast_vol", "real_vol", "regime",
                        "persistence", "half_life", "lb_p", "converged")],
               by = "symbol", all.x = TRUE)
  } else {
    t$ratio <- NA_real_
    t$forecast_vol <- NA_real_
    t$real_vol <- NA_real_
    t$regime <- "無資料"
    t$persistence <- NA_real_
    t$half_life <- NA_real_
    t$lb_p <- NA_real_
    t$converged <- NA
  }

  # GARCH 分數：ratio 越低（預期波動相對趨緩）越好
  gs <- cross_section_score(t$ratio, FALSE)
  gs[!is.finite(gs)] <- 50          # 沒有 GARCH 結果時給中性 50 分
  t$garch_score <- gs

  t$p10_score <-
    w[["growth"]]    * t$growth_score +
    w[["risk"]]      * t$risk_score +
    w[["stability"]] * t$stability_score +
    w[["garch"]]     * t$garch_score

  t$p10_score[!is.finite(t$p10_score)] <- NA_real_
  t <- t[order(-t$p10_score), , drop = FALSE]
  t$p10_rank <- seq_len(nrow(t))
  t$name <- get_stock_name(t$symbol)
  t$industry <- get_stock_industry(t$symbol)
  t
}

# ------------------------------------------------------------
# 3-1. 由報酬矩陣直接評分（不依賴 app.R 的內部函數）
#      回測、持有追蹤、本期名單都用這一個版本，確保一致
# ------------------------------------------------------------

p10_pct <- function(x, higher = TRUE) {
  x <- as.numeric(x)
  n <- length(x)
  k <- sum(is.finite(x))
  if (k < 2) {
    out <- rep(NA_real_, n)
    out[is.finite(x)] <- 50
    return(out)
  }
  r <- rank(x, na.last = "keep", ties.method = "average")
  g <- (r - 1) / (k - 1) * 100
  if (!higher) g <- 100 - g
  g
}

p10_matrix_scores <- function(ret_mat, vol_mat = NULL, preset = "A") {
  ret_mat <- as.matrix(ret_mat)
  n <- nrow(ret_mat)
  syms <- colnames(ret_mat)
  if (is.null(syms) || n < 260) return(NULL)

  per <- c(h1w = 5, h1m = 21, h3m = 63, h6m = 126, h1y = 252, h3y = 756)
  pw  <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.25)

  win <- function(k) {
    k <- min(k, n)
    ret_mat[(n - k + 1):n, , drop = FALSE]
  }

  # ---- 期間累積報酬 ----
  rp <- sapply(per, function(h) {
    sub <- win(h)
    ok <- colSums(is.finite(sub)) > 0
    v <- colSums(sub, na.rm = TRUE)
    v[!ok] <- NA_real_
    exp(v) - 1
  })
  if (is.null(dim(rp))) rp <- matrix(rp, nrow = length(syms))

  # ---- 各期間年化報酬 / 年化波動 ----
  ret_p <- sapply(per, function(h) {
    sub <- win(h)
    ok <- colSums(is.finite(sub)) > 0
    v <- colMeans(sub, na.rm = TRUE) * 252
    v[!ok] <- NA_real_
    v
  })
  if (is.null(dim(ret_p))) ret_p <- matrix(ret_p, nrow = length(syms))

  vol_p <- sapply(per, function(h) {
    if (h < 21) return(rep(NA_real_, length(syms)))
    sub <- win(h)
    apply(sub, 2, function(r) {
      r <- r[is.finite(r)]
      if (length(r) < 15) NA_real_ else stats::sd(r) * sqrt(252)
    })
  })
  if (is.null(dim(vol_p))) vol_p <- matrix(vol_p, nrow = length(syms))

  # ---- 近一年主要指標 ----
  base <- win(252)
  cnt <- colSums(is.finite(base))
  mu  <- colMeans(base, na.rm = TRUE) * 252
  ann_ret <- ifelse(cnt >= 60, exp(mu) - 1, NA_real_)
  ann_vol <- apply(base, 2, function(r) {
    r <- r[is.finite(r)]
    if (length(r) < 60) NA_real_ else stats::sd(r) * sqrt(252)
  })
  mdd <- apply(base, 2, function(r) {
    r <- r[is.finite(r)]
    if (length(r) < 60) return(NA_real_)
    w <- exp(cumsum(r))
    min(w / cummax(w) - 1)
  })
  cvar <- apply(base, 2, function(r) {
    r <- r[is.finite(r)]
    if (length(r) < 20) return(NA_real_)
    q <- stats::quantile(r, 0.05, names = FALSE)
    mean(r[r <= q])
  })
  sharpe <- ifelse(is.finite(ann_vol) & ann_vol > 0, mu / ann_vol, NA_real_)

  score_from <- function(M, w) {
    M <- as.matrix(M)
    out <- rep(NA_real_, nrow(M))
    for (i in seq_len(nrow(M))) {
      z <- M[i, ]
      ok <- is.finite(z)
      if (!any(ok)) next
      out[i] <- sum(z[ok] * w[ok]) / sum(w[ok])
    }
    out
  }

  # ---- 四大分數 ----
  rp_pct <- apply(rp, 2, p10_pct, higher = TRUE)
  if (is.null(dim(rp_pct))) rp_pct <- matrix(rp_pct, nrow = length(syms))

  multi <- score_from(rp_pct, pw)
  return_score <- 0.70 * multi + 0.30 * p10_pct(ann_ret, TRUE)
  growth_score <- 0.70 * return_score + 0.30 * p10_pct(sharpe, TRUE)

  risk_score <- 0.35 * p10_pct(ann_vol, FALSE) +
                0.35 * p10_pct(mdd, TRUE) +
                0.30 * p10_pct(cvar, TRUE)

  sd_of <- function(M) {
    apply(M, 1, function(z) {
      z <- z[is.finite(z)]
      if (length(z) < 2) NA_real_ else stats::sd(z)
    })
  }
  stability_score <- 0.30 * p10_pct(sd_of(rp_pct), FALSE) +
                     0.40 * p10_pct(sd_of(ret_p), FALSE) +
                     0.30 * p10_pct(sd_of(vol_p), FALSE)

  # ---- GARCH / EWMA 波動比率 ----
  fv <- rep(NA_real_, length(syms))
  ratio <- rep(NA_real_, length(syms))
  if (!is.null(vol_mat)) {
    vm <- as.matrix(vol_mat)
    fv <- unname(vm[nrow(vm), syms])
    ratio <- fv / ann_vol
  }
  garch_score <- p10_pct(ratio, FALSE)
  garch_score[!is.finite(garch_score)] <- 50

  w <- P10_PRESETS[[preset]]
  if (is.null(w)) w <- P10_PRESETS$A
  p10 <- w[["growth"]] * growth_score +
         w[["risk"]] * risk_score +
         w[["stability"]] * stability_score +
         w[["garch"]] * garch_score

  out <- data.frame(
    symbol = syms,
    growth_score = growth_score,
    return_score = return_score,
    risk_score = risk_score,
    stability_score = stability_score,
    garch_score = garch_score,
    p10_score = p10,
    forecast_vol = fv,
    ratio = ratio,
    stringsAsFactors = FALSE
  )
  out$name <- get_stock_name(out$symbol)
  out$industry <- get_stock_industry(out$symbol)
  out
}

# ------------------------------------------------------------
# 4. 選股與權重
# ------------------------------------------------------------

p10_select <- function(tbl, n = 10) {
  t <- tbl[is.finite(tbl$p10_score), , drop = FALSE]
  t <- t[order(-t$p10_score), , drop = FALSE]
  utils::head(t, n)
}

p10_cap_weights <- function(w, cap = 0.20, iter = 100) {
  w <- as.numeric(w)
  if (sum(w, na.rm = TRUE) <= 0) return(rep(1 / length(w), length(w)))
  w <- w / sum(w)
  n <- length(w)
  if (cap * n < 1) cap <- 1 / n

  for (k in seq_len(iter)) {
    over <- w > cap + 1e-10
    if (!any(over)) break
    w[over] <- cap
    free <- !over
    if (!any(free)) break
    rest <- 1 - sum(w[over])
    w[free] <- rest * w[free] / sum(w[free])
  }
  w
}

p10_weights <- function(sel, scheme = c("inv_vol", "equal", "score"), cap = 0.20) {
  scheme <- match.arg(scheme)
  n <- nrow(sel)
  if (n == 0) return(numeric(0))

  w <- switch(
    scheme,
    equal   = rep(1 / n, n),
    score   = sel$p10_score,
    inv_vol = {
      v <- sel$forecast_vol
      if (all(!is.finite(v)) || any(v <= 0, na.rm = TRUE)) {
        v <- rep(1, n)
      } else {
        v[!is.finite(v) | v <= 0] <- stats::median(v[is.finite(v) & v > 0])
      }
      1 / v
    }
  )

  w <- p10_cap_weights(w, cap)
  names(w) <- sel$symbol
  w
}

# ------------------------------------------------------------
# 5. 交易成本
# ------------------------------------------------------------

p10_cost_detail <- function(amount, side = c("buy", "sell"), cost = P10_COST) {
  side <- match.arg(side)
  fee <- max(cost$min_fee, amount * cost$fee)
  tax <- if (side == "sell") amount * cost$tax else 0
  slip <- amount * cost$slip
  list(fee = fee, tax = tax, slip = slip, total = fee + tax + slip)
}

# 換手率 → 成本率（後續回測用；最低手續費不影響比率型計算）
p10_turnover_cost_rate <- function(turnover, cost = P10_COST) {
  turnover * (2 * cost$fee + 2 * cost$slip + cost$tax)
}

# ------------------------------------------------------------
# 5-1. EWMA 波動矩陣（GARCH 家族 lambda=0.94）
#      只用當日以前資訊，可安全用於歷史回測
# ------------------------------------------------------------

p10_vol_matrix <- function(ret_mat, lambda = 0.94) {
  ret_mat <- as.matrix(ret_mat)
  n <- nrow(ret_mat)
  k <- ncol(ret_mat)
  out <- matrix(NA_real_, n, k, dimnames = dimnames(ret_mat))

  for (j in seq_len(k)) {
    r <- ret_mat[, j]
    v <- mean(r[is.finite(r)]^2, na.rm = TRUE)
    if (!is.finite(v) || v <= 0) next
    vv <- numeric(n)
    for (t in seq_len(n)) {
      if (is.finite(r[t])) v <- lambda * v + (1 - lambda) * r[t]^2
      vv[t] <- v
    }
    out[, j] <- sqrt(vv * 252)
  }
  out
}

# ------------------------------------------------------------
# 6. 季度再平衡回測
#    ret_mat : 日期 × 股票 的日對數報酬（小數）
#    vol_mat : 日期 × 股票 的年化 GARCH 波動（小數，可為 NULL）
#    rebalance_idx : 每次評分日在 ret_mat 的列索引
# ------------------------------------------------------------

p10_backtest <- function(ret_mat, vol_mat = NULL, rebalance_idx,
                         hold = 63, n = 10, preset = "A",
                         scheme = "inv_vol", cap = 0.20,
                         cost = P10_COST, progress = NULL) {

  ret_mat <- as.matrix(ret_mat)
  symbols <- colnames(ret_mat)
  if (is.null(symbols)) return(NULL)
  w <- P10_PRESETS[[preset]]
  if (is.null(w)) w <- P10_PRESETS$A

  rec <- list()
  prev_w <- NULL
  prev_ret <- NULL
  equity <- 1
  equity_0050 <- 1

  for (k in seq_along(rebalance_idx)) {
    i <- rebalance_idx[k]
    if (!is.null(progress)) progress(k, length(rebalance_idx), NA)

    if (i < 260 || i >= nrow(ret_mat)) next

    sub <- ret_mat[1:i, , drop = FALSE]
    vs <- if (!is.null(vol_mat)) vol_mat[1:i, , drop = FALSE] else NULL

    sc <- try(p10_matrix_scores(sub, vs, preset = preset), silent = TRUE)
    if (inherits(sc, "try-error") || is.null(sc) || !nrow(sc)) next
    sc <- sc[sc$symbol != "0050", , drop = FALSE]

    sel <- sc[is.finite(sc$p10_score), , drop = FALSE]
    sel <- utils::head(sel[order(-sel$p10_score), , drop = FALSE], n)
    if (nrow(sel) < 3) next

    ww <- p10_weights(sel, scheme = scheme, cap = cap)

    # --- 未來持有期報酬 ---
    j <- min(i + hold, nrow(ret_mat))
    fwd <- exp(colSums(ret_mat[(i + 1):j, , drop = FALSE], na.rm = TRUE)) - 1

    gross <- sum(ww * fwd[names(ww)], na.rm = TRUE)

    # --- 換手率與成本 ---
    if (is.null(prev_w)) {
      turnover <- 1
      cost_rate <- cost$fee + cost$slip          # 建倉：只有買方成本
    } else {
      drift <- prev_w * (1 + prev_ret[names(prev_w)])
      drift[!is.finite(drift)] <- 0
      if (sum(drift) > 0) drift <- drift / sum(drift)
      all_sym <- union(names(prev_w), names(ww))
      a <- ifelse(all_sym %in% names(drift), drift[all_sym], 0)
      b <- ifelse(all_sym %in% names(ww), ww[all_sym], 0)
      turnover <- sum(abs(b - a), na.rm = TRUE) / 2
      cost_rate <- p10_turnover_cost_rate(turnover, cost)
    }

    net <- (1 + gross) * (1 - cost_rate) - 1

    bench <- fwd["0050"]
    if (!is.finite(bench)) bench <- 0

    equity <- equity * (1 + net)
    equity_0050 <- equity_0050 * (1 + as.numeric(bench))

    rec[[length(rec) + 1]] <- data.frame(
      date = as.Date(rownames(ret_mat)[i]),
      n_stock = nrow(sel),
      turnover = turnover,
      cost_rate = cost_rate,
      gross = gross,
      net = net,
      bench = as.numeric(bench),
      equity = equity,
      equity_0050 = equity_0050,
      top = paste(paste0(sel$symbol, "(", round(ww * 100, 1), "%)"), collapse = " "),
      stringsAsFactors = FALSE
    )

    prev_w <- ww
    prev_ret <- fwd
  }

  if (!length(rec)) return(NULL)
  q <- do.call(rbind, rec)

  per_year <- 252 / hold
  r <- q$net
  ann_ret <- (prod(1 + r))^(per_year / length(r)) - 1
  ann_vol <- stats::sd(r) * sqrt(per_year)
  dd <- q$equity / cummax(q$equity) - 1

  stats <- list(
    n_periods = length(r),
    cum_ret = prod(1 + r) - 1,
    ann_ret = ann_ret,
    ann_vol = ann_vol,
    sharpe = if (ann_vol > 0) ann_ret / ann_vol else NA_real_,
    mdd = min(dd, na.rm = TRUE),
    win_vs_bench = mean(q$net > q$bench, na.rm = TRUE),
    avg_turnover = mean(q$turnover, na.rm = TRUE),
    total_cost = sum(q$cost_rate, na.rm = TRUE),
    bench_cum = prod(1 + q$bench) - 1,
    bench_ann = (prod(1 + q$bench))^(per_year / length(q$bench)) - 1
  )

  list(quarters = q, stats = stats)
}

# ------------------------------------------------------------
# 7. 下一次再平衡日（下一個季度的第一個交易日）
# ------------------------------------------------------------

p10_next_rebalance <- function(last_date) {
  d <- as.Date(last_date)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))
  q <- (m - 1) %/% 3 + 1
  if (q == 4) { y <- y + 1; m0 <- 1 } else { m0 <- q * 3 + 1 }
  first <- as.Date(sprintf("%04d-%02d-01", y, m0))
  while (as.integer(format(first, "%u")) > 5) first <- first + 1
  first
}
