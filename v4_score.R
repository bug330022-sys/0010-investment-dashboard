# ============================================================
# v4_score.R
# 第 4 版計分：成長／動能分（單一）＋ 風險／穩定分（單一）
# 只用 base R，不依賴 app.R 內部函數
# ============================================================

V4_TD <- 252

# 動能分權重：6 個月 + 12-1 動能（Jegadeesh–Titman 標準形成期）
V4_MOM_W <- c(m6 = 0.55, m12_1 = 0.45)

# 風險穩定分權重
V4_RISK_W <- c(vol = 0.30, mdd = 0.25, cvar = 0.20, sharpe = 0.15, garch = 0.10)

# 0010 合成權重（動能主導、風險把關）
V4_BLEND <- c(mom = 0.70, risk = 0.30)

# 分數平滑：本期 70% + 上期 30%（依動能持續性實證）
V4_SMOOTH <- 0.30

# 硬性篩選門檻（風險把關，不是加權）
V4_SCREEN <- list(
  min_obs  = 280,
  max_vol  = 0.60,    # 年化波動上限
  max_mdd  = -0.60,   # 最大回撤下限（更負就剔除）
  max_cvar = 0.06,    # 單日 CVaR95 上限
  max_industry = 3,   # 同一產業最高檔數（控制集中度）
  risk_floor = 20,    # 【主要門檻】風險穩定分低於此就剔除（相對後 20%）
  hard_vol = 0.90,    # 絕對值只擋極端股
  hard_mdd = -0.85,
  hard_cvar = 0.10
)

# ------------------------------------------------------------
# 0. 百分位分數（自帶，不依賴外部函數）
# ------------------------------------------------------------

v4_pct <- function(x, higher_better = TRUE) {
  v <- as.numeric(x)
  k <- sum(is.finite(v))
  if (k < 2) return(rep(NA_real_, length(v)))
  r <- rank(v, na.last = "keep", ties.method = "average")
  g <- (r - 1) / (k - 1) * 100
  if (!higher_better) g <- 100 - g
  g
}

# 加權平均（缺值時重新標準化權重）
v4_wmean <- function(z, w) {
  ok <- is.finite(z)
  if (!any(ok)) return(NA_real_)
  sum(z[ok] * w[ok]) / sum(w[ok])
}

# ------------------------------------------------------------
# 1. 單檔原始指標
# ------------------------------------------------------------

v4_one_metrics <- function(px) {
  px <- as.numeric(px)
  px <- px[is.finite(px) & px > 0]
  n <- length(px)
  out <- list(n = n, r6 = NA_real_, r12_1 = NA_real_, vol = NA_real_,
              mdd = NA_real_, cvar = NA_real_, sharpe = NA_real_)
  if (n < 130) return(out)

  out$r6 <- if (n >= 127) px[n] / px[n - 126] - 1 else NA_real_
  out$r12_1 <- if (n >= 274) px[n - 21] / px[n - 252] - 1 else NA_real_

  if (n >= 253) {
    w <- px[(n - 251):n]
    r <- diff(log(w))
    out$vol <- stats::sd(r) * sqrt(V4_TD)
    out$mdd <- min(w / cummax(w) - 1)
    q <- stats::quantile(r, 0.05, na.rm = TRUE, names = FALSE)
    tail_r <- r[r <= q]
    out$cvar <- if (length(tail_r)) -mean(tail_r) else NA_real_
    mu <- mean(r) * V4_TD
    out$sharpe <- if (is.finite(out$vol) && out$vol > 0) mu / out$vol else NA_real_
  }
  out
}

# ------------------------------------------------------------
# 2. 主函數：算出兩大分數 + 0010 分
#    df        : 長表（symbol, date, adjusted）
#    garch_tbl : 可選，含 symbol / ratio 的 GARCH 表（沒有就用 50 分中性）
#    prev      : 可選，上期 0010 分數（named numeric，用於平滑）
# ------------------------------------------------------------

v4_score_table <- function(df, end_date = NULL, garch_tbl = NULL, prev = NULL,
                           mom_w = V4_MOM_W, risk_w = V4_RISK_W,
                           blend = V4_BLEND, smooth = 0,
                           screen = V4_SCREEN) {

  d <- as.data.frame(df)
  d$date <- as.Date(d$date)
  if (is.null(end_date)) end_date <- max(d$date, na.rm = TRUE)
  d <- d[!is.na(d$adjusted) & d$adjusted > 0 & d$date <= as.Date(end_date), , drop = FALSE]
  if (!nrow(d)) return(NULL)

  d <- d[order(d$symbol, d$date), , drop = FALSE]
  sp <- split(d$adjusted, d$symbol)

  rows <- lapply(names(sp), function(s) {
    m <- v4_one_metrics(sp[[s]])
    if (m$n < screen$min_obs) return(NULL)
    data.frame(symbol = s, n_obs = m$n, r6 = m$r6, r12_1 = m$r12_1,
               vol = m$vol, mdd = m$mdd, cvar = m$cvar, sharpe = m$sharpe,
               stringsAsFactors = FALSE)
  })
  raw <- do.call(rbind, rows)
  if (is.null(raw) || !nrow(raw)) return(NULL)

  raw <- raw[raw$symbol != "0050", , drop = FALSE]
  if (nrow(raw) < 5) return(NULL)

  # ---- 成長／動能分 ----
  p6 <- v4_pct(raw$r6, TRUE)
  p12 <- v4_pct(raw$r12_1, TRUE)
  mom <- mapply(function(a, b) v4_wmean(c(m6 = a, m12_1 = b), mom_w), p6, p12)

  # ---- 風險／穩定分 ----
  pv <- v4_pct(raw$vol, FALSE)
  pm <- v4_pct(raw$mdd, TRUE)
  pc <- v4_pct(raw$cvar, FALSE)
  ps <- v4_pct(raw$sharpe, TRUE)

  pg <- rep(50, nrow(raw))
  ratio_v <- rep(NA_real_, nrow(raw))
  if (!is.null(garch_tbl) && nrow(garch_tbl)) {
    ratio_v <- garch_tbl$ratio[match(raw$symbol, garch_tbl$symbol)]
    g <- v4_pct(ratio_v, FALSE)
    g[!is.finite(g)] <- 50
    pg <- g
  }
  

  risk <- mapply(function(a, b, cc, dd, ee) {
    v4_wmean(c(vol = a, mdd = b, cvar = cc, sharpe = dd, garch = ee), risk_w)
  }, pv, pm, pc, ps, pg)

  out <- raw
  out$mom_score <- mom
  out$risk_score <- risk
  out$p_vol <- pv
  out$p_mdd <- pm
  out$p_cvar <- pc
  out$ratio <- ratio_v
  out$p_sharpe <- ps
  out$p_garch <- pg

  # ---- 硬性篩選（風險把關）----
  out$pass <- with(out, is.finite(mom_score) & is.finite(risk_score) &
                     risk_score >= screen$risk_floor &
                     vol <= screen$hard_vol &
                     mdd >= screen$hard_mdd &
                     cvar <= screen$hard_cvar)
  out$reason <- with(out,
    ifelse(!is.finite(mom_score) | !is.finite(risk_score), "分數不足",
    ifelse(risk_score < screen$risk_floor, "風險分過低（相對後 20%）",
    ifelse(vol > screen$hard_vol, "波動極端",
    ifelse(mdd < screen$hard_mdd, "回撤極端",
    ifelse(cvar > screen$hard_cvar, "尾端風險極端", "通過"))))))

  # ---- 0010 合成分 ----
  out$p10_score <- blend[["mom"]] * out$mom_score + blend[["risk"]] * out$risk_score

  # ---- 平滑（與上期混合，降低換手）----
  if (!is.null(prev) && length(prev) && smooth > 0) {
    pv_prev <- rep(NA_real_, nrow(out))
    hit <- out$symbol %in% names(prev)
    pv_prev[hit] <- prev[out$symbol[hit]]
    out$p10_smooth <- ifelse(is.finite(pv_prev),
                             (1 - smooth) * out$p10_score + smooth * pv_prev,
                             out$p10_score)
  } else {
    out$p10_smooth <- out$p10_score
  }

  out$name <- if (exists("get_stock_name")) get_stock_name(out$symbol) else out$symbol
  out$industry <- if (exists("get_stock_industry")) get_stock_industry(out$symbol) else "未分類"
  out <- out[order(-out$p10_smooth), , drop = FALSE]
  out$rank <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

# ------------------------------------------------------------
# 3. 選 10 檔 + 權重
# ------------------------------------------------------------

v4_pick <- function(tbl, n = 10, max_industry = V4_SCREEN$max_industry) {
  t <- tbl[tbl$pass, , drop = FALSE]
  t <- t[is.finite(t$p10_smooth), , drop = FALSE]
  t <- t[order(-t$p10_smooth), , drop = FALSE]
  if (!"industry" %in% names(t) || nrow(t) <= n) return(utils::head(t, n))

  keep <- integer(0)
  cnt <- list()
  for (i in seq_len(nrow(t))) {
    ind <- as.character(t$industry[i])
    if (is.na(ind) || !nzchar(ind)) ind <- "未分類"
    c_i <- if (is.null(cnt[[ind]])) 0L else cnt[[ind]]
    if (c_i >= max_industry) next
    cnt[[ind]] <- c_i + 1L
    keep <- c(keep, i)
    if (length(keep) >= n) break
  }
  t[keep, , drop = FALSE]
}

v4_cap <- function(w, cap = 0.20, iter = 100) {
  w <- as.numeric(w)
  if (sum(w, na.rm = TRUE) <= 0) return(rep(1 / length(w), length(w)))
  w <- w / sum(w)
  k <- length(w)
  if (cap * k < 1) cap <- 1 / k
  for (i in seq_len(iter)) {
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

v4_weights <- function(sel, scheme = c("inv_vol", "equal", "score"), cap = 0.20) {
  scheme <- match.arg(scheme)
  n <- nrow(sel)
  if (n == 0) return(numeric(0))

  w <- switch(
    scheme,
    equal = rep(1 / n, n),
    score = sel$p10_smooth,
    inv_vol = {
      v <- sel$vol
      if (!any(is.finite(v)) || any(!is.finite(v))) {
        v[!is.finite(v)] <- stats::median(v[is.finite(v)])
      }
      v[!is.finite(v)] <- 1
      1 / v
    }
  )
  w <- v4_cap(w, cap)
  names(w) <- sel$symbol
  w
}
