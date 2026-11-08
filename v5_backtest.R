# ============================================================
# v5_backtest.R
# v4 季度回測：T+1 成交、交易明細、成本、準確度
# 依賴：v4_score.R、v4_drawdown.R
# ============================================================

V5_COST <- list(fee = 0.001425, min_fee = 20, tax = 0.003, slip = 0.001)

# 每季最後一個交易日
v5_quarter_ends <- function(dates) {
  dates <- as.Date(dates)
  key <- paste0(format(dates, "%Y"), "Q", ceiling(as.integer(format(dates, "%m")) / 3))
  sort(as.integer(tapply(seq_along(dates), key, max)))
}

# 單筆交易成本
v5_cost_one <- function(amount, side = c("buy", "sell"), cost = V5_COST) {
  side <- match.arg(side)
  fee <- max(cost$min_fee, amount * cost$fee)
  tax <- if (side == "sell") amount * cost$tax else 0
  slip <- amount * cost$slip
  c(fee = fee, tax = tax, slip = slip, total = fee + tax + slip)
}

# ------------------------------------------------------------
# 主回測
#   df       : 長表（symbol, date, adjusted）
#   hold     : 持有交易日數（63）
#   t1       : TRUE = 評分日後一個交易日成交
#   capital  : 期初資金
# ------------------------------------------------------------

v5_backtest_v4 <- function(df, hold = 63, n = 10, cap = 0.20,
                           max_ind = V4_SCREEN$max_industry,
                           capital = 1e6, cost = V5_COST,
                           start_after = 300, t1 = TRUE,
                           progress = NULL) {

  d <- as.data.frame(df)
  d$date <- as.Date(d$date)
  d <- d[!is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
  d <- d[d$symbol %in% c("0050", unique(d$symbol)), , drop = FALSE]
  if (!nrow(d)) return(NULL)

  dates <- sort(unique(d$date))
  qe <- v5_quarter_ends(dates)
  qe <- qe[qe > start_after & qe + hold + 2 <= length(dates)]
  if (!length(qe)) return(NULL)

  w <- as.data.frame(
    tidyr::pivot_wider(d[, c("date", "symbol", "adjusted")],
                       names_from = symbol, values_from = adjusted)
  )
  pm <- as.matrix(w[, -1, drop = FALSE])
  rownames(pm) <- as.character(as.Date(w$date))
  storage.mode(pm) <- "double"

  prev_w <- NULL
  prev_ret <- NULL
  equity <- 1
  eq_bench <- 1
  capital_now <- capital

  rec <- list()
  tradelog <- list()

  for (k in seq_along(qe)) {
    i <- qe[k]
    if (!is.null(progress)) progress(k, length(qe))
    d0 <- dates[i]

    sc <- v4_score_table(d[d$date <= d0, , drop = FALSE])
    if (is.null(sc) || nrow(sc) < n) next

    sel <- v4_pick(sc, n, max_ind)
    if (nrow(sel) < 3) next
    ww <- v4_weights(sel, scheme = "inv_vol", cap = cap)

    buy_i  <- if (t1) i + 1 else i
    sell_i <- min(buy_i + hold, nrow(pm))
    if (sell_i <= buy_i) next

    syms <- names(ww)
    syms <- syms[syms %in% colnames(pm)]
    if (length(syms) < 3) next
    ww <- ww[syms] / sum(ww[syms])

    buy_px  <- pm[buy_i, syms]
    sell_px <- pm[sell_i, syms]
    ok <- is.finite(buy_px) & is.finite(sell_px) & buy_px > 0
    if (!any(ok)) next
    syms <- syms[ok]; ww <- ww[ok] / sum(ww[ok])
    buy_px <- buy_px[ok]; sell_px <- sell_px[ok]

    r <- as.numeric(sell_px / buy_px - 1)
    names(r) <- syms
    gross <- sum(ww * r)

    # ---- 交易成本（逐檔）----
    target_amt <- ww * capital_now
    if (is.null(prev_w)) {
      prev_amt <- rep(0, length(syms)); names(prev_amt) <- syms
    } else {
      drift <- prev_w * (1 + prev_ret)
      drift <- drift / sum(drift)
      prev_amt <- as.numeric(drift[syms]) * capital_now
      prev_amt[!is.finite(prev_amt)] <- 0
      names(prev_amt) <- syms
    }

    delta <- target_amt - prev_amt
    buys  <- ifelse(delta > 0, delta, 0)
    sells <- ifelse(delta < 0, -delta, 0)

    fee_b  <- sum(ifelse(buys  > 0, pmax(cost$min_fee, buys  * cost$fee), 0))
    slip_b <- sum(buys * cost$slip)
    fee_s  <- sum(ifelse(sells > 0, pmax(cost$min_fee, sells * cost$fee), 0))
    tax_s  <- sum(sells * cost$tax)
    slip_s <- sum(sells * cost$slip)
    cost_total <- fee_b + slip_b + fee_s + tax_s + slip_s
    cost_rate <- cost_total / capital_now

    net <- gross - cost_rate
    turnover <- sum(abs(delta), na.rm = TRUE) / 2 / capital_now

    bench <- NA_real_
    if ("0050" %in% colnames(pm)) {
      bb <- pm[buy_i, "0050"]
      ss <- pm[sell_i, "0050"]
      if (is.finite(bb) && is.finite(ss) && bb > 0) bench <- ss / bb - 1
    }

    # ---- 準確度：IC、命中率、Spread ----
    acc_syms <- intersect(sc$symbol, colnames(pm))
    fwd_all <- pm[sell_i, acc_syms] / pm[buy_i, acc_syms] - 1
    ok2 <- is.finite(fwd_all)
    score_vec <- sc$p10_smooth[match(acc_syms, sc$symbol)]
    ic <- if (sum(ok2) >= 5) {
      suppressWarnings(stats::cor(score_vec[ok2], fwd_all[ok2], method = "spearman"))
    } else NA_real_

    ord <- order(-fwd_all, na.last = TRUE)
    top_actual <- acc_syms[utils::head(ord, n)]
    hit <- length(intersect(syms, top_actual)) / length(syms)

    bot <- acc_syms[utils::tail(order(fwd_all, na.last = TRUE), n)]
    spread <- mean(fwd_all[syms], na.rm = TRUE) - mean(fwd_all[bot], na.rm = TRUE)

    capital_now <- capital_now * (1 + net)
    equity <- equity * (1 + net)
    eq_bench <- eq_bench * (1 + ifelse(is.finite(bench), bench, 0))

    lab <- paste0(
      get_stock_name(syms), " ", sprintf("%.1f%%", ww * 100),
      collapse = "、"
    )

    rec[[length(rec) + 1]] <- data.frame(
      評分日 = d0, 成交日 = dates[buy_i], 出場日 = dates[sell_i],
      檔數 = length(syms),
      換手率 = turnover,
      買進金額 = sum(buys), 賣出金額 = sum(sells),
      手續費 = fee_b + fee_s,
      證交稅 = tax_s,
      滑價 = slip_b + slip_s,
      成本合計 = cost_total,
      成本率 = cost_rate,
      毛報酬 = gross, 淨報酬 = net, 基準0050 = bench,
      超額 = net - ifelse(is.finite(bench), bench, 0),
      累積淨值 = equity, 累積0050 = eq_bench,
      命中率 = hit, IC = ic, Spread = spread,
      持股 = lab,
      stringsAsFactors = FALSE
    )

    prev_w <- ww
    prev_ret <- r
  }

  if (!length(rec)) return(NULL)
  q <- do.call(rbind, rec)

  per_year <- 252 / hold
  ann <- function(x) if (length(x)) prod(1 + x)^(per_year / length(x)) - 1 else NA_real_
  dd <- q$累積淨值 / cummax(q$累積淨值) - 1

  stats <- list(
    n = nrow(q),
    cum = utils::tail(q$累積淨值, 1) - 1,
    ann = ann(q$淨報酬),
    vol = stats::sd(q$淨報酬) * sqrt(per_year),
    sharpe = if (stats::sd(q$淨報酬) > 0) ann(q$淨報酬) / (stats::sd(q$淨報酬) * sqrt(per_year)) else NA_real_,
    mdd = min(dd, na.rm = TRUE),
    bench_cum = utils::tail(q$累積0050, 1) - 1,
    bench_ann = ann(q$基準0050),
    win = mean(q$淨報酬 > q$基準0050, na.rm = TRUE),
    avg_turnover = mean(q$換手率, na.rm = TRUE),
    total_cost = sum(q$成本合計, na.rm = TRUE),
    hit = mean(q$命中率, na.rm = TRUE),
    ic = mean(q$IC, na.rm = TRUE),
    spread = mean(q$Spread, na.rm = TRUE)
  )

  list(quarters = q, stats = stats)
}
