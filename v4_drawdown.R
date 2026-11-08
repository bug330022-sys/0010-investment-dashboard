# ============================================================
# v4_drawdown.R
# 回撤分析：0010 組合的回撤曲線、最大回撤、水下時間
# ============================================================

# ------------------------------------------------------------
# 1. 回撤序列
# ------------------------------------------------------------

v4_dd_series <- function(value, date) {
  v <- as.numeric(value)
  d <- as.Date(date)
  ok <- is.finite(v)
  v <- v[ok]
  d <- d[ok]
  if (length(v) < 2) return(NULL)

  peak <- cummax(v)
  data.frame(date = d, value = v, peak = peak, dd = v / peak - 1,
             stringsAsFactors = FALSE)
}

# ------------------------------------------------------------
# 2. 回撤統計
#    最大回撤、高點日、低點日、回復日、水下比例、最長水下期間
# ------------------------------------------------------------

v4_dd_stats <- function(s) {
  if (is.null(s) || nrow(s) < 2) return(NULL)

  i <- which.min(s$dd)
  max_dd <- s$dd[i]

  # 高點：低點之前、回撤為 0 的最後一天
  before <- which(s$dd[seq_len(i)] >= -1e-12)
  peak_i <- if (length(before)) max(before) else 1

  # 回復：低點之後，價格回到前高
  after <- which(s$value[(i + 1):nrow(s)] >= s$peak[i])
  rec_i <- if (length(after)) i + min(after) else NA_integer_

  # 水下比例
  underwater <- mean(s$dd < -1e-9)

  # 最長連續水下
  r <- rle(s$dd < -1e-9)
  longest <- if (any(r$values)) max(r$lengths[r$values]) else 0

  list(
    max_dd = max_dd,
    peak_date = s$date[peak_i],
    trough_date = s$date[i],
    recover_date = if (is.na(rec_i)) as.Date(NA) else s$date[rec_i],
    days_to_trough = as.integer(s$date[i] - s$date[peak_i]),
    days_to_recover = if (is.na(rec_i)) NA_integer_ else as.integer(s$date[rec_i] - s$date[i]),
    underwater_pct = underwater,
    longest_underwater = longest,
    avg_dd = mean(s$dd[s$dd < 0])
  )
}

# ------------------------------------------------------------
# 3. 買進持有組合的每日淨值
#    pm : 價格矩陣（列名 = 日期字串，欄名 = 股票代號）
#    w  : 權重（named numeric）
# ------------------------------------------------------------

v4_buyhold <- function(pm, w, from_idx = 1) {
  syms <- names(w)
  syms <- syms[syms %in% colnames(pm)]
  if (!length(syms) || from_idx >= nrow(pm)) return(NULL)
  
  sub <- pm[from_idx:nrow(pm), syms, drop = FALSE]
  base <- sub[1, ]
  ok <- is.finite(base) & base > 0
  if (!any(ok)) return(NULL)
  
  sub <- sub[, ok, drop = FALSE]
  base <- base[ok]
  ww <- w[names(base)] / sum(w[names(base)])
  if (!is.finite(sum(ww)) || sum(ww) <= 0) return(NULL)
  
  rel <- sweep(sub, 2, base, "/")
  val <- as.numeric(rel %*% ww)
  data.frame(date = as.Date(rownames(pm)[from_idx:nrow(pm)]),
             value = val, stringsAsFactors = FALSE)
}


# ------------------------------------------------------------
# 4. 兩條線並排（0010 vs 0050）的回撤比較表
# ------------------------------------------------------------

v4_dd_compare <- function(list_series, list_label) {
  rows <- lapply(seq_along(list_series), function(k) {
    st <- v4_dd_stats(list_series[[k]])
    if (is.null(st)) return(NULL)
    data.frame(
      策略 = list_label[k],
      最大回撤 = st$max_dd,
      高點日 = as.character(st$peak_date),
      低點日 = as.character(st$trough_date),
      回復日 = if (is.na(st$recover_date)) "未回復" else as.character(st$recover_date),
      下跌天數 = st$days_to_trough,
      回復天數 = if (is.na(st$days_to_recover)) NA_integer_ else st$days_to_recover,
      水下比例 = st$underwater_pct,
      最長水下 = st$longest_underwater,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(NULL)
  out
}

# ------------------------------------------------------------
# 5. 格式化（給 renderTable 用）
# ------------------------------------------------------------

v4_dd_table <- function(st) {
  if (is.null(st)) return(data.frame(項目 = "資料不足"))
  data.frame(
    項目 = c("最大回撤", "高點日期", "低點日期", "回復日期",
             "高點到低點（交易日）", "低點到回復（交易日）",
             "水下時間比例", "最長連續水下（交易日）", "平均回撤深度"),
    數值 = c(
      sprintf("%.2f%%", st$max_dd * 100),
      as.character(st$peak_date),
      as.character(st$trough_date),
      if (is.na(st$recover_date)) "尚未回復" else as.character(st$recover_date),
      as.character(st$days_to_trough),
      if (is.na(st$days_to_recover)) "—" else as.character(st$days_to_recover),
      sprintf("%.1f%%", st$underwater_pct * 100),
      as.character(st$longest_underwater),
      if (is.finite(st$avg_dd)) sprintf("%.2f%%", st$avg_dd * 100) else "—"
    ),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
