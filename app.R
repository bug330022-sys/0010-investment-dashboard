# 0010 Investment Research v3.6：Growth 主排名＋防守資訊分離＋白話研究說明＋模型比較驗證
# Connect Cloud auto-publish test
# ============================================================
# 0010 Investment Research
# 第 1 段：資料、共用函數、UI
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

options(scipen = 999)
source("garch_model.R", local = TRUE)
source("p10_model.R", local = TRUE)
#source("update_data.R", local = TRUE)
source("v4_score.R", local = TRUE)
source("v4_ts.R", local = TRUE)
source("v4_drawdown.R", local = TRUE)
source("v5_backtest.R", local = TRUE)
source("v5_garch_diag.R", local = TRUE)
# ============================================================
# 1. 基本設定
# ============================================================

DATA_FILE <- "data/stock_data.rds"

RESEARCH_START_DATE <- as.Date("2016-01-04")
RESEARCH_END_DATE   <- Sys.Date()

TRADING_DAYS_PER_YEAR <- 252
MAX_SELECTED_STOCKS <- 5
TRADE_UNIT <- 1

# ------------------------------------------------------------
# 全站統一時間尺度
# ------------------------------------------------------------
# 所有統計頁面共用同一套交易日定義，避免不同頁面各自使用不同期間。
ANALYSIS_WINDOWS <- c(
  `1週` = 5,
  `1個月` = 21,
  `3個月` = 63,
  `6個月` = 126,
  `1年` = 252,
  `3年` = 756
)

# 多期間報酬 / 穩定性權重。短期可以反映近期變化，
# 但不讓單一一週行情主導整體結果。
RETURN_PERIOD_WEIGHTS <- c(
  `1週` = 0.05,
  `1個月` = 0.10,
  `3個月` = 0.15,
  `6個月` = 0.20,
  `1年` = 0.25,
  `3年` = 0.25
)

# ------------------------------------------------------------
# 新版主排名：成長分數（Growth Score）
# ------------------------------------------------------------
# 目前是「研究候選設定」，不是宣稱唯一最佳權重。
# 報酬分數負責描述相對強弱；夏普分數補充風險調整後的報酬效率。
GROWTH_WEIGHTS <- c(
  ret = 0.70,
  sharpe = 0.30
)

# 舊版 Composite 保留，只作模型比較與研究歷程對照。
LEGACY_COMPOSITE_WEIGHTS <- c(
  stability = 0.40,
  ret = 0.25,
  risk = 0.20,
  sharpe = 0.15
)

# 主要風險、夏普與「目前狀態」以近 1 年為共同基礎。
SCORING_BASE_DAYS <- ANALYSIS_WINDOWS[["1年"]]

# 歷史模型驗證可用的未來觀察期間。
VALIDATION_FORWARD_WINDOWS <- c(
  `1週` = 5,
  `1個月` = 21,
  `3個月` = 63,
  `6個月` = 126,
  `1年` = 252,
  `2年` = 504,
  `3年` = 756,
  `4年` = 1008,
  `5年` = 1260
)


# ============================================================
# 2. 0050 成分股資料
# ============================================================

stock_info <- data.frame(
  
  symbol = c(
    "2330","2454","2308","2317","3711",
    "2303","2383","3037","2881","2891",
    "1303","3017","2882","2887","2345",
    "2382","2327","2360","2885","2884",
    "2059","2408","6669","2357","2883",
    "2886","3008","2890","3443","3231",
    "2301","2344","2412","3653","2880",
    "2892","3665","1216","4958","6446",
    "7769","2368","2449","2395","5880",
    "8046","2603","4904","3045","6505"
  ),
  
  name = c(
    "台積電","聯發科","台達電","鴻海","日月光投控",
    "聯電","台光電","欣興","富邦金","中信金",
    "南亞","奇鋐","國泰金","台新新光金","智邦",
    "廣達","國巨","致茂","元大金","玉山金",
    "川湖","南亞科","緯穎","華碩","凱基金",
    "兆豐金","大立光","永豐金","創意","緯創",
    "光寶科","華邦電","中華電","健策","華南金",
    "第一金","貿聯-KY","統一","臻鼎-KY","藥華藥",
    "鴻勁","金像電","京元電子","研華","合庫金",
    "南電","長榮","遠傳","台灣大","台塑化"
  ),
  
  industry = c(
    "半導體業","半導體業","電子零組件業","其他電子業","半導體業",
    "半導體業","電子零組件業","電子零組件業","金融保險","金融保險",
    "塑膠工業","電腦及週邊設備業","金融保險","金融保險","通信網路業",
    "電腦及週邊設備業","電子零組件業","其他電子業","金融保險","金融保險",
    "電子零組件業","半導體業","電腦及週邊設備業","電腦及週邊設備業","金融保險",
    "金融保險","其他電子業","金融保險","半導體業","電腦及週邊設備業",
    "電腦及週邊設備業","半導體業","通信網路業","電子零組件業","金融保險",
    "金融保險","其他電子業","食品工業","電子零組件業","生技醫療業",
    "半導體業","電子零組件業","半導體業","電腦及週邊設備業","金融保險",
    "電子零組件業","航運業","通信網路業","通信網路業","油電燃氣業"
  ),
  
  stringsAsFactors = FALSE
)

benchmark_info <- data.frame(
  symbol = "0050",
  name = "元大台灣50",
  industry = "比較基準",
  stringsAsFactors = FALSE
)
# ============================================================
# 產業分類：少於 4 檔的產業合併為「其他」
# ============================================================

industry_count <- table(
  stock_info$industry
)

major_industries <- names(
  industry_count[
    industry_count >= 4
  ]
)

industry_group <- function(x) {
  
  ifelse(
    x %in% major_industries,
    x,
    "其他"
  )
  
}

# ============================================================
# 3. 共用函數
# ============================================================

# ============================================================
# 股票名稱 / 產業 / 顯示標籤
# 支援單一股票，也支援整個向量
# ============================================================

get_stock_name_one <- function(symbol) {
  
  symbol <- as.character(symbol)
  
  if (
    length(symbol) == 0 ||
    is.na(symbol)
  ) {
    return(NA_character_)
  }
  
  if (symbol == "0050") {
    return("元大台灣50")
  }
  
  result <- stock_info$name[
    match(
      symbol,
      stock_info$symbol
    )
  ]
  
  if (
    length(result) == 0 ||
    is.na(result)
  ) {
    return(symbol)
  }
  
  result
}


get_stock_name <- function(symbol) {
  
  vapply(
    as.character(symbol),
    get_stock_name_one,
    character(1)
  )
  
}


get_stock_industry_one <- function(symbol) {
  
  symbol <- as.character(symbol)
  
  if (
    length(symbol) == 0 ||
    is.na(symbol)
  ) {
    return(NA_character_)
  }
  
  if (symbol == "0050") {
    return("比較基準")
  }
  
  result <- stock_info$industry[
    match(
      symbol,
      stock_info$symbol
    )
  ]
  
  if (
    length(result) == 0 ||
    is.na(result)
  ) {
    return("其他")
  }
  
  result
}


get_stock_industry <- function(symbol) {
  
  vapply(
    as.character(symbol),
    get_stock_industry_one,
    character(1)
  )
  
}


get_stock_label <- function(symbol) {
  
  symbol <- as.character(symbol)
  
  paste0(
    get_stock_name(symbol),
    " (",
    symbol,
    ")"
  )
  
}


convert_to_date <- function(x) {
  
  if (inherits(x, "Date")) {
    return(x)
  }
  
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  if (is.numeric(x)) {
    
    finite_x <- x[is.finite(x)]
    
    if (
      length(finite_x) > 0 &&
      min(finite_x) >= 10000 &&
      max(finite_x) <= 60000
    ) {
      
      return(
        as.Date(
          x,
          origin = "1899-12-30"
        )
      )
    }
  }
  
  as.Date(x)
}


safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}


fmt_price <- function(x) {
  ifelse(
    is.finite(x),
    sprintf("%.2f", x),
    "NA"
  )
}


fmt_num <- function(x, digits = 0) {
  
  ifelse(
    is.finite(x),
    formatC(
      x,
      format = "f",
      digits = digits,
      big.mark = ","
    ),
    "NA"
  )
}

fmt_pct <- function(
    x,
    digits = 2,
    show_sign = TRUE
) {
  
  if (show_sign) {
    
    ifelse(
      is.finite(x),
      sprintf(
        paste0("%+.", digits, "f%%"),
        x * 100
      ),
      "NA"
    )
    
  } else {
    
    ifelse(
      is.finite(x),
      sprintf(
        paste0("%.", digits, "f%%"),
        x * 100
      ),
      "NA"
    )
  }
}


annualized_return <- function(
    start_value,
    end_value,
    trading_days
) {
  
  if (
    !is.finite(start_value) ||
    !is.finite(end_value) ||
    start_value <= 0 ||
    end_value <= 0 ||
    trading_days <= 1
  ) {
    
    return(NA_real_)
  }
  
  (
    end_value / start_value
  ) ^ (
    TRADING_DAYS_PER_YEAR /
      trading_days
  ) - 1
}


drawdown_value <- function(x) {
  
  x <- as.numeric(x)
  
  high <- cummax(x)
  
  x / high - 1
}


nearest_date <- function(
    dates,
    target
) {
  
  dates <- sort(unique(as.Date(dates)))
  target <- as.Date(target)
  
  if (
    length(dates) == 0 ||
    is.na(target)
  ) {
    return(NA)
  }
  
  dates[
    which.min(
      abs(
        as.numeric(dates - target)
      )
    )
  ]
}



# ============================================================
# 3-1. 統計評分與風險共用函數（融合版新增）
# ============================================================

# 條件風險值（CVaR / Expected Shortfall）
# alpha = 0.95 代表觀察最差 5% 日對數報酬的平均值。
cvar_value <- function(x, alpha = 0.95) {
  x <- x[is.finite(x)]
  if (length(x) < 20) return(NA_real_)
  cutoff <- as.numeric(
    quantile(
      x,
      probs = 1 - alpha,
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )
  )
  tail_x <- x[x <= cutoff]
  if (length(tail_x) == 0) return(NA_real_)
  mean(tail_x, na.rm = TRUE)
}

# 將橫斷面指標轉成 0–100 分。
# higher_better = TRUE：數值越大分數越高。
# higher_better = FALSE：數值越小分數越高。
cross_section_score <- function(x, higher_better = TRUE) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  n_ok <- sum(ok)
  if (n_ok == 0) return(out)
  if (n_ok == 1) {
    out[ok] <- 50
    return(out)
  }
  r <- rank(x[ok], ties.method = "average")
  s <- (r - 1) / (n_ok - 1) * 100
  if (!higher_better) s <- 100 - s
  out[ok] <- s
  out
}

# 平均輪廓係數，不額外依賴其他套件。
average_silhouette <- function(x, cluster) {
  x <- as.matrix(x)
  if (nrow(x) < 3 || length(unique(cluster)) < 2) return(NA_real_)
  d <- as.matrix(dist(x))
  s <- rep(NA_real_, nrow(x))
  groups <- sort(unique(cluster))
  for (i in seq_len(nrow(x))) {
    same <- which(cluster == cluster[i] & seq_len(nrow(x)) != i)
    a <- if (length(same) > 0) mean(d[i, same]) else 0
    other_means <- sapply(
      groups[groups != cluster[i]],
      function(g) mean(d[i, cluster == g])
    )
    b <- min(other_means)
    denom <- max(a, b)
    s[i] <- if (is.finite(denom) && denom > 0) (b - a) / denom else 0
  }
  mean(s, na.rm = TRUE)
}

# 安全格式：0–100 分
fmt_score <- function(x, digits = 1) {
  ifelse(
    is.finite(x),
    sprintf(paste0("%.", digits, "f"), x),
    "NA"
  )
}


# ============================================================
# 3-2. 技術狀態、區塊拔靴法與歷史回測共用函數（v2 新增）
# ============================================================

# RSI（14 日）簡化計算：僅作為市場狀態輔助，不作買賣建議。
rsi_value <- function(price, n = 14) {
  price <- price[is.finite(price)]
  if (length(price) < n + 1) return(NA_real_)
  d <- diff(tail(price, n + 1))
  gain <- mean(pmax(d, 0), na.rm = TRUE)
  loss <- mean(pmax(-d, 0), na.rm = TRUE)
  if (!is.finite(gain) || !is.finite(loss)) return(NA_real_)
  if (loss == 0 && gain > 0) return(100)
  if (gain == 0 && loss == 0) return(50)
  rs <- gain / loss
  100 - 100 / (1 + rs)
}

# 產生區塊拔靴法索引。區塊會保留連續數日的結構。
block_bootstrap_indices <- function(n, block_length = 10) {
  if (n <= 1) return(seq_len(n))
  block_length <- max(2, min(as.integer(block_length), n))
  starts <- seq_len(max(1, n - block_length + 1))
  idx <- integer(0)
  while (length(idx) < n) {
    s <- sample(starts, 1)
    idx <- c(idx, s:min(n, s + block_length - 1))
  }
  idx[seq_len(n)]
}

# 由「日期 × 股票」的日對數報酬矩陣計算研究分數。
# 用於區塊拔靴法與歷史模型驗證；與主排名共用相同時間定義。
score_return_matrix <- function(
    return_matrix,
    composite_weights = LEGACY_COMPOSITE_WEIGHTS
) {
  return_matrix <- as.matrix(return_matrix)
  if (nrow(return_matrix) < 63 || ncol(return_matrix) < 2) return(data.frame())
  
  symbols <- colnames(return_matrix)
  if (is.null(symbols)) symbols <- paste0("S", seq_len(ncol(return_matrix)))
  
  period_days <- ANALYSIS_WINDOWS
  period_weights <- RETURN_PERIOD_WEIGHTS
  
  # 主風險 / 夏普只取最後 1 年「共同矩陣列」，
  # 再檢查各股票在同一段日期內有多少有效觀測。
  base_rows <- lapply(seq_along(symbols), function(j) {
    
    base_row_count <- min(
      SCORING_BASE_DAYS,
      nrow(return_matrix)
    )
    
    r_window <- tail(
      return_matrix[, j],
      base_row_count
    )
    
    r <- r_window[
      is.finite(r_window)
    ]
    
    min_required <- max(
      21,
      floor(
        base_row_count * 0.75
      )
    )
    
    if (length(r) < min_required) {
      return(data.frame(
        symbol = symbols[j],
        n_obs = length(r),
        annual_return = NA_real_,
        annual_volatility = NA_real_,
        max_drawdown = NA_real_,
        cvar_95 = NA_real_,
        mean_daily_log = NA_real_,
        sharpe = NA_real_
      ))
    }
    
    wealth <- exp(cumsum(r))
    ann_vol <- sd(r, na.rm = TRUE) * sqrt(TRADING_DAYS_PER_YEAR)
    mean_daily <- mean(r, na.rm = TRUE)
    
    data.frame(
      symbol = symbols[j],
      n_obs = length(r),
      annual_return = exp(mean_daily * TRADING_DAYS_PER_YEAR) - 1,
      annual_volatility = ann_vol,
      max_drawdown = min(drawdown_value(wealth), na.rm = TRUE),
      cvar_95 = cvar_value(r, 0.95),
      mean_daily_log = mean_daily,
      sharpe = ifelse(
        is.finite(ann_vol) && ann_vol > 0,
        mean_daily * TRADING_DAYS_PER_YEAR / ann_vol,
        NA_real_
      ),
      stringsAsFactors = FALSE
    )
  })
  
  base <- bind_rows(base_rows)
  
  period_long <- bind_rows(lapply(names(period_days), function(period_name) {
    n_day <- period_days[[period_name]]
    
    rows <- lapply(seq_along(symbols), function(j) {
      
      row_count <- min(
        n_day,
        nrow(return_matrix)
      )
      
      r_window <- tail(
        return_matrix[, j],
        row_count
      )
      
      rr <- r_window[
        is.finite(r_window)
      ]
      
      min_required <- max(
        3,
        floor(
          row_count * 0.75
        )
      )
      
      if (length(rr) < min_required) {
        return(data.frame(
          symbol = symbols[j],
          period = period_name,
          period_log_return = NA_real_,
          annualized_log_mean = NA_real_,
          annualized_volatility = NA_real_
        ))
      }
      
      data.frame(
        symbol = symbols[j],
        period = period_name,
        period_log_return = sum(rr, na.rm = TRUE),
        annualized_log_mean = mean(rr, na.rm = TRUE) * TRADING_DAYS_PER_YEAR,
        # 1 週樣本太少，不強行拿來估計風險穩定性。
        annualized_volatility = ifelse(
          n_day >= 21 && length(rr) >= 15,
          sd(rr, na.rm = TRUE) * sqrt(TRADING_DAYS_PER_YEAR),
          NA_real_
        ),
        stringsAsFactors = FALSE
      )
    })
    
    bind_rows(rows)
  })) %>%
    group_by(period) %>%
    mutate(return_percentile = cross_section_score(period_log_return, TRUE)) %>%
    ungroup()
  
  stability_raw <- period_long %>%
    group_by(symbol) %>%
    summarise(
      rank_variation = ifelse(
        sum(is.finite(return_percentile)) >= 3,
        sd(return_percentile, na.rm = TRUE),
        NA_real_
      ),
      return_variation = ifelse(
        sum(is.finite(annualized_log_mean)) >= 3,
        sd(annualized_log_mean, na.rm = TRUE),
        NA_real_
      ),
      risk_variation = ifelse(
        sum(is.finite(annualized_volatility)) >= 2,
        sd(annualized_volatility, na.rm = TRUE),
        NA_real_
      ),
      valid_periods = sum(is.finite(period_log_return)),
      .groups = "drop"
    ) %>%
    mutate(
      rank_stability_score = cross_section_score(rank_variation, FALSE),
      return_stability_score = cross_section_score(return_variation, FALSE),
      risk_stability_score = cross_section_score(risk_variation, FALSE),
      stability_score =
        0.30 * rank_stability_score +
        0.40 * return_stability_score +
        0.30 * risk_stability_score
    )
  
  multi_return <- period_long %>%
    mutate(weight = period_weights[period]) %>%
    group_by(symbol) %>%
    summarise(
      multi_period_return_score = ifelse(
        sum(is.finite(return_percentile)) >= 3,
        weighted.mean(return_percentile, weight, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    )
  
  momentum_6m <- period_long %>%
    filter(period == "6個月") %>%
    transmute(
      symbol,
      momentum_6m_score = return_percentile
    )
  
  base %>%
    mutate(
      annual_return_score = cross_section_score(annual_return, TRUE),
      volatility_score = cross_section_score(annual_volatility, FALSE),
      drawdown_score = cross_section_score(max_drawdown, TRUE),
      cvar_score = cross_section_score(cvar_95, TRUE),
      sharpe_score = cross_section_score(sharpe, TRUE)
    ) %>%
    left_join(stability_raw, by = "symbol") %>%
    left_join(multi_return, by = "symbol") %>%
    left_join(momentum_6m, by = "symbol") %>%
    mutate(
      return_score = 0.70 * multi_period_return_score + 0.30 * annual_return_score,
      risk_score = 0.35 * volatility_score + 0.35 * drawdown_score + 0.30 * cvar_score,
      growth_score =
        GROWTH_WEIGHTS[["ret"]] * return_score +
        GROWTH_WEIGHTS[["sharpe"]] * sharpe_score,
      composite_score =
        composite_weights[["stability"]] * stability_score +
        composite_weights[["ret"]] * return_score +
        composite_weights[["risk"]] * risk_score +
        composite_weights[["sharpe"]] * sharpe_score
    )
}


# 將指定期間內每一檔股票整理成 PCA / 分群需要的統計特徵。
# 一檔股票 = 一列；所有特徵都來自同一個選定期間。
compute_pca_features <- function(stock_data, window_days) {
  
  window_days <- as.integer(window_days)
  
  global_dates <- stock_data %>%
    filter(
      symbol %in% stock_info$symbol,
      is.finite(log_return)
    ) %>%
    pull(date) %>%
    unique() %>%
    sort()
  
  if (length(global_dates) < 20) return(data.frame())
  
  window_start_index <- max(
    1,
    length(global_dates) - window_days + 1
  )
  
  window_start <- global_dates[window_start_index]
  window_end <- tail(global_dates, 1)
  
  symbols <- intersect(
    stock_info$symbol,
    unique(stock_data$symbol)
  )
  
  base <- bind_rows(lapply(symbols, function(s) {
    
    z <- stock_data %>%
      filter(
        symbol == s,
        date >= window_start,
        date <= window_end,
        is.finite(log_return),
        is.finite(adjusted)
      ) %>%
      arrange(date)
    
    min_required <- max(
      20,
      floor(
        min(
          window_days,
          length(global_dates)
        ) * 0.80
      )
    )
    
    if (nrow(z) < min_required) {
      return(NULL)
    }
    
    r <- z$log_return
    vol <- sd(
      r,
      na.rm = TRUE
    ) *
      sqrt(
        TRADING_DAYS_PER_YEAR
      )
    
    data.frame(
      symbol = s,
      name = get_stock_name(s),
      industry = get_stock_industry(s),
      n_obs = nrow(z),
      start_date = window_start,
      end_date = window_end,
      annual_return =
        exp(
          mean(
            r,
            na.rm = TRUE
          ) *
            TRADING_DAYS_PER_YEAR
        ) - 1,
      annual_volatility = vol,
      max_drawdown =
        min(
          drawdown_value(
            z$adjusted
          ),
          na.rm = TRUE
        ),
      cvar_95 =
        cvar_value(
          r,
          0.95
        ),
      sharpe =
        ifelse(
          is.finite(vol) &&
            vol > 0,
          mean(
            r,
            na.rm = TRUE
          ) *
            TRADING_DAYS_PER_YEAR /
            vol,
          NA_real_
        ),
      stringsAsFactors = FALSE
    )
  }))
  
  if (nrow(base) < 6) return(data.frame())
  
  usable_periods <-
    ANALYSIS_WINDOWS[
      ANALYSIS_WINDOWS <= window_days
    ]
  
  period_long <- bind_rows(
    lapply(
      names(usable_periods),
      function(period_name) {
        
        n_day <-
          usable_periods[[
            period_name
          ]]
        
        sub_start_index <- max(
          1,
          length(global_dates) - n_day + 1
        )
        
        sub_start <-
          global_dates[
            sub_start_index
          ]
        
        rows <- lapply(
          base$symbol,
          function(s) {
            
            z <- stock_data %>%
              filter(
                symbol == s,
                date >= sub_start,
                date <= window_end,
                is.finite(log_return)
              ) %>%
              arrange(date)
            
            target_days <- min(
              n_day,
              length(global_dates)
            )
            
            min_required <- max(
              3,
              floor(
                target_days * 0.75
              )
            )
            
            if (
              nrow(z) <
              min_required
            ) {
              return(NULL)
            }
            
            data.frame(
              symbol = s,
              period = period_name,
              period_log_return =
                sum(
                  z$log_return,
                  na.rm = TRUE
                ),
              annualized_log_mean =
                mean(
                  z$log_return,
                  na.rm = TRUE
                ) *
                TRADING_DAYS_PER_YEAR,
              annualized_volatility =
                ifelse(
                  n_day >= 21 &&
                    nrow(z) >= 15,
                  sd(
                    z$log_return,
                    na.rm = TRUE
                  ) *
                    sqrt(
                      TRADING_DAYS_PER_YEAR
                    ),
                  NA_real_
                ),
              stringsAsFactors = FALSE
            )
          }
        )
        
        bind_rows(
          rows
        )
      }
    )
  )
  
  if (
    nrow(period_long) == 0
  ) {
    return(
      data.frame()
    )
  }
  
  period_long <-
    period_long %>%
    group_by(
      period
    ) %>%
    mutate(
      return_percentile =
        cross_section_score(
          period_log_return,
          TRUE
        )
    ) %>%
    ungroup()
  
  stability <-
    period_long %>%
    group_by(
      symbol
    ) %>%
    summarise(
      rank_variation =
        ifelse(
          sum(
            is.finite(
              return_percentile
            )
          ) >= 3,
          sd(
            return_percentile,
            na.rm = TRUE
          ),
          NA_real_
        ),
      return_variation =
        ifelse(
          sum(
            is.finite(
              annualized_log_mean
            )
          ) >= 3,
          sd(
            annualized_log_mean,
            na.rm = TRUE
          ),
          NA_real_
        ),
      risk_variation =
        ifelse(
          sum(
            is.finite(
              annualized_volatility
            )
          ) >= 2,
          sd(
            annualized_volatility,
            na.rm = TRUE
          ),
          NA_real_
        ),
      .groups = "drop"
    ) %>%
    mutate(
      rank_stability_score =
        cross_section_score(
          rank_variation,
          FALSE
        ),
      return_stability_score =
        cross_section_score(
          return_variation,
          FALSE
        ),
      risk_stability_score =
        cross_section_score(
          risk_variation,
          FALSE
        )
    )
  
  base %>%
    left_join(
      stability,
      by = "symbol"
    )
}


# 權重敏感度：模型驗證時可切換三組設定。
get_weight_preset <- function(x) {
  switch(
    as.character(x),
    "balanced" = c(stability = 0.35, ret = 0.30, risk = 0.20, sharpe = 0.15),
    "return_tilt" = c(stability = 0.30, ret = 0.35, risk = 0.20, sharpe = 0.15),
    c(stability = 0.40, ret = 0.25, risk = 0.20, sharpe = 0.15)
  )
}


# 驗證期間內的等權組合統計。
# 這裡目的不是模擬下單，而是比較高分組與低分組的後續表現。
validation_group_metrics <- function(future_data, symbols) {
  
  z <- future_data %>%
    filter(
      symbol %in% symbols,
      is.finite(log_return)
    ) %>%
    mutate(simple_return = exp(log_return) - 1) %>%
    group_by(date) %>%
    summarise(
      daily_return = mean(simple_return, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(date)
  
  if (nrow(z) < 2) {
    return(c(
      future_return = NA_real_,
      annual_volatility = NA_real_,
      max_drawdown = NA_real_,
      sharpe = NA_real_,
      n_days = nrow(z)
    ))
  }
  
  wealth <- cumprod(1 + z$daily_return)
  future_return <- tail(wealth, 1) - 1
  
  ann_vol <- if (nrow(z) >= 21) {
    sd(log1p(z$daily_return), na.rm = TRUE) * sqrt(TRADING_DAYS_PER_YEAR)
  } else {
    NA_real_
  }
  
  sharpe <- if (is.finite(ann_vol) && ann_vol > 0) {
    mean(log1p(z$daily_return), na.rm = TRUE) *
      TRADING_DAYS_PER_YEAR / ann_vol
  } else {
    NA_real_
  }
  
  c(
    future_return = future_return,
    annual_volatility = ann_vol,
    max_drawdown = min(drawdown_value(wealth), na.rm = TRUE),
    sharpe = sharpe,
    n_days = nrow(z)
  )
}


# 將長表資料轉為日期 × 股票的日對數報酬矩陣。
return_wide_matrix <- function(x, symbols = NULL) {
  if (!is.null(symbols)) x <- x %>% filter(symbol %in% symbols)
  wide <- x %>%
    filter(is.finite(log_return)) %>%
    select(date, symbol, log_return) %>%
    distinct(date, symbol, .keep_all=TRUE) %>%
    pivot_wider(names_from=symbol, values_from=log_return) %>%
    arrange(date)
  if (nrow(wide) == 0 || ncol(wide) < 3) return(NULL)
  m <- as.matrix(wide[, -1, drop=FALSE])
  storage.mode(m) <- "double"
  m
}

# 60 日滾動相關性：使用移動加總公式，避免每一天都重新呼叫 cor()。
rolling_correlation_fast <- function(x, y, window = 60) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x[!ok] <- NA_real_
  y[!ok] <- NA_real_
  roll_sum <- function(z) as.numeric(stats::filter(z, rep(1, window), sides = 1))
  n_obs <- roll_sum(as.numeric(ok))
  x0 <- ifelse(ok, x, 0)
  y0 <- ifelse(ok, y, 0)
  sx <- roll_sum(x0)
  sy <- roll_sum(y0)
  sxx <- roll_sum(x0 * x0)
  syy <- roll_sum(y0 * y0)
  sxy <- roll_sum(x0 * y0)
  cov_num <- sxy - sx * sy / n_obs
  vx <- sxx - sx * sx / n_obs
  vy <- syy - sy * sy / n_obs
  denom <- sqrt(vx * vy)
  out <- cov_num / denom
  out[n_obs < window | !is.finite(out)] <- NA_real_
  out
}

# ============================================================
# 4. 資料整理函數
# ============================================================

prepare_stock_data <- function(raw_data) {
  
  if (!is.data.frame(raw_data)) {
    stop("stock_data.rds 不是 data.frame。")
  }
  
  required <- c(
    "date",
    "symbol"
  )
  
  missing_cols <-
    setdiff(
      required,
      names(raw_data)
    )
  
  if (length(missing_cols) > 0) {
    
    stop(
      paste0(
        "資料缺少必要欄位：",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }
  
  x <- raw_data
  
  
  # ----------------------------------------------------------
  # 日期與股票代號
  # ----------------------------------------------------------
  
  x$date <- convert_to_date(
    x$date
  )
  
  if (is.numeric(x$symbol)) {
    
    x$symbol <- sprintf(
      "%04d",
      as.integer(x$symbol)
    )
    
  } else {
    
    x$symbol <- as.character(
      x$symbol
    )
  }
  
  
  # ----------------------------------------------------------
  # 調整後價格
  # ----------------------------------------------------------
  
  if ("adjusted" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$adjusted
    )
    
  } else if ("price.adjusted" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$price.adjusted
    )
    
  } else if ("close" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$close
    )
    
  } else if ("price.close" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$price.close
    )
    
  } else {
    
    stop(
      "找不到 adjusted / price.adjusted / close / price.close。"
    )
  }
  
  
  # ----------------------------------------------------------
  # 普通收盤價
  # ----------------------------------------------------------
  
  if ("close" %in% names(x)) {
    
    x$close <- safe_numeric(
      x$close
    )
    
  } else if ("price.close" %in% names(x)) {
    
    x$close <- safe_numeric(
      x$price.close
    )
    
  } else {
    
    x$close <- x$adjusted
  }
  
  
  # ----------------------------------------------------------
  # 成交量
  # ----------------------------------------------------------
  
  if ("volume" %in% names(x)) {
    
    x$volume <- safe_numeric(
      x$volume
    )
    
  } else if ("price.volume" %in% names(x)) {
    
    x$volume <- safe_numeric(
      x$price.volume
    )
    
  } else {
    
    x$volume <- NA_real_
  }
  
  
  # ----------------------------------------------------------
  # 股票名稱與產業
  # ----------------------------------------------------------
  
  x$name <- vapply(
    x$symbol,
    get_stock_name,
    character(1)
  )
  
  x$industry <- vapply(
    x$symbol,
    get_stock_industry,
    character(1)
  )
  
  x$industry_group <-
    ifelse(
      x$industry %in%
        major_industries,
      x$industry,
      "其他"
    )
  
  
  # ----------------------------------------------------------
  # 基本清理
  # ----------------------------------------------------------
  
  x <- x %>%
    filter(
      !is.na(date),
      !is.na(symbol)
    ) %>%
    arrange(
      symbol,
      date
    ) %>%
    distinct(
      symbol,
      date,
      .keep_all = TRUE
    ) %>%
    mutate(
      adjusted = ifelse(
        is.finite(adjusted) &
          adjusted > 0,
        adjusted,
        NA_real_
      ),
      close = ifelse(
        is.finite(close) &
          close > 0,
        close,
        NA_real_
      )
    )
  
  
  # ----------------------------------------------------------
  # 日報酬
  # ----------------------------------------------------------
  
  x <- x %>%
    group_by(symbol) %>%
    arrange(
      date,
      .by_group = TRUE
    ) %>%
    mutate(
      
      daily_return =
        adjusted /
        lag(adjusted) - 1,
      
      log_return =
        log(
          adjusted /
            lag(adjusted)
        )
      
    ) %>%
    ungroup() %>%
    arrange(
      symbol,
      date
    )
  
  
  if (!"0050" %in% x$symbol) {
    stop("資料中找不到 0050。")
  }
  
  x
}


# ============================================================
# 5. 載入初始資料
# ============================================================

if (!file.exists(DATA_FILE)) {
  
  stop(
    paste0(
      "找不到資料檔：",
      DATA_FILE,
      "\n請確認 data/stock_data.rds 存在。"
    )
  )
}

initial_data <- prepare_stock_data(
  readRDS(DATA_FILE)
)

data_min_date <- min(
  initial_data$date,
  na.rm = TRUE
)

data_max_date <- max(
  initial_data$date,
  na.rm = TRUE
)

available_symbols <- sort(
  unique(
    initial_data$symbol
  )
)

dashboard_symbols <- c(
  "0050",
  stock_info$symbol
)

dashboard_symbols <-
  dashboard_symbols[
    dashboard_symbols %in%
      available_symbols
  ]

dashboard_stock_choices <-
  setNames(
    dashboard_symbols,
    vapply(
      dashboard_symbols,
      get_stock_label,
      character(1)
    )
  )
portfolio_default_end_date <- data_max_date

portfolio_default_start_date <- max(
  data_min_date,
  data_max_date - 365
)

# ============================================================
# 共用顏色
# ============================================================

UP_COLOR <- "#d95b5b"
DOWN_COLOR <- "#4fa66a"
NEUTRAL_COLOR <- "#7b8794"

# ============================================================
# 代表色系統
# ============================================================

# 0050 固定使用中等藍色，其餘股票依固定順序產生代表色。
# 因此只要股票代號相同，同一檔股票在所有圖形都會維持相同顏色。

other_stock_symbols <-
  setdiff(
    dashboard_symbols,
    "0050"
  )

stock_colors <- c(
  "0050" = "#5b8ecb"
)

if (
  length(other_stock_symbols) > 0
) {
  
  stock_colors[
    other_stock_symbols
  ] <-
    hcl.colors(
      length(other_stock_symbols),
      palette = "Dynamic"
    )
  
}


# ------------------------------------------------------------
# 產業分類
#
# 少於 4 檔成分股的產業全部合併為「其他」。
# ------------------------------------------------------------

stock_info$industry_group <- industry_group(
  stock_info$industry
)

available_industries <- sort(
  unique(
    stock_info$industry_group
  )
)

# ============================================================
# 產業重新分組
# 少於 4 檔的產業全部合併為「其他」
# ============================================================

industry_count <-
  table(
    stock_info$industry
  )

major_industries <-
  names(
    industry_count[
      industry_count >= 4
    ]
  )

stock_info$industry_group <-
  ifelse(
    stock_info$industry %in%
      major_industries,
    stock_info$industry,
    "其他"
  )

available_industries <-
  sort(
    unique(
      stock_info$industry_group
    )
  )


# ------------------------------------------------------------
# 產業代表色
# ------------------------------------------------------------

industry_colors <-
  setNames(
    hcl.colors(
      length(available_industries),
      palette = "Dynamic"
    ),
    available_industries
  )

if (
  "其他" %in% names(industry_colors)
) {
  
  industry_colors[
    "其他"
  ] <- "gray45"
  
}


# ============================================================
# 6. 共用產業
# ============================================================

available_industries <- sort(
  unique(
    stock_info$industry_group
  )
)

# ============================================================
# 7. UI
# ============================================================

ui <- fluidPage(
  
  lang = "zh-Hant",
  
  tags$head(
    
    tags$style(
      
      HTML(
        
        "
        :root {
          --blue:#5b8ecb;
          --blue2:#82b5e8;
          --purple:#9a7fd1;
          --green:#65a77b;
          --amber:#dca75a;
          --red:#d95b5b;
          --ink:#334155;
          --muted:#6b7b8d;
          --bg:#f6f9fc;
          --card:#ffffff;
          --line:#e1e8f0;
        }

        body {
          background:linear-gradient(180deg,#f3f8fd 0,#f8fafc 260px,#f6f9fc 100%);
          color:var(--ink);
          font-family:-apple-system,BlinkMacSystemFont,'Segoe UI','Microsoft JhengHei',sans-serif;
        }

        .container-fluid { padding:22px 32px 42px 32px; max-width:1600px; }

        .hero {
          background:linear-gradient(120deg,#eaf4ff 0%,#e7f1fb 55%,#edf0ff 100%);
          color:#29445f;
          border:1px solid #d5e4f2;
          border-radius:20px;
          padding:26px 30px;
          margin-bottom:18px;
          box-shadow:0 8px 24px rgba(72,105,140,.10);
        }
        .dashboard-title { font-size:32px; font-weight:800; letter-spacing:.3px; margin-bottom:6px; color:#24415f; }
        .dashboard-subtitle { color:#526b82; margin-bottom:0; font-size:16px; line-height:1.7; }
        .hero-badge { display:inline-block; margin-top:12px; padding:6px 11px; border-radius:999px; background:rgba(255,255,255,.78); color:#56708a; border:1px solid #d4e2ef; font-size:13px; }

        .nav-tabs {
          margin-bottom:22px; border-bottom:0; background:white; padding:8px;
          border-radius:14px; box-shadow:0 4px 14px rgba(72,105,140,.07);
        }
        .nav-tabs > li > a { font-weight:700; color:#5d6f81; border:0!important; border-radius:10px; margin:2px; }
        .nav-tabs > li > a:hover { background:#eef6fd; color:#477cad; }
        .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
          background:#6f9fd0; color:white; border:0!important;
          box-shadow:0 4px 10px rgba(91,142,203,.18);
        }

        .section-box {
          background:rgba(255,255,255,.98); border:1px solid var(--line); border-radius:18px;
          padding:26px; margin-bottom:22px; box-shadow:0 6px 20px rgba(72,105,140,.06);
        }
        .section-box h2 { font-weight:800; color:#29445f; margin-top:0; }
        .section-box h3 { font-weight:750; color:#3d5e7e; margin-top:26px; }
        .page-description { color:#60758a; line-height:1.8; font-size:15px; }

        .reading-guide {
          background:linear-gradient(90deg,#f1f7fd,#f5f3fb); border-left:5px solid #7fa9d3;
          padding:15px 18px; border-radius:10px; margin:15px 0 20px; line-height:1.75;
        }
        .plot-explanation {
          background:#fafbfc; border:1px solid #e4e9ef; border-left:4px solid #a18bd0;
          border-radius:10px; padding:13px 16px; margin:10px 0 20px; line-height:1.75; color:#56697a;
        }
        .method-note {
          background:#fffaf2; border:1px solid #f1dfc4; border-left:4px solid #dca75a;
          border-radius:10px; padding:14px 16px; margin:12px 0 18px; line-height:1.75;
        }

        .info-card {
          background:#ffffff; border:1px solid #e0e8f0;
          border-radius:15px; padding:17px 18px; min-height:112px; margin-bottom:15px;
          box-shadow:0 4px 13px rgba(72,105,140,.06); transition:transform .15s ease, box-shadow .15s ease;
        }
        .info-card:hover { transform:translateY(-2px); box-shadow:0 7px 17px rgba(72,105,140,.09); }
        .info-card-title { color:#718195; font-size:13px; font-weight:700; margin-bottom:8px; }
        .info-card-value { font-size:25px; font-weight:800; color:#385775; }
        .accent-blue { border-top:4px solid #78a7d5; }
        .accent-purple { border-top:4px solid #a28bd2; }
        .accent-green { border-top:4px solid #7caf8c; }
        .accent-amber { border-top:4px solid #dfb46f; }
        .accent-red { border-top:4px solid #de7777; }

        .data-status { background:#f9fbfd; border:1px solid #e0e8f0; border-radius:12px; padding:16px; line-height:1.85; }
        .warning-note { background:#fffaf0; border-left:5px solid #dfb46f; padding:13px 16px; border-radius:8px; margin:10px 0; line-height:1.7; }
        .quick-button-row { margin:10px 0 15px; }
        .quick-button-row .btn {
          margin-right:6px; margin-bottom:6px; border-radius:9px;
          background:#ffffff; border:1px solid #cfd9e4; color:#4d6277;
          box-shadow:none;
        }
        .quick-button-row .btn:hover { background:#f2f7fb; border-color:#9eb8d0; color:#365b7d; }
        .btn-primary { border-radius:9px; background:#6f9fd0; border-color:#6f9fd0; }
        .btn-primary:hover { background:#5f8fbe; border-color:#5f8fbe; }
        .time-info {
          background:#f7fbff; border:1px solid #dbe8f3; border-left:4px solid #7fa9d3;
          border-radius:10px; padding:12px 15px; margin:12px 0 18px; color:#526b82; line-height:1.7;
        }

        table { background:white; }
        .table > thead > tr > th { background:#f0f5fa; color:#49647e; border-bottom:2px solid #d6e1eb; }
        .table-striped > tbody > tr:nth-of-type(odd) { background:#ffffff; }
        .table-hover > tbody > tr:hover { background:#f7fafc; }

        details.method-details { background:#fbfcfd; border:1px solid #e1e8f0; border-radius:12px; padding:12px 16px; margin:10px 0; }
        details.method-details summary { cursor:pointer; font-weight:750; color:#527fae; }
        .term-chip { display:inline-block; padding:5px 9px; margin:3px; background:#f0f4fb; color:#6d6298; border-radius:999px; font-size:12px; font-weight:700; }

        .dashboard-footer { text-align:center; color:#8b99a8; font-size:13px; padding-top:22px; }

        /* v8：首頁視覺微調，所有其他分頁樣式維持原樣 */
        .v8-up { color:#d94b4b !important; font-weight:800; }
        .v8-down { color:#238a58 !important; font-weight:800; }
        .v8-flat { color:#5e7185 !important; }
        .home-data-foot p { margin-bottom:5px; }
        .home-intro .home-side { min-height:385px; }
        @media (max-width:700px) { .home-intro .home-side { min-height:0; } }
        /* v7 首頁：左右對照，只影響總覽新元件 */
        .home-heading { display:flex; justify-content:space-between; align-items:flex-start; gap:16px; }
        .home-heading h2 { margin:0; font-size:clamp(32px,2.8vw,40px); line-height:1.1; letter-spacing:1px; }
        .home-date { color:#60758a; font-size:13px; white-space:nowrap; padding-top:6px; }
        .home-qualifier { background:#f6f8fa; padding:12px 15px; border-radius:9px; font-size:13px; line-height:1.7; color:#556779; margin:16px 0 8px; }
        .home-window { font-size:13px; color:#60758a; margin-bottom:14px; }
        .home-compare-grid { display:grid; grid-template-columns:minmax(0,2fr) minmax(160px,1fr) minmax(0,2fr); gap:13px; }
        .home-side { border:1px solid #dfe7ee; border-radius:13px; padding:18px 20px; min-width:0; }
        .home-side-0010 { background:#eef5fc; border-color:#d6e5f3; }
        .home-side-0050 { background:#fff5f1; border-color:#f1e0d9; }
        .home-center { background:#f8fafc; text-align:center; padding-left:11px; padding-right:11px; }
        .home-side h2 { font-size:34px; font-weight:800; text-align:center; margin:0 0 3px; }
        .home-side-subtitle { text-align:center; color:#708092; font-size:12px; min-height:21px; margin:0 0 12px; }
        .home-stat { border-top:1px solid rgba(130,148,168,.20); padding:9px 0; display:flex; justify-content:space-between; align-items:center; gap:8px; }
        .home-stat > span { font-size:13px; color:#677789; }
        .home-stat > strong { font-size:32px !important; color:#34465a; white-space:nowrap; line-height:1.25; }
        /* Shiny uiOutput() 會產生巢狀 span：顯示數字必須繼承 strong 的字級，而不是被標籤 span 的 13px 覆蓋。 */
        .home-compare-grid .home-stat > strong span { font-size:inherit !important; line-height:inherit; }
        .home-center h2 { font-size:34px; }
        .home-center .home-stat { display:block; text-align:center; }
        .home-center .home-stat > span { display:block; font-size:11px; margin-bottom:4px; }
        .home-center .home-stat > strong { font-size:26px !important; }
        /* 小螢幕才適度縮字，桌面及平板寬度不再強制蓋掉數值字體。 */
        @media(max-width:700px) { .home-stat > strong { font-size:28px !important; } .home-center .home-stat > strong { font-size:24px !important; } }
        /* v10：只影響「排名與分數／權重實驗」 */
        .rank-lab .rank-lab-ratio { font-size:28px; font-weight:800; color:#395d80; margin:3px 0 7px; }
        .rank-lab .rank-lab-hint { font-size:13px; color:#667a8e; line-height:1.7; }
        .rank-lab .rank-lab-actions .btn { margin-right:6px; margin-bottom:8px; }
        .rank-lab .rank-lab-formula { padding:12px 14px; border-radius:10px; background:#f2f6fa; margin:12px 0; }
        .rank-lab .rank-lab-section { margin-top:25px; }
        /* v12 排名第二頁：雙 Top10 長條圖＋可展開細項（只作用於此頁） */
        .rank-compare .rank-compare-lead { color:#465c72; font-size:16px; font-weight:600; line-height:1.55; margin:0 0 13px; }
        .rank-compare .rank-compare-date { margin:0 0 15px; }
        .rank-compare .rank-compare-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:16px; align-items:start; }
        .rank-compare .rank-compare-card { min-width:0; align-self:start; height:auto; border:1px solid #dce7f1; border-radius:14px; padding:17px 16px; background:#fff; display:flex; flex-direction:column; }
        .rank-compare .rank-compare-card.momentum { border-top:4px solid #5289c0; background:#f9fcff; }
        .rank-compare .rank-compare-card.risk { border-top:4px solid #d39651; background:#fffdf9; }
        .rank-compare .rank-compare-card h3 { font-size:22px; font-weight:800; color:#30495e; margin:0 0 6px; }
        .rank-compare .rank-compare-description { color:#597083; font-size:13px; margin:0 0 4px; line-height:1.5; }
        .rank-compare .rank-compare-chart { flex:0 0 auto; min-width:0; padding:4px 0 2px; }
        .rank-compare .rank-compare-chart .shiny-plot-output { max-width:100%; }
        .rank-compare .rank-compare-hint { font-size:12px; color:#71869a; margin:2px 0 15px; line-height:1.5; }
        .rank-compare details.rank-compare-expand { border:1px solid #cddce9; background:#fff; border-radius:10px; padding:0; margin-top:0; }
        .rank-compare details.rank-compare-expand > summary { cursor:pointer; list-style:none; text-align:center; padding:11px 9px; color:#3b648b; font-size:14px; font-weight:750; border-radius:10px; }
        .rank-compare details.rank-compare-expand > summary::-webkit-details-marker { display:none; }
        .rank-compare details.rank-compare-expand > summary:after { content:' ▾'; }
        .rank-compare details.rank-compare-expand[open] > summary:after { content:' ▴'; }
        .rank-compare details.rank-compare-expand[open] > summary { border-bottom:1px solid #dce7f1; border-radius:10px 10px 0 0; }
        .rank-compare .rank-compare-expanded { padding:13px 12px; min-width:0; overflow-x:auto; }
        .rank-compare .rank-compare-expanded .table { font-size:12px; }
        .rank-compare .rank-compare-expanded .table th, .rank-compare .rank-compare-expanded .table td { white-space:nowrap; }
        .rank-compare .rank-compare-weight { background:#f1f5f9; border-radius:9px; padding:11px 13px; font-size:13px; color:#465e77; margin:0 0 14px; line-height:1.7; }
        .rank-compare .rank-compare-section { font-size:14px; font-weight:800; color:#415e78; margin:9px 0 9px; }
        .rank-compare .rank-compare-flow { margin:17px 0 0; padding:16px; border:1px solid #dfebf4; border-radius:12px; background:#f9fbfd; }
        .rank-compare .rank-compare-flow h3 { font-size:16px; font-weight:800; margin:0 0 12px; }
        .rank-compare .rank-compare-steps { display:flex; gap:9px; align-items:center; }
        .rank-compare .rank-compare-step { flex:1; min-width:0; text-align:center; background:#fff; border:1px solid #e0e8f0; border-radius:9px; padding:11px 5px; }
        .rank-compare .rank-compare-step strong { display:block; color:#385a79; font-size:14px; }
        .rank-compare .rank-compare-step small { display:block; color:#738599; margin-top:4px; font-size:12px; }
        .rank-compare .rank-compare-arrow { flex:none; color:#7f97aa; font-size:20px; font-weight:700; }
        .rank-compare .rank-compare-note { color:#7a8996; font-size:12px; line-height:1.55; margin:12px 0 0; }
        @media(max-width:980px) { .rank-compare .rank-compare-grid { grid-template-columns:1fr; } }
        @media(max-width:570px) { .rank-compare .rank-compare-flow { padding:11px; } .rank-compare .rank-compare-steps { gap:4px; } .rank-compare .rank-compare-step { padding:8px 3px; } .rank-compare .rank-compare-step strong { font-size:12px; } .rank-compare .rank-compare-step small { font-size:10px; } .rank-compare .rank-compare-arrow { font-size:14px; } }
        /* v10：僅限 0010 組合呈現，所有模型計算維持現況 */
        .g10-v10-lead { color:#526b82; font-size:15px; line-height:1.6; margin:7px 0 17px; }
        .g10-v10-flow { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:10px; margin:0 0 23px; }
        .g10-v10-flow-step { background:#f6f9fc; border:1px solid #e2eaf2; border-radius:11px; padding:13px 12px; }
        .g10-v10-flow-step strong { display:block; color:#385b7c; font-size:14px; margin-bottom:5px; }
        .g10-v10-flow-step small { display:block; font-size:12px; color:#6d8193; line-height:1.45; }
        .g10-v10-title { font-size:21px; font-weight:800; color:#365471; margin:21px 0 12px; }
        .g10-v10-metrics { display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:11px; margin:8px 0 17px; }
        .g10-v10-metric { border:1px solid #dce8f1; border-radius:12px; background:#f9fbfe; padding:14px 13px; min-width:0; }
        .g10-v10-metric .g10-v10-label { display:block; color:#718396; font-size:12px; font-weight:700; }
        .g10-v10-metric .shiny-text-output { display:block; color:#365c7c; font-size:25px; font-weight:800; margin-top:7px; white-space:nowrap; }
        .g10-v10-main { display:grid; grid-template-columns:minmax(0,0.92fr) minmax(0,1.28fr); gap:15px; align-items:start; }
        .g10-v10-panel { border:1px solid #e0e9f1; background:#fff; border-radius:13px; padding:15px 14px; min-width:0; }
        .g10-v10-panel h4 { color:#405f7b; font-weight:750; font-size:16px; margin:0 0 10px; }
        .g10-v10-panel .table { font-size:13px; }
        .g10-v10-panel .table th,.g10-v10-panel .table td { white-space:nowrap; }
        .g10-v10-panel .table-responsive { overflow-x:auto; }
        .g10-v10-caption { color:#798a9c; font-size:12px; line-height:1.6; margin:10px 0 14px; }
        details.g10-v10-expand { border:1px solid #dee8f1; border-radius:11px; background:#fcfdff; margin:12px 0; overflow:hidden; }
        details.g10-v10-expand > summary { cursor:pointer; color:#446d95; font-weight:750; font-size:14px; padding:13px 16px; }
        details.g10-v10-expand[open] > summary { border-bottom:1px solid #e3eaf1; }
        .g10-v10-expand-body { padding:13px 16px 17px; overflow-x:auto; }
        .g10-v10-note { background:#f5f8fb; border-left:4px solid #88accd; padding:12px 15px; color:#61778b; font-size:13px; line-height:1.65; border-radius:8px; margin:14px 0; }
        .g10-v10-subtle { color:#72849a; font-size:13px; line-height:1.6; }
        @media(max-width:1160px) { .g10-v10-main { grid-template-columns:1fr; } }
        @media(max-width:750px) { .g10-v10-flow { grid-template-columns:1fr; } .g10-v10-metrics { grid-template-columns:repeat(2,minmax(0,1fr)); } .g10-v10-metric .shiny-text-output { font-size:21px; } }
        /* v11：0010 本期持股配置直接顯示於第一屏，長條由實際持股權重產生 */
        .g10-v11-head { margin:4px 0 13px; }
        .g10-v11-head h3 { color:#31516d; font-size:25px; font-weight:800; margin:0 0 7px; }
        .g10-v11-head p { color:#677b8d; font-size:14px; margin:0; line-height:1.5; }
        .g10-v11-layout { display:grid; grid-template-columns:minmax(0,1.05fr) minmax(0,1.25fr); gap:14px; align-items:start; margin:12px 0 9px; }
        .g10-v11-pane { border:1px solid #dbe7f0; border-radius:14px; padding:16px; background:#fff; min-width:0; }
        .g10-v11-pane h4 { margin:0 0 13px; color:#35536e; font-size:17px; font-weight:800; }
        .g10-v11-row { display:grid; grid-template-columns:minmax(118px,1.2fr) minmax(60px,2fr) 55px; align-items:center; gap:8px; margin:9px 0; min-height:24px; }
        .g10-v11-name { color:#365773; font-size:12px; line-height:1.35; overflow-wrap:anywhere; }
        .g10-v11-track { height:15px; width:100%; border-radius:5px; background:#eef3f8; overflow:hidden; }
        .g10-v11-fill { height:100%; border-radius:5px; background:#4a83bb; min-width:2px; }
        .g10-v11-value { font-size:13px; font-weight:800; text-align:right; color:#315b7d; white-space:nowrap; }
        .g10-v11-pane .table { font-size:12px; margin-bottom:5px; }
        .g10-v11-pane .table td,.g10-v11-pane .table th { white-space:nowrap; }
        .g10-v11-pane .table-responsive { overflow-x:auto; }
        .g10-v11-subnote { color:#7b8c9e; font-size:12px; margin:10px 0 0; line-height:1.6; }
        .g10-v11-minor { color:#778b9d; font-size:12px; margin:9px 0 13px; }
        .g10-v11-metrics-title { color:#466480; font-size:17px; font-weight:750; margin:19px 0 10px; }
        @media(max-width:1180px) { .g10-v11-layout { grid-template-columns:1fr; } }
        @media(max-width:570px) { .g10-v11-head h3 { font-size:21px; } .g10-v11-pane { padding:12px; } .g10-v11-row { grid-template-columns:minmax(91px,1.1fr) minmax(48px,1.6fr) 47px; gap:5px; } .g10-v11-name,.g10-v11-value { font-size:11px; } }
        /* v12：0010 子頁精簡排版。柔和藍灰＋暖色點綴，不修改其他頁面 */
        .g10-v12 { --pf-ink:#344b60; --pf-muted:#697e90; --pf-line:#dce6ed; --pf-blue:#527fa8; --pf-warm:#c4a07c; color:var(--pf-ink); }
        .g10-v12 .g10-v12-banner { display:flex; gap:16px; justify-content:space-between; align-items:center; padding:19px 22px; border:1px solid #dce7ef; border-radius:15px; background:linear-gradient(110deg,#f0f6fb 0%,#f7fafd 72%,#fbf7f2 100%); margin:0 0 16px; }
        .g10-v12 .g10-v12-eyebrow { display:inline-block; font-weight:750; font-size:11px; letter-spacing:1px; color:#6f91ac; margin-bottom:6px; }
        .g10-v12 .g10-v12-banner h3 { color:#2f526e; font-weight:800; font-size:25px; margin:0 0 7px; }
        .g10-v12 .g10-v12-banner p { color:#6d8191; line-height:1.55; font-size:13px; margin:0; }
        .g10-v12 .g10-v12-amount { background:#fff; border:1px solid #d9e5ee; border-radius:12px; padding:10px 14px; min-width:225px; width:240px; flex:none; box-shadow:0 2px 8px rgba(59,87,112,.035); }
        .g10-v12 .g10-v12-amount .form-group { margin:0; }
        .g10-v12 .g10-v12-amount label { font-size:12px; color:#59758d; font-weight:750; }
        .g10-v12 .g10-v12-amount input { border-color:#bfd1e0; font-size:18px; font-weight:750; color:#315674; background:#fcfdff; height:39px; }
        .g10-v12 .g10-v12-amount small { color:#8798a7; font-size:11px; display:block; margin-top:6px; line-height:1.4; }
        .g10-v12 .g10-v12-top-grid { display:grid; grid-template-columns:minmax(0,1.25fr) minmax(0,.95fr); gap:14px; align-items:start; }
        .g10-v12 .g10-v12-card { background:#fff; border:1px solid var(--pf-line); border-radius:14px; padding:17px 18px; min-width:0; }
        .g10-v12 .g10-v12-card h4 { color:#365770; font-size:17px; font-weight:800; margin:0 0 9px; }
        .g10-v12 .g10-v12-card-sub { color:#7e90a0; font-size:12px; margin:0 0 12px; line-height:1.6; }
        .g10-v12 .g10-v12-weight-card { border-top:3px solid #85a7c4; }
        .g10-v12 .g10-v11-row { grid-template-columns:minmax(120px,1.1fr) minmax(78px,2.1fr) 58px; margin:9px 0; }
        .g10-v12 .g10-v11-track { background:#ebf1f6; }
        .g10-v12 .g10-v11-fill { background:linear-gradient(90deg,#82a8c9,#527fa8); }
        .g10-v12 .g10-v12-side { display:flex; flex-direction:column; gap:13px; min-width:0; }
        .g10-v12 .g10-v10-metrics { grid-template-columns:repeat(2,minmax(0,1fr)); margin:4px 0 0; gap:10px; }
        .g10-v12 .g10-v10-metric { background:#f6f9fc; border-color:#e3ebf1; padding:14px 12px; }
        .g10-v12 .g10-v10-metric:nth-child(3) { background:#f8f9f8; }
        .g10-v12 .g10-v10-metric:nth-child(4) { background:#fbf8f4; }
        .g10-v12 .g10-v10-metric .shiny-text-output { font-size:22px; }
        .g10-v12 .g10-v12-mini-flow { background:#f7fafc; border:1px solid #e2e9ef; padding:13px 14px; border-radius:12px; }
        .g10-v12 .g10-v12-mini-flow strong { display:block; font-size:12px; color:#54738e; margin-bottom:8px; }
        .g10-v12 .g10-v12-steps { display:flex; align-items:center; gap:7px; }
        .g10-v12 .g10-v12-steps span { border:1px solid #dee8ef; background:#fff; color:#547188; border-radius:7px; padding:7px 5px; flex:1; text-align:center; font-weight:700; font-size:12px; white-space:nowrap; }
        .g10-v12 .g10-v12-steps i { color:#8ca6b7; font-size:14px; font-style:normal; }
        .g10-v12 .g10-v12-table-card { margin-top:15px; border-top:3px solid #b2bfc9; }
        .g10-v12 .g10-v12-table-wrap { overflow-x:auto; }
        .g10-v12 .g10-v12-table-wrap .table { font-size:13px; margin:0; }
        .g10-v12 .g10-v12-table-wrap .table th,.g10-v12 .g10-v12-table-wrap .table td { white-space:nowrap; padding:9px 11px; }
        .g10-v12 .g10-v12-table-wrap .table > thead > tr > th { background:#f0f5f8; color:#48647e; border-bottom:2px solid #d5e2e9; }
        .g10-v12 .g10-v12-table-wrap .table-striped > tbody > tr:nth-of-type(odd) { background:#fbfcfd; }
        .g10-v12 .g10-v12-table-foot { font-size:12px; color:#8091a0; margin:11px 0 0; line-height:1.6; }
        .g10-v12 .g10-v12-explore-title { color:#385773; font-weight:800; font-size:19px; margin:23px 0 5px; }
        .g10-v12 .g10-v12-explore-lead { font-size:12px; color:#8393a0; margin-bottom:12px; }
        .g10-v12 details.g10-v12-expand { border:1px solid #dce5ed; border-radius:12px; background:#fff; margin:11px 0; overflow:hidden; box-shadow:0 2px 7px rgba(49,76,101,.025); }
        .g10-v12 details.g10-v12-expand > summary { display:flex; gap:13px; align-items:center; cursor:pointer; padding:15px 17px; list-style:none; background:#f8fafc; transition:background .18s ease; }
        .g10-v12 details.g10-v12-expand > summary::-webkit-details-marker { display:none; }
        .g10-v12 details.g10-v12-expand > summary:hover { background:#f0f6fa; }
        .g10-v12 details.g10-v12-expand > summary:focus-visible { outline:2px solid #8eb3cf; outline-offset:-3px; }
        .g10-v12 details.g10-v12-expand[open] > summary { background:#f2f7fa; border-bottom:1px solid #e0e9ef; }
        .g10-v12 .g10-v12-summary-icon { display:flex; align-items:center; justify-content:center; width:35px; height:35px; border-radius:10px; flex:none; background:#e6eff6; color:#48739a; font-size:17px; font-weight:700; }
        .g10-v12 .g10-v12-summary-icon.warm { background:#f4eee7; color:#a88763; }
        .g10-v12 .g10-v12-summary-copy { flex:1; min-width:0; }
        .g10-v12 .g10-v12-summary-copy strong { display:block; font-weight:800; color:#365770; font-size:14px; margin-bottom:3px; }
        .g10-v12 .g10-v12-summary-copy small { display:block; font-size:12px; line-height:1.5; font-weight:400; color:#8192a2; }
        .g10-v12 .g10-v12-chevron { color:#718ea5; font-size:20px; transform:rotate(0deg); transition:transform .18s; }
        .g10-v12 details.g10-v12-expand[open] .g10-v12-chevron { transform:rotate(180deg); }
        .g10-v12 .g10-v10-expand-body { padding:17px 19px 20px; }
        .g10-v12 .g10-v10-note { margin-top:20px; }
        @media(max-width:1080px) { .g10-v12 .g10-v12-top-grid { grid-template-columns:1fr; } }
        @media(max-width:720px) { .g10-v12 .g10-v12-banner { flex-direction:column; align-items:stretch; padding:16px; } .g10-v12 .g10-v12-amount { width:100%; min-width:0; } .g10-v12 .g10-v12-card { padding:13px; } .g10-v12 .g10-v12-summary-copy small { font-size:11px; } }
        /* v13：0010 組合資訊順序修正與更清晰的展開卡片；不影響其它頁面 */
        .g10-v13 { --pf-ink:#344b60; --pf-line:#dce6ed; }
        .g10-v13 .g10-v13-head { border-radius:14px; padding:19px 22px; margin:0 0 18px; background:linear-gradient(110deg,#edf4f9,#f8fafc 72%,#f8f6f2); border:1px solid #dbe5ed; border-left:5px solid #6d94b5; }
        .g10-v13 .g10-v13-eyebrow { display:block; font-size:11px; font-weight:800; color:#6d91ac; letter-spacing:.7px; margin:0 0 5px; }
        .g10-v13 .g10-v13-head h3 { color:#2e506d; font-size:24px; font-weight:800; margin:0 0 5px; }
        .g10-v13 .g10-v13-head p { color:#6a7e90; font-size:13px; margin:0; line-height:1.6; }
        .g10-v13 .g10-v13-block { background:#fff; border:1px solid #dce6ee; border-radius:13px; padding:17px 20px; margin:0 0 15px; min-width:0; box-shadow:0 3px 12px rgba(48,82,108,.04); }
        .g10-v13 .g10-v13-block-top { display:flex; align-items:center; gap:9px; margin:0 0 12px; }
        .g10-v13 .g10-v13-block-num { border-radius:6px; padding:5px 7px; background:#e7f0f7; color:#426c91; font-size:11px; font-weight:800; }
        .g10-v13 .g10-v13-block h4 { color:#365a78; font-size:17px; font-weight:800; margin:0; }
        .g10-v13 .g10-v13-block-sub { color:#7d90a1; font-size:12px; margin:0 0 13px; line-height:1.55; }
        .g10-v13 .g10-v10-metrics { grid-template-columns:repeat(4,minmax(0,1fr)); gap:11px; margin:0; }
        .g10-v13 .g10-v10-metric { background:#f4f8fb; border:1px solid #e1eaf1; border-radius:10px; padding:14px 15px; }
        .g10-v13 .g10-v10-metric:nth-child(2) { background:#f1f6fa; }
        .g10-v13 .g10-v10-metric:nth-child(3) { background:#f6f8f8; }
        .g10-v13 .g10-v10-metric:nth-child(4) { background:#faf7f2; }
        .g10-v13 .g10-v10-metric .shiny-text-output { font-size:25px; font-weight:800; color:#355d7f; }
        .g10-v13 .g10-v13-config { border-left:4px solid #90b3c9; background:#fafcfe; }
        .g10-v13 .g10-v13-config-grid { display:grid; grid-template-columns:minmax(175px,.85fr) minmax(195px,1fr) minmax(270px,1.25fr); gap:18px; align-items:start; }
        .g10-v13 .g10-v13-config-grid .form-group { margin-bottom:4px; }
        .g10-v13 .g10-v13-config-grid label { color:#45657f; font-size:13px; font-weight:750; }
        .g10-v13 .g10-v13-config-grid input[type=number], .g10-v13 .g10-v13-config-grid select { border-color:#b7cddb; background:#fff; color:#2e516f; font-weight:750; height:38px; }
        .g10-v13 .g10-v13-setup-foot { font-size:12px; color:#698195; border-top:1px dashed #d9e5ed; padding-top:11px; margin:10px 0 0; line-height:1.6; }
        .g10-v13 .g10-v13-table { border-top:3px solid #7da2c0; }
        .g10-v13 .g10-v13-table .g10-v12-table-wrap { overflow-x:auto; }
        .g10-v13 .g10-v13-table .table { margin:0; min-width:790px; font-size:13px; }
        .g10-v13 .g10-v13-table .table th, .g10-v13 .g10-v13-table .table td { white-space:nowrap; padding:10px 12px; vertical-align:middle; }
        .g10-v13 .g10-v13-table .table thead th { background:#edf4f8; border-bottom:2px solid #bfd0df; color:#385c79; }
        .g10-v13 .g10-v13-chart { background:#fbfcfe; }
        .g10-v13 .g10-v11-row { grid-template-columns:minmax(150px,.85fr) minmax(160px,2.5fr) 70px; margin:10px 0; }
        .g10-v13 .g10-v11-track { height:15px; }
        .g10-v13 .g10-v11-fill { background:linear-gradient(90deg,#84aac8,#567da4); }
        .g10-v13 .g10-v13-expand-heading { font-size:18px; color:#365a79; font-weight:800; margin:22px 0 5px; }
        .g10-v13 .g10-v13-expand-sub { color:#8090a0; font-size:12px; margin:0 0 13px; }
        .g10-v13 details.g10-v13-expand { margin:12px 0; border:1px solid #d5e2ed; border-left:5px solid #82a7c5; border-radius:13px; background:white; overflow:hidden; box-shadow:0 4px 12px rgba(56,86,110,.06); }
        .g10-v13 details.g10-v13-expand.warm { border-left-color:#b49b7d; }
        .g10-v13 details.g10-v13-expand > summary { display:flex; align-items:center; gap:15px; padding:17px 20px; background:#f4f8fb; list-style:none; cursor:pointer; transition:background .2s; }
        .g10-v13 details.g10-v13-expand.warm > summary { background:#faf7f3; }
        .g10-v13 details.g10-v13-expand > summary::-webkit-details-marker { display:none; }
        .g10-v13 details.g10-v13-expand > summary:hover { background:#eaf3f9; }
        .g10-v13 details.g10-v13-expand.warm > summary:hover { background:#f4ece4; }
        .g10-v13 details.g10-v13-expand > summary:focus-visible { outline:3px solid #92bad5; outline-offset:-3px; }
        .g10-v13 .g10-v13-exp-icon { display:flex; justify-content:center; align-items:center; width:43px; height:43px; flex:none; border-radius:12px; background:#dfeef7; color:#3f6f94; font-size:21px; font-weight:750; }
        .g10-v13 .g10-v13-expand.warm .g10-v13-exp-icon { background:#f1e7da; color:#9d7c5b; }
        .g10-v13 .g10-v13-exp-copy { min-width:0; flex:1; }
        .g10-v13 .g10-v13-exp-copy strong { display:block; color:#2f516e; font-size:16px; font-weight:800; margin:0 0 4px; }
        .g10-v13 .g10-v13-exp-copy small { display:block; color:#6e8294; font-size:12px; line-height:1.5; font-weight:450; }
        .g10-v13 .g10-v13-exp-action { border:1px solid #a9c6d9; background:#fff; color:#3e7195; border-radius:9px; padding:8px 10px; white-space:nowrap; font-size:12px; font-weight:750; }
        .g10-v13 .g10-v13-exp-action .when-open { display:none; }
        .g10-v13 details.g10-v13-expand[open] > summary { border-bottom:1px solid #dce7ef; }
        .g10-v13 details.g10-v13-expand[open] .g10-v13-exp-action { background:#e7f1f7; }
        .g10-v13 details.g10-v13-expand[open] .g10-v13-exp-action .when-open { display:inline; }
        .g10-v13 details.g10-v13-expand[open] .g10-v13-exp-action .when-closed { display:none; }
        .g10-v13 details.g10-v13-expand .g10-v10-expand-body { padding:18px 21px 23px; }
        @media(max-width:1000px) { .g10-v13 .g10-v13-config-grid { grid-template-columns:repeat(2,minmax(0,1fr)); } .g10-v13 .g10-v13-config-grid > div:last-child { grid-column:1 / -1; } }
        @media(max-width:740px) { .g10-v13 .g10-v13-block { padding:14px; } .g10-v13 .g10-v10-metrics { grid-template-columns:repeat(2,minmax(0,1fr)); } .g10-v13 .g10-v13-config-grid { grid-template-columns:1fr; gap:9px; } .g10-v13 .g10-v13-config-grid > div:last-child { grid-column:auto; } .g10-v13 .g10-v11-row { grid-template-columns:minmax(90px,1fr) minmax(60px,1.6fr) 54px; gap:7px; } .g10-v13 .g10-v13-exp-action { padding:7px 8px; font-size:11px; } .g10-v13 .g10-v13-exp-copy strong { font-size:14px; } }
        .home-small-note { color:#7b8794; font-size:12px; margin:13px 0 0; }
        .home-legacy { margin-top:4px; }
        @media(max-width:900px) { .home-compare-grid { grid-template-columns:1fr; } .home-center { order:3; } .home-heading { flex-wrap:wrap; } .home-date { white-space:normal; } }

        "
        
      )
    )
  ),
  
  
  div(
    class = "hero",
    div(class = "dashboard-title", "0010 Investment Research"),
    div(
      class = "dashboard-subtitle",
      "以日對數報酬為統計基礎，從成長／動能、風險、穩定性、不確定性與歷史驗證分析 0050 及其成分股。"
    ),
    span(
      class = "hero-badge",
      "統計研究｜非投資建議｜資料更新至：",
      textOutput("header_latest_date", inline = TRUE)
    )
  ),
  
  
  # ==========================================================
  # 主分頁
  # ==========================================================
  
  tabsetPanel(
    
    id = "main_tabs",
    
    
    # ========================================================
    # 1. 總覽
    # ========================================================
    
    tabPanel(
      "總覽",
      div(class = "section-box home-intro",
          div(class = "home-heading",
              div(h2("0010 VS 0050"),
                  p(class = "page-description", "自訂十檔組合與元大台灣50｜績效與風險對照")),
              div(class = "home-date", "資料更新至：", textOutput("v7_home_date", inline = TRUE))),
          div(class = "home-window", "共同比較期間：", textOutput("v7_home_period", inline = TRUE)),
          div(class = "home-compare-grid",
              div(class = "home-side home-side-0010",
                  h2("0010"), p("自訂十檔投資組合", class = "home-side-subtitle"),
                  div(class = "home-stat", span("本期累積報酬"), strong(uiOutput("v7_10_ret", inline = TRUE))),
                  div(class = "home-stat", span("年化報酬"), strong(uiOutput("v7_10_ann", inline = TRUE))),
                  div(class = "home-stat", span("最大回撤"), strong(uiOutput("v7_10_mdd", inline = TRUE))),
                  div(class = "home-stat", span("年化波動"), strong(uiOutput("v7_10_vol", inline = TRUE))),
                  div(class = "home-stat", span("Sharpe 比率"), strong(uiOutput("v7_10_sharpe", inline = TRUE)))),
              div(class = "home-side home-center",
                  h2("VS"), p("0010 − 0050", class = "home-side-subtitle"),
                  div(class = "home-stat", span("超額報酬"), strong(uiOutput("v7_diff_ret", inline = TRUE))),
                  div(class = "home-stat", span("年化報酬差"), strong(uiOutput("v7_diff_ann", inline = TRUE))),
                  div(class = "home-stat", span("最大回撤差"), strong(uiOutput("v7_diff_mdd", inline = TRUE))),
                  div(class = "home-stat", span("年化波動差"), strong(uiOutput("v7_diff_vol", inline = TRUE))),
                  div(class = "home-stat", span("Sharpe 差"), strong(uiOutput("v7_diff_sharpe", inline = TRUE)))),
              div(class = "home-side home-side-0050",
                  h2("0050"), p("元大台灣50 ETF", class = "home-side-subtitle"),
                  div(class = "home-stat", span("本期累積報酬"), strong(uiOutput("v7_50_ret", inline = TRUE))),
                  div(class = "home-stat", span("年化報酬"), strong(uiOutput("v7_50_ann", inline = TRUE))),
                  div(class = "home-stat", span("最大回撤"), strong(uiOutput("v7_50_mdd", inline = TRUE))),
                  div(class = "home-stat", span("年化波動"), strong(uiOutput("v7_50_vol", inline = TRUE))),
                  div(class = "home-stat", span("Sharpe 比率"), strong(uiOutput("v7_50_sharpe", inline = TRUE))))
          ),
          p(class = "home-small-note", "漲跌數字：漲紅、跌綠。差額以百分點（pp）表示；風險指標不以正負代表好壞。"),
          div(class = "home-qualifier",
              "比較口徑：0010 為目前十檔名單及權重的本期回溯持有試算，非歷史各期實際選股績效；",
              "未扣交易成本。完整 T+1、含成本回測請見「0010組合 → 季度再平衡回測」。")
      ),
      div(class = "section-box",
          h2("0010 與 0050 累積報酬走勢"),
          p(class = "page-description", "選擇任意日期範圍，兩者都從選定區間的首個共同交易日 0% 開始。0010 採目前持股與權重回溯試算，非歷史逐期回測。"),
          dateRangeInput("v9_curve_range", "累積報酬圖｜選擇起訖日期：",
                         start=max(data_min_date, data_max_date-365), end=data_max_date,
                         min=data_min_date, max=data_max_date, format="yyyy-mm-dd", separator=" 至 ", language="zh-TW"),
          textOutput("v9_curve_actual_range"),
          plotOutput("v7_home_curve", height = "420px")),
      div(class = "section-box",
          h2("日對數報酬率｜0010 與 0050"),
          p(class = "page-description", "兩圖使用同一組日期與共同縱軸尺度，可獨立於上方的累積報酬圖調整期間。0010 為目前持股與權重的回溯試算。"),
          dateRangeInput("v9_daily_range", "日對數報酬圖｜選擇起訖日期：",
                         start=max(data_min_date, data_max_date-126), end=data_max_date,
                         min=data_min_date, max=data_max_date, format="yyyy-mm-dd", separator=" 至 ", language="zh-TW"),
          textOutput("v9_daily_actual_range"),
          fluidRow(
            column(6, h3("0010｜日對數報酬率"), plotOutput("v8_p10_daily_log", height = "290px")),
            column(6, h3("0050｜日對數報酬率"), plotOutput("v8_0050_daily_log", height = "290px"))
          )),
      div(class = "section-box",
          h2("0050 近期市場概況"),
          fluidRow(
            column(4, div(class = "info-card accent-blue", div(class = "info-card-title", "均線趨勢"), h3(class = "info-card-value", textOutput("market_0050_trend", inline = TRUE)))),
            column(4, div(class = "info-card accent-purple", div(class = "info-card-title", "14 日 RSI"), h3(class = "info-card-value", textOutput("market_0050_rsi", inline = TRUE)))),
            column(4, div(class = "info-card accent-amber", div(class = "info-card-title", "近期波動狀態"), h3(class = "info-card-value", textOutput("market_0050_vol_state", inline = TRUE))))
          ),
          fluidRow(
            column(3, div(class = "info-card", div(class = "info-card-title", "0050 調整後價格"), h3(class = "info-card-value", textOutput("market_0050_price", inline = TRUE)))),
            column(3, div(class = "info-card", div(class = "info-card-title", "0050 最新日對數報酬"), h3(class = "info-card-value", uiOutput("v8_market_log_return", inline = TRUE)))),
            column(3, div(class = "info-card", div(class = "info-card-title", "年初至今報酬"), h3(class = "info-card-value", uiOutput("v8_market_ytd", inline = TRUE)))),
            column(3, div(class = "info-card", div(class = "info-card-title", "50 檔漲跌家數"), h3(class = "info-card-value", textOutput("market_breadth", inline = TRUE))))
          ),
          tags$details(class = "method-details",
                       tags$summary("變更市場觀察日期"),
                       dateInput("market_observation_date", "市場觀察日期", value=data_max_date,
                                 min=data_min_date, max=data_max_date, format="yyyy-mm-dd", language="zh-TW", weekstart=1)),
          p(class = "home-small-note", "均線、RSI、波動狀態僅供描述市場，不是買賣訊號。")
      ),
      div(class = "section-box",
          h2("近 30 個交易日成分股漲跌幅"),
          plotOutput("market_30d_return_ranking", height = "700px")
      ),
      div(class = "section-box",
          h2("0010 本期持股與權重"),
          tableOutput("v4_home_table"),
          tags$details(class = "method-details",
                       tags$summary("選股門檻（展開查看）"),
                       tags$ul(
                         tags$li("有效資料：至少 280 個交易日，不足者剔除。"),
                         tags$li("風險穩定分：低於 20 分者剔除。"),
                         tags$li("年化波動度：超過 90% 者剔除。"),
                         tags$li("最大回撤：低於 −85% 者剔除。"),
                         tags$li("單日 CVaR 95%：尾端損失超過 6% 者剔除。"),
                         tags$li("產業分散：單一產業至多選取 3 檔。")
                       ),
                       p("以上為原選股模型設定，這次僅恢復顯示，不更改評分或篩選規則。"),
                       tags$details(tags$summary("50 檔完整篩選結果"), tableOutput("v4_all_table"))),
          tags$details(class = "method-details",
                       tags$summary("權重與資金分配（展開查看）"),
                       tableOutput("v4_home_weight")),
          p(class = "home-small-note", "本頁的選股名單是目前評分結果，並非歷史各季度當時的名單。")
      ),
      div(class = "section-box home-data-foot",
          h3("資料資訊"),
          p("交易資料更新至：", textOutput("global_latest_date", inline=TRUE),
            "｜股票數：", textOutput("global_stock_count", inline=TRUE),
            "｜資料筆數：", textOutput("global_observation_count", inline=TRUE)),
          p(class = "home-small-note", "統計研究與歷史資料試算，非投資建議。")
      )
    ),

    # ========================================================
    # 3. 成長／動能排名
    # ========================================================
    
    tabPanel(
      "排名與分數",
      tabsetPanel(
        id = "rank_subtabs",
        type = "pills",

        # v12：直接呈現兩張 Top10 長條圖，公式與全名單各自展開。
        tabPanel(
          "雙排名對照",
          div(
            class = "section-box rank-compare",
            h2("成分股排名與評分分析"),
            p(class = "rank-compare-lead",
              "從動能與風險兩個角度選股，再透過歷史回測檢驗排名的價值。"),
            div(class = "time-info rank-compare-date", strong("評分日："),
                textOutput("v6_rank_date", inline = TRUE),
                "｜同一評分日各自排序，分數不是預測漲幅。"),
            div(
              class = "rank-compare-grid",
              div(
                class = "rank-compare-card momentum",
                h3("動能分排名"),
                p(class = "rank-compare-description", "尋找具有報酬延續潛力的候選股"),
                div(class = "rank-compare-chart", plotOutput("rank_compare_mom_chart_v8", height = "490px")),
                p(class = "rank-compare-hint", "依動能分由高至低排列；分數越高，代表過去的相對動能越強。"),
                tags$details(
                  class = "rank-compare-expand",
                  tags$summary("查看動能評分細節與完整排名"),
                  div(class = "rank-compare-expanded",
                      div(class = "rank-compare-section", "評分組成"),
                      div(class = "rank-compare-weight",
                          "近 6 個月報酬百分位：55%", tags$br(),
                          "12−1 個月動能百分位：45%"),
                      div(class = "rank-compare-section", "完整成分股動能排名（不含產業欄）"),
                      tableOutput("rank_compare_mom_detail"))
                )
              ),
              div(
                class = "rank-compare-card risk",
                h3("風險穩定分排名"),
                p(class = "rank-compare-description", "評估股票的歷史風險是否相對可控"),
                div(class = "rank-compare-chart", plotOutput("rank_compare_risk_chart_v8", height = "490px")),
                p(class = "rank-compare-hint", "依穩定分由高至低排列；分數越高，代表綜合風險評價較有利。"),
                tags$details(
                  class = "rank-compare-expand",
                  tags$summary("查看風險評分細節與完整排名"),
                  div(class = "rank-compare-expanded",
                      div(class = "rank-compare-section", "評分組成"),
                      div(class = "rank-compare-weight",
                          "年化波動：30%｜最大回撤：25%", tags$br(),
                          "CVaR：20%｜Sharpe：15%｜GARCH：10%"),
                      div(class = "rank-compare-section", "完整成分股風險指標拆解"),
                      tableOutput("v4_risk_decomp"),
                      p(class = "rank-compare-note",
                        "GARCH 尚未估計時沿用既有的中性 50 分設定；模型及計分方法未調整。"))
                )
              )
            ),
            div(
              class = "rank-compare-flow",
              h3("這些排名會用在哪裡？"),
              div(class = "rank-compare-steps",
                  div(class = "rank-compare-step", strong("動能＋風險"), tags$small("形成選股分數")),
                  span(class = "rank-compare-arrow", "→"),
                  div(class = "rank-compare-step", strong("組成 0010"), tags$small("評分＋風險門檻")),
                  span(class = "rank-compare-arrow", "→"),
                  div(class = "rank-compare-step", strong("歷史驗證"), tags$small("檢查排名是否有用")))
            ),
            p(class = "rank-compare-note",
              "兩張榜單獨立排名，左右相同名次不一定是同一檔股票。排名是研究依據，不代表未來報酬保證。")
          )
        ),

        # v10：權重敏感度實驗，與正式 0010 持倉完全獨立。
        tabPanel(
          "權重比較實驗",
          div(
            class = "section-box rank-lab",
            h2("選股權重比較實驗"),
            p(class = "page-description",
              "如果只看動能、只看風險，或兩者各占一部分，選股結果會有什麼差別？這裡調整的是研究用排名，不影響正式 0010 組合。"),
            div(class = "time-info", strong("目前評分日："), textOutput("rank_lab_date", inline = TRUE),
                "｜與前兩個排名使用同一批當期分數。"),
            fluidRow(
              column(8,
                     sliderInput("rank_lab_mom_weight", "動能權重（剩餘為風險權重）",
                                 min = 0, max = 100, value = 70, step = 5, post = "%")),
              column(4,
                     p("目前比例", class = "rank-lab-hint"),
                     div(class = "rank-lab-ratio", textOutput("rank_lab_mix_text", inline = TRUE)))
            ),
            div(class = "rank-lab-actions",
                actionButton("rank_lab_all_mom", "純動能 100／0"),
                actionButton("rank_lab_all_risk", "純風險 0／100"),
                actionButton("rank_lab_half", "各半 50／50"),
                actionButton("rank_lab_default", "現行 70／30")),
            div(class = "rank-lab-formula", strong("合成評分："),
                textOutput("rank_lab_formula", inline = TRUE)),
            p(class = "rank-lab-hint",
              "沿用既有動能分、風險穩定分、風險門檻及產業上限（每產業最多 3 檔）。僅更換兩種分數的合成比例；GARCH 計分機制維持現況。"),
            h3("這組權重會選出哪些股票？", class = "rank-lab-section"),
            plotOutput("rank_lab_top10_plot", height = "390px"),
            tableOutput("rank_lab_selected_table"),
            tags$details(class = "method-details",
                         tags$summary("展開完整排名與篩選理由"),
                         tableOutput("rank_lab_full_table")),
            h3("排名的歷史有效性比較", class = "rank-lab-section"),
            p(class = "page-description",
              "比較純動能、純風險、各半、現行 70／30，以及目前自訂比例：每季依當時資料評分，下一交易日開始觀察未來 63 個交易日。"),
            actionButton("rank_lab_run_history", "執行歷史比較", class = "btn-primary"),
            div(style = "margin-top:12px;", textOutput("rank_lab_history_status")),
            tableOutput("rank_lab_history_table"),
            div(class = "plot-explanation",
                "這是『排名訊號』的比較：Top 10 後續報酬與 Bottom 10 後續報酬都採等權平均，未扣交易成本；不是正式 0010 組合的 T+1 含成本回測。",
                tags$br(),
                "IC 為當期分數與未來 63 日報酬的 Spearman 相關；Top10 − Bottom10 依當期評分排序，不使用未來報酬挑選 Bottom10。",
                tags$br(),
                "歷史分數在缺少各期 GARCH 預測時沿用既有中性 50 分設定；樣本依目前的 0050 成分股清單回溯，存在成分股存活者偏誤。")
          )
        )
      )
    ),
    
    
    # ========================================================
    # 11. 研究方法與資料說明
    # ========================================================
    # ========================================================
    # 9. GARCH 預測與 0010 組合（新增頁籤）
    #    插入位置：放在「研究方法與說明」tabPanel 之前
    # ========================================================
    
    tabPanel(
      "0010組合",
      div(
        class = "section-box",
        h2("0010 組合研究"),
        p(class = "g10-v10-lead",
          "以統計評分建立十檔股票組合，從選股結果、波動模型到季度回測，逐步檢驗方法的表現。"),
        tags$details(class = "method-details",
          tags$summary("研究方法與名詞說明"),
          tags$ul(
            tags$li("動能 70%＋風險穩定 30% 為現行綜合評分比例，並搭配風險門檻與產業分散限制。"),
            tags$li("權重是股票占組合資金的比例；預設以波動度反比配置，單檔上限 20%。"),
            tags$li("換手率代表持股配置需要調整的程度；滑價則是理想成交價與實際成交價的差距。"),
            tags$li("歷史季度回測使用當期資訊進行選股，和本期手動調整資金配置是不同的試算。"),
            tags$li("GARCH 是風險評分的一項輸入；歷史回測的波動估計採 EWMA，不等於全期 GARCH 回測。")
          )
        ),
        tabsetPanel(
          id = "g10_tabs",
          type = "pills",
          selected = "0010 組合",
          
          # --------------------------------------------------
          # 1. 0010 組合（本期）：先看結果，再按需要展開統計細項
          # --------------------------------------------------
          tabPanel(
            "0010 組合",
            br(),
            div(class = "g10-v12 g10-v13",
              div(class = "g10-v13-head",
                span(class = "g10-v13-eyebrow", "0010 / PORTFOLIO"),
                h3("本期 0010｜持股與統計評分"),
                p("先看評分和資金配置，再往下看權重圖；進階分析可自由展開。")
              ),
              div(class = "g10-v13-block",
                div(class = "g10-v13-block-top",
                  span(class = "g10-v13-block-num", "01"),
                  h4("本期統計摘要")
                ),
                div(class = "g10-v10-metrics",
                  div(class = "g10-v10-metric",
                    span(class = "g10-v10-label", "本期入選股票"),
                    textOutput("g10_v10_count")),
                  div(class = "g10-v10-metric",
                    span(class = "g10-v10-label", "配置加權動能分"),
                    textOutput("g10_v10_mom")),
                  div(class = "g10-v10-metric",
                    span(class = "g10-v10-label", "配置加權風險分"),
                    textOutput("g10_v10_risk")),
                  div(class = "g10-v10-metric",
                    span(class = "g10-v10-label", "最大單檔權重"),
                    textOutput("g10_v10_maxw"))
                )
              ),
              div(class = "g10-v13-block g10-v13-config",
                div(class = "g10-v13-block-top",
                  span(class = "g10-v13-block-num", "02"),
                  h4("本期資金配置｜直接調整")
                ),
                div(class = "g10-v13-config-grid",
                  div(numericInput("g10_capital", "模擬投入金額（NT$）",
                    value = 1000000, min = 100000, step = 100000)),
                  div(selectInput("g10_scheme", "持股權重方式",
                    choices = c("波動度反比" = "inv_vol", "等權重" = "equal",
                                "分數加權" = "score"), selected = "inv_vol")),
                  div(sliderInput("g10_cap", "單檔權重上限",
                    min = 0.10, max = 0.30, value = 0.20, step = 0.01))
                ),
                p(class = "g10-v13-setup-foot",
                  "變更金額會更新下方模擬持股金額及成本；變更配置方式／上限會更新本期權重，不會重新計算股票評分，也不會改動歷史季度回測。")
              ),
              div(class = "g10-v13-block g10-v13-table",
                div(class = "g10-v13-block-top",
                  span(class = "g10-v13-block-num", "03"),
                  h4("本期選股結果｜完整評分")
                ),
                p(class = "g10-v13-block-sub", "只留一張完整表格：排名、股票、產業、0010 分、動能分、風險分、GARCH 比值、配置權重與模擬金額。"),
                div(class = "g10-v12-table-wrap", tableOutput("g10_weights")),
                p(class = "g10-v12-table-foot",
                  "分數代表選股評價，不是預測漲幅。若尚未執行 GARCH 估計，比值可能顯示「—」；投入金額僅供模擬。")
              ),
              div(class = "g10-v13-block g10-v13-chart",
                div(class = "g10-v13-block-top",
                  span(class = "g10-v13-block-num", "04"),
                  h4("十檔持股權重｜長條圖")
                ),
                p(class = "g10-v13-block-sub", "長條越長，代表該檔股票在本期 0010 配置的資金比例越高。"),
                uiOutput("g10_v11_weight_bars")
              ),
              h4(class = "g10-v13-expand-heading", "延伸研究與操作"),
              p(class = "g10-v13-expand-sub", "以下兩張卡片可以點擊展開；預設收合，不影響上方資金配置。"),
              tags$details(class = "g10-v13-expand",
                tags$summary(
                  span(class = "g10-v13-exp-icon", "↔"),
                  span(class = "g10-v13-exp-copy",
                    strong("本季換股與交易成本"),
                    tags$small("看本次換了哪些股票、換手率、交易費用，以及下一次再平衡日期")),
                  span(class = "g10-v13-exp-action",
                    span(class = "when-closed", "展開查看 ▾"),
                    span(class = "when-open", "收合內容 ▴"))
                ),
                div(class = "g10-v10-expand-body",
                  h3("本期換股摘要"),
                  fluidRow(
                    column(3, div(class = "info-card",
                      div(class = "info-card-title", "本次新納入"),
                      h3(class = "info-card-value", textOutput("g10_n_new", inline = TRUE)))),
                    column(3, div(class = "info-card",
                      div(class = "info-card-title", "續抱"),
                      h3(class = "info-card-value", textOutput("g10_n_hold", inline = TRUE)))),
                    column(3, div(class = "info-card",
                      div(class = "info-card-title", "本次剔除"),
                      h3(class = "info-card-value", textOutput("g10_n_drop", inline = TRUE)))),
                    column(3, div(class = "info-card",
                      div(class = "info-card-title", "預估換手率"),
                      h3(class = "info-card-value", textOutput("g10_turnover_est", inline = TRUE))))
                  ),
                  h3("交易成本試算"),
                  tableOutput("g10_cost"),
                  p(class = "g10-v10-subtle",
                    "手續費 0.1425%（每筆最低 20 元）、證交稅 0.3%（僅賣出）、滑價 0.1%（既有假設）。"),
                  h3("下一次再平衡"),
                  uiOutput("g10_next_date")
                )
              ),
              tags$details(class = "g10-v13-expand warm",
                tags$summary(
                  span(class = "g10-v13-exp-icon", "⌁"),
                  span(class = "g10-v13-exp-copy",
                    strong("持有追蹤與風險控制"),
                    tags$small("查看持股變化、報酬、波動倍數，以及停損／停利參考價")),
                  span(class = "g10-v13-exp-action",
                    span(class = "when-closed", "展開查看 ▾"),
                    span(class = "when-open", "收合內容 ▴"))
                ),
                div(class = "g10-v10-expand-body",
                  p(class = "g10-v10-subtle",
                    "以先前季末的持股為基準，觀察目前報酬與波動倍數所對應的參考停損／停利價。"),
                  fluidRow(
                    column(6, sliderInput("g10_k_stop", "停損倍數 k",
                      min = 1, max = 3, value = 2, step = 0.1)),
                    column(6, sliderInput("g10_k_take", "停利倍數 k",
                      min = 2, max = 5, value = 3, step = 0.1))
                  ),
                  tableOutput("g10_track"),
                  uiOutput("g10_track_note")
                )
              ),
              div(class = "g10-v10-note",
                strong("研究重點："),
                "本期評分與配置顯示如何選股；是否有效，仍需在季度回測中用歷史資料驗證。")
            )
          ),

          # --------------------------------------------------
          # 2. 單檔 GARCH 模型
          # --------------------------------------------------
          tabPanel(
            "GARCH 診斷：單檔",
            br(),
            fluidRow(
              column(4, selectInput("g10_stock", "選擇股票：",
                                    choices = setNames(stock_info$symbol,
                                                       paste0(stock_info$name, " (", stock_info$symbol, ")")),
                                    selected = "2330")),
              column(4, selectInput("g10_window", "估計樣本（交易日）：",
                                    choices = c("近 2 年" = 500, "近 4 年" = 1000, "近 6 年" = 1500),
                                    selected = 1000)),
              column(4, div(class = "time-info",
                            strong("預測期："), "未來 63 個交易日（約一季）"))
            ),
            
            h3("預測結果"),
            fluidRow(
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "預測年化波動（下一季）"),
                            h3(class = "info-card-value", textOutput("g10_kpi_fvol", inline = TRUE)))),
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "近一年實現波動"),
                            h3(class = "info-card-value", textOutput("g10_kpi_rvol", inline = TRUE)))),
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "預測／實現"),
                            h3(class = "info-card-value", textOutput("g10_kpi_ratio", inline = TRUE)))),
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "波動狀態"),
                            h3(class = "info-card-value", textOutput("g10_kpi_regime", inline = TRUE))))
            ),
            
            h3("參數估計（最大概似法）"),
            tableOutput("g10_param_table"),
            
            h3("條件波動（模型配適）"),
            plotOutput("g10_vol_path", height = "380px"),
            
            h3("未來 63 日波動預測"),
            plotOutput("g10_fan", height = "340px"),
            
            h3("模型診斷"),
            tableOutput("g10_diag"),
          ),
          
          # --------------------------------------------------
          # 3. 波動預測總表
          # --------------------------------------------------
          tabPanel(
            "GARCH 診斷：全體",
            br(),
            actionButton("g10_run_all", "估計全部成分股 GARCH（約 5–15 秒）", class = "btn-primary"),
            div(class = "method-note",
                "第一次使用請先按這個按鈕。結果會快取，切換頁籤不會重算。"),
            h3("50 檔成分股波動預測"),
            tableOutput("g10_all_table"),
            h3("預測／實現波動比"),
            plotOutput("g10_ratio_bar", height = "700px"),
          ),
          
          # --------------------------------------------------
          # 4. 季度再平衡回測
          # --------------------------------------------------
          tabPanel(
            "季度再平衡回測",
            br(),
            div(class = "reading-guide", strong("這頁在驗證什麼？"),
                "每一季只用當時已知的資料重新評分、選股、給權重，持有 63 個交易日後再換股，並扣掉手續費、證交稅與滑價。看的是「扣完成本之後還剩多少」。"),
            div(class = "warning-note",
                strong("注意："),
                "本頁數字為新分數（動能 70% ＋ 風險穩定 30%）的結果；成交時點為評分日的次一交易日（T+1），避免前視偏差。"),
            actionButton("v5_run_bt", "執行季度回測（約 20–60 秒）", class = "btn-primary"),
            div(class = "method-note",
                "波動估計採 EWMA（λ = 0.94）；只有當期最新預測使用完整 GARCH(1,1)。兩者不可混稱為「完整 GARCH 回測」。"),

            h3("準確度"),
            tableOutput("v5_bt_acc"),
            div(class = "plot-explanation", textOutput("v5_bt_result", inline = TRUE)),

            h3("績效摘要"),
            tableOutput("v5_bt_stats"),

            h3("淨值曲線（已扣成本）"),
            plotOutput("v5_bt_curve", height = "420px"),

            h3("交易明細（可追溯）"),
            tableOutput("v5_bt_log"),
            downloadButton("v5_bt_csv", "下載交易明細 CSV")
          )
        )
      )
    ),
    
    
    # ========================================================
    # 8. 模型驗證：時間序列檢定與 GARCH 診斷
    # ========================================================
    
    tabPanel(
      "模型驗證",
      div(
        class = "section-box",
        h2("模型驗證｜時間序列檢定與 GARCH 診斷"),
        p(
          class = "page-description",
          "這一頁檢查三件事：報酬序列是否定態、ARIMA 是否優於白噪音、GARCH 的殘差是否還有結構。所有檢定都是即時計算，不是預先寫好的數字。"
        ),
        h3("時間序列檢定"),
        p(class = "page-description",
          "所選股票近 5 年日對數報酬的定態性、模型選擇與殘差診斷，以及全體成分股的動能持續性檢定。"),
        fluidRow(
          column(4, selectInput("v4ts_stock", "檢定標的：",
                                choices = stats::setNames(stock_info$symbol,
                                  paste0(stock_info$name, " (", stock_info$symbol, ")")),
                                selected = "2330")),
          column(4, div(class = "time-info", strong("樣本："), textOutput("v4ts_n", inline = TRUE))),
          column(4, actionButton("v4ts_run_persist", "計算動能持續性（約 3 秒）", class = "btn-primary"))
        ),
        h4("定態性與殘差診斷"),
        tableOutput("v4ts_diag"),
        h4("ARIMA 模型選擇（依 AIC 排序）"),
        tableOutput("v4ts_grid"),
        tableOutput("v4ts_persist"),
        h3("GARCH 模型診斷與樣本外評估"),
        p(class = "page-description",
          "檢查 GARCH(1,1) 的殘差是否仍有結構（含平方殘差與 ARCH 效果）、樣本外預測是否優於 EWMA 與滾動波動，並列出全體成分股的收斂狀況。"),
        fluidRow(
          column(4, selectInput("v5_g_stock", "單檔診斷：",
                                choices = stats::setNames(stock_info$symbol,
                                  paste0(stock_info$name, " (", stock_info$symbol, ")")),
                                selected = "2330")),
          column(4, actionButton("v5_run_gall", "全體收斂檢查（約 20–40 秒）", class = "btn-primary")),
          column(4, div(class = "time-info", strong("估計視窗："), "與「0010組合」頁的估計樣本相同"))
        ),
        h4("殘差診斷（標準化殘差、平方殘差、ARCH 效果）"),
        tableOutput("v5_gdiag_tab"),
        h4("樣本外預測評估（後 20% 資料）"),
        tableOutput("v5_goos_tab"),
        plotOutput("v5_goos_plot", height = "360px"),
        tableOutput("v5_gall_summary"),
        tableOutput("v5_gall_tab"),

      )
    ),
    
    
    # ========================================================
    # 5. 主成分與分群
    # ========================================================
    
    tabPanel(
      "其他",
      tabsetPanel(
        id = "appendix_tabs",
        type = "pills",
    
    
    # ========================================================
    # 6. 個股分析
    # ========================================================
    
    tabPanel(
      
      "個股分析",
      
      div(
        class = "section-box",
        
        h2("個股分析"),
        
        p(
          class = "page-description",
          "最多選擇 5 檔股票，以指定期間比較價格、累積報酬、日對數報酬與風險。"
        ),
        
        div(
          class = "reading-guide",
          strong("分析原則："),
          "研究分析使用調整後價格；日對數報酬率為 ln(Pt / Pt-1)。"
        ),
        
        selectizeInput(
          "selected_stock",
          "請選擇股票（最多 5 檔）：",
          choices = dashboard_stock_choices,
          selected = "0050",
          multiple = TRUE,
          options = list(
            maxItems = MAX_SELECTED_STOCKS,
            plugins = list("remove_button")
          )
        ),
        
        
        
        div(
          class = "quick-button-row",
          
          strong("快速期間："),
          
          actionButton(
            "period_research",
            "研究期間"
          ),
          
          actionButton(
            "period_all",
            "全部"
          ),
          
          actionButton(
            "period_1w",
            "1 週"
          ),
          
          actionButton(
            "period_1m",
            "1 個月"
          ),
          
          actionButton(
            "period_1q",
            "1 季"
          ),
          
          actionButton(
            "period_6m",
            "6 個月"
          ),
          
          actionButton(
            "period_1y",
            "1 年"
          ),
          
          actionButton(
            "period_3y",
            "3 年"
          ),
          
          actionButton(
            "period_ytd",
            "今年以來"
          )
        ),
        
        dateRangeInput(
          "date_range",
          "分析期間：",
          start = data_max_date - 365,
          end = min(
            RESEARCH_END_DATE,
            data_max_date
          ),
          min = data_min_date,
          max = data_max_date,
          format = "yyyy-mm-dd",
          separator = " 至 "
        ),
        
        textOutput(
          "analysis_period_text"
        ),
        
        hr(),
        
        h3("所選股票實際資料期間"),
        
        tableOutput(
          "selected_stock_ranges"
        ),
        
        uiOutput(
          "stock_warning"
        ),
        
        hr(),
        
        h3("調整後收盤價"),
        
        plotOutput(
          "price_plot",
          height = "500px"
        ),
        
        
        uiOutput(
          "cumulative_return_display"
        ),
        
        
        
        hr(),
        
        h3("日對數報酬描述統計"),
        
        tableOutput(
          "return_summary"
        ),
        
        hr(),
        
        h3("日對數報酬分布"),
        
        uiOutput(
          "return_distribution_ui"
        ),
        
        
        
        hr(),
        
        h3("日對數報酬箱型圖"),
        
        uiOutput(
          "return_boxplot_ui"
        ),
        
        
        hr(),
        
        h3("日對數報酬時間序列"),
        
        uiOutput(
          "log_return_series_ui"
        ),
        
        
        hr(),
        
        h3("期間績效"),
        
        tableOutput(
          "period_performance"
        ),
        
        hr(),
        
        h3("價格描述統計"),
        
        tableOutput(
          "price_summary"
        ),
        
        hr(),
        
        h3("風險摘要"),
        
        tableOutput(
          "risk_summary"
        ),
        
        hr(),
        
        h3("回撤"),
        
        uiOutput(
          "drawdown_charts_ui"
        ),
        
        
        hr(),
        
        h3("報酬與風險關係"),
        
        plotOutput(
          "return_risk_chart",
          height = "550px"
        ),
        
        hr(),
        h3("與 0050 的關係"),
        div(
          class = "reading-guide",
          strong("這裡在看什麼？ "),
          "相關係數看這檔股票和 0050 是否常常一起漲跌；60 日滾動相關性則看這種關係有沒有隨時間改變；Beta 則描述股票對 0050 漲跌的敏感程度。"
        ),
        tableOutput("individual_correlation_table"),
        h4("60 日滾動相關性"),
        plotOutput("individual_rolling_correlation_chart", height = "420px"),
      )
    ),
    
    
    # ========================================================
    # 7. 產業比較
    # ========================================================
    
    tabPanel(
      
      "產業比較",
      
      div(
        class = "section-box",
        
        h2("產業比較"),
        
        p(
          class = "page-description",
          "產業比較中，原始產業分類若少於 4 檔成分股，統一合併為「其他」。"
        ),
        
        
        # ========================================================
        # 單一產業
        # ========================================================
        
        div(
          class = "time-info",
          strong("目前產業分析期間： "),
          textOutput("industry_period_info", inline = TRUE)
        ),
        
        h3("單一產業分析"),
        
        selectInput(
          "selected_industry",
          "請選擇產業：",
          choices = available_industries,
          selected = available_industries[1]
        ),
        
        h3("產業成分股"),
        
        tableOutput(
          "industry_stock_table"
        ),
        
        h3("產業報酬摘要"),
        
        tableOutput(
          "industry_summary"
        ),
        
        h3("產業累積報酬"),
        
        plotOutput(
          "industry_cumulative_chart",
          height = "550px"
        ),
        
        h3("產業風險與報酬"),
        
        plotOutput(
          "industry_risk_return_chart",
          height = "500px"
        ),
        
        
        hr(),
        
        # ========================================================
        # 所有產業
        # ========================================================
        
        h3("所有產業綜合比較"),
        
        h4("產業累積報酬比較"),
        
        plotOutput(
          "industry_all_cumulative_chart",
          height = "600px"
        ),
        
        h4("產業日對數報酬分布"),
        
        uiOutput(
          "industry_all_distribution_ui"
        ),
        
        h4("產業報酬與風險"),
        
        plotOutput(
          "industry_all_risk_return_chart",
          height = "550px"
        ),
        
        
      )
      
    ))),

    tabPanel(
      "研究方法與說明",
      div(
        class = "section-box",
        h2("研究方法、指標與資料說明"),
        tags$details(class = "method-details",
          tags$summary("12. 資料字典與口徑"),
          p("所有計算共用同一份日資料，欄位與用途如下。"),
          tags$ul(
            tags$li("symbol：股票代號（4 碼，0050 為基準）。"),
            tags$li("date：交易日，以證交所行事曆為準。"),
            tags$li("adjusted：調整後收盤價，已還原除權息與分割；所有報酬、波動、回撤皆以此計算。"),
            tags$li("open / high / low / close / volume：原始行情，僅供顯示與核對，不進入統計模型。"),
            tags$li("log_return：日對數報酬 ln(P_t / P_(t-1))，由 adjusted 計算。"),
            tags$li("資料期間 2016-01-04 起；每日由 update_data.R 增量更新，只補最近 10 個交易日。"),
            tags$li("0050 於 2025-06-18 實施 4 比 1 分割，adjusted 序列必須連續，否則會出現假暴跌。")
          ),
          p("口徑提醒：0010 與 0050 的報酬都以 adjusted 計算，等於股利自動再投資，不可再把現金股利另外加一次。0050 若模擬買賣，賣出證交稅為 0.1%（ETF），與個股 0.3% 不同。")
        ),

        tags$details(class = "method-details",
          tags$summary("13. EWMA 與 GARCH 的差別"),
          p("EWMA 是 GARCH 家族的特例：ω = 0 且 α + β = 1（IGARCH）。優點是不需估計參數、完全不使用未來資料；缺點是沒有均值回歸。"),
          p("GARCH(1,1) 允許 α + β < 1，因此預測會隨時間收斂到長期平均波動，適合前瞻預測。"),
          p("本研究的用法：歷史季度回測採 EWMA（λ = 0.94），因為不需要每季重新估計；當期最新一期採完整 GARCH(1,1) 最大概似估計。"),
          p("因此報告不可把整段歷史回測稱為「完整 GARCH 回測」。正確寫法：「歷史回測以 EWMA 估計波動；當期預測採 GARCH(1,1)」。")
        ),


        p(class="page-description", "如果看到某個名詞不知道在說什麼，可以回到這一頁。每個重要指標都會先用白話說明『它是什麼、怎麼看、要注意什麼』，需要時再看計算方式。"),
        div(class="reading-guide", strong("核心原則： "), "價格用來觀察走勢；日對數報酬率用來進行主要統計分析。網站結果為統計研究與歷史資料分析，不構成投資建議。"),
        # ======================================================
        # v4 新增：研究方法（11 個可折疊區塊）
        # ======================================================
        
        tags$details(class = "method-details",
                     tags$summary("1. 資料與樣本"),
                     p("母體：元大台灣50（0050）及其 50 檔成分股。期間：2016-01-04 起至今，每日更新。頻率：日資料。"),
                     p("報酬定義：日對數報酬 r_t = ln(P_t / P_{t-1})。價格只用來畫走勢與計算回撤，所有統計模型都建立在報酬上。"),
                     p("為何用調整後價格：還原除權息與分割，避免把配息誤判成下跌。為何用對數報酬：可加總、分布較接近常態，且與 GARCH 的標準設定一致。")
        ),
        
        tags$details(class = "method-details",
                     tags$summary("2. 分數機制：0 到 100 是什麼"),
                     p("所有分數都是同一評分日的橫斷面百分位：先對 50 檔算出原始指標，由小到大排名，再線性轉成 0 到 100（最好 100、最差 0）。方向為「越低越好」的指標（如波動度）則將分數反轉。"),
                     p("三件必須知道的事：第一，90 分不是 90% 報酬，是相對位置很前面；第二，分數不能跨時間比較，今年 90 分不等於明年 90 分；第三，有同分時採平均排名，有效樣本只有 1 檔時給 50 分。")
        ),
        
        tags$details(class = "method-details",
                     tags$summary("3. 成長／動能分"),
                     p("公式：動能分 = 0.55 x P(近 6 個月報酬) + 0.45 x P(近 12-1 個月報酬)，P 代表橫斷面百分位。"),
                     p("r_6M = P_t / P_(t-126) - 1；r_12-1M = P_(t-21) / P_(t-252) - 1，也就是近 12 個月報酬但跳過最近 1 個月。"),
                     p("為何是這兩個期間：6 個月是台灣動能文獻與 FactSet 市值動能指數的主要形成期；12-1 個月是 Jegadeesh-Titman 的標準做法，跳過最近 1 個月是為了避開短期反轉。"),
                     p("已移除的期間：1 週（噪音太大）、1 個月（與 6 個月高度重疊）、3 年（與 12 個月相關係數過高，屬冗餘資訊）。"),
                     p("持續性平滑：動能分（平滑後）= 0.70 x 當期分數 + 0.30 x 上期分數。依據是 panel AR(1) 檢定，動能分持續性 beta = 0.504（t = 23.66、R 平方 = 0.254），代表上期分數對本期有實質解釋力，平滑可降低不必要的換股。")
        ),
        
        tags$details(class = "method-details",
                     tags$summary("4. 風險／穩定分"),
                     p("公式：風險穩定分 = 0.30 x P(年化波動, 反向) + 0.25 x P(最大回撤, 正向) + 0.20 x P(CVaR 95%, 反向) + 0.15 x P(Sharpe, 正向) + 0.10 x P(GARCH 比值, 反向)。全部取近 252 個交易日。"),
                     p("年化波動 = sd(r) x 根號 252；最大回撤 = min(P_t / max(P_1..P_t) - 1)；CVaR 95% 為最差 5% 交易日的平均損失；Sharpe = (平均日報酬 x 252) / 年化波動，未扣無風險利率；GARCH 比值 = 未來 63 日預測波動 / 近一年實現波動。"),
                     p("為何這樣配權重：波動 30% 最直觀且穩定；回撤 25% 是一般使用者真正有感的風險；CVaR 20% 補足波動看不到的尾端風險；Sharpe 15% 避免風險分變成「專挑不會動的股票」；GARCH 10% 為前瞻資訊但配適不穩定，只給輔助權重。")
        ),
        
        tags$details(class = "method-details",
                     tags$summary("5. 0010 合成、篩選與權重"),
                     p("合成：0010 分 = 0.70 x 動能分 + 0.30 x 風險穩定分。動能給 70% 的理由是本研究單因子回測顯示動能類分數對未來報酬具正向區分能力，而風險與穩定性分數在預測報酬的目的下為負向。"),
                     p("篩選（先篩再排，不是全部加權）：有效資料至少 280 個交易日；風險穩定分至少 20 分（相對後 20% 剔除，為主要門檻）；年化波動至多 90%、最大回撤不低於 -85%、CVaR 至多 6%（僅擋極端值）；同一產業至多 3 檔。"),
                     p("為何用相對門檻而不是「波動小於 60%」：實證顯示絕對門檻會系統性排除高動能股，使 0010 平均動能分從 77 分掉到 50 分，組合退化為金融與電信股，與「選出相對強勢」的目標矛盾。"),
                     p("權重：三種可選，預設波動度反比，w_i 正比於 1 / 波動_i，單檔上限 20%，超過部分以注水法重新分配。不使用 Markowitz 的原因是 50x50 共變異數矩陣求逆極不穩定，權重會出現負值或極端集中。")
        ),
        
        tags$details(class = "method-details",
                     tags$summary("6. 交易成本與再平衡"),
                     p("手續費 0.1425%（每筆最低 20 元，買賣各一次）、證交稅 0.3%（僅賣出）、滑價 0.1%（研究設定，買賣各一次）。"),
                     p("建倉成本 = 買進金額 x (0.1425% + 0.1%)；換股成本率 = 換手率 x (2 x 0.1425% + 2 x 0.1% + 0.3%)。"),
                     p("換手率 = 0.5 x 各檔權重變動絕對值之和，其中舊權重採用上一季持股經一季漲跌後自然漂移的權重，而非上一季的目標權重，否則會高估換手。"),
                     p("再平衡頻率：每季一次（63 個交易日）。相鄰兩季的持有期間不重疊，避免重疊樣本造成的統計偏誤。")
        ),
        
        tags$details(class = "method-details",
                     tags$summary("7. 時間序列模型與檢定"),
                     p("為什麼不用 ARIMA 預測報酬（實證）：季報酬 AR(1) 在 50 檔上的平均係數為 -0.030，僅 28% 個股顯著；0050 月報酬 AR(1) 係數 0.0128、t 值 2.16，但 R 平方僅 0.00016。結論是統計顯著但沒有解釋力，因此不以時間序列模型預測報酬。"),
                     p("定態性（ADF，落後期 4）：log 價格統計量 0.678，大於 5% 臨界值 -2.86，不拒絕單根，為非定態；日對數報酬統計量 -4.55，小於 -2.86，拒絕單根，為定態。因此價格水準不可直接建模，必須用報酬。"),
                     p("模型選擇：以 ARIMA(p, 0, q)、p 與 q 為 0 到 2 網格搜尋，依 AIC 與 BIC 排序。若最佳模型與 ARIMA(0,0,0) 的差距極小，代表加入 ARMA 項沒有實質改善。"),
                     p("殘差診斷：Ljung-Box Q(10) 用來看殘差是否仍有自我相關（p 大於 0.05 代表通過）；ARCH-LM(5) 用來檢定殘差平方是否仍有自我相關（p 小於 0.05 代表存在波動叢聚，需要 GARCH）。"),
                     p("動能持續性（panel AR(1)）：score_it = a + b x score_i,(t-1) + u_it。實證結果 b = 0.504、t = 23.66、R 平方 0.254、樣本 1647（34 季 x 50 檔）；加入個股固定效果後 b = 0.462、R 平方 0.214，代表持續性不是個股異質性造成。這是「用當期分數選股並做平滑」的統計依據。")
        ),
        
        tags$details(class = "method-details",
                     tags$summary("8. GARCH 波動模型"),
                     p("模型：r_t = epsilon_t，epsilon_t = sigma_t x z_t，z_t 為標準常態；變異數方程 sigma_t 平方 = omega + alpha x epsilon_(t-1) 平方 + beta x sigma_(t-1) 平方。"),
                     p("參數意義：omega 為長期變異數常數；alpha 為 ARCH 項，衡量昨日衝擊對今日波動的影響；beta 為 GARCH 項，衡量波動的記憶；alpha + beta 為持續性，越接近 1 越難消退；半衰期 = ln(0.5) / ln(alpha + beta)。"),
                     p("估計方法：以最大概似法估計，對數概似函數為 -0.5 乘以 (ln(2 pi) + ln(sigma_t 平方) + r_t 平方 / sigma_t 平方) 之和。以重新參數化保證 alpha + beta 小於 1 的約束成立；標準誤以數值 Hessian 取逆後，再以 delta method 轉回原始參數。"),
                     p("實證結果（示範）：0050 的 omega = 0.0711、alpha = 0.1159（t = 5.5）、beta = 0.8601（t = 34.6）、alpha + beta = 0.9759、半衰期 28.5 日；台積電 alpha = 0.0985（t = 4.3）、alpha + beta = 0.9410、半衰期 11.4 日；中華電 alpha + beta = 0.8585、半衰期 4.5 日。所有 alpha 均顯著，顯示成分股普遍存在波動叢聚。"),
                     p("診斷與限制：中華電的標準化殘差 Ljung-Box Q(10) = 28.98 大於 18.31，代表仍有殘留結構，配適不足，需考慮 GARCH-t 或加入 AR 項；部分個股 alpha + beta 接近 1（近 IGARCH），長期預測不穩定，此時只報方向不報幅度。"),
                     p("多期預測：E[sigma_(T+h) 平方] = omega + (alpha + beta) x E[sigma_(T+h-1) 平方]，從當期變異數反覆套用，h 越大越收斂到長期平均。這是模型設計（均值回歸），不是預測失效。網站取 h 為 1 到 63 的平均，換算年化波動作為下一季預測。")
        ),
        
        tags$details(class = "method-details",
                     tags$summary("9. 回撤分析"),
                     p("定義：峰值_t = max(value_1..value_t)；回撤_t = value_t / 峰值_t - 1；最大回撤 = 回撤_t 的最小值。"),
                     p("輸出項目：最大回撤、高點日、低點日、回復日、高點到低點天數、低點到回復天數、水下時間比例、最長連續水下天數。"),
                     p("為何不只看最大回撤：跌 30% 但三個月回來，與跌 20% 但兩年才回來，投資體驗完全不同，深度與時間必須一起看。"),
                     p("兩個視角：歷史回測回撤（季度再平衡淨值，已扣成本）與當期持有回撤（本期權重買進持有，自上次再平衡起算）。")
        ),
        
        tags$details(class = "method-details",
                     tags$summary("10. 模型驗證"),
                     p("設定：回看近 3 年（756 個交易日）、持有未來 6 個月（126 個交易日）、每月重新評分、起始年 2019。"),
                     p("比較模型：動能分、風險穩定分、0010、6 個月動能、舊版綜合分數。"),
                     p("核心指標：Top5 與 Bottom5 的未來報酬差（Spread）、Top5 勝率、勝過 0050 比例、Spearman 等級相關、有效回測次數。"),
                     p("判讀門檻：Spread 大於 0、勝率大於 50%、Spearman 大於 0，三者同時成立才算具有區分能力。")
        ),
        
        tags$details(class = "method-details",
                     tags$summary("11. 研究限制"),
                     p("存活者偏誤：以現行成分股回推歷史，2019 年的成分股不等於現在。"),
                     p("重疊樣本：模型驗證每月評分但看未來 6 個月，相鄰觀測重疊 5 個月，Spread 的 t 值不能直接當獨立樣本使用。"),
                     p("GARCH 參數穩定性：部分個股 alpha + beta 接近 1，長期預測不穩定；中華電等個股殘差檢定未通過。"),
                     p("交易成本估計：滑價 0.1% 為假設值，未考慮流動性衝擊與大單折價。"),
                     p("相對分數的限制：0 到 100 分是當期相對位置，不具跨期可比性。"),
                     p("權重為研究設定：70/30、30/25/20/15/10 等比例皆為本研究設定，非文獻固定標準，仍需敏感度分析。")
        ),
        
        tags$details(
          class="method-details",
          open=NA,
          tags$summary("全站時間尺度：所有頁面都用同一套定義"),
          p("1 週 = 約 5 個交易日；1 個月 = 約 21 個交易日；3 個月 = 約 63 個交易日；6 個月 = 約 126 個交易日；1 年 = 約 252 個交易日；3 年 = 約 756 個交易日。"),
          p("成長／動能排名所使用的主要風險、條件風險值（CVaR）、夏普比率與近 1 年年化報酬，以最近約 252 個交易日為共同基礎；穩定性與多期間報酬則同時比較 1 週到 3 年。"),
          p("PCA／分群會依使用者選擇的 3 個月、6 個月、1 年或 3 年重算，而且同一次分析中的所有特徵都使用相同起訖日期。"),
          p("模型驗證會把『評分時往回看多久』和『評分後往前驗證多久』分開設定，避免把未來資料混進當時的評分。")
        ),
        tags$details(class="method-details", open=NA,
                     tags$summary("日對數報酬率：是什麼、怎麼算、怎麼看？"),
                     p("定義：相鄰兩個交易日價格變化的對數形式。"),
                     p("計算：ln(今日調整後價格 / 昨日調整後價格)。"),
                     p("解讀：正值代表上漲、負值代表下跌；本研究的波動、相關性、條件風險值、主成分與拔靴法主要都從這個報酬序列出發。")
        ),
        tags$details(class="method-details",
                     tags$summary("風險指標：平常會晃多大？最慘曾跌多少？"),
                     p("年化波動度：可以把它想成『平常價格晃動有多大』。數值越高，通常代表漲跌幅度比較大。"),
                     p("最大回撤：過去曾經從一個高點一路跌到低點時，最嚴重跌了多少。例如 -30% 代表曾經從高點最多跌約 30%。"),
                     p("95% 條件風險值（CVaR, Conditional Value at Risk）：專門看最差的一小段行情。如果只挑最差約 5% 的日子，平均大概會損失多少，用來補充一般波動度看不到的極端風險。")
        ),
        tags$details(class="method-details", open=NA,
                     tags$summary("新版主排名：成長分數（Growth Score）到底在挑什麼？"),
                     p("新版主排名專注在『相對強弱』，不再把所有目的硬塞成一個總分。成長分數目前由 70% 報酬分數（Return Score）與 30% 夏普分數（Sharpe Score）組成。"),
                     p("報酬分數：比較多個時間尺度的相對報酬，再加入近 1 年年化報酬。分數高表示歷史報酬表現相對較強。"),
                     p("夏普分數：來自夏普比率（Sharpe Ratio）。白話來說，就是把『賺多少』和『為了這個報酬承擔多少波動』一起看。"),
                     p("70% / 30% 是目前研究候選設定，不是宣稱唯一最佳比例。70% 代表報酬／動能是主要訊號；30% 用來補充風險調整後的效率。模型驗證頁會讓這個設定和單獨報酬、6 個月動能、舊綜合模型一起比較。"),
                     p("動能（Momentum）：白話來說，就是觀察過去一段時間相對較強的股票，之後是否仍較容易維持相對強勢。本網站把 6 個月動能保留成一個簡單基準，拿來檢查複雜模型是否真的有增加價值。")
        ),
        tags$details(class="method-details",
                     tags$summary("風險與穩定性為什麼不直接加進主排名？"),
                     p("風險控制分數（Risk Score）主要看年化波動度、最大回撤（Maximum Drawdown）與條件風險值（CVaR, Conditional Value at Risk）。它回答的是『歷史上有多防守』，不是『未來誰一定漲最多』。"),
                     p("穩定性分數（Stability Score）看不同期間的報酬、排名與波動是否一致。它回答的是『表現是否依賴某一段行情』，也不是未來報酬保證。"),
                     p("因此新版把成長能力和防守資訊分開：主排名用 Growth；Risk 與 Stability 放在旁邊幫助判斷股票是『強勢但波動大』，還是『強勢且相對防守』。")
        ),
        h3("目前資料清單與有效期間"),
        tableOutput("stock_table"),
        h3("研究限制"),
        div(class="warning-note", "0050 成分股會隨時間調整；若以目前成分股回溯過去，可能產生存活者偏誤（Survivorship Bias）。此外，所有統計結果都會受到研究期間、資料品質與市場狀態影響。")
      )
    )
  ),
  
  
  div(
    class = "dashboard-footer",
    "0010 Investment Research｜統計研究用途・非投資建議"
  )
)


# ============================================================
# 第 1 段結束
# ============================================================

# ============================================================
# 第 2 段：Server
# ============================================================

server <- function(input, output, session) {
  
  # v5 新功能（T+1 回測、GARCH 診斷、樣本外評估）
  source("v5_server.R", local = TRUE)
  # ============================================================
  # v4 首頁：0010 總覽
  # 貼在 server 內（位置任意，建議最後）
  # 依賴：v4_score.R、v4_drawdown.R
  # ============================================================
  # v11：第二頁雙排名對照用資料；只讀現有 v4_tbl()，不重算或改寫模型分數。
  output$rank_compare_mom_top <- shiny::renderTable({
    t <- v4_tbl()
    shiny::req(!is.null(t), nrow(t) > 0)
    o <- utils::head(t[order(-t$mom_score), , drop = FALSE], 10)
    data.frame(
      名次 = seq_len(nrow(o)),
      股票 = paste0(o$name, "（", o$symbol, "）"),
      動能分 = sprintf("%.1f", o$mom_score),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$rank_compare_risk_top <- shiny::renderTable({
    t <- v4_tbl()
    shiny::req(!is.null(t), nrow(t) > 0)
    o <- utils::head(t[order(-t$risk_score), , drop = FALSE], 10)
    data.frame(
      名次 = seq_len(nrow(o)),
      股票 = paste0(o$name, "（", o$symbol, "）"),
      風險穩定分 = sprintf("%.1f", o$risk_score),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # v12：兩側長條圖直接顯示 Top 10。沿用 v4_tbl() 現有分數，完全不改計算。
  rank_compare_bar_v8 <- function(t, score_key, bar_color, axis_name) {
    x <- t[is.finite(t[[score_key]]), , drop = FALSE]
    x <- utils::head(x[order(-x[[score_key]], x$symbol), , drop = FALSE], 10)
    shiny::validate(shiny::need(nrow(x) > 0, "目前沒有可用的評分資料。"))
    names <- paste0(x$name, "（", x$symbol, "）")
    df <- data.frame(stock = factor(names, levels = rev(names)), score = x[[score_key]])
    ggplot2::ggplot(df, ggplot2::aes(x = stock, y = score)) +
      ggplot2::geom_col(fill = bar_color, width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", score)),
                         hjust = -0.12, size = 3.5, fontface = "bold", color = "#405369") +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_y_continuous(limits = c(0, 112), breaks = c(0, 25, 50, 75, 100),
                                  expand = ggplot2::expansion(mult = c(0, 0))) +
      ggplot2::labs(x = NULL, y = axis_name) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_blank(),
                     panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size = 10.2, color = "#344f66"),
                     axis.text.x = ggplot2::element_text(size = 10, color = "#73869a"),
                     axis.title.x = ggplot2::element_text(size = 10, color = "#62778b"),
                     plot.margin = ggplot2::margin(8, 23, 8, 7))
  }

  output$rank_compare_mom_chart_v8 <- shiny::renderPlot({
    t <- v4_tbl()
    shiny::req(!is.null(t), nrow(t) > 0)
    rank_compare_bar_v8(t, "mom_score", "#5289c0", "動能分（0～100）")
  }, res = 110)

  output$rank_compare_risk_chart_v8 <- shiny::renderPlot({
    t <- v4_tbl()
    shiny::req(!is.null(t), nrow(t) > 0)
    rank_compare_bar_v8(t, "risk_score", "#d39651", "風險穩定分（0～100）")
  }, res = 110)

  # 原始 v6_mom_table 在 v5_server.R；改成新的顯示表，避免動到共用模組。
  output$rank_compare_mom_detail <- shiny::renderTable({
    t <- v4_tbl()
    shiny::req(!is.null(t), nrow(t) > 0)
    o <- t[order(-t$mom_score), , drop = FALSE]
    data.frame(
      名次 = seq_len(nrow(o)),
      股票 = paste0(o$name, "（", o$symbol, "）"),
      動能分 = sprintf("%.1f", o$mom_score),
      `6個月報酬` = ifelse(is.finite(o$r6), sprintf("%+.1f%%", o$r6 * 100), "—"),
      `12-1個月報酬` = ifelse(is.finite(o$r12_1), sprintf("%+.1f%%", o$r12_1 * 100), "—"),
      `0010分` = sprintf("%.1f", o$p10_smooth),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$v4_risk_decomp <- shiny::renderTable({
    t <- v4_tbl()
    shiny::req(!is.null(t))
    o <- t[order(-t$risk_score), , drop = FALSE]
    data.frame(
      排名 = seq_len(nrow(o)),
      股票 = paste0(o$name, "（", o$symbol, "）"),
      風險穩定分 = sprintf("%.1f", o$risk_score),
      波動 = sprintf("%.1f", o$p_vol),
      回撤 = sprintf("%.1f", o$p_mdd),
      `CVaR` = sprintf("%.1f", o$p_cvar),
      Sharpe = sprintf("%.1f", o$p_sharpe),
      `GARCH` = sprintf("%.1f", o$p_garch),
      `GARCH 比值` = ifelse(is.finite(o$ratio), sprintf("%.2f", o$ratio), "—"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  v4_cap_amount <- 1000000   # 首頁試算金額
  
  # ---- 價格矩陣 ----
  v4_pm <- shiny::reactive({
    d <- as.data.frame(data())
    d <- d[d$symbol %in% c("0050", stock_info$symbol), , drop = FALSE]
    d <- d[!is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
    d <- d[order(d$date), , drop = FALSE]
    
    w <- as.data.frame(
      tidyr::pivot_wider(d[, c("date", "symbol", "adjusted")],
                         names_from = symbol, values_from = adjusted)
    )
    dates <- as.Date(w$date)
    m <- as.matrix(w[, -1, drop = FALSE])
    rownames(m) <- as.character(dates)
    storage.mode(m) <- "double"
    m
  })
  
  # ---- 分數表（若 GARCH 表已存在就一併納入）----
  v4_tbl <- shiny::reactive({
    g <- tryCatch(get("g10_store")(), error = function(e) NULL)
    v4_score_table(as.data.frame(data()), garch_tbl = g)
  })
  
  v4_sel <- shiny::reactive({
    t <- v4_tbl()
    shiny::req(!is.null(t), nrow(t) >= 10)
    v4_pick(t, 10)
  })
  
  v4_w <- shiny::reactive({
    s <- v4_sel()
    shiny::req(nrow(s) > 0)
    v4_weights(s, scheme = "inv_vol", cap = 0.20)
  })
  
  # ============================================================
  # v10 第二頁：動能／風險權重實驗（只讀既有分數，不影響 v4_tbl / v4_sel）
  # ============================================================
  rank_lab_mix <- function(t, momentum_pct = 70) {
    if (is.null(t) || !nrow(t)) return(t)
    x <- t
    a <- max(0, min(100, as.numeric(momentum_pct))) / 100
    x$p10_score <- a * x$mom_score + (1 - a) * x$risk_score
    x$p10_smooth <- x$p10_score  # 現行排名未啟用跨期平滑
    x <- x[order(-x$p10_score, x$symbol), , drop = FALSE]
    x$rank <- seq_len(nrow(x))
    rownames(x) <- NULL
    x
  }

  shiny::observeEvent(input$rank_lab_all_mom, {
    shiny::updateSliderInput(session, "rank_lab_mom_weight", value = 100)
  })
  shiny::observeEvent(input$rank_lab_all_risk, {
    shiny::updateSliderInput(session, "rank_lab_mom_weight", value = 0)
  })
  shiny::observeEvent(input$rank_lab_half, {
    shiny::updateSliderInput(session, "rank_lab_mom_weight", value = 50)
  })
  shiny::observeEvent(input$rank_lab_default, {
    shiny::updateSliderInput(session, "rank_lab_mom_weight", value = 70)
  })

  output$rank_lab_date <- shiny::renderText({
    d <- as.Date(rownames(v4_pm()))
    as.character(max(d, na.rm = TRUE))
  })

  output$rank_lab_mix_text <- shiny::renderText({
    w <- input$rank_lab_mom_weight
    shiny::req(!is.null(w))
    paste0(w, "% ／ ", 100 - w, "%")
  })
  output$rank_lab_formula <- shiny::renderText({
    w <- input$rank_lab_mom_weight
    shiny::req(!is.null(w))
    sprintf("%.2f × 動能分 ＋ %.2f × 風險分", w / 100, (100 - w) / 100)
  })

  rank_lab_current <- shiny::reactive({
    shiny::req(!is.null(input$rank_lab_mom_weight))
    rank_lab_mix(v4_tbl(), input$rank_lab_mom_weight)
  })
  rank_lab_selection <- shiny::reactive({
    x <- rank_lab_current()
    shiny::req(!is.null(x), nrow(x) > 0)
    v4_pick(x, n = 10, max_industry = V4_SCREEN$max_industry)
  })

  output$rank_lab_top10_plot <- shiny::renderPlot({
    s <- rank_lab_selection()
    shiny::validate(shiny::need(nrow(s) > 0, "目前沒有符合選股條件的股票。"))
    d <- data.frame(
      label = factor(paste0(s$name, "（", s$symbol, "）"),
                     levels = rev(paste0(s$name, "（", s$symbol, "）"))),
      score = s$p10_score
    )
    ggplot2::ggplot(d, ggplot2::aes(x = label, y = score)) +
      ggplot2::geom_col(fill = "#7fa9d3", width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(limits = c(0, 100), expand = ggplot2::expansion(mult = c(0, 0.03))) +
      ggplot2::labs(title = "目前權重下選出的十檔", x = NULL, y = "合成分數（0～100）") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  output$rank_lab_selected_table <- shiny::renderTable({
    s <- rank_lab_selection()
    shiny::req(nrow(s) > 0)
    data.frame(
      入選序 = seq_len(nrow(s)),
      股票 = paste0(s$name, "（", s$symbol, "）"),
      產業 = s$industry,
      動能分 = sprintf("%.1f", s$mom_score),
      風險分 = sprintf("%.1f", s$risk_score),
      合成分 = sprintf("%.1f", s$p10_score),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$rank_lab_full_table <- shiny::renderTable({
    t <- rank_lab_current()
    s <- rank_lab_selection()
    shiny::req(!is.null(t))
    state <- ifelse(!t$pass, t$reason,
                    ifelse(t$symbol %in% s$symbol, "入選", "未入選（名次或產業上限）"))
    data.frame(
      名次 = seq_len(nrow(t)),
      股票 = paste0(t$name, "（", t$symbol, "）"),
      動能分 = sprintf("%.1f", t$mom_score),
      風險分 = sprintf("%.1f", t$risk_score),
      合成分 = sprintf("%.1f", t$p10_score),
      篩選狀態 = state,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # 歷史驗證只在按鈕觸發時計算；當期權重滑桿不會觸發重算或修改正式持倉。
  rank_lab_history <- shiny::eventReactive(input$rank_lab_run_history, {
    custom_w <- shiny::isolate(input$rank_lab_mom_weight)
    shiny::req(!is.null(custom_w))
    cases <- c("純動能 100／0" = 100, "純風險 0／100" = 0,
               "各半 50／50" = 50, "現行 70／30" = 70,
               "自訂權重" = as.numeric(custom_w))
    d <- as.data.frame(data())
    d$date <- as.Date(d$date)
    d <- d[d$symbol %in% c("0050", stock_info$symbol) &
             is.finite(d$adjusted) & d$adjusted > 0 & !is.na(d$date),
           c("date", "symbol", "adjusted"), drop = FALSE]
    d <- d[order(d$date, d$symbol), , drop = FALSE]
    shiny::validate(shiny::need(nrow(d) > 0, "沒有足夠歷史價格資料。"))

    wide <- as.data.frame(tidyr::pivot_wider(d,
               names_from = symbol, values_from = adjusted))
    wide <- wide[order(wide$date), , drop = FALSE]
    dates <- as.Date(wide$date)
    pm <- as.matrix(wide[, setdiff(names(wide), "date"), drop = FALSE])
    storage.mode(pm) <- "double"
    rownames(pm) <- as.character(dates)
    # 和既有 v5 季度回測共用季末定義；評分日後 T+1 買入、持有 63 交易日。
    quarters <- v5_quarter_ends(dates)
    quarters <- quarters[quarters > 300 & quarters + 64 <= length(dates)]
    shiny::validate(shiny::need(length(quarters) > 0, "尚無可檢驗的完整季度。"))

    hist <- list()
    shiny::withProgress(message = "比較歷史評分權重…", value = 0, {
      for (k in seq_along(quarters)) {
        i <- quarters[k]
        shiny::setProgress(k / length(quarters),
          detail = paste0("第 ", k, "／", length(quarters), " 個評分季"))
        score <- try(v4_score_table(d[d$date <= dates[i], , drop = FALSE]), silent = TRUE)
        if (inherits(score, "try-error") || is.null(score) || nrow(score) < 20) next

        buy_i <- i + 1L
        sell_i <- buy_i + 63L
        if (sell_i > nrow(pm)) next
        buy_px <- pm[buy_i, ]
        sell_px <- pm[sell_i, ]
        fwd <- sell_px / buy_px - 1
        fwd[!is.finite(buy_px) | buy_px <= 0 | !is.finite(sell_px) |
              sell_px <= 0 | !is.finite(fwd)] <- NA_real_

        for (case_name in names(cases)) {
          w <- cases[[case_name]]
          t <- rank_lab_mix(score, w)
          selected <- v4_pick(t, n = 10, max_industry = V4_SCREEN$max_industry)
          if (nrow(selected) < 3) next
          eligible <- t[t$pass & is.finite(t$p10_score), , drop = FALSE]
          bottom_pool <- eligible[!eligible$symbol %in% selected$symbol, , drop = FALSE]
          bottom_syms <- utils::tail(bottom_pool$symbol, 10)

          top_fwd <- fwd[match(selected$symbol, names(fwd))]
          bottom_fwd <- fwd[match(bottom_syms, names(fwd))]
          top_mean <- if (sum(is.finite(top_fwd)) >= 3) mean(top_fwd, na.rm = TRUE) else NA_real_
          bottom_mean <- if (length(bottom_syms) >= 3 && sum(is.finite(bottom_fwd)) >= 3)
            mean(bottom_fwd, na.rm = TRUE) else NA_real_
          spread <- if (is.finite(top_mean) && is.finite(bottom_mean))
            top_mean - bottom_mean else NA_real_

          ranked_syms <- eligible$symbol
          fut <- fwd[match(ranked_syms, names(fwd))]
          raw_score <- eligible$p10_score
          ok <- is.finite(raw_score) & is.finite(fut)
          ic <- if (sum(ok) >= 5 && stats::sd(raw_score[ok]) > 0 &&
                    stats::sd(fut[ok]) > 0)
            suppressWarnings(stats::cor(raw_score[ok], fut[ok], method = "spearman"))
          else NA_real_

          hist[[length(hist) + 1L]] <- data.frame(
            scenario = case_name, pct = w, date = dates[i],
            top = top_mean, spread = spread, ic = ic,
            stringsAsFactors = FALSE)
        }
      }
    })
    shiny::validate(shiny::need(length(hist) > 0, "歷史有效樣本不足，無法完成比較。"))
    raw <- do.call(rbind, hist)
    # 公平比較：五種方案只使用三項驗證結果皆有效的共同季度。
    # 不因某策略少了有效價格，而偷偷更換比較期間。
    common_dates <- unique(as.character(raw$date))
    common_dates <- common_dates[vapply(common_dates, function(day) {
      z <- raw[as.character(raw$date) == day, , drop = FALSE]
      all(names(cases) %in% z$scenario) &&
        all(is.finite(z$top) & is.finite(z$spread) & is.finite(z$ic))
    }, logical(1))]
    shiny::validate(shiny::need(length(common_dates) > 0,
      "五種方案沒有共同的完整驗證季度；請確認歷史資料足夠。"))
    raw <- raw[as.character(raw$date) %in% common_dates, , drop = FALSE]
    summarise_one <- function(label) {
      z <- raw[raw$scenario == label, , drop = FALSE]
      safe_mean <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
      data.frame(scenario = label, weight = cases[[label]],
                 n = sum(is.finite(z$top)),
                 top = safe_mean(z$top), spread = safe_mean(z$spread),
                 ic = safe_mean(z$ic), stringsAsFactors = FALSE)
    }
    list(summary = do.call(rbind, lapply(names(cases), summarise_one)),
         from = min(raw$date), to = max(raw$date), custom = custom_w)
  })

  output$rank_lab_history_status <- shiny::renderText({
    if (is.null(input$rank_lab_run_history) || input$rank_lab_run_history == 0)
      return("尚未執行歷史比較；可先調整滑桿觀察當期排名。")
    h <- rank_lab_history()
    shiny::req(!is.null(h))
    paste0("完成：共同評分日 ", h$from, " ～ ", h$to,
           "｜自訂權重以按下按鈕時的 ", h$custom, "／", 100-h$custom,
           " 計算；調整滑桿後請重新按『執行歷史比較』。")
  })
  output$rank_lab_history_table <- shiny::renderTable({
    if (is.null(input$rank_lab_run_history) || input$rank_lab_run_history == 0) return(NULL)
    h <- rank_lab_history()
    shiny::req(!is.null(h))
    t <- h$summary
    p <- function(v) ifelse(is.finite(v), sprintf("%+.2f%%", 100 * v), "—")
    fmt_ic <- function(v) ifelse(is.finite(v), sprintf("%+.3f", v), "—")
    data.frame(
      評分方案 = ifelse(t$scenario == "自訂權重",
                      paste0("自訂 ", t$weight, "／", 100-t$weight), t$scenario),
      有效季數 = t$n,
      `Top10後續63日平均報酬` = p(t$top),
      `Top10－Bottom10` = p(t$spread),
      `平均Spearman IC` = fmt_ic(t$ic),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # ---- 上一次再平衡日（最近一個季末）----
  v4_reb <- shiny::reactive({
    m <- v4_pm()
    d <- as.Date(rownames(m))
    key <- paste0(format(d, "%Y"), "Q", ceiling(as.integer(format(d, "%m")) / 3))
    last_idx <- sort(as.integer(tapply(seq_along(d), key, max)))
    i <- if (length(last_idx) >= 2) last_idx[length(last_idx) - 1] else last_idx[1]
    list(idx = i, date = d[i])
  })
  
  # ---- 當期持有（買進持有，不算再平衡）----
  v4_hold <- shiny::reactive({
    v4_buyhold(v4_pm(), v4_w(), v4_reb()$idx)
  })
  
  v4_bench <- shiny::reactive({
    m <- v4_pm()
    i <- v4_reb()$idx
    x <- m[i:nrow(m), "0050"]
    data.frame(date = as.Date(rownames(m)[i:nrow(m)]),
               value = x / x[1])
  })
  
  output$v7_home_date <- shiny::renderText(as.character(latest_date()))
  output$v7_home_date_second <- shiny::renderText(as.character(latest_date()))
  output$v7_home_period <- shiny::renderText({
    z <- v7_home_pair()
    paste0(as.character(min(z$date)), " ～ ", as.character(max(z$date)))
  })
  output$v7_home_period_second <- shiny::renderText({
    z <- v7_home_pair()
    paste0(as.character(min(z$date)), " ～ ", as.character(max(z$date)))
  })

  # v7 首頁：全部比較數據只在同一批日期計算，不改變既有選股與回測模型。
  v7_home_pair <- shiny::reactive({
    a <- v4_hold()
    b <- v4_bench()
    shiny::req(!is.null(a), !is.null(b))
    a <- data.frame(date = as.Date(a$date), p10 = as.numeric(a$value))
    b <- data.frame(date = as.Date(b$date), etf = as.numeric(b$value))
    z <- merge(a, b, by = "date")
    z <- z[order(z$date), , drop = FALSE]
    z <- z[is.finite(z$p10) & is.finite(z$etf) & z$p10 > 0 & z$etf > 0, , drop = FALSE]
    shiny::req(nrow(z) >= 2)
    # 同一天共同基準 = 1.00
    z$p10 <- z$p10 / z$p10[1]
    z$etf <- z$etf / z$etf[1]
    z
  })

  v7_home_stats <- shiny::reactive({
    z <- v7_home_pair()
    metric <- function(v) {
      r <- diff(log(v))
      days <- length(r)
      ret <- tail(v, 1) / v[1] - 1
      dd <- min(v / cummax(v) - 1)
      # 樣本不足時不提供容易誤導的年化數值
      ann <- if (days >= 21) (tail(v, 1) / v[1])^(252 / days) - 1 else NA_real_
      vol <- if (days >= 21 && stats::sd(r) > 0) stats::sd(r) * sqrt(252) else NA_real_
      sharpe <- if (is.finite(vol) && vol > 0) mean(r) * 252 / vol else NA_real_
      c(ret = ret, ann = ann, mdd = dd, vol = vol, sharpe = sharpe)
    }
    list(p10 = metric(z$p10), etf = metric(z$etf))
  })

  # v8 數字：僅報酬類以漲紅跌綠；波動、回撤、Sharpe 等維持中性色。
  v8_color_value <- function(x, label, use_color = TRUE) {
    cls <- if (!use_color || !is.finite(x)) "v8-flat" else
      if (x > 0) "v8-up" else if (x < 0) "v8-down" else "v8-flat"
    shiny::span(class=cls, label)
  }
  for (k in c("ret", "ann", "mdd", "vol", "sharpe")) {
    local({
      key <- k
      format_metric <- function(v, difference=FALSE) {
        if (length(v) != 1 || !is.finite(v)) return("—")
        if (key == "sharpe") return(sprintf("%+.2f", v))
        if (difference) return(sprintf("%+.2f pp", v * 100))
        sprintf("%+.2f%%", v * 100)
      }
      output[[paste0("v7_10_", key)]] <- shiny::renderUI({
        v <- v7_home_stats()$p10[[key]]
        v8_color_value(v, format_metric(v), key %in% c("ret", "ann"))
      })
      output[[paste0("v7_50_", key)]] <- shiny::renderUI({
        v <- v7_home_stats()$etf[[key]]
        v8_color_value(v, format_metric(v), key %in% c("ret", "ann"))
      })
      output[[paste0("v7_diff_", key)]] <- shiny::renderUI({
        x <- v7_home_stats()
        v <- x$p10[[key]] - x$etf[[key]]
        v8_color_value(v, format_metric(v, difference=TRUE), key %in% c("ret", "ann"))
      })
    })
  }

  # v9：圖表日期獨立可選，不改 v7_home_pair / v7_home_stats 的本期統計口徑。
  # 採當前十檔及當前權重的歷史回溯試算，不能宣稱是當時的歷史選股回測。
  v9_available_prices <- shiny::reactive({
    pm <- v4_pm()
    ww <- v4_w()
    symbols <- names(ww)
    shiny::req(length(symbols) > 0, all(symbols %in% colnames(pm)), "0050" %in% colnames(pm))
    # 保持同日共同資料，不以前一天價格填補缺漏。
    m <- pm[, c(symbols, "0050"), drop=FALSE]
    ok <- apply(m, 1, function(row) all(is.finite(row) & row > 0))
    m <- m[ok, , drop=FALSE]
    shiny::req(nrow(m) >= 3)
    m
  })

  v9_slice_pair <- function(selected_range) {
    shiny::req(!is.null(selected_range), length(selected_range)==2)
    m <- v9_available_prices()
    dates <- as.Date(rownames(m))
    take <- which(dates >= as.Date(selected_range[1]) & dates <= as.Date(selected_range[2]))
    shiny::validate(shiny::need(length(take) >= 3, "選定期間共同有效交易日不足，請放寬日期範圍。"))
    m <- m[take, , drop=FALSE]
    symbols <- setdiff(colnames(m), "0050")
    ww <- v4_w()[symbols]
    ww <- as.numeric(ww / sum(ww))
    relative <- sweep(m[, symbols, drop=FALSE], 2, m[1,symbols], "/")
    p10 <- as.numeric(relative %*% ww)
    etf <- as.numeric(m[, "0050"] / m[1, "0050"])
    data.frame(date=as.Date(rownames(m)), p10=p10, etf=etf)
  }

  v9_curve_pair <- shiny::reactive({ v9_slice_pair(input$v9_curve_range) })
  v9_daily_pair <- shiny::reactive({ v9_slice_pair(input$v9_daily_range) })
  output$v9_curve_actual_range <- shiny::renderText({
    z <- v9_curve_pair()
    paste0("實際共同交易期間：",min(z$date)," 至 ",max(z$date),"｜",nrow(z)," 個交易日")
  })
  output$v9_daily_actual_range <- shiny::renderText({
    z <- v9_daily_pair()
    paste0("實際共同交易期間：",min(z$date)," 至 ",max(z$date),"｜",nrow(z)," 個交易日")
  })
  output$v7_home_curve <- shiny::renderPlot({
    z <- v9_curve_pair()
    d <- rbind(data.frame(date=z$date, pct=(z$p10-1)*100, label="0010"),
               data.frame(date=z$date, pct=(z$etf-1)*100, label="0050"))
    ggplot2::ggplot(d, ggplot2::aes(x=date, y=pct, colour=label)) +
      ggplot2::geom_hline(yintercept=0, linetype="dashed", colour="gray75") +
      ggplot2::geom_line(linewidth=1.2, na.rm=TRUE) +
      ggplot2::scale_colour_manual(values=c("0010"="#397db8", "0050"="#d75c57")) +
      ggplot2::scale_y_continuous(labels=function(x) sprintf("%+.1f%%",x)) +
      ggplot2::labs(x=NULL, y="累積報酬率", colour=NULL) +
      ggplot2::theme_minimal(base_size=13) +
      ggplot2::theme(legend.position="bottom", panel.grid.minor=ggplot2::element_blank())
  })

  v8_daily <- shiny::reactive({
    z <- v9_daily_pair()
    daily <- data.frame(date=z$date[-1],
                        p10=100*diff(log(z$p10)),
                        etf=100*diff(log(z$etf)))
    daily[is.finite(daily$p10) & is.finite(daily$etf), , drop=FALSE]
  })
  v8_daily_limits <- shiny::reactive({
    z <- v8_daily()
    vmax <- max(abs(c(z$p10,z$etf)), na.rm=TRUE)
    if (!is.finite(vmax) || vmax <= 0) vmax <- 1
    c(-vmax*1.12, vmax*1.12)
  })
  v8_daily_plot <- function(z, col, title) {
    d <- data.frame(date=z$date, pct=z[[col]])
    ggplot2::ggplot(d, ggplot2::aes(x=date,y=pct)) +
      ggplot2::geom_hline(yintercept=0, colour="gray65") +
      ggplot2::geom_col(ggplot2::aes(fill=pct>=0), width=.85, show.legend=FALSE) +
      ggplot2::scale_fill_manual(values=c("TRUE"="#d94b4b", "FALSE"="#238a58")) +
      ggplot2::scale_y_continuous(limits=v8_daily_limits(),labels=function(x) paste0(round(x,1),"%")) +
      ggplot2::labs(x=NULL,y="日對數報酬率") +
      ggplot2::theme_minimal(base_size=11) +
      ggplot2::theme(panel.grid.minor=ggplot2::element_blank())
  }
  output$v8_p10_daily_log <- shiny::renderPlot({
    z <- v8_daily(); shiny::req(nrow(z)>0)
    v8_daily_plot(z,"p10", "0010")
  })
  output$v8_0050_daily_log <- shiny::renderPlot({
    z <- v8_daily(); shiny::req(nrow(z)>0)
    v8_daily_plot(z,"etf", "0050")
  })

  output$v8_market_log_return <- shiny::renderUI({
    d <- selected_market_date()
    z <- data() %>% dplyr::filter(symbol=="0050", date<=d, is.finite(log_return)) %>%
      dplyr::arrange(dplyr::desc(date)) %>% dplyr::slice_head(n=1)
    shiny::req(nrow(z)>0)
    v <- z$log_return[1]
    v8_color_value(v,sprintf("%+.2f%%",100*v))
  })
  output$v8_market_ytd <- shiny::renderUI({
    d <- selected_market_date()
    z <- data() %>% dplyr::filter(symbol=="0050", date<=d,
       format(date,"%Y")==format(d,"%Y"), is.finite(adjusted)) %>% dplyr::arrange(date)
    shiny::req(nrow(z)>=2)
    v <- tail(z$adjusted,1)/head(z$adjusted,1)-1
    v8_color_value(v,sprintf("%+.2f%%",100*v))
  })

  output$v7_home_result <- shiny::renderText({
    a <- v7_home_stats()
    extra <- (a$p10[["ret"]] - a$etf[["ret"]]) * 100
    risk <- (a$p10[["vol"]] - a$etf[["vol"]]) * 100
    direction <- if (extra > 0) "高於" else if (extra < 0) "低於" else "與其相同"
    volatility <- if (!is.finite(risk)) "本期資料不足，暫不比較年化波動。" else
      sprintf("年化波動相差 %+.2f 個百分點。", risk)
    paste0("在本期共同日期的回溯試算中，0010 累積報酬", direction,
           " 0050（差額 ", sprintf("%+.2f", extra), " 個百分點）。", volatility,
           "這不是逐季實際選股績效，不能據此認定策略具有超額報酬能力。")
  })

  v4_hold_ret <- shiny::reactive({
    h <- v4_hold()
    shiny::req(!is.null(h))
    h$value[nrow(h)] - 1
  })
  
  v4_bench_ret <- shiny::reactive({
    b <- v4_bench()
    shiny::req(!is.null(b))
    b$value[nrow(b)] - 1
  })
  
  output$v4_k_ret <- shiny::renderText({
    v <- v4_hold_ret()
    ifelse(is.finite(v), sprintf("%+.2f%%", v * 100), "—")
  })
  
  output$v4_k_bench <- shiny::renderText({
    v <- v4_bench_ret()
    ifelse(is.finite(v), sprintf("%+.2f%%", v * 100), "—")
  })
  
  output$v4_k_excess <- shiny::renderText({
    v <- v4_hold_ret() - v4_bench_ret()
    ifelse(is.finite(v), sprintf("%+.2f%%", v * 100), "—")
  })
  
  output$v4_k_mdd <- shiny::renderText({
    h <- v4_hold()
    shiny::req(!is.null(h))
    s <- v4_dd_series(h$value, h$date)
    st <- v4_dd_stats(s)
    if (is.null(st)) "—" else sprintf("%.2f%%", st$max_dd * 100)
  })
  
  # ---- 名單 ----
  output$v4_home_table <- shiny::renderTable({
    s <- v4_sel()
    w <- v4_w()
    data.frame(
      名次 = seq_len(nrow(s)),
      股票 = paste0(s$name, "（", s$symbol, "）"),
      產業 = s$industry,
      動能分 = sprintf("%.1f", s$mom_score),
      風險分 = sprintf("%.1f", s$risk_score),
      `0010 分` = sprintf("%.1f", s$p10_smooth),
      權重 = sprintf("%.1f%%", as.numeric(w[s$symbol]) * 100),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # ---- 回撤圖 ----
  output$v4_home_dd <- shiny::renderPlot({
    h <- v4_hold()
    b <- v4_bench()
    shiny::req(!is.null(h), !is.null(b))
    
    sh <- v4_dd_series(h$value, h$date)
    sb <- v4_dd_series(b$value, b$date)
    
    df <- rbind(
      data.frame(date = sh$date, dd = sh$dd, series = "0010"),
      data.frame(date = sb$date, dd = sb$dd, series = "0050")
    )
    
    ggplot2::ggplot(df, ggplot2::aes(x = date, y = dd, colour = series)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%")) +
      ggplot2::labs(title = "本期回撤（自上次再平衡起）", x = NULL, y = "回撤", colour = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })
  
  output$v4_home_dd_text <- shiny::renderText({
    h <- v4_hold()
    shiny::req(!is.null(h))
    st <- v4_dd_stats(v4_dd_series(h$value, h$date))
    if (is.null(st)) return("資料不足")
    paste0(
      "最大回撤 ", sprintf("%.2f%%", st$max_dd * 100),
      "（", as.character(st$trough_date), "）",
      "；水下時間 ", sprintf("%.0f%%", st$underwater_pct * 100),
      "；最長連續水下 ", st$longest_underwater, " 個交易日。"
    )
  })
  
  # ---- 權重與金額 ----
  output$v4_home_weight <- shiny::renderTable({
    s <- v4_sel()
    w <- v4_w()
    amt <- as.numeric(w[s$symbol]) * v4_cap_amount
    fee <- pmax(20, amt * 0.001425)
    
    data.frame(
      股票 = paste0(s$name, "（", s$symbol, "）"),
      權重 = sprintf("%.1f%%", as.numeric(w[s$symbol]) * 100),
      金額 = formatC(round(amt), format = "d", big.mark = ","),
      買進手續費 = formatC(round(fee), format = "d", big.mark = ","),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$v4_home_note <- shiny::renderText({
    d <- as.Date(rownames(v4_pm()))
    last <- max(d, na.rm = TRUE)
    y <- as.integer(format(last, "%Y")); mo <- as.integer(format(last, "%m"))
    q <- (mo - 1) %/% 3 + 1
    if (q == 4) { y <- y + 1; mo <- 1 } else { mo <- q * 3 + 1 }
    nd <- as.Date(sprintf("%04d-%02d-01", y, mo))
    while (as.integer(format(nd, "%u")) > 5) nd <- nd + 1
    
    paste0("評分日：", as.character(v4_reb()$date),
           "｜下次再平衡：", as.character(nd),
           "｜投入金額試算：", formatC(v4_cap_amount, format = "d", big.mark = ","), " 元")
  })
  
  # ---- 50 檔完整表 ----
  output$v4_all_table <- shiny::renderTable({
    t <- v4_tbl()
    shiny::req(!is.null(t))
    data.frame(
      排名 = seq_len(nrow(t)),
      股票 = paste0(t$name, "（", t$symbol, "）"),
      產業 = t$industry,
      動能 = sprintf("%.1f", t$mom_score),
      風險 = sprintf("%.1f", t$risk_score),
      `0010` = sprintf("%.1f", t$p10_smooth),
      波動 = sprintf("%.1f%%", t$vol * 100),
      回撤 = sprintf("%.1f%%", t$mdd * 100),
      CVaR = sprintf("%.2f%%", t$cvar * 100),
      篩選 = t$reason,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  
  # ==========================================================
  # 1. 自動讀取資料
  # ==========================================================
  #
  # 每 60 秒檢查一次 stock_data.rds。
  # 如果 update_data.R 更新了 RDS，
  # Dashboard 會重新讀取資料。
  #
  # 不需要重新啟動網站。
  # ==========================================================
  
  raw_data <- reactiveFileReader(
    intervalMillis = 60000,
    session = session,
    filePath = DATA_FILE,
    readFunc = readRDS
  )
  
  data <- reactive({
    prepare_stock_data(
      raw_data()
    )
  })
  
  trading_dates <- reactive({
    sort(
      unique(
        data()$date
      )
    )
  })
  
  latest_date <- reactive({
    max(
      data()$date,
      na.rm = TRUE
    )
  })
  
  earliest_date <- reactive({
    min(
      data()$date,
      na.rm = TRUE
    )
  })
  
  
  # ==========================================================
  # 1-1. 融合版研究指標與綜合評分
  # ==========================================================
  
  research_metrics <- reactive({
    
    x <- data() %>%
      filter(
        symbol %in% dashboard_symbols,
        is.finite(log_return),
        is.finite(adjusted)
      ) %>%
      arrange(symbol, date)
    
    req(nrow(x) > 0)
    
    # --------------------------------------------------------
    # 共同比較日期：所有股票都使用同一段市場交易日期
    # --------------------------------------------------------
    common_dates <- trading_dates()
    common_end_date <- tail(common_dates, 1)
    
    base_start_index <- max(
      1,
      length(common_dates) - SCORING_BASE_DAYS + 1
    )
    
    base_common_start <- common_dates[base_start_index]
    
    # --------------------------------------------------------
    # 主要風險 / 夏普 / 年化報酬：統一使用最近 1 年
    # --------------------------------------------------------
    base <- x %>%
      filter(
        date >= base_common_start,
        date <= common_end_date
      ) %>%
      group_by(symbol) %>%
      summarise(
        n_obs = n(),
        base_start_date = base_common_start,
        base_end_date = common_end_date,
        annual_return = ifelse(
          n_obs >= floor(SCORING_BASE_DAYS * 0.75),
          exp(mean(log_return, na.rm = TRUE) * TRADING_DAYS_PER_YEAR) - 1,
          NA_real_
        ),
        annual_volatility = ifelse(
          n_obs >= floor(SCORING_BASE_DAYS * 0.75),
          sd(log_return, na.rm = TRUE) * sqrt(TRADING_DAYS_PER_YEAR),
          NA_real_
        ),
        max_drawdown = ifelse(
          n_obs >= floor(SCORING_BASE_DAYS * 0.75),
          min(drawdown_value(adjusted), na.rm = TRUE),
          NA_real_
        ),
        cvar_95 = ifelse(
          n_obs >= floor(SCORING_BASE_DAYS * 0.75),
          cvar_value(log_return, 0.95),
          NA_real_
        ),
        mean_daily_log = ifelse(
          n_obs >= floor(SCORING_BASE_DAYS * 0.75),
          mean(log_return, na.rm = TRUE),
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      mutate(
        sharpe = ifelse(
          is.finite(annual_volatility) & annual_volatility > 0,
          mean_daily_log * TRADING_DAYS_PER_YEAR / annual_volatility,
          NA_real_
        )
      )
    
    # --------------------------------------------------------
    # 多時間尺度：1週 / 1月 / 3月 / 6月 / 1年 / 3年
    # --------------------------------------------------------
    period_days <- ANALYSIS_WINDOWS
    period_weights <- RETURN_PERIOD_WEIGHTS
    
    period_list <- lapply(
      names(period_days),
      function(period_name) {
        
        n_day <- period_days[[period_name]]
        
        period_start_index <- max(
          1,
          length(common_dates) - n_day + 1
        )
        
        common_period_start <- common_dates[period_start_index]
        
        target_days <- min(
          n_day,
          length(common_dates)
        )
        
        x %>%
          filter(
            date >= common_period_start,
            date <= common_end_date
          ) %>%
          group_by(symbol) %>%
          summarise(
            period = period_name,
            n = n(),
            start_date = common_period_start,
            end_date = common_end_date,
            period_log_return = ifelse(
              n >= max(3, floor(target_days * 0.75)),
              sum(log_return, na.rm = TRUE),
              NA_real_
            ),
            annualized_log_mean = ifelse(
              n >= max(3, floor(target_days * 0.75)),
              mean(log_return, na.rm = TRUE) * TRADING_DAYS_PER_YEAR,
              NA_real_
            ),
            # 1 週太短，不把它硬塞進風險穩定性。
            annualized_volatility = ifelse(
              n_day >= 21 &&
                n >= max(15, floor(target_days * 0.75)),
              sd(log_return, na.rm = TRUE) * sqrt(TRADING_DAYS_PER_YEAR),
              NA_real_
            ),
            .groups = "drop"
          )
      }
    )
    
    period_long <- bind_rows(period_list) %>%
      group_by(period) %>%
      mutate(return_percentile = cross_section_score(period_log_return, TRUE)) %>%
      ungroup()
    
    # --------------------------------------------------------
    # 穩定性：排名、報酬、風險
    # --------------------------------------------------------
    stability_raw <- period_long %>%
      group_by(symbol) %>%
      summarise(
        rank_variation = ifelse(
          sum(is.finite(return_percentile)) >= 3,
          sd(return_percentile, na.rm = TRUE),
          NA_real_
        ),
        return_variation = ifelse(
          sum(is.finite(annualized_log_mean)) >= 3,
          sd(annualized_log_mean, na.rm = TRUE),
          NA_real_
        ),
        risk_variation = ifelse(
          sum(is.finite(annualized_volatility)) >= 2,
          sd(annualized_volatility, na.rm = TRUE),
          NA_real_
        ),
        valid_periods = sum(is.finite(period_log_return)),
        .groups = "drop"
      ) %>%
      mutate(
        rank_stability_score = cross_section_score(rank_variation, FALSE),
        return_stability_score = cross_section_score(return_variation, FALSE),
        risk_stability_score = cross_section_score(risk_variation, FALSE),
        stability_score =
          0.30 * rank_stability_score +
          0.40 * return_stability_score +
          0.30 * risk_stability_score
      )
    
    # --------------------------------------------------------
    # 多期間報酬分數
    # --------------------------------------------------------
    period_scored <- period_long %>%
      mutate(weight = period_weights[period])
    
    multi_return <- period_scored %>%
      group_by(symbol) %>%
      summarise(
        multi_period_return_score = ifelse(
          sum(is.finite(return_percentile)) >= 3,
          weighted.mean(return_percentile, weight, na.rm = TRUE),
          NA_real_
        ),
        .groups = "drop"
      )
    
    momentum_6m <- period_long %>%
      filter(period == "6個月") %>%
      transmute(
        symbol,
        momentum_6m_score = return_percentile
      )
    
    period_wide <- period_long %>%
      select(symbol, period, period_log_return, return_percentile) %>%
      pivot_wider(
        names_from = period,
        values_from = c(period_log_return, return_percentile),
        names_sep = "_"
      )
    
    # --------------------------------------------------------
    # 四大面向分數
    # --------------------------------------------------------
    score <- base %>%
      mutate(
        annual_return_score = cross_section_score(annual_return, TRUE),
        volatility_score = cross_section_score(annual_volatility, FALSE),
        drawdown_score = cross_section_score(max_drawdown, TRUE),
        cvar_score = cross_section_score(cvar_95, TRUE),
        sharpe_score = cross_section_score(sharpe, TRUE)
      ) %>%
      left_join(stability_raw, by = "symbol") %>%
      left_join(multi_return, by = "symbol") %>%
      left_join(momentum_6m, by = "symbol") %>%
      left_join(period_wide, by = "symbol") %>%
      mutate(
        return_score = 0.70 * multi_period_return_score + 0.30 * annual_return_score,
        risk_score = 0.35 * volatility_score + 0.35 * drawdown_score + 0.30 * cvar_score,
        growth_score =
          GROWTH_WEIGHTS[["ret"]] * return_score +
          GROWTH_WEIGHTS[["sharpe"]] * sharpe_score,
        # 舊綜合分數保留，只作歷史比較，不再作網站主排名。
        composite_score =
          LEGACY_COMPOSITE_WEIGHTS[["stability"]] * stability_score +
          LEGACY_COMPOSITE_WEIGHTS[["ret"]] * return_score +
          LEGACY_COMPOSITE_WEIGHTS[["risk"]] * risk_score +
          LEGACY_COMPOSITE_WEIGHTS[["sharpe"]] * sharpe_score
      )
    
    # --------------------------------------------------------
    # 0050 比較基準
    # --------------------------------------------------------
    benchmark_score <- score %>%
      filter(symbol == "0050") %>%
      slice_head(n = 1)
    
    if (nrow(benchmark_score) == 1) {
      score <- score %>%
        mutate(
          score_diff_0050 = growth_score - benchmark_score$growth_score,
          relative_0050 = case_when(
            score_diff_0050 >= 3 ~ "成長分數高於 0050",
            score_diff_0050 <= -3 ~ "成長分數低於 0050",
            TRUE ~ "成長分數接近 0050"
          )
        )
    } else {
      score <- score %>%
        mutate(
          score_diff_0050 = NA_real_,
          relative_0050 = "無法比較"
        )
    }
    
    # --------------------------------------------------------
    # 白話標籤
    # --------------------------------------------------------
    component <- score %>% filter(symbol %in% stock_info$symbol)
    q_return_80 <- quantile(component$annual_return, 0.80, na.rm = TRUE)
    q_stability_80 <- quantile(component$stability_score, 0.80, na.rm = TRUE)
    q_vol_80 <- quantile(component$annual_volatility, 0.80, na.rm = TRUE)
    q_dd_20 <- quantile(component$max_drawdown, 0.20, na.rm = TRUE)
    q_cvar_20 <- quantile(component$cvar_95, 0.20, na.rm = TRUE)
    
    b_ann_return <- if (nrow(benchmark_score) == 1) benchmark_score$annual_return else NA_real_
    b_stability <- if (nrow(benchmark_score) == 1) benchmark_score$stability_score else NA_real_
    b_vol <- if (nrow(benchmark_score) == 1) benchmark_score$annual_volatility else NA_real_
    b_dd <- if (nrow(benchmark_score) == 1) benchmark_score$max_drawdown else NA_real_
    b_cvar <- if (nrow(benchmark_score) == 1) benchmark_score$cvar_95 else NA_real_
    
    score$labels <- vapply(
      seq_len(nrow(score)),
      function(i) {
        
        row <- score[i, ]
        lab <- character(0)
        
        if (is.finite(row$annual_return) &&
            row$annual_return >= q_return_80 &&
            (!is.finite(b_ann_return) || row$annual_return > b_ann_return)) {
          lab <- c(lab, "高報酬")
        }
        
        if (is.finite(row$stability_score) &&
            row$stability_score >= q_stability_80 &&
            (!is.finite(b_stability) || row$stability_score > b_stability)) {
          lab <- c(lab, "跨期表現一致")
        }
        
        if (is.finite(row$annual_volatility) &&
            row$annual_volatility >= q_vol_80 &&
            (!is.finite(b_vol) || row$annual_volatility > b_vol)) {
          lab <- c(lab, "高波動")
        }
        
        if (is.finite(row$max_drawdown) &&
            row$max_drawdown <= q_dd_20 &&
            (!is.finite(b_dd) || row$max_drawdown < b_dd)) {
          lab <- c(lab, "深度回撤")
        }
        
        if (is.finite(row$cvar_95) &&
            row$cvar_95 <= q_cvar_20 &&
            (!is.finite(b_cvar) || row$cvar_95 < b_cvar)) {
          lab <- c(lab, "尾端風險偏高")
        }
        
        if (
          "return_percentile_1週" %in% names(row) &&
          "return_percentile_1年" %in% names(row) &&
          is.finite(row$return_percentile_1週) &&
          is.finite(row$return_percentile_1年) &&
          row$return_percentile_1週 + 30 < row$return_percentile_1年
        ) {
          lab <- c(lab, "近期轉弱")
        }
        
        if (length(lab) == 0) "一般" else paste(lab, collapse = "｜")
      },
      character(1)
    )
    
    score %>%
      mutate(
        name = get_stock_name(symbol),
        industry = get_stock_industry(symbol)
      )
  })
  
  
  # 排名頁明確列出每個時間尺度真正使用的日期。
  research_period_table <- reactive({
    
    dates <- trading_dates()
    req(length(dates) > 0)
    
    bind_rows(
      lapply(
        names(ANALYSIS_WINDOWS),
        function(period_name) {
          
          n_day <- ANALYSIS_WINDOWS[[period_name]]
          
          start_index <- max(
            1,
            length(dates) - n_day + 1
          )
          
          start_date <- dates[start_index]
          end_date <- tail(dates, 1)
          
          counts <- data() %>%
            filter(
              date >= start_date,
              date <= end_date,
              symbol %in% dashboard_symbols,
              is.finite(log_return)
            ) %>%
            count(
              symbol,
              name = "n_obs"
            )
          
          component_counts <- counts %>%
            filter(
              symbol %in% stock_info$symbol
            )
          
          benchmark_n <- counts$n_obs[
            counts$symbol == "0050"
          ]
          
          data.frame(
            時間尺度 = period_name,
            目標交易日 = n_day,
            實際起始日 = format(
              start_date,
              "%Y-%m-%d"
            ),
            實際結束日 = format(
              end_date,
              "%Y-%m-%d"
            ),
            `0050有效觀測` = ifelse(
              length(benchmark_n) == 1,
              benchmark_n,
              NA_integer_
            ),
            `成分股最少有效觀測` = ifelse(
              nrow(component_counts) > 0,
              min(
                component_counts$n_obs,
                na.rm = TRUE
              ),
              NA_integer_
            ),
            用途 = ifelse(
              period_name == "1週",
              "近期報酬與相對排名",
              ifelse(
                period_name %in% c(
                  "1個月",
                  "3個月"
                ),
                "短中期報酬、排名與穩定性",
                "中長期報酬、風險與穩定性"
              )
            ),
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        }
      )
    )
  })
  
  
  # ==========================================================
  # 1-2. 主成分與分群
  # ==========================================================
  
  pca_cluster_result <- reactive({
    
    window_days <- as.integer(input$pca_window)
    req(is.finite(window_days))
    
    m <- compute_pca_features(
      data(),
      window_days
    )
    
    req(nrow(m) >= 6)
    
    feature_names <- c(
      "annual_return",
      "annual_volatility",
      "max_drawdown",
      "cvar_95",
      "sharpe",
      "return_stability_score",
      "rank_stability_score",
      "risk_stability_score"
    )
    
    ok <- complete.cases(m[, feature_names, drop = FALSE])
    m2 <- m[ok, , drop = FALSE]
    
    req(nrow(m2) >= 6)
    
    pca <- prcomp(
      m2[, feature_names, drop = FALSE],
      center = TRUE,
      scale. = TRUE
    )
    
    # 分群使用能保留大部分資訊的前幾個主成分，最多取 4 個。
    variance_ratio <- (pca$sdev ^ 2) / sum(pca$sdev ^ 2)
    pc_count <- which(cumsum(variance_ratio) >= 0.80)[1]
    if (!is.finite(pc_count)) pc_count <- min(3, ncol(pca$x))
    pc_count <- max(2, min(pc_count, 4, ncol(pca$x)))
    
    cluster_x <- pca$x[, seq_len(pc_count), drop = FALSE]
    
    max_k <- min(6, nrow(m2) - 1)
    k_candidates <- 2:max_k
    
    quality <- lapply(
      k_candidates,
      function(k) {
        
        set.seed(50 + k)
        
        km <- kmeans(
          cluster_x,
          centers = k,
          nstart = 30
        )
        
        data.frame(
          k = k,
          total_withinss = km$tot.withinss,
          silhouette = average_silhouette(cluster_x, km$cluster),
          stringsAsFactors = FALSE
        )
      }
    ) %>%
      bind_rows()
    
    best_k <- quality$k[which.max(quality$silhouette)]
    
    set.seed(50)
    
    km <- kmeans(
      cluster_x,
      centers = best_k,
      nstart = 50
    )
    
    m2$cluster <- km$cluster
    m2$PC1 <- pca$x[, 1]
    m2$PC2 <- if (ncol(pca$x) >= 2) pca$x[, 2] else 0
    
    m2 <- m2 %>%
      mutate(
        overall_stability =
          rowMeans(
            cbind(
              return_stability_score,
              rank_stability_score,
              risk_stability_score
            ),
            na.rm = TRUE
          )
      )
    
    ret_mid <- median(m2$annual_return, na.rm = TRUE)
    vol_mid <- median(m2$annual_volatility, na.rm = TRUE)
    stab_mid <- median(m2$overall_stability, na.rm = TRUE)
    sharpe_mid <- median(m2$sharpe, na.rm = TRUE)
    
    group_character <- m2 %>%
      group_by(cluster) %>%
      summarise(
        股票數 = n(),
        avg_return = mean(annual_return, na.rm = TRUE),
        avg_volatility = mean(annual_volatility, na.rm = TRUE),
        avg_drawdown = mean(max_drawdown, na.rm = TRUE),
        avg_cvar = mean(cvar_95, na.rm = TRUE),
        avg_sharpe = mean(sharpe, na.rm = TRUE),
        avg_stability = mean(overall_stability, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        type = case_when(
          avg_stability >= stab_mid &
            avg_volatility <= vol_mid &
            avg_return >= ret_mid ~ "穩健成長型",
          
          avg_stability >= stab_mid &
            avg_volatility <= vol_mid ~ "低波動穩定型",
          
          avg_return >= ret_mid &
            avg_volatility > vol_mid ~ "高報酬高風險型",
          
          avg_return < ret_mid &
            avg_volatility > vol_mid ~ "高波動承壓型",
          
          avg_sharpe >= sharpe_mid ~ "均衡效率型",
          
          TRUE ~ "均衡型"
        ),
        cluster_label = paste0(
          "第",
          cluster,
          "群｜",
          type
        )
      )
    
    m2 <- m2 %>%
      left_join(
        group_character %>%
          select(
            cluster,
            cluster_label
          ),
        by = "cluster"
      )
    
    list(
      data = m2,
      pca = pca,
      quality = quality,
      best_k = best_k,
      group_character = group_character,
      window_days = window_days,
      pc_count = pc_count
    )
  })
  
  
  # ==========================================================
  # 2. 共用日期工具
  # ==========================================================
  
  selected_market_date <- reactive({
    
    req(
      input$market_observation_date
    )
    
    nearest_date(
      trading_dates(),
      input$market_observation_date
    )
  })
  
  selected_monitor_date <- reactive({
    
    req(
      input$monitor_date
    )
    
    nearest_date(
      trading_dates(),
      input$monitor_date
    )
  })
  
  
  output$header_latest_date <- renderText({
    format(latest_date(), "%Y-%m-%d")
  })
  
  # ==========================================================
  # 3. 市場總覽
  # ==========================================================
  
  output$global_latest_date <- renderText({
    format(
      latest_date(),
      "%Y-%m-%d"
    )
  })
  
  output$global_earliest_date <- renderText({
    format(
      earliest_date(),
      "%Y-%m-%d"
    )
  })
  
  output$global_stock_count <- renderText({
    n_distinct(
      data()$symbol
    )
  })
  
  output$global_observation_count <- renderText({
    fmt_num(
      nrow(data())
    )
  })
  
  output$global_file_modified <- renderText({
    
    info <- file.info(
      DATA_FILE
    )
    
    if (
      nrow(info) == 0 ||
      is.na(info$mtime)
    ) {
      
      return("無法取得")
      
    }
    
    format(
      info$mtime,
      "%Y-%m-%d %H:%M:%S"
    )
  })
  
  
  # ----------------------------------------------------------
  # 市場觀察日期自動同步
  # ----------------------------------------------------------
  
  observe({
    
    req(
      latest_date()
    )
    
    updateDateInput(
      session,
      "market_observation_date",
      value = latest_date(),
      min = earliest_date(),
      max = latest_date()
    )
    
  })
  
  
  observe({
    
    req(
      latest_date()
    )
    
    updateDateInput(
      session,
      "monitor_date",
      value = latest_date(),
      min = earliest_date(),
      max = latest_date()
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 0050 最新價格
  # ----------------------------------------------------------
  
  output$market_0050_price <- renderText({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date <= d,
        is.finite(adjusted)
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(
        n = 1
      )
    
    req(
      nrow(x) > 0
    )
    
    fmt_price(
      x$adjusted[1]
    )
  })
  
  
  # ----------------------------------------------------------
  # 0050 最近一日普通報酬
  # ----------------------------------------------------------
  
  output$market_0050_daily_return <- renderText({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date <= d,
        is.finite(log_return)
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(
        n = 1
      )
    
    req(
      nrow(x) > 0
    )
    
    fmt_pct(
      x$log_return[1]
    )
  })
  
  
  # ----------------------------------------------------------
  # 0050 YTD
  # ----------------------------------------------------------
  
  output$market_ytd_return <- renderText({
    
    d <- selected_market_date()
    
    year_now <-
      format(
        d,
        "%Y"
      )
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date <= d,
        format(date, "%Y") == year_now,
        is.finite(adjusted)
      ) %>%
      arrange(date)
    
    req(
      nrow(x) >= 2
    )
    
    ytd <-
      tail(x$adjusted, 1) /
      head(x$adjusted, 1) - 1
    
    fmt_pct(
      ytd
    )
  })
  
  
  # ----------------------------------------------------------
  # 50 檔成分股漲跌家數
  # ----------------------------------------------------------
  
  output$market_breadth <- renderText({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol %in% stock_info$symbol,
        date <= d,
        is.finite(daily_return)
      ) %>%
      group_by(symbol) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(n = 1) %>%
      ungroup()
    
    up <- sum(
      x$daily_return > 0,
      na.rm = TRUE
    )
    
    down <- sum(
      x$daily_return < 0,
      na.rm = TRUE
    )
    
    flat <- sum(
      x$daily_return == 0,
      na.rm = TRUE
    )
    
    paste0(
      up,
      " ↑ / ",
      down,
      " ↓ / ",
      flat,
      " –"
    )
  })
  
  
  # ----------------------------------------------------------
  # 0050 歷史價格
  # ----------------------------------------------------------
  
  output$market_0050_price_chart <- renderPlot({
    
    req(
      input$market_chart_date_range
    )
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date >= as.Date(
          input$market_chart_date_range[1]
        ),
        date <= as.Date(
          input$market_chart_date_range[2]
        ),
        is.finite(adjusted)
      ) %>%
      arrange(date)
    
    req(
      nrow(x) > 1
    )
    
    # ----------------------------------------------------------
    # 未勾選：
    # 整條線維持黑色
    # ----------------------------------------------------------
    
    if (
      !isTRUE(
        input$market_price_color_by_change
      )
    ) {
      
      return(
        
        ggplot(
          x,
          aes(
            x = date,
            y = adjusted
          )
        ) +
          
          geom_line(
            color = stock_colors["0050"],
            linewidth = 1.0
          ) +
          
          labs(
            title = "0050 調整後收盤價",
            x = "日期",
            y = "調整後價格"
          ) +
          
          theme_minimal() +
          
          theme(
            plot.title =
              element_text(
                hjust = 0.5,
                face = "bold"
              ),
            
            axis.text.x =
              element_text(
                angle = 45,
                hjust = 1
              )
          )
        
      )
    }
    
    
    # ----------------------------------------------------------
    # 勾選：
    # 依當日漲跌把線段染色
    #
    # 紅 = 上漲
    # 綠 = 下跌
    # 灰 = 持平
    # ----------------------------------------------------------
    
    x <- x %>%
      mutate(
        next_date = lead(date),
        next_price = lead(adjusted),
        
        direction = case_when(
          lead(adjusted) > adjusted ~ "上漲",
          lead(adjusted) < adjusted ~ "下跌",
          TRUE ~ "持平"
        )
      ) %>%
      filter(
        is.finite(next_price)
      )
    
    
    ggplot() +
      
      geom_segment(
        data = x,
        aes(
          x = date,
          y = adjusted,
          xend = next_date,
          yend = next_price,
          color = direction
        ),
        linewidth = 0.85
      ) +
      
      scale_color_manual(
        values = c(
          "上漲" = UP_COLOR,
          "下跌" = DOWN_COLOR,
          "持平" = NEUTRAL_COLOR
        )
      ) +
      
      labs(
        title = "0050 調整後收盤價",
        x = "日期",
        y = "調整後價格",
        color = NULL
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        
        legend.position =
          "bottom",
        
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 0050 近一年每日對數報酬
  # ----------------------------------------------------------
  
  output$market_0050_daily_return_timeseries <- renderPlot({
    
    d <- selected_market_date()
    
    dates <- trading_dates()
    
    end_index <- max(
      which(
        dates <= d
      )
    )
    
    start_index <- max(
      1,
      end_index - 251
    )
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date >= dates[start_index],
        date <= d,
        is.finite(log_return)
      ) %>%
      arrange(date)
    
    req(
      nrow(x) > 1
    )
    
    # --------------------------------------------------------
    # 不啟用著色：黑色
    # --------------------------------------------------------
    
    if (
      !isTRUE(
        input$market_return_color_by_change
      )
    ) {
      
      return(
        
        ggplot(
          x,
          aes(
            x = date,
            y = log_return
          )
        ) +
          
          geom_hline(
            yintercept = 0,
            linetype = "dashed"
          ) +
          
          geom_line(
            color = stock_colors["0050"],
            linewidth = 0.8
          ) +
          
          labs(
            title = "0050 近一年每日對數報酬率",
            x = "日期",
            y = "日對數報酬率"
          ) +
          
          scale_y_continuous(
            labels = function(z) {
              paste0(
                round(
                  z * 100,
                  2
                ),
                "%"
              )
            }
          ) +
          
          theme_minimal() +
          
          theme(
            plot.title =
              element_text(
                hjust = 0.5,
                face = "bold"
              )
          )
        
      )
    }
    
    # --------------------------------------------------------
    # 啟用著色
    # --------------------------------------------------------
    
    x <- x %>%
      mutate(
        next_date =
          lead(date),
        
        next_return =
          lead(log_return),
        
        direction =
          case_when(
            next_return > log_return ~ "上升",
            next_return < log_return ~ "下降",
            TRUE ~ "持平"
          )
      ) %>%
      filter(
        is.finite(next_return)
      )
    
    ggplot(
      x
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_segment(
        aes(
          x = date,
          y = log_return,
          xend = next_date,
          yend = next_return,
          color = direction
        ),
        linewidth = 0.8
      ) +
      
      scale_color_manual(
        values = c(
          "上升" = UP_COLOR,
          "下降" = DOWN_COLOR,
          "持平" = NEUTRAL_COLOR
        )
      ) +
      
      labs(
        title = "0050 近一年每日對數報酬率",
        x = "日期",
        y = "日對數報酬率",
        color = NULL
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              2
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        
        legend.position =
          "bottom"
      )
    
  })
  
  # ----------------------------------------------------------
  # 近 30 個交易日排名
  # ----------------------------------------------------------
  
  output$market_30d_return_ranking <- renderPlot({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol %in% stock_info$symbol,
        date <= d,
        is.finite(log_return)
      ) %>%
      group_by(symbol) %>%
      arrange(date, .by_group = TRUE) %>%
      slice_tail(n = 30) %>%
      summarise(
        n = n(),
        return_30d =
          ifelse(
            n >= 20,
            sum(log_return, na.rm = TRUE),
            NA_real_
          ),
        .groups = "drop"
      ) %>%
      filter(
        is.finite(return_30d)
      )
    
    req(
      nrow(x) > 0
    )
    
    x <- x %>%
      arrange(return_30d) %>%
      mutate(
        label = paste0(
          symbol,
          " ",
          get_stock_name(symbol)
        )
      )
    
    x <- x %>%
      mutate(
        direction = case_when(
          return_30d > 0 ~ "上漲",
          return_30d < 0 ~ "下跌",
          TRUE ~ "持平"
        )
      )
    
    ggplot(
      x,
      aes(
        x = return_30d,
        y = reorder(label, return_30d),
        fill = direction
      )
    ) +
      
      geom_col(
        width = 0.7
      ) +
      
      scale_fill_manual(
        values = c(
          "上漲" = UP_COLOR,
          "下跌" = DOWN_COLOR,
          "持平" = NEUTRAL_COLOR
        )
      )  +
      geom_vline(
        xintercept = 0,
        linetype = "dashed"
      ) +
      labs(
        title = "50 檔成分股近 30 個交易日累積對數報酬",
        x = "累積對數報酬",
        y = NULL
      ) +
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(z * 100, 1),
            "%"
          )
        }
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 最新行情表
  # ----------------------------------------------------------
  
  latest_market_table <- reactive({
    
    d <- selected_market_date()
    
    symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    result <- lapply(
      symbols,
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            date <= d
          ) %>%
          arrange(
            desc(date)
          ) %>%
          slice_head(n = 1)
        
        if (nrow(x) == 0) {
          return(NULL)
        }
        
        data.frame(
          股票代號 = s,
          股票名稱 = get_stock_name(s),
          日期 = x$date[1],
          調整後價格 = x$adjusted[1],
          日對數報酬率 = x$log_return[1],
          成交量 = x$volume[1],
          stringsAsFactors = FALSE
        )
      }
    )
    
    bind_rows(result)
    
  })
  
  
  output$realtime_monitor_table <- renderTable({
    
    latest_market_table() %>%
      mutate(
        日期 = format(
          日期,
          "%Y-%m-%d"
        ),
        調整後價格 = fmt_price(
          調整後價格
        ),
        日對數報酬率 = fmt_pct(
          日對數報酬率
        ),
        成交量 = fmt_num(
          成交量
        )
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  
  # ==========================================================
  # 4. 51 檔行情監控
  # ==========================================================
  
  monitor_data <- reactive({
    
    d <- selected_monitor_date()
    
    symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    result <- lapply(
      symbols,
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            date <= d
          ) %>%
          arrange(
            desc(date)
          ) %>%
          slice_head(
            n = 1
          )
        
        if (
          nrow(x) == 0
        ) {
          return(NULL)
        }
        
        data.frame(
          股票代號 = s,
          股票名稱 = get_stock_name(s),
          日期 = x$date[1],
          收盤價 = x$close[1],
          調整後價格 = x$adjusted[1],
          日對數報酬率 = x$log_return[1],
          成交量 = x$volume[1],
          stringsAsFactors = FALSE
        )
        
      }
    )
    
    bind_rows(
      result
    )
    
  })
  
  
  output$monitor_table <- renderTable({
    
    monitor_data() %>%
      mutate(
        日期 = format(
          日期,
          "%Y-%m-%d"
        ),
        收盤價 = fmt_price(
          收盤價
        ),
        調整後價格 = fmt_price(
          調整後價格
        ),
        日對數報酬率 = fmt_pct(
          日對數報酬率
        ),
        成交量 = fmt_num(
          成交量
        )
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ==========================================================
  # 5. 個股分析
  # ==========================================================
  
  analysis_range <- reactive({
    
    req(
      input$date_range
    )
    
    as.Date(
      input$date_range
    )
    
  })
  
  
  analysis_symbols <- reactive({
    
    s <- input$selected_stock
    
    if (
      is.null(s) ||
      length(s) == 0
    ) {
      return("0050")
    }
    
    head(
      unique(s),
      MAX_SELECTED_STOCKS
    )
    
  })
  
  
  analysis_data <- reactive({
    
    r <- analysis_range()
    
    data() %>%
      filter(
        symbol %in% analysis_symbols(),
        date >= r[1],
        date <= r[2]
      )
    
  })
  
  
  output$analysis_period_text <- renderText({
    
    r <- analysis_range()
    
    paste0(
      "目前分析期間：",
      format(r[1], "%Y-%m-%d"),
      " 至 ",
      format(r[2], "%Y-%m-%d")
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 快速期間
  # ----------------------------------------------------------
  
  update_analysis_range <- function(
    start_date,
    end_date
  ) {
    
    start_date <- nearest_date(
      trading_dates(),
      start_date
    )
    
    end_date <- nearest_date(
      trading_dates(),
      end_date
    )
    
    if (
      start_date > end_date
    ) {
      tmp <- start_date
      start_date <- end_date
      end_date <- tmp
    }
    
    updateDateRangeInput(
      session,
      "date_range",
      start = start_date,
      end = end_date
    )
  }
  
  
  observeEvent(
    input$period_research,
    {
      update_analysis_range(
        RESEARCH_START_DATE,
        min(
          RESEARCH_END_DATE,
          latest_date()
        )
      )
    }
  )
  
  
  observeEvent(
    input$period_all,
    {
      update_analysis_range(
        earliest_date(),
        latest_date()
      )
    }
  )
  
  
  
  observeEvent(
    input$period_1w,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 5) {
        update_analysis_range(
          dates[n - 4],
          dates[n]
        )
      }
    }
  )
  
  observeEvent(
    input$period_1m,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 21) {
        update_analysis_range(
          dates[n - 20],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_1q,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 63) {
        update_analysis_range(
          dates[n - 62],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_6m,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 126) {
        update_analysis_range(
          dates[n - 125],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_1y,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 252) {
        update_analysis_range(
          dates[n - 251],
          dates[n]
        )
      }
    }
  )
  
  
  
  observeEvent(
    input$period_3y,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 756) {
        update_analysis_range(
          dates[n - 755],
          dates[n]
        )
      } else if (n >= 2) {
        update_analysis_range(
          dates[1],
          dates[n]
        )
      }
    }
  )
  
  observeEvent(
    input$period_ytd,
    {
      d <- latest_date()
      
      y <- format(
        d,
        "%Y"
      )
      
      y_dates <- trading_dates()[
        format(
          trading_dates(),
          "%Y"
        ) == y &
          trading_dates() <= d
      ]
      
      if (length(y_dates) >= 2) {
        
        update_analysis_range(
          min(y_dates),
          max(y_dates)
        )
      }
    }
  )
  
  
  # ----------------------------------------------------------
  # 個股資料期間
  # ----------------------------------------------------------
  
  output$selected_stock_ranges <- renderTable({
    
    result <- lapply(
      analysis_symbols(),
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            is.finite(adjusted)
          )
        
        if (nrow(x) == 0) {
          return(NULL)
        }
        
        data.frame(
          股票代號 = s,
          股票名稱 = get_stock_name(s),
          最早日期 = format(
            min(x$date),
            "%Y-%m-%d"
          ),
          最新日期 = format(
            max(x$date),
            "%Y-%m-%d"
          ),
          有效觀測數 = nrow(x),
          stringsAsFactors = FALSE
        )
      }
    )
    
    bind_rows(result)
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 資料期間警告
  # ----------------------------------------------------------
  
  output$stock_warning <- renderUI({
    
    r <- analysis_range()
    
    selected <- analysis_symbols()
    
    warnings <- lapply(
      selected,
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            is.finite(adjusted)
          )
        
        if (nrow(x) == 0) {
          return(
            div(
              class = "warning-note",
              paste0(
                get_stock_label(s),
                " 沒有有效價格資料。"
              )
            )
          )
        }
        
        actual_start <- min(x$date)
        actual_end <- max(x$date)
        
        if (
          actual_start > r[1] ||
          actual_end < r[2]
        ) {
          
          div(
            class = "warning-note",
            paste0(
              get_stock_label(s),
              " 的實際資料範圍為 ",
              format(actual_start, "%Y-%m-%d"),
              " 至 ",
              format(actual_end, "%Y-%m-%d"),
              "，與目前分析期間不完全重疊。"
            )
          )
          
        } else {
          
          NULL
        }
      }
    )
    
    do.call(
      tagList,
      warnings
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 個股調整後收盤價
  # 所選股票全部合併在同一張圖
  # ----------------------------------------------------------
  
  output$price_plot <- renderPlot({
    
    symbols <- analysis_symbols()
    
    req(
      length(symbols) >= 1
    )
    
    x <- analysis_data() %>%
      filter(
        symbol %in% symbols,
        is.finite(adjusted)
      ) %>%
      arrange(
        date
      )
    
    req(
      nrow(x) > 0
    )
    
    # --------------------------------------------------------
    # 建立目前選擇股票的代表色
    # --------------------------------------------------------
    
    plot_colors <- v6_order_colors(symbols)
    
    # --------------------------------------------------------
    # 建立股票名稱
    # --------------------------------------------------------
    
    plot_labels <- get_stock_label(
      symbols
    )
    
    names(plot_labels) <- symbols
    
    # --------------------------------------------------------
    # 繪圖
    # --------------------------------------------------------
    
    ggplot(
      x,
      aes(
        x = date,
        y = adjusted,
        color = symbol,
        group = symbol
      )
    ) +
      
      geom_line(
        linewidth = 0.9,
        na.rm = TRUE
      ) +
      
      scale_color_manual(
        values = plot_colors,
        breaks = symbols,
        labels = plot_labels,
        drop = FALSE
      ) +
      
      labs(
        title = "個股調整後收盤價",
        x = "日期",
        y = "調整後價格",
        color = NULL
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        
        legend.position =
          "bottom",
        
        legend.text =
          element_text(
            size = 10
          ),
        
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 累積報酬（補回舊版 UI 原本缺少的 Server 輸出）
  # ----------------------------------------------------------
  
  output$cumulative_return_display <- renderUI({
    req(length(analysis_symbols()) > 0)
    plotOutput("cumulative_return_plot", height = "500px")
  })
  
  output$cumulative_return_plot <- renderPlot({
    symbols <- analysis_symbols()
    x <- analysis_data() %>%
      filter(symbol %in% symbols, is.finite(log_return)) %>%
      group_by(symbol) %>%
      arrange(date, .by_group = TRUE) %>%
      mutate(cumulative_return = exp(cumsum(log_return)) - 1) %>%
      ungroup()
    req(nrow(x) > 0)
    ggplot(x, aes(x=date, y=cumulative_return, color=symbol, group=symbol)) +
      geom_hline(yintercept=0, linetype="dashed", color="#94a3b8") +
      geom_line(linewidth=1.0) +
      scale_color_manual(
        values=v6_order_colors(symbols),
        breaks=symbols,
        labels=setNames(get_stock_label(symbols), symbols),
        drop=FALSE
      ) +
      scale_y_continuous(labels=function(z) paste0(round(z*100,1), "%")) +
      labs(title="期間累積報酬", x="日期", y="累積報酬率", color=NULL) +
      theme_minimal() +
      theme(plot.title=element_text(hjust=0.5, face="bold"), legend.position="bottom")
  })
  
  # ----------------------------------------------------------
  # 日對數報酬描述統計
  # ----------------------------------------------------------
  
  output$return_summary <- renderTable({
    
    x <- analysis_data() %>%
      filter(
        is.finite(log_return)
      ) %>%
      group_by(symbol) %>%
      summarise(
        有效交易日 = n(),
        平均數 = mean(
          log_return,
          na.rm = TRUE
        ),
        標準差 = sd(
          log_return,
          na.rm = TRUE
        ),
        最大值 = max(
          log_return,
          na.rm = TRUE
        ),
        最小值 = min(
          log_return,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    req(
      nrow(x) > 0
    )
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        平均數 = fmt_pct(平均數, 3),
        標準差 = fmt_pct(
          標準差,
          3,
          show_sign = FALSE
        ),
        最大值 = fmt_pct(最大值, 3),
        最小值 = fmt_pct(最小值, 3)
      ) %>%
      select(
        股票代號,
        股票名稱,
        有效交易日,
        平均數,
        標準差,
        最大值,
        最小值
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 日對數報酬分布
  # ----------------------------------------------------------
  
  output$return_distribution_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    req(
      length(symbols) > 0
    )
    
    lapply(
      
      seq(
        1,
        length(symbols),
        by = 3
      ),
      
      function(i) {
        
        ids <-
          symbols[
            i:min(
              i + 2,
              length(symbols)
            )
          ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(symbol) {
              
              local({
                
                s <- symbol
                
                plot_id <-
                  paste0(
                    "return_distribution_plot_",
                    s
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- analysis_data() %>%
                    filter(
                      symbol == s,
                      is.finite(log_return)
                    )
                  
                  req(
                    nrow(x) > 10
                  )
                  
                  base_col <-
                    stock_colors[s]
                  
                  light_col <-
                    adjustcolor(
                      base_col,
                      alpha.f = 0.50
                    )
                  
                  hist_col <-
                    adjustcolor(
                      base_col,
                      alpha.f = 0.35
                    )
                  
                  mu <-
                    mean(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  sig <-
                    sd(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  p <- ggplot(
                    
                    x,
                    
                    aes(
                      x = log_return
                    )
                    
                  ) +
                    
                    geom_histogram(
                      
                      aes(
                        y =
                          after_stat(
                            density
                          )
                      ),
                      
                      bins = 40,
                      
                      fill =
                        light_col,
                      
                      color =
                        hist_col,
                      
                      linewidth = 0.3
                      
                    ) +
                    
                    labs(
                      
                      title =
                        get_stock_label(s),
                      
                      x =
                        "日對數報酬率",
                      
                      y =
                        "密度"
                      
                    ) +
                    
                    scale_x_continuous(
                      
                      labels =
                        function(z) {
                          
                          paste0(
                            round(
                              z * 100,
                              1
                            ),
                            "%"
                          )
                          
                        }
                      
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        )
                      
                    )
                  
                  if (
                    is.finite(sig) &&
                    sig > 0
                  ) {
                    
                    p <- p +
                      
                      stat_function(
                        
                        fun = dnorm,
                        
                        args = list(
                          mean = mu,
                          sd = sig
                        ),
                        
                        color =
                          base_col,
                        
                        linewidth =
                          1.2
                        
                      )
                    
                  }
                  
                  p
                  
                })
                
                column(
                  
                  width = 4,
                  
                  plotOutput(
                    plot_id,
                    height = "350px"
                  )
                  
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 日對數報酬箱型圖
  # 固定 1 × 5 排版
  # 即使只選 1 檔，也保留 5 個固定位置
  # ----------------------------------------------------------
  
  output$return_boxplot_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    req(
      length(symbols) > 0
    )
    
    div(
      style = "
      display:flex;
      width:100%;
      align-items:flex-start;
    ",
      
      lapply(
        seq_len(5),
        function(i) {
          
          if (i <= length(symbols)) {
            
            local({
              
              s <- symbols[i]
              
              plot_id <- paste0(
                "return_boxplot_",
                s
              )
              
              output[[plot_id]] <- renderPlot({
                
                x <- analysis_data() %>%
                  filter(
                    symbol == s,
                    is.finite(log_return)
                  )
                
                req(
                  nrow(x) > 0
                )
                
                current_color <-
                  stock_colors[s]
                
                ggplot(
                  x,
                  aes(
                    x = "",
                    y = log_return
                  )
                ) +
                  
                  geom_hline(
                    yintercept = 0,
                    linetype = "dashed"
                  ) +
                  
                  geom_boxplot(
                    width = 0.55,
                    fill = current_color,
                    color = current_color,
                    outlier.alpha = 0.35
                  ) +
                  
                  labs(
                    title = get_stock_label(s),
                    x = NULL,
                    y = "日對數報酬率"
                  ) +
                  
                  scale_y_continuous(
                    labels = function(z) {
                      paste0(
                        round(
                          z * 100,
                          2
                        ),
                        "%"
                      )
                    }
                  ) +
                  
                  theme_minimal() +
                  
                  theme(
                    plot.title =
                      element_text(
                        hjust = 0.5,
                        face = "bold",
                        size = 12
                      ),
                    
                    axis.text.x =
                      element_blank(),
                    
                    axis.ticks.x =
                      element_blank(),
                    
                    plot.margin =
                      margin(
                        10,
                        5,
                        10,
                        5
                      )
                  )
                
              })
              
              div(
                style = "
                flex:0 0 20%;
                width:20%;
                padding:0 5px;
              ",
                
                plotOutput(
                  plot_id,
                  height = "400px"
                )
              )
              
            })
            
          } else {
            
            div(
              style = "
              flex:0 0 20%;
              width:20%;
              padding:0 5px;
              height:400px;
            "
            )
            
          }
          
        }
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # 每檔股票獨立日對數報酬時間序列
  # ----------------------------------------------------------
  
  output$log_return_series_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    lapply(
      
      seq(
        1,
        length(symbols),
        by = 2
      ),
      
      function(i) {
        
        ids <-
          symbols[
            i:min(
              i + 1,
              length(symbols)
            )
          ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(symbol) {
              
              local({
                
                s <- symbol
                
                plot_id <-
                  paste0(
                    "log_return_plot_",
                    s
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- analysis_data() %>%
                    filter(
                      symbol == s,
                      is.finite(log_return)
                    )
                  
                  req(
                    nrow(x) > 1
                  )
                  
                  ggplot(
                    
                    x,
                    
                    aes(
                      x = date,
                      y = log_return
                    )
                    
                  ) +
                    
                    geom_hline(
                      yintercept = 0,
                      linetype = "dashed"
                    ) +
                    
                    geom_line(
                      color =
                        stock_colors[s],
                      linewidth = 0.7
                    ) +
                    
                    labs(
                      title =
                        get_stock_label(s),
                      x = "日期",
                      y = "日對數報酬率"
                    ) +
                    
                    scale_y_continuous(
                      labels = function(z) {
                        paste0(
                          round(
                            z * 100,
                            2
                          ),
                          "%"
                        )
                      }
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        ),
                      axis.text.x =
                        element_text(
                          angle = 45,
                          hjust = 1
                        )
                    )
                  
                })
                
                column(
                  
                  width = 6,
                  
                  plotOutput(
                    plot_id,
                    height = "270px"
                  )
                  
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 期間績效
  # ----------------------------------------------------------
  
  output$period_performance <- renderTable({
    
    x <- analysis_data() %>%
      filter(
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      arrange(date, .by_group = TRUE) %>%
      summarise(
        起始日期 = first(date),
        結束日期 = last(date),
        起始價格 = first(adjusted),
        結束價格 = last(adjusted),
        有效交易日 = n(),
        .groups = "drop"
      ) %>%
      mutate(
        累積報酬率 =
          結束價格 /
          起始價格 - 1,
        年化報酬率 =
          mapply(
            annualized_return,
            起始價格,
            結束價格,
            有效交易日
          )
      )
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        起始日期 = format(
          起始日期,
          "%Y-%m-%d"
        ),
        結束日期 = format(
          結束日期,
          "%Y-%m-%d"
        ),
        起始價格 = fmt_price(
          起始價格
        ),
        結束價格 = fmt_price(
          結束價格
        ),
        累積報酬率 = fmt_pct(
          累積報酬率
        ),
        年化報酬率 = fmt_pct(
          年化報酬率
        )
      ) %>%
      select(
        股票代號,
        股票名稱,
        起始日期,
        起始價格,
        結束日期,
        結束價格,
        有效交易日,
        累積報酬率,
        年化報酬率
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 價格摘要
  # ----------------------------------------------------------
  
  output$price_summary <- renderTable({
    
    x <- analysis_data() %>%
      filter(
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      summarise(
        平均價格 = mean(
          adjusted,
          na.rm = TRUE
        ),
        價格標準差 = sd(
          adjusted,
          na.rm = TRUE
        ),
        期間最低價 = min(
          adjusted,
          na.rm = TRUE
        ),
        期間最高價 = max(
          adjusted,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        平均價格 = fmt_price(
          平均價格
        ),
        價格標準差 = fmt_price(
          價格標準差
        ),
        期間最低價 = fmt_price(
          期間最低價
        ),
        期間最高價 = fmt_price(
          期間最高價
        )
      ) %>%
      select(
        股票代號,
        股票名稱,
        平均價格,
        價格標準差,
        期間最低價,
        期間最高價
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 風險摘要
  # ----------------------------------------------------------
  
  risk_data <- reactive({
    
    x <- analysis_data() %>%
      filter(
        is.finite(log_return),
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      summarise(
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        max_drawdown =
          min(
            drawdown_value(adjusted),
            na.rm = TRUE
          ),
        
        mean_daily =
          mean(
            log_return,
            na.rm = TRUE
          ),
        
        .groups = "drop"
      ) %>%
      mutate(
        
        sharpe =
          ifelse(
            annual_volatility > 0,
            mean_daily *
              TRADING_DAYS_PER_YEAR /
              annual_volatility,
            NA_real_
          )
        
      )
    
    x
    
  })
  
  
  output$risk_summary <- renderTable({
    
    x <- risk_data()
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        年化波動度 = fmt_pct(
          annual_volatility,
          2,
          FALSE
        ),
        最大回撤 = fmt_pct(
          max_drawdown
        ),
        夏普比率 =
          ifelse(
            is.finite(sharpe),
            sprintf(
              "%.3f",
              sharpe
            ),
            "NA"
          )
      ) %>%
      select(
        股票代號,
        股票名稱,
        年化波動度,
        最大回撤,
        夏普比率
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 回撤圖
  # ----------------------------------------------------------
  
  output$drawdown_charts_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    lapply(
      
      seq(
        1,
        length(symbols),
        by = 2
      ),
      
      function(i) {
        
        ids <- symbols[
          i:min(
            i + 1,
            length(symbols)
          )
        ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(symbol) {
              
              local({
                
                s <- symbol
                
                plot_id <-
                  paste0(
                    "drawdown_plot_",
                    s
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- analysis_data() %>%
                    filter(
                      symbol == s,
                      is.finite(adjusted)
                    ) %>%
                    arrange(date)
                  
                  req(
                    nrow(x) > 1
                  )
                  
                  x$drawdown <-
                    drawdown_value(
                      x$adjusted
                    )
                  
                  ggplot(
                    x,
                    aes(
                      x = date,
                      y = drawdown
                    )
                  ) +
                    
                    geom_hline(
                      yintercept = 0,
                      linetype = "dashed"
                    ) +
                    
                    geom_line(
                      color =
                        stock_colors[s],
                      linewidth = 0.9
                    ) +
                    
                    labs(
                      title =
                        get_stock_label(s),
                      x = "日期",
                      y = "回撤"
                    ) +
                    
                    scale_y_continuous(
                      labels = function(z) {
                        paste0(
                          round(
                            z * 100,
                            1
                          ),
                          "%"
                        )
                      }
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        ),
                      
                      axis.text.x =
                        element_text(
                          angle = 45,
                          hjust = 1
                        )
                    )
                  
                })
                
                column(
                  6,
                  plotOutput(
                    plot_id,
                    height = "320px"
                  )
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 報酬與風險圖
  # ----------------------------------------------------------
  
  output$return_risk_chart <- renderPlot({
    
    x <- analysis_data() %>%
      filter(
        is.finite(log_return),
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      summarise(
        
        annual_return =
          annualized_return(
            first(adjusted),
            last(adjusted),
            n()
          ),
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      
      x,
      
      aes(
        x = annual_volatility,
        y = annual_return,
        color = symbol,
        label = symbol
      )
      
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_point(
        size = 3
      ) +
      
      geom_text(
        vjust = -0.8
      ) +
      
      scale_color_manual(
        values = stock_colors,
        guide = "none"
      ) +
      
      labs(
        title = "報酬與風險關係",
        x = "年化波動度",
        y = "年化報酬率"
      ) +
      
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          )
      )
    
  })
  
  
  
  # ----------------------------------------------------------
  # 個股 vs 0050：整體相關係數、Beta、60 日滾動相關性
  # ----------------------------------------------------------
  individual_relation_data <- reactive({
    symbols <- setdiff(analysis_symbols(), "0050")
    if (length(symbols) == 0) return(data.frame())
    r <- analysis_range()
    bench <- data() %>%
      filter(symbol == "0050", date >= r[1], date <= r[2], is.finite(log_return)) %>%
      select(date, benchmark_return = log_return)
    bind_rows(lapply(symbols, function(s) {
      z <- data() %>%
        filter(symbol == s, date >= r[1], date <= r[2], is.finite(log_return)) %>%
        select(date, stock_return = log_return) %>%
        inner_join(bench, by = "date") %>%
        arrange(date)
      if (nrow(z) < 20) return(NULL)
      bvar <- var(z$benchmark_return, na.rm = TRUE)
      beta <- if (is.finite(bvar) && bvar > 0) cov(z$stock_return, z$benchmark_return, use = "complete.obs") / bvar else NA_real_
      data.frame(
        symbol = s,
        name = get_stock_name(s),
        observations = nrow(z),
        correlation = cor(z$stock_return, z$benchmark_return, use = "complete.obs"),
        beta = beta,
        stringsAsFactors = FALSE
      )
    }))
  })
  
  output$individual_correlation_table <- renderTable({
    x <- individual_relation_data()
    if (nrow(x) == 0) {
      return(data.frame(說明 = "請至少再選 1 檔成分股，才能和 0050 比較相關性與 Beta。"))
    }
    x %>% transmute(
      股票 = paste0(name, " (", symbol, ")"),
      有效交易日 = observations,
      與0050相關係數 = sprintf("%.3f", correlation),
      Beta = sprintf("%.3f", beta),
      白話解讀 = case_when(
        correlation >= 0.8 ~ "和 0050 的漲跌通常很同步",
        correlation >= 0.5 ~ "和 0050 有中度同步",
        correlation > 0 ~ "有同向關係，但同步程度較弱",
        TRUE ~ "和 0050 的同向關係不明顯"
      )
    )
  }, striped = FALSE, bordered = TRUE, hover = TRUE)
  
  output$individual_rolling_correlation_chart <- renderPlot({
    symbols <- setdiff(analysis_symbols(), "0050")
    req(length(symbols) > 0)
    r <- analysis_range()
    bench <- data() %>%
      filter(symbol == "0050", date >= r[1], date <= r[2], is.finite(log_return)) %>%
      select(date, benchmark_return = log_return)
    long <- bind_rows(lapply(symbols, function(s) {
      z <- data() %>%
        filter(symbol == s, date >= r[1], date <= r[2], is.finite(log_return)) %>%
        select(date, stock_return = log_return) %>%
        inner_join(bench, by = "date") %>%
        arrange(date)
      if (nrow(z) < 60) return(NULL)
      z$rolling_correlation <- rolling_correlation_fast(z$stock_return, z$benchmark_return, 60)
      z$symbol <- s
      z
    })) %>% filter(is.finite(rolling_correlation))
    req(nrow(long) > 0)
    ggplot(long, aes(x = date, y = rolling_correlation, color = symbol)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#b4bec8") +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = stock_colors, labels = function(z) get_stock_label(z)) +
      scale_y_continuous(limits = c(-1, 1)) +
      labs(title = "與 0050 的 60 日滾動相關性", x = "日期", y = "相關係數", color = NULL) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")
  })
  
  # ==========================================================
  # 6. 融合版：0050 摘要、綜合排名、風險穩定性、主成分與分群
  # ==========================================================
  
  output$market_0050_growth_score <- renderText({
    x <- research_metrics() %>% filter(symbol == "0050")
    req(nrow(x) == 1)
    paste0(fmt_score(x$growth_score), " / 100")
  })
  
  output$market_0050_risk_score <- renderText({
    x <- research_metrics() %>% filter(symbol == "0050")
    req(nrow(x) == 1)
    paste0(fmt_score(x$risk_score), " / 100")
  })
  
  output$market_0050_stability_score <- renderText({
    x <- research_metrics() %>% filter(symbol == "0050")
    req(nrow(x) == 1)
    paste0(fmt_score(x$stability_score), " / 100")
  })
  
  output$market_0050_volatility <- renderText({
    x <- research_metrics() %>% filter(symbol == "0050")
    req(nrow(x) == 1)
    fmt_pct(x$annual_volatility, 2, FALSE)
  })
  
  output$market_0050_max_drawdown <- renderText({
    x <- research_metrics() %>% filter(symbol == "0050")
    req(nrow(x) == 1)
    fmt_pct(x$max_drawdown, 2, TRUE)
  })
  
  output$market_0050_trend <- renderText({
    x <- data() %>% filter(symbol == "0050", is.finite(adjusted)) %>% arrange(date)
    req(nrow(x) >= 60)
    p <- tail(x$adjusted, 1)
    ma20 <- mean(tail(x$adjusted, 20), na.rm=TRUE)
    ma60 <- mean(tail(x$adjusted, 60), na.rm=TRUE)
    if (p > ma20 && ma20 > ma60) "偏強" else if (p < ma20 && ma20 < ma60) "偏弱" else "整理"
  })
  
  output$market_0050_rsi <- renderText({
    x <- data() %>% filter(symbol == "0050", is.finite(adjusted)) %>% arrange(date)
    value <- rsi_value(x$adjusted, 14)
    if (!is.finite(value)) return("NA")
    state <- if (value >= 70) "動能偏熱" else if (value <= 30) "動能偏弱" else "中性"
    paste0(sprintf("%.1f", value), "｜", state)
  })
  
  output$market_0050_vol_state <- renderText({
    x <- data() %>% filter(symbol == "0050", is.finite(log_return)) %>% arrange(date)
    req(nrow(x) >= 63)
    recent <- sd(tail(x$log_return, 21), na.rm=TRUE)
    base <- sd(tail(x$log_return, min(252, nrow(x))), na.rm=TRUE)
    ratio <- recent / base
    if (!is.finite(ratio)) "NA" else if (ratio >= 1.20) "波動偏高" else if (ratio <= 0.80) "波動偏低" else "接近一年水準"
  })
  
  output$ranking_period_info <- renderText({
    x <- research_metrics() %>%
      filter(symbol == "0050") %>%
      slice_head(n = 1)
    
    req(nrow(x) == 1)
    
    paste0(
      "主風險、近 1 年年化報酬與夏普：",
      format(x$base_start_date, "%Y-%m-%d"),
      " 至 ",
      format(x$base_end_date, "%Y-%m-%d"),
      "；穩定性與多期間報酬另外同時比較 1週、1月、3月、6月、1年、3年。"
    )
  })
  
  output$risk_period_info <- renderText({
    x <- research_metrics() %>%
      filter(symbol == "0050") %>%
      slice_head(n = 1)
    
    req(nrow(x) == 1)
    
    paste0(
      "波動度、最大回撤、CVaR、夏普與近 1 年年化報酬統一使用 ",
      format(x$base_start_date, "%Y-%m-%d"),
      " 至 ",
      format(x$base_end_date, "%Y-%m-%d"),
      "；穩定性則比較 1週、1月、3月、6月、1年、3年六個時間尺度。"
    )
  })
  
  output$ranking_period_table <- renderTable({
    research_period_table()
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  output$composite_ranking_chart <- renderPlot({
    x <- research_metrics() %>%
      filter(symbol %in% stock_info$symbol, is.finite(growth_score)) %>%
      arrange(desc(growth_score)) %>%
      slice_head(n = 10) %>%
      mutate(label = paste0(symbol, " ", name))
    req(nrow(x) > 0)
    ggplot(x, aes(x = growth_score, y = reorder(label, growth_score), fill = symbol)) +
      geom_col(width = 0.72) +
      scale_fill_manual(values = stock_colors, guide = "none") +
      geom_vline(
        xintercept = research_metrics() %>% filter(symbol == "0050") %>% pull(growth_score),
        linetype = "dashed"
      ) +
      labs(
        title = "成分股成長分數前 10 名",
        subtitle = "虛線為 0050 成長分數；Risk / Stability 為輔助判讀",
        x = "成長分數（Growth Score，0–100）",
        y = NULL
      ) +
      xlim(0, 100) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$composite_ranking_table <- renderTable({
    x <- research_metrics() %>%
      filter(symbol %in% stock_info$symbol, is.finite(growth_score)) %>%
      arrange(desc(growth_score)) %>%
      mutate(
        排名 = row_number(),
        股票代號 = symbol,
        股票名稱 = name,
        `成長分數（Growth）` = fmt_score(growth_score),
        `報酬分數（Return）` = fmt_score(return_score),
        `夏普分數（Sharpe）` = fmt_score(sharpe_score),
        `風險控制分數（Risk）` = fmt_score(risk_score),
        `穩定性分數（Stability）` = fmt_score(stability_score),
        `相對0050成長` = relative_0050,
        `成長分數差` = ifelse(is.finite(score_diff_0050), sprintf("%+.1f", score_diff_0050), "NA"),
        標籤 = labels
      ) %>%
      select(排名, 股票代號, 股票名稱, `成長分數（Growth）`, `報酬分數（Return）`, `夏普分數（Sharpe）`, `風險控制分數（Risk）`, `穩定性分數（Stability）`, `相對0050成長`, `成長分數差`, 標籤)
    x
  }, striped = FALSE, bordered = TRUE, hover = TRUE)
  
  output$stability_ranking_chart <- renderPlot({
    x <- research_metrics() %>%
      filter(symbol %in% stock_info$symbol, is.finite(stability_score)) %>%
      arrange(desc(stability_score)) %>%
      slice_head(n = 10) %>%
      mutate(label = paste0(symbol, " ", name))
    req(nrow(x) > 0)
    ggplot(x, aes(x = stability_score, y = reorder(label, stability_score), fill = symbol)) +
      geom_col(width = 0.72) +
      scale_fill_manual(values = stock_colors, guide = "none") +
      labs(
        title = "成分股穩定性分數前 10 名",
        x = "穩定性分數（0–100）",
        y = NULL
      ) +
      xlim(0, 100) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$risk_stability_table <- renderTable({
    research_metrics() %>%
      filter(symbol %in% c("0050", stock_info$symbol)) %>%
      arrange(desc(stability_score)) %>%
      transmute(
        股票代號 = symbol,
        股票名稱 = name,
        `風險控制分數（Risk）` = fmt_score(risk_score),
        `穩定性分數（Stability）` = fmt_score(stability_score),
        年化波動度 = fmt_pct(annual_volatility, 2, FALSE),
        `最大回撤（Maximum Drawdown）` = fmt_pct(max_drawdown, 2, TRUE),
        `95%條件風險值（CVaR）` = fmt_pct(cvar_95, 2, TRUE),
        `夏普比率（Sharpe Ratio）` = ifelse(is.finite(sharpe), sprintf("%.3f", sharpe), "NA"),
        標籤 = labels
      )
  }, striped = FALSE, bordered = TRUE, hover = TRUE)
  
  output$pca_period_info <- renderText({
    
    r <- pca_cluster_result()
    x <- r$data
    
    req(nrow(x) > 0)
    
    period_name <- names(ANALYSIS_WINDOWS)[
      match(r$window_days, ANALYSIS_WINDOWS)
    ]
    
    paste0(
      ifelse(length(period_name) == 1 && !is.na(period_name), period_name, paste0(r$window_days, " 個交易日")),
      "｜",
      format(min(x$start_date), "%Y-%m-%d"),
      " 至 ",
      format(max(x$end_date), "%Y-%m-%d"),
      "｜納入 ",
      nrow(x),
      " 檔股票；每檔至少需有約 80% 的有效觀測。"
    )
  })
  
  output$pca_variance_table <- renderTable({
    
    p <- pca_cluster_result()$pca
    variance <- (p$sdev ^ 2) / sum(p$sdev ^ 2)
    
    data.frame(
      主成分 = paste0("PC", seq_along(variance)),
      解釋變異比例 = paste0(round(variance * 100, 2), "%"),
      累積比例 = paste0(round(cumsum(variance) * 100, 2), "%"),
      stringsAsFactors = FALSE
    )
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  output$pca_loading_table <- renderTable({
    
    p <- pca_cluster_result()$pca
    
    loading <- as.data.frame(
      round(
        p$rotation[
          ,
          seq_len(min(3, ncol(p$rotation))),
          drop = FALSE
        ],
        3
      )
    )
    
    feature_label <- c(
      annual_return = "年化報酬率",
      annual_volatility = "年化波動度",
      max_drawdown = "最大回撤",
      cvar_95 = "95% 條件風險值（CVaR）",
      sharpe = "夏普比率",
      return_stability_score = "報酬穩定性",
      rank_stability_score = "排名穩定性",
      risk_stability_score = "風險穩定性"
    )
    
    loading$指標 <- feature_label[rownames(loading)]
    
    loading <- loading %>%
      select(
        指標,
        everything()
      )
    
    loading
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  output$pca_plain_explanation <- renderText({
    
    p <- pca_cluster_result()$pca
    
    feature_label <- c(
      annual_return = "年化報酬",
      annual_volatility = "波動度",
      max_drawdown = "最大回撤",
      cvar_95 = "極端損失",
      sharpe = "夏普效率",
      return_stability_score = "報酬穩定性",
      rank_stability_score = "排名穩定性",
      risk_stability_score = "風險穩定性"
    )
    
    pc_text <- function(pc) {
      
      if (ncol(p$rotation) < pc) return(NULL)
      
      z <- p$rotation[, pc]
      top <- names(sort(abs(z), decreasing = TRUE))[1:min(3, length(z))]
      
      paste0(
        "PC",
        pc,
        " 主要受到 ",
        paste(feature_label[top], collapse = "、"),
        " 影響"
      )
    }
    
    paste(
      c(
        "白話解讀：",
        pc_text(1),
        pc_text(2)
      ),
      collapse = "；"
    )
  })
  
  output$cluster_quality_table <- renderTable({
    
    x <- pca_cluster_result()$quality %>%
      mutate(
        群數 = k,
        群內平方和 = round(total_withinss, 2),
        平均輪廓係數 = round(silhouette, 3),
        建議 = ifelse(
          k == pca_cluster_result()$best_k,
          "目前較佳",
          ""
        )
      ) %>%
      select(
        群數,
        群內平方和,
        平均輪廓係數,
        建議
      )
    
    x
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  output$pca_cluster_chart <- renderPlot({
    
    x <- pca_cluster_result()$data
    
    req(nrow(x) > 0)
    
    ggplot(
      x,
      aes(
        x = PC1,
        y = PC2,
        color = cluster_label,
        label = symbol
      )
    ) +
      geom_point(size = 3) +
      geom_text(vjust = -0.7, size = 3) +
      labs(
        title = paste0(
          "主成分分群圖（",
          pca_cluster_result()$best_k,
          " 群）"
        ),
        subtitle = paste0(
          "分析期間：最近 ",
          pca_cluster_result()$window_days,
          " 個交易日"
        ),
        x = "第一主成分",
        y = "第二主成分",
        color = "分群"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        plot.subtitle = element_text(
          hjust = 0.5
        ),
        legend.position = "bottom"
      )
  })
  
  output$cluster_summary_table <- renderTable({
    
    x <- pca_cluster_result()$group_character %>%
      transmute(
        分群類型 = cluster_label,
        股票數 = 股票數,
        平均年化報酬 = fmt_pct(avg_return, 2, TRUE),
        平均年化波動 = fmt_pct(avg_volatility, 2, FALSE),
        平均最大回撤 = fmt_pct(avg_drawdown, 2, TRUE),
        平均CVaR = fmt_pct(avg_cvar, 2, TRUE),
        平均夏普 = ifelse(
          is.finite(avg_sharpe),
          sprintf("%.3f", avg_sharpe),
          "NA"
        ),
        平均穩定性 = fmt_score(avg_stability)
      )
    
    x
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  observe({
    
    r <- pca_cluster_result()
    
    choices <- c(
      "全部" = "ALL",
      setNames(
        unique(r$data$cluster_label),
        unique(r$data$cluster_label)
      )
    )
    
    current <- isolate(input$cluster_filter)
    
    selected_choice <- if (
      !is.null(current) &&
      length(current) == 1 &&
      current %in% unname(choices)
    ) {
      current
    } else {
      "ALL"
    }
    
    updateSelectInput(
      session,
      "cluster_filter",
      choices = choices,
      selected = selected_choice
    )
  })
  
  output$cluster_membership_table <- renderTable({
    
    x <- pca_cluster_result()$data
    
    if (
      !is.null(input$cluster_filter) &&
      input$cluster_filter != "ALL"
    ) {
      x <- x %>%
        filter(
          cluster_label == input$cluster_filter
        )
    }
    
    x %>%
      arrange(
        cluster,
        desc(annual_return)
      ) %>%
      transmute(
        股票代號 = symbol,
        股票名稱 = name,
        產業 = industry,
        群組 = cluster_label,
        有效觀測數 = n_obs,
        資料起始日 = format(start_date, "%Y-%m-%d"),
        資料結束日 = format(end_date, "%Y-%m-%d"),
        年化報酬 = fmt_pct(annual_return, 2, TRUE),
        年化波動 = fmt_pct(annual_volatility, 2, FALSE),
        最大回撤 = fmt_pct(max_drawdown, 2, TRUE),
        夏普 = ifelse(
          is.finite(sharpe),
          sprintf("%.3f", sharpe),
          "NA"
        )
      )
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ==========================================================
  # 6-1. 不確定性分析：區塊拔靴法
  # ==========================================================
  
  bootstrap_result <- eventReactive(input$run_bootstrap, {
    
    req(input$bootstrap_stock)
    
    
    
    window_days <- as.integer(input$bootstrap_window)
    block_length <- as.integer(input$bootstrap_block)
    reps <- as.integer(input$bootstrap_reps)
    
    dates <- trading_dates()
    req(length(dates) >= 63)
    
    start_index <- max(
      1,
      length(dates) - window_days + 1
    )
    
    selected_dates <- dates[
      start_index:length(dates)
    ]
    
    x <- data() %>%
      filter(
        date >= min(selected_dates),
        date <= max(selected_dates),
        symbol %in% dashboard_symbols,
        is.finite(log_return)
      )
    
    m <- return_wide_matrix(
      x,
      dashboard_symbols
    )
    
    req(
      !is.null(m),
      nrow(m) >= 63,
      "0050" %in% colnames(m),
      input$bootstrap_stock %in% colnames(m)
    )
    
    diffs <- rep(
      NA_real_,
      reps
    )
    
    withProgress(
      message = "正在重新抽樣並檢查結果…",
      value = 0,
      {
        
        for (b in seq_len(reps)) {
          
          idx <- block_bootstrap_indices(
            nrow(m),
            block_length
          )
          
          s <- score_return_matrix(
            m[idx, , drop = FALSE]
          )
          
          if (nrow(s) > 0) {
            
            stock_score <- s$growth_score[
              s$symbol == input$bootstrap_stock
            ]
            
            bench_score <- s$growth_score[
              s$symbol == "0050"
            ]
            
            if (
              length(stock_score) == 1 &&
              length(bench_score) == 1 &&
              is.finite(stock_score) &&
              is.finite(bench_score)
            ) {
              diffs[b] <- stock_score - bench_score
            }
          }
          
          if (
            b %% max(1, floor(reps / 20)) == 0 ||
            b == reps
          ) {
            setProgress(
              value = b / reps,
              detail = paste0(
                "已完成 ",
                b,
                " / ",
                reps,
                " 次"
              )
            )
          }
        }
      }
    )
    
    diffs <- diffs[
      is.finite(diffs)
    ]
    
    req(
      length(diffs) >= max(
        20,
        reps * 0.4
      )
    )
    
    ci <- as.numeric(
      quantile(
        diffs,
        c(0.025, 0.975),
        na.rm = TRUE,
        names = FALSE
      )
    )
    
    prob_above <- mean(
      diffs > 0,
      na.rm = TRUE
    )
    
    label <- if (ci[1] > 0) {
      "多數重新抽樣後仍高於 0050，結果相對穩定"
    } else if (ci[2] < 0) {
      "多數重新抽樣後仍低於 0050"
    } else {
      "結果有時高、有時低，暫時無法說優勢穩定"
    }
    
    list(
      diffs = diffs,
      ci = ci,
      prob_above = prob_above,
      label = label,
      reps = length(diffs),
      stock = input$bootstrap_stock,
      window_days = window_days,
      block = block_length,
      start_date = min(selected_dates),
      end_date = max(selected_dates),
      n_matrix_days = nrow(m)
    )
  },
  ignoreInit = TRUE
  )
  
  
  output$bootstrap_period_info <- renderText({
    
    window_days <- as.integer(input$bootstrap_window)
    
    dates <- trading_dates()
    req(length(dates) > 0)
    
    start_index <- max(
      1,
      length(dates) - window_days + 1
    )
    
    paste0(
      format(dates[start_index], "%Y-%m-%d"),
      " 至 ",
      format(tail(dates, 1), "%Y-%m-%d"),
      "｜目標約 ",
      window_days,
      " 個交易日。這個期間只用來做本次拔靴法，不會自動延伸到更久的歷史。"
    )
  })
  
  
  output$bootstrap_summary_table <- renderTable({
    
    r <- bootstrap_result()
    
    data.frame(
      比較標的 = get_stock_label(r$stock),
      資料期間 = paste0(
        format(r$start_date, "%Y-%m-%d"),
        " ～ ",
        format(r$end_date, "%Y-%m-%d")
      ),
      有效交易日 = r$n_matrix_days,
      有效重複次數 = r$reps,
      區塊長度 = paste0(
        r$block,
        " 個交易日"
      ),
      `平均成長分數差` = sprintf(
        "%+.2f",
        mean(r$diffs)
      ),
      `95%區間下限` = sprintf(
        "%+.2f",
        r$ci[1]
      ),
      `95%區間上限` = sprintf(
        "%+.2f",
        r$ci[2]
      ),
      `高於0050比例` = paste0(
        round(
          r$prob_above * 100,
          1
        ),
        "%"
      ),
      結論 = r$label,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$bootstrap_diff_chart <- renderPlot({
    
    r <- bootstrap_result()
    
    x <- data.frame(
      diff = r$diffs
    )
    
    ggplot(
      x,
      aes(
        x = diff
      )
    ) +
      geom_histogram(
        aes(
          y = after_stat(density)
        ),
        bins = 36,
        fill = "#a28bd2",
        color = "white",
        alpha = 0.85
      ) +
      geom_density(
        color = "#5b8ecb",
        linewidth = 1.1
      ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        color = "#d95b5b",
        linewidth = 1
      ) +
      geom_vline(
        xintercept = r$ci,
        linetype = "dotted",
        color = "#dca75a",
        linewidth = 1
      ) +
      labs(
        title = paste0(
          get_stock_label(r$stock),
          " 相對 0050 的成長分數區塊拔靴分布"
        ),
        subtitle = paste0(
          format(r$start_date, "%Y-%m-%d"),
          " ～ ",
          format(r$end_date, "%Y-%m-%d"),
          "｜95% 區間：",
          sprintf("%+.2f", r$ci[1]),
          " ～ ",
          sprintf("%+.2f", r$ci[2])
        ),
        x = "成長分數差（成分股－0050）",
        y = "密度"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        plot.subtitle = element_text(
          hjust = 0.5
        )
      )
  })
  
  
  # ==========================================================
  # 6-2. 歷史回測：排名模型驗證
  # ==========================================================
  
  backtest_result <- eventReactive(input$run_backtest, {
    
    tryCatch({
      
      lookback_days <- as.integer(input$backtest_lookback_days)
      forward_days <- as.integer(input$backtest_forward_days)
      frequency_days <- as.integer(input$backtest_frequency)
      start_year <- as.integer(input$backtest_start_year)
      model_key <- as.character(input$backtest_model)
      
      model_score_column <- switch(
        model_key,
        growth = "growth_score",
        return = "return_score",
        momentum_6m = "momentum_6m_score",
        legacy_composite = "composite_score",
        "growth_score"
      )
      
      model_label <- switch(
        model_key,
        growth = "新版成長分數（70% 報酬 + 30% 夏普）",
        return = "報酬分數（Return Score）",
        momentum_6m = "6 個月動能（Momentum）",
        legacy_composite = "舊綜合分數（40/25/20/15）",
        "新版成長分數"
      )
      
      dates <- trading_dates()
      
      if (
        length(dates) <
        lookback_days + forward_days + 1
      ) {
        return(
          list(
            error = paste0(
              "目前資料長度不足。需要至少約 ",
              lookback_days + forward_days,
              " 個交易日，才能同時保留評分資料與未來驗證資料。"
            )
          )
        )
      }
      
      min_index <- lookback_days
      max_index <- length(dates) - forward_days
      
      eval_index <- seq(
        min_index,
        max_index,
        by = max(1, frequency_days)
      )
      
      eval_index <- eval_index[
        as.integer(
          format(
            dates[eval_index],
            "%Y"
          )
        ) >= start_year
      ]
      
      if (length(eval_index) < 1) {
        return(
          list(
            error = "目前設定下沒有可用的評分日期，請把起始年度往前或縮短評分／驗證期間。"
          )
        )
      }
      
      result_rows <- list()
      spearman_rows <- list()
      row_counter <- 0
      spearman_counter <- 0
      
      withProgress(
        message = "正在做滾動樣本外模型驗證…",
        value = 0,
        {
          
          for (
            ii in seq_along(eval_index)
          ) {
            
            idx <- eval_index[ii]
            score_date <- dates[idx]
            
            hist_dates <- dates[
              (idx - lookback_days + 1):idx
            ]
            
            future_dates <- dates[
              (idx + 1):(idx + forward_days)
            ]
            
            hist <- data() %>%
              filter(
                date >= min(hist_dates),
                date <= max(hist_dates),
                symbol %in% dashboard_symbols,
                is.finite(log_return)
              )
            
            m <- return_wide_matrix(
              hist,
              dashboard_symbols
            )
            
            if (
              is.null(m) ||
              nrow(m) < min(63, floor(lookback_days * 0.75))
            ) {
              next
            }
            
            scores <- score_return_matrix(
              m
            ) %>%
              mutate(
                model_score = .data[[model_score_column]]
              ) %>%
              filter(
                symbol %in% stock_info$symbol,
                is.finite(model_score)
              ) %>%
              arrange(
                desc(model_score)
              )
            
            if (nrow(scores) < 20) {
              next
            }
            
            top5 <- head(
              scores$symbol,
              5
            )
            
            top10 <- head(
              scores$symbol,
              min(10, nrow(scores))
            )
            
            bottom5 <- tail(
              scores$symbol,
              min(5, nrow(scores))
            )
            
            future_all <- data() %>%
              filter(
                date >= min(future_dates),
                date <= max(future_dates),
                symbol %in% c(
                  "0050",
                  stock_info$symbol
                ),
                is.finite(log_return)
              )
            
            if (nrow(future_all) == 0) {
              next
            }
            
            # --------------------------------------------------
            # 分數高低 vs 未來個股表現：Spearman 等級相關
            # --------------------------------------------------
            future_stock <- future_all %>%
              filter(
                symbol %in% stock_info$symbol
              ) %>%
              group_by(symbol) %>%
              summarise(
                n = n(),
                future_return = exp(
                  sum(
                    log_return,
                    na.rm = TRUE
                  )
                ) - 1,
                .groups = "drop"
              ) %>%
              mutate(
                future_return = ifelse(
                  n >= max(
                    2,
                    floor(
                      forward_days * 0.75
                    )
                  ),
                  future_return,
                  NA_real_
                )
              )
            
            rank_check <- scores %>%
              select(
                symbol,
                model_score
              ) %>%
              inner_join(
                future_stock,
                by = "symbol"
              ) %>%
              filter(
                is.finite(model_score),
                is.finite(future_return)
              )
            
            if (nrow(rank_check) >= 10) {
              
              rho <- suppressWarnings(
                cor(
                  rank_check$model_score,
                  rank_check$future_return,
                  method = "spearman",
                  use = "complete.obs"
                )
              )
              
              if (is.finite(rho)) {
                
                spearman_counter <- spearman_counter + 1
                
                spearman_rows[[spearman_counter]] <- data.frame(
                  評分日期 = score_date,
                  驗證截止日 = max(future_dates),
                  有效股票數 = nrow(rank_check),
                  Spearman = rho,
                  stringsAsFactors = FALSE
                )
              }
            }
            
            # --------------------------------------------------
            # Top / Bottom / 0050 後續表現
            # --------------------------------------------------
            group_list <- list(
              Top5 = top5,
              Top10 = top10,
              Bottom5 = bottom5,
              `0050` = "0050"
            )
            
            for (
              strategy_name in names(group_list)
            ) {
              
              g <- validation_group_metrics(
                future_all,
                group_list[[strategy_name]]
              )
              
              row_counter <- row_counter + 1
              
              result_rows[[row_counter]] <- data.frame(
                評分日期 = score_date,
                驗證截止日 = max(future_dates),
                策略 = strategy_name,
                後續報酬 = as.numeric(g[["future_return"]]),
                年化波動 = as.numeric(g[["annual_volatility"]]),
                最大回撤 = as.numeric(g[["max_drawdown"]]),
                夏普 = as.numeric(g[["sharpe"]]),
                有效交易日 = as.numeric(g[["n_days"]]),
                stringsAsFactors = FALSE,
                check.names = FALSE
              )
            }
            
            setProgress(
              value = ii / length(eval_index),
              detail = paste0(
                "評分日期：",
                format(score_date, "%Y-%m-%d")
              )
            )
          }
        }
      )
      
      detail <- bind_rows(
        result_rows
      )
      
      if (nrow(detail) == 0) {
        return(
          list(
            error = "目前條件下找不到足夠的完整資料完成模型驗證。可以縮短評分期間、縮短未來驗證期間，或把起始年度往後調整。"
          )
        )
      }
      
      benchmark <- detail %>%
        filter(
          策略 == "0050"
        ) %>%
        select(
          評分日期,
          benchmark_return = 後續報酬
        )
      
      detail <- detail %>%
        left_join(
          benchmark,
          by = "評分日期"
        ) %>%
        mutate(
          勝過0050 = ifelse(
            策略 == "0050" |
              !is.finite(後續報酬) |
              !is.finite(benchmark_return),
            NA,
            後續報酬 > benchmark_return
          )
        )
      
      summary <- detail %>%
        group_by(策略) %>%
        summarise(
          有效驗證次數 = sum(
            is.finite(後續報酬)
          ),
          平均後續報酬 = mean(
            後續報酬,
            na.rm = TRUE
          ),
          中位數後續報酬 = median(
            後續報酬,
            na.rm = TRUE
          ),
          平均年化波動 = ifelse(
            sum(is.finite(年化波動)) > 0,
            mean(
              年化波動,
              na.rm = TRUE
            ),
            NA_real_
          ),
          平均最大回撤 = mean(
            最大回撤,
            na.rm = TRUE
          ),
          平均夏普 = ifelse(
            sum(is.finite(夏普)) > 0,
            mean(
              夏普,
              na.rm = TRUE
            ),
            NA_real_
          ),
          勝過0050比例 = ifelse(
            first(策略) == "0050",
            NA_real_,
            mean(
              勝過0050,
              na.rm = TRUE
            )
          ),
          .groups = "drop"
        )
      
      spearman_detail <- bind_rows(
        spearman_rows
      )
      
      if (nrow(spearman_detail) > 0) {
        
        spearman_summary <- data.frame(
          有效驗證次數 = nrow(spearman_detail),
          平均Spearman = mean(
            spearman_detail$Spearman,
            na.rm = TRUE
          ),
          中位數Spearman = median(
            spearman_detail$Spearman,
            na.rm = TRUE
          ),
          正相關比例 = mean(
            spearman_detail$Spearman > 0,
            na.rm = TRUE
          ),
          stringsAsFactors = FALSE
        )
        
      } else {
        
        spearman_summary <- data.frame(
          有效驗證次數 = 0,
          平均Spearman = NA_real_,
          中位數Spearman = NA_real_,
          正相關比例 = NA_real_,
          stringsAsFactors = FALSE
        )
      }
      
      wide <- detail %>%
        select(
          評分日期,
          驗證截止日,
          策略,
          後續報酬
        ) %>%
        pivot_wider(
          names_from = 策略,
          values_from = 後續報酬
        ) %>%
        arrange(
          評分日期
        )
      
      separation_detail <- wide %>%
        mutate(
          Spread = ifelse(
            is.finite(Top5) & is.finite(Bottom5),
            Top5 - Bottom5,
            NA_real_
          ),
          Top5勝 = ifelse(
            is.finite(Top5) & is.finite(Bottom5),
            Top5 > Bottom5,
            NA
          )
        )
      
      separation_summary <- data.frame(
        有效驗證次數 = sum(is.finite(separation_detail$Spread)),
        平均報酬差距 = mean(separation_detail$Spread, na.rm = TRUE),
        Top5勝率 = mean(separation_detail$Top5勝, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      
      list(
        error = NULL,
        detail = detail,
        summary = summary,
        spearman_detail = spearman_detail,
        spearman_summary = spearman_summary,
        wide = wide,
        separation_detail = separation_detail,
        separation_summary = separation_summary,
        lookback_days = lookback_days,
        forward_days = forward_days,
        frequency_days = frequency_days,
        model_key = model_key,
        model_label = model_label,
        first_score_date = min(detail$評分日期),
        last_score_date = max(detail$評分日期)
      )
      
    },
    error = function(e) {
      
      list(
        error = paste0(
          "模型驗證暫時無法完成：",
          conditionMessage(e)
        )
      )
    })
  },
  ignoreInit = TRUE
  )
  
  
  output$backtest_status <- renderUI({
    
    r <- backtest_result()
    
    if (!is.null(r$error)) {
      
      div(
        class = "warning-note",
        strong("目前無法完成驗證： "),
        r$error
      )
      
    } else {
      
      div(
        class = "reading-guide",
        strong("驗證完成： "),
        paste0(
          "目前驗證模型：",
          r$model_label,
          "；評分資料往回看 ",
          r$lookback_days,
          " 個交易日；評分後觀察 ",
          r$forward_days,
          " 個交易日。評分日期範圍：",
          format(r$first_score_date, "%Y-%m-%d"),
          " 至 ",
          format(r$last_score_date, "%Y-%m-%d"),
          "。所有未來資料都只在算完當時分數後才拿來驗證。"
        )
      )
    }
  })
  
  
  output$backtest_cumulative_chart <- renderPlot({
    
    r <- backtest_result()
    
    req(
      is.null(r$error),
      nrow(r$detail) > 0
    )
    
    x <- r$detail %>%
      filter(
        is.finite(後續報酬)
      )
    
    ggplot(
      x,
      aes(
        x = 評分日期,
        y = 後續報酬,
        color = 策略,
        group = 策略
      )
    ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "#94a3b8"
      ) +
      geom_line(
        linewidth = 0.9,
        alpha = 0.85
      ) +
      geom_point(
        size = 1.8
      ) +
      scale_color_manual(
        values = c(
          "Top5" = "#6f9fd0",
          "Top10" = "#a28bd2",
          "Bottom5" = "#dfb46f",
          "0050" = "#5b8ecb"
        )
      ) +
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      labs(
        title = "每次評分後的實際後續報酬",
        subtitle = "這張圖不是交易策略累積線，而是逐次檢查高分組與低分組後來表現如何",
        x = "評分日期",
        y = "後續報酬",
        color = "比較組別"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        plot.subtitle = element_text(
          hjust = 0.5
        ),
        legend.position = "bottom"
      )
  })
  
  
  output$backtest_summary_table <- renderTable({
    
    r <- backtest_result()
    
    req(
      is.null(r$error),
      nrow(r$summary) > 0
    )
    
    r$summary %>%
      mutate(
        平均後續報酬 = fmt_pct(
          平均後續報酬,
          2,
          TRUE
        ),
        中位數後續報酬 = fmt_pct(
          中位數後續報酬,
          2,
          TRUE
        ),
        平均年化波動 = fmt_pct(
          平均年化波動,
          2,
          FALSE
        ),
        平均最大回撤 = fmt_pct(
          平均最大回撤,
          2,
          TRUE
        ),
        平均夏普 = ifelse(
          is.finite(平均夏普),
          sprintf(
            "%.3f",
            平均夏普
          ),
          "NA"
        ),
        勝過0050比例 = ifelse(
          is.finite(勝過0050比例),
          paste0(
            round(
              勝過0050比例 * 100,
              1
            ),
            "%"
          ),
          "—"
        )
      )
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$backtest_separation_table <- renderTable({
    
    r <- backtest_result()
    
    req(
      is.null(r$error),
      nrow(r$separation_summary) > 0
    )
    
    r$separation_summary %>%
      mutate(
        平均報酬差距 = fmt_pct(平均報酬差距, 2, TRUE),
        Top5勝率 = ifelse(
          is.finite(Top5勝率),
          paste0(round(Top5勝率 * 100, 1), "%"),
          "NA"
        )
      ) %>%
      rename(
        `平均 Spread（Top5－Bottom5）` = 平均報酬差距
      )
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$backtest_spearman_table <- renderTable({
    
    r <- backtest_result()
    
    req(
      is.null(r$error)
    )
    
    x <- r$spearman_summary
    
    data.frame(
      有效驗證次數 = x$有效驗證次數,
      平均Spearman = ifelse(
        is.finite(x$平均Spearman),
        sprintf(
          "%.3f",
          x$平均Spearman
        ),
        "NA"
      ),
      中位數Spearman = ifelse(
        is.finite(x$中位數Spearman),
        sprintf(
          "%.3f",
          x$中位數Spearman
        ),
        "NA"
      ),
      正相關比例 = ifelse(
        is.finite(x$正相關比例),
        paste0(
          round(
            x$正相關比例 * 100,
            1
          ),
          "%"
        ),
        "NA"
      ),
      解讀 = ifelse(
        is.finite(x$平均Spearman) &&
          x$平均Spearman > 0,
        "平均為正：高分股票之後傾向有較高的相對表現",
        "平均未呈正向：目前沒有看到穩定的正向排序能力"
      ),
      stringsAsFactors = FALSE
    )
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$backtest_yearly_table <- renderTable({
    
    r <- backtest_result()
    
    req(
      is.null(r$error),
      nrow(r$wide) > 0
    )
    
    x <- r$wide %>%
      arrange(
        desc(評分日期)
      ) %>%
      slice_head(
        n = 40
      ) %>%
      mutate(
        評分日期 = format(
          評分日期,
          "%Y-%m-%d"
        ),
        驗證截止日 = format(
          驗證截止日,
          "%Y-%m-%d"
        )
      )
    
    for (
      nm in intersect(
        c(
          "Top5",
          "Top10",
          "Bottom5",
          "0050"
        ),
        names(x)
      )
    ) {
      x[[nm]] <- fmt_pct(
        x[[nm]],
        2,
        TRUE
      )
    }
    
    x
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ==========================================================
  # 7. 產業比較
  # ==========================================================
  
  output$industry_period_info <- renderText({
    r <- analysis_range()
    
    paste0(
      format(r[1], "%Y-%m-%d"),
      " 至 ",
      format(r[2], "%Y-%m-%d"),
      "。產業比較沿用個股分析目前選定的日期範圍，避免同一份網站使用兩套不同期間。"
    )
  })
  
  selected_industry_stocks <- reactive({
    
    stock_info %>%
      filter(
        industry_group ==
          input$selected_industry
      ) %>%
      pull(symbol)
    
  })
  
  
  selected_industry_data <- reactive({
    
    data() %>%
      filter(
        symbol %in%
          selected_industry_stocks(),
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      )
    
  })
  
  
  industry_daily <- reactive({
    
    selected_industry_data() %>%
      group_by(date) %>%
      summarise(
        log_return =
          mean(
            log_return,
            na.rm = TRUE
          ),
        .groups = "drop"
      ) %>%
      arrange(date)
    
  })
  
  
  output$industry_stock_table <- renderTable({
    
    stock_info %>%
      filter(
        industry_group ==
          input$selected_industry
      ) %>%
      transmute(
        股票代號 = symbol,
        股票名稱 = name,
        產業 = industry_group
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$industry_summary <- renderTable({
    
    x <- industry_daily()
    
    req(
      nrow(x) > 1
    )
    
    data.frame(
      
      產業 =
        input$selected_industry,
      
      成分股數 =
        length(
          selected_industry_stocks()
        ),
      
      平均日對數報酬 =
        fmt_pct(
          mean(
            x$log_return,
            na.rm = TRUE
          ),
          3
        ),
      
      日對數報酬標準差 =
        fmt_pct(
          sd(
            x$log_return,
            na.rm = TRUE
          ),
          3,
          FALSE
        ),
      
      累積報酬率 =
        fmt_pct(
          exp(
            sum(
              x$log_return,
              na.rm = TRUE
            )
          ) - 1
        ),
      
      有效交易日 =
        nrow(x),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 單一產業累積報酬
  # ----------------------------------------------------------
  
  output$industry_cumulative_chart <- renderPlot({
    
    industry_x <- industry_daily() %>%
      mutate(
        cumulative =
          exp(
            cumsum(
              log_return
            )
          ) - 1
      )
    
    benchmark <- data() %>%
      filter(
        symbol == "0050",
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      ) %>%
      arrange(date) %>%
      mutate(
        cumulative =
          exp(
            cumsum(
              log_return
            )
          ) - 1,
        series = "0050"
      )
    
    req(
      nrow(industry_x) > 1,
      nrow(benchmark) > 1
    )
    
    ggplot() +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_line(
        data = industry_x,
        aes(
          x = date,
          y = cumulative
        ),
        color =
          industry_colors[
            input$selected_industry
          ],
        linewidth = 1
      ) +
      
      geom_line(
        data = benchmark,
        aes(
          x = date,
          y = cumulative
        ),
        color = "red",
        linewidth = 1,
        linetype = "dashed"
      ) +
      
      labs(
        title =
          paste0(
            input$selected_industry,
            " 與 0050 累積報酬比較"
          ),
        x = "日期",
        y = "累積報酬率"
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 單一產業風險報酬
  # ----------------------------------------------------------
  
  output$industry_risk_return_chart <- renderPlot({
    
    x <- selected_industry_data() %>%
      group_by(symbol) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      summarise(
        
        annual_return =
          exp(
            mean(
              log_return,
              na.rm = TRUE
            ) *
              TRADING_DAYS_PER_YEAR
          ) - 1,
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = annual_volatility,
        y = annual_return,
        label = symbol
      )
    ) +
      
      geom_point(
        size = 3,
        color =
          industry_colors[
            input$selected_industry
          ]
      ) +
      
      geom_text(
        vjust = -0.8,
        color =
          industry_colors[
            input$selected_industry
          ]
      ) +
      
      labs(
        title =
          paste0(
            input$selected_industry,
            " 成分股風險與報酬"
          ),
        x = "年化波動度",
        y = "年化報酬率"
      ) +
      
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          )
      )
    
  })
  
  
  # ==========================================================
  # 所有產業綜合比較
  # ==========================================================
  
  all_industry_daily <- reactive({
    
    data() %>%
      filter(
        symbol %in%
          stock_info$symbol,
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      ) %>%
      group_by(
        industry_group,
        date
      ) %>%
      summarise(
        log_return =
          mean(
            log_return,
            na.rm = TRUE
          ),
        .groups = "drop"
      ) %>%
      arrange(
        industry_group,
        date
      )
    
  })
  
  
  output$industry_all_cumulative_chart <- renderPlot({
    
    x <- all_industry_daily() %>%
      group_by(
        industry_group
      ) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      mutate(
        cumulative =
          exp(
            cumsum(
              log_return
            )
          ) - 1
      ) %>%
      ungroup()
    
    ggplot(
      x,
      aes(
        x = date,
        y = cumulative,
        color = industry_group
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_line(
        linewidth = 0.9
      ) +
      
      scale_color_manual(
        values =
          industry_colors
      ) +
      
      labs(
        title =
          "所有產業累積報酬比較",
        x = "日期",
        y = "累積報酬率",
        color = "產業"
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        legend.position =
          "bottom"
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 所有產業報酬分布
  # 3 × N
  # ----------------------------------------------------------
  
  output$industry_all_distribution_ui <- renderUI({
    
    industries <-
      sort(
        unique(
          all_industry_daily()$industry_group
        )
      )
    
    lapply(
      
      seq(
        1,
        length(industries),
        by = 3
      ),
      
      function(i) {
        
        ids <-
          industries[
            i:min(
              i + 2,
              length(industries)
            )
          ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(industry) {
              
              local({
                
                ind <- industry
                
                plot_id <-
                  paste0(
                    "industry_distribution_",
                    make.names(ind)
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- all_industry_daily() %>%
                    filter(
                      industry_group == ind,
                      is.finite(log_return)
                    )
                  
                  req(
                    nrow(x) > 10
                  )
                  
                  col <-
                    industry_colors[ind]
                  
                  light <-
                    adjustcolor(
                      col,
                      alpha.f = 0.50
                    )
                  
                  edge <-
                    adjustcolor(
                      col,
                      alpha.f = 0.35
                    )
                  
                  mu <-
                    mean(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  sig <-
                    sd(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  p <- ggplot(
                    x,
                    aes(
                      x = log_return
                    )
                  ) +
                    
                    geom_histogram(
                      aes(
                        y =
                          after_stat(
                            density
                          )
                      ),
                      bins = 40,
                      fill = light,
                      color = edge
                    ) +
                    
                    labs(
                      title = ind,
                      x = "平均日對數報酬率",
                      y = "密度"
                    ) +
                    
                    scale_x_continuous(
                      labels = function(z) {
                        paste0(
                          round(
                            z * 100,
                            1
                          ),
                          "%"
                        )
                      }
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        )
                    )
                  
                  if (
                    is.finite(sig) &&
                    sig > 0
                  ) {
                    
                    p <- p +
                      stat_function(
                        fun = dnorm,
                        args = list(
                          mean = mu,
                          sd = sig
                        ),
                        color = col,
                        linewidth = 1.1
                      )
                    
                  }
                  
                  p
                  
                })
                
                column(
                  width = 4,
                  plotOutput(
                    plot_id,
                    height = "340px"
                  )
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 所有產業風險報酬
  # ----------------------------------------------------------
  
  output$industry_all_risk_return_chart <- renderPlot({
    
    x <- all_industry_daily() %>%
      group_by(
        industry_group
      ) %>%
      summarise(
        
        annual_return =
          exp(
            mean(
              log_return,
              na.rm = TRUE
            ) *
              TRADING_DAYS_PER_YEAR
          ) - 1,
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      
      x,
      
      aes(
        x = annual_volatility,
        y = annual_return,
        color = industry_group,
        label = industry_group
      )
      
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_point(
        size = 4
      ) +
      
      geom_text(
        vjust = -0.8
      ) +
      
      scale_color_manual(
        values =
          industry_colors
      ) +
      
      labs(
        title =
          "所有產業報酬與風險比較",
        x = "年化波動度",
        y = "年化報酬率",
        color = "產業"
      ) +
      
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        legend.position =
          "bottom"
      )
    
  })
  
  
  # ==========================================================
  # 7. 相關性分析
  # ==========================================================
  
  correlation_data <- reactive({
    
    req(
      input$cor_stock
    )
    
    data() %>%
      filter(
        symbol %in% input$cor_stock,
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      ) %>%
      select(
        date,
        symbol,
        log_return
      ) %>%
      pivot_wider(
        names_from = symbol,
        values_from = log_return
      )
    
  })
  
  
  correlation_matrix <- reactive({
    
    x <- correlation_data()
    
    req(
      nrow(x) >= 2,
      ncol(x) >= 3
    )
    
    r <- cor(
      x %>%
        select(-date),
      use = "pairwise.complete.obs"
    )
    
    r
    
  })
  
  
  output$correlation_table <- renderTable({
    
    r <- correlation_matrix()
    
    result <- as.data.frame(
      round(
        r,
        3
      )
    )
    
    result$股票 <- rownames(
      result
    )
    
    result <- result %>%
      select(
        股票,
        everything()
      )
    
    names(result)[-1] <-
      vapply(
        names(result)[-1],
        function(s) {
          get_stock_label(s)
        },
        character(1)
      )
    
    result
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$correlation_heatmap <- renderPlot({
    
    r <- correlation_matrix()
    
    long <- as.data.frame(
      as.table(r)
    )
    
    names(long) <- c(
      "Stock1",
      "Stock2",
      "Correlation"
    )
    
    ggplot(
      long,
      aes(
        x = Stock1,
        y = Stock2,
        fill = Correlation
      )
    ) +
      geom_tile(
        color = "white"
      ) +
      geom_text(
        aes(
          label =
            sprintf(
              "%.2f",
              Correlation
            )
        ),
        size = 5
      ) +
      scale_fill_gradient2(
        limits = c(-1, 1),
        midpoint = 0
      ) +
      scale_x_discrete(
        labels = function(z) {
          vapply(
            z,
            get_stock_name,
            character(1)
          )
        }
      ) +
      scale_y_discrete(
        labels = function(z) {
          vapply(
            z,
            get_stock_name,
            character(1)
          )
        }
      ) +
      labs(
        title = "日對數報酬相關係數熱圖",
        x = NULL,
        y = NULL,
        fill = "相關係數"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ==========================================================
  # 8. 投資組合
  # ==========================================================
  
  # ----------------------------------------------------------
  # 動態交易設定
  # ----------------------------------------------------------
  
  output$portfolio_trade_inputs <- renderUI({
    
    req(
      input$portfolio_stock
    )
    
    tagList(
      
      lapply(
        
        input$portfolio_stock,
        
        function(symbol) {
          
          x <- data() %>%
            filter(
              symbol == !!symbol,
              is.finite(adjusted)
            ) %>%
            arrange(date)
          
          if (nrow(x) == 0) {
            return(
              div(
                class = "warning-note",
                paste0(
                  get_stock_label(symbol),
                  " 沒有可用資料。"
                )
              )
            )
          }
          
          dates <- unique(
            x$date
          )
          buy_default <- nearest_date(
            dates,
            max(
              min(dates),
              max(dates) - 365
            )
          )
          
          sell_default <- nearest_date(
            dates,
            max(dates)
          )
          
          if (
            sell_default < buy_default
          ) {
            sell_default <- max(dates)
          }
          
          div(
            
            class = "portfolio-trade-box",
            
            h4(
              get_stock_label(symbol)
            ),
            
            fluidRow(
              
              column(
                
                4,
                
                dateInput(
                  paste0(
                    "portfolio_buy_",
                    symbol
                  ),
                  "買入日期：",
                  value = buy_default,
                  min = min(dates),
                  max = max(dates),
                  format = "yyyy-mm-dd",
                  language = "zh-TW"
                )
                
              ),
              
              column(
                
                4,
                
                dateInput(
                  paste0(
                    "portfolio_sell_",
                    symbol
                  ),
                  "賣出日期：",
                  value = sell_default,
                  min = min(dates),
                  max = max(dates),
                  format = "yyyy-mm-dd",
                  language = "zh-TW"
                )
                
              ),
              
              column(
                
                4,
                
                if (
                  input$portfolio_trade_mode ==
                  "以投入金額設定"
                ) {
                  
                  numericInput(
                    paste0(
                      "portfolio_amount_",
                      symbol
                    ),
                    "配置資金（元）：",
                    value = 100000,
                    min = 0,
                    step = 10000
                  )
                  
                } else {
                  
                  numericInput(
                    paste0(
                      "portfolio_shares_",
                      symbol
                    ),
                    "持有股數：",
                    value = 1000,
                    min = 0,
                    step = TRADE_UNIT
                  )
                }
                
              )
            ),
            
            p(
              class = "control-description",
              "若輸入日期不是交易日，計算時會自動使用該股票最近的有效交易日。"
            )
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 手續費函數
  # ----------------------------------------------------------
  
  brokerage_fee <- function(
    value
  ) {
    
    rate <- max(
      0,
      input$brokerage_rate
    ) / 100
    
    minimum <- max(
      0,
      input$minimum_fee
    )
    
    if (
      !is.finite(value) ||
      value <= 0
    ) {
      return(0)
    }
    
    max(
      value * rate,
      minimum
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 證交稅
  # ----------------------------------------------------------
  
  sell_tax <- function(
    symbol,
    value
  ) {
    
    if (
      !is.finite(value) ||
      value <= 0
    ) {
      return(0)
    }
    
    rate <- if (
      symbol == "0050"
    ) {
      input$etf_tax_rate
    } else {
      input$stock_tax_rate
    }
    
    value * max(
      0,
      rate
    ) / 100
    
  }
  
  
  # ----------------------------------------------------------
  # 股票最近有效價格
  # ----------------------------------------------------------
  
  price_on_or_before <- function(
    symbol,
    d
  ) {
    
    x <- data() %>%
      filter(
        symbol == !!symbol,
        date <= d,
        is.finite(adjusted)
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(
        n = 1
      )
    
    if (
      nrow(x) == 0
    ) {
      return(NA_real_)
    }
    
    x$adjusted[1]
  }
  
  
  # ----------------------------------------------------------
  # 投資組合交易資料
  # ----------------------------------------------------------
  
  portfolio_trades <- reactive({
    
    req(
      input$portfolio_stock
    )
    
    result <- lapply(
      
      input$portfolio_stock,
      
      function(symbol) {
        
        x <- data() %>%
          filter(
            symbol == !!symbol,
            is.finite(adjusted)
          ) %>%
          arrange(date)
        
        if (
          nrow(x) == 0
        ) {
          return(NULL)
        }
        
        dates <- unique(
          x$date
        )
        
        buy_input <- input[[
          paste0(
            "portfolio_buy_",
            symbol
          )
        ]]
        
        sell_input <- input[[
          paste0(
            "portfolio_sell_",
            symbol
          )
        ]]
        
        if (
          is.null(buy_input) ||
          is.null(sell_input)
        ) {
          return(NULL)
        }
        
        buy_date <- nearest_date(
          dates,
          buy_input
        )
        
        sell_date <- nearest_date(
          dates,
          sell_input
        )
        
        if (
          is.na(buy_date) ||
          is.na(sell_date) ||
          sell_date < buy_date
        ) {
          
          return(
            data.frame(
              股票代號 = symbol,
              股票名稱 = get_stock_name(symbol),
              狀態 = "日期設定無效",
              stringsAsFactors = FALSE
            )
          )
        }
        
        buy_price <- price_on_or_before(
          symbol,
          buy_date
        )
        
        sell_price <- price_on_or_before(
          symbol,
          sell_date
        )
        
        if (
          !is.finite(buy_price) ||
          !is.finite(sell_price)
        ) {
          return(NULL)
        }
        
        if (
          input$portfolio_trade_mode ==
          "以投入金額設定"
        ) {
          
          amount_input <- input[[
            paste0(
              "portfolio_amount_",
              symbol
            )
          ]]
          
          amount_input <- gsub(
            ",",
            "",
            amount_input
          )
          
          amount <- suppressWarnings(
            as.numeric(amount_input)
          )
          
          if (
            !is.finite(amount)
          ) {
            amount <- 0
          }
          
          amount <- max(
            0,
            amount
          )
          
          shares <- floor(
            amount /
              (
                buy_price *
                  TRADE_UNIT
              )
          ) *
            TRADE_UNIT
          
          while (
            shares > 0 &&
            (
              shares *
              buy_price +
              brokerage_fee(
                shares *
                buy_price
              )
            ) > amount
          ) {
            
            shares <-
              shares -
              TRADE_UNIT
            
          }
          
          allocation <- amount
          
        } else {
          
          shares <- max(
            0,
            floor(
              input[[
                paste0(
                  "portfolio_shares_",
                  symbol
                )
              ]] /
                TRADE_UNIT
            ) *
              TRADE_UNIT
          )
          
          allocation <-
            shares *
            buy_price +
            brokerage_fee(
              shares *
                buy_price
            )
        }
        
        buy_value <-
          shares *
          buy_price
        
        buy_fee <-
          brokerage_fee(
            buy_value
          )
        
        sell_value <-
          shares *
          sell_price
        
        sell_fee <-
          brokerage_fee(
            sell_value
          )
        
        tax <-
          sell_tax(
            symbol,
            sell_value
          )
        
        net_sell <-
          sell_value -
          sell_fee -
          tax
        
        data.frame(
          
          股票代號 = symbol,
          
          股票名稱 =
            get_stock_name(symbol),
          
          買入日期 =
            buy_date,
          
          買入價格 =
            buy_price,
          
          賣出日期 =
            sell_date,
          
          賣出價格 =
            sell_price,
          
          股數 =
            shares,
          
          配置資金 =
            allocation,
          
          買入成交金額 =
            buy_value,
          
          買入手續費 =
            buy_fee,
          
          實際投入資金 =
            buy_value +
            buy_fee,
          
          賣出成交金額 =
            sell_value,
          
          賣出手續費 =
            sell_fee,
          
          證交稅 =
            tax,
          
          賣出後所得 =
            net_sell,
          
          狀態 = ifelse(
            shares > 0,
            "有效",
            "股數為 0"
          ),
          
          stringsAsFactors =
            FALSE
          
        )
      }
    )
    
    trades <- bind_rows(
      result
    )
    
    # --------------------------------------------------------
    # 確保交易資料具有固定欄位
    # --------------------------------------------------------
    
    required_cols <- c(
      "股票代號",
      "股票名稱",
      "買入日期",
      "買入價格",
      "賣出日期",
      "賣出價格",
      "股數",
      "配置資金",
      "買入成交金額",
      "買入手續費",
      "實際投入資金",
      "賣出成交金額",
      "賣出手續費",
      "證交稅",
      "賣出後所得",
      "狀態"
    )
    
    date_cols <- c(
      "買入日期",
      "賣出日期"
    )
    
    character_cols <- c(
      "股票代號",
      "股票名稱",
      "狀態"
    )
    
    for (
      nm in setdiff(
        required_cols,
        names(trades)
      )
    ) {
      
      if (
        nm %in% date_cols
      ) {
        
        trades[[nm]] <- as.Date(
          NA
        )
        
      } else if (
        nm %in% character_cols
      ) {
        
        trades[[nm]] <- NA_character_
        
      } else {
        
        trades[[nm]] <- NA_real_
        
      }
    }
    
    trades <- trades[
      ,
      required_cols,
      drop = FALSE
    ]
    
    trades
    
  })
  
  
  output$portfolio_trade_table <- renderTable({
    
    x <- portfolio_trades()
    
    req(
      nrow(x) > 0
    )
    
    x %>%
      mutate(
        買入日期 =
          ifelse(
            is.na(買入日期),
            "NA",
            format(
              as.Date(買入日期),
              "%Y-%m-%d"
            )
          ),
        賣出日期 =
          ifelse(
            is.na(賣出日期),
            "NA",
            format(
              as.Date(賣出日期),
              "%Y-%m-%d"
            )
          ),
        買入價格 =
          fmt_price(買入價格),
        賣出價格 =
          fmt_price(賣出價格),
        股數 =
          fmt_num(股數),
        配置資金 =
          fmt_num(配置資金, 2),
        買入成交金額 =
          fmt_num(買入成交金額, 2),
        買入手續費 =
          fmt_num(買入手續費, 2),
        實際投入資金 =
          fmt_num(實際投入資金, 2),
        賣出成交金額 =
          fmt_num(賣出成交金額, 2),
        賣出手續費 =
          fmt_num(賣出手續費, 2),
        證交稅 =
          fmt_num(證交稅, 2),
        賣出後所得 =
          fmt_num(賣出後所得, 2)
      )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 投資組合每日資產價值
  # ----------------------------------------------------------
  
  portfolio_daily <- reactive({
    
    trades <- portfolio_trades() %>%
      filter(
        狀態 == "有效",
        股數 > 0,
        is.finite(買入價格),
        is.finite(賣出價格)
      )
    
    req(
      nrow(trades) > 0
    )
    
    # --------------------------------------------------------
    # 初始資金
    # --------------------------------------------------------
    
    initial_capital <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      
      sum(
        trades$配置資金,
        na.rm = TRUE
      )
      
    } else {
      
      sum(
        trades$實際投入資金,
        na.rm = TRUE
      )
    }
    
    req(
      is.finite(initial_capital),
      initial_capital > 0
    )
    
    
    # --------------------------------------------------------
    # 投資組合期間
    # 第一筆買入 → 最後一筆賣出
    # --------------------------------------------------------
    
    start_date <- min(
      trades$買入日期,
      na.rm = TRUE
    )
    
    end_date <- max(
      trades$賣出日期,
      na.rm = TRUE
    )
    
    req(
      is.finite(start_date),
      is.finite(end_date),
      start_date <= end_date
    )
    
    
    # 只使用投資組合期間內的交易日
    dates <- trading_dates()[
      trading_dates() >= start_date &
        trading_dates() <= end_date
    ]
    
    req(
      length(dates) > 0
    )
    
    
    # --------------------------------------------------------
    # 每一個交易日計算
    # --------------------------------------------------------
    
    result <- lapply(
      
      dates,
      
      function(d) {
        
        # ======================================================
        # 1. 從初始資金開始
        # ======================================================
        
        cash <- initial_capital
        
        
        # ======================================================
        # 2. 扣除截至今天已經完成的買入
        # ======================================================
        
        bought <- trades[
          trades$買入日期 <= d,
          ,
          drop = FALSE
        ]
        
        if (
          nrow(bought) > 0
        ) {
          
          cash <-
            cash -
            sum(
              bought$實際投入資金,
              na.rm = TRUE
            )
          
        }
        
        
        # ======================================================
        # 3. 加回截至今天已經完成的賣出
        # ======================================================
        
        sold <- trades[
          trades$賣出日期 <= d,
          ,
          drop = FALSE
        ]
        
        if (
          nrow(sold) > 0
        ) {
          
          cash <-
            cash +
            sum(
              sold$賣出後所得,
              na.rm = TRUE
            )
          
        }
        
        
        # ======================================================
        # 4. 計算目前仍然持有的股票市值
        # ======================================================
        
        holdings <- 0
        
        active_trades <- trades[
          trades$買入日期 <= d &
            trades$賣出日期 > d,
          ,
          drop = FALSE
        ]
        
        if (
          nrow(active_trades) > 0
        ) {
          
          for (
            i in seq_len(
              nrow(active_trades)
            )
          ) {
            
            tr <- active_trades[i, ]
            
            current_price <- price_on_or_before(
              tr$股票代號,
              d
            )
            
            if (
              is.finite(current_price)
            ) {
              
              holdings <-
                holdings +
                tr$股數 *
                current_price
              
            }
          }
        }
        
        
        # ======================================================
        # 5. 投資組合總資產
        # ======================================================
        
        portfolio_value <-
          cash +
          holdings
        
        
        data.frame(
          date = d,
          cash = cash,
          holdings = holdings,
          portfolio_value = portfolio_value,
          stringsAsFactors = FALSE
        )
        
      }
      
    )
    
    
    result <- bind_rows(
      result
    )
    
    
    # --------------------------------------------------------
    # 數值安全檢查
    # --------------------------------------------------------
    
    result <- result %>%
      mutate(
        cash = ifelse(
          is.finite(cash),
          cash,
          NA_real_
        ),
        holdings = ifelse(
          is.finite(holdings),
          holdings,
          NA_real_
        ),
        portfolio_value = ifelse(
          is.finite(portfolio_value),
          portfolio_value,
          NA_real_
        )
      )
    
    
    result
    
  })
  
  
  output$portfolio_value_chart <- renderPlot({
    
    x <- portfolio_daily()
    
    req(
      input$portfolio_value_date_range
    )
    
    x <- x %>%
      filter(
        date >= as.Date(
          input$portfolio_value_date_range[1]
        ),
        date <= as.Date(
          input$portfolio_value_date_range[2]
        )
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = date,
        y = portfolio_value
      )
    ) +
      geom_line(
        linewidth = 0.9
      ) +
      labs(
        title = "投資組合每日資產價值",
        x = "日期",
        y = "資產價值（元）"
      ) +
      scale_y_continuous(
        labels = function(z) {
          format(
            round(z),
            big.mark = ","
          )
        }
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 投資組合績效摘要
  # ----------------------------------------------------------
  
  portfolio_metrics <- reactive({
    
    x <- portfolio_daily()
    trades <- portfolio_trades() %>%
      filter(
        狀態 == "有效",
        股數 > 0
      )
    
    initial <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      sum(
        trades$配置資金,
        na.rm = TRUE
      )
    } else {
      sum(
        trades$實際投入資金,
        na.rm = TRUE
      )
    }
    
    final <- tail(
      x$portfolio_value,
      1
    )
    
    daily_ret <- log(
      x$portfolio_value /
        lag(x$portfolio_value)
    )
    
    daily_ret <-
      daily_ret[
        is.finite(daily_ret)
      ]
    
    total_return <-
      final /
      initial - 1
    
    ann_return <-
      annualized_return(
        initial,
        final,
        nrow(x)
      )
    
    ann_vol <-
      sd(
        daily_ret,
        na.rm = TRUE
      ) *
      sqrt(
        TRADING_DAYS_PER_YEAR
      )
    
    sharpe <-
      if (
        is.finite(ann_vol) &&
        ann_vol > 0
      ) {
        mean(
          daily_ret,
          na.rm = TRUE
        ) *
          TRADING_DAYS_PER_YEAR /
          ann_vol
      } else {
        NA_real_
      }
    
    dd <- drawdown_value(
      x$portfolio_value
    )
    
    max_dd <- min(
      dd,
      na.rm = TRUE
    )
    
    list(
      initial = initial,
      final = final,
      total_return = total_return,
      annual_return = ann_return,
      annual_volatility = ann_vol,
      sharpe = sharpe,
      max_drawdown = max_dd,
      days = nrow(x)
    )
    
  })
  
  
  output$portfolio_summary <- renderTable({
    
    m <- portfolio_metrics()
    
    data.frame(
      
      指標 = c(
        "初始配置資金",
        "期末資產價值",
        "累積報酬率",
        "年化報酬率",
        "有效交易日數"
      ),
      
      數值 = c(
        
        fmt_num(
          m$initial,
          0
        ),
        
        fmt_num(
          m$final,
          2
        ),
        
        fmt_pct(
          m$total_return
        ),
        
        fmt_pct(
          m$annual_return
        ),
        
        fmt_num(
          m$days
        )
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$portfolio_performance_metrics <- renderTable({
    
    m <- portfolio_metrics()
    
    data.frame(
      
      指標 = c(
        "年化報酬率",
        "年化波動度",
        "最大回撤",
        "夏普比率"
      ),
      
      數值 = c(
        
        fmt_pct(
          m$annual_return
        ),
        
        fmt_pct(
          m$annual_volatility,
          2,
          FALSE
        ),
        
        fmt_pct(
          m$max_drawdown
        ),
        
        ifelse(
          is.finite(m$sharpe),
          sprintf(
            "%.3f",
            m$sharpe
          ),
          "NA"
        )
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 投資組合與 0050 比較
  # ----------------------------------------------------------
  
  portfolio_benchmark <- reactive({
    
    p <- portfolio_daily()
    
    req(
      nrow(p) > 1
    )
    
    start <- min(
      p$date
    )
    
    end <- max(
      p$date
    )
    
    b <- data() %>%
      filter(
        symbol == "0050",
        date >= start,
        date <= end,
        is.finite(adjusted)
      ) %>%
      arrange(date)
    
    req(
      nrow(b) > 1
    )
    
    p_return <-
      p$portfolio_value /
      first(p$portfolio_value) - 1
    
    b_return <-
      b$adjusted /
      first(b$adjusted) - 1
    
    p2 <- data.frame(
      date = p$date,
      portfolio_return = p_return
    )
    
    b2 <- data.frame(
      date = b$date,
      benchmark_return = b_return
    )
    
    list(
      portfolio = p2,
      benchmark = b2
    )
    
  })
  
  
  output$portfolio_benchmark_summary <- renderTable({
    
    x <- portfolio_benchmark()
    
    p <- tail(
      x$portfolio$portfolio_return,
      1
    )
    
    b <- tail(
      x$benchmark$benchmark_return,
      1
    )
    
    data.frame(
      
      指標 = c(
        "投資組合",
        "0050",
        "超額報酬"
      ),
      
      累積報酬率 = c(
        fmt_pct(p),
        fmt_pct(b),
        fmt_pct(p - b)
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$portfolio_benchmark_chart <- renderPlot({
    
    x <- portfolio_benchmark()
    
    p <- x$portfolio
    b <- x$benchmark
    
    merged <- full_join(
      p,
      b,
      by = "date"
    ) %>%
      arrange(date)
    
    ggplot(
      merged,
      aes(
        x = date
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_line(
        aes(
          y = portfolio_return,
          color = "投資組合"
        ),
        linewidth = 1
      ) +
      
      geom_line(
        aes(
          y = benchmark_return,
          color = "0050"
        ),
        linewidth = 1,
        linetype = "dashed"
      ) +
      
      scale_color_manual(
        values = c(
          "投資組合" = "steelblue",
          "0050" = "red"
        )
      ) +
      
      labs(
        title = "投資組合與 0050 累積報酬比較",
        x = "日期",
        y = "累積報酬率",
        color = "比較標的"
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(z * 100, 1),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        legend.position = "bottom",
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ==========================================================
  # 9. 資料明細
  # ==========================================================
  
  output$stock_table <- renderTable({
    
    x <- data()
    
    symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    result <- lapply(
      symbols,
      function(s) {
        
        z <- x %>%
          filter(
            symbol == s,
            is.finite(adjusted)
          )
        
        if (
          nrow(z) == 0
        ) {
          return(NULL)
        }
        
        data.frame(
          
          股票代號 = s,
          
          股票名稱 =
            get_stock_name(s),
          
          產業 =
            get_stock_industry(s),
          
          最早日期 =
            format(
              min(z$date),
              "%Y-%m-%d"
            ),
          
          最新日期 =
            format(
              max(z$date),
              "%Y-%m-%d"
            ),
          
          有效觀測數 =
            nrow(z),
          
          stringsAsFactors =
            FALSE
          
        )
      }
    )
    
    bind_rows(
      result
    )
    
  },
  striped = FALSE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ==========================================================
  # 10. 日期同步
  # ==========================================================
  
  # ----------------------------------------------------------
  # 市場價格圖：資料更新後同步可選日期範圍
  # ----------------------------------------------------------
  
  observe({
    
    req(
      latest_date(),
      earliest_date()
    )
    
    updateDateRangeInput(
      session,
      "market_chart_date_range",
      min = earliest_date(),
      max = latest_date()
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 投資組合：跟著實際交易期間同步
  # ----------------------------------------------------------
  
  observeEvent(
    portfolio_trades(),
    {
      
      trades <- portfolio_trades() %>%
        filter(
          狀態 == "有效",
          股數 > 0,
          !is.na(買入日期),
          !is.na(賣出日期)
        )
      
      if (
        nrow(trades) == 0
      ) {
        return()
      }
      
      portfolio_start <- min(
        trades$買入日期,
        na.rm = TRUE
      )
      
      portfolio_end <- max(
        trades$賣出日期,
        na.rm = TRUE
      )
      
      if (
        !is.finite(portfolio_start) ||
        !is.finite(portfolio_end) ||
        portfolio_start > portfolio_end
      ) {
        return()
      }
      
      updateDateRangeInput(
        session,
        "portfolio_value_date_range",
        start = portfolio_start,
        end = portfolio_end,
        min = earliest_date(),
        max = latest_date()
      )
      
    },
    ignoreInit = FALSE
  )
  # ============================================================
  # 第 3 版：GARCH 波動預測與 0010 組合
  # 整段貼在 server 內（位置任意，建議貼在最後）
  # 所有名稱都加 g10_ 前綴
  # ============================================================
  
  g10_pct <- function(x, d = 2, sign = TRUE) {
    ifelse(is.finite(x), sprintf(if (sign) paste0("%+.", d, "f%%") else paste0("%.", d, "f%%"), x * 100), "—")
  }
  g10_num <- function(x, d = 2) {
    ifelse(is.finite(x), sprintf(paste0("%.", d, "f"), x), "—")
  }
  
  # ----------------------------------------------------------
  # 1. 單檔 GARCH
  # ----------------------------------------------------------
  
  g10_one <- shiny::reactive({
    s <- input$g10_stock
    shiny::req(!is.null(s), nzchar(s))
    d <- as.data.frame(data())
    x <- d[d$symbol == s & !is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
    x <- x[order(x$date), , drop = FALSE]
    shiny::req(nrow(x) > 300)
    garch_analyse_one(x$adjusted, window = as.integer(input$g10_window), horizon = 63)
  })
  
  output$g10_kpi_fvol <- shiny::renderText({
    a <- g10_one(); shiny::req(!is.null(a))
    g10_pct(a$summary$forecast_vol, 2, FALSE)
  })
  
  output$g10_kpi_rvol <- shiny::renderText({
    a <- g10_one(); shiny::req(!is.null(a))
    g10_pct(a$real_vol, 2, FALSE)
  })
  
  output$g10_kpi_ratio <- shiny::renderText({
    a <- g10_one(); shiny::req(!is.null(a))
    g10_num(a$ratio, 3)
  })
  
  output$g10_kpi_regime <- shiny::renderText({
    a <- g10_one(); shiny::req(!is.null(a))
    a$regime
  })
  
  output$g10_param_table <- shiny::renderTable({
    a <- g10_one(); shiny::req(!is.null(a))
    f <- a$fit
    data.frame(
      參數 = c("ω（常數項）", "α（ARCH 衝擊）", "β（GARCH 延續）", "α + β（持續性）", "半衰期（交易日）"),
      估計值 = c(g10_num(f$omega, 4), g10_num(f$alpha, 4), g10_num(f$beta, 4),
              g10_num(f$persistence, 4), g10_num(f$half_life, 1)),
      標準誤 = c(g10_num(f$se_omega, 4), g10_num(f$se_alpha, 4), g10_num(f$se_beta, 4), "—", "—"),
      t值 = c(g10_num(f$t_omega, 2), g10_num(f$t_alpha, 2), g10_num(f$t_beta, 2), "—", "—"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$g10_vol_path <- shiny::renderPlot({
    a <- g10_one(); shiny::req(!is.null(a))
    f <- a$fit
    v <- f$sigma2
    n <- length(v)
    idx <- max(1, n - 249):n
    gvol <- sqrt(v[idx] * GARCH_TD) / 100
    
    r <- a$returns
    k <- length(r)
    real <- rep(NA_real_, k)
    w <- 21
    for (i in seq(w, k)) real[i] <- sqrt(mean(r[(i - w + 1):i]^2) * GARCH_TD) / 100
    
    df <- data.frame(t = seq_along(idx), garch = gvol, real = real[idx])
    ggplot2::ggplot(df, ggplot2::aes(x = t)) +
      ggplot2::geom_line(ggplot2::aes(y = real), colour = "grey65", linewidth = 0.7) +
      ggplot2::geom_line(ggplot2::aes(y = garch), colour = "#1976d2", linewidth = 0.9) +
      ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%")) +
      ggplot2::labs(title = "GARCH 條件波動（藍）vs 21 日實現波動（灰）",
                    x = "最近 250 個交易日", y = "年化波動") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })
  
  output$g10_fan <- shiny::renderPlot({
    a <- g10_one(); shiny::req(!is.null(a))
    fv <- garch11_forecast(a$fit, 63)
    df <- data.frame(h = seq_along(fv), vol = sqrt(fv * GARCH_TD) / 100)
    ggplot2::ggplot(df, ggplot2::aes(x = h, y = vol)) +
      ggplot2::geom_line(colour = "#2e7d32", linewidth = 1) +
      ggplot2::geom_hline(yintercept = a$real_vol, linetype = 2, colour = "#e53935") +
      ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%")) +
      ggplot2::labs(title = "未來 63 日年化波動預測（紅虛線 = 近一年實現波動）",
                    x = "預測期（交易日）", y = "年化波動") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })
  
  output$g10_diag <- shiny::renderTable({
    a <- g10_one(); shiny::req(!is.null(a))
    f <- a$fit
    data.frame(
      項目 = c("樣本數", "對數概似", "AIC", "BIC", "Ljung-Box Q(10)", "Q 檢定 p 值", "收斂", "無條件年化波動", "1 日 VaR 95%", "1 日 CVaR 95%"),
      數值 = c(as.character(f$n), g10_num(f$loglik, 1), g10_num(f$aic, 1), g10_num(f$bic, 1),
             g10_num(f$lb_stat, 2), g10_num(f$lb_p, 4),
             if (isTRUE(f$converged)) "是" else "否",
             g10_pct(f$uncond_vol, 2, FALSE),
             g10_pct(a$summary$var95, 2, FALSE),
             g10_pct(a$summary$cvar95, 2, FALSE)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # ----------------------------------------------------------
  # 2. 全成分股 GARCH 表（按鈕觸發 + 快取）
  # ----------------------------------------------------------
  
  g10_store <- shiny::reactiveVal(NULL)
  
  shiny::observeEvent(input$g10_run_all, {
    d <- as.data.frame(data())
    syms <- stock_info$symbol
    withProgress(message = "估計 GARCH(1,1)…", value = 0, {
      tbl <- p10_garch_table(
        d, syms,
        window = as.integer(input$g10_window),
        horizon = 63,
        progress = function(k, n, s) {
          setProgress(k / n, detail = paste0("第 ", k, " / ", n, " 檔：", s))
        }
      )
    })
    g10_store(tbl)
  })
  
  output$g10_all_table <- shiny::renderTable({
    g <- g10_store()
    if (is.null(g)) return(data.frame(提示 = "請先按上方按鈕估計"))
    out <- data.frame(
      股票 = paste0(get_stock_name(g$symbol), "（", g$symbol, "）"),
      `ω` = g10_num(g$omega, 4),
      `α` = g10_num(g$alpha, 4),
      `β` = g10_num(g$beta, 4),
      `α+β` = g10_num(g$persistence, 4),
      半衰期 = g10_num(g$half_life, 1),
      預測波動 = g10_pct(g$forecast_vol, 1, FALSE),
      實現波動 = g10_pct(g$real_vol, 1, FALSE),
      比值 = g10_num(g$ratio, 2),
      狀態 = g$regime,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    out[order(g$ratio), , drop = FALSE]
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$g10_ratio_bar <- shiny::renderPlot({
    g <- g10_store()
    shiny::req(!is.null(g))
    x <- g[is.finite(g$ratio), , drop = FALSE]
    x <- x[order(x$ratio), , drop = FALSE]
    labs <- paste0(get_stock_name(x$symbol), "(", x$symbol, ")")
    x$label <- factor(labs, levels = labs)
    ggplot2::ggplot(x, ggplot2::aes(x = label, y = ratio, fill = ratio)) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient2(midpoint = 1, low = "#1565c0", mid = "#ffffff", high = "#c62828") +
      ggplot2::labs(title = "預測波動 ÷ 近一年實現波動", x = NULL, y = "比值") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     axis.text.y = ggplot2::element_text(size = 8),
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })
  
  # ----------------------------------------------------------
  # 3. 0010 組合
  # ----------------------------------------------------------
  
  # 共用的寬表（報酬、價格、EWMA 波動），只建一次
  g10_mats <- shiny::reactive({
    d <- as.data.frame(data())
    d <- d[d$symbol %in% c("0050", stock_info$symbol), , drop = FALSE]
    d <- d[!is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
    d <- d[order(d$date), , drop = FALSE]
    
    wide <- as.data.frame(tidyr::pivot_wider(d[, c("date", "symbol", "log_return")],
                                             names_from = symbol, values_from = log_return))
    widep <- as.data.frame(tidyr::pivot_wider(d[, c("date", "symbol", "adjusted")],
                                              names_from = symbol, values_from = adjusted))
    shiny::req(nrow(wide) > 300, nrow(wide) == nrow(widep))
    
    dates <- as.Date(wide$date)
    m <- as.matrix(wide[, -1, drop = FALSE]); rownames(m) <- as.character(dates)
    pm <- as.matrix(widep[, -1, drop = FALSE]); rownames(pm) <- as.character(dates)
    storage.mode(m) <- "double"; storage.mode(pm) <- "double"
    
    list(dates = dates, m = m, pm = pm, vol = p10_vol_matrix(m))
  })
  
  g10_scores <- shiny::reactive({
    mt <- g10_mats()
    vol <- mt$vol
    
    # 若已估計 GARCH，用最新一期的 GARCH 預測波動取代最後一列
    g <- g10_store()
    if (!is.null(g) && nrow(g) > 0) {
      idx <- match(colnames(mt$m), g$symbol)
      fv <- g$forecast_vol[idx]
      ok <- is.finite(fv)
      if (any(ok)) vol[nrow(vol), ok] <- fv[ok]
    }
    
    sc <- p10_matrix_scores(mt$m, vol, preset = input$g10_preset)
    shiny::req(!is.null(sc), nrow(sc) > 0)
    sc <- sc[sc$symbol != "0050", , drop = FALSE]
    sc[order(-sc$p10_score), , drop = FALSE]
  })
  
  g10_pick <- shiny::reactive({
    t <- g10_scores()
    sel <- p10_select(t, 10)
    w <- p10_weights(sel, scheme = input$g10_scheme, cap = input$g10_cap)
    list(sel = sel, w = w)
  })
  
  output$g10_weights <- shiny::renderTable({
    p <- g10_pick()
    sel <- p$sel
    w <- p$w
    cap <- as.numeric(input$g10_capital)
    amt <- as.numeric(w) * cap
    data.frame(
      排名 = seq_len(nrow(sel)),
      股票 = paste0(sel$name, "（", sel$symbol, "）"),
      `0010 分數` = g10_num(sel$p10_score, 1),
      Growth = g10_num(sel$growth_score, 1),
      Risk = g10_num(sel$risk_score, 1),
      Stability = g10_num(sel$stability_score, 1),
      GARCH = g10_num(sel$garch_score, 1),
      預測波動 = g10_pct(sel$forecast_vol, 1, FALSE),
      權重 = paste0(round(as.numeric(w) * 100, 1), "%"),
      金額 = formatC(round(amt), format = "d", big.mark = ","),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$g10_cost <- shiny::renderTable({
    p <- g10_pick()
    cap <- as.numeric(input$g10_capital)
    amt <- as.numeric(p$w) * cap
    
    buy_fee <- sum(pmax(P10_COST$min_fee, amt * P10_COST$fee))
    buy_slip <- sum(amt * P10_COST$slip)
    sell_fee <- sum(pmax(P10_COST$min_fee, amt * P10_COST$fee))
    sell_tax <- sum(amt * P10_COST$tax)
    sell_slip <- sum(amt * P10_COST$slip)
    
    out <- data.frame(
      項目 = c("買進手續費", "買進滑價", "賣出手續費", "證交稅（賣出）", "賣出滑價",
             "建倉成本合計", "假設全數賣出之總成本", "佔投入金額（%）"),
      金額 = c(buy_fee, buy_slip, sell_fee, sell_tax, sell_slip,
             buy_fee + buy_slip,
             buy_fee + buy_slip + sell_fee + sell_tax + sell_slip,
             (buy_fee + buy_slip + sell_fee + sell_tax + sell_slip) / cap * 100),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    out$金額 <- ifelse(out$項目 == "佔投入金額（%）",
                     paste0(sprintf("%.2f", out$金額), "%"),
                     formatC(round(out$金額), format = "d", big.mark = ","))
    out
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$g10_next_date <- shiny::renderUI({
    d <- as.data.frame(data())
    nd <- p10_next_rebalance(max(d$date, na.rm = TRUE))
    div(class = "time-info",
        strong("下一次再平衡日："), format(nd, "%Y-%m-%d"),
        "（每季一次；評分使用當日以前資料，持有 63 個交易日）")
  })
  
  # ----------------------------------------------------------
  # 3-1. 持有追蹤（以上一期季末名單為基準）
  # ----------------------------------------------------------
  
  g10_track_data <- shiny::reactive({
    tryCatch({
      mt <- g10_mats()
      dates <- mt$dates
      m <- mt$m
      pm <- mt$pm
      
      # 最近一個「已結束」的季末：必須排除資料最後一天所屬的本季，
      # 否則買進日 = 今天，每檔報酬永遠是 0，而且會跟現在的名單完全一樣。
      key <- paste0(format(dates, "%Y"), "Q", ceiling(as.integer(format(dates, "%m")) / 3))
      lastq <- sort(as.integer(tapply(seq_along(dates), key, max)))
      idx_all <- sort(as.integer(lastq))
      idx_all <- idx_all[idx_all < nrow(m)]
      if (!length(idx_all)) stop("資料裡找不到已結束的季度")
      
      i <- if (length(lastq) >= 2) lastq[length(lastq) - 1] else lastq[1]
      if (i < 300) stop("最近季末之前的交易日不足 300 天")
      
      sub <- m[1:i, , drop = FALSE]
      vs <- mt$vol[1:i, , drop = FALSE]
      
      sc <- p10_matrix_scores(sub, vs, preset = input$g10_preset)
      if (is.null(sc) || !nrow(sc)) stop("該季無法計算分數（樣本不足）")
      sc <- sc[sc$symbol != "0050", , drop = FALSE]
      sc <- sc[is.finite(sc$p10_score), , drop = FALSE]
      if (nrow(sc) < 3) stop("有效分數不足 3 檔")
      
      sel <- utils::head(sc[order(-sc$p10_score), , drop = FALSE], 10)
      ww <- p10_weights(sel, scheme = input$g10_scheme, cap = input$g10_cap)
      
      buy <- unname(pm[i, sel$symbol]); names(buy) <- sel$symbol
      now <- unname(pm[nrow(pm), sel$symbol]); names(now) <- sel$symbol
      for (k in seq_along(now)) {
        if (!is.finite(now[k])) {
          col <- pm[, sel$symbol[k]]
          jj <- suppressWarnings(max(which(is.finite(col))))
          if (is.finite(jj)) now[k] <- col[jj]
        }
      }
      ret <- now / buy - 1
      
      list(ok = TRUE, date = dates[i], sel = sel, w = ww,
           buy = buy, now = now, ret = ret,
           i = i, nrow_m = nrow(m), n_scored = nrow(sc))
    }, error = function(e) {
      list(ok = FALSE, msg = conditionMessage(e))
    })
  })
  
  g10_cmp <- shiny::reactive({
    t <- g10_track_data()
    if (!isTRUE(t$ok)) return(NULL)
    p <- tryCatch(g10_pick(), error = function(e) NULL)
    if (is.null(p) || is.null(p$w) || !length(p$w)) return(NULL)
    
    prev <- names(t$w)
    cur <- names(p$w)
    
    drift <- t$w * (1 + t$ret[names(t$w)])
    drift[!is.finite(drift)] <- 0
    if (sum(drift) > 0) drift <- drift / sum(drift)
    
    all_sym <- union(prev, cur)
    a <- ifelse(all_sym %in% names(drift), drift[all_sym], 0)
    b <- ifelse(all_sym %in% names(p$w), p$w[all_sym], 0)
    
    list(new = setdiff(cur, prev), hold = intersect(cur, prev), drop = setdiff(prev, cur),
         turnover = sum(abs(b - a), na.rm = TRUE) / 2)
  })
  
  output$g10_n_new <- shiny::renderText({
    c <- g10_cmp(); if (is.null(c)) return("—"); paste0(length(c$new), " 檔")
  })
  
  output$g10_n_hold <- shiny::renderText({
    c <- g10_cmp(); if (is.null(c)) return("—"); paste0(length(c$hold), " 檔")
  })
  
  output$g10_n_drop <- shiny::renderText({
    c <- g10_cmp(); if (is.null(c)) return("—"); paste0(length(c$drop), " 檔")
  })
  
  output$g10_turnover_est <- shiny::renderText({
    c <- g10_cmp(); if (is.null(c)) return("—"); paste0(round(c$turnover * 100, 1), "%")
  })
  
  output$g10_track <- shiny::renderTable({
    t <- g10_track_data()
    if (!isTRUE(t$ok)) return(data.frame(說明 = t$msg, check.names = FALSE))
    
    g <- g10_store()
    fv <- NULL
    if (!is.null(g)) fv <- g$forecast_vol[match(t$sel$symbol, g$symbol)]
    if (is.null(fv) || all(!is.finite(fv))) fv <- t$sel$forecast_vol
    bad <- !is.finite(fv)
    fv[bad] <- t$sel$forecast_vol[bad]
    
    h <- 21
    s <- fv / sqrt(252) * sqrt(h)
    stop_px <- t$buy * (1 - input$g10_k_stop * s)
    take_px <- t$buy * (1 + input$g10_k_take * s)
    
    status <- ifelse(t$now <= stop_px, "已觸停損",
                     ifelse(t$now >= take_px, "已觸停利", "持有中"))
    status[is.na(status)] <- "—"
    
    data.frame(
      股票 = paste0(get_stock_name(t$sel$symbol), "（", t$sel$symbol, "）"),
      權重 = paste0(round(as.numeric(t$w) * 100, 1), "%"),
      買進價 = ifelse(is.finite(t$buy), sprintf("%.2f", t$buy), "—"),
      現價 = ifelse(is.finite(t$now), sprintf("%.2f", t$now), "—"),
      報酬 = g10_pct(t$ret, 2),
      停損價 = ifelse(is.finite(stop_px), sprintf("%.2f", stop_px), "—"),
      停利價 = ifelse(is.finite(take_px), sprintf("%.2f", take_px), "—"),
      狀態 = status,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$g10_track_note <- shiny::renderUI({
    t <- g10_track_data()
    if (!isTRUE(t$ok)) {
      return(div(class = "warning-note",
                 strong("無法計算持有追蹤："), t$msg))
    }
    g <- g10_store()
    src <- if (is.null(g)) "EWMA（尚未估計 GARCH）" else "GARCH（最新一期）"
    div(class = "time-info",
        strong("基準日："), format(t$date, "%Y-%m-%d"),
        "｜波動估計：", src,
        "｜資料截止：", format(max(g10_mats()$dates), "%Y-%m-%d"),
        "｜季末索引 ", t$i, " / ", t$nrow_m, " 日，有效評分 ", t$n_scored, " 檔")
  })
  
  # ----------------------------------------------------------
  # 4. 季度再平衡回測
  # ----------------------------------------------------------
  
  g10_bt <- shiny::eventReactive(input$g10_run_bt, {
    d <- as.data.frame(data())
    d <- d[d$symbol %in% c("0050", stock_info$symbol) & is.finite(d$log_return), , drop = FALSE]
    d <- d[order(d$date), , drop = FALSE]
    
    wide <- as.data.frame(
      tidyr::pivot_wider(d[, c("date", "symbol", "log_return")],
                         names_from = symbol, values_from = log_return)
    )
    shiny::req(ncol(wide) >= 20)
    
    dates <- as.Date(wide$date)
    m <- as.matrix(wide[, -1, drop = FALSE])
    rownames(m) <- as.character(dates)
    storage.mode(m) <- "double"
    
    vol <- p10_vol_matrix(m)
    
    key <- paste0(format(dates, "%Y"), "Q", ceiling(as.integer(format(dates, "%m")) / 3))
    last_idx <- tapply(seq_along(dates), key, max)
    idx <- sort(as.integer(last_idx))
    idx <- idx[idx > 800 & idx < (length(dates) - 63)]
    shiny::req(length(idx) >= 4)
    
    withProgress(message = "季度回測中…", value = 0, {
      p10_backtest(
        m, vol, idx,
        hold = 63, n = 10,
        preset = input$g10_preset,
        scheme = input$g10_scheme,
        cap = input$g10_cap,
        progress = function(k, n, s) {
          setProgress(k / n, detail = paste0("第 ", k, " / ", n, " 季"))
        }
      )
    })
  })
  
  output$g10_bt_stats <- shiny::renderTable({
    r <- g10_bt()
    shiny::req(!is.null(r))
    s <- r$stats
    data.frame(
      指標 = c("回測季數", "累積報酬（扣成本）", "年化報酬", "年化波動", "Sharpe", "最大回撤",
             "季勝率（vs 0050）", "平均換手率", "累計成本", "0050 同期累積報酬"),
      數值 = c(as.character(s$n_periods),
             g10_pct(s$cum_ret), g10_pct(s$ann_ret), g10_pct(s$ann_vol, 2, FALSE),
             g10_num(s$sharpe, 2), g10_pct(s$mdd),
             paste0(round(s$win_vs_bench * 100, 1), "%"),
             paste0(round(s$avg_turnover * 100, 1), "%"),
             g10_pct(s$total_cost, 2, FALSE),
             g10_pct(s$bench_cum)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$g10_bt_curve <- shiny::renderPlot({
    r <- g10_bt()
    shiny::req(!is.null(r))
    q <- r$quarters
    df <- rbind(
      data.frame(date = q$date, value = q$equity, series = "0010（扣成本）"),
      data.frame(date = q$date, value = q$equity_0050, series = "0050")
    )
    ggplot2::ggplot(df, ggplot2::aes(x = date, y = value, colour = series)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::labs(title = "季度再平衡淨值曲線", x = NULL, y = "淨值（起始 = 1）", colour = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })
  
  output$g10_bt_quarters <- shiny::renderTable({
    r <- g10_bt()
    shiny::req(!is.null(r))
    q <- r$quarters
    data.frame(
      評分日 = format(q$date, "%Y-%m-%d"),
      換手率 = paste0(round(q$turnover * 100, 1), "%"),
      成本 = g10_pct(q$cost_rate, 2, FALSE),
      毛報酬 = g10_pct(q$gross, 2),
      淨報酬 = g10_pct(q$net, 2),
      `0050` = g10_pct(q$bench, 2),
      持股 = q$top,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # ============================================================
  # v10：0010 組合頁呈現用統計摘要與配置圖（只讀 v4_sel/v4_w）
  # ============================================================
  output$g10_v10_count <- shiny::renderText({
    paste0(nrow(v4_sel()), " 檔")
  })

  g10_v10_display <- shiny::reactive({
    s <- v4_sel()
    w <- v4_w()
    shiny::req(nrow(s) > 0, length(w) > 0)
    x <- data.frame(
      symbol = as.character(s$symbol),
      name = as.character(s$name),
      mom = as.numeric(s$mom_score),
      risk = as.numeric(s$risk_score),
      score = as.numeric(s$p10_smooth),
      weight = as.numeric(w[s$symbol]),
      stringsAsFactors = FALSE
    )
    shiny::req(all(is.finite(x$weight)))
    x
  })

  output$g10_v10_mom <- shiny::renderText({
    d <- g10_v10_display()
    sprintf("%.1f / 100", stats::weighted.mean(d$mom, d$weight, na.rm = TRUE))
  })

  output$g10_v10_risk <- shiny::renderText({
    d <- g10_v10_display()
    sprintf("%.1f / 100", stats::weighted.mean(d$risk, d$weight, na.rm = TRUE))
  })

  output$g10_v10_maxw <- shiny::renderText({
    sprintf("%.1f%%", 100 * max(g10_v10_display()$weight))
  })

  output$g10_v10_simple_table <- shiny::renderTable({
    d <- g10_v10_display()
    data.frame(
      股票 = paste0(d$name, "（", d$symbol, "）"),
      `動能分` = sprintf("%.1f", d$mom),
      `風險分` = sprintf("%.1f", d$risk),
      `綜合分` = sprintf("%.1f", d$score),
      `權重` = sprintf("%.1f%%", d$weight * 100),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # v11：用真實選股與資金權重產生可直接辨識的持股長條（只改呈現）
  output$g10_v11_weight_bars <- shiny::renderUI({
    d <- tryCatch(g10_v10_display(), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) {
      return(shiny::div(class = "g10-v11-subnote",
        "目前無法取得持股資料。請確認已載入股票資料並完成 0010 選股評分。"))
    }
    d <- d[is.finite(d$weight) & d$weight >= 0, , drop = FALSE]
    if (!nrow(d)) {
      return(shiny::div(class = "g10-v11-subnote", "尚無有效的持股配置權重。"))
    }
    d <- d[order(-d$weight, d$symbol), , drop = FALSE]
    cap <- suppressWarnings(as.numeric(input$g10_cap))
    if (!length(cap) || !is.finite(cap)) cap <- 0.20
    upper <- max(cap, d$weight, 0.10, na.rm = TRUE)
    rows <- lapply(seq_len(nrow(d)), function(i) {
      nm <- as.character(d$name[i])
      if (is.na(nm) || !nzchar(trimws(nm))) nm <- as.character(d$symbol[i])
      label <- paste0(nm, "（", d$symbol[i], "）")
      pct <- sprintf("%.1f%%", 100 * d$weight[i])
      bar_width <- max(0, min(100, 100 * d$weight[i] / upper))
      shiny::div(class = "g10-v11-row",
        shiny::span(class = "g10-v11-name", label),
        shiny::div(class = "g10-v11-track",
          shiny::div(class = "g10-v11-fill", style = sprintf("width:%.2f%%;", bar_width))),
        shiny::strong(class = "g10-v11-value", pct))
    })
    do.call(shiny::tagList, rows)
  })

  output$g10_v10_weight_plot <- shiny::renderPlot({
    d <- g10_v10_display()
    d <- d[is.finite(d$weight) & d$weight >= 0, , drop = FALSE]
    shiny::req(nrow(d) > 0)
    d$label <- paste0(d$name, "（", d$symbol, "）")
    d <- d[order(d$weight), , drop = FALSE]
    d$label <- factor(d$label, levels = unique(d$label))
    ggplot2::ggplot(d, ggplot2::aes(x = label, y = weight)) +
      ggplot2::geom_col(fill = "#5d90c4", width = 0.68) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", weight * 100)),
                         hjust = -0.15, size = 3.5, color = "#3f607b") +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_y_continuous(
        labels = function(v) paste0(round(v * 100), "%"),
        expand = ggplot2::expansion(mult = c(0, 0.22))) +
      ggplot2::labs(x = NULL, y = "資金權重") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(color = "#45617a", size = 10),
        axis.text.x = ggplot2::element_text(color = "#8295a8"),
        axis.title.x = ggplot2::element_text(color = "#72869a"),
        plot.margin = ggplot2::margin(6, 26, 6, 6))
  })

  # ============================================================
  # v4 覆寫：0010 組合頁改接「動能分 + 風險穩定分」
  # ============================================================
  
  v4_w <- shiny::reactive({
    s <- v4_sel()
    shiny::req(nrow(s) > 0)
    cap <- input$g10_cap
    if (is.null(cap) || !is.finite(cap)) cap <- 0.20
    sch <- input$g10_scheme
    if (is.null(sch)) sch <- "inv_vol"
    v4_weights(s, scheme = sch, cap = cap)
  })
  
  g10_track_v4 <- shiny::reactive({
    d <- as.data.frame(data())
    reb <- v4_reb()
    shiny::req(!is.null(reb), !is.na(reb$date))
    
    t <- v4_score_table(d[d$date <= reb$date, , drop = FALSE])
    shiny::req(!is.null(t), nrow(t) >= 3)
    
    pm <- v4_pm()
    sel <- v4_pick(t, 10)
    sel <- sel[sel$symbol %in% colnames(pm), , drop = FALSE]
    shiny::req(nrow(sel) >= 3)
    
    w <- v4_weights(sel, scheme = "inv_vol", cap = 0.20)
    i <- reb$idx
    buy <- unname(pm[i, sel$symbol]); names(buy) <- sel$symbol
    now <- unname(pm[nrow(pm), sel$symbol]); names(now) <- sel$symbol
    for (k in seq_along(now)) {
      if (!is.finite(now[k])) {
        col <- pm[, sel$symbol[k]]
        idx <- suppressWarnings(max(which(is.finite(col))))
        if (is.finite(idx)) now[k] <- col[idx]
      }
    }
    list(date = reb$date, sel = sel, w = w, buy = buy, now = now,
         ret = now / buy - 1)
  })
  
  g10_cmp_v4 <- shiny::reactive({
    tr <- g10_track_v4()
    cur <- v4_sel()
    cw <- v4_w()
    
    prev <- names(tr$w)
    now <- names(cw)
    drift <- tr$w * (1 + tr$ret[names(tr$w)])
    drift[!is.finite(drift)] <- 0
    if (sum(drift) > 0) drift <- drift / sum(drift)
    
    all_sym <- union(prev, now)
    a <- ifelse(all_sym %in% names(drift), drift[all_sym], 0)
    b <- ifelse(all_sym %in% names(cw), cw[all_sym], 0)
    
    list(new = setdiff(now, prev), hold = intersect(now, prev),
         drop = setdiff(prev, now),
         turnover = sum(abs(b - a), na.rm = TRUE) / 2)
  })
  
  output$g10_n_new <- shiny::renderText({
    paste0(length(g10_cmp_v4()$new), " 檔")
  })
  
  output$g10_n_hold <- shiny::renderText({
    paste0(length(g10_cmp_v4()$hold), " 檔")
  })
  
  output$g10_n_drop <- shiny::renderText({
    paste0(length(g10_cmp_v4()$drop), " 檔")
  })
  
  output$g10_turnover_est <- shiny::renderText({
    paste0(round(g10_cmp_v4()$turnover * 100, 1), "%")
  })
  
  output$g10_weights <- shiny::renderTable({
    s <- v4_sel()
    w <- v4_w()
    cap <- as.numeric(input$g10_capital)
    amt <- as.numeric(w[s$symbol]) * cap
    
    data.frame(
      排名 = seq_len(nrow(s)),
      股票 = paste0(s$name, "（", s$symbol, "）"),
      產業 = s$industry,
      `0010 分` = sprintf("%.1f", s$p10_smooth),
      動能分 = sprintf("%.1f", s$mom_score),
      風險穩定分 = sprintf("%.1f", s$risk_score),
      `GARCH 比值` = ifelse(is.finite(s$ratio), sprintf("%.2f", s$ratio), "—"),
      權重 = sprintf("%.1f%%", as.numeric(w[s$symbol]) * 100),
      金額 = formatC(round(amt), format = "d", big.mark = ","),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$g10_cost <- shiny::renderTable({
    w <- v4_w()
    cap <- as.numeric(input$g10_capital)
    amt <- as.numeric(w) * cap
    
    buy_fee  <- sum(pmax(P10_COST$min_fee, amt * P10_COST$fee))
    buy_slip <- sum(amt * P10_COST$slip)
    sell_fee <- sum(pmax(P10_COST$min_fee, amt * P10_COST$fee))
    sell_tax <- sum(amt * P10_COST$tax)
    sell_slip <- sum(amt * P10_COST$slip)
    total <- buy_fee + buy_slip + sell_fee + sell_tax + sell_slip
    
    out <- data.frame(
      項目 = c("買進手續費", "買進滑價", "賣出手續費", "證交稅（賣出）",
             "賣出滑價", "建倉成本合計", "全數賣出之總成本", "佔投入金額（%）"),
      金額 = c(buy_fee, buy_slip, sell_fee, sell_tax, sell_slip,
             buy_fee + buy_slip, total, total / cap * 100),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    out$金額 <- ifelse(out$項目 == "佔投入金額（%）",
                     paste0(sprintf("%.2f", out$金額), "%"),
                     formatC(round(out$金額), format = "d", big.mark = ","))
    out
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$g10_next_date <- shiny::renderUI({
    d <- as.Date(rownames(v4_pm()))
    last <- max(d, na.rm = TRUE)
    y <- as.integer(format(last, "%Y")); mo <- as.integer(format(last, "%m"))
    q <- (mo - 1) %/% 3 + 1
    if (q == 4) { y <- y + 1; mo <- 1 } else { mo <- q * 3 + 1 }
    nd <- as.Date(sprintf("%04d-%02d-01", y, mo))
    while (as.integer(format(nd, "%u")) > 5) nd <- nd + 1
    
    div(class = "time-info",
        strong("下一次再平衡日："), as.character(nd),
        "（每季一次；評分使用當日以前資料，持有 63 個交易日）")
  })
  
  output$g10_track <- shiny::renderTable({
    tr <- g10_track_v4()
    g <- tryCatch(get("g10_store")(), error = function(e) NULL)
    fv <- NULL
    if (!is.null(g)) fv <- g$forecast_vol[match(tr$sel$symbol, g$symbol)]
    if (is.null(fv) || all(!is.finite(fv))) fv <- tr$sel$vol
    bad <- !is.finite(fv)
    fv[bad] <- tr$sel$vol[bad]
    
    s <- fv / sqrt(252) * sqrt(21)
    stop_px <- tr$buy * (1 - input$g10_k_stop * s)
    take_px <- tr$buy * (1 + input$g10_k_take * s)
    status <- ifelse(tr$now <= stop_px, "已觸停損",
                     ifelse(tr$now >= take_px, "已觸停利", "持有中"))
    status[is.na(status)] <- "—"
    
    data.frame(
      股票 = paste0(tr$sel$name, "（", tr$sel$symbol, "）"),
      權重 = sprintf("%.1f%%", as.numeric(tr$w) * 100),
      買進價 = ifelse(is.finite(tr$buy), sprintf("%.2f", tr$buy), "—"),
      現價 = ifelse(is.finite(tr$now), sprintf("%.2f", tr$now), "—"),
      報酬 = ifelse(is.finite(tr$ret), sprintf("%+.2f%%", tr$ret * 100), "—"),
      停損價 = ifelse(is.finite(stop_px), sprintf("%.2f", stop_px), "—"),
      停利價 = ifelse(is.finite(take_px), sprintf("%.2f", take_px), "—"),
      狀態 = status,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$g10_track_note <- shiny::renderUI({
    div(class = "method-note",
        "停損／停利距離 = k × 預測 21 日波動。波動估計優先採用 GARCH 預測值；若尚未執行全成分股估計，改用近一年實現波動。此為統計參考距離，非交易建議。")
  })
  
  
  
  
  # ============================================================
  # 時間序列檢定（畫面）
  # ============================================================

  v4ts_px <- shiny::reactive({
    s <- input$v4ts_stock
    shiny::req(!is.null(s), nzchar(s))
    d <- as.data.frame(data())
    x <- d[d$symbol == s & !is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
    x <- x[order(x$date), , drop = FALSE]
    shiny::req(nrow(x) > 300)
    as.numeric(x$adjusted)
  })

  output$v4ts_n <- shiny::renderText({
    paste0(length(v4ts_px()), " 個交易日")
  })

  v4ts_calc <- shiny::reactive({
    p <- utils::tail(v4ts_px(), 1261)
    lp <- log(p)
    r <- diff(lp)

    grid <- v4_arima_grid(r, 2, 2, 0)
    best <- NULL
    if (!is.null(grid) && nrow(grid)) {
      b <- grid[1, , drop = FALSE]
      fit <- try(stats::arima(r, order = c(b$p, b$d, b$q), method = "ML"), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        res <- as.numeric(fit$residuals)
        best <- list(p = b$p, d = b$d, q = b$q, aic = b$aic, bic = b$bic,
                     lb10 = v4_ljung(res, 10), lb20 = v4_ljung(res, 20),
                     arch = v4_arch_lm(res, 5))
      }
    }
    list(adf_p = v4_adf(lp, 4), adf_r = v4_adf(r, 4),
         grid = grid, best = best, n = length(r))
  })

  output$v4ts_diag <- shiny::renderTable({
    x <- v4ts_calc()
    f <- function(v, d = 3) ifelse(is.finite(v), sprintf(paste0("%.", d, "f"), v), "—")
    adf_txt <- function(a) {
      if (is.null(a)) return("—")
      paste0(f(a$stat), "  →  ",
             if (isTRUE(a$reject5)) "拒絕單根（定態）" else "不拒絕單根（非定態）")
    }

    rows <- c(
      "樣本（交易日）" = as.character(x$n),
      "ADF：log 價格" = adf_txt(x$adf_p),
      "ADF：日對數報酬" = adf_txt(x$adf_r),
      "最佳 ARIMA（AIC）" = if (is.null(x$best)) "—" else paste0("ARIMA(", x$best$p, ",", x$best$d, ",", x$best$q, ")"),
      "AIC / BIC" = if (is.null(x$best)) "—" else paste0(f(x$best$aic, 1), " / ", f(x$best$bic, 1)),
      "Ljung-Box Q(10)" = if (is.null(x$best) || is.null(x$best$lb10)) "—" else paste0(f(x$best$lb10$stat, 2), "（p = ", f(x$best$lb10$p, 4), "）"),
      "Ljung-Box Q(20)" = if (is.null(x$best) || is.null(x$best$lb20)) "—" else paste0(f(x$best$lb20$stat, 2), "（p = ", f(x$best$lb20$p, 4), "）"),
      "ARCH-LM(5)" = if (is.null(x$best) || is.null(x$best$arch)) "—" else paste0("F = ", f(x$best$arch$F, 2), "（p = ", f(x$best$arch$p, 4), "）")
    )
    data.frame(項目 = names(rows), 結果 = unname(rows),
               check.names = FALSE, stringsAsFactors = FALSE)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$v4ts_grid <- shiny::renderTable({
    g <- v4ts_calc()$grid
    if (is.null(g)) return(data.frame(結果 = "樣本不足"))
    g <- utils::head(g, 6)
    data.frame(
      模型 = paste0("ARIMA(", g$p, ",", g$d, ",", g$q, ")"),
      對數概似 = sprintf("%.2f", g$loglik),
      AIC = sprintf("%.2f", g$aic),
      BIC = sprintf("%.2f", g$bic),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  v4ts_persist <- shiny::eventReactive(input$v4ts_run_persist, {
    pm <- v4_pm()
    n <- nrow(pm)
    shiny::req(n > 400)
    r <- log(pm[-1, , drop = FALSE] / pm[-n, , drop = FALSE])
    dates <- as.Date(rownames(pm))[-1]
    key <- paste0(format(dates, "%Y"), "Q", ceiling(as.integer(format(dates, "%m")) / 3))
    idx <- sort(as.integer(tapply(seq_along(dates), key, max)))
    idx <- idx[idx > 300]
    withProgress(message = "計算動能持續性…", value = 0.5, {
      v4_persistence(r, idx, 126)
    })
  })

  output$v4ts_persist <- shiny::renderTable({
    x <- v4ts_persist()
    if (is.null(x)) return(data.frame(提示 = "請按上方「計算動能持續性」"))
    data.frame(
      統計量 = c("beta（持續性）", "t 值", "R 平方", "樣本數", "季數"),
      數值 = c(sprintf("%.3f", x$beta), sprintf("%.2f", x$t),
               sprintf("%.3f", x$r2), as.character(x$n), as.character(x$quarters)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # ============================================================
  # v4 季度回測（取代舊四因子版本）
  # ============================================================

  v4_backtest <- function(df, cost = P10_COST, cap = 0.20, hold = 63, n = 10,
                          scheme = "inv_vol", progress = NULL) {
    d <- as.data.frame(df)
    d <- d[d$symbol %in% c("0050", stock_info$symbol), , drop = FALSE]
    d <- d[!is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
    d <- d[order(d$date), , drop = FALSE]

    w <- as.data.frame(tidyr::pivot_wider(d[, c("date", "symbol", "adjusted")],
                                          names_from = symbol, values_from = adjusted))
    dates <- as.Date(w$date)
    pm <- as.matrix(w[, -1, drop = FALSE])
    rownames(pm) <- as.character(dates)
    storage.mode(pm) <- "double"

    key <- paste0(format(dates, "%Y"), "Q", ceiling(as.integer(format(dates, "%m")) / 3))
    idx <- sort(as.integer(tapply(seq_along(dates), key, max)))
    idx <- idx[idx > 300 & idx < (length(dates) - hold)]
    if (length(idx) < 4) return(NULL)

    rec <- list()
    prev_w <- NULL
    prev_ret <- NULL
    eq <- 1
    eq0 <- 1

    for (k in seq_along(idx)) {
      i <- idx[k]
      if (!is.null(progress)) progress(k, length(idx), NA)

      tt <- try(v4_score_table(d[d$date <= dates[i], , drop = FALSE]), silent = TRUE)
      if (inherits(tt, "try-error") || is.null(tt) || nrow(tt) < n) next

      sel <- v4_pick(tt, n)
      if (nrow(sel) < 3) next
      ww <- v4_weights(sel, scheme = scheme, cap = cap)

      j <- min(i + hold, nrow(pm))
      fwd <- exp(colSums(log(pm[(i + 1):j, , drop = FALSE] /
                              pm[i:(j - 1), , drop = FALSE]), na.rm = TRUE)) - 1
      gross <- sum(ww * fwd[names(ww)], na.rm = TRUE)

      if (is.null(prev_w)) {
        turnover <- 1
        cost_rate <- cost$fee + cost$slip
      } else {
        drift <- prev_w * (1 + prev_ret[names(prev_w)])
        drift[!is.finite(drift)] <- 0
        if (sum(drift) > 0) drift <- drift / sum(drift)
        all_sym <- union(names(prev_w), names(ww))
        a <- ifelse(all_sym %in% names(drift), drift[all_sym], 0)
        b <- ifelse(all_sym %in% names(ww), ww[all_sym], 0)
        turnover <- sum(abs(b - a), na.rm = TRUE) / 2
        cost_rate <- turnover * (2 * cost$fee + 2 * cost$slip + cost$tax)
      }

      net <- (1 + gross) * (1 - cost_rate) - 1
      bench <- fwd["0050"]
      if (!is.finite(bench)) bench <- 0

      eq <- eq * (1 + net)
      eq0 <- eq0 * (1 + as.numeric(bench))

      rec[[length(rec) + 1]] <- data.frame(
        date = dates[i], turnover = turnover, cost_rate = cost_rate,
        gross = gross, net = net, bench = as.numeric(bench),
        equity = eq, equity_0050 = eq0,
        top = paste(paste0(sel$symbol, "(", round(as.numeric(ww) * 100, 1), "%)"), collapse = " "),
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

    list(quarters = q, stats = list(
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
    ))
  }

  g10_bt <- shiny::eventReactive(input$g10_run_bt, {
    cap <- input$g10_cap
    if (is.null(cap) || !is.finite(cap)) cap <- 0.20
    sch <- input$g10_scheme
    if (is.null(sch)) sch <- "inv_vol"
    withProgress(message = "季度回測中…", value = 0, {
      v4_backtest(as.data.frame(data()), cost = P10_COST, cap = cap,
                  hold = 63, n = 10, scheme = sch,
                  progress = function(k, nn, s) {
                    setProgress(k / nn, detail = paste0("第 ", k, " / ", nn, " 季"))
                  })
    })
  })


  # ==========================================================
  # 11. 結束 Server
  # ==========================================================
  
}


# ============================================================
# 12. 啟動 Shiny
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
