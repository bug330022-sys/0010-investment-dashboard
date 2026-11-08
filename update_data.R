# ============================================================
# 0050 Investment Research Dashboard
# update_data.R
#
# 用途：
# 1. 讀取 data/initial_data.rds
# 2. 第一次執行時建立 data/stock_data.rds
# 3. 後續執行時只抓最新一段資料
# 4. 自動合併新舊資料
# 5. 移除重複的 股票 + 日期
# 6. 輸出 Dashboard 使用的 CSV
# 7. 建立資料更新資訊 data_meta.json
#
# 注意：
# initial_data.rds 永遠不修改。
# ============================================================


# ============================================================
# 1. 載入套件
# ============================================================

required_packages <- c(
  "dplyr",
  "purrr",
  "stringr",
  "readr",
  "tidyquant",
  "lubridate"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  
  stop(
    paste0(
      "缺少以下 R 套件：\n",
      paste(missing_packages, collapse = ", "),
      "\n\n請在 RStudio Console 執行：\n\n",
      "install.packages(c(",
      paste(
        shQuote(missing_packages),
        collapse = ", "
      ),
      "))"
    )
  )
}


library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tidyquant)
library(lubridate)


# ============================================================
# 2. 設定專案路徑
# ============================================================

PROJECT_DIR <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

DATA_DIR <- file.path(
  PROJECT_DIR,
  "data"
)

DASHBOARD_DIR <- file.path(
  PROJECT_DIR,
  "dashboard",
  "data"
)

dir.create(
  DATA_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  DASHBOARD_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. 設定檔案
# ============================================================

INITIAL_FILE <- file.path(
  DATA_DIR,
  "initial_data.rds"
)

CURRENT_FILE <- file.path(
  DATA_DIR,
  "stock_data.rds"
)

DASHBOARD_FILE <- file.path(
  DASHBOARD_DIR,
  "stock_data.csv"
)

META_FILE <- file.path(
  DATA_DIR,
  "data_meta.json"
)


# ============================================================
# 4. 確認初始資料存在
# ============================================================

if (!file.exists(INITIAL_FILE)) {
  
  stop(
    paste0(
      "找不到初始資料：\n",
      INITIAL_FILE,
      "\n\n",
      "請確認 data/initial_data.rds 存在。"
    )
  )
}


# ============================================================
# 5. 第一次執行 or 後續更新
# ============================================================

if (!file.exists(CURRENT_FILE)) {
  
  message(
    "第一次執行 update_data.R"
  )
  
  message(
    "建立 stock_data.rds ..."
  )
  
  current_data <- readRDS(
    INITIAL_FILE
  )
  
} else {
  
  message(
    "讀取目前 stock_data.rds ..."
  )
  
  current_data <- readRDS(
    CURRENT_FILE
  )
}


# ============================================================
# 6. 資料格式檢查
# ============================================================

required_columns <- c(
  "symbol",
  "date",
  "open",
  "high",
  "low",
  "close",
  "volume",
  "adjusted"
)

missing_columns <- setdiff(
  required_columns,
  names(current_data)
)

if (length(missing_columns) > 0) {
  
  stop(
    paste0(
      "資料缺少必要欄位：\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 7. 整理既有資料
# ============================================================

current_data <- current_data %>%
  
  mutate(
    symbol = as.character(symbol),
    date = as.Date(date)
  ) %>%
  
  filter(
    !is.na(symbol),
    !is.na(date)
  )


# ============================================================
# 8. 股票代號整理
# ============================================================

stock_codes <- current_data %>%
  
  pull(symbol) %>%
  
  str_extract(
    "[0-9]{4}"
  ) %>%
  
  unique() %>%
  
  sort()


# 確保 0050 存在
if (!"0050" %in% stock_codes) {
  
  stop(
    "目前資料中找不到 0050。"
  )
}


message(
  "\n目前股票數：",
  length(stock_codes)
)

message(
  "股票代號：",
  paste(
    stock_codes,
    collapse = ", "
  )
)


# ============================================================
# 9. 判斷目前資料最新日期
# ============================================================

latest_existing_date <- max(
  current_data$date,
  na.rm = TRUE
)

message(
  "\n目前資料最新日期：",
  format(
    latest_existing_date,
    "%Y-%m-%d"
  )
)


# ============================================================
# 10. 設定下載區間
# ============================================================
#
# 不需要每天重新抓 10 年資料。
#
# 每次只重新下載最近 10 天。
#
# 這樣可以涵蓋：
# - 週末
# - 國定假日
# - Yahoo 資料延遲
# - 漏掉的交易日
# ============================================================

download_from <- latest_existing_date -
  days(10)

download_to <- Sys.Date() + days(1)


message(
  "本次下載期間：",
  format(download_from, "%Y-%m-%d"),
  " ~ ",
  format(download_to, "%Y-%m-%d")
)


# ============================================================
# 11. Yahoo Finance 下載函數
# ============================================================

download_yahoo_stock <- function(stock_code) {
  
  yahoo_symbol <- paste0(
    stock_code,
    ".TW"
  )
  
  message(
    "Yahoo：",
    yahoo_symbol
  )
  
  tryCatch({
    
    result <- tidyquant::tq_get(
      yahoo_symbol,
      get = "stock.prices",
      from = download_from,
      to = download_to
    )
    
    if (
      is.null(result) ||
      nrow(result) == 0
    ) {
      return(NULL)
    }
    
    result %>%
      transmute(
        symbol = stock_code,
        date = as.Date(date),
        open = as.numeric(open),
        high = as.numeric(high),
        low = as.numeric(low),
        close = as.numeric(close),
        volume = as.numeric(volume),
        adjusted = as.numeric(adjusted)
      ) %>%
      filter(
        !is.na(date),
        !is.na(close)
      )
    
  }, error = function(e) {
    
    warning(
      paste0(
        "Yahoo 下載失敗：",
        stock_code,
        " | ",
        conditionMessage(e)
      )
    )
    
    NULL
  })
}


# ============================================================
# 12. TWSE 官方資料下載
# ============================================================

download_twse_month <- function(
    stock_code,
    target_date
) {
  
  target_date <- as.Date(target_date)
  
  query_date <- format(
    target_date,
    "%Y%m01"
  )
  
  url <- paste0(
    "https://www.twse.com.tw/rwd/zh/afterTrading/STOCK_DAY",
    "?date=",
    query_date,
    "&response=json",
    "&stockNo=",
    stock_code
  )
  
  message(
    "TWSE 備援：",
    stock_code,
    " / ",
    format(target_date, "%Y-%m")
  )
  
  tryCatch({
    
    response <- httr2::request(url) %>%
      httr2::req_timeout(30) %>%
      httr2::req_perform()
    
    result <- httr2::resp_body_json(
      response,
      simplifyVector = TRUE
    )
    
    if (
      is.null(result$data) ||
      length(result$data) == 0
    ) {
      return(NULL)
    }
    
    raw <- as.data.frame(
      result$data,
      stringsAsFactors = FALSE
    )
    
    if (ncol(raw) < 7) {
      return(NULL)
    }
    
    # --------------------------------------------------------
    # 民國日期轉西元日期
    # 例如 115/09/23 → 2026-09-23
    # --------------------------------------------------------
    
    convert_twse_date <- function(x) {
      
      parts <- strsplit(
        x,
        "/",
        fixed = TRUE
      )[[1]]
      
      if (length(parts) != 3) {
        return(as.Date(NA))
      }
      
      yyyy <- as.integer(parts[1]) + 1911
      mm <- as.integer(parts[2])
      dd <- as.integer(parts[3])
      
      as.Date(
        sprintf(
          "%04d-%02d-%02d",
          yyyy,
          mm,
          dd
        )
      )
    }
    
    
    twse_data <- tibble(
      
      symbol =
        stock_code,
      
      date =
        as.Date(
          vapply(
            raw[[1]],
            convert_twse_date,
            FUN.VALUE = as.Date(NA)
          )
        ),
      
      open =
        as.numeric(
          gsub(
            ",",
            "",
            raw[[4]]
          )
        ),
      
      high =
        as.numeric(
          gsub(
            ",",
            "",
            raw[[5]]
          )
        ),
      
      low =
        as.numeric(
          gsub(
            ",",
            "",
            raw[[6]]
          )
        ),
      
      close =
        as.numeric(
          gsub(
            ",",
            "",
            raw[[7]]
          )
        ),
      
      volume =
        as.numeric(
          gsub(
            ",",
            "",
            raw[[2]]
          )
        )
    ) %>%
      
      filter(
        !is.na(date),
        !is.na(close)
      )
    
    
    # --------------------------------------------------------
    # 只留下需要的日期
    # --------------------------------------------------------
    
    twse_data %>%
      filter(
        date == target_date
      )
    
    
  }, error = function(e) {
    
    warning(
      paste0(
        "TWSE 備援失敗：",
        stock_code,
        " / ",
        format(target_date, "%Y-%m-%d"),
        " | ",
        conditionMessage(e)
      )
    )
    
    NULL
  })
}

# ============================================================
# 13. 建立 Yahoo + TWSE 備援機制
# ============================================================

download_one_stock <- function(stock_code) {
  
  # ----------------------------------------------------------
  # 先抓 Yahoo
  # ----------------------------------------------------------
  
  yahoo_data <- download_yahoo_stock(
    stock_code
  )
  
  
  # ----------------------------------------------------------
  # 找出 Yahoo 沒有有效資料的日期
  #
  # 目前先針對已知的最近交易區間檢查。
  # ----------------------------------------------------------
  
  if (
    is.null(yahoo_data) ||
    nrow(yahoo_data) == 0
  ) {
    
    warning(
      paste0(
        stock_code,
        " Yahoo 沒有有效資料，嘗試 TWSE。"
      )
    )
    
    # 如果整段 Yahoo 都失敗，
    # 先用目前下載期間的每一天逐日檢查 TWSE。
    
    possible_dates <- seq.Date(
      download_from,
      as.Date(Sys.Date()),
      by = "day"
    )
    
    twse_result <- purrr::map_dfr(
      possible_dates,
      ~ download_twse_month(
        stock_code,
        .x
      )
    )
    
    if (
      nrow(twse_result) == 0
    ) {
      return(NULL)
    }
    
    twse_result <- twse_result %>%
      mutate(
        adjusted = NA_real_
      )
    
    return(
      twse_result
    )
  }
  
  
  # ----------------------------------------------------------
  # 找 Yahoo 中缺失 / 無效的日期
  # ----------------------------------------------------------
  
  expected_dates <- seq.Date(
    min(download_from, min(yahoo_data$date)),
    max(yahoo_data$date),
    by = "day"
  )
  
  
  # 台股週六日不是交易日，
  # 因此我們不把所有缺日期都當成異常。
  
  yahoo_dates <- unique(
    yahoo_data$date
  )
  
  
  # ----------------------------------------------------------
  # 目前已知 0050 的 2026-09-23 問題，
  # 用 TWSE 做官方備援。
  # ----------------------------------------------------------
  
  target_dates <- as.Date(
    character()
  )
  
  
  if (stock_code == "0050") {
    
    target_dates <- as.Date(
      "2026-09-23"
    )
    
    target_dates <- target_dates[
      target_dates >= download_from &
        target_dates <= Sys.Date()
    ]
  }
  
  
  # ----------------------------------------------------------
  # 檢查這些日期 Yahoo 是否有有效資料
  # ----------------------------------------------------------
  
  missing_dates <- target_dates[
    !target_dates %in% yahoo_dates
  ]
  
  
  # ----------------------------------------------------------
  # TWSE 補資料
  # ----------------------------------------------------------
  
  twse_data <- purrr::map_dfr(
    
    missing_dates,
    
    function(d) {
      
      download_twse_month(
        stock_code,
        d
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # TWSE 沒有補到
  # ----------------------------------------------------------
  
  if (
    nrow(twse_data) == 0
  ) {
    
    return(
      yahoo_data
    )
  }
  
  
  # ----------------------------------------------------------
  # 找附近 Yahoo 的 adjusted / close 比例
  #
  # 用來估計 TWSE 補入資料的 adjusted。
  #
  # 因為 TWSE 提供收盤價，但不直接提供 Yahoo 定義的
  # adjusted 欄位。
  # ----------------------------------------------------------
  
  ratio_data <- yahoo_data %>%
    
    filter(
      !is.na(close),
      !is.na(adjusted),
      close != 0
    ) %>%
    
    mutate(
      adjustment_ratio =
        adjusted / close
    )
  
  
  if (
    nrow(ratio_data) > 0
  ) {
    
    adjustment_ratio <- median(
      ratio_data$adjustment_ratio,
      na.rm = TRUE
    )
    
  } else {
    
    adjustment_ratio <- 1
  }
  
  
  twse_data <- twse_data %>%
    
    mutate(
      adjusted =
        close * adjustment_ratio
    )
  
  
  # ----------------------------------------------------------
  # 合併 Yahoo + TWSE
  # ----------------------------------------------------------
  
  bind_rows(
    yahoo_data,
    twse_data
  ) %>%
    
    arrange(
      date
    ) %>%
    
    distinct(
      symbol,
      date,
      .keep_all = TRUE
    )
}

# ============================================================
# 12. 下載所有股票
# ============================================================

new_data <- purrr::map_dfr(
  stock_codes,
  download_one_stock
)


# ============================================================
# 13. 檢查新資料
# ============================================================

if (nrow(new_data) == 0) {
  
  stop(
    "這次沒有取得任何新資料，因此不會修改原有資料。"
  )
}


# 必須有 0050
if (!"0050" %in% new_data$symbol) {
  
  stop(
    "本次沒有成功取得 0050，為避免錯誤更新，停止執行。"
  )
}


# ============================================================
# 14. 合併舊資料 + 新資料
# ============================================================

updated_data <- bind_rows(
  current_data,
  new_data
)


# ============================================================
# 15. 移除重複資料
# ============================================================
#
# 同一股票 + 同一天只能保留一筆。
#
# 新下載資料放在後面，
# 因此 distinct(..., .keep_all = TRUE)
# 配合先 arrange 可確保最新下載資料保留。
# ============================================================

updated_data <- updated_data %>%
  
  arrange(
    symbol,
    date
  ) %>%
  
  distinct(
    symbol,
    date,
    .keep_all = TRUE
  )


# ============================================================
# 16. 最終排序
# ============================================================

updated_data <- updated_data %>%
  
  arrange(
    date,
    symbol
  ) %>%
  
  select(
    symbol,
    date,
    open,
    high,
    low,
    close,
    volume,
    adjusted
  )


# ============================================================
# 17. 最終資料檢查
# ============================================================

final_latest_date <- max(
  updated_data$date,
  na.rm = TRUE
)

# ============================================================
# 檢查 0050 最近資料是否完整
# ============================================================

check_0050 <- updated_data %>%
  filter(
    symbol == "0050",
    date >= as.Date("2026-09-21")
  ) %>%
  arrange(date)

cat("\n")
cat("0050 最近交易資料檢查：\n")

print(check_0050)


final_earliest_date <- min(
  updated_data$date,
  na.rm = TRUE
)

final_stock_count <- n_distinct(
  updated_data$symbol
)

final_row_count <- nrow(
  updated_data
)


# 檢查 0050
stock_0050 <- updated_data %>%
  filter(
    symbol == "0050"
  )

if (nrow(stock_0050) == 0) {
  
  stop(
    "更新後資料中沒有 0050。"
  )
}


# ============================================================
# 18. 儲存正式資料
# ============================================================

saveRDS(
  updated_data,
  CURRENT_FILE
)


# ============================================================
# 19. 輸出 Dashboard CSV
# ============================================================

write_csv(
  updated_data,
  DASHBOARD_FILE
)


# ============================================================
# 20. 建立資料更新資訊
# ============================================================

update_time <- format(
  Sys.time(),
  tz = "Asia/Taipei",
  format = "%Y-%m-%d %H:%M:%S"
)


metadata <- list(
  
  project =
    "0050 Investment Research Dashboard",
  
  status =
    "success",
  
  latest_date =
    format(
      final_latest_date,
      "%Y-%m-%d"
    ),
  
  earliest_date =
    format(
      final_earliest_date,
      "%Y-%m-%d"
    ),
  
  updated_at =
    update_time,
  
  stock_count =
    final_stock_count,
  
  row_count =
    final_row_count,
  
  source =
    "Yahoo Finance",
  
  official_market_source =
    "TWSE OpenAPI",
  
  update_type =
    "incremental",
  
  update_from =
    format(
      download_from,
      "%Y-%m-%d"
    )
)


# ============================================================
# 21. 儲存 metadata
# ============================================================

jsonlite::write_json(
  metadata,
  META_FILE,
  auto_unbox = TRUE,
  pretty = TRUE
)


# ============================================================
# 22. 完成訊息
# ============================================================

cat("\n")
cat("==================================================\n")
cat("0050 Investment Dashboard\n")
cat("資料更新完成\n")
cat("==================================================\n")

cat(
  "資料起始日：",
  format(
    final_earliest_date,
    "%Y-%m-%d"
  ),
  "\n"
)

cat(
  "最新交易日：",
  format(
    final_latest_date,
    "%Y-%m-%d"
  ),
  "\n"
)

cat(
  "股票數量：",
  final_stock_count,
  "\n"
)

cat(
  "資料筆數：",
  final_row_count,
  "\n"
)

cat(
  "更新時間：",
  update_time,
  "\n"
)

cat("==================================================\n")



getwd()