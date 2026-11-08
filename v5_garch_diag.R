# ============================================================
# v5_garch_diag.R
# GARCH 殘差診斷（平方殘差 LB、ARCH-LM）、樣本外預測評估、收斂清單
# 依賴：garch_model.R、v4_ts.R
# ============================================================

# ------------------------------------------------------------
# 1. 單檔殘差診斷
# ------------------------------------------------------------
v5_garch_diag_one <- function(px, window = 1000, horizon = 63) {
  a <- garch_analyse_one(px, window = window, horizon = horizon)
  if (is.null(a)) return(NULL)

  f <- a$fit
  z <- a$returns / sqrt(f$sigma2)
  z2 <- z^2

  lb1 <- v4_ljung(z, 10)
  lb2 <- v4_ljung(z, 20)
  lb3 <- v4_ljung(z2, 10)
  lb4 <- v4_ljung(z2, 20)
  ar  <- v4_arch_lm(z, 5)

  df <- data.frame(
    檢定 = c("標準化殘差 LB(10)", "標準化殘差 LB(20)",
             "平方殘差 LB(10)", "平方殘差 LB(20)", "ARCH-LM(5)"),
    統計量 = c(lb1$stat, lb2$stat, lb3$stat, lb4$stat, if (is.null(ar)) NA else ar$lm),
    p值 = c(lb1$p, lb2$p, lb3$p, lb4$p, if (is.null(ar)) NA else ar$p),
    stringsAsFactors = FALSE
  )
  df$判定 <- ifelse(!is.finite(df$p值), "—",
                    ifelse(df$p值 > 0.05, "通過（無殘留相關）", "未通過（仍有結構）"))

  list(fit = f, table = df, analysis = a,
       pass = all(df$p值 > 0.05, na.rm = TRUE))
}

# ------------------------------------------------------------
# 2. 樣本外預測評估
#    切前 80% 估參數，後 20% 用固定參數遞迴更新變異數
#    比較：GARCH(1,1) vs EWMA(0.94) vs 21 日滾動
# ------------------------------------------------------------
v5_garch_oos <- function(px, split = 0.8, lambda = 0.94, roll = 21) {
  px <- as.numeric(px)
  px <- px[is.finite(px) & px > 0]
  if (length(px) < 500) return(NULL)

  r <- 100 * diff(log(px))
  n <- length(r)
  cut <- floor(n * split)
  if (n - cut < 120) return(NULL)

  fit <- garch11_fit(r[1:cut])
  if (is.null(fit)) return(NULL)

  w <- fit$omega; a <- fit$alpha; b <- fit$beta
  v <- fit$sigma2_last

  m <- n - cut
  sig <- ew <- rv <- numeric(m)

  ev <- mean(r[1:cut]^2)
  for (k in seq_len(m)) {
    i <- cut + k
    v  <- w + a * r[i - 1]^2 + b * v
    ev <- lambda * ev + (1 - lambda) * r[i - 1]^2
    lo <- max(1, i - roll)
    sig[k] <- sqrt(v)
    ew[k]  <- sqrt(ev)
    rv[k]  <- sqrt(mean(r[lo:(i - 1)]^2))
  }

  obs <- abs(r[(cut + 1):n])
  out <- data.frame(obs = obs, garch = sig, ewma = ew, roll = rv)
  out <- out[is.finite(out$obs) & out$obs > 0, , drop = FALSE]
  if (!nrow(out)) return(NULL)

  qlike <- function(s, o) mean(log(s^2) + (o^2) / (s^2))
  rmse  <- function(s, o) sqrt(mean((s - o)^2))

  tab <- data.frame(
    模型 = c("GARCH(1,1)", "EWMA(0.94)", "21 日滾動"),
    RMSE = c(rmse(out$garch, out$obs), rmse(out$ewma, out$obs), rmse(out$roll, out$obs)),
    MAE  = c(mean(abs(out$garch - out$obs)), mean(abs(out$ewma - out$obs)), mean(abs(out$roll - out$obs))),
    QLIKE = c(qlike(out$garch, out$obs), qlike(out$ewma, out$obs), qlike(out$roll, out$obs)),
    stringsAsFactors = FALSE
  )
  tab <- tab[order(tab$QLIKE), , drop = FALSE]

  list(table = tab, series = out, n_test = nrow(out),
       split_date_index = cut, fit = fit)
}

# ------------------------------------------------------------
# 3. 全體收斂與失敗清單
# ------------------------------------------------------------
v5_garch_all <- function(df, symbols, window = 1000, progress = NULL) {
  d <- as.data.frame(df)
  d <- d[!is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
  d <- d[order(d$symbol, d$date), , drop = FALSE]
  sp <- split(d$adjusted, d$symbol)

  rows <- list()
  for (k in seq_along(symbols)) {
    s <- symbols[k]
    if (!is.null(progress)) progress(k, length(symbols), s)
    x <- sp[[s]]
    if (is.null(x) || length(x) < 260) {
      rows[[length(rows) + 1]] <- data.frame(
        symbol = s, n = if (is.null(x)) 0 else length(x),
        omega = NA, alpha = NA, beta = NA, persistence = NA,
        converged = FALSE, lb_p = NA, arch_p = NA,
        status = "資料不足", stringsAsFactors = FALSE)
      next
    }

    g <- v5_garch_diag_one(x, window = window)
    if (is.null(g)) {
      rows[[length(rows) + 1]] <- data.frame(
        symbol = s, n = length(x), omega = NA, alpha = NA, beta = NA,
        persistence = NA, converged = FALSE, lb_p = NA, arch_p = NA,
        status = "估計失敗", stringsAsFactors = FALSE)
      next
    }

    f <- g$fit
    lb_ps <- g$table$p值[1:4]
    lb_p <- if (all(!is.finite(lb_ps))) NA_real_ else min(lb_ps, na.rm = TRUE)
    arch_p <- g$table$p值[5]
    status <- if (!isTRUE(f$converged)) "未收斂" else
      if (isTRUE(is.finite(arch_p) && arch_p < 0.05)) "殘差仍有 ARCH 效果" else
        if (isTRUE(is.finite(lb_p) && lb_p < 0.05)) "殘差仍有自我相關" else "正常"

    rows[[length(rows) + 1]] <- data.frame(
      symbol = s, n = f$n, omega = f$omega, alpha = f$alpha, beta = f$beta,
      persistence = f$persistence, converged = isTRUE(f$converged),
      lb_p = lb_p, arch_p = arch_p, status = status, stringsAsFactors = FALSE)
  }

  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  out$name <- get_stock_name(out$symbol)
  rownames(out) <- NULL
  out
}
