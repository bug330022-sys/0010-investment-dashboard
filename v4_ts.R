# ============================================================
# v4_ts.R
# 時間序列檢定：ADF 單根、ARIMA 選模、Ljung-Box、ARCH-LM、動能持續性
# 只用 base R（stats::lm.fit / stats::arima / stats::Box.test）
# ============================================================

# ------------------------------------------------------------
# 1. ADF 單根檢定（常數項版本，落後期 p 由使用者指定）
#    Δx_t = a + γ·x_{t-1} + Σ δ_i·Δx_{t-i} + ε_t
#    H0: γ = 0（有單根，非定態）
# ------------------------------------------------------------

v4_adf <- function(x, p = 4) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < p + 40) return(NULL)

  dx <- diff(x)
  m <- length(dx)
  if (m - p < 20) return(NULL)

  y <- dx[(p + 1):m]
  X <- cbind(rep(1, length(y)), x[(p + 1):m])
  if (p > 0) {
    for (i in seq_len(p)) X <- cbind(X, dx[(p + 1 - i):(m - i)])
  }

  fit <- stats::lm.fit(X, y)
  res <- fit$residuals
  df <- nrow(X) - ncol(X)
  if (df < 5) return(NULL)

  s2 <- sum(res^2) / df
  XtXinv <- try(solve(crossprod(X)), silent = TRUE)
  if (inherits(XtXinv, "try-error")) return(NULL)

  se <- sqrt(diag(XtXinv) * s2)
  stat <- fit$coefficients[2] / se[2]

  cv <- c(`1%` = -3.43, `5%` = -2.86, `10%` = -2.57)
  list(stat = stat, cv = cv, reject5 = isTRUE(stat < cv[2]),
       lag = p, n = length(y))
}

# ------------------------------------------------------------
# 2. ARIMA 選模（AIC / BIC 表格）
# ------------------------------------------------------------

v4_arima_grid <- function(x, p_max = 2, q_max = 2, d = 0) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 50) return(NULL)

  rows <- list()
  for (p in 0:p_max) {
    for (q in 0:q_max) {
      if (p == 0 && q == 0 && d == 0) next
      fit <- try(stats::arima(x, order = c(p, d, q), method = "ML"), silent = TRUE)
      if (inherits(fit, "try-error")) next
      k <- length(fit$coef) + 1
      rows[[length(rows) + 1]] <- data.frame(
        p = p, d = d, q = q,
        loglik = as.numeric(fit$loglik),
        aic = as.numeric(fit$aic),
        bic = as.numeric(fit$aic) + k * (log(n) - 2),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  out[order(out$aic), , drop = FALSE]
}

# ------------------------------------------------------------
# 3. Ljung-Box 殘差自我相關
# ------------------------------------------------------------

v4_ljung <- function(x, lag = 10) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < lag + 15) return(NULL)
  b <- stats::Box.test(x, lag = lag, type = "Ljung-Box")
  list(stat = as.numeric(b$statistic), p = as.numeric(b$p.value),
       df = lag, n = length(x))
}

# ------------------------------------------------------------
# 4. ARCH-LM（檢定殘差平方是否有自我相關，即波動叢聚）
# ------------------------------------------------------------

v4_arch_lm <- function(e, lag = 5) {
  e <- as.numeric(e)
  e <- e[is.finite(e)]
  n <- length(e)
  if (n < lag + 25) return(NULL)

  e2 <- e^2
  y <- e2[(lag + 1):n]
  X <- cbind(rep(1, length(y)))
  for (i in seq_len(lag)) X <- cbind(X, e2[(lag + 1 - i):(n - i)])

  fit <- stats::lm.fit(X, y)
  res <- fit$residuals
  sst <- sum((y - mean(y))^2)
  if (sst <= 0) return(NULL)

  r2 <- 1 - sum(res^2) / sst
  k <- ncol(X)
  df2 <- length(y) - k
  if (df2 < 5) return(NULL)

  F <- (r2 / lag) / ((1 - r2) / df2)
  p <- stats::pf(F, lag, df2, lower.tail = FALSE)
  list(lm = length(y) * r2, df = lag, F = F, p = p)
}

# ------------------------------------------------------------
# 5. 動能分數的持續性（panel AR(1)）
#    score_it = a + b·score_i,t-1 + u_it
#    b 明顯大於 0 代表動能分可延續 → 支持用當期分數選股＋平滑
# ------------------------------------------------------------

v4_mom_score_at <- function(ret_mat, i, look = 126) {
  j0 <- max(1, i - look + 1)
  sub <- ret_mat[j0:i, , drop = FALSE]
  cum <- exp(colSums(sub, na.rm = TRUE)) - 1
  cum[!is.finite(cum)] <- NA_real_
  v4_pct(cum, TRUE)
}

v4_persistence <- function(ret_mat, idx, look = 126) {
  sc <- lapply(idx, function(i) v4_mom_score_at(ret_mat, i, look))
  xs <- numeric(0)
  ys <- numeric(0)
  for (k in seq_along(sc)[-1]) {
    a <- as.numeric(sc[[k - 1]])
    b <- as.numeric(sc[[k]])
    ok <- is.finite(a) & is.finite(b)
    xs <- c(xs, a[ok])
    ys <- c(ys, b[ok])
  }
  n <- length(xs)
  if (n < 50) return(NULL)

  fit <- stats::lm.fit(cbind(1, xs), ys)
  res <- fit$residuals
  df <- n - 2
  s2 <- sum(res^2) / df
  XtXinv <- try(solve(crossprod(cbind(1, xs))), silent = TRUE)
  if (inherits(XtXinv, "try-error")) return(NULL)

  se <- sqrt(diag(XtXinv) * s2)
  beta <- fit$coefficients[2]
  r2 <- 1 - sum(res^2) / sum((ys - mean(ys))^2)

  list(beta = beta, t = beta / se[2], r2 = r2, n = n, quarters = length(idx))
}

# ------------------------------------------------------------
# 6. 一鍵報告：把上面幾項整理成可直接貼進研究方法頁的表
# ------------------------------------------------------------

v4_ts_report <- function(px, label = "", arima_pmax = 2, arima_qmax = 2) {
  px <- as.numeric(px)
  px <- px[is.finite(px) & px > 0]
  if (length(px) < 300) return(NULL)

  lp <- log(px)
  r <- diff(lp)

  adf_p <- v4_adf(lp, 4)
  adf_r <- v4_adf(r, 4)
  grid <- v4_arima_grid(r, arima_pmax, arima_qmax, 0)

  best <- NULL
  if (!is.null(grid) && nrow(grid)) {
    b <- grid[1, , drop = FALSE]
    fit <- try(stats::arima(r, order = c(b$p, b$d, b$q), method = "ML"), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      lb <- v4_ljung(as.numeric(fit$residuals), 10)
      lb20 <- v4_ljung(as.numeric(fit$residuals), 20)
      arch <- v4_arch_lm(as.numeric(fit$residuals), 5)
      best <- list(p = b$p, d = b$d, q = b$q, aic = b$aic, bic = b$bic,
                   lb10 = lb, lb20 = lb20, arch = arch,
                   coef = fit$coef)
    }
  }

  list(label = label, n = length(r),
       adf_price = adf_p, adf_return = adf_r,
       grid = grid, best = best)
}
