# ============================================================
# garch_model.R
# GARCH(1,1) 以最大概似法自行估計（只用 base R，不需 rugarch）
# 內部一律以「百分比日對數報酬」計算，輸出再換回小數
# ============================================================

GARCH_TD <- 252

# ------------------------------------------------------------
# 1. 概似函數
#    sigma2_t = omega + alpha * r2_{t-1} + beta * sigma2_{t-1}
#    參數化：omega = exp(t1)
#            rho   = alpha + beta = plogis(t2)   （恆 < 1，不會撞界）
#            s     = alpha / rho = plogis(t3)
#            alpha = rho * s,  beta = rho * (1 - s)
# ------------------------------------------------------------

garch11_negll_fun <- function(r, burn = 20) {
  n <- length(r)
  r2 <- r^2
  v0 <- mean(r2)
  x_lag <- c(v0, r2[-n])

  function(par) {
    w   <- exp(par[1])
    rho <- stats::plogis(par[2])
    s   <- stats::plogis(par[3])
    a   <- rho * s
    b   <- rho * (1 - s)

    if (!is.finite(w) || w <= 0 || !is.finite(a) || !is.finite(b)) return(1e10)

    v <- as.numeric(stats::filter(a * x_lag + w, b, method = "recursive", init = v0))
    if (any(!is.finite(v)) || any(v <= 1e-10)) return(1e10)

    idx <- (burn + 1):n
    sum(0.5 * (log(2 * pi) + log(v[idx]) + r2[idx] / v[idx]))
  }
}

# ------------------------------------------------------------
# 2. 估計
# ------------------------------------------------------------

garch11_fit <- function(r_pct, maxit = 2000, burn = 20) {
  r <- as.numeric(r_pct)
  r <- r[is.finite(r)]
  n <- length(r)
  if (n < 250) return(NULL)

  v0 <- mean(r^2)
  if (!is.finite(v0) || v0 <= 0) return(NULL)

  fn <- garch11_negll_fun(r, burn)

  starts <- list(
    c(log(v0 * 0.05), stats::qlogis(0.95), stats::qlogis(0.08)),
    c(log(v0 * 0.05), stats::qlogis(0.97), stats::qlogis(0.06)),
    c(log(v0 * 0.05), stats::qlogis(0.90), stats::qlogis(0.12)),
    c(log(v0 * 0.02), stats::qlogis(0.99), stats::qlogis(0.05))
  )

  best <- NULL
  for (p0 in starts) {
    o <- try(stats::optim(p0, fn, method = "Nelder-Mead",
                          control = list(maxit = maxit, reltol = 1e-10)),
             silent = TRUE)
    if (!inherits(o, "try-error") && is.finite(o$value) && o$value < 1e9) {
      if (is.null(best) || o$value < best$value) best <- o
    }
  }
  if (is.null(best)) return(NULL)

  par <- best$par
  w   <- exp(par[1])
  rho <- stats::plogis(par[2])
  s   <- stats::plogis(par[3])
  a   <- rho * s
  b   <- rho * (1 - s)

  r2 <- r^2
  x_lag <- c(v0, r2[-n])
  v <- as.numeric(stats::filter(a * x_lag + w, b, method = "recursive", init = v0))

  # ---- 標準誤：observed information + delta method ----
  se <- c(NA_real_, NA_real_, NA_real_)
  H <- try(stats::optimHess(par, fn), silent = TRUE)
  if (!inherits(H, "try-error")) {
    V <- try(solve(H), silent = TRUE)
    if (!inherits(V, "try-error") && all(is.finite(V))) {
      J <- matrix(0, 3, 3)
      J[1, 1] <- w
      J[2, 2] <- s * rho * (1 - rho)
      J[2, 3] <- rho * s * (1 - s)
      J[3, 2] <- (1 - s) * rho * (1 - rho)
      J[3, 3] <- -rho * s * (1 - s)
      Vp <- J %*% V %*% t(J)
      d <- diag(Vp)
      if (all(is.finite(d)) && all(d >= 0)) se <- sqrt(d)
    }
  }

  # ---- 殘差診斷：標準化殘差的 Ljung-Box Q(10) ----
  z <- r / sqrt(v)
  q <- try(stats::Box.test(z, lag = 10, type = "Ljung-Box"), silent = TRUE)

  list(
    omega = w, alpha = a, beta = b,
    persistence = rho, alpha_share = s,
    se_omega = se[1], se_alpha = se[2], se_beta = se[3],
    t_omega = if (is.finite(se[1]) && se[1] > 0) w / se[1] else NA_real_,
    t_alpha = if (is.finite(se[2]) && se[2] > 0) a / se[2] else NA_real_,
    t_beta  = if (is.finite(se[3]) && se[3] > 0) b / se[3] else NA_real_,
    half_life = if (rho < 1) log(0.5) / log(rho) else Inf,
    uncond_vol = sqrt(w / (1 - rho) * GARCH_TD) / 100,
    loglik = -best$value,
    aic = 2 * 3 + 2 * best$value,
    bic = 3 * log(n) + 2 * best$value,
    n = n,
    converged = isTRUE(best$convergence == 0),
    sigma2 = v,
    sigma2_last = v[n],
    lb_stat = if (inherits(q, "try-error")) NA_real_ else as.numeric(q$statistic),
    lb_p    = if (inherits(q, "try-error")) NA_real_ else as.numeric(q$p.value)
  )
}

# ------------------------------------------------------------
# 3. 預測
# ------------------------------------------------------------

garch11_forecast <- function(fit, h = 63) {
  v <- fit$sigma2_last
  rho <- fit$persistence
  out <- numeric(h)
  for (i in seq_len(h)) {
    v <- fit$omega + rho * v
    out[i] <- v
  }
  out
}

# 未來 h 日的年化波動、1 日 VaR / CVaR（常態假設）
garch11_summary <- function(fit, h = 63) {
  fv <- garch11_forecast(fit, h)
  s1 <- sqrt(fit$sigma2_last)
  list(
    forecast_vol = sqrt(mean(fv) * GARCH_TD) / 100,
    next_vol = sqrt(fv[1] * GARCH_TD) / 100,
    var95 = 1.6449 * s1 / 100,
    cvar95 = 2.0627 * s1 / 100
  )
}

# ------------------------------------------------------------
# 4. EWMA 對照組（RiskMetrics，lambda = 0.94）
# ------------------------------------------------------------

ewma_vol <- function(r_pct, lambda = 0.94) {
  r <- as.numeric(r_pct)
  r <- r[is.finite(r)]
  if (length(r) < 30) return(NA_real_)
  v <- mean(r^2)
  for (x in r) v <- lambda * v + (1 - lambda) * x^2
  sqrt(v * GARCH_TD) / 100
}

# ------------------------------------------------------------
# 5. 單檔完整分析（給 UI 用）
# ------------------------------------------------------------

garch_analyse_one <- function(px, window = 1000, horizon = 63) {
  px <- as.numeric(px)
  px <- px[is.finite(px) & px > 0]
  if (length(px) < 260) return(NULL)

  r_all <- 100 * diff(log(px))
  r <- if (length(r_all) > window) r_all[(length(r_all) - window + 1):length(r_all)] else r_all

  fit <- garch11_fit(r)
  if (is.null(fit)) return(NULL)

  sm <- garch11_summary(fit, horizon)

  tail_n <- min(252, length(r))
  rvol <- sqrt(mean(r[(length(r) - tail_n + 1):length(r)]^2) * GARCH_TD) / 100
  ew <- ewma_vol(r)

  ratio <- if (is.finite(rvol) && rvol > 0) sm$forecast_vol / rvol else NA_real_

  list(
    fit = fit, summary = sm, returns = r,
    ratio = ratio, real_vol = rvol, ewma_vol = ew,
    regime = if (!is.finite(ratio)) "無資料" else
      if (ratio >= 1.10) "預期波動上升" else
        if (ratio <= 0.90) "預期波動下降" else "預期波動持平"
  )
}
