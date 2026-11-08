# ============================================================
# v5_server.R  （由 app.R 內以 source(..., local = TRUE) 載入）
# 新功能：v4 季度回測、交易明細、準確度、GARCH 診斷與樣本外評估
# 依賴：v5_backtest.R、v5_garch_diag.R、v4_score.R、v4_ts.R
# ============================================================

  # ----------------------------------------------------------
  # 1. 季度回測（v4 分數 + T+1 成交）
  # ----------------------------------------------------------

  v5_bt <- shiny::eventReactive(input$v5_run_bt, {
    d <- as.data.frame(data())
    withProgress(message = "季度回測（T+1 成交）…", value = 0, {
      v5_backtest_v4(
        d, hold = 63, n = 10, cap = 0.20, capital = 1000000,
        t1 = TRUE, start_after = 300,
        progress = function(k, n) {
          setProgress(k / n, detail = paste0("第 ", k, " / ", n, " 季"))
        }
      )
    })
  }, ignoreInit = TRUE)

  output$v5_bt_acc <- shiny::renderTable({
    r <- v5_bt()
    shiny::req(!is.null(r))
    s <- r$stats

    data.frame(
      指標 = c("平均 IC（分數與未來報酬的 Spearman）",
              "平均命中率（選進 10 檔、實際也進前 10 的比例）",
              "季勝率（淨報酬贏過 0050 的比例）",
              "平均 Spread（Top10 − Bottom10 未來報酬）"),
      數值 = c(
        ifelse(is.finite(s$ic), sprintf("%.3f", s$ic), "—"),
        ifelse(is.finite(s$hit), sprintf("%.1f%%", s$hit * 100), "—"),
        ifelse(is.finite(s$win), sprintf("%.1f%%", s$win * 100), "—"),
        ifelse(is.finite(s$spread), sprintf("%+.2f%%", s$spread * 100), "—")
      ),
      對照基準 = c("0（無排序能力）", "20%（隨機選 10 檔的期望值）", "50%", "0"),
      判讀 = c(
        ifelse(!is.finite(s$ic), "—",
               ifelse(s$ic >= 0.05, "有正向排序能力",
                      ifelse(s$ic > 0, "弱正向", "無明顯能力"))),
        ifelse(!is.finite(s$hit), "—",
               ifelse(s$hit > 0.30, "明顯高於隨機",
                      ifelse(s$hit > 0.20, "略高於隨機", "接近隨機"))),
        ifelse(s$win > 0.5, "多數季度贏大盤", "未過半"),
        ifelse(is.finite(s$spread) && s$spread > 0, "高分組確實較好", "低分組反而較好")
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$v5_bt_stats <- shiny::renderTable({
    r <- v5_bt()
    shiny::req(!is.null(r))
    s <- r$stats
    data.frame(
      項目 = c("回測季數", "累積報酬（扣成本）", "年化報酬", "年化波動",
              "Sharpe", "最大回撤", "0050 同期累積", "0050 年化",
              "平均換手率", "累計交易成本"),
      數值 = c(
        as.character(s$n),
        sprintf("%+.2f%%", s$cum * 100),
        sprintf("%+.2f%%", s$ann * 100),
        sprintf("%.2f%%", s$vol * 100),
        ifelse(is.finite(s$sharpe), sprintf("%.2f", s$sharpe), "—"),
        sprintf("%.2f%%", s$mdd * 100),
        sprintf("%+.2f%%", s$bench_cum * 100),
        sprintf("%+.2f%%", s$bench_ann * 100),
        sprintf("%.1f%%", s$avg_turnover * 100),
        formatC(round(s$total_cost), format = "d", big.mark = ",")
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$v5_bt_curve <- shiny::renderPlot({
    r <- v5_bt()
    shiny::req(!is.null(r))
    q <- r$quarters
    df <- rbind(
      data.frame(date = q$成交日, value = q$累積淨值, series = "0010（扣成本）"),
      data.frame(date = q$成交日, value = q$累積0050, series = "0050")
    )
    ggplot2::ggplot(df, ggplot2::aes(x = date, y = value, colour = series)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::labs(title = "季度再平衡淨值（T+1 成交）", x = NULL,
                    y = "淨值（起始 = 1）", colour = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  output$v5_bt_log <- shiny::renderTable({
    r <- v5_bt()
    shiny::req(!is.null(r))
    q <- r$quarters
    data.frame(
      評分日 = format(q$評分日, "%Y-%m-%d"),
      成交日 = format(q$成交日, "%Y-%m-%d"),
      出場日 = format(q$出場日, "%Y-%m-%d"),
      換手率 = sprintf("%.1f%%", q$換手率 * 100),
      買進金額 = formatC(round(q$買進金額), format = "d", big.mark = ","),
      賣出金額 = formatC(round(q$賣出金額), format = "d", big.mark = ","),
      手續費 = formatC(round(q$手續費), format = "d", big.mark = ","),
      證交稅 = formatC(round(q$證交稅), format = "d", big.mark = ","),
      滑價 = formatC(round(q$滑價), format = "d", big.mark = ","),
      成本合計 = formatC(round(q$成本合計), format = "d", big.mark = ","),
      毛報酬 = sprintf("%+.2f%%", q$毛報酬 * 100),
      淨報酬 = sprintf("%+.2f%%", q$淨報酬 * 100),
      `0050` = sprintf("%+.2f%%", q$基準0050 * 100),
      持股 = q$持股,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$v5_bt_csv <- shiny::downloadHandler(
    filename = function() paste0("0010_trades_", Sys.Date(), ".csv"),
    content = function(file) {
      r <- v5_bt()
      shiny::req(!is.null(r))
      con <- file(file, "wb")
      writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
      utils::write.csv(r$quarters, con, row.names = FALSE)
      close(con)
    }
  )

  # ----------------------------------------------------------
  # 2. GARCH 全體收斂與失敗清單
  # ----------------------------------------------------------

  v5_gall <- shiny::eventReactive(input$v5_run_gall, {
    d <- as.data.frame(data())
    win <- input$g10_window
    if (is.null(win) || !is.finite(win)) win <- 1000
    withProgress(message = "GARCH 全體診斷…", value = 0, {
      v5_garch_all(
        d, stock_info$symbol,
        window = as.integer(win),
        progress = function(k, n, s) {
          setProgress(k / n, detail = paste0("第 ", k, " / ", n, " 檔：", s))
        }
      )
    })
  }, ignoreInit = TRUE)

  output$v5_gall_summary <- shiny::renderTable({
    g <- v5_gall()
    shiny::req(!is.null(g))
    tab <- table(g$status)
    data.frame(
      狀態 = names(tab),
      檔數 = as.integer(tab),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$v5_gall_tab <- shiny::renderTable({
    g <- v5_gall()
    shiny::req(!is.null(g))
    out <- data.frame(
      股票 = paste0(g$name, "（", g$symbol, "）"),
      樣本 = g$n,
      `ω` = ifelse(is.finite(g$omega), sprintf("%.4f", g$omega), "—"),
      `α` = ifelse(is.finite(g$alpha), sprintf("%.4f", g$alpha), "—"),
      `β` = ifelse(is.finite(g$beta), sprintf("%.4f", g$beta), "—"),
      `α+β` = ifelse(is.finite(g$persistence), sprintf("%.4f", g$persistence), "—"),
      收斂 = ifelse(g$converged, "是", "否"),
      `LB p` = ifelse(is.finite(g$lb_p), sprintf("%.3f", g$lb_p), "—"),
      `ARCH p` = ifelse(is.finite(g$arch_p), sprintf("%.3f", g$arch_p), "—"),
      狀態 = g$status,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    out[order(out$狀態), , drop = FALSE]
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # ----------------------------------------------------------
  # 3. GARCH 單檔殘差診斷與樣本外評估
  # ----------------------------------------------------------

  v5_gdiag <- shiny::reactive({
    s <- input$v5_g_stock
    shiny::req(!is.null(s), nzchar(s))
    d <- as.data.frame(data())
    x <- d[d$symbol == s & !is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
    x <- x[order(x$date), , drop = FALSE]
    shiny::req(nrow(x) > 300)
    win <- input$g10_window
    if (is.null(win) || !is.finite(win)) win <- 1000
    v5_garch_diag_one(x$adjusted, window = as.integer(win))
  })

  output$v5_gdiag_tab <- shiny::renderTable({
    g <- v5_gdiag()
    shiny::req(!is.null(g))
    df <- g$table
    data.frame(
      檢定 = df$檢定,
      統計量 = ifelse(is.finite(df$統計量), sprintf("%.3f", df$統計量), "—"),
      p值 = ifelse(is.finite(df$p值), sprintf("%.4f", df$p值), "—"),
      判定 = df$判定,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  v5_goos <- shiny::reactive({
    s <- input$v5_g_stock
    shiny::req(!is.null(s), nzchar(s))
    d <- as.data.frame(data())
    x <- d[d$symbol == s & !is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
    x <- x[order(x$date), , drop = FALSE]
    shiny::req(nrow(x) > 600)
    v5_garch_oos(x$adjusted)
  })

  output$v5_goos_tab <- shiny::renderTable({
    o <- v5_goos()
    shiny::req(!is.null(o))
    data.frame(
      模型 = o$table$模型,
      RMSE = sprintf("%.4f", o$table$RMSE),
      MAE = sprintf("%.4f", o$table$MAE),
      QLIKE = sprintf("%.4f", o$table$QLIKE),
      排名 = seq_len(nrow(o$table)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$v5_goos_plot <- shiny::renderPlot({
    o <- v5_goos()
    shiny::req(!is.null(o))
    s <- o$series
    df <- rbind(
      data.frame(t = seq_len(nrow(s)), v = s$obs,   series = "實際 |報酬|"),
      data.frame(t = seq_len(nrow(s)), v = s$garch, series = "GARCH 預測"),
      data.frame(t = seq_len(nrow(s)), v = s$ewma,  series = "EWMA 預測")
    )
    ggplot2::ggplot(df, ggplot2::aes(x = t, y = v, colour = series)) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::labs(title = "樣本外波動預測比較（後 20% 資料）",
                    x = "樣本外第 N 個交易日", y = "日波動（%）", colour = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  # ----------------------------------------------------------
  # 4. 一句話結果（取代「怎麼看？」）
  # ----------------------------------------------------------

  output$v5_rank_result <- shiny::renderText({
    t <- v4_tbl()
    shiny::req(!is.null(t))
    top <- t[order(-t$p10_smooth), , drop = FALSE][1, ]
    paste0("目前 0010 分數最高的是 ", top$name, "（", top$symbol, "）",
           "，動能 ", sprintf("%.1f", top$mom_score),
           "、風險穩定 ", sprintf("%.1f", top$risk_score),
           "，合計 ", sprintf("%.1f", top$p10_smooth), " 分。")
  })

  output$v5_bt_result <- shiny::renderText({
    r <- v5_bt()
    shiny::req(!is.null(r))
    s <- r$stats
    paste0("回測 ", s$n, " 季，0010 扣成本累積 ",
           sprintf("%+.1f%%", s$cum * 100), "，0050 同期 ",
           sprintf("%+.1f%%", s$bench_cum * 100), "；季勝率 ",
           sprintf("%.0f%%", s$win * 100), "，平均 IC ",
           sprintf("%.3f", s$ic), "。")
  })

  # ==========================================================
  # v6 追加：選擇順序配色、本期區間、排名頁（動能分／風險穩定分）
  # ==========================================================

  v6_order_colors <- function(syms) {
    pal <- c("#1976d2", "#e53935", "#2e7d32", "#f9a825", "#6a1b9a", "#00838f",
             "#ad1457", "#4e342e", "#283593", "#558b2f", "#ef6c00", "#00897b")
    syms <- as.character(syms)
    cols <- rep(pal, length.out = length(syms))
    stats::setNames(cols, syms)
  }

  output$v6_period <- shiny::renderText({
    reb <- v4_reb()
    d <- as.Date(rownames(v4_pm()))
    paste0(as.character(reb$date), " ～ ", as.character(max(d, na.rm = TRUE)))
  })

  output$v6_rank_date <- shiny::renderText({
    d <- as.Date(rownames(v4_pm()))
    as.character(max(d, na.rm = TRUE))
  })

  output$v6_mom_chart <- shiny::renderPlot({
    t <- v4_tbl()
    shiny::req(!is.null(t))
    top <- utils::head(t[order(-t$mom_score), , drop = FALSE], 10)
    lab <- paste0(top$name, "（", top$symbol, "）")
    df <- data.frame(label = factor(lab, levels = rev(lab)), v = top$mom_score)

    ggplot2::ggplot(df, ggplot2::aes(x = label, y = v, fill = v)) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", v)),
                         hjust = -0.15, size = 3.5) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient(low = "#90caf9", high = "#0d47a1") +
      ggplot2::labs(title = "動能分前 10 名", x = NULL, y = "動能分（0–100）") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  output$v6_mom_result <- shiny::renderText({
    t <- v4_tbl()
    shiny::req(!is.null(t))
    top <- utils::head(t[order(-t$mom_score), , drop = FALSE], 10)
    best <- top[1, ]
    paste0("目前動能分最高的是 ", best$name, "（", best$symbol, "），",
           sprintf("%.1f", best$mom_score), " 分；前 10 名的動能分介於 ",
           sprintf("%.1f", min(top$mom_score)), " 到 ",
           sprintf("%.1f", max(top$mom_score)), " 分之間。")
  })

  output$v6_mom_table <- shiny::renderTable({
    t <- v4_tbl()
    shiny::req(!is.null(t))
    o <- t[order(-t$mom_score), , drop = FALSE]
    data.frame(
      排名 = seq_len(nrow(o)),
      股票 = paste0(o$name, "（", o$symbol, "）"),
      產業 = o$industry,
      動能分 = sprintf("%.1f", o$mom_score),
      `6個月報酬` = ifelse(is.finite(o$r6), sprintf("%+.1f%%", o$r6 * 100), "—"),
      `12-1個月報酬` = ifelse(is.finite(o$r12_1), sprintf("%+.1f%%", o$r12_1 * 100), "—"),
      `0010分` = sprintf("%.1f", o$p10_smooth),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$v6_risk_chart <- shiny::renderPlot({
    t <- v4_tbl()
    shiny::req(!is.null(t))
    top <- utils::head(t[order(-t$risk_score), , drop = FALSE], 10)
    lab <- paste0(top$name, "（", top$symbol, "）")
    df <- data.frame(label = factor(lab, levels = rev(lab)), v = top$risk_score)

    ggplot2::ggplot(df, ggplot2::aes(x = label, y = v, fill = v)) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", v)),
                         hjust = -0.15, size = 3.5) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient(low = "#ffcdd2", high = "#1565c0") +
      ggplot2::labs(title = "風險穩定分前 10 名", x = NULL, y = "風險穩定分（0–100）") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  output$v6_risk_result <- shiny::renderText({
    t <- v4_tbl()
    shiny::req(!is.null(t))
    top <- utils::head(t[order(-t$risk_score), , drop = FALSE], 10)
    best <- top[1, ]
    paste0("風險穩定分最高的是 ", best$name, "（", best$symbol, "），",
           sprintf("%.1f", best$risk_score), " 分；代表它的波動、回撤、尾端風險與夏普在 50 檔中相對可控。")
  })
