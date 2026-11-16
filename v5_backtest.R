# ============================================================
# C51 | 整合 C50 末季出場邊界修正：歷史連續日淨值季度回測（測試版）
# 評分只使用評分日以前資料；下季 T+1 收盤換倉；同日完整計入買賣成本。
# 所有策略與 0050 使用相同交易日；短缺價容許至多兩個交易日的舊收盤估值；
# 必須使用真實可得價格成交，未完成季度予以截斷並保留先前完成紀錄，不事後刪股。
# 注意：adjusted 價格作為含股利調整的「成交代理價」，不是可保證成交價。
# 注意：沒有歷史 0050 成分股名單時，股票池仍是目前成分的回溯代理。
# ============================================================
V5_COST <- list(fee=0.001425, min_fee=20, tax=0.003, slip=0.001)

v5_quarter_ends <- function(dates) {
  dates <- as.Date(dates)
  key <- paste0(format(dates, "%Y"), "Q",
                ceiling(as.integer(format(dates, "%m")) / 3))
  sort(as.integer(tapply(seq_along(dates), key, max)))
}

v5_cost_one <- function(amount, side=c("buy","sell"), cost=V5_COST) {
  side <- match.arg(side)
  if (!is.finite(amount) || amount <= 1e-8)
    return(c(fee=0,tax=0,slip=0,total=0))
  fee <- max(cost$min_fee, amount*cost$fee)
  tax <- if (side=="sell") amount*cost$tax else 0
  slip <- amount*cost$slip
  c(fee=fee,tax=tax,slip=slip,total=fee+tax+slip)
}

# 計入聯集：被完全移除的舊股票也必須賣出。
# 用反覆求解使「交易後配置資金 + 買賣費用 = 交易前淨值」。
v48_trade_at_close <- function(prior_shares, target_w, prices, cost=V5_COST) {
  prior_shares <- prior_shares[is.finite(prior_shares) & prior_shares >= 0]
  target_w <- target_w[is.finite(target_w) & target_w > 0]
  if (length(target_w)) target_w <- target_w / sum(target_w)
  syms <- union(names(prior_shares), names(target_w))
  if (!length(syms)) stop("沒有可交易股票。")
  p <- as.numeric(prices[syms]); names(p) <- syms
  if (any(!is.finite(p) | p <= 0))
    stop(paste0("再平衡日股票價格缺漏：", paste(syms[!is.finite(p)|p<=0],collapse=",")))
  oldval <- stats::setNames(rep(0,length(syms)),syms)
  if (length(prior_shares)) oldval[names(prior_shares)] <- prior_shares * p[names(prior_shares)]
  before <- sum(oldval)
  if (!is.finite(before) || before <= 0) stop("交易前淨值為零或無效。")
  after <- before
  calc <- function(candidate) {
    target <- stats::setNames(rep(0,length(syms)),syms)
    if (length(target_w)) target[names(target_w)] <- as.numeric(target_w)*candidate
    delta <- target-oldval
    buy <- pmax(delta,0); sell <- pmax(-delta,0)
    buy_fee <- ifelse(buy>1e-8,pmax(cost$min_fee,buy*cost$fee),0)
    sell_fee <- ifelse(sell>1e-8,pmax(cost$min_fee,sell*cost$fee),0)
    fee <- sum(buy_fee+sell_fee)
    tax <- sum(sell)*cost$tax
    slip <- sum(buy+sell)*cost$slip
    list(target=target, delta=delta, buy=buy,sell=sell,fee=fee,tax=tax,
         slip=slip,total=fee+tax+slip)
  }
  for (j in seq_len(60L)) {
    tr <- calc(after)
    new_after <- before-tr$total
    if (!is.finite(new_after) || new_after <= 0) stop("交易成本超過資金；停止回測。")
    if (abs(after-new_after) < 1e-7) {after <- new_after;break}
    after <- new_after
  }
  tr <- calc(after)
  if (abs(before-after-tr$total)>0.02) stop("交易成本與交易後資金不守恆。")
  shares <- tr$target/p
  shares <- shares[shares > 1e-12]
  list(shares=shares, after=after, before=before,
       buy=tr$buy, sell=tr$sell,
       fee=tr$fee, tax=tr$tax, slip=tr$slip, cost=tr$total,
       turnover=sum(abs(tr$delta))/(2*before), price=p,
       deltas=tr$delta)
}

# 若提供完整歷史 0050 名單，CSV 欄位：symbol,effective_from,effective_to
# effective_to 留空代表仍有效；本版本不自行編造歷史成分。
v48_membership <- function(path="data/0050_membership_history.csv") {
  if (!file.exists(path)) return(NULL)
  x <- utils::read.csv(path,stringsAsFactors=FALSE,fileEncoding="UTF-8")
  if (!all(c("symbol","effective_from","effective_to") %in% names(x)))
    stop("歷史成分股 CSV 缺少 symbol、effective_from、effective_to 欄位。")
  x$effective_from <- as.Date(x$effective_from)
  x$effective_to <- as.Date(ifelse(is.na(x$effective_to)|!nzchar(x$effective_to),
                                     "9999-12-31",x$effective_to))
  if (anyNA(x$effective_from) || anyNA(x$effective_to) || any(x$effective_to<x$effective_from))
    stop("歷史成分股 CSV 日期無效。")
  x
}

v5_backtest_v4 <- function(df, hold=63, n=10, cap=0.20,
                           max_ind=V4_SCREEN$max_industry, capital=1e6,
                           cost=V5_COST,start_after=300,t1=TRUE,progress=NULL) {
  # 舊 hold 參數只保留相容性。真正出場日由下一個季度 T+1 計算，避免 63 日重疊。
  if (!isTRUE(t1)) stop("C48 正式季度回測僅支援下一交易日 T+1 換股。")
  if (!is.finite(capital)||capital<=0 || n<2) stop("回測資金或持股數參數無效。")
  d <- as.data.frame(df)
  if (!all(c("date","symbol","adjusted") %in% names(d)))
    stop("股價資料缺少 date、symbol 或 adjusted。")
  d$date <- as.Date(d$date)
  d$symbol <- as.character(d$symbol)
  d$adjusted <- suppressWarnings(as.numeric(d$adjusted))
  d <- d[!is.na(d$date)&!is.na(d$symbol)&nzchar(d$symbol),,drop=FALSE]
  if (anyDuplicated(d[,c("date","symbol")])) stop("股票與日期有重複資料；請先整理 stock_data.rds。")
  d <- d[order(d$date,d$symbol),,drop=FALSE]
  bench0 <- d[d$symbol=="0050",,drop=FALSE]
  bench0 <- bench0[is.finite(bench0$adjusted)&bench0$adjusted>0,,drop=FALSE]
  # 若多檔股票有當日有效報價，0050 卻缺價，禁止把那天悄悄排除。
  active <- d[d$symbol!="0050" & is.finite(d$adjusted) & d$adjusted>0,,drop=FALSE]
  other_counts <- table(as.character(active$date))
  market_days <- as.Date(names(other_counts)[other_counts>=n])
  bench_missing <- setdiff(market_days,bench0$date)
  # 0050 非有效交易日不作虛構補價；保留診斷資訊，後續一律用 0050 有效日
  # 建立共同日期。此時其他股票可以有報價，但不會對 0050 補零報酬。
  dates <- sort(unique(bench0$date))
  if (length(dates)<start_after+30L) stop("0050 的有效價格不足以回測。")
  if (anyDuplicated(bench0$date)) stop("0050 有同日重複價格。")
  # 依確定的 0050 有效交易日建立價格矩陣（拒絕 row-index 錯位）。
  d <- d[d$date %in% dates,,drop=FALSE]
  if (!requireNamespace("tidyr",quietly=TRUE)) stop("缺少 tidyr 套件。")
  pw <- as.data.frame(tidyr::pivot_wider(d[,c("date","symbol","adjusted"),drop=FALSE],
                       names_from=symbol,values_from=adjusted))
  pw <- pw[order(as.Date(pw$date)),,drop=FALSE]
  pm <- as.matrix(pw[,-1,drop=FALSE]);storage.mode(pm)<-"double"
  rownames(pm) <- as.character(as.Date(pw$date))
  if (!identical(as.Date(rownames(pm)),dates)) stop("股票與 0050 交易日無法正確對齊。")
  if (!"0050"%in%colnames(pm) || any(!is.finite(pm[,"0050"]) | pm[,"0050"]<=0))
    stop("0050 交易日價格缺漏；不以零報酬替代。")

  # C52：交易必須使用當天真正的觀測價格。遇到原定交易日無報價時，
  # 整組持倉延後最多 2 個 0050 交易日成交；不回頭找前一日價格假裝當日成交。
  max_trade_delay <- 2L
  max_valuation_stale <- 2L
  v52_trade_idx <- function(base_idx, syms, prev_idx=0L) {
    syms <- unique(as.character(syms))
    if (!length(syms) || base_idx>nrow(pm)) return(NA_integer_)
    last_idx <- min(nrow(pm),base_idx+max_trade_delay)
    for (ii in seq.int(base_idx,last_idx)) {
      if (ii<=prev_idx) next
      px <- as.numeric(pm[ii,syms,drop=TRUE])
      if (length(px)==length(syms) && all(is.finite(px) & px>0)) return(ii)
    }
    NA_integer_
  }
  # 估值可暫用過去已知收盤價，絕不回填未來報價或用於實際成交。
  # 第 3 個連續缺價交易日起視為資料不足，結束於上一個已驗證季度。
  v52_valuation <- function(ii,syms) {
    syms <- as.character(syms)
    px <- as.numeric(pm[ii,syms,drop=TRUE]);names(px)<-syms
    stale <- character(0)
    bad <- !is.finite(px)|px<=0
    if (any(bad)) for (sym in syms[bad]) {
      start <- max(1L,ii-max_valuation_stale)
      if (start>=ii)next
      prior <- seq.int(start,ii-1L)
      prior <- prior[is.finite(pm[prior,sym]) & pm[prior,sym]>0]
      if (length(prior)) {px[sym] <- pm[utils::tail(prior,1L),sym];stale <- c(stale,sym)}
    }
    list(price=px,stale=stale,missing=syms[!is.finite(px)|px<=0])
  }
  # 追蹤披露：缺價估值天數、交易日期順延、後段未完成的原因。
  stale_rows <- list()
  delayed_rows <- list()
  truncated_reason <- NULL
  membership <- v48_membership()
  universe_type <- if (is.null(membership)) "當前股票池歷史代理（存在成分股事後偏誤風險）" else
    "依 CSV 有效日期過濾的歷史成分股池（完整性須人工核對）"
  qend <- v5_quarter_ends(dates)
  # 最新日期所屬季度還沒結束，不能當成已完成季度。
  if (length(qend)>0) qend <- head(qend,-1L)
  qend <- qend[qend>=start_after & qend<length(dates)]
  if (length(qend)<2L)
    stop(paste0("可驗證的完整季度不足：目前僅 ", length(qend),
                " 個可評分季度（要求至少 2 個）；0050 有效交易日 ",length(dates),"。"))

  # C50: qend 最後一筆是「已完成季末」；其 T+1 僅作為前季平倉邊界。
  # 不在最後季末再次開倉，因為最新（當季）的完整出場日尚未到來。
  # 例如資料至 2026-11 時：06/30 評分、07/01 買入、10/01 平倉；
  # 09/30 是這筆持倉的出場邊界，而不是另一筆可計入已實現報酬的新持倉。
  score_qend <- head(qend, -1L)
  plan <- list(); initial_skips <- 0L
  first_skip_detail <- ""
  last_valid_plan_k <- NA_integer_
  for (k in seq_along(score_qend)) {
    i <- score_qend[k];rated <- dates[i]
    if (!is.null(progress)) progress(k,length(score_qend))
    available <- d[d$date<=rated & d$symbol!="0050",,drop=FALSE]
    if (!is.null(membership)) {
      sy <- unique(membership$symbol[membership$effective_from<=rated &
                                     membership$effective_to>=rated])
      available <- available[available$symbol %in% sy,,drop=FALSE]
    }
    sc <- v4_score_table(available,end_date=rated)
    chosen <- if (is.null(sc)) NULL else v4_pick(sc,n,max_industry=max_ind)
    valid <- !is.null(chosen) && nrow(chosen)==n
    if (!valid) {
      got <- if (is.null(chosen)) 0L else nrow(chosen)
      pass_count <- if (is.null(sc)) 0L else sum(sc$pass %in% TRUE)
      reason <- paste0("評分日 ",rated,"：通過風險篩選 ",pass_count,
                       " 檔，依產業上限最後僅能選 ",got," / ",n," 檔")
      if (!length(plan)) {
        initial_skips <- initial_skips+1L
        first_skip_detail <- reason
        next
      }
      # 中途若不足固定十檔，從這季停止延伸歷史；已完成部分照常公布，
      # 但會標示截斷原因。不能跳過差的季度再從下一季接上來。
      truncated_reason <- paste0("後續季度未納入：",reason)
      break
    }
    ww <- v4_weights(chosen,scheme="inv_vol",cap=cap)
    if (length(ww)!=n || any(!is.finite(ww)) || any(ww<0) || sum(ww)<=0)
      stop(paste0("評分日 ",rated," 權重計算失敗。"))
    syms <- names(ww)
    # 延後整組交易；舊持股也需要在同一成交日有實際價格才可換手。
    old_symbols <- if(length(plan))names(plan[[length(plan)]]$weights) else character()
    entry_idx <- v52_trade_idx(i+1L,union(syms,old_symbols),
                if(length(plan))plan[[length(plan)]]$buy_idx else 0L)
    if (!is.finite(entry_idx)) {
      reason <- paste0("評分日 ",rated,"：T+1 至 T+3 無法取得全部新舊持股的真實成交價格")
      if (!length(plan)) {initial_skips<-initial_skips+1L;first_skip_detail<-reason;next}
      truncated_reason <- paste0("後續季度未納入：",reason)
      break
    }
    if(entry_idx>i+1L)
      delayed_rows[[length(delayed_rows)+1L]] <- data.frame(評分日=rated,
        原定交易日=dates[i+1L],實際交易日=dates[entry_idx],
        延後交易日數=entry_idx-(i+1L))
    plan[[length(plan)+1L]] <- list(rate_idx=i,rate_date=rated,
       buy_idx=entry_idx,buy_date=dates[entry_idx], weights=ww,
       score=sc,chosen=chosen)
    last_valid_plan_k <- k
  }
  if (!length(plan))
    stop(paste0("全部 ",length(qend)," 個評分季度都無法建立固定 ",n,
                " 檔的選股計畫。最後檢查：",first_skip_detail,
                "。請核對歷史股價覆蓋率、風險門檻與產業分散限制；勿直接降低門檻。"))
  # 最後一筆有效計畫的「下一個完整季末」是平倉邊界。
  # 中途發現後續無法選出 10 檔時，不能把最後邊界強行設在全資料末日。
  # 若末季所有舊持股都沒有可交易的真實價格，只放棄最後一個未完成季度；
  # 不會刪除前面已完成的績效，也不會把缺價股票賣在虛構價格上。
  end_idx <- NA_integer_
  repeat {
    if(!length(plan))stop("沒有可用的完整歷史持有季度：最早幾季缺少必要交易報價。")
    boundary_k <- last_valid_plan_k+1L
    if (boundary_k>length(qend))stop("找不到最後已完成季度的出場邊界。")
    last_q <- qend[boundary_k]
    end_idx <- v52_trade_idx(last_q+1L,names(plan[[length(plan)]]$weights),
               plan[[length(plan)]]$buy_idx)
    if(is.finite(end_idx))break
    truncated_reason <- paste0("後段持股缺少可成交的出場價格（",dates[last_q],
                               " 起兩個交易日內）；最後一季未納入")
    plan <- utils::head(plan,-1L)
    last_valid_plan_k <- last_valid_plan_k-1L
  }
  if (end_idx>length(dates) || end_idx<=plan[[length(plan)]]$buy_idx)
    stop("最後一季仍缺少完整持有區間。")
  plan[[length(plan)+1L]] <- list(
    rate_idx=last_q, rate_date=dates[last_q],
    buy_idx=end_idx, buy_date=dates[end_idx],
    weights=numeric(0), score=NULL, chosen=NULL
  )
  # 真實回測區間：首個有效評分季 T+1 建倉，最後已完成季度的 T+1 平倉。
  last_boundary <- plan[[length(plan)]]$buy_idx
  trade_points <- vapply(plan,function(x)x$buy_idx,integer(1))
  if (any(diff(trade_points)<=0)) stop("再平衡交易日期重複或逆序。")

  first <- plan[[1L]]
  px0 <- as.numeric(pm[first$buy_idx,names(first$weights)]);
  names(px0) <- names(first$weights)
  # 為了沿用同一筆成交及成本算法，初始使用現金直接計算買入費。
  # 現金交易前淨值 capital 不由舊持倉組成，使用以下專用求解。
  v48_cash_buy <- function(target_w,prices,cash) {
    target_w <- target_w/sum(target_w)
    post <- cash
    for (ii in 1:50) {
      amts <- target_w*post
      fee <- sum(pmax(cost$min_fee,amts*cost$fee))
      slip <- sum(amts)*cost$slip
      new_post <- cash-fee-slip
      if (new_post<=0) stop("初次買入手續費超過資金。")
      if (abs(new_post-post)<1e-7){post<-new_post;break}
      post<-new_post
    }
    amts <- target_w*post
    fees <- sum(pmax(cost$min_fee,amts*cost$fee)); slips <- sum(amts)*cost$slip
    if (abs(cash-post-fees-slips)>0.02) stop("首次買入成本不守恆。")
    list(shares=amts/prices,after=post, before=cash,
         buy=amts,sell=stats::setNames(rep(0,length(amts)),names(amts)),
         fee=fees,tax=0,slip=slips,cost=fees+slips,turnover=sum(amts)/(2*cash))
  }
  entering <- v48_cash_buy(first$weights,px0,capital)
  held <- entering$shares
  start_fee <- entering
  base_equity <- capital
  equity_after_buy <- entering$after
  bench_start <- pm[first$buy_idx,"0050"]
  daily <- list(data.frame(date=dates[first$buy_idx],
    equity10=equity_after_buy/capital,equity50=1,stringsAsFactors=FALSE))
  rec <- list(); trade_log <- list()
  add_trade <- function(tx,date,price_map) {
    vals <- c(tx$buy,tx$sell)
    if (is.null(names(tx$buy))) return(NULL)
    ss <- union(names(tx$buy),names(tx$sell))
    if (!length(ss))return(NULL)
    out <- lapply(ss,function(sym) {
      b <- if (sym%in%names(tx$buy)) as.numeric(tx$buy[sym]) else 0
      sl <- if (sym%in%names(tx$sell)) as.numeric(tx$sell[sym]) else 0
      if (b+sl<=1e-8)return(NULL)
      data.frame(date=as.Date(date),symbol=sym,buy=b,sell=sl,
                 proxy_adjusted_price=as.numeric(price_map[sym]),stringsAsFactors=FALSE)
    })
    do.call(rbind,out)
  }
  trade_log[[1]] <- add_trade(entering,dates[first$buy_idx],px0)
  completed <- length(plan)-1L
  for (j in seq_len(completed)) {
    cur <- plan[[j]]
    nxt <- plan[[j+1L]]
    start_i <- cur$buy_idx;end_i <- nxt$buy_idx
    if (end_i<=start_i) stop("持有期間重疊或無交易日。")
    # 先檢查整季估值完整性。若缺價超過 2 個交易日，保留之前已完成
    # 的季度績效，最後不完整的季度不會顯示，也不跨過缺失季度接續回測。
    symbol_check <- names(held)
    bad_day <- NA_integer_
    if(end_i-start_i>1L) for (ii in seq.int(start_i+1L,end_i-1L)) {
      check <- v52_valuation(ii,symbol_check)
      if (length(check$missing)) {bad_day<-ii;break}
    }
    if (is.finite(bad_day)) {
      truncated_reason <- paste0("歷史資料僅涵蓋前 ",length(rec)," 個已完成季度；",
        dates[bad_day]," 的持股缺價已超過 ",max_valuation_stale,
        " 個交易日（",paste(v52_valuation(bad_day,symbol_check)$missing,collapse=","),
        "）。未用未來股價補值。")
      if(!length(rec))stop(truncated_reason)
      break
    }
    old_equity_after_buy <- equity_after_buy
    old_shares <- held
    holding_symbols <- names(old_shares)
    if (!length(holding_symbols)) stop("持股清單為空。")
    if (end_i-start_i > 1L) for (ii in seq.int(start_i+1L,end_i-1L)) {
      observed <- v52_valuation(ii,holding_symbols)
      px <- observed$price
      if(length(observed$stale))
        stale_rows[[length(stale_rows)+1L]] <- data.frame(
          date=as.Date(dates[ii]),symbol=observed$stale,
          用途="僅供當日估值（沿用前次收盤價）")
      ev <- sum(old_shares*px)
      daily[[length(daily)+1L]] <- data.frame(date=dates[ii],
         equity10=ev/capital,equity50=pm[ii,"0050"]/bench_start)
    }
    pxend <- as.numeric(pm[end_i,holding_symbols]);names(pxend)<-holding_symbols
    if (any(!is.finite(pxend)|pxend<=0))
      stop(paste0("季度交接日 ",dates[end_i]," 舊持股價格缺失，停止回測。"))
    before_rebalance <- sum(old_shares*pxend)
    # 最後一季在最後邊界全部出清；不再買入未回測的下一季股票。
    target <- if (j==completed) numeric(0) else nxt$weights
    allsyms <- union(holding_symbols,names(target))
    trade_px <- as.numeric(pm[end_i,allsyms]);names(trade_px)<-allsyms
    trans <- v48_trade_at_close(old_shares,target,trade_px,cost)
    trade_log[[length(trade_log)+1L]] <- add_trade(trans,dates[end_i],trade_px)
    held <- trans$shares
    equity_after_buy <- trans$after
    daily[[length(daily)+1L]] <- data.frame(date=dates[end_i],
       equity10=equity_after_buy/capital,equity50=pm[end_i,"0050"]/bench_start)
    starting_equity <- if (j==1L) capital else old_equity_after_buy
    net <- equity_after_buy/starting_equity-1
    gross <- before_rebalance/old_equity_after_buy-1
    bench <- pm[end_i,"0050"]/pm[start_i,"0050"]-1
    if (!is.finite(net)||net<=-1 || !is.finite(bench) || bench<=-1) stop("無效報酬或淨值。")
    # 季度邊界以「換股後淨值」衡量：完整成本歸屬每個季末的換股事件。
    cost_period <- trans$cost + if(j==1L) start_fee$cost else 0
    fee_period <- trans$fee + if(j==1L) start_fee$fee else 0
    tax_period <- trans$tax
    slip_period <- trans$slip + if(j==1L) start_fee$slip else 0
    buy_period <- sum(trans$buy) + if(j==1L)sum(start_fee$buy) else 0
    sell_period <- sum(trans$sell)
    turnover_period <- trans$turnover + if(j==1L)start_fee$turnover else 0
    score <- cur$score
    universe_score <- score[score$symbol%in%colnames(pm),,drop=FALSE]
    ss <- universe_score$symbol
    fwd <- as.numeric(pm[end_i,ss]/pm[start_i,ss]-1)
    valid_fwd <- is.finite(fwd)
    ic <- if(sum(valid_fwd)>=5L) suppressWarnings(stats::cor(universe_score$p10_smooth[valid_fwd],
                             fwd[valid_fwd],method="spearman")) else NA_real_
    future_rank <- ss[order(-fwd,na.last=TRUE)]
    top_actual <- utils::head(future_rank,n)
    bottom_actual <- utils::tail(future_rank,n)
    hit <- length(intersect(names(cur$weights),top_actual))/n
    spread <- if(all(names(cur$weights)%in%ss))
      mean(fwd[match(names(cur$weights),ss)],na.rm=TRUE)-mean(fwd[match(bottom_actual,ss)],na.rm=TRUE) else NA_real_
    held_label <- paste0(if(exists("get_stock_name",mode="function"))get_stock_name(names(cur$weights)) else
                          names(cur$weights)," ",sprintf("%.1f%%",cur$weights*100),collapse="、")
    rec[[length(rec)+1L]] <- data.frame(
      評分日=as.Date(cur$rate_date),成交日=as.Date(dates[start_i]),出場日=as.Date(dates[end_i]),
      檔數=length(cur$weights),換手率=turnover_period,買進金額=buy_period,賣出金額=sell_period,
      手續費=fee_period,證交稅=tax_period,滑價=slip_period,
      成本合計=cost_period,成本率=cost_period/starting_equity,
      毛報酬=gross,淨報酬=net,基準0050=bench,超額=net-bench,
      累積淨值=equity_after_buy/capital,累積0050=pm[end_i,"0050"]/bench_start,
      命中率=hit,IC=ic,Spread=spread,持股=held_label,
      stringsAsFactors=FALSE)
  }
  if(!length(rec))stop("尚無已完成且價格有效的季度；請檢查歷史持股覆蓋率。")
  q <- do.call(rbind,rec)
  # 未完成季度的暫時估值不應留在「已完成」的歷史序列。
  day <- do.call(rbind,daily)
  day <- day[day$date<=utils::tail(q$出場日,1L),,drop=FALSE]
  day <- day[!duplicated(day$date),,drop=FALSE]
  if(length(stale_rows)) {
    stale_table <- do.call(rbind,stale_rows)
    stale_table <- stale_table[stale_table$date<=utils::tail(q$出場日,1L),,drop=FALSE]
  } else stale_table <- data.frame(date=as.Date(character()),symbol=character(),用途=character())
  if(length(delayed_rows)) {
    delayed_table <- do.call(rbind,delayed_rows)
    delayed_table <- delayed_table[delayed_table$實際交易日<=utils::tail(q$出場日,1L),,drop=FALSE]
  } else delayed_table <- data.frame(評分日=as.Date(character()),原定交易日=as.Date(character()),
                                   實際交易日=as.Date(character()),延後交易日數=integer())
  if (anyDuplicated(day$date) || any(diff(day$date)<=0)) stop("每日淨值時間軸重複或逆序。")
  if (any(!is.finite(day$equity10)|!is.finite(day$equity50)|day$equity10<=0|day$equity50<=0))
    stop("每日淨值異常。")
  # 強制驗證季度績效能還原每日淨值；誤差超過 1e-8 即停止。
  eq_recompute <- cumprod(1+q$淨報酬)
  if (max(abs(eq_recompute-q$累積淨值))>1e-8 ||
      max(abs(cumprod(1+q$基準0050)-q$累積0050))>1e-8)
    stop("季度報酬和累積淨值不一致。")
  if (max(abs(day$equity10[match(q$出場日,day$date)]-q$累積淨值))>1e-8)
    stop("季度曲線終點與每日淨值不一致。")
  if (any(!is.finite(q$成本合計)) || any(q$成本合計<0))
    stop("交易成本欄位不合理。")
  day$log_return10 <- c(log(day$equity10[1]),diff(log(day$equity10)))
  day$log_return50 <- c(0,diff(log(day$equity50)))
  lr10 <- day$log_return10
  lr50 <- day$log_return50
  annual <- function(x) exp(mean(x)*252)-1
  dd10 <- c(1,day$equity10);dd50 <- c(1,day$equity50)
  stats <- list(n=nrow(q),cum=tail(day$equity10,1)-1,ann=annual(lr10),
      vol=stats::sd(lr10)*sqrt(252),
      sharpe=if(stats::sd(lr10)>0) mean(lr10)*252/(stats::sd(lr10)*sqrt(252)) else NA_real_,
      mdd=min(dd10/cummax(dd10)-1),bench_cum=tail(day$equity50,1)-1,
      bench_ann=annual(lr50), win=mean(q$淨報酬>q$基準0050),
      avg_turnover=mean(q$換手率),total_cost=sum(q$成本合計),
      hit=mean(q$命中率,na.rm=TRUE),ic=mean(q$IC,na.rm=TRUE),
      spread=mean(q$Spread,na.rm=TRUE),
      universe_type=universe_type,initial_skipped_quarters=initial_skips,
      truncated_reason=truncated_reason,
      price_carry_observations=nrow(stale_table),delayed_trades=nrow(delayed_table),
      last_verified_date=utils::tail(q$出場日,1L),
      daily_mdd=min(dd10/cummax(dd10)-1),bench_daily_mdd=min(dd50/cummax(dd50)-1))
  list(quarters=q,daily=day,stats=stats,
       trades=do.call(rbind,trade_log),valuation_carry=stale_table,
       delayed_trades=delayed_table,
       version="C52 tolerant verified-prefix backtest (C50 boundary retained)",
       warnings=c(if(is.null(membership)) "沒有歷史成分股名單：結果仍可能有存活者偏誤。" else
                              "歷史成分股名單須人工核對完整性。",
                 "估值缺價只沿用過去最多 2 個交易日；成交需有真實報價，最多延後 2 個交易日。",
                 if(!is.null(truncated_reason))truncated_reason,
                 if(length(bench_missing))paste0("部分股票交易日無 0050 報價，兩者僅比較共同 0050 有效交易日：",
                   length(bench_missing)," 日"),
                 "以調整後收盤價代理成交價；持倉允許小數股、不含流動性衝擊。",
                 "季度報酬以換股後淨值為邊界，當期出場日的換股交易費用計入該期。"))
}
