# v33｜每日 T+1 全體預先估計（Ridge Logistic 與一般 Logistic 同期比較）（僅依 app.R 的選股名單）
# Rscript precompute_t1.R
# 不啟動 Shiny，不修改 app.R / 模型公式；直接取用 app.R 的純計算函數。
# 固定預設設定 252/63/95%。其他自訂設定仍可網站手動計算。

input_path <- "data/stock_data.rds"
output_path <- "data/t1_precomputed.rds"
if(!file.exists(input_path))stop("找不到 ", input_path, "，請先執行原本的資料更新流程")
if(!file.exists("app.R"))
  stop("請在包含 app.R 的網站專案根目錄執行")

# 只解析 app.R 的股票清單、版本與 T+1 純計算函數，不執行 Shiny。
# 每日預算與網站因此使用完全相同的模型公式。
expressions <- parse("app.R",keep.source=FALSE)
assignments <- Filter(function(expr) is.call(expr)&&
  identical(expr[[1L]],as.name("<-"))&&length(expr)>=3L&&is.symbol(expr[[2L]]),
  as.list(expressions))
needed <- c("stock_info","T1_ENGINE_VERSION","v29_ar_inputs",
  "v29_garch_path","v29_garch_fit","v29_interval_score","v30_market_returns",
  "v31_logit_prepare","v30_logit_prob","v33_ridge_fit",
  "v33_ridge_prob","v27_one_step","v27_rolling_oos",
  "v28_universe","v28_forecast_snapshot","v28_all_oos")
found <- vapply(assignments,function(expr)as.character(expr[[2L]]),character(1))
if(!all(needed%in%found))stop("app.R 缺少必要 T+1 模型函數：",
                            paste(setdiff(needed,found),collapse=", "))
model_env <- new.env(parent=.GlobalEnv)
for(expr in assignments[found%in%needed])eval(expr,envir=model_env)
for(nm in needed)assign(nm,get(nm,envir=model_env),envir=.GlobalEnv)
symbols <- as.character(stock_info$symbol)
if(!length(symbols))stop("股票清單為空")

d <- readRDS(input_path)
if(!is.data.frame(d)||!all(c("symbol","date")%in%names(d)))
  stop("stock_data.rds 缺少 symbol/date 欄")
if(is.numeric(d$symbol))d$symbol<-sprintf("%04d",as.integer(d$symbol))
d$symbol<-as.character(d$symbol)
if(!"adjusted"%in%names(d)){
  alt<-c("price.adjusted","close","price.close")
  k<-alt[alt%in%names(d)]
  if(!length(k))stop("資料缺少 adjusted/close 欄位")
  d$adjusted<-d[[k[[1L]]]]
}
d$adjusted<-suppressWarnings(as.numeric(d$adjusted))
if(inherits(d$date,"Date")){} else if(is.numeric(d$date)){
  d$date<-as.Date(d$date,origin="1970-01-01")
} else d$date<-as.Date(d$date)
d<-d[d$symbol%in%c("0050",symbols)&!is.na(d$date)&is.finite(d$adjusted)&d$adjusted>0,
     c("symbol","date","adjusted"),drop=FALSE]
if(!nrow(d))stop("股票價格資料為空")

params<-list(window=252L,test_days=63L,level=.95)
input_md5<-as.character(unname(tools::md5sum(input_path))[[1L]])
# 相同資料、相同模型、相同設定時，不再重複重估（降低 Actions 使用量）。
cache_is_current <- FALSE
if(file.exists(output_path)){
  old<-tryCatch(readRDS(output_path),error=function(e)NULL)
  if(!is.null(old)&&identical(old$schema,1L)&&identical(old$data_md5,input_md5)&&
     identical(old$engine_version,T1_ENGINE_VERSION)&&identical(old$params,params)&&
     !is.null(old$result$history$per_stock)){
    message("已有相同資料版本的有效 T+1 快取，跳過重算")
    cache_is_current <- TRUE
  }
}
if(!cache_is_current) {
  start<-proc.time()[[3L]]
  lookup<-v30_market_returns(d)
  cache_env<-new.env(parent=emptyenv())
  message("開始預算 T+1：有效清單 ",length(symbols)," 檔")
  # 診斷僅在全體快照失敗時執行；正常路徑不增加估計負擔。
  inspect_snapshot_failure <- function(err) {
    message("[診斷] 全體快照失敗：", conditionMessage(err))
    uu <- tryCatch(v28_universe(d, symbols), error=function(e) {
      message("[診斷] 交易日篩選錯誤：", conditionMessage(e)); NULL
    })
    if(is.null(uu))return(invisible(NULL))
    message("[診斷] 0050 基準資料日：", format(uu$date, "%Y-%m-%d"),
            "；同期有價格股票：", length(uu$symbols), " 檔")
    if(!length(uu$symbols))return(invisible(NULL))
    counts <- vapply(uu$symbols, function(sym) {
      sum(uu$data$symbol == sym)
    }, integer(1L))
    need <- params$window + 2L
    message("[診斷] 本次每檔最低價格筆數：", need,
            "；足夠筆數的股票：", sum(counts >= need), "/", length(counts))
    message("[診斷] 各檔歷史筆數範圍：", min(counts), "～", max(counts))
    order_sym <- uu$symbols[order(counts, decreasing=TRUE)]
    # 使用歷史最多的股票作樣本，優先區分資料缺漏、Logistic、AR、GARCH。
    for(sym in head(order_sym, 5L)) {
      ss <- uu$data[uu$data$symbol==sym,,drop=FALSE]
      ss <- ss[order(ss$date),,drop=FALSE]
      pp <- as.numeric(ss$adjusted)
      message("[診斷] ", sym, ": n=", length(pp),
              "；最新日=", format(max(ss$date), "%Y-%m-%d"))
      if(length(pp) < need) {
        message("[診斷] ", sym, ": 資料不足（需要至少 ",need," 筆）")
        next
      }
      lg <- tryCatch(v31_logit_prepare(pp,ss$date,lookup),error=function(e){
        message("[診斷] ",sym," Logistic 特徵失敗：",conditionMessage(e));NULL
      })
      if(is.null(lg))next
      prob <- tryCatch(v30_logit_prob(pp,ss$date,length(pp),params$window,
                                      lookup,feature_cache=lg),error=function(e){
        message("[診斷] ",sym," Logistic 估計失敗：",conditionMessage(e));NULL
      })
      if(is.null(prob))next
      ridge <- tryCatch(v33_ridge_prob(lg,length(pp),params$window,lambda=10),
        error=function(e){
          message("[診斷] ",sym," Ridge 估計失敗：",conditionMessage(e));NULL
        })
      if(is.null(ridge))next
      ar <- tryCatch(v29_ar_inputs(pp,length(pp),params$window),error=function(e){
        message("[診斷] ",sym," AR 估計失敗：",conditionMessage(e));NULL
      })
      if(is.null(ar))next
      gg <- tryCatch(v29_garch_fit(ar$resid_pct),error=function(e){
        message("[診斷] ",sym," GARCH 估計失敗：",conditionMessage(e));NULL
      })
      if(is.null(gg))next
      fc <- tryCatch(v27_one_step(pp,length(pp),params$window,gfit=gg,
                       level=params$level,dates=ss$date,market_returns=lookup,
                       logit_cache=lg),error=function(e){
        message("[診斷] ",sym," 最終預測失敗：",conditionMessage(e));NULL
      })
      if(!is.null(fc))message("[診斷] ",sym," 單檔完整估計成功，請核對全體資料的型別與日期")
    }
    invisible(NULL)
  }
  snap <- tryCatch(
    v28_forecast_snapshot(d,symbols,window=params$window,level=params$level,
                          market_returns=lookup,logit_cache_env=cache_env),
    error=function(e) {
      inspect_snapshot_failure(e)
      stop("全體預測未完成，以上 [診斷] 訊息指出具體問題；舊快取沒有被覆蓋。原始錯誤：",
           conditionMessage(e),call.=FALSE)
    }
  )
  history<-v28_all_oos(d,symbols,test_days=params$test_days,window=params$window,
                         level=params$level,market_returns=lookup,logit_cache_env=cache_env,
    progress=function(j,n){
      if(j%%10L==0L||j==n)message("歷史驗證：",j,"/",n," 檔")
    })
  if(!nrow(snap$result)||!nrow(history$per_stock))stop("無有效預算結果，不覆蓋舊檔")
  if(!identical(input_md5,as.character(unname(tools::md5sum(input_path))[[1L]])))
    stop("計算期間原始資料檔被更新，拒絕寫入不一致快取，請重新執行")
  payload<-list(schema=1L,engine_version=T1_ENGINE_VERSION,data_md5=input_md5,
    params=params,computed_at=Sys.time(),result=list(snapshot=snap,history=history))
  dir.create(dirname(output_path),showWarnings=FALSE,recursive=TRUE)
  tmp<-tempfile("t1_precompute_",tmpdir=dirname(output_path),fileext=".rds")
  saveRDS(payload,tmp,compress="xz")
  # 優先原子更名；部分平台目標檔存在會失敗，退回複製。
  if(!file.rename(tmp,output_path)){
    if(!file.copy(tmp,output_path,overwrite=TRUE))stop("無法寫入快取：",output_path)
    unlink(tmp)
  }
  message("完成：",nrow(snap$result)," 檔預測；",nrow(history$per_stock),
          " 檔樣本外驗證；歷時 ",round(proc.time()[[3L]]-start,1)," 秒")

}
