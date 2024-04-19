
# baRlib code



###

GetURL <- function(symbol, year) 
{
  lower.symbol <- tolower(symbol)
  accession.no.raw <- finreportr:::GetAccessionNo(symbol, year, foreign = FALSE)
  accession.no <- gsub("-", "", accession.no.raw)
  CIK <- CompanyInfo(symbol)
  CIK <- as.numeric(CIK$CIK)
  report.period <- finreportr:::ReportPeriod(symbol, CIK, accession.no, accession.no.raw)
  report.period <- gsub("-", "", report.period)
  inst.url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", 
                     accession.no, "/", lower.symbol, "-", report.period, ".xml")
  return(inst.url)
}


class(GetURL) <- "baR.libs"


###   


GetFinStat <- function(year, symbol)
{
  options(HTTPUserAgent = "name_surname   name_surname@domain.com")

  geturl <- GetURL(symbol=symbol, year=year) 
  xbrl_data <- xbrlDoAll(geturl)
  xbrl_stat <- xbrl_stat2 <- xbrl_get_statements(xbrl_data)
  nst <- toupper(names(xbrl_stat))
  names(xbrl_stat) <- nst
  names.bsf <- c("CONSOLIDATED BALANCE SHEET", 
                 "CONSOLIDATED BALANCE SHEETS", 
                 "CONSOLIDATED STATEMENT OF FINANCIAL POSITION", 
                 "CONSOLIDATED STATEMENTS OF FINANCIAL POSITION", 
                 "CONSOLIDATED FINANCIAL POSITION", 
                 "OF FINANCIAL POSITION CLASSIFIED",
                 "BALANCE SHEETS")

  names.isf <- c("CONSOLIDATED STATEMENTS OF INCOME", 
                 "CONSOLIDATED STATEMENT OF INCOME", 
                 "CONSOLIDATED STATEMENTS OF OPERATIONS", 
                 "CONSOLIDATED STATEMENT OF OPERATIONS", 
                 "CONSOLIDATED STATEMENT OF EARNINGS", 
                 "CONSOLIDATED STATEMENTS OF EARNINGS",  
                 "CONSOLIDATED RESULTS OF OPERATIONS", 
                 "INCOME STATEMENTS",
                 "OF INCOME")

  vars.bsf <- nst[nst %in% gsub(" ","", paste0("STATEMENT",names.bsf))]
  vars.isf <- nst[nst %in% gsub(" ","", paste0("STATEMENT",names.isf))]
  bsf <- xbrl_stat[[vars.bsf]]
  isf <- xbrl_stat[[vars.isf]]
  allf <- merge(bsf, isf)
  ans <- list(allf=allf, bsf=bsf, isf=isf)

  ans
}  


class(GetFinStat) <- "baR.libs"


###


@@ -88,263 +26,9 @@ GetFinStats <- function(symbol, years)
class(GetFinStats) <- "baR.libs"


###


simplify.bs <- function(bsf) 
{  
  ans <- expose(bsf,
                # Assets
                `Current Assets` = "AssetsCurrent",
                `Noncurrent Assets` = other("Assets"),
                # Liabilites and equity
                `Current Liabilities` = "LiabilitiesCurrent",
                `Noncurrent Liabilities` = other(c("Liabilities", "CommitmentsAndContingencies")),
                `Stockholders Equity` = "StockholdersEquity")

  ans
}


###


plot.bsf <- function(bsf, prop = FALSE, psave = FALSE)
{
  bsf.prop <- proportional(bsf)

  theme_set(theme_gray())

  if(prop == FALSE)
  {  
    p <- plot_grid(plot_double_stacked_bar(bsf),
                   plot_double_stacked_bar(bsf, by_date = FALSE), nrow=2)
  }

  if(prop == TRUE)
  {  
    p <- plot_grid(plot_double_stacked_bar(bsf.prop),
                   plot_double_stacked_bar(bsf.prop, by_date = FALSE), nrow=2)
  }   

  if(psave == TRUE) save_plot("plot.pdf", p, nrow=2, ncol = 2)

  print(p)   
}


###


plot.isf <- function(isf, dates=FALSE, prop = FALSE, psave = FALSE)
{
  plot.wfd <- function(date, x) plot_waterfall(x=x, date=date)
  isf.prop <- proportional(isf)
  isf.dates <- isf$endDate

  if(is.null(dates)) isf.dates <- isf$endDate[1]

  if(!is.null(dates)) 
  {
    stopifnot(max(dates) <= length(isf.dates))
    stopifnot(length(dates) <= 2)
    isf.dates <- isf$endDate[dates]
  }

  ldat <- length(isf.dates)
  theme_set(theme_gray())
  layo <- matrix(c(1:ldat), nrow=ldat)

  if(prop == FALSE)
  {  
    p <- multiplot(plotlist=lapply(X=isf.dates, FUN=plot.wfd, x=isf), layout=layo)
  }

  print(p)   
}


###


horizonal.analysis <- function(fs, type, firstDate=NULL, lastDate=NULL, units = 1000, export = F)
{
  dates <- fs[["endDate"]]

  stopifnot(length(dates) > 1)

  if((is.null(firstDate) | is.null(lastDate)) & length(dates) > 1)
  {
    dates <- rev(fs[["endDate"]])
    firstDate <- dates[2]
    lastDate <- dates[1]
  }

  if(type == 'bs')
  {
    tmp.names <- c(			
      as.character("Assets =")	,
      as.character("+ AssetsCurrent =")	,
      as.character("	+ CashAndCashEquivalentsAtCarryingValue")	,
      as.character("	+ AvailableForSaleSecuritiesCurrent")	,
      as.character("	+ AccountsReceivableNetCurrent")	,
      as.character("	+ InventoryNet")	,
      as.character("	+ DeferredTaxAssetsNetCurrent")	,
      as.character("	+ NontradeReceivablesCurrent")	,
      as.character("	+ OtherAssetsCurrent")	,
      as.character("+ AvailableForSaleSecuritiesNoncurrent")	,
      as.character("+ PropertyPlantAndEquipmentNet")	,
      as.character("+ Goodwill")	,
      as.character("+ IntangibleAssetsNetExcludingGoodwill")	,
      as.character("+ OtherAssetsNoncurrent")	,
      as.character("LiabilitiesAndStockholdersEquity =")	,
      as.character("+ Liabilities =")	,
      as.character("	+ LiabilitiesCurrent =")	,
      as.character("	  + AccountsPayableCurrent")	,
      as.character("	  + AccruedLiabilitiesCurrent")	,
      as.character("	  + DeferredRevenueCurrent")	,
      as.character("	  + CommercialPaper")	,
      as.character("	+ DeferredRevenueNoncurrent")	,
      as.character("	+ LongTermDebt")	,
      as.character("	+ OtherLiabilitiesNoncurrent")	,
      as.character("+ CommitmentsAndContingencies")	,
      as.character("+ StockholdersEquity =")	,
      as.character("	+ CommonStockValue")	,
      as.character("	+ RetainedEarningsAccumulatedDeficit")	,
      as.character("	+ AccumulatedOtherComprehensiveIncomeLossNetOfTax")	,
      as.character("	+ CommonStocksIncludingAdditionalPaidInCapital")	)
  }

  if(type == 'is')
  {
    tmp.names <- c(			
      as.character("	NetIncomeLoss ="),
      as.character("	+ IncomeLossFromContinuingOperationsBefore... ="),
      as.character("	  + OperatingIncomeLoss ="),
      as.character("	    + GrossProfit ="),
      as.character("	      + SalesRevenueNet"),
      as.character("	      - CostOfGoodsAndServicesSold"),
      as.character("	    - OperatingExpenses ="),
      as.character("	      + ResearchAndDevelopmentExpense"),
      as.character("	      + SellingGeneralAndAdministrativeExpense"),
      as.character("	  + NonoperatingIncomeExpense"),
      as.character("	- IncomeTaxExpenseBenefit") )
  }


  stm.names <- gsub("[\t]", " ", tmp.names)
  fdate <- fs %>% filter(endDate == firstDate) %>% select(!!! rlang::syms(names(.)[-(1:4)]))
  ldate <- fs %>% filter(endDate == lastDate) %>% select(!!! rlang::syms(names(.)[-(1:4)]))
  ans <- round(t(rbind(ldate/units,fdate/units, (ldate - fdate)/units, (ldate - fdate)/fdate*100)),2)

  rownames(ans) <- stm.names
  colnames(ans) <- c(lastDate, firstDate, "variation", "% variation") 

  if(export) write.csv(ans, file = 'horizontal_analisys.csv')

  ans
}

class(horizonal.analysis) <- "baR.libs"


###


vertical.analysis <- function(fs, type, total = NULL, digits = 2, export = F) 
{
  parent_id <- tmp <- total

  if(type == 'bs' & !is.null(total)) parent_id <- tmp <- total

  if(type == 'is' & is.null(total)) parent_id <- tmp <- 'SalesRevenueNet'

  ans <- fs

  for(col_name in get_elements(fs)[["elementId"]]) 
  {
    if(is.null(tmp)) parent_id <- get_ascendant(fs, col_name)[[1]]

    ans[[col_name]] <- fs[[col_name]]/fs[[parent_id]]*100

    if(!missing(digits)) 
    {
      ans[[col_name]] <- round(ans[[col_name]], digits)
    }
  }

  ans[["decimals"]] <- 0

  if(export) write.csv(ans, file = 'vertical_analisys.csv')

  ans
}


class(vertical.analysis) <- "baR.libs"


###


make.statement <- function(template, skeleton, digits = 2) 
{
  fs <- read.csv(template, header = TRUE)
  dates <- colnames(fs)[-1]
  ans <- eval(as.name(load(file = skeleton)))
  items <- get_elements(ans)[["elementId"]]

  for(i in 1:length(items)) 
  { 
    # for(col_name in get_elements(fs)[["elementId"]]) 

    # if(is.null(tmp)) parent_id <- get_ascendant(fs, col_name)[[1]]

    ans[[items[i]]] <- rev(as.numeric(fs[i,-1]))

    if(!missing(digits)) 
    {
      ans[[items[i]]] <- rev(round(as.numeric((fs[i,-1])), digits))
    }
  }

  ans[["decimals"]] <- 0

  ans
}



class(make.statement) <- "baR.libs"


###


barTable <- function(x)
{
  g <- tableGrob(x)
  grid.newpage()
  grid.draw(g)
}


###


invisible <- function(x) .Primitive("invisible")



###


print.baR.libs <- body <- ls <- function(x, ...)
{
  invisible(x)
}



### ratios

@@ -383,106 +67,3 @@ isf.ratios <- calculation(
  DaysSalesOutstanding = .AccountReceivableAvg / SalesRevenueNet * 365)


###



capm.fit <- function(x, mkt)
{
               dat <- as.list(x)
              ndat <- names(dat)
                cl <- lapply(dat, "[", , 4, simplify = TRUE)
            prices <- do.call(merge, cl)
              rets <- as.xts(100*apply(log(prices), 2, diff)) 
             out.l <- is.na(rets) | rets < -10 | rets > 10
       rets[out.l] <- 0
    colnames(rets) <- ndat
          mkt.rets <- rets[, mkt]
              rcol <- which(ndat == mkt)
          all.rets <- rets[, -rcol]
             lm.eq <- paste(colnames(all.rets), '~', mkt)
           lm.form <- lapply(lm.eq, formula)
         lm.models <- lapply(lm.form, lm, data = rets)
           reg.par <- lapply(lm.models, coef)
             betas <- c(1, unlist(lapply(reg.par, "[", 2)))
      names(betas) <- c(mkt, colnames(all.rets))
              mmkt <- mean(mkt.rets, na.rm = TRUE)
             mrets <- c(mmkt, apply(all.rets, 2, mean, na.rm = TRUE))
      names(mrets) <- c(mkt, colnames(all.rets))
               sml <- lm(mrets~betas)
               tmp <- round(data.frame(mean.rets = mrets, betas = betas),2)
               ans <- list(capm=tmp, mkt = mkt.rets, stk = all.rets)

  par(lwd=2)
  plot(x=betas, y=mrets, xlim = c(min(betas)-0.1, max(betas)+0.1), 
       ylim = c(min(mrets)-0.1, max(mrets)+0.1),
       ylab = 'expected returns', main = '')

       text(betas[1], mrets[1], mkt ,pos=3,col="red")
       text(betas[-1], mrets[-1],colnames(all.rets),pos=3,col="blue")
       abline(sml, col="darkgrey", lwd=1)
       text(x = 0.79, y = -0.01, 'Security Market Line',pos=4,col="green")
  grid(lwd=1)

  ans
}



###



portfolio.fit <- function(x, target = 'max.eret', rp.method = 'simplex')
{
               mkt_ret <- x$mkt
             asset_ret <- x$stk
     colnames(mkt_ret) <- "market"
             port_spec <- portfolio.spec(assets = colnames(asset_ret))
           # port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")
             port_spec <- add.constraint(portfolio=port_spec,type="weight_sum",min_sum=0.98,max_sum=1.02)
             port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

  if(target=='min.stdv')  port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

  if(target=='max.eret')  port_spec <- add.objective(portfolio = port_spec, type="return", name="mean")

                          print(port_spec)

                    rp <- random_portfolios(portfolio=port_spec, permutations = 1000, rp_method = rp.method)   

                   opt <- optimize.portfolio(R = asset_ret, portfolio = port_spec, 
                                                      optimize_method = "random", rp = rp, trace = TRUE)

               weights <- round(extractWeights(opt), 3)
             portf_ret <- Return.portfolio(R = asset_ret, weights = extractWeights(opt))
   colnames(portf_ret) <- "portfolio"
                  rets <- aux <- merge(portf_ret, mkt_ret)
                          rets[is.na(aux) | aux < -100 | aux > 100] <- 0                

                 means <- apply(rets, 2, mean, na.rm = TRUE)
                  vols <- apply(rets, 2, sd, na.rm = TRUE)
                 stats <- round(data.frame(mean_ret = means, volatility = vols), 3)
                c.rets <- reclass(apply(X = rets, MARGIN = 2, FUN = cumsum), match.to=rets)     
                   ans <- list(rets = rets, c.rets = c.rets, opt = opt, stats = stats, weights = weights)         

  # plot.xts(c.rets, main = 'Cumulative returns')

  ans          
}  



class(portfolio.fit) <- "baR.libs"



###
