# load code



.baR.load <- function() 
{
  require(devtools) 
  require(RCurl)
  require(plyr)
  require(dplyr)
  require(XBRL)
  require(finreportr)
  require(ggplot2)
  require(cowplot)
  require(quantmod)
  require(Rmisc)
  require(gridExtra)
  require(finstr)
  
  trw <- "https://raw.githubusercontent.com/Alecardi1973/TrLab2/master/barlib.R"
  source_url(trw)
  
}


class(.baR.load) <- "baRlib_cl" 


### code


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


class(GetURL) <- "baRlib_cl"


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


class(GetFinStat) <- "baRlib_cl"





###



GetFinStats <- function(symbol, years)
{
  nyrs <- length(years)
  #  tmp <- vector(mode = 'list', length = nyrs)
  tmp <- lapply(X = as.list(years), FUN = GetFinStat, symbol = symbol)
  allfs <- lapply(tmp, getElement, name = 'allf')
  bsfs <- lapply(tmp, getElement, name = 'bsf')
  isfs <- lapply(tmp, getElement, name = 'isf')
  allfy <- do.call(merge, allfs)  
  bsfy <- do.call(merge, bsfs)
  isfy <- do.call(merge, isfs)
  ans <- list(allfy=allfy, bsfy=bsfy, isfy=isfy)
  
  ans 
}


class(GetFinStats) <- "baRlib_cl"





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


# class(simplify.bs) <- "baRlib_cl"



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




### ratios


fin.ratios <- function(fstats, ratios)
{
  tmp <- fstats %>% calculate(date = as.Date(endDate), calculations = ratios, digits = 2)
  tmp[is.na(tmp)] <- 0
  ans <- xts(x = tmp[,-1], order.by = as.Date(tmp[,1]))
  
  ans
}



##



profit.margins <- calculation(
  Gross_Margin = (SalesRevenueNet-CostOfGoodsAndServicesSold)/SalesRevenueNet,
  Operating_Margin = OperatingIncomeLoss / SalesRevenueNet,
  Net_Margin = NetIncomeLoss / SalesRevenueNet)




bsf.ratios <- calculation(
  Current_Ratio = AssetsCurrent / LiabilitiesCurrent,
  Quick_Ratio = (CashAndCashEquivalentsAtCarryingValue + 
                   AvailableForSaleSecuritiesCurrent +
                   AccountsReceivableNetCurrent) / LiabilitiesCurrent)


isf.ratios <- calculation(
  .AccountReceivableLast = lag(AccountsReceivableNetCurrent),
  .AccountReceivableAvg = (.AccountReceivableLast + AccountsReceivableNetCurrent)/2,
  DaysSalesOutstanding = .AccountReceivableAvg / SalesRevenueNet * 365)





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

class(horizonal.analysis) <- "baRlib_cl"



#######





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



class(vertical.analysis) <- "baRlib_cl"




###########




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
    
    ans[[items[i]]] <- as.numeric(fs[i,-1])
    
    if(!missing(digits)) 
    {
      ans[[items[i]]] <- round(as.numeric((fs[i,-1])), digits)
    }
  }
  
  ans[["decimals"]] <- 0
  
  ans
}



class(make.statement) <- "baRlib_cl"





###########


barTable <- function(x)
{
  g <- tableGrob(x)
  grid.newpage()
  grid.draw(g)
}
  
  
 

############


invisible <- function(x) .Primitive("invisible")



#####



print.baRlib_cl <- body <- function(x, ...)
{
  invisible(x)
}
  
  
  
  
  
  
  
