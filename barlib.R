
# baRlib code



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


class(GetFinStats) <- "baR.libs"






### ratios


fin.ratios <- function(fstats, ratios)
{
  tmp <- fstats %>% calculate(date = as.Date(endDate), calculations = ratios, digits = 2)
  tmp[is.na(tmp)] <- 0
  ans <- xts(x = tmp[,-1], order.by = as.Date(tmp[,1]))
  
  ans
}


###


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


