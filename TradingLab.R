##################
# performance bt
##################





Performance <- function(rets, posi) 
{
         nn <- length(rets) 
   
        if(length(posi) == 1) posi <- c(rep(0, nn-2),1:2)
           
    cumRetx <- sum(rets, na.rm = T)
      volat <- sd(rets, na.rm = TRUE)
    sharpex <- mean(rets, na.rm = TRUE)/volat
     turnvr <- mean(abs(diff(posi)), na.rm=TRUE)                                                    
    winpctx <- length(rets[rets > 0])/length(rets[rets != 0])
       Perf <- round(c(cumRetx, sharpex, turnvr, winpctx, volat),3)

    names(Perf) <- c("Cumulative Return","Sharpe Ratio","Turnover","Win %", "Volatility")
    return(Perf)
}


class(Performance) <- "TradingLab"





#


ewma.strat <- function(x, nn)
{
         xle <- length(x)
         ema <- EMA(x = x, n = nn) 
         tmp <- ifelse(x[xle] > ema[xle], 1, 0)
    position <- ifelse(x[xle] < ema[xle], -1, tmp)

return(position)
}



class(ewma.strat) <- "TradingLab"



#



ewma2.strat <- function(x, nn1, nn2)
{
         xle <- length(x)
        ema1 <- EMA(x = x, n = nn1)       ## nn1 < nn2
        ema2 <- EMA(x = x, n = nn2) 
         tmp <- ifelse(ema1[xle] > ema2[xle], 1, 0)
    position <- ifelse(ema1[xle] < ema2[xle], -1, tmp)

return(position)
}



##



ewma3.strat <- function(x, nn, w.int)
{
         xle <- length(x)
         ema <- EMA(x = x, n = nn) 
         tmp <- ifelse(x[xle] > (1 + w.int) * ema[xle], 1, 0)
    position <- ifelse(x[xle] < (1 - w.int) * ema[xle], -1, tmp)

return(position)
}





#


my.backtest <- function(dprices, wsiz, strat.fun, ...)
{
                 ndata <- nrow(dprices)
         if(is.xts(dprices))    dates <- as.POSIXct(as.vector(dprices[,1]), origin = index(dprices[1,1]))
         if(! is.xts(dprices))  dates <- as.POSIXct(dprices[,1], origin = character(dprices[,1])) 
                prices <- dprices[,2]
         if(ndata <= wsiz) stop('wsiz must be smaller than the length of data')

            bmkReturns <- c(0, diff(prices, na.pad = FALSE))
        bmkReturns.xts <- xts(bmkReturns, order.by=dates)
             positions <- rep(0, ndata)

  for(i in (wsiz + 1): ndata) positions[i] <- strat.fun(x = prices[(i-wsiz):i], ...)

           positions.bt <- c(positions[-ndata],0) 
             myReturns <- c(0, bmkReturns[-1] * positions[-ndata])
         myReturns.xts <- xts(myReturns, order.by= dates)
            posReturns <- negReturns <- myReturns
                          negReturns[positions.bt >= 0] <- 0
                          posReturns[positions.bt <= 0] <- 0
            # charts.PerformanceSummary(cbind(bmkReturns,myReturns))


             par(mfrow = c(3,1))
             ts.plot(cbind(cumsum(bmkReturns),cumsum(myReturns)), col = c(4,1), lty = 1:2, ylab = 'Cumulative Returns')
             ts.plot(cbind(cumsum(posReturns),cumsum(negReturns)), col = 1:2, lty = c(1,1))
             plot.ts(c(positions[-1],0), col = 4, ylab = 'positions')
             par(mfrow = c(1,1))

             ans <- cbind(Me=Performance(myReturns.xts, posi = positions.bt),Benchmark = Performance(bmkReturns.xts, posi = 1))
   
return(ans)
}


class(my.backtest) <- "TradingLab"


#####


invisible <- function(x) .Primitive("invisible")



#####



print.TradingLab <- body <- function(x, ...)
{
	invisible(x)
}



#####


            
