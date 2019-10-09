"trigger_index = trigIndexDU; event_days = 10; market = mkt_xts; risk_free_rate = rf_xts; prices = price100" # arguments

abnormal_returns <- function(trigger_index, event_days, prices, market, risk_free_rate){

  j = 0 # initialise column number counter variable
  
  pos <- 1 # counter for number of elements in the list
  
  mylist <- vector("list", length = length(trigger_index))
  
  for (x in trigger_index) { 
    j = j + 1 # column number
    
    ticker <- colnames(prices[,j,drop=F])
    
    if (j > ncol(prices)){
      break # if j surpasses the number of columns, exit the for loop
    }
    
    CAR <- matrix(0, nrow = length(x), ncol = 4)
    event_dates <- vector('list', length = length(x))
    r <- 1
    
    for (i in x) {
      
      prev <- i - 5
      
      if (prev < 0) {
        prev <- 0
      }
      
      tmp <- prices[prev:(i-1), j, drop = FALSE]
      
      no_trades <- duplicated(tmp, nmax = 1) %>% sum() 
      # sum number of times same value appears more than once
      # that means no trade occurred that day
      
      a <- i + 1
      b <- a + event_days
      temp <- prices[a:b, j, drop = FALSE]
      
      m <- a + 1
      
      if (b > nrow(market)) {
        CAR[r,] <- NA
        event_dates[[r]] <- paste(ticker, sep = "-", date_range[1])
        r <- r + 1
        break
        # if the 21 days is after the last day in the sample, next iteration
      }
      
      date_range <- index(market[m:b])
      
      if (no_trades < 3 & nrow(tmp) > 0) {

        rets <- temp %>% returns(., method = "simple")
        
        rets <-  rets - risk_free_rate[date_range]
        
        
        rf_hold <- risk_free_rate[date_range]
        mkt_hold <- market[date_range]
        
        mrp <- mkt_hold - rf_hold
        
        if (any(is.na(rets)) == FALSE) {

          mod <- lm(rets ~ mrp)
          
          sum_mod <- summary(mod)
          
          abr <- sum_mod$coefficients[1,] %>% as.data.frame() %>% t()
          
          CAR[r,] <- abr[1,]
          
          }
        
      } else {

        CAR[r,] <- NA

      }
      
      event_dates[[r]] <- paste(ticker, sep = "-", date_range[1])
      r <- r + 1
    }
    
    colnames(CAR) <- colnames(abr)
    rownames(CAR) <- unlist(event_dates)
    mylist[[pos]] <- CAR
    pos <- pos + 1
  }
  names(mylist) <- names(trigger_index)
  return(mylist)
}

