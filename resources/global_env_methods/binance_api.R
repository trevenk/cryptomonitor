library(httr)
library(jsonlite)
library(ggplot2)
BINANCE_ENDPOINT_G <<- 'https://api.binance.com'


#### Core Methods ####
makeRequest <- function(request, parameters, api_key) {
  if(missing(api_key)) {
    if(missing(parameters)) {                         #### needed
      return(GET(paste0(BINANCE_ENDPOINT_G, request)))
    } else {
      return(GET(paste0(BINANCE_ENDPOINT_G, request, parameters)))
    }
  } else {
    if(missing(parameters)) {                         #### needed
      return(GET(paste0(BINANCE_ENDPOINT_G, request), add_headers("X-MBX-APIKEY" = api_key$api.key)))
    } else {
      return(GET(paste0(BINANCE_ENDPOINT_G, request, parameters), add_headers("X-MBX-APIKEY" = api_key$api.key)))
    }
  }
}
#### Library functions ####
binanceTestConnectivity <- function() {
  makeRequest(request = '/api/v3/ping')
}
binanceCheckServerTime <- function() {
  makeRequest(request = '/api/v3/time')
}
binanceExchangeInfo <- function() {
  makeRequest(request = '/api/v3/exchangeInfo')
}
binanceOrderBook <- function(symbol, limit, parsed) {
  if(missing(parsed)) parsed <- FALSE
  if(!parsed) {
    if(missing(limit)) {
      return(makeRequest(request = '/api/v3/depth', parameters = paste0("?symbol=", symbol)))
    } else {
      if(!(limit %in% c(5, 10, 20, 50, 100, 500, 1000, 5000))) {
        warning("Available limits are 5, 10, 20, 50, 100, 500, 1000 or 5000 (max)")
        limit <- 5
      }
      return(makeRequest(request = '/api/v3/depth', parameters = paste0("?symbol=", symbol,  "&limit=", limit)))
    }
  } else {
    if(missing(limit)) {
      response <- content(makeRequest(request = '/api/v3/depth', parameters = paste0("?symbol=", symbol)))
      asks <- data.frame(do.call(rbind, response$asks))
      asks$X1 <- unlist(asks$X1)
      asks$X2 <- unlist(asks$X2)
      colnames(asks) <- c("price", "amount")
      bids <- data.frame(do.call(rbind, response$bids))
      bids$X1 <- unlist(bids$X1)
      bids$X2 <- unlist(bids$X2)
      colnames(bids) <- c("price", "amount")
      return(
        list("last_updated_id" = response$lastUpdatedId, 
             "asks" = asks, 
             "bids" = bids)
      )
    } else {
      if(!(limit %in% c(5, 10, 20, 50, 100, 500, 1000, 5000))) {
        warning("Available limits are 5, 10, 20, 50, 100, 500, 1000 or 5000 (max)")
        limit <- 5
      }
      response <- content(makeRequest(request = '/api/v3/depth', parameters = paste0("?symbol=", symbol,  "&limit=", limit)))
      asks <- data.frame(do.call(rbind, response$asks))
      asks$X1 <- unlist(asks$X1)
      asks$X2 <- unlist(asks$X2)
      colnames(asks) <- c("price", "amount")
      bids <- data.frame(do.call(rbind, response$bids))
      bids$X1 <- unlist(bids$X1)
      bids$X2 <- unlist(bids$X2)
      colnames(bids) <- c("price", "amount")
      return(
        list("last_updated_id" = response$lastUpdatedId, 
             "asks" = asks, 
             "bids" = bids)
      )
    }
  }
}
binanceRecentTradedList <- function(symbol, limit, parsed) {
  if(missing(parsed)) parsed <- FALSE
  if(!parsed) {
    if(missing(limit)) {
      return(makeRequest(request = '/api/v3/trades', parameters = paste0("?symbol=", symbol)))
    } else {
      if(limit > 1000) {
        warning("Maximum limit admited is 1000 Ther response will return 500 observations")
        limit <- 1000
      }
      return(makeRequest(request = '/api/v3/trades', parameters = paste0("?symbol=", symbol,  "&limit=", limit)))
    }
  } else {
    if(missing(limit)) {
      response <- makeRequest(request = '/api/v3/trades', parameters = paste0("?symbol=", symbol))
      traded <- data.frame(do.call(rbind, content(response)))
    } else {
      if(limit < 500) {
        warning("Minimum limit admited is 500. Ther response will return 500 observations")
        limit <- 500
      }
      response <- makeRequest(request = '/api/v3/trades', parameters = paste0("?symbol=", symbol,  "&limit=", limit))
      traded <- data.frame(do.call(rbind, content(response)))
    }
    traded <- data.frame(unlist(apply(traded, 2, unlist)))
    return(traded)
  }
}
binanceOldTradeLookup <- function(symbol, limit, id, parsed) {
  api_key <- getCredentials(name = "binance")
  if(missing(parsed)) parsed <- FALSE
  if(missing(id)) {
    if(!parsed) {
      if(missing(limit)) {
        return(makeRequest(request = '/api/v3/historicalTrades', parameters = paste0("?symbol=", symbol), api_key = api_key))
      } else {
        if(!(limit %in% c(500, 1000))) {
          warning("Limtis admited are 500 or 1000")
          limit <- 1000
        }
        return(makeRequest(request = '/api/v3/historicalTrades', parameters = paste0("?symbol=", symbol,  "&limit=", limit), api_key = api_key))
      }
    } else {
      if(missing(limit)) {
        response <- makeRequest(request = '/api/v3/historicalTrades', parameters = paste0("?symbol=", symbol), api_key = api_key)
        traded <- data.frame(do.call(rbind, content(response)))
      } else {
        if(!(limit %in% c(500, 1000))) {
          warning("Limtis admited are 500 or 1000")
          limit <- 1000
        }
        response <- makeRequest(request = '/api/v3/historicalTrades', parameters = paste0("?symbol=", symbol,  "&limit=", limit), api_key = api_key)
        traded <- data.frame(do.call(rbind, content(response)))
      }
      traded <- data.frame(unlist(apply(traded, 2, unlist)))
      return(traded)
    }
  } else {
    if(!parsed) {
      if(missing(limit)) {
        return(makeRequest(request = '/api/v3/historicalTrades', parameters = paste0("?symbol=", symbol), api_key = api_key))
      } else {
        if(!(limit %in% c(500, 1000))) {
          warning("Limtis admited are 500 or 1000")
          limit <- 1000
        }
        return(makeRequest(request = '/api/v3/historicalTrades', parameters = paste0("?symbol=", symbol,  "&limit=", limit), api_key = api_key))
      }
    } else {
      if(missing(limit)) {
        response <- makeRequest(request = '/api/v3/historicalTrades', parameters = paste0("?symbol=", symbol, "&fromId=", id), api_key = api_key)
        traded <- data.frame(do.call(rbind, content(response)))
      } else {
        if(!(limit %in% c(500, 1000))) {
          warning("Limtis admited are 500 or 1000")
          limit <- 1000
        }
        response <- makeRequest(request = '/api/v3/historicalTrades', parameters = paste0("?symbol=", symbol, "&fromId=", id,  "&limit=", limit), api_key = api_key)
        traded <- data.frame(do.call(rbind, content(response)))
      }
      traded <- data.frame(unlist(apply(traded, 2, unlist)))
      return(traded)
    }
  }
}
binanceSymbolPriceTricker <- function(symbol, parsed = F) {
  response <- makeRequest(request = '/api/v3/ticker/price', parameters = paste0("?symbol=", symbol))
  if(parsed) {
    return(content(response))
  } else {
    return(response)
  }
}

response <- binanceSymbolPriceTricker(symbol = "BTCUSDT", parsed = T)
content(response)
