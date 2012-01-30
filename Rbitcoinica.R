#############################################################################
# Rbitcoinica v 0.1
#############################################################################
# Dislcaimer:  Use at your own risk.  We make no gaurantees that this will work.  Please
# be advised that bitcoin is highly volitile and you can lose money.
#
# Questions:  Visit http://www.stochastically.com
# Donations: 18DjCVYqbHo6vnEZvVkp9ZNNWHJFnHQoyw
#
# This software is released under the GNU General Public License,
# version 3 (GPL-3.0)
# http://www.opensource.org/licenses/gpl-3.0.htmlthis 
# 
# Purpose:  To utizilize the Bitcoinica API for utilization in R for statistical
# analysis and trading.
#
# R is a free software environemnt for statistical computing and graphics.
# It is platform independent.  To download see http://www.r-project.org/
#
# Bitcoin is a peer-to-peer digital currency that enables instant payments to anyone.
# For more information see http://www.bitcoin.org
#
# Version Changes:
# v.0.1 - just mimic the Bitcoinica API with R functions
#
#
#####################
# STARTING
#####################
# You need to download and/or load the packages used in these functions.
#install.packages("RCurl","rjson", "xts")
    require(RCurl)
    require(rjson)
    require(xts)

# Enter your Bitcoinica username and password in the R Global Environment.
# All URL requests and sends are done through HTTPS so it should be secure
# but use at your own risk.  Read the EULA at the top.

> username = "myusername"
> password = "mypassword"

######################
# Authentication
######################
# this allows you to access restricted areas.

    auth = function ( username=username, password =password ) {
        colon = ":"
        userpass = paste(username,colon,password,sep="")
        return (userpass)
        }

######################
# Ticker
######################
# Gets the latest bid and ask prices

    tick = function ( pair="BTCUSD" ) {
        x = auth ( )
        url1 = ("https://www.bitcoinica.com/api/")
        url2 = "quotes/"
        url4 = sprintf("%s%s%s", url1, url2, pair)
        y = fromJSON(getURI( url4, userpwd=x) )
        t = as.POSIXlt(Sys.time(),tz="GMT", origin="1970-01-01")  
        y = as.data.frame(y)
        y$Time = t
        y$spread = y$selling - y$buying
        return(y)
        }

######################
# Historical Quotes
######################
# Get historical of a total of 1000 max

    quotes.history = function ( pair="BTCUSD", n=100) {
        x = auth ( )

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "quotes/"
        url3 = "/history.json?n="
        url4 = sprintf("%s%s%s%s%d", url1, url2, pair, url3, n)
        y = fromJSON(getURI( url4, userpwd=x ) )
        y = do.call(rbind,lapply(y,as.data.frame))
        colnames(y) = c("buying","Time","pair","selling")
        y$Time <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',y$Time)
        # do as.POSIXlt with %z
        y$Time <- as.POSIXlt(y$Time, format="%Y-%m-%dT%H:%M:%S%z", 
                     origin="1970-01-01",tz="GMT")
        y = y[order(y$Time), ]
        return(y)
        }

######################
# Exchange Style Quotes
######################
#This API action is for those applications which strictly require last, low, high, volume, bid and ask. 
#We have converted our latest quotes in this format. Please note that last, low and high are midpoint prices, not tradable quotes. Please also remember that Bitcoinica never trades this way.

    quotes.exchange = function ( pair="BTCUSD" ) {
        x = auth ( )
        
        url1 = "https://www.bitcoinica.com/api/"
        url2 = "quotes/"
        url3 = "/compatible.json"
        url4 = sprintf("%s%s%s%s", url1, url2, pair, url3)
        y = fromJSON(getURI( url4, userpwd=x ) )
        y = as.data.frame(y)
        y$spread = y$ask - y$bid
        y$Time = as.POSIXlt(Sys.time(),tz="GMT", origin="1970-01-01") 
        return (y)
        }
        
        
######################
# Candlesticks
######################
## Returns Open-High-Low-Close data of a currency pair and period.

    candlesticks = function  ( pair="BTCUSD",period=3600, n=250 ) {
        x = auth ( )

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "candlesticks/"
        url3 = ".json?period="
        url4 = "&n="
        url5 = sprintf("%s%s%s%s%d%s%d", url1, url2, pair, url3, period, url4, n)
        y = fromJSON(getURI( url5, userpwd=x ) )
        y = do.call(rbind,lapply(y,as.data.frame))
        colnames(y) = c(".Close",".High",".Low",".Open","Time")
        y$Time <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',y$Time)
        # do as.POSIXlt with %z
        y$Time <- as.POSIXlt(y$Time, format="%Y-%m-%dT%H:%M:%S%z", 
                     origin="1970-01-01",tz="GMT")
        y = y[order(y$Time), ]
        y = xts(y[,-5], order.by=y[,5])
        return (y)
        }

########################
# Get Orders
########################
#this function gets your orders.  Set the number of orders as n, max 1000.

    orders.all = function (  n="5") {
    
        x = auth ( )

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "orders"
        url3 = ".json?"
        url4 = "n="
        url5 = sprintf("%s%s%s%s%d", url1, url2, url3, url4, n)
        y = fromJSON(getURI( url5, userpwd=x) )
        y = do.call(rbind,lapply(y,as.data.frame))
        colnames(y) = c("amount","Time","ID","pair","price","status","type","UpdateTime","UserID")
        y$Time <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',y$Time)
        # do as.POSIXlt with %z
        y$Time <- as.POSIXlt(y$Time, format="%Y-%m-%dT%H:%M:%S%z", 
                     origin="1970-01-01",tz="GMT")
        y$UpdateTime <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',y$UpdateTime)
        # do as.POSIXlt with %z
        y$UpdateTime <- as.POSIXlt(y$UpdateTime, format="%Y-%m-%dT%H:%M:%S%z", 
                     origin="1970-01-01",tz="GMT")
        y = y[order(y$Time), ]
        return(y)
        }

########################
# Active Orders
########################
# A list of active orders

    orders.active = function (  n="5") {
    
        x = auth ( )

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "orders/active"
        url3 = ".json?"
        url4 = "n="
        url5 = sprintf("%s%s%s%s%d", url1, url2, url3, url4, n)
        y = fromJSON(getURI( url5, userpwd=x) )
        y = do.call(rbind,lapply(y,as.data.frame))
        colnames(y) = c("amount","Time","ID","pair","price","status","type","UpdateTime","UserID")
        y$Time <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',y$Time)
        # do as.POSIXlt with %z
        y$Time <- as.POSIXlt(y$Time, format="%Y-%m-%dT%H:%M:%S%z", 
                     origin="1970-01-01",tz="GMT")
        y$UpdateTime <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',y$UpdateTime)
        # do as.POSIXlt with %z
        y$UpdateTime <- as.POSIXlt(y$UpdateTime, format="%Y-%m-%dT%H:%M:%S%z", 
                     origin="1970-01-01",tz="GMT")
        y = y[order(y$Time), ]
        return(y)
        }

########################
# Show Orders
########################
# A data frame of id, pair, price, amount, type, status, created_at, updated_at and user_id.

    orders.show = function (  orderid = 205060) {
    
        x = auth ( )

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "orders/"
        url3 = ".json"
        url4 = sprintf("%s%s%d%s", url1, url2, orderid, url3)
        
        y = fromJSON(getURI( url4, userpwd=x) )
        y = as.data.frame(y)
        colnames(y) = c("amount","Time","ID","pair","price","status","type","UpdateTime","UserID")
        y$Time <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',y$Time)
        # do as.POSIXlt with %z
        y$Time <- as.POSIXlt(y$Time, format="%Y-%m-%dT%H:%M:%S%z", 
                     origin="1970-01-01",tz="GMT")
        y$UpdateTime <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',y$UpdateTime)
        # do as.POSIXlt with %z
        y$UpdateTime <- as.POSIXlt(y$UpdateTime, format="%Y-%m-%dT%H:%M:%S%z", 
                     origin="1970-01-01",tz="GMT")
        return(y)
        }


########################
# Place Orders ----- STILL UNDER DEVELOPMENT
########################

    orders.place = function ( pair="BTCUSD", type, price, amount) {

        x = auth ( )

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "orders"
        url3 = ".json"
        url4 = sprintf("%s%s%s", url1, url2, url3)

        y = postForm(url4, pair=pair, type=type, price=price, 
        amount=amount, .opts = list(userpwd = x))
        
        }


########################
# CANCEL ORDERS ---- UNDER DEVELOPMENT
########################

        orders.cancel ( orderID ) {

        x = auth ( )

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "orders/"
        url3 = ".json"
        url4 = sprintf("%s%s%d%s", url1, url2, orderID, url3)

        postForm(url4, _method="DELETE", .opts = list(userpwd = x))

        postForm(url4, pair=pair, type=type, price=price, amount=amount, .opts = list(userpwd = x))
        }

########################
# GET POSITIONS
########################

    position.all ( n=100) {

        x = auth ( )

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "positions"
        url3 = ".json?n="
        url4 = sprintf("%s%s%s%d", url1, url2, url3, n)
        y = fromJSON(getURI( url4, userpwd=x) )
        y = do.call(rbind,lapply(y,as.data.frame))
        colnames(y) = c("amount","base","Time","ID","pair","status","UpdateTime","UserID")
        y$Time <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',y$Time)
        # do as.POSIXlt with %z
        y$Time <- as.POSIXlt(y$Time, format="%Y-%m-%dT%H:%M:%S%z", 
                     origin="1970-01-01",tz="GMT")
        return(y)
        }
    

########################
# ACTIVE POSITIONS  ----- UNDER DEVELOPMENT
########################
# # # Need to add if there is more than one position open

    position.active = function (  ) {

        x = auth (  )

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "positions/"
        url3 = "active.json"
        url4 = sprintf("%s%s%s", url1, url2, url3)
        y = fromJSON(getURI( url4, userpwd=x) )
        y = as.data.frame(y)
        colnames(y) = c("amount","base","Time","ID","pair","status","UpdateTime","UserID")
        y$Time <- gsub('([-+][0-9]{2}):([0-9]{2})$','\\1\\2',y$Time)
        # do as.POSIXlt with %z
        y$Time <- as.POSIXlt(y$Time, format="%Y-%m-%dT%H:%M:%S%z", 
                     origin="1970-01-01",tz="GMT")
        return(y)
        }

########################
# ACCOUNT INFORMATION
########################

    account.info = function {

        x = auth ( ) 

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "account.json"
        url3 = sprintf("%s%s", url1, url2 )
        y = as.data.frame(y)


########################
# LIQUIDATE
########################

    position.liquidate = function ( pair="BTCUSD", type="MARKET" ) {

        x = auth ( )
        y = position.active ( )
        amount = as.character(-y$amount)

        url1 = "https://www.bitcoinica.com/api/"
        url2 = "orders"
        url3 = ".json"
        url4 = sprintf("%s%s%s", url1, url2, url3)

        liq = postForm(url4, pair=pair, type=type, 
        amount=amount, .opts = list(userpwd = x))
        liq = as.data.frame(liq)
        
        }
        
        
        
