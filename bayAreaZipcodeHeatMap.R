library(gdata)
library(jsonlite)
library(RCurl)


# Parse zillow data by zip code
fn <- "zillowData.byZip.dump"
if(!file.exists(fn)) {
  csv <- read.csv("zdata/Zip_Zhvi_SingleFamilyResidence.csv")
  sf <- csv[csv$Metro == "San Francisco" & csv$City %in% c("Oakland", "San Francisco", "San Mateo") & csv$X2016.02 < 900000,]
  sfsimp <- sf[,c("City", "RegionName", "X2016.02")]
  
  dump(c('sf', 'sfsimp'), fn)
} else {
  source(fn)
}

# pull google data
gfn <- "gData.dump"
if (!file.exists(gfn)) {
  credFile <- "credentials.dump"
  if (!file.exists(credFile)) {
    stop("Missing credentials file: credentials.dump. See README.md for formatting")
  }
  source(credFile)
  
  url <- function(zip) {
    root <- "https://maps.googleapis.com/maps/api/directions/json?"
    u <- paste(sep="", 
               root, 
               "origin=", zip,
               "&destination=225 Bush St, San Francisco, CA",
               "&mode=transit",
               "&key=", apiKey)
    URLencode(u)
  }
  
  gdata <- list()
  rn <- sfsimp$RegionName
  for (i in seq_along(rn)) {
    
    print(rn[i])
    doc <- fromJSON(url(rn[i]))
    gdata[[as.character(rn[i])]] <- doc
  }
  
  dump(c('gdata'), gfn)
} else {
  source(gfn)
}


avgRouteDuration <- function(routes, i) {
  Reduce(function(s, l) {
    v = l$duration$value
    if(!is.null(v)) {
      s <- s + v
    }
    s
  }, routes[i,"legs"], 0)
}

routefn <- "gdata.routes.dump"
if (!file.exists(routefn)) {
  distances <- data.frame(zip=numeric(), distance=numeric())
  # avg minutes per trip
  for (n in names(gdata)) {
    vname <- paste(sep="", "g", n)
    fn <- paste(sep="", "gdata/", n, ".dump")
    if (file.exists(fn)) {
      source(fn)
    } else {
      g <- gdata[[n]]
      runsum <- 0
      runcnt <- 0
      # dput(list('nrow routes', nrow(g$routes)))
      for (r in 1:nrow(g$routes)) {
        s <- avgRouteDuration(g$routes, r)
        runsum <- runsum + s
        runcnt <- runcnt + 1
      }
      # dput(list(n, sprintf("%0.2f", runsum/runcnt/60)))
      assign(vname, list(zip=as.numeric(n), distance=runsum/runcnt/60))
      dump(vname, file=fn)
    }

    distances <- rbind(distances, get(vname))
  }
  dump(c('distances'), routefn)
} else {
  source(routefn)
}



final <- cbind(distances, midCost=sfsimp[sfsimp$RegionName == distances$zip, "X2016.02"])



# SF crime API
# https://data.sfgov.org/resource/cuks-n6tp.csv?$limit=50000&$where=date between '2016-01-01' and '2016-03-31'


# Oakland crime API
# https://data.acgov.org/resource/js8f-yfqf.csv?$limit=50000&$where=DateTime between '2016-01-01' and '2016-03-31'