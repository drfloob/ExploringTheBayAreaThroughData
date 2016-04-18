
library(gdata)
library(jsonlite)
library(RCurl)
library(tidyr)
library(data.table)
library(stringr)

data("zipcode")


# Parse zillow data by zip code
fn <- "zillowData.byZip.dump"
if(!file.exists(fn)) {
    zdatafn <- "zdata/Zip_Zhvi_SingleFamilyResidence.csv"
    zdataurl <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_SingleFamilyResidence.csv"
    if (!file.exists(zdatafn)) {
        download.file(zdataurl, zdatafn)
    }
    csv <- read.csv(zdatafn)
    sf <- csv[csv$Metro == "San Francisco" & csv$City %in% c("Oakland", "San Francisco", "San Mateo") & csv$X2016.02 < 900000,]
    sfsimp <- sf[,c("City", "RegionName", "X2016.02")]
    
} else {
    dump(c('sf', 'sfsimp'), fn)
    source(fn)
}

# pull google data
gfn <- "gData.dump"
if (!file.exists(gfn)) {
    credFile <- "credentials.R"
    if (!file.exists(credFile)) {
        stop("Missing credentials file: credentials.R. See README.md for formatting")
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
}
source(gfn)


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
            runsum <- 0
            g <- gdata[[n]]
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

commonLimits <- "$limit=500000&$where=%s between '2015-01-01T00:00:00.000' and '2016-03-31T23:59:59.999'"

pullCrime <- function(url, fn, dt) {
    l <- sprintf(commonLimits, dt)
    if (!file.exists(fn)) {
        download.file(paste(sep="", url, l), fn)
    }
    read.csv(fn, as.is = c("location_1", "location", "block_location"))
}

# SF crime API
sfcrimeurl <- "https://data.sfgov.org/resource/cuks-n6tp.csv?"
sfcrimefn <- "cdata/sfCrime.csv"
sfc <- pullCrime(sfcrimeurl, sfcrimefn, "date")

# Alameda County crime API
accrimeurl <- "https://data.acgov.org/resource/js8f-yfqf.csv?"
accrimefn <- "cdata/acCrime.csv"
acc <- pullCrime(accrimeurl, accrimefn, "datetime")

# Oakland crime API
oakcrimeurl <- "https://data.oaklandnet.com/resource/3xav-7geq.csv?"
oakcrimefn <- "cdata/oakCrime.csv"
oak <- pullCrime(oakcrimeurl, oakcrimefn, "datetime")

# Berkeley crime API
berkcrimeurl <- "https://data.cityofberkeley.info/resource/s24d-wsnp.csv?"
berkcrimefn <- "cdata/berkCrime.csv"
berk <- pullCrime(berkcrimeurl, berkcrimefn, "eventdt")

# San Mateo, Daly City, etc crime
# NOPE!


# Unified descriptions
sfc['d'] <- sfc$descript
acc['d'] <- acc$crimedescription
oak['d'] <- oak$description
berk['d'] <- berk$offense


crimDescGrepPattern <- "kidnap|weapons|violent|firearm|robbery|assault|homicide|stolen vehicle|vehicle theft"
## Multi-column search version
# getRelevantIndexes <- function(...) {
#   v <- list(...)
#   # dput(list('v', length(v), class(v)))
#   unlist(lapply(v, function(x) {
#     grep(crimDescGrepPattern, x, ignore.case = T)
#   }))
# }
getRelevantIndexes <- function(col) {
    grep(crimDescGrepPattern, col, ignore.case = T)
}





print("filtering crimes")

sfcImpCrimes <- sfc[getRelevantIndexes(sfc$d),]
accImpCrimes <- acc[getRelevantIndexes(acc$d),]
oakImpCrimes <- oak[getRelevantIndexes(oak$d),]
berkImpCrimes <- berk[getRelevantIndexes(berk$d),]




# data.table for zipcodes, role="nearest"
# turn "POINT( x y )" into latitude and longitude values
# convert lat/lng to zipcodes through data.table(data(zipcode), key=c("latitude", "longitude"))


getLatLon <- function(ds, var) {
    re <- "([-\\.[:alnum:]]+)\\s([-\\.[:alnum:]]+)"
    ds %>% extract_(var, c("longitude", "latitude"), regex=re, remove = FALSE, convert = TRUE)
}

print("parsting latitude / longitude")

sfcImpLL <- getLatLon(sfcImpCrimes, "location")
accImpLL <- getLatLon(accImpCrimes, "location_1")
oakImpLL <- getLatLon(oakImpCrimes, "location_1")
berkImpLL <- getLatLon(berkImpCrimes, "block_location")

zc <- data.table(zipcode, key=c("latitude", "longitude"))
sfcdt <- data.table(sfcImpLL, key=c("latitude", "longitude"))
accdt <- data.table(accImpLL, key=c("latitude", "longitude"))
oakcdt <- data.table(oakImpLL, key=c("latitude", "longitude"))
berkcdt <- data.table(berkImpLL, key=c("latitude", "longitude"))

getZipViaLstSqrz <- function(lng,lat) {
    vec <- with(zc, (lng-longitude)^2 + (lat-latitude)^2)
    zc[which.min(vec),]$zip
}
zipCrime <- function(dt) {
    dt %>% rowwise() %>% mutate(zip=getZipViaLstSqrz(longitude, latitude))
}


print("finding zip codes")

sfCrimeZipped <- zipCrime(sfcdt)
acCrimeZipped <- zipCrime(accdt)
oakCrimeZipped <- zipCrime(oakcdt)
berkCrimeZipped <- zipCrime(berkcdt)

print("summing crimes by zip")

sfCrimeCount <- sfCrimeZipped[,list(count=sum(!is.na(d))), by=zip]
acCrimeCount <- acCrimeZipped[,list(count=sum(!is.na(d))), by=zip]
oakCrimeCount <- oakCrimeZipped[,list(count=sum(!is.na(d))), by=zip]
berkCrimeCount <- berkCrimeZipped[,list(count=sum(!is.na(d))), by=zip]

