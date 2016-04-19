
library(gdata)
library(jsonlite)
library(RCurl)
library(tidyr)
library(dplyr)
library(data.table)
library(stringr)
library(zipcode)
library(leaflet)
library(httr)

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


# -------------------------------------------------------------------------------
#    Gather crime data

commonLimits <- paste0("$limit=500000&$where=%s between '2015-01-01T00:00:00.000' and '2016-03-31T23:59:59.999'&$$app_token=", appToken)
pullCrime <- function(url, fn, dt) {
    l <- sprintf(commonLimits, dt)
    url <- paste0(url, l)
    print(url)
    if (!file.exists(fn)) {
        x <- httr::GET(url=URLencode(url))
        write.csv(content(x, col_names=TRUE, col_types=NULL), file = fn)
    }
    read.csv(fn, colClasses = "character")
}

pullCrimeGraphics <- function(url, fn, postData, dt, dateFormat, as.is=NULL) {
    if (!file.exists(fn)) {
        resp <- POST(url, body = postData, encode = "json", add_headers("Content-Type"="application/json", Accept="application/json", "Content-Length"=str_length(postData)))
        df <- do.call(rbind.data.frame, c(content(resp)$d, stringsAsFactors=FALSE))
        write.csv(df, file=fn)
    }
    df <- read.csv(fn, as.is=as.is)

    # CrimeGraphics nests multiple crimes in the same location into a single
    # observation. This code undoes that.
    df <- unnest(df, Description = lapply( 
                     strsplit(Description, split="<big>", fixed = TRUE),
                     function(x){x[-1]}))
    df
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

# Daly City crime (not an official API)
dccrimeurl <- "http://dcpd.crimegraphics.com/2013/MapData.asmx/GetMapPoints"
dccrimefn <- "cdata/dcCrime.csv"
dccrimepost <- '{"AGCODE":"DCPD","StartDate":"01/01/2016","EndDate":"03/31/2016","MapType":"I","GroupTypes":"851,459,240,211","CirLat":0,"CirLon":0,"CirRad":0}'
dcdateformat <- "%m/%d/%Y %H:%M:%S %p"
dcasis <- c("Description", "Title", "Group", "TabTitle", "Location", "Icon", "Shadow", "Status", "TimeOpened", "DateClosed", "TimeClosed")
dc <- pullCrimeGraphics(dccrimeurl, dccrimefn, dccrimepost, "DateOpened", dcdateformat, dcasis)

# San Mateo Country Sheriff (not an official API)
smccrimeurl <- "http://smso.crimegraphics.com/2013/MapData.asmx/GetMapPoints"
smccrimefn <- "cdata/smcCrime.csv"
smccrimepost <- '{"AGCODE":"SMSO","StartDate":"01/01/2016","EndDate":"03/31/2016","MapType":"C","GroupTypes":"HOMICIDE,MANSLAU,ROBBERY,ASSAULT,BURGLARY,STOLVEH,ATTBURG","CirLat":0,"CirLon":0,"CirRad":0}'
smcdateformat <- "%m/%d/%Y %H:%M:%S %p"
smcasis <- c("Description", "Title", "Group", "TabTitle", "Location", "Icon", "Shadow")
smc <- pullCrimeGraphics(smccrimeurl, smccrimefn, smccrimepost, "DateOpened", smcdateformat, smcasis)

# Redwood City
rwccrimeurl <- "https://moto.data.socrata.com/resource/9wfx-9qes.csv?"
rwccrimefn <- "cdata/rwcCrime.csv"
rwc <- pullCrime(rwccrimeurl, rwccrimefn, "incident_datetime")

# Menlo Park Police Dept.
mpcrimeurl <- "https://moto.data.socrata.com/resource/ex86-feqy.csv?"
mpcrimefn <- "cdata/mpCrime.csv"
mp <- pullCrime(mpcrimeurl, mpcrimefn, "incident_datetime")


# -------------------------------------------------------------------------------
#    Tidying data
   

print("unifying descriptions")
sfc <- mutate(sfc, description=as.character(descript))
acc <- mutate(acc, description=as.character(crimedescription))
oak <- mutate(oak, description=as.character(description))
berk <- mutate(berk, description=as.character(offense))
dc <- mutate(dc, description=as.character(Description))
smc <- mutate(smc, description=as.character(Description))
rwc <- mutate(rwc, description=as.character(incident_description))
mp <- mutate(mp, description=as.character(incident_description))



print("parsting latitude / longitude")
getLatLon <- function(ds, var) {
    re <- "([-\\.[:alnum:]]+)\\s([-\\.[:alnum:]]+)"
    ds %>% extract_(var, c("longitude", "latitude"), regex=re, remove = FALSE, convert = TRUE)
}
sfc <- getLatLon(sfc, "location")
acc <- getLatLon(acc, "location_1")
oak <- getLatLon(oak, "location_1")
berk <- getLatLon(berk, "block_location")
dc <- rename(dc, longitude=Longitude, latitude=Latitude)
smc <- rename(smc, longitude=Longitude, latitude=Latitude)
# rwc already has latitude & longitude values that are populated whenever
# location is also populated. Well done, guys!
# rwc <- getLatLon(rwc, "location")

# same with menlo park?



dateformat.socrata <- "%Y-%m-%dT%H:%M:%S"
dateformat.crimegraphics <- "%m/%d/%Y %H:%M:%S %p"

print("unifying dates")
sfc$date <- as.POSIXct(strptime(sfc$date, dateformat.socrata))
acc$date <- as.POSIXct(strptime(acc$datetime, dateformat.socrata))
oak$date <- as.POSIXct(strptime(oak$datetime, dateformat.socrata))
berk$date <- as.POSIXct(strptime(berk$eventdt, dateformat.socrata))
dc$date <- as.POSIXct(strptime(dc$DateOpened, dateformat.crimegraphics))
smc$date <- as.POSIXct(strptime(smc$DateOpened, dateformat.crimegraphics))
rwc$date <- as.POSIXct(strptime(rwc$incident_datetime, dateformat.socrata))
mp$date <- as.POSIXct(strptime(mp$incident_datetime, dateformat.socrata))


print("annotating datasets with origin key")
sfc$origin <- "sfc"
acc$origin <- "acc"
oak$origin <- "oak"
berk$origin <- "berk"
dc$origin <- "dc"
smc$origin <- "smc"
rwc$origin <- "rwc"
mp$origin <- "mp"


print("merging data")
merged <- Reduce(function(x,y) {
    rbind(select(x, origin, description, date, latitude, longitude),
          select(y, origin, description, date, latitude, longitude))
}, list(sfc,acc,oak,berk,dc,smc,rwc,mp)) %>% mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude))


print("removing NA lat/lng")
# TODO: lookup lat/lng from street names, most are available
merged <- filter(merged, !is.na(latitude) & !is.na(longitude))


print("filtering down to pertinent crime types")
crimeDescGrepPattern <- "kidnap|weapon|violent|firearm|robbery|assault|homicide|stolen vehicle|knife|cut|vehicle theft"
merged <- rowwise(merged) %>% filter(grepl(crimeDescGrepPattern, description, ignore.case=TRUE))
    
    

print("associating zip codes with lat/lng")
zc <- data.table(zipcode, key=c("latitude", "longitude"))
merged <- data.table(merged, key=c("latitude", "longitude"))

getZipViaLstSqrz <- function(lng,lat) {
    vec <- with(zc, (lng-longitude)^2 + (lat-latitude)^2)
    zc[which.min(vec),]$zip
}

merged <- merged %>% rowwise() %>% mutate(zip=getZipViaLstSqrz(longitude, latitude))




print("mapping crimes")

m <- leaflet(data=merged)
m <- setView(m, lat=37.65, lng=-122.23, zoom=9)
m <- addProviderTiles(m, "CartoDB.Positron")
m <- addMarkers(m, lng=~longitude, lat=~latitude, clusterOptions = markerClusterOptions(maxClusterRadius=30))
print(m)
