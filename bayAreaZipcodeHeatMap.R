# install.packages(c("gdata", "jsonline", "RCurl", "tidyr", "dplyr", "data.table", "stringr", "zipcode", "leaflet", "httr", "readr", "lawn", "RColorBrewer"))

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
library(RColorBrewer)


data("zipcode")

credFile <- "credentials.R"
source(credFile)


# -------------------------------------------------------------------------------
#    Gather Zillow Data


# Parse zillow data by zip code
fn <- "zillowData.byZip.dump"
if(!file.exists(fn)) {
    zdatafn <- "zdata/Zip_Zhvi_SingleFamilyResidence.csv"
    zdataurl <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_SingleFamilyResidence.csv"
    
    if (!dir.exists(dirname(zdatafn))) {
      dir.create(dirname(zdatafn), recursive = TRUE)
    }
    if (!file.exists(zdatafn)) {
        download.file(zdataurl, zdatafn, method = "libcurl")
    }
    zdata <- read.csv(zdatafn) %>% 
        filter(Metro == "San Francisco") %>% 
        select(city=City, zip=RegionName, zhvi=X2016.02)
    
} else {
    dump(c('sf', 'sfsimp'), fn)
    source(fn)
}




# -------------------------------------------------------------------------------
#    Gather crime data

if (!dir.exists("cdata/")) {
    dir.create("cdata")
}

commonLimits <- paste0("$limit=500000&$where=%s between '2016-01-01T00:00:00.000' and '2016-03-31T23:59:59.999'&$$app_token=", appToken)
pullCrime <- function(url, fn, dt, cl=commonLimits) {
    l <- sprintf(cl, dt)
    url <- paste0(url, l)
    # print(url)
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

# Oakland crime API
# special, since they only provide 90 days of rolling crime data
oakcrimeurl <- "https://data.oaklandnet.com/resource/3xav-7geq.csv?"
oakcrimefn <- "cdata/oakCrime.csv"
oakcl <- paste0("$limit=500000&$$app_token=", appToken)
oak <- pullCrime(oakcrimeurl, oakcrimefn, "datetime", oakcl)

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

# Redwood City
rwccrimeurl <- "https://moto.data.socrata.com/resource/9wfx-9qes.csv?"
rwccrimefn <- "cdata/rwcCrime.csv"
rwc <- pullCrime(rwccrimeurl, rwccrimefn, "incident_datetime")

# Menlo Park Police Dept.
mpcrimeurl <- "https://moto.data.socrata.com/resource/ex86-feqy.csv?"
mpcrimefn <- "cdata/mpCrime.csv"
mp <- pullCrime(mpcrimeurl, mpcrimefn, "incident_datetime")

# Piedmont
piedcrimeurl <- "https://moto.data.socrata.com/resource/p52h-m2j9.csv?"
piedcrimefn <- "cdata/piedmontCrime.csv"
pied <- pullCrime(piedcrimeurl, piedcrimefn, "incident_datetime")

# Emeryville
emcrimeurl <- "https://moto.data.socrata.com/resource/icdc-r3z6.csv?"
emcrimefn <- "cdata/emCrime.csv"
em <- pullCrime(emcrimeurl, emcrimefn, "incident_datetime")

# San Leandro
slcrimeurl <- "https://moto.data.socrata.com/resource/6nbc-apvm.csv?"
slcrimefn <- "cdata/slCrime.csv"
sl <- pullCrime(slcrimeurl, slcrimefn, "incident_datetime")

# Albany
albanycrimeurl <- "https://moto.data.socrata.com/resource/n5yu-6kr8.csv?"
albanycrimefn <- "cdata/albanyCrime.csv"
albany <- pullCrime(albanycrimeurl, albanycrimefn, "incident_datetime")

# El Cerrito
eccrimeurl <- "https://moto.data.socrata.com/resource/a9zr-qcds.csv?"
eccrimefn <- "cdata/ecCrime.csv"
ec <- pullCrime(eccrimeurl, eccrimefn, "incident_datetime")

# Lafayette
lfcrimeurl <- "https://moto.data.socrata.com/resource/bxwm-gjq4.csv?"
lfcrimefn <- "cdata/lfCrime.csv"
lf <- pullCrime(lfcrimeurl, lfcrimefn, "incident_datetime")

# Campbell
campbellcrimeurl <- "https://moto.data.socrata.com/resource/shz2-ig3z.csv?"
campbellcrimefn <- "cdata/capmbellCrime.csv"
campbell <- pullCrime(campbellcrimeurl, campbellcrimefn, "incident_datetime")

# Mountain View
mtvcrimeurl <- "https://moto.data.socrata.com/resource/k56q-mt3c.csv?"
mtvcrimefn <- "cdata/mtvCrime.csv"
mtv <- pullCrime(mtvcrimeurl, mtvcrimefn, "incident_datetime")

# Union City
uccrimeurl <- "https://moto.data.socrata.com/resource/xqci-zc8x.csv?"
uccrimefn <- "cdata/ucCrime.csv"
uc <- pullCrime(uccrimeurl, uccrimefn, "incident_datetime")

# Fremont
frcrimeurl <- "https://moto.data.socrata.com/resource/nnzs-rxi5.csv?"
frcrimefn <- "cdata/frCrime.csv"
fr <- pullCrime(frcrimeurl, frcrimefn, "incident_datetime")

# Dublin
dublincrimeurl <- "https://moto.data.socrata.com/resource/ifng-r995.csv?"
dublincrimefn <- "cdata/dublinCrime.csv"
dublin <- pullCrime(dublincrimeurl, dublincrimefn, "incident_datetime")

# Pleasant Hill
phcrimeurl <- "https://moto.data.socrata.com/resource/i8a5-7vrs.csv?"
phcrimefn <- "cdata/phCrime.csv"
ph <- pullCrime(phcrimeurl, phcrimefn, "incident_datetime")

# Martinez
martcrimeurl <- "https://moto.data.socrata.com/resource/vpaa-2jww.csv?"
martcrimefn <- "cdata/martinezCrime.csv"
mart <- pullCrime(martcrimeurl, martcrimefn, "created_at")



# Counties - potentially difficult to integrate with city police data for this project

# Alameda County crime API
accrimeurl <- "https://moto.data.socrata.com/resource/bvi2-5rde.csv?"
accrimefn <- "cdata/acCrime.csv"
acc <- pullCrime(accrimeurl, accrimefn, "incident_datetime")



# -------------------------------------------------------------------------------
#    Tidying data
   

print("unifying descriptions")
sfc <- mutate(sfc, description=paste(sep=" - ", as.character(category), as.character(descript)))
oak <- mutate(oak, description=paste(sep=" - ", as.character(crimetype), as.character(description)))
berk <- mutate(berk, description=paste(sep=" - ", as.character(cvlegend), as.character(offense)))
dc <- mutate(dc, description=paste(sep=" - ", as.character(Title), as.character(TabTitle), as.character(Description)))
rwc <- mutate(rwc, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
mp <- mutate(mp, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
pied <- mutate(pied, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
em <- mutate(em, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
sl <- mutate(sl, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
albany <- mutate(albany, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
ec <- mutate(ec, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
lf <- mutate(lf, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
campbell <- mutate(campbell, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
mtv <- mutate(mtv, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
uc <- mutate(uc, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
fr <- mutate(fr, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
dublin <- mutate(dublin, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
ph <- mutate(ph, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
mart <- mutate(mart, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))
acc <- mutate(acc, description=paste(sep=" - ", as.character(incident_type_primary), as.character(parent_incident_type), as.character(incident_description)))




# counties
#smc <- mutate(smc, description=as.character(Description))


print("parsting latitude / longitude")
getLatLon <- function(ds, var) {
    re <- "([-\\.[:alnum:]]+)\\s([-\\.[:alnum:]]+)"
    ds %>% extract_(var, c("longitude", "latitude"), regex=re, remove = FALSE, convert = TRUE)
}
sfc <- getLatLon(sfc, "location")
oak <- getLatLon(oak, "location_1")
berk <- getLatLon(berk, "block_location")
dc <- rename(dc, longitude=Longitude, latitude=Latitude)
# rwc done
# mp done
# pied done
# em done
# sl done
# albany done
# ec done
# lf done
# capmbell done
# mtv done
# uc done
# fr done
# dublin done
# ph done
# mart done
# acc done


# counties
#smc <- rename(smc, longitude=Longitude, latitude=Latitude)



dateformat.socrata <- "%Y-%m-%d %H:%M:%S"
dateformat.crimegraphics <- "%m/%d/%Y %H:%M:%S %p"

print("unifying dates")
sfc$date <- as.POSIXct(strptime(paste(sfc$date, sfc$time), "%Y-%m-%d %H:%M"))
oak$date <- as.POSIXct(strptime(oak$datetime, dateformat.socrata))
berk$date <- as.POSIXct(strptime(paste(berk$eventdt, berk$eventtm), "%Y-%m-%d %H:%M"))
dc$date <- as.POSIXct(strptime(dc$DateOpened, dateformat.crimegraphics))
rwc$date <- as.POSIXct(strptime(rwc$incident_datetime, dateformat.socrata))
mp$date <- as.POSIXct(strptime(mp$incident_datetime, dateformat.socrata))
pied$date <- as.POSIXct(strptime(pied$incident_datetime, dateformat.socrata))
em$date <- as.POSIXct(strptime(em$incident_datetime, dateformat.socrata))
sl$date <- as.POSIXct(strptime(sl$incident_datetime, dateformat.socrata))
albany$date <- as.POSIXct(strptime(albany$incident_datetime, dateformat.socrata))
ec$date <- as.POSIXct(strptime(ec$incident_datetime, dateformat.socrata))
lf$date <- as.POSIXct(strptime(lf$incident_datetime, dateformat.socrata))
campbell$date <- as.POSIXct(strptime(campbell$incident_datetime, dateformat.socrata))
mtv$date <- as.POSIXct(strptime(mtv$incident_datetime, dateformat.socrata))
uc$date <- as.POSIXct(strptime(uc$incident_datetime, dateformat.socrata))
fr$date <- as.POSIXct(strptime(fr$incident_datetime, dateformat.socrata))
dublin$date <- as.POSIXct(strptime(dublin$incident_datetime, dateformat.socrata))
ph$date <- as.POSIXct(strptime(ph$incident_datetime, dateformat.socrata))
mart$date <- as.POSIXct(strptime(mart$created_at, dateformat.socrata))
acc$date <- as.POSIXct(strptime(acc$incident_datetime, dateformat.socrata))

# normalize oakland crime dates to 1/1 to 3/31. This is so we can filter out
# wildly inaccurate dates later on.
oakdtdiff <- min(oak$date, na.rm = T) - as.POSIXct("2016-01-01")
oak <- mutate(oak, date = date - oakdtdiff)



# counties
#smc$date <- as.POSIXct(strptime(smc$DateOpened, dateformat.crimegraphics))


print("annotating datasets with origin key")
sfc$origin <- "sfc"
oak$origin <- "oak"
berk$origin <- "berk"
dc$origin <- "dc"
rwc$origin <- "rwc"
mp$origin <- "mp"
pied$origin <- "pied"
em$origin <- "em"
sl$origin <- "sl"
albany$origin <- "albany"
ec$origin <- "ec"
lf$origin <- "lf"
campbell$origin <- "campbell"
mtv$origin <- "mtv"
uc$origin <- "uc"
fr$origin <- "fr"
dublin$origin <- "dublin"
ph$origin <- "ph"
mart$origin <- "mart"
acc$origin <- "acc"


# counties
#smc$origin <- "smc"





print("merging data")
merged <- Reduce(function(x,y) {
    rbind(select(x, origin, description, date, latitude, longitude),
          select(y, origin, description, date, latitude, longitude))
}, list(sfc,oak,berk,dc,rwc,mp,pied,em,sl,albany,ec,lf,campbell,mtv,uc,fr,dublin,ph,mart,acc)) %>% 
    mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude))

print("refiltering by date") # just in case
merged <- filter(merged, date >= "2016-01-01 00:00" & date < "2016-04-01 00:00")


print("removing NA lat/lng")
# TODO: lookup lat/lng from street names, most are available
merged <- filter(merged, !is.na(latitude) & !is.na(longitude))


print("filtering down to pertinent crime types")
crimeDescGrepPattern <- "kidnap|weapon|violent|firearm|gun|deadly|weapon|strongarm|homicide|knife|murder"
merged <- rowwise(merged) %>% filter(grepl(crimeDescGrepPattern, description, ignore.case=TRUE))


print("filtering out false positives for violent crime")
falsePositiveCrimeDescGrepPattern <- "no weapon|poss|carry|turn"
merged <- rowwise(merged) %>% filter(!grepl(falsePositiveCrimeDescGrepPattern, description, ignore.case=TRUE))


print("associating zip codes with lat/lng")
zc <- data.table(zipcode, key=c("latitude", "longitude"))
merged <- data.table(merged, key=c("latitude", "longitude"))

getZipViaLstSqrz <- function(lng,lat) {
    vec <- with(zc, (lng-longitude)^2 + (lat-latitude)^2)
    zc[which.min(vec),]$zip
}

merged <- merged %>% rowwise() %>% mutate(zip=getZipViaLstSqrz(longitude, latitude))


# NOTES!!!
# 
# Vehicle crimes get reported all over the place. Probably due to vehicle
# recovery some place way out of town. Removing vehicle crimes to see if it
# clear up the crime picture.
# 
# remapping zipcodes for zips with crimes < 20
# 
# The idea: the rank of the first quartile is 54.75. 80% of zip codes have more 
# than 20 crimes listed. Upon inspection of zipcodes for which there are <= 10 
# crimes, all appear to be reported by police from another city. Most for 
# vehicle theft or recovery, some for domestic abuse situations. I think it's 
# safe to exclude these zip codes from comparison since there is no proper crime
# data from these cities.

# -------------------------------------------------------------------------------
#    Pull Google Maps Data by Zipcode

print("pulling google maps - public transportation times")
gfn <- "gData.dump"
if (!file.exists(gfn)) {
    if (!file.exists(credFile)) {
        stop("Missing credentials file: credentials.R. See README.md for formatting")
    }
    
    arrival_time <- ymd_h("2016-05-02 9am")
    url <- function(zip) {
        root <- "https://maps.googleapis.com/maps/api/directions/json?"
        u <- paste(sep="", 
                   root, 
                   "origin=", zip,
                   "&destination=225 Bush St, San Francisco, CA",
                   "&mode=transit",
                   "&avoid=tolls",
                   paste0("&arrival_time=", as.numeric(arrival_time)),
                   "&key=", apiKey)
        URLencode(u)
    }
    gdata <- list()

    uniqueZips <- unique(merged$zip)
    for (i in seq_along(uniqueZips)) {
        print(uniqueZips[i])
        doc <- fromJSON(url(uniqueZips[i]))
        gdata[[as.character(uniqueZips[i])]] <- doc
    }
    
    save(gdata, file = gfn)
} else {
    load(file = gfn)
}

avgRouteDuration <- function(routes, i) {
    ret <- Reduce(function(s, leg) {
        v <- Reduce(function(t, step) {
            # exclude walk time from transit directions. It's the biggest source
            # of variability in transit durations since it's the slowest transit
            # method by an order of magnitude.
            tmp <- step[step$travel_mode != "WALKING",]
            sum(tmp$duration$value)
        }, leg$steps, 0)
        s <- s + v
        s
    }, routes[i,"legs"], 0)
    #print(list(ret=ret))
    ret
}
routefn <- "gdata.routes.dump"
if (!file.exists(routefn)) {
    distances <- data.frame(zip=numeric(), distance=numeric())
    # avg minutes per trip
    if (!dir.exists("gdata")) {
        dir.create("gdata")
    }
    for (n in names(gdata)) {
        vname <- paste(sep="", "g", n)
        fn <- paste0("gdata/", n, ".dump")
        if (file.exists(fn)) {
            source(fn)
        } else {
            runsum <- 0
            g <- gdata[[n]]
            runcnt <- 0
            #print(list(lengthOfGRoutes = length(g$routes)))
            if (length(g$routes) == 0) {
                runsum<-99999
                runcnt<-1
            } else {
                for (r in 1:nrow(g$routes)) {
                    s <- avgRouteDuration(g$routes, r)
                    runsum <- runsum + s
                    runcnt <- runcnt + 1
                }
            }
            # dput(list(n, sprintf("%0.2f", runsum/runcnt/60)))
            assign(vname, list(zip=as.numeric(n), distance=runsum/runcnt/60))
            dump(vname, file=fn)
        }
        
        distances <- rbind(distances, get(vname))
    }
    save(distances, file = routefn)
} else {
    load(routefn)
}





# -------------------------------------------------------------------------------
#    Mapping


print("mapping crimes")

mapPopups <- with(merged, sprintf("(%f,%f) %s [zip: %s][origin: %s]", latitude, longitude, description, zip, origin))

# TEST filtering out sub-10 reported crimes
# subN.n <- 30
# subN <- merged[merged$zip %in% unlist(merged %>% group_by(zip) %>% summarise(count = n()) %>% filter(count <= subN.n) %>% select(zip)),]
# mapPopups <- with(subN, sprintf("(%f,%f) %s [zip: %s][origin: %s]", latitude, longitude, description, zip, origin))
# m <- leaflet(data=subN)
# end TEST

print("runing gatherZipcodeShapes.R")
source("gatherZipcodeShapes.R")

# -------------------------------------------------------------------------------
# zip code shapes and clustered crimes with popups
m <- leaflet(data=merged)
m <- setView(m, lat=37.65, lng=-122.23, zoom=9)
m <- addProviderTiles(m, "CartoDB.Positron")
m <- addMarkers(m, lng=~longitude, lat=~latitude, clusterOptions = markerClusterOptions(maxClusterRadius=30), popup = mapPopups)
m <- addGeoJSON(map=m, geojson = topoData, color = "#333", opacity = "0.6", weight = 2)
print(m)

# -------------------------------------------------------------------------------
# Choropleth map of crime distribution by zipcode
#
# inspiration: 
#  * http://www.rpubs.com/enzoma/79630, 
#  * http://rpubs.com/jcheng/us-states-2, 
#  * https://rstudio.github.io/leaflet/json.html
#
# CA zipcode shapes: generated using instructions from
# https://github.com/jgoodall/us-maps
# 
# colored zips: black to bright green (black is crimey, bright green is ok)

pal <- colorNumeric(c("green", "black"), domain = range(lapply(topoData$features, function(f) {f$properties$pt_count})))

td2 <- topoData
td2$style <- list(
    fillOpacity = 0.8
)
td2$features <- lapply(td2$features, function(feat) {
    feat$properties$style <- list(fillColor = pal(as.numeric(feat$properties$pt_count)))
    # feat$properties$popupContent <- paste("Crime count: ", feat$properties$pt_count)
    feat
})

m <- leaflet(data=merged)
m <- setView(m, lat=37.70, lng=-122.25, zoom=11)
m <- addProviderTiles(m, "CartoDB.Positron")
m <- addGeoJSON(map=m, geojson = td2, color = "#333", opacity = "0.75", weight = 2)
m <- addLegend(m, "bottomleft", pal = pal, title="Crimes, Q1 2016", opacity = 0.8,
               values = unlist(lapply(topoData$features, function(f) {f$properties$pt_count})))
print(m)



# -------------------------------------------------------------------------------
# Choropleth map of public transportation time
pal.range <- c(0,90)
pal <- colorNumeric(c("green", "black", "red"), domain = pal.range)

td.dist <- topoData
td.dist$style <- list(
    fillOpacity = 0.8
)
td.dist$features <- lapply(td.dist$features, function(feat) {
    feat$properties$style <- list(fillColor = pal(as.numeric(distances[distances$zip == feat$properties$ZCTA5CE10,"distance"])))
    feat
})


m <- leaflet()
m <- setView(m, lat=37.70, lng=-122.25, zoom=11)
m <- addProviderTiles(m, "CartoDB.Positron")
m <- addGeoJSON(map=m, geojson = td.dist, color = "#333", opacity = "0.75", weight = 2)
m <- addLegend(m, "bottomleft", pal = pal, title="Public Transit<br />Time (minutes)", opacity = 0.8, values = pal.range)
print(m)


# -------------------------------------------------------------------------------
# Choropleth map of home value indexes (Zillow)

pal.breaks <- c(0,5,8,13,18,100)*100000
x <- cut(zdata$zhvi, breaks=pal.breaks)
pal.qtl <- as.vector(table(x)) / sum(as.vector(table(x)))
lapply(seq_along(pal.qtl), function(i) {sum(pal.qtl[1:i])}) %>% unlist -> pal.qtl

pal.range = range(zdata$zhvi)
pal.pal <- brewer.pal(n=4, "RdYlGn") %>% rev
pal <- colorQuantile(pal.pal, domain = zdata$zhvi, probs = c(0, pal.qtl))
# previewColors(pal, arrange(zdata, zhvi)$zhvi)

td.z <- topoData
td.z$style <- list(
    fillOpacity = 0.8
)
td.z$features <- lapply(td.z$features, function(feat) {
    feat$properties$style <- list(fillColor = pal(zdata[zdata$zip == feat$properties$ZCTA5CE10,"zhvi"]))
    feat
})

m <- leaflet()
m <- setView(m, lat=37.70, lng=-122.25, zoom=11)
m <- addProviderTiles(m, "CartoDB.Positron")
m <- addGeoJSON(map=m, geojson = td.z, color = "#333", opacity = "0.75", weight = 2)
m <- addLegend(m, "bottomleft", pal = pal, title="ZHVI", opacity = 0.8, values = zdata$zhvi,
               labFormat = function(type, qtl, p) {
                   fmt <- function(x) {
                       if (x < 1000000)
                           paste0("$", x %/% 1000, "K")
                       else
                           sprintf("$%0.1fM", x / 1000000)
                   }
                   ret <- c()
                   qtl <- as.vector(qtl)
                   for (i in 2:length(qtl)) {
                       ret <- c(ret, paste(sep=" - ", fmt(qtl[i-1]), fmt(qtl[i])))
                   }
                   ret
               })
print(m)


