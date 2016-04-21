# to be called from within bayAreaZipcodeHeadMap.R, after merged dataset exists

library(jsonlite)
library(magrittr)
library(lawn)

zipDir <- "zipGeo"
if (!dir.exists(zipDir)) dir.create(zipDir)

zipFile <- file.path(zipDir, "tl_2010_06_zcta510.zip")
shapeFilename <- "tl_2010_06_zcta510.shp"

if (!file.exists(file.path(zipDir, shapeFilename))) {
    
    if (!file.exists(zipFile)) {
        stop("Zip shape data is not available.",
             "Download the 2010 California Zipcode data set from https://www.census.gov/cgi-bin/geo/shapefiles2010/main ",
             "and place it in the ", zipDir, " folder")
    }
    
    unzip(zipfile = zipFile, exdir = zipDir)
}

zipFile.gj <- file.path(getwd(), zipDir, "zcta5.json")
if (!file.exists(zipFile.gj)) {
    print("The next step requires you to install the GDAL tool: http://www.gdal.org/")
    print("At a command line, execute:")
    
    infile <- file.path(getwd(), zipDir, shapeFilename)
    print(sprintf("ogr2ogr -f \"GeoJSON\" %s %s", zipFile.gj, infile), quote = F)
}

# -------------------------------------------------------------------------------
#    Filter JSON

filteredZipFile <- file.path(zipDir, "filtered.save")
if (!file.exists(filteredZipFile)) {
    topoData <- readLines(zipFile.gj, warn = F) %>% paste(collapse = "\n") %>% fromJSON(simplifyVector = F)
    goodZips <- merged %>% group_by(zip) %>% summarise(count=n()) %>% filter(count > 10) %>% select(zip) %>% unlist
    
    topoData$features <- Filter(function(f) {
        f$properties$ZCTA5CE10 %in% goodZips
    }, topoData$features)

    # stolen from https://github.com/ropensci/lawn/blob/master/R/zzz.R
    convert <- function(x) { jsonlite::toJSON(unclass(x), auto_unbox = TRUE, digits = 22) }
    
    
    # lawn is being used to count the number of crimes that fall within each
    # zipcode shape, to correct for "nearest centroid" zipcode matching
    mergedGeoPoints <- lawn_featurecollection(apply(merged, 1, function(x) { lawn_point(as.numeric(c(x["longitude"], x["latitude"])))}))
    tdjson <- convert(topoData)
    mgpjson <- convert(mergedGeoPoints)
    
    lc <- lawn_count(tdjson, mgpjson)
    
    ## WOOOO! lc$features$properties$pt_count
    
    save(topoData, file = filteredZipFile)
} else {
    load(filteredZipFile, verbose = TRUE)
}