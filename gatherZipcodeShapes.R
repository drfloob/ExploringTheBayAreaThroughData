library(jsonlite)
library(magrittr)

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
    source("gData.dump")
    goodZips <- names(gdata)
    topoData$features <- Filter(function(f) {
        f$properties$ZCTA5CE10 %in% goodZips
    }, topoData$features)
    
    save(topoData, file = filteredZipFile)
} else {
    load(filteredZipFile, verbose = TRUE)
}