# Exploring the Bay Area through data

AJ Heller, April 23rd, 2016

## Good news, everyone!

My wife Kelly and I are relocating! She accepted a great job at [Radius Intelligence](https://radius.com/), and in the near future, we'll be unboxing our stuff in a new home not far from San Francisco. With data projects on my mind, I decided to see if I could piece together a picture of the Bay Area, with the goal of figuring out where we'd like to live.

This script walks you through using a couple different web APIs for data collection and remote services, a handful of best practices in data tidying, some exploratory data analysis, and ultimately a few great map visualizations.

*Are you hiring? I'm actively looking for work in the Bay Area.* [*Drop me a line!*](mailto:aj@drfloob.com?subject=Hiring in the Bay Area)


--------------------------------------------------------------------------------

## Files

### bayAreaZipcodeHeatMap.Rmd

The main document of this report. It contains the bulk of the data analysis and visualization.

### bayAreaZipcodeHeatMap.R

This file is now redundant. All code has been subsumed into `bayAreaZipcodeHeatMap.Rmd`.

### credentials.R

```R
apiKey <- "my google maps API key"
appToken <- "my socrata app token"
```

*Note: do not commit this file to your git repository!*

### gatherZipcodeShapes.R

This script assists you in downloading the US Census zip code shape files and converting them to GeoJSON. It also performs a more accurate count of crime occurrences within each zip code.

### *.dump

Temporary files used to shorten execution time. A full run with these cache files takes about 2 minutes on my chromebook.