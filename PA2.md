# Title
Stephan Koenig  
January 27, 2016  



## Synopsis


```r
# Load all required packages installing any that are missing.
requiredPackages <- c("dplyr", "ggplot2", "lubridate")
missingPackages <- !is.element(requiredPackages, installed.packages()[,1])
missingPackages <- requiredPackages[missingPackages]
if (length(missingPackages) != 0) install.packages(missingPackages)
lapply(requiredPackages, library, character.only = TRUE)
rm(requiredPackages, missingPackages)
```



## Data processing


```r
# Time when copy of "repdata-data-StormData.csv.bz2" in this rep was downloaded.
dateDownloaded <- "Wed Jan 27 16:16:59 2016"
filename <- "repdata-data-StormData.csv.bz2"

# Download data if not present and update time when the copy was downloaded.
if (!file.exists(filename)) {
    
    url = paste("https://d396qusza40orc.cloudfront.net/",
                "repdata%2Fdata%2FStormData.csv.bz2", sep = "")
    
    # Determine download.file method based on OS.
    if (.Platform$OS.type == "unix") {
        osMethod = "libcurl"
    } else {
        osMethod = "wininet"
    }
    
    download.file(url, filename, method = osMethod)
    dateDownloaded <- date()
    rm(url, osMethod)
}
```

File *repdata-data-StormData.csv.bz2* was downloaded on **Wed Jan 27 16:16:59 2016**.


```r
stormData <- tbl_df(read.csv("repdata-data-StormData.csv.bz2"))
```



## Results



## Summary
