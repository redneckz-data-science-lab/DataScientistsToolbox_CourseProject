read.monitors.by.id <- function(directory, id = 1:332) {
    ## Patterns for monitors search
    patterns <- paste("^0*", id, "\\.csv$", sep = "")
    ## Monitors choosed by id
    monitors <- Map(function(pattern) {
        list.files(directory, pattern = pattern, full.names = T, ignore.case = T)
    }, patterns)
    ## Read data from monitors
    Map(read.csv, monitors)
}

## Counts complete cases for one monitor
## Returns pair of monitor id and nobs (count of complete cases)
count.complete.cases <- function(monitorData) {
    completeSulfateLogicalIndex <- !is.na(monitorData[["sulfate"]])
    completePollutantLogicalIndex <- !is.na(monitorData[completeSulfateLogicalIndex,
                                                        "nitrate"])
    data.frame(id = monitorData[[1, "ID"]], nobs = sum(completePollutantLogicalIndex))
}
