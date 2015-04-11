source("common.R")

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations

    monitors <- list.files(directory, pattern = "csv$", full.names = T, ignore.case = T)
    ## Data from all monitors
    monitorsData <- Map(read.csv, monitors)

    monitorsCompleteCasesCount <- Reduce(rbind, Map(count.complete.cases,
                                                    monitorsData))
    ## Monitors where the number of completely observed cases is greater than the threshold
    appropriateMonitorsData <- monitorsData[monitorsCompleteCasesCount[["nobs"]] > threshold]

    as.numeric(Map(function(monitorData) {
        cor(x = monitorData[["sulfate"]], y = monitorData[["nitrate"]], use = "pairwise.complete.obs")
    }, appropriateMonitorsData))
}
