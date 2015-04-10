pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)

    ## Computes pollutant mean in CSV file
    pollutantmeanForFile <-  function(file) {
        pollutantDataFrame <- read.csv(file.path(directory, file), colClasses = c("character", "numeric", "numeric", "numeric"))
        pollutantColumnData <- pollutantDataFrame[pollutantDataFrame[["ID"]] %in% id, pollutant]
        mean(pollutantColumnData, na.rm = T)
    }

    files <- list.files(directory)

    ## Vector of means for each CSV file inside directory
    means <- sapply(files, pollutantmeanForFile)

    mean(means, na.rm = T)
}

