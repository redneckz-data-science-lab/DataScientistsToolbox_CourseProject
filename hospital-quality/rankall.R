OUTCOME_DATA <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
STATES <- sort(unique(OUTCOME_DATA[["State"]]))
OUTCOME_COLUMNS_BY_CODE <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
for (col in OUTCOME_COLUMNS_BY_CODE) {
    OUTCOME_DATA[, col] <- as.numeric(OUTCOME_DATA[, col])
}

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

    if (is.null(outcome) || !(outcome %in% names(OUTCOME_COLUMNS_BY_CODE))) {
        stop("invalid outcome")
    }
    if (!is.numeric(num) && (num != "best") && (num != "worst")) {
        stop("invalid num")
    }

    data.frame(hospital = sapply(STATES, rankhospital.by.state, outcome, num), state = STATES)
}

rankhospital.by.state <- function(state, outcome, num) {
    col <- OUTCOME_COLUMNS_BY_CODE[[outcome]]
    outcomeDataForState <- OUTCOME_DATA[OUTCOME_DATA[["State"]] == state, ]
    sortedOutcomeData <- outcomeDataForState[order(outcomeDataForState[[col]],
                                                   outcomeDataForState[["Hospital.Name"]],
                                                   na.last = NA), ]
    if (is.numeric(num)) {
        targetIndex <- num
    } else if (num == "best") {
        targetIndex <- 1
    } else if (num == "worst") {
        targetIndex <- nrow(sortedOutcomeData)
    }
    sortedOutcomeData[targetIndex, "Hospital.Name"]
}
