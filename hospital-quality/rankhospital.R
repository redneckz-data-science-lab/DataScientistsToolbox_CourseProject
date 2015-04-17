OUTCOME_DATA <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
STATES <- OUTCOME_DATA[["State"]]
OUTCOME_COLUMNS_BY_CODE <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
for (col in OUTCOME_COLUMNS_BY_CODE) {
    OUTCOME_DATA[, col] <- as.numeric(OUTCOME_DATA[, col])
}

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    if (is.null(state) || !(state %in% STATES)) {
        stop("invalid state")
    }
    if (is.null(outcome) || !(outcome %in% names(OUTCOME_COLUMNS_BY_CODE))) {
        stop("invalid outcome")
    }
    if (!is.numeric(num) && (num != "best") && (num != "worst")) {
        stop("invalid num")
    }

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
