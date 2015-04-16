OUTCOME_DATA <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
STATES <- OUTCOME_DATA[["State"]]
OUTCOME_COLUMNS_BY_CODE <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
for (col in OUTCOME_COLUMNS_BY_CODE) {
    OUTCOME_DATA[, col] <- as.numeric(OUTCOME_DATA[, col])
}

best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate

    if (is.null(state) || !(state %in% STATES)) {
        stop("invalid state")
    }
    if (is.null(outcome) || !(outcome %in% names(OUTCOME_COLUMNS_BY_CODE))) {
        stop("invalid outcome")
    }

    col <- OUTCOME_COLUMNS_BY_CODE[[outcome]]
    outcomeDataForState <- OUTCOME_DATA[OUTCOME_DATA[["State"]] == state,]
    bestIndex <- which.min(outcomeDataForState[[col]])
    bestIndices <- which(outcomeDataForState[[col]] == outcomeDataForState[bestIndex, col])
    hospitalNames <- sort(outcomeDataForState[bestIndices, "Hospital.Name"])
    hospitalNames[1]
}
