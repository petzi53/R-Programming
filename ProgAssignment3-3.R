# 3rd part of programming assignment 3
# Using Google Style Guide: https://google.github.io/styleguide/Rguide.xml
#
#
# Ranking hospitals by outome and state
library(readr)
library(dplyr)


RankHospital <- function(state, outcome, num = "best") {
    # Rank hospitals by outcome and state
    #
    # Args:
    #   state: a two letter abbreviation of one of the US states
    #   outcome: specified mortality condition
    #       allowed are heart attack, heart failure, pneumonia
    #   num: rank of the hospital (lower is better) 
    #       allowed are additionally "best" and "worst"
    result <- read_csv("ProgAssignment3-data/outcome-of-care-measures.csv", 
                  # n_max = 10, # for test purposes
                  na = "Not Available",
                  col_types = cols_only(
                      "Hospital Name" = col_character(),
                      "State" = col_character(),
                      "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
                      = col_double(),
                      "Hospital 30-Day Death (Mortality) Rates from Heart Failure"
                      = col_double(),
                      "Hospital 30-Day Death (Mortality) Rates from Pneumonia"
                      = col_double()))
    names(result) <- c("hospital", "state", 
                  "heart attack", "heart failure", "pneumonia")
    ## ---------------------
    # check now the arguments with following preparation
    # 1. convert column state to factors
    # 2. extract the factors as a character vector for comparison
    # (using state.abb does not work, as there are more than 50 states 
    # as the file includes some Commonwealth territory)
    result$state <- as.factor(result$state) 
    allowed.states <- as.character(levels(result$state)) 
    allowed.outcome <- c("heart attack", "heart failure", "pneumonia")
    error.msg <- NULL
    error.msg <- checkArgs(state, outcome, allowed.states, allowed.outcome)
    if (!is.null(error.msg)) { 
        return(stop(error.msg)) 
    }
    # --------------------------
    # Args were ok, so we can continue 
    result <- select(result, hospital, state, matches(outcome))
    result <- na.omit(result)
    result <- result[ order(result[[2]], result[[3]], result[[1]]), ]
    result.split <- split(result, result$state)
    # result.df <- as.data.frame(result.split)
    hospital.names <- sapply(result.split, function(data) data$hospital)
    us.states <- names(hospital.names)
    # df <- as.data.frame((hospital = hospital.names, states = us.states))
    # result.rank <- sapply(result.df, function(data) )
    # print(result.split)
    # print(hospital.names[[state]][[num]])
    return(hospital.names[[state]][[num]])
}
    
checkArgs <- function(state, outcome, states, outcomes) {
    # check if arguments are allowed
    # caller function is RankHospital()
    # 
    # Args:
    #   state: two letter string for state abbreviation
    #   outcome: character string for mortality condition
    #   states: character vector of allowed states abbreviations
    #   outcomes: character vector of allowed outcomes
    if (!(state %in% states)) {
        msg <- "invalide state"
    } else if (!(outcome %in% outcomes)) {
        msg <- "invalide outcome"
    }
    return(NULL)
} 

# -----------------------------------------------------------------------------
# test data
result <- RankHospital("MD", "heart failure", 4)
# result2 <- RankHospital("TX", "heart failure")
# result3 <- RankHospital("MD", "heart attack")
# result4 <- RankHospital("MD", "pneumonia")
# result5 <- RankHospital("PR", "heart failure")   # Commonwealth Territory
# result5 <- RankHospital("BB", "heart attack") # error message via stop function
# last line does not work because of error of previous function call
# call the function call below separetely
# result6 <- RankHospital("NY", "hert attack")  # error message via stop function

