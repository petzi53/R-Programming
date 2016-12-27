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
    f.outcome <- read_csv("outcome-of-care-measures.csv", 
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
    names(f.outcome) <- c("nr", "hospital", "state", 
                  "heart attack", "heart failure", "pneumonia")
    f.outcome <- select (f.outcome, hospital, state, matches(outcome))

    # check now the arguments
    # 1. convert column state to factors
    # 2. extract the factors as a character vector for comparison
    # (using state.abb does not work, as there are more than 50 states 
    # as the file includes some Commonwealth territory)
    f.outcome$state <- as.factor(f.outcome$state) 
    allowed.states <- as.character(levels(f.outcome$state)) 
    allowed.outcome <- c("heart attack", "heart failure", "pneumonia")
    error.msg <- NULL
    error.msg <- checkArgs(state, outcome, allowed.states, allowed.outcome)
    if (!is.null(error.msg)) { return(stop(error.msg)) }
    # f <- na.omit(f)
    # f <- f[ order(f[[2]], f[[3]], f[[1]]), ]
    # f <- split(f, f$state)
    # mutate(f, rank = 0L)
    return(f.outcome)
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
    }   else if (!(outcome %in% outcomes)) {
        msg <- "invalide outcome"
    }
    return(msg)
} 

# -----------------------------------------------------------------------------
# test data
result1 <- best("TX", "heart attack")
result2 <- best("TX", "heart failure")
result3 <- best("MD", "heart attack")
result4 <- best("MD", "pneumonia")
result5 <- best("PR", "heart failure")   # one of the Commonwealth Territories
result5 <- best("BB", "heart attack") # throws error message via stop function
# last line does not work because of error of previous function call
# call the function call below separetely
result6 <- best("NY", "hert attack")  # throws error message via stop function

