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

    # check parameter num
    #
    # number of hospitals by state and mortatility = max number allowed
    # not a perfect solution for indices outside of range
    # returning NA automatically by system would be better
    # but then I have to find a soltion without indexing
    max <- length(hospital.names[[state]])
    num <- checkNum(num, max)
    if (num ==  -1L) return(stop("invalid ranking expression"))
    if (num == "NA") return("NA")
    return(hospital.names[[state]][[num]])
}


checkNum <- function(n.rank, n.max) {
    if (is.character(n.rank)) {
        if (n.rank == "best")   return(1)
        if (n.rank  == "worst") return(n.max)
    }
    if (n.rank > n.max) return("NA")
    return(n.rank)
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
# result1 <- RankHospital("TX", "heart failure", 4)
# result2 <- RankHospital("TX", "heart attack", "best")
# result3 <- RankHospital("MD", "heart attack", "worst")
# result4 <- RankHospital("MN", "heart attack", 5000)
# result5 <- RankHospital("BB", "heart attack") # error message via stop function
# result6 <- RankHospital("TX", "heart attack", "second")
# #last line does not work because of error of previous function call
# #call the function call below separetely
# result7 <- RankHospital("NY", "hert attack")  # error message via stop function
