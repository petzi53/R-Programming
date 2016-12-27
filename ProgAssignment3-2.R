## Finding the best hospital in a state
library(readr)
library(dplyr)

best <- function(state, outcome) {
    # Find the baste hospital for a certain outcome in a certain state
    #
    # Args:
    #   state is the two-letter abbreviation of US states
    #   outcome is the 30-day mortality rate of certain conditions
    #       allowed outcomes are: heart attack, heart failure, pneumonia
    my_msg <- NULL
    my_msg <- checkArgs(state, outcome)
        if (!is.null(my_msg)) {
        return(stop(my_msg))
        }
    f <- read_csv("outcome-of-care-measures.csv", 
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
    names(f) <- c("hospital", "state", 
                  "heart attack", "heart failure", "pneumonia")
    f <- select (f, hospital, state, matches(outcome))
    f <- na.omit(f)
    f <- f[ order(f[[2]], f[[3]], f[[1]]), ]
    f <- split(f, f$state == state)
    f <- f[[2]][[c(1,1)]]
    print(f)
    return(f)
}

checkArgs <- function(myState, myOutcome) {
    # check if arguments are allowed
    # caller function is best()
    # 
    # Args:
    #   myState: has to be one of the two letter abbreviations for US states
    #       ATTENTION: There are 54 states: 50 and DC, GU, PR, VI
    #       = the standard 50 and 4 Commonwealth Territories
    #       (see http://www.50states.com/abbreviations.htm)
    #       next part of assignment I provide a more general solution
    #   myOutcome: one of three condition, defined in allowed_outcome below
    msg <- NULL
    allowed_outcome <- c("heart attack", "heart failure", "pneumonia")
    allowed_states <- c(state.abb, "DC","GU", "PR", "VI")
    if (!(myState %in% allowed_states)) {
        msg <- "invalide state"
    }   else if (!(myOutcome %in% allowed_outcome)) {
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




