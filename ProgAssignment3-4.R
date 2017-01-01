Sys.setenv(LANG = "en")
library(readr)
library(dplyr)

RankAll <- function(outcome, num = "best") {
    # Rank hospitals by outcome and state
    #
    # Args:
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
    # now do the hard work
    result <- select(result, hospital, state, matches(outcome))
    result <- na.omit(result)
    result <- result[ order(result[[2]], result[[3]], result[[1]]), ]
    result <- split(result, result$state)

    j <- length(result)
    for (i in 1:j) {
         result[i] <- lapply(result[i], function(data) mutate(result[[i]],
                         rank = seq(NROW(result[[i]]))))
    }
    
    # Extract first row of first tbl_df: t[[1]][1, ]
    # Extract first row of first tbl_df: filter(t[[1]], rank == 1)
    df0 <- data.frame()
    j <- length(result)
    for (i in 1:j) {
         df1 <- filter(result[[i]], rank == num)
         if (NROW(df1) == 0) {
             df1 <- data_frame(state = as.character(result[[i]][1,2]))
         }
         df0 <- bind_rows(df0, df1)
    }
    return(df0)
    
}

# t <- RankAll("heart failure", 1)
# t <- head(RankAll("heart attack", 20), 10)
t <- tail(RankAll("pneumonia", "worst"), 3)
# print(t)