## Get a data frame that stores the hospital name and state matching a numerical
## rank
## Author: Matthew G. Hogan (matthewghogan _AT_ proton .dot. me)
source("rankhospital.R")


rankall <- function(
    outcome,
    num = "best"
){
    #' Get a data frame that stores the hospital name and state matching the
    #' input number rank.
    #'
    #' Arguments
    #' ---------
    #' outcome : Length 1 character vector either "heart attack", 
    #'           "heart failure", or "pneumonia"
    #' num : Length 1 vector either a positive-definite integer or the strings
    #'       "best" or "worst"
    #' 
    #' Returns
    #' -------
    #' Data frame with 2 columns and 56 rows. Each row is a hospital name or NA
    #' and state abbreviation pair. The row names are also set to the state
    #' abbreviation. 
    #' 
    #' Stops
    #' -----
    #' When any the following are true:
    #'  * `outcome` is not supported, or
    #'  * `num` is not a length 1 vector, not a positive-definite integer, or
    #'    neither "best" nor "worst".
    
    # Define valid state inputs
    states <- c(
        "AK", "AL", "AR", "AS", "AZ", "CA",
        "CO", "CT", "DC", "DE", "FL", "GA",
        "GU", "HI", "IA", "ID", "IL", "IN",
        "KS", "KY", "LA", "MA", "MD", "ME",
        "MI", "MN", "MO", "MP", "MS", "MT",
        "NC", "ND", "NE", "NH", "NJ", "NM",
        "NV", "NY", "OH", "OK", "OR", "PA",
        "PR", "RI", "SC", "SD", "TN", "TX",
        "UT", "VA", "VI", "VT", "WA", "WI",
        "WV", "WY"
    )

    # Define valid outcome inputs.
    # The key, value pairs map from outcome name to data frame column number
    valid_outcomes <- list(
        "heart attack"=11,
        "heart failure"=17,
        "pneumonia"=23
    )
    
    # Define valid rank input
    valid_nums <- c("best", "worst")
    is_valid_nums <- function(test){
        if(length(test) != 1){
            return(FALSE)
        }
        else if(is.numeric(test)){
            return(test > 0)
        }
        else if(is.character(test)){
            return(any(test == valid_nums))
        }
        else{
            return(FALSE)
        }
    }
    
    # Test for invalid inputs
    if(!any(outcome == names(valid_outcomes))){
        stop("invalid outcome")
    }
    if(!is_valid_nums(num)){
        stop("invalid num")
    }
    
    # Hospital rank data frame
    ranks <- data.frame(
        hospital = rep(NA, length(states)),
        state = states,
        row.names = states
    )
    
    # Set the hospital name if found
    for(state in states){
        hospital <- rankhospital(state, outcome, num)
        if(is.na(hospital)){
            next
        }
        ranks[ranks$state == state, ]$hospital <- hospital
    }
    
    # Return the hospital's rank data frame
    return(ranks)
}
