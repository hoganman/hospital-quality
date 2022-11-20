## Get the best hospital by state initials filtered by the patient outcomes
## Author: Matthew G. Hogan (matthewghogan _AT_ proton .dot. me)

best <- function(
    state,  
    outcome
){
    #' Get the best hospital by state initials filtered by the patient outcomes
    #' which minimizes the risk-adjusted 30-day death rate.
    #'
    #' Arguments
    #' ---------
    #' state : Length 1 character vector of abbreviated 2-character state
    #'         initials.
    #' outcome : Length 1 character vector either "heart attack", 
    #'           "heart failure", or "pneumonia"
    #' 
    #' Returns
    #' -------
    #' Length 1 character vector of the hospital name or NA is none are found
    #' 
    #' Stops
    #' -----
    #' When the state abbreviation is not found or if outcome is not supported.

    # Define valid state inputs
    valid_states <- c(
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
    
    # Test for invalid inputs
    if(!any(state == valid_states)){
        stop("invalid state")
    }
    if(!any(outcome == names(valid_outcomes))){
        stop("invalid outcome")
    }
    
    outcomes_df <- read.csv(
        "outcome-of-care-measures.csv",
        colClasses = "character"
    )
    
    # Keep state of interest
    rel_state <- outcomes_df[, "State"] == state
    outcomes_df <- outcomes_df[rel_state, ]
    
    # Select columns that have name and 30-day death rate
    rel_cols <- list(
        "Hospital.Name"=2,
        "outcome"=valid_outcomes[[outcome]]
    )
    name_column <- which("Hospital.Name" == names(rel_cols))
    to_numeric_column <- which("outcome" == names(rel_cols))
    
    # Remove all unavailable data
    dr30d <- outcomes_df[, as.numeric(rel_cols)]
    dr30d[, to_numeric_column] <- as.numeric(dr30d[, to_numeric_column])
    avail_rate <- !is.na(dr30d[, to_numeric_column])
    dr30d <- dr30d[avail_rate, ]
    if(nrow(dr30d) == 0){
        return(NA)
    }
    
    # Order the data from least to most deaths, with hospital name used to
    # break degeneracy
    order_by <- order(
        dr30d[, to_numeric_column],
        dr30d[, name_column]
    )
    dr30d <- dr30d[order_by, ]
    
    # Return the hospital name
    return(as.character(dr30d[, name_column][1]))
}
