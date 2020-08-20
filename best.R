## The purpose of this function is to identify the hospital with the best, 
## i.e. lowest, 30-day mortality rate for a specified outcome, in the specified
## state. If a tie occurs, the first hospital in alphabetic order is returned.

## Data is stored in a file called;  outcome-of-care-measures.csv
## field names we need:
##      Hospital.Name (col 2)
##      State (col 7)
##      Hospital 30-Day Death (Mortality) Rates from Heart Attack (col 11)
##      Hospital 30-Day Death (Mortality) Rates from Heart Failure (col17)
##      Hospital 30-Day Death (Mortality) Rates from Pneumonia (col23)


best <- function(state,outcome) {

        ## read outcome data from outcome-of-care-measures.csv file
        datafile <- "outcome-of-care-measures.csv"
        outcomes <- read.csv(datafile)
        ## rename the columns of interest for readability and ease of use
        ## names(outcome)[11] <- "heart.attack"
        ## names(outcome)[17] <- "heart.failure"
        ## names(outcome)[23] <- "pneumonia"
        

        ## validate state
        valid_states <- unique(outcomes$State)
        if (is.na(match(state, valid_states))) {
                stop("invalid state")
        }
        
        ## validate outcome 
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia") 
        if (is.na(match(outcome, valid_outcomes))) {
                stop("invalid outcome")
        }
        
        ## define the column numbers of interest, retrieve
        ## the correct one for the outcome
        cols <- c(11,17,23)
        col <- cols[match(outcome,valid_outcomes)]
        
        ## subset the outcome data for state of interest, ommitting hospitals
        ## that didn't report on the particular outcome of interest
        sub.outcomes <- outcomes[((outcomes$State == state) & 
                        (outcomes[,col] != "Not Available")),]
        
        ## identify the lowest mortality rate
        bestvalue <- min(as.numeric(sub.outcomes[,col]))
        besthospital <- sub.outcomes[  (sub.outcomes[,col] == bestvalue),]
        
        ## if there are ties, return the first hospital alphabetically
        min(besthospital$Hospital.Name)
        
        ## bestvalue <- outcomes[, min(sub.outcomes[,col])
        
       
        
        ## return hospital name in that state with the lowest 30-day mortality 
        ## rate

}