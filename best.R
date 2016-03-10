best <- function(state,outcome){
    
        ## Read outcome data
        
        main_path <- "rprog-data-ProgAssignment3-data"
        df <- read.csv(paste0(main_path,"/","outcome-of-care-measures.csv"), colClasses = "character")
        
        ## Check that state and outcome are valid
        states <- unique(df$State)
        
        if (!(state %in% states)) {
                stop("invalid state")
        }
        
        if (toupper(outcome)==toupper("heart attack")) num_col <- 11
        else if (toupper(outcome)==toupper("heart failure")) num_col <- 17
        else if (toupper(outcome)==toupper("pneumonia")) num_col <- 23
        else  stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        df2 <- df[df$State==state,c(2,num_col)]
        df2[,2] <- as.numeric(df2[,2])
      
        ## Order the values
        o <- order(df2[,2],df2[,1])
        df2 <-df2[o,]
        df2[1,1]
}  