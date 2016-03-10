rankall <- function(outcome,num="best"){
        ## Read outcome data
        main_path <- "rprog-data-ProgAssignment3-data"
        df <- read.csv(paste0(main_path,"/","outcome-of-care-measures.csv"), colClasses = "character")
        
        ## Check the if outcome is valid
        
        if (toupper(outcome)==toupper("heart attack")) num_col <- 11
        else if (toupper(outcome)==toupper("heart failure")) num_col <- 17
        else if (toupper(outcome)==toupper("pneumonia")) num_col <- 23
        else  stop("invalid outcome")
        
        ## For each state, find the hospital of the given rank
        df2 <- df[,c(2,7,num_col)]
        df2[,3] <- as.numeric(df2[,3])
        o <- order(df2[,2],df2[,3],df2[,1])
        df2 <- df2[o,]
        df2 <- na.omit(df2)
        states <- unique(df2[,2])
        result <- data.frame(Hospital.Name=0,State=0)
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        for(i in 1:length(states)){
                df3 <- df2[df2$State==states[i],]
                n_total <- nrow(df3)
                if(num=="best"){
                        result <- rbind.data.frame(result,df3[1,c(1,2)])
                }  
                else if(num=="worst") {
                        result <- rbind.data.frame(result,df3[n_total,c(1,2)])
                }
                else if((num<=n_total) & (is.numeric(num))){
                        result <- rbind.data.frame(result,df3[num,c(1,2)])  
                } 
        }
        
       names(result) <- c("hospital","state")
       result        
}

rankall_imp <- function(outcome, num = "best") {
        ## Read the outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        states = unique(dat[, 7])
        switch(outcome, `heart attack` = {
                col = 11
        }, `heart failure` = {
                col = 17
        }, pneumonia = {
                col = 23
        }, stop("invalid outcome"))
        
        ## Return hospital name in that state with the given rank 30-day death rate
        dat[, col] = as.numeric(dat[, col])
        dat = dat[, c(2, 7, col)]  # leave only name, state, and death rate
        dat = na.omit(dat)
        # head(dat) Hospital.Name State 1 SOUTHEAST ALABAMA MEDICAL CENTER AL 2
        # MARSHALL MEDICAL CENTER SOUTH AL 3 ELIZA COFFEE MEMORIAL HOSPITAL AL 7 ST
        # VINCENT'S EAST AL 8 DEKALB REGIONAL MEDICAL CENTER AL 9 SHELBY BAPTIST
        # MEDICAL CENTER AL
        # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack 1 14.3 2 18.5 3
        # 18.1 7 17.7 8 18.0 9 15.9
        rank_in_state <- function(state) {
                df = dat[dat[, 2] == state, ]
                nhospital = nrow(df)
                switch(num, best = {
                        num = 1
                }, worst = {
                        num = nhospital
                })
                if (num > nhospital) {
                        result = NA
                }
                o = order(df[, 3], df[, 1])
                result = df[o, ][num, 1]
                c(result, state)
        }
        output = do.call(rbind, lapply(states, rank_in_state))
        output = output[order(output[, 2]), ]
        rownames(output) = output[, 2]
        colnames(output) = c("hospital", "state")
        data.frame(output)
}