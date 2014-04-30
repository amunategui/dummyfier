dummyfier <- function(theDataFrame, theColumnName, alternateIndex=NULL) {
        # Function wrapper for Caret's dummyVars to quickly break out every factor for a given data.frame column
        # Handles one column formula of 2 or more levels and returns altered data.frame with new columns minus original
        # Defaults to row index but can be assigned an alternative index in situations where row isn't unique
        # Automatically builds formula, appends new columns to data.frame and removes old one
        # example use:
        # dummyfier(df, 'FLAG') -- 2 vars
        # dummyfier(df, 'CHIEFCOMPLAINT') -- multi vars
        
        if (length(levels(theDataFrame[,theColumnName])) == 2) {
                print(paste("Levels found:",length(levels(theDataFrame[,theColumnName]))))
                theDataFrame[,theColumnName] <- ifelse(theDataFrame[,theColumnName] == levels(theDataFrame[,theColumnName])[1], 1, 0)
        }
        else if (length(levels(theDataFrame[,theColumnName])) > 2) {
                print(paste("Levels found:",length(levels(theDataFrame[,theColumnName]))))
                theDataFrame[,theColumnName] <- droplevels(theDataFrame[,theColumnName])
                if (!is.null(alternateIndex)) {
                        dummies <- dummyVars(paste("~", theColumnName, "+", alternateIndex), data = theDataFrame)
                }
                else {
                        dummies <- dummyVars(paste("~", theColumnName), data = theDataFrame)
                }
                tempdata <- data.frame(predict(dummies, newdata = theDataFrame))
                # join new data to data.frame
                theDataFrame <- cbind(theDataFrame, tempdata)
                # drop original column
                var.out<- setdiff(names(theDataFrame),c(theColumnName))
                theDataFrame <- theDataFrame[var.out]
        }
        else {
                print("No suitable levels")
        }
        return (theDataFrame)
} 