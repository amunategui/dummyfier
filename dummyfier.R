dummyfier <- function(thedataframe,theColumnName) {
        # Function wrapper for Caret's dummyVars to quickly break out every factor for a given data.frame column
        # Handles one column formula or 2 or more levels and returns altered data.frame with new columns minus original
        # Automatically builds formula, appends new columns to data.frame and removes old one
        # example use:
        # dummyfier(df,  'FLAG') -- 2 vars
        # dummyfier(df,  'CHIEFCOMPLAINT') -- multi vars
        
        if (length(levels(thedataframe[,theColumnName])) == 2) {
                print(paste("Levels found:",length(levels(thedataframe[,theColumnName]))))
                thedataframe[,theColumnName] <- ifelse(thedataframe[,theColumnName] == levels(thedataframe[,theColumnName])[1], 1, 0)
        }
        else if (length(levels(thedataframe[,theColumnName])) > 2) {
                print(paste("Levels found:",length(levels(thedataframe[,theColumnName]))))
                thedataframe[,theColumnName] <- droplevels(thedataframe[,theColumnName])
                dummies <- dummyVars(paste("~", theColumnName), data = thedataframe)
                tempdata <- data.frame(predict(dummies, newdata = thedataframe))
                # join new data to data.frame
                thedataframe <- cbind(thedataframe, tempdata)
                # drop original column
                var.out<- setdiff(names(thedataframe),c(theColumnName))
                thedataframe <- thedataframe[var.out]
        }
        else {
                print("No suitable levels")
        }
        return (thedataframe)
} 