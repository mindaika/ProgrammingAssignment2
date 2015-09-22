corr <- function(directory, threshold = 0) {
    completes <- complete(directory)
    usableMonitors <-
        data.frame(id = completes[completes$nobs > threshold,]$id, nobs = completes[completes$nobs > threshold,]$nobs)
    results <- rep(NA, nrow(usableMonitors))
    
    if (nrow(usableMonitors) > 0) {
        for (i in 1:nrow(usableMonitors)) {
            fileName <-
                paste(
                    directory, "/", formatC(
                        usableMonitors$id[i], width = 3, format = "d", flag = "0"
                    ), ".csv", sep = ""
                )
            df <- read.csv(fileName)
            df <- df[complete.cases(df),]
            results[i] <- cor(df$sulfate, df$nitrate)
        }
    }
    results
}