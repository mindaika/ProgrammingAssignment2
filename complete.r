complete <- function(directory, id = 1:332) {
        
		numMonitors <- length(id)
		results <- data.frame(id = NA, nobs = NA)
		
		for(i in 1:numMonitors) {
			fileName <- paste(directory, "/", formatC(id[i], width = 3, format = "d", flag = "0"), ".csv", sep="")
			df <- read.csv(fileName)
			results[i,] <- c(id[i], nrow(df[complete.cases(df),]))
		}
	results		
}