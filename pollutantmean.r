pollutantmean <- function(directory, pollutant, id = 1:332) {
		
		numMonitors <- length(id)
		meanByMonitor <- numeric(numMonitors)
		globalSum <- 0
		globalCount <- 0
		
		for (i in 1:numMonitors) {
			fileName <- paste(directory, "/", formatC(id[i], width = 3, format = "d", flag = "0"), ".csv", sep="")
			dfTemp <- read.csv(fileName)
			globalSum <- globalSum + sum(dfTemp[[pollutant]], na.rm = TRUE)
			globalCount <- globalCount + length(dfTemp[[pollutant]][!is.na(dfTemp[[pollutant]])])
		}
		globalSum / globalCount
}

