library('geosphere')
data.raw <- read.csv('data/batch-geocode-1670152375.csv')
googleData <- read.csv('data/Google Maps Data 12-2-22.csv')

distThresh <- 5000
reviewThresh <- 25

allTheCloseStuff = c()
numNearby = c()

nearbyEstablishments = c()
numNearbyEstablishments = c()
averageReviews = c()
for (houseCurrent in 1:nrow(data.raw)){
    houseLAT <- data.raw$Latitude[houseCurrent]
    houseLON <- data.raw$Longitude[houseCurrent]

    localNumNearby <- 0
    closeThings <- ""
    for (otherHouse in 1:nrow(data.raw)) {
        otherLAT <- data.raw$Latitude[otherHouse]
        otherLON <- data.raw$Longitude[otherHouse]
        dist <- distHaversine(c(houseLON, houseLAT), c(otherLON, otherLAT))
        if (dist < distThresh & dist != 0) {
            if (length(closeThings) == 0) {
                closeThings <- data.raw$Location[otherHouse]
            }
            else {
                closeThings <- paste(closeThings, data.raw$Location[otherHouse], sep="|")
            }
            localNumNearby <- localNumNearby + 1
        }
    }

    if (closeThings == "") {
        allTheCloseStuff[[length(allTheCloseStuff)+1]] <- c("N/A")
    }
    else {
        allTheCloseStuff[[length(allTheCloseStuff)+1]] <- closeThings
    }
    numNearby <- append(numNearby, localNumNearby)


    establishmentNames <- ""
    establishmentAverageReviews = c()
    thisVariableCountsNearbyEstablishments = 0
    for (establishment in 1:nrow(googleData)) {
        establishmentLAT <- googleData$locationlat[establishment]
        establishmentLON <- googleData$locationlng[establishment]
        dist <- distHaversine(c(houseLON, houseLAT), c(establishmentLON, establishmentLAT))
        if (isTRUE(dist < distThresh & dist != 0) == TRUE) {
            if (googleData$reviewsCount[establishment] >= reviewThresh) {
                if (establishmentNames == "") {
                    establishmentNames <- googleData$title[establishment]
                }
                else {
                    establishmentNames <- paste(establishmentNames, googleData$title[establishment], sep="|")
                }
                establishmentAverageReviews <- append(establishmentAverageReviews, googleData$totalScore[establishment])
                thisVariableCountsNearbyEstablishments <- thisVariableCountsNearbyEstablishments + 1
            }
        }
    }

    if (establishmentNames == "") {
        nearbyEstablishments[[length(nearbyEstablishments)+1]] <- c("N/A")
    }
    else {
        nearbyEstablishments[[length(nearbyEstablishments)+1]] <- establishmentNames
    }

    if (length(establishmentAverageReviews) > 0) {
        averageReviews <- append(averageReviews, mean(establishmentAverageReviews))
    }
    else {
        averageReviews <- append(averageReviews, -1)
    }

    numNearbyEstablishments <- append(numNearbyEstablishments, thisVariableCountsNearbyEstablishments)
}

data.raw$NearbyProperties <- allTheCloseStuff
data.raw$NumNearbyProperties <- numNearby
data.raw$NearbyEstablishments <- nearbyEstablishments
data.raw$NumNearbyEstablishments <- numNearbyEstablishments
data.raw$AverageReviewsOfnearbyEstablishments <- averageReviews

data.raw <- apply(data.raw,2,as.character)
write.csv(data.raw, "Output/KCMarketData.csv", row.names=FALSE)
