#------------------------------- function space ------------------------------#

###:: plot of total counts vs day ::###
printCountsPerDay <- function(click){
   fname <- paste("CountsPerDay.pdf",sep="")
   cat("Plotting ",fname,"...\n")
   pdf(file=fname)
   hist(strptime(click$Timestamp, "%d-%m-%Y %H:%M"), breaks="days", freq=TRUE,
        xlab="Date", ylab="Count", main="Total Hits per Day")
   dev.off()
   res <- hist(strptime(click$Timestamp, "%d-%m-%Y %H:%M"), breaks="days", freq=TRUE,
               xlab="Date", ylab="Count", main="Total Hits per Day")
   for(i in 1:length(res$breaks)){
      cat(res$breaks[[i]], ", ", res$counts[[i]], "\n")
   }
}

###:: plot of total counts vs hour ::###
printCountsPerHour <- function(click){
   fname <- paste("CountsPerHour.pdf",sep="")
   cat("Plotting ",fname,"...\n")
   pdf(file=fname)
   hist(strptime(click$Timestamp, "%d-%m-%Y %H:%M")$hour,
        xlab="Hour", ylab="Count", xlim=c(0,23), ylim=c(0,40000), 
        xaxt="n", main="Total Counts per hour")
   axis(side=1, at=seq(0,23,1), labels=seq(0,23,1))
   dev.off()
}

###:: plot barplot of each item ::###
printBarplots <- function(click){
   for(name in names(click)){
      fname <- paste("Barplot",name,".pdf",sep="")
      cat("Plotting ",fname,"...\n")
      pdf(fname)
      barplot(height=table(click[[name]]))
      dev.off()
   }
}

###:: barplot of visit per country
getVisitPerCountry <- function(click){
   click <- click[order(click$UserID),]
   users <- levels(click$UserID)
   countries <- levels(click$Country)
   countries
 # set the list of counties as labels to the visits
   visits <- setNames(rep(0,length(countries)), countries)
 # run through the users list and sum the visits
   for(U in 1:length(users)){
      user <- users[[U]]
      thisUser <- click[which(click$UserID == user),]
      countryList <- levels(thisUser$Country)
      for(C in 1:length(countryList)){
         CO <- countryList[[C]]
         visits[CO] <- visits[CO] + 1
      }
   }
   countryList <- levels(click$Country)
   for(C in 1:length(countryList)){
      CO <- countryList[[C]]
      cat(CO,"\t",visits[[CO]],"\n")
   }
}


###:: replace URLs with genre & plot ::###
printURLAsGenre <- function(click, product, delay){
   cat("Plotting URLAsGenre.pdf...\n")
   if(missing(delay)){
      sleep <- 15
   } else {
      sleep <- delay
   }
 # set the URL as a character for modification
   click$URL <- as.character(click$URL)
 # cover all cases except the original site (books)
   for(i in 2:length(product$URL)){
      url <- as.character(product$URL[[i]])
      title <- as.character(product$Category[[i]])
      click$URL[click$URL == url] <- title
   }
 # cover original site (books)
   url <- as.character(product$URL[[1]])
   title <- as.character(product$Category[[1]])
   click$URL[click$URL == url] <- title
 # set back as factor
   click$URL <- as.factor(click$URL)
 # save to PDF
   pdf(file="URLAsGenreLog.pdf")
   barplot(height=table(click$URL), las=2,
           log="y", xpd=FALSE,
           col=palette())
   dev.off()
   return(click)
}


###:: print URLs by nation ::###
printURLByNation <- function(click, product){
   countryList <- levels(click$Country)
   for(country in countryList){
    # get name of file
      fname <- paste("URLAsGenre",country,".pdf", sep="")
      fname <- gsub(" ", "", fname)
      fname <- gsub("\t", "", fname)
      cat("Opening ",fname," now\n")
    # set this country
      thisCountry <- click[which(click$Country == country),]
    # prepare for modification
      thisCountry$URL <- as.character(thisCountry$URL)
      for(i in 1:length(product$URL)){
         url <- as.character(product$URL[[i]])
         title <- as.character(product$Category[[i]])
         thisCountry$URL[thisCountry$URL == url] <- title
      }
      thisCountry$URL <- as.factor(thisCountry$URL)
      pdf(file=fname)
      barplot(height=table(thisCountry$URL), las=2,
              log="y", xpd=FALSE, col=palette(),
              main=paste(c("Page hits by ",country), sep=""))
      dev.off()
   }
}


###:: save movers to string ::###
saveMovers <- function(moved, user, Mover){
   thisLine <- paste("User ",user, moved, as.character(Mover)[1],"\n", sep="")
   for(L in 2:length(Mover)){
      thisLine <- paste(thisLine,
                        "                                                 ",
                        as.character(Mover)[L], "\n", sep="")
   }
   return(thisLine)
}

###:: Find the movers between countries ::###
findCountryMovers <- function(click, profiles){
   users <- levels(profiles$UserID) # this is sorted
   Movers <- 0
   fname <- "MovingCountryUsers.txt"
   cat("", file=fname)
   # sort based on user id
   click <- click[order(click$UserID),]
   # iterate through users list
   for(U in 1:length(users)){
      user <- users[[U]]
      thisUser <- click[which(click$UserID == user),]
      Mover <- unique(thisUser$Country)
      if(length(Mover) > 1){
         Movers <- Movers + 1
         thisLine <- saveMovers(" was in ", user, Mover)
         cat(thisLine, file=fname, append=TRUE)
      }
   }
   cat("Out of ",length(users)," users, ",Movers,
       " were found to have moved from their home country.\n")
}

###:: Find the Regional Movers ::###
findRegionMovers <- function(click, profiles){
   users <- levels(profiles$UserID) # this is sorted
   Movers <- 0
   fname <- "MovingRegionUsers.txt"
   cat("", file=fname)
   # sort based on user id
   click <- click[order(click$UserID),]
   # iterate through users list
   for(U in 1:length(users)){
      user <- users[[U]]
      thisUser <- click[which(click$UserID == user),]
      Mover <- unique(thisUser$Region.State)
      if(length(Mover) > 1){
         Movers <- Movers + 1
         thisLine <- saveMovers(" was in ", user, Mover)
         cat(thisLine, file=fname, append=TRUE)
      }
   }
   cat("Out of ",length(users)," users, ",Movers,
       " were found to have moved from their home city/town.\n")
}


###:: Average number of visits ::###
getAverageVisitCount <- function(click, profiles, ndays){
   click <- click[order(click$UserID),]
   users <- levels(profiles$UserID)
   userVisits <- 0
   nVisits <- 0
 # run through the users list and sum the visits
   for(U in 1:length(users)){
      user <- users[[U]]
      thisUser <- click[which(click$UserID == user),]
      p <- levels(thisUser$Timestamp)
      if(p > 1){
         userVisits <- userVisits + p
         nVisits <- nVisits + 1
      }
   }
   avgTotalVisits <- userVisits/nVisits
   cat("The average site visitor had visited ",avgTotalVisits," in ",ndays," days\n")
   cat("The average site visitor had visited ",avgTotalVisits/ndays," per day\n")
}

###:: Average number of friends ::###
getAverageFriends <- function(profiles){
   nprof <- levels(profiles$UserID)
   friends <- 0
   for(N in 1:nprof){
      friends <- friends + profiles$NumberOfFriends
   }
   cat("Average number of friends is ",friends/nprof,".\n")
}


###:: Find the mode of visits ::###
findModeVisits <- function(click){
   click <- click[order(click$UserID), ]
   click$City <- as.character(click$City)
   users <- levels(click$UserID)
   fname <- "MostCommonCity.txt"
   cat("", file=fname)
   for(U in 1:length(users)){
      user = users[[U]]
      thisUser <- click[which(click$UserId == user), ]
      thisCity <- Mode(thisUser$City)
      cat(thisCity, "\n", file=fname, append=TRUE)
   }
}
