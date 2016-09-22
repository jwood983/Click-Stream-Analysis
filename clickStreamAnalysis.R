# simple header stuff (e.g., sources, libraries, etc)
source("clickStreamFunctions.R")
library(ggplot2)


# dataframe containing ClickStreamData.txt info 
#  set as tab separated, buteasily changed
click <- read.table("ClickStreamData.new.txt", header=TRUE, sep="\t")
#~ str(click)

# dataframe containing ProductCategoryData
#~ product <- read.table("ProductCategoryData.txt", header=TRUE, sep="\t")

# dataframe containing UserProfielData
profiles <- read.table("UserProfileDataClean.txt", header=TRUE, sep="\t")


# do the work now....
printCountsPerDay(click)
printCountsPerHour(click)
printBarplots(click)

findCountryMovers(click, product)
findRegionMovers(click, product)

plotGlobalLocales(click)

getVisitPerCountry(click)


