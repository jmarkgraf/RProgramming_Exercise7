# Exercise 10: Plotting
# Date: February 21, 2017
# Author: Jonas Markgraf
#########################


rm(list = ls())
library(foreign)

# read data
setwd("~/Dropbox/Hertie School/(4) Applied Statistical Programming (WUSTL)/09_Lecture/")
      
abort <- read.csv("ForClass.csv")
# "gop": 1 for republican
# "bugsid": identifier for person

# drop duplicates
unique(abort[,2:8])

# creating subsets for Democrats and Republicans
dem <- subset(abort, gop == 0)
rep <- subset(abort, gop == 1)

# estimating mean values per congress
dem_ability <- aggregate(dem$ability, list(dem$congress), mean)
rep_ability <- aggregate(rep$ability, list(rep$congress), mean)
congress <- dem_ability$Group.1

# line chart
plot(dem_ability, ylim = range(-2:2), type = "l", col = "blue",
     main = "Position of Congress Members, by party",
     ylab = "Ability scores", xlab = "No Congress (time)")
lines(rep_ability, type = "l", col = "red")
legend("right", title = "Testlegend")
abline(h = 0)


# create new data frame with mean values
dem.rep.compare <- rbind(Democrats = dem_ability$x, Republicans = rep_ability$x)
colnames(dem.rep.compare) <- dem_ability$Group.1

# generate barplot
barplot(dem.rep.compare, beside = T, legend = T)  # this is only somewhat informative
