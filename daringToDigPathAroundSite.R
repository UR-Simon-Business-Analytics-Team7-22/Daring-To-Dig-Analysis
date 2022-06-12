# Script for finding the daringToDig landing page people, and see what pages they
# Viewed outside of daring to dig as well

library(googleAnalyticsR)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(gridExtra) 
library(maps)
library(stringr)
ga_auth()

# api call of sessions 
data <- google_analytics (209738439,
                          date_range = c('2021-03-01','2021-05-31'),
                          metrics = c('sessionDuration'),
                          dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'pagePath', 'sourceMedium','latitude','longitude'),
                          anti_sample = TRUE)

data2 <- google_analytics (209738439,
                           date_range = c('2021-06-01','2021-07-31'),
                           metrics = c('sessionDuration'),
                           dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'pagePath', 'sourceMedium', 'latitude','longitude'),
                           anti_sample = TRUE)

data3 <- google_analytics (209738439,
                           date_range = c('2021-08-01','2021-09-30'),
                           metrics = c('sessionDuration'),
                           dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'pagePath', 'sourceMedium', 'latitude','longitude'),
                           anti_sample = TRUE)

data4 <- google_analytics (209738439,
                           date_range = c('2021-10-01','2021-11-30'),
                           metrics = c('sessionDuration'),
                           dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'pagePath', 'sourceMedium', 'latitude','longitude'),
                           anti_sample = TRUE)

data5 <- google_analytics (209738439,
                           date_range = c('2021-12-01','2022-01-31'),
                           metrics = c('sessionDuration'),
                           dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'pagePath', 'sourceMedium', 'latitude','longitude'),
                           anti_sample = TRUE)

data6 <- google_analytics (209738439,
                           date_range = c('2022-02-01','2022-02-28'),
                           metrics = c('sessionDuration'),
                           dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'pagePath', 'sourceMedium', 'latitude','longitude'),
                           anti_sample = TRUE)

data7 <- rbind(data,data2,data3,data4,data5,data6)


#get sessions where the landing page was the daring to dig exhibit
daringToDig <- data7[grepl('^/daring-to-dig', data7$landingPagePath),]


# Goal is to find each session where either the pagePath, or the exitPagePath
# is not a part of daringToDig. 

# Create a new column which is going to be used for analysis on exitPagePath
# This column removes the / that is at the start of each page path
daringToDig$exitPageAnalysis = substring(daringToDig$exitPagePath, 2)
daringToDig$pagePathAnalysis = substring(daringToDig$pagePath, 2)

# Create a new column which gives the path at the beginning of the page path,
# example if path is "/about/about-the-museum" then this will provide "about" 
daringToDig$firstExitPathValue = sub(pattern = '(/.+)', replacement = '', 
                                     x = daringToDig$exitPageAnalysis)
daringToDig$firstPagePathValue = sub(pattern = '(/.+)', replacement = '', 
                                     x = daringToDig$pagePathAnalysis)

# Simplify the daring-to-dig pathways
daringToDig$firstExitPathValue = sub(pattern = 'daring-to-dig.+', replacement = 'daring-to-dig',
                                     x = daringToDig$firstExitPathValue)
daringToDig$firstPagePathValue = sub(pattern = 'daring-to-dig.+', replacement = 'daring-to-dig',
                                     x = daringToDig$firstPagePathValue)

# Simplify the daring-to-dig pathways for exhibits
daringToDig$firstExitPathValue = sub(pattern = 'exhibit.+', replacement = 'exhibit',
                                     x = daringToDig$firstExitPathValue)
daringToDig$firstPagePathValue = sub(pattern = 'exhibit.+', replacement = 'exhibit',
                                     x = daringToDig$firstPagePathValue)

# Simplify the daring-to-dig pathways for bees
daringToDig$firstExitPathValue = sub(pattern = 'bees.+', replacement = 'bees',
                                     x = daringToDig$firstExitPathValue)
daringToDig$firstPagePathValue = sub(pattern = 'bees.+', replacement = 'bees',
                                     x = daringToDig$firstPagePathValue)

# Create a dataframe with only people who landed on daring to dig, but exited
# on some other page
daringToDigExitElsewhere <- daringToDig[!grepl('^/daring-to-dig', daringToDig$exitPagePath),]

# Create a dataframe with only people who landed on daring to dig, but were on
# some other page for their pagePath
daringToDigPagePathElsewhere <- daringToDig[!grepl('^/daring-to-dig', daringToDig$pagePath),]

# Create a table giving the count of each time a specific path occured for the exit page
countFirstExitPath = table(daringToDigExitElsewhere$firstExitPathValue)

# Create a table giving the count of each time a specific path occured for the page Path
countFirstPagePath = table(daringToDigPagePathElsewhere$firstPagePathValue)

# Turn each table into a dataframe 
as.data.frame(countFirstExitPath)
as.data.frame(countFirstPagePath)

# Merge the two dataframes together on car1
countFirstPath = merge(countFirstExitPath, countFirstPagePath, by = 'Var1', all = TRUE)


# Rename the columns in the final dataframe to make it more interpretable
countFirstPath = rename(countFirstPath, pathTitle = Var1)
countFirstPath =rename(countFirstPath, exitPage = Freq.x)
countFirstPath =rename(countFirstPath, pagePath = Freq.y)

# Print the dataframe to check work
countFirstPath
# Create a plot of the exit page path by pathTitle
barplot(countFirstPath$exitPage, main = 'exitPageFreq', names.arg = countFirstPath$pathTitle)



