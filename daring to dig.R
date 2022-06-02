#load packages

library(googleAnalyticsR)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(gridExtra) 
library(maps)
ga_auth()

# api call of sessions 

data <- google_analytics (209738439,
                          date_range = c('2021-03-01','2021-05-31'),
                          metrics = c('sessionDuration'),
                          dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'sourceMedium','latitude','longitude'),
                          anti_sample = TRUE)

data2 <- google_analytics (209738439,
                          date_range = c('2021-06-01','2021-07-31'),
                          metrics = c('sessionDuration'),
                          dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'sourceMedium', 'latitude','longitude'),
                          anti_sample = TRUE)

data3 <- google_analytics (209738439,
                           date_range = c('2021-08-01','2021-09-30'),
                           metrics = c('sessionDuration'),
                           dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'sourceMedium', 'latitude','longitude'),
                           anti_sample = TRUE)

data4 <- google_analytics (209738439,
                           date_range = c('2021-10-01','2021-11-30'),
                           metrics = c('sessionDuration'),
                           dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'sourceMedium', 'latitude','longitude'),
                           anti_sample = TRUE)

data5 <- google_analytics (209738439,
                           date_range = c('2021-12-01','2022-01-31'),
                           metrics = c('sessionDuration'),
                           dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'sourceMedium', 'latitude','longitude'),
                           anti_sample = TRUE)

data6 <- google_analytics (209738439,
                           date_range = c('2022-02-01','2022-02-28'),
                           metrics = c('sessionDuration'),
                           dimensions = c('clientID','date','userType', 'landingPagePath', 'exitPagePath', 'sourceMedium', 'latitude','longitude'),
                           anti_sample = TRUE)

data7 <- rbind(data,data2,data3,data4,data5,data6)

#---------------------------------Daring to Dig Time Series Analysis------------------------

#get sessions where the landing page was the daring to dig exhibit

daringToDig <- data7[grepl('^/daring-to-dig', data7$landingPagePath),]



#Separate Sessions into buckets based off of session duration


#get the session duration of an individual session
    
    
#if session is less than 10, assign it to 0 to 10 bucket and so on

SessionDurationBucket <- c()

for (i in 1:nrow(daringToDig)){
    
    if (daringToDig[i,7]<= 10){
        SessionDurationBucket[i] <- 'zero_to_ten'
    } 
    if (daringToDig[i,7] > 10 & daringToDig[i,7] <= 60){
        SessionDurationBucket[i] <- 'ten_to_sixty'
    } 
    if (daringToDig[i,7] > 60){
        SessionDurationBucket[i] <-'sixty_plus'
    }
    
    print(i)
    
}
   

#add column to the dataframe

daringToDig$SessionDurationBucket <- SessionDurationBucket

daringToDigTS <- daringToDig %>% group_by(date,SessionDurationBucket) %>% summarize(total_users = length(unique(clientID))) %>% arrange(date)

#reshape data from long format to wide format

daringToDigWide <-  dcast(daringToDigTS, date~SessionDurationBucket,sum)


#create time series plots 

plot1 <- ggplot(daringToDigWide) + 
    geom_line(aes(x = date,
                  y = zero_to_ten
    ), color = 'Red') 
        
plot2 <- ggplot(daringToDigWide) + 
    geom_line(aes(x = date,
                  y = ten_to_sixty
    ), color = 'Blue') 

plot3 <- ggplot(daringToDigWide) + 
    geom_line(aes(x = date,
                  y = sixty_plus
    ), color = 'Green') 

# Put all three plots side by side in a single image

grid.arrange(plot1, plot2, plot3, ncol = 3, top = "Daring To Dig : Unique Sessions Per Day", left = "Session Duration Range (Seconds)") 

#--------------------------------Analysis of Daring To Dig Landing Pages-----------

#Group by landing page and take average session duration and number of unique users

daringToDigSubPages <- daringToDig %>% group_by(landingPagePath) %>% summarize(average_session_duration = mean(sessionDuration), total_unique_users = length(unique(clientID))) %>% arrange(-average_session_duration)

#Average Session Duration of Daring To Dig's Landing Pages
LandingAvgSessDuration <- mean(daringToDigSubPages$average_session_duration)

#Average Unique Users of Daring To Dig's Landing Pages

LandingAvgUniqueUsers <- mean(daringToDigSubPages$total_unique_users)

#empty vector

aboveAvgVec <- c()

#for each daring to dig landing page, mark as true if the page has higher than average avg session duration and total unique users
for (j in 1:nrow(daringToDigSubPages)){
    if (daringToDigSubPages[j,2] > LandingAvgSessDuration & daringToDigSubPages[j,3] > LandingAvgUniqueUsers){
        aboveAvgVec[j] <- TRUE
    } else{ aboveAvgVec[j] <- FALSE}
}

#add vector indicating whether or not the landing page is above average to the data frame

daringToDigSubPages$AboveAverage <- aboveAvgVec


daringAboveAvg <- daringToDigSubPages[aboveAvgVec,]

ggplot(data=daringAboveAvg, aes(x=landingPagePath, y=average_session_duration)) +
    geom_bar(stat="identity", fill = 'steelblue', width=0.5) + theme(axis.text.x = element_text(angle = 90)) +
    ggtitle('Above Average Landing Pages Session Duration')

ggplot(data=daringAboveAvg, aes(x=landingPagePath, y=total_unique_users)) +
    geom_bar(stat="identity", fill = 'darkgreen', width=0.5) + theme(axis.text.x = element_text(angle = 90)) +
    ggtitle('Above Average Landing Pages Total Unique Users')

#------------------------------heat map--------------------------------------------

daringToDigGeo<- daringToDig %>% group_by(latitude,longitude) %>% summarize(unique_users = length(unique(clientID)))



