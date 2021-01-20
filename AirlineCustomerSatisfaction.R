dev.off() # Clear the graph window
cat('\014') # Clear the console
rm(list=ls()) # Clear user objects from the environment

setwd('C:\\Users\\sheet\\Syracuse\\SUCourses\\Sem 1\\IST 687 - DS\\Project') # Set workspace directory where data files are stored 
getwd()

#install.packages("rjson")
#library(rjson)

#install.packages("dplyr")
library(dplyr)

data <- ("fall2019-survey-m05.json")

df <- jsonlite::fromJSON(data)
View(df)

#get dimension of dataframe
dim(df)

#get columns names
colnames(df)
#[1] "Destination.City"               "Origin.City"                   
#[3] "Airline.Status"                 "Age"                           
#[5] "Gender"                         "Price.Sensitivity"             
#[7] "Year.of.First.Flight"           "Flights.Per.Year"              
#[9] "Loyalty"                        "Type.of.Travel"                
#[11] "Total.Freq.Flyer.Accts"         "Shopping.Amount.at.Airport"    
#[13] "Eating.and.Drinking.at.Airport" "Class"                         
#[15] "Day.of.Month"                   "Flight.date"                   
#[17] "Partner.Code"                   "Partner.Name"                  
#[19] "Origin.State"                   "Destination.State"             
#[21] "Scheduled.Departure.Hour"       "Departure.Delay.in.Minutes"    
#[23] "Arrival.Delay.in.Minutes"       "Flight.cancelled"              
#[25] "Flight.time.in.minutes"         "Flight.Distance"               
#[27] "Likelihood.to.recommend"        "olong"                         
#[29] "olat"                           "dlong"                         
#[31] "dlat"                           "freeText"

temp_df <- df[c("Origin.City", "Origin.State", "Destination.City", "Destination.State", "Class",
                "Age", "Gender","Type.of.Travel", "Price.Sensitivity" ,
                "Year.of.First.Flight", "Flights.Per.Year", "Airline.Status", "Total.Freq.Flyer.Accts",
                "Loyalty", "Likelihood.to.recommend",
                "Shopping.Amount.at.Airport", "Eating.and.Drinking.at.Airport",
                "Partner.Code", "Partner.Name", "Flight.cancelled",
                "Flight.date", "Day.of.Month", "Scheduled.Departure.Hour", 
                "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes",
                "Flight.time.in.minutes", "Flight.Distance",
                "olong", "olat", "dlong", "dlat", "freeText")] 

dim(temp_df)
View(temp_df)
#colnames(temp_df)

#divide data to manipulate in
df_flightsummary <- temp_df[1:5]

df_flyersummary <- temp_df[6:13]

df_flightsuggestcolumns <- temp_df[14:15]

df_flyermoneyspent <- temp_df[16:17]

df_flightpartners <- temp_df[18:19]

df_flightdetails <- temp_df[20:27]

df_flightlonglat <- temp_df[28:31]

df_flyercomments <- temp_df[32]

#clean df_flightsummary-----------------------------------------------------------------------------------------
View(df_flightsummary)
str(df_flightsummary)

colSums(is.na(df_flightsummary)) 


#df_flightsummary$Origin.City <- gsub(",.*","",df_flightsummary$Origin.City)
#df_flightsummary$Destination.City <- gsub(",.*","",df_flightsummary$Destination.City)

df_flightsummary$Origin.City <- as.factor(df_flightsummary$Origin.City)
df_flightsummary$Origin.State <- as.factor(df_flightsummary$Origin.State)
df_flightsummary$Destination.City <- as.factor(df_flightsummary$Destination.City)
df_flightsummary$Destination.State <- as.factor(df_flightsummary$Destination.State)
df_flightsummary$Class <- as.factor(df_flightsummary$Class)

colnames(df_flightsummary) <- c("Origin City", "Origin State","Destination City","Destination State","Class")

summary(df_flightsummary)
str(df_flightsummary)

#clean df_flyersummary----------------------------------------------------------------------------------------
View(df_flyersummary)
str(df_flyersummary)

colSums(is.na(df_flyersummary))

df_flyersummary$Gender <- as.factor(df_flyersummary$Gender)
df_flyersummary$Airline.Status <- as.factor(df_flyersummary$Airline.Status)
df_flyersummary$Type.of.Travel <- as.factor(df_flyersummary$Type.of.Travel)
df_flyersummary$Flights.Per.Year <- as.numeric(df_flyersummary$Flights.Per.Year)
df_flyersummary$Total.Freq.Flyer.Accts <- as.numeric(df_flyersummary$Total.Freq.Flyer.Accts)

#table(df_flyersummary$Price.Sensitivity)

colnames(df_flyersummary) <- c("Age","Gender","Type of Travel","Price Sensitivity","Year of First Flight",
                               "Flights Per Year","Flyer Status","Total Frequent Flyer Accounts")

summary(df_flyersummary)
str(df_flyersummary)

#clean df_flightsuggestcolumns----------------------------------------------------------------------------------
View(df_flightsuggestcolumns)

colSums(is.na(df_flightsuggestcolumns))

df_flightsuggestcolumns[which(is.na(df_flightsuggestcolumns$Likelihood.to.recommend)),]

df_flightsuggestcolumns$Likelihood.to.recommend <- as.numeric(df_flightsuggestcolumns$Likelihood.to.recommend)
table(df_flightsuggestcolumns$Likelihood.to.recommend)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- getmode(df_flightsuggestcolumns$Likelihood.to.recommend)
print(result)

df_flightsuggestcolumns$Likelihood.to.recommend[is.na(df_flightsuggestcolumns$Likelihood.to.recommend)] <- result

colnames(df_flightsuggestcolumns) <- c("Loyalty","Likelihood to Recommend")

summary(df_flightsuggestcolumns)
str(df_flightsuggestcolumns)

#clean df_flyermoneyspent---------------------------------------------------------------------------------------
View(df_flyermoneyspent)
df_flyermoneyspent$Shopping.Amount.at.Airport[is.na(df_flyermoneyspent$Shopping.Amount.at.Airport)]
df_flyermoneyspent$Eating.and.Drinking.at.Airport[is.na(df_flyermoneyspent$Eating.and.Drinking.at.Airport)]

df_flyermoneyspent$Shopping.Amount.at.Airport <- as.numeric(df_flyermoneyspent$Shopping.Amount.at.Airport)
df_flyermoneyspent$Eating.and.Drinking.at.Airport <- as.numeric(df_flyermoneyspent$Eating.and.Drinking.at.Airport)

colnames(df_flyermoneyspent) <- c("Shopping at Airport", "Eating and Drinking at Airport")

summary(df_flyermoneyspent)
str(df_flyermoneyspent)

#clean df_flightpartners----------------------------------------------------------------------------------------
View(df_flightpartners)
df_flightpartners$Partner.Code[is.na(df_flightpartners$Partner.Code)]
df_flightpartners$Partner.Name[is.na(df_flightpartners$Partner.Name)]

df_flightpartners$Partner.Code <- as.factor(df_flightpartners$Partner.Code)
df_flightpartners$Partner.Name <- as.factor(df_flightpartners$Partner.Name)

table(df_flightpartners$Partner.Name)

colnames(df_flightpartners) <- c("Partner Code","Partner Name")

summary(df_flightpartners)

str(df_flightpartners)
#clean df_flightdetails-----------------------------------------------------------------------------------------
View(df_flightdetails)

df_flightdetails$Flight.cancelled[is.na(df_flightdetails$Flight.cancelled)]
df_flightdetails$Flight.date[is.na(df_flightdetails$Flight.date)]


df_flightdetails$Flight.cancelled <- as.factor(df_flightdetails$Flight.cancelled)

#df_flightdetails$Flight.date <- as.factor(df_flightdetails$Flight.date)
#df_flightdetails$Flight.newdate <- as.Date(df_flightdetails$Flight.date,"%d/%m/%Y")
#install.packages("tidyr")
library(tidyr)
df_flightdetails <- df_flightdetails %>%
  separate(Flight.date, sep="/", into = c("Month", "Day", "Year"))

df_flightdetails$Month <- as.numeric(df_flightdetails$Month)
df_flightdetails$Day.of.Month <- as.numeric(df_flightdetails$Day.of.Month)
df_flightdetails$Scheduled.Departure.Hour <- as.numeric(df_flightdetails$Scheduled.Departure.Hour)
df_flightdetails$Departure.Delay.in.Minutes <- as.numeric(df_flightdetails$Departure.Delay.in.Minutes)
df_flightdetails$Arrival.Delay.in.Minutes <- as.numeric(df_flightdetails$Arrival.Delay.in.Minutes)
df_flightdetails$Flight.time.in.minutes <- as.numeric(df_flightdetails$Flight.time.in.minutes)
df_flightdetails$Flight.Distance  <- as.numeric(df_flightdetails$Flight.Distance)

#df_flightsummary$Day <- gsub("/.*","",df_flightsummary$Flight.date)

df_flightdetails <- df_flightdetails[,-3:-4]

df_flightdetails$Flight.time.in.minutes[is.na(df_flightdetails$Flight.time.in.minutes)] <- mean(df_flightdetails$Flight.time.in.minutes, na.rm = TRUE)
df_flightdetails$Flight.Distance[is.na(df_flightdetails$Flight.Distance)] <- mean(df_flightdetails$Flight.Distance, na.rm = TRUE)

df_flightdetails$Departure.Delay.in.Minutes[is.na(df_flightdetails$Departure.Delay.in.Minutes)] <- mean(df_flightdetails$Departure.Delay.in.Minutes, na.rm = TRUE)
df_flightdetails$Arrival.Delay.in.Minutes[is.na(df_flightdetails$Arrival.Delay.in.Minutes)] <- mean(df_flightdetails$Arrival.Delay.in.Minutes, na.rm = TRUE)

colnames(df_flightdetails) <- c("Flight Cancelled","Month of Flight", "Day of Flight","Scheduled Time of Departure",
                                "Departure Delay In Minutes", "Arrival Delay in Minutes", "Flight Duration in Minutes", "Flight Distance")

summary(df_flightdetails)
str(df_flightdetails)

#clean df_flightlonglat-----------------------------------------------------------------------------------------
View(df_flightlonglat)

colnames(df_flightlonglat) <- c("Origin Longitude","Origin Latitude", "Destination Longitude", "Destination Latitude")

summary(df_flightlonglat)
str(df_flightlonglat)

#clean df_flyercomments-----------------------------------------------------------------------------------------
View(df_flyercomments)

colnames(df_flyercomments) <- c("Comments")

#merge dataframes into newdf dataframe--------------------------------------------------------------------------
new_df <- data.frame(df_flightsummary,df_flyersummary,df_flightsuggestcolumns,df_flyermoneyspent,df_flightpartners,
                     df_flightdetails,df_flightlonglat,df_flyercomments)
View(new_df)
summary(new_df)
str(new_df)

#data profiling----------------------------------------------------------
table(new_df$Origin.City)
table(new_df$Origin.State)
table(new_df$Destination.City)
table(new_df$Destination.State)
table(new_df$Class)
hist(new_df$Age)
table(new_df$Gender)
table(new_df$Type.of.Travel)
hist(new_df$Price.Sensitivity)
hist(new_df$Year.of.First.Flight)
hist(new_df$Flights.Per.Year)
table(new_df$Flyer.Status)
hist(new_df$Total.Frequent.Flyer.Accounts)
hist(new_df$Loyalty)
hist(new_df$Likelihood.to.Recommend)
hist(new_df$Shopping.at.Airport)
hist(new_df$Eating.and.Drinking.at.Airport)
table(new_df$Partner.Code)
table(new_df$Partner.Name)
hist(new_df$Month.of.Flight)
hist(new_df$Day.of.Flight)
hist(new_df$Scheduled.Time.of.Departure)
hist(new_df$Departure.Delay.In.Minutes)
hist(new_df$Arrival.Delay.in.Minutes)
hist(new_df$Flight.Duration.in.Minutes)
hist(new_df$Flight.Distance)
hist(new_df$Origin.Longitude)
hist(new_df$Origin.Latitude)
hist(new_df$Destination.Longitude)
hist(new_df$Destination.Latitude)

#drop unwanted columns -----------------------------------------------

View(new_df)
new_df1 <- subset(new_df, select=-c(Origin.City,Destination.City,Year.of.First.Flight,Partner.Code,
                                    Month.of.Flight,Day.of.Flight))

dim(new_df1)
str(new_df1)

new_df1$Likelihood.to.Recommend[new_df1$Likelihood.to.Recommend < 6] <- 1
#6 to 8 Medium Satisfaction  2
new_df1$Likelihood.to.Recommend[new_df1$Likelihood.to.Recommend >5 & new_df1$Likelihood.to.Recommend<9] <- 2
#9 to 10 High Satisfaction  3
new_df1$Likelihood.to.Recommend[new_df1$Likelihood.to.Recommend > 8] <- 3


cor_df <- new_df1

cor_df$Class <- as.character(cor_df$Class)
cor_df$Class[cor_df$Class == "Eco"] <- 0
cor_df$Class[cor_df$Class == "Eco Plus"] <- 1
cor_df$Class[cor_df$Class == "Business"] <- 2
cor_df$Class <- as.numeric(cor_df$Class)
#View(cor_df)

cor_df$Gender <- as.character(cor_df$Gender)
cor_df$Gender[cor_df$Gender == "Female"] <- 0
cor_df$Gender[cor_df$Gender == "Male"] <- 1
cor_df$Gender <- as.numeric(cor_df$Gender)

cor_df$Type.of.Travel <- as.character(cor_df$Type.of.Travel)
cor_df$Type.of.Travel[cor_df$Type.of.Travel == "Mileage tickets"] <- 0
cor_df$Type.of.Travel[cor_df$Type.of.Travel == "Personal Travel"] <- 1
cor_df$Type.of.Travel[cor_df$Type.of.Travel == "Business travel"] <- 2
cor_df$Type.of.Travel <- as.numeric(cor_df$Type.of.Travel)

cor_df$Flyer.Status <- as.character(cor_df$Flyer.Status)
cor_df$Flyer.Status[cor_df$Flyer.Status == "Blue"] <- 0
cor_df$Flyer.Status[cor_df$Flyer.Status == "Silver"] <- 1
cor_df$Flyer.Status[cor_df$Flyer.Status == "Gold"] <- 2
cor_df$Flyer.Status[cor_df$Flyer.Status == "Platinum"] <- 2
cor_df$Flyer.Status <- as.numeric(cor_df$Flyer.Status)

cor_df$Flight.Cancelled <- as.character(cor_df$Flight.Cancelled)
cor_df$Flight.Cancelled[cor_df$Flight.Cancelled == "Yes"] <- 0
cor_df$Flight.Cancelled[cor_df$Flight.Cancelled == "No"] <- 1
cor_df$Flight.Cancelled <- as.numeric(cor_df$Flight.Cancelled)

cor_df$Age <- as.numeric(cor_df$Age)
cor_df$Price.Sensitivity <- as.numeric(cor_df$Price.Sensitivity)
cor_df$Likelihood.to.Recommend <- as.numeric(cor_df$Likelihood.to.Recommend)

cor_num_df <-  subset(cor_df, select=-c(Partner.Name,Origin.State,Destination.State,Comments))
str(cor_num_df)
cor_pd_df <- cor(cor_num_df)

#Correlation Plot -----------------------------------------------------------------------
#install.packages("corrplot")
library(corrplot)
corrplot(cor_pd_df, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.5)

library(ggplot2)


# Satisfaction and Age
new_df1$Likelihood.to.Recommend <- as.factor(new_df1$Likelihood.to.Recommend)
sat_Age <- ggplot(new_df1,aes(x=Age))
sat_Age <- sat_Age + geom_histogram(aes(fill=Likelihood.to.Recommend),position = "dodge",bins=20)
sat_Age <- sat_Age + ggtitle("Satisfaction versus Age")
sat_Age

# Satisfaction and Flights Per Year
sat_flperyear <- ggplot(new_df1,aes(x=Flights.Per.Year))
sat_flperyear <- sat_flperyear + geom_histogram(aes(fill=Likelihood.to.Recommend),position = "dodge")
sat_flperyear <- sat_flperyear + ggtitle("Satisfaction versus No of Flights")
sat_flperyear

sat_flperyear_density <- ggplot(new_df1,aes(x=Flights.Per.Year)) +
  geom_density(aes(fill=Likelihood.to.Recommend),position = "fill")
sat_flperyear_density


# Satisfaction and Departure Delay
Dpdy96 <- quantile(new_df1$Departure.Delay.In.Minutes,0.96)
sat_depdelay <- ggplot(new_df1[new_df1$Departure.Delay.In.Minutes<Dpdy96,],aes(x=Departure.Delay.In.Minutes))
sat_depdelay <- sat_depdelay + geom_histogram(aes(fill=Likelihood.to.Recommend),position = "dodge")
sat_depdelay <- sat_depdelay + ggtitle("Satisfaction versus Departure Delay")
sat_depdelay

# Satisfaction and Arrival Delay
Ardy96 <- quantile(new_df1$Arrival.Delay.in.Minutes,0.96)
sat_arrdelay <-
  ggplot(new_df1[new_df1$Arrival.Delay.in.Minutes<Ardy96,],aes(x=Arrival.Delay.in.Minutes))
sat_arrdelay <- sat_arrdelay + geom_histogram(aes(fill=Likelihood.to.Recommend),position = "dodge")
sat_arrdelay <- sat_arrdelay + ggtitle("Satisfaction versus Arrival Delay")
sat_arrdelay

#Linear Regression -----------------------------------------------------
#install.packages("MASS")
library(MASS)

lr_df <- subset(cor_num_df, select=-c(Origin.Longitude,Origin.Latitude,Destination.Longitude,Destination.Latitude))

str(lr_df)


lr_model <- lm(formula = Likelihood.to.Recommend ~., data = lr_df)
stepAIC(lr_model)
summary(lr_model)


lr_model_2 <- lm(formula = Likelihood.to.Recommend ~  Class + Age + Gender + 
     Type.of.Travel + Price.Sensitivity + Flights.Per.Year + Flyer.Status + 
     Arrival.Delay.in.Minutes, data = lr_df)

summary(lr_model_2)





