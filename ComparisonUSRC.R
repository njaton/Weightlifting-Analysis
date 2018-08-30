#Nick Jaton 
#August 2018
#Copywrite 2018 
#Analysis of American, Russian and Chinese Weightlfting in the Olympics
#Data taken from Randi H Griffen via Kaggle

library(ggplot2)

athlete_events <- read.csv("~/Downloads/athlete_events.csv")
data <- as.data.frame(athlete_events)

#Seperate Weightlifting
weightlifting <- data[grepl("Weightlifting", data$Sport),]
#---------------------------------------------------------
#Seperate American Athletes 
American <- weightlifting[grepl("United States", weightlifting$Team),]

#Find total amount of athletes
lengthUS <- length(American$Team) #154 total lifters 

#Seperate data by sex
americanMaleAthletes <- American[grepl("M", American$Sex),]
americanFemaleAthletes <- American[grepl("F", American$Sex),]
#---------------------------------------------------------
#Seperate Russian Atheletes 
Russian <- weightlifting[grepl("Russia", weightlifting$Team),]

#Find total amount of atheltes
lengthRussia <- length(Russian$Team) #45 total lifters

#Seperate data by sex
russianMaleAthletes <- Russian[grepl("M", Russian$Sex),]
russianFemaleAthletes <- Russian[grepl("F", Russian$Sex),]
#--------------------------------------------------------
#Seperate Chinese Atheletes
Chinese <- weightlifting[grepl("China", weightlifting$Team),]

#Find total amount of athletes
lengthChinese <- length(Chinese$Team) #93 total lifters 

#Seperate data by sex
chineseMaleAthletes <- Chinese[grepl("M", Chinese$Sex),]
chineseFemaleAthletes <- Chinese[grepl("F", Chinese$Sex),]
#--------------------------------------------------------
#Graph data regarding athelete amounts
countries <- c(rep("United States", 2), rep("Russia", 2), rep("China", 2))
amountOfAthletesTotal <- c(length(americanMaleAthletes$Name), length(americanFemaleAthletes$Name),
                           length(russianMaleAthletes$Name), length(russianFemaleAthletes$Name), 
                           length(chineseMaleAthletes$Name), length(chineseFemaleAthletes$Name))
athleteSex<- rep(c("Male", "Female"), 3)
dataForPlot1 <- data.frame(countries, amountOfAthletesTotal, athleteSex)

# Grouped
plot1 <- ggplot(dataForPlot1, aes(fill=athleteSex, y=amountOfAthletesTotal, x=countries)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("Comparing Athlete Quantities by Team") + 
  xlab("Team (Country)") + ylab("Amount of Athletes") + labs(fill="Gender") + 
  scale_fill_brewer(palette = "Accent")
plot1 + theme(plot.title = element_text(hjust = .5))
#-------------------------------------------------------
#Create the same graph for athletes that place in the top 3

#male athletes that placed
maleMedalUS <- americanMaleAthletes[grepl("", americanMaleAthletes$Medal),]
maleMedalRussia <- russianMaleAthletes[grepl("", russianMaleAthletes$Medal),]
maleMedalChina <- chineseMaleAthletes[grepl("", chineseMaleAthletes$Medal),]

#female athletes that placed
femaleMedalUs <- americanFemaleAthletes[grepl("", americanFemaleAthletes$Medal),]
femaleMedalRussia <- russianFemaleAthletes[grepl("", russianFemaleAthletes$Medal),]
femaleMedalChina <- chineseFemaleAthletes[grepl("", chineseFemaleAthletes$Medal),]

amountOfAthletesTotalMedals <- c(length(maleMedalUS$Name), length(femaleMedalUs$Name),
                           length(maleMedalRussia$Name), length(femaleMedalRussia$Name), 
                           length(maleMedalChina$Name), length(femaleMedalChina$Name))
dataforPlot2 <- data.frame(countries, amountOfAthletesTotalMedals, athleteSex)

plot2 <- ggplot(dataforPlot2, aes(fill=athleteSex, y=amountOfAthletesTotalMedals, x=countries)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("Comparing Quantities of Winning Athletes by Team") + 
  xlab("Team (Country)") + ylab("Amount of Athletes") + labs(fill="Gender") + 
  scale_fill_brewer(palette = "Dark2")
plot2 + theme(plot.title = element_text(hjust = .5))
#-----------------------------------------------------