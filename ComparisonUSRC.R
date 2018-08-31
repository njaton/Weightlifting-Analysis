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
femaleMedalUS <- americanFemaleAthletes[grepl("", americanFemaleAthletes$Medal),]
femaleMedalRussia <- russianFemaleAthletes[grepl("", russianFemaleAthletes$Medal),]
femaleMedalChina <- chineseFemaleAthletes[grepl("", chineseFemaleAthletes$Medal),]

amountOfAthletesTotalMedals <- c(length(maleMedalUS$Name), length(femaleMedalUS$Name),
                           length(maleMedalRussia$Name), length(femaleMedalRussia$Name), 
                           length(maleMedalChina$Name), length(femaleMedalChina$Name))
dataforPlot2 <- data.frame(countries, amountOfAthletesTotalMedals, athleteSex)

plot2 <- ggplot(dataforPlot2, aes(fill=athleteSex, y=amountOfAthletesTotalMedals, x=countries)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("Comparing Quantities of Winning Athletes by Team") + 
  xlab("Team (Country)") + ylab("Amount of Athletes") + labs(fill="Gender") + 
  scale_fill_brewer(palette = "Dark2")
plot2 + theme(plot.title = element_text(hjust = .5))
#-----------------------------------------------------
#find the percent of athletes in each team that take home a medal
#-----------------------------------------------------
#Find US Stats 
#Male
length(americanMaleAthletes$Medal) #139 
maleGoldUS <- maleMedalUS[grepl("Gold", maleMedalUS$Medal),] #15
maleSilverUS <- maleMedalUS[grepl("Silver", maleMedalUS$Medal),] #16
maleBronzeUS <- maleMedalUS[grepl("Bronze", maleMedalUS$Medal),] #8 

print(15 / 139) #.1079137
print(16 / 139) #0.1151079
print(8 / 139) #0.05755396
print(length(maleMedalUS$Medal) / 139) #0.2805755

#Female 
length(americanFemaleAthletes$Medal) #15
femaleGoldUS <- femaleMedalUS[grepl("Gold", femaleMedalUS$Medal),] #1
femaleSilverUS <- femaleMedalUS[grepl("Silver", femaleMedalUS$Medal),] #0
femaleBronzeUS <- femaleMedalUS[grepl("Bronze", femaleMedalUS$Medal),] #2

print(1 / 15) #0.06666667
print(0 / 15) #0
print(2 / 15) #0.1333333
print(length(femaleMedalUS$Medal) / 15) #0.2
#-----------------------------------------------------
#Find Russian Stats
#Male
length(russianMaleAthletes$Medal) #31
maleGoldRussia <- maleMedalRussia[grepl("Gold", maleMedalRussia$Medal),] #3
maleSilverRussia <- maleMedalRussia[grepl("Silver", maleMedalRussia$Medal),] #6
maleBronzeRussia <- maleMedalRussia[grepl("Bronze", maleMedalRussia$Medal),] #7

print(3 / 31) #0.09677419
print(6 / 31) #0.1935484
print(7 / 31) #0.2258065
print(length(maleMedalRussia$Medal) / 31) #0.516129

#Female 
length(russianFemaleAthletes$Medal) #14
femaleGoldRussia <- femaleMedalRussia[grepl("Gold", femaleMedalRussia$Medal),] #0
femaleSilverRussia <- femaleMedalRussia[grepl("Silver", femaleMedalRussia$Medal),] #7
femaleBronzeRussia <- femaleMedalRussia[grepl("Bronze", femaleMedalRussia$Medal),] #2

print(0 / 14) #0
print(7 / 14) #0.05
print(2 / 14) #0.1428571
print(length(femaleMedalRussia$Medal) / 14) #0.7142857
#-----------------------------------------------------
#Find Chinese Stats
#Male
length(chineseMaleAthletes$Medal) #73
maleGoldChina <- maleMedalChina[grepl("Gold", maleMedalChina$Medal),] #17
maleSilverChina <- maleMedalChina[grepl("Silver", maleMedalChina$Medal),] #14
maleBronzeChina <- maleMedalChina[grepl("Bronze", maleMedalChina$Medal),] #8

print(17 / 73) #0.2328767
print(14 / 73) #0.1917808
print(8 / 73) #0.109589
print(length(maleMedalChina$Medal) / 73) #0.5342466

#Female 
length(chineseFemaleAthletes$Medal) #20
femaleGoldChina <- femaleMedalChina[grepl("Gold", femaleMedalChina$Medal),] #17
femaleSilverChina <- femaleMedalChina[grepl("Silver", femaleMedalChina$Medal),] #1
femaleBronzeChina <- femaleMedalChina[grepl("Bronze", femaleMedalChina$Medal),] #0

print(17 / 20) #0.85
print(1 / 20) #0.05
print(0 / 20) #0
print(length(femaleMedalChina$Medal) / 20) #0.9
#---------------------------------------------------

