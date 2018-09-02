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
  scale_fill_brewer(palette = "Accent") +
  geom_text(aes(label=amountOfAthletesTotal), position = position_dodge(width=0.9), vjust=-0.15)
plot1 + theme(plot.title = element_text(hjust = .5), legend.position = "bottom",
              legend.background = element_rect(color = "grey", 
              fill = "grey90", size = 1, linetype = "solid"))
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
  scale_fill_brewer(palette = "Dark2") + 
  geom_text(aes(label=amountOfAthletesTotalMedals), position = position_dodge(width=0.9), vjust=-0.15)
plot2 + theme(plot.title = element_text(hjust = .5),legend.position = "bottom",
              legend.background = element_rect(color = "grey", 
              fill = "grey90", size = 1, linetype = "solid"))
#-----------------------------------------------------
#find the percent of athletes in each team that take home a medal
#-----------------------------------------------------
#Find US Stats 
#Male
length(americanMaleAthletes$Medal) #139 
maleGoldUS <- maleMedalUS[grepl("Gold", maleMedalUS$Medal),] #15
maleSilverUS <- maleMedalUS[grepl("Silver", maleMedalUS$Medal),] #16
maleBronzeUS <- maleMedalUS[grepl("Bronze", maleMedalUS$Medal),] #8 

maleGoldUSPercent <- (15 / 139) * 100 #10.79137
maleSilverUSPercent <- (16 / 139) * 100 #11.51079
maleBronzeUSPercent <-  (8 / 139) * 100 #5.755396
print((length(maleMedalUS$Medal) / 139) * 100) #28.05755

#Female 
length(americanFemaleAthletes$Medal) #15
femaleGoldUS <- femaleMedalUS[grepl("Gold", femaleMedalUS$Medal),] #1
femaleSilverUS <- femaleMedalUS[grepl("Silver", femaleMedalUS$Medal),] #0
femaleBronzeUS <- femaleMedalUS[grepl("Bronze", femaleMedalUS$Medal),] #2

femaleGoldUSPercent <- (1 / 15) * 100 #6.666667
femaleSilverUSPercent <- 0 #0
femaleBronzeUSPercent <- (2 / 15) #13.33333
print((length(femaleMedalUS$Medal) / 15) * 100) #20
#-----------------------------------------------------
#Find Russian Stats
#Male
length(russianMaleAthletes$Medal) #31
maleGoldRussia <- maleMedalRussia[grepl("Gold", maleMedalRussia$Medal),] #3
maleSilverRussia <- maleMedalRussia[grepl("Silver", maleMedalRussia$Medal),] #6
maleBronzeRussia <- maleMedalRussia[grepl("Bronze", maleMedalRussia$Medal),] #7

maleGoldRussiaPercent <- (3 / 31) * 100 #9.677419
maleSilverRussiaPercent <- (6 / 31) * 100 #19.35484
maleBronzeRussiaPercent <- (7 / 31) * 100 #22.58065
print((length(maleMedalRussia$Medal) / 31) * 100) #51.6129

#Female 
length(russianFemaleAthletes$Medal) #14
femaleGoldRussia <- femaleMedalRussia[grepl("Gold", femaleMedalRussia$Medal),] #0
femaleSilverRussia <- femaleMedalRussia[grepl("Silver", femaleMedalRussia$Medal),] #7
femaleBronzeRussia <- femaleMedalRussia[grepl("Bronze", femaleMedalRussia$Medal),] #2

femaleGoldRussiaPercent <- 0 #0
femaleSilverRussiaPercent <- (7 / 14) * 100 #50
femaleBronzeRussiaPercent <- (2 / 14) * 100#14.28571
print((length(femaleMedalRussia$Medal) / 14) * 100) #71.42857
#-----------------------------------------------------
#Find Chinese Stats
#Male
length(chineseMaleAthletes$Medal) #73
maleGoldChina <- maleMedalChina[grepl("Gold", maleMedalChina$Medal),] #17
maleSilverChina <- maleMedalChina[grepl("Silver", maleMedalChina$Medal),] #14
maleBronzeChina <- maleMedalChina[grepl("Bronze", maleMedalChina$Medal),] #8

maleGoldChinaPercent <- (17 / 73) * 100 #23.28767
maleSilverChinaPercent <- (14 / 73) * 100 #19.17808
maleBronzeChinaPercent <- (8 / 73) * 100 #10.9589
print((length(maleMedalChina$Medal) / 73) * 100) #53.42466

#Female 
length(chineseFemaleAthletes$Medal) #20
femaleGoldChina <- femaleMedalChina[grepl("Gold", femaleMedalChina$Medal),] #17
femaleSilverChina <- femaleMedalChina[grepl("Silver", femaleMedalChina$Medal),] #1
femaleBronzeChina <- femaleMedalChina[grepl("Bronze", femaleMedalChina$Medal),] #0

femaleGoldChinaPercent <- (17 / 20) * 100 #85
femaleSilverChinaPercent <- (1 / 20) * 100 #5
femaleBronzeChinaPercent <- 0 #0
print((length(femaleMedalChina$Medal) / 20) * 100)#90
#---------------------------------------------------
#Graph data regarding athlete amounts
#Male Athletes
countries2 <- c(rep("United States", 3), rep("Russia", 3), rep("China", 3))
malePercentagePerTeam <- c(round(maleGoldUSPercent, digits = 2),round(maleSilverUSPercent, digits = 2), 
                           round(maleBronzeUSPercent, digits = 2), round(maleGoldRussiaPercent, digits = 2), 
                           round(maleSilverRussiaPercent, digits = 2), round(maleBronzeRussiaPercent, digits = 2),
                           round(maleGoldChinaPercent, digits = 2), round(maleSilverChinaPercent, digits = 2),
                           round(maleBronzeChinaPercent, digits = 2))
medalsPlot3 <- rep(c("Gold", "Silver", "Bronze"), 3)
dataForPlot3 <- data.frame(countries2, malePercentagePerTeam, medalsPlot3)

#plot the data
plot3 <- ggplot(dataForPlot3, aes(fill=medalsPlot3, y=malePercentagePerTeam, x=countries2)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("Percent of Winning Athletes by Team (Male)") + 
  xlab("Team (Country)") + ylab("Winning Percentage Out of the Team") + labs(fill="Medal") + 
  scale_fill_brewer(palette = "Set2") + 
  geom_text(aes(label=malePercentagePerTeam), position = position_dodge(width=0.9), vjust=-0.25)
plot3 + theme(plot.title = element_text(hjust = .5), legend.position = c(0.9, 0.8),
              legend.background = element_rect(color = "grey", 
              fill = "grey90", size = 1, linetype = "solid"))
#---------------------------------------------------
#Graph data regarding athlete amounts
#Female Athletes
femalePercentagePerTeam <- c(round(femaleGoldUSPercent, digits = 2), round(femaleSilverUSPercent, digits = 2),
                             round(femaleBronzeUSPercent, digits = 2), round(femaleGoldRussiaPercent, digits = 2),
                             round(femaleSilverRussiaPercent, digits = 2), round(femaleBronzeRussiaPercent, digits = 2),
                             round(femaleGoldChinaPercent, digits = 2), round(femaleSilverChinaPercent, digits = 2),
                             round(femaleBronzeChinaPercent, digits = 2))
dataForPlot4 <- data.frame(countries2,femalePercentagePerTeam, medalsPlot3)

#Plot4
plot4 <- ggplot(dataForPlot4, aes(fill=medalsPlot3, y=femalePercentagePerTeam, x=countries2)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("Percent of Winning Athletes by Team (Female)") + 
  xlab("Team (Country)") + ylab("Winning Percentage") + labs(fill="Medal") + 
  scale_fill_brewer(palette = "Set2") + 
  geom_text(aes(label=femalePercentagePerTeam), position = position_dodge(width=0.9), vjust=-0.25)
plot4 + theme(plot.title = element_text(hjust = .5), legend.position = c(0.9, 0.8),
              legend.background = element_rect(color = "grey", 
              fill = "grey90", size = 1, linetype = "solid"))
#--------------------------------------------------
#Quick Math for Results section 

#Percent of male athlete quantities against US
print(73 / 139 * 100) #China 
print(31 / 139 * 100) #Russio

#Chinese percent of medal earning male athletes
print(39 / 73 * 100) #52.42%
#Russsian percent of medal earning male athletes
print(16 / 31 * 100) #51.61%
#US percent of medal earning male athletes
print(39 / 139 * 100) #28.06%

#Chinese percent of medal earning female athletes
print(18 / 20 * 100) #90%
#Russsian percent of medal earning female athletes
print(10 / 14 * 100) #71.43%
#US percent of medal earning feathletes
print(3 / 15 * 100) #20%
