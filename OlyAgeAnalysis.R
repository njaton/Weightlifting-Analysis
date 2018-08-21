#Nick Jaton 
#August 2018
#Copywrite 2018 
#Analysis of Weightlifting age of medalists in the Olympics
#Data taken from Randi H Griffen via Kaggle

athlete_events <- read.csv("~/Downloads/athlete_events.csv")

data <- as.data.frame(athlete_events)

#Seperate Weightlifting
weightlifting <- data[grepl("Weightlifting", data$Sport),]

#Seperate Medal winners
bronze <- weightlifting[grepl("Bronze", weightlifting$Medal),]
silver <- weightlifting[grepl("Silver", weightlifting$Medal),]
gold <- weightlifting[grepl("Gold", weightlifting$Medal),]

#-------------------------------------------------------------------------
#Bronze analysis 
bronzeM <- bronze[grepl("M", bronze$Sex),]
bronzeF <- bronze[grepl("F", bronze$Sex),]

#Look into the age of the bronze medalists
barplot(table(bronzeM$Age), main = "Age of Men Bronze Medalists", xlab = "Age of Athlete", 
        ylab = "Occurance Rate", col = c("blue"))
barplot(table(bronzeF$Age), main = "Age of Women Bronze Medalists", xlab = "Age of Athlete", 
        ylab = "Occurance Rate", col = c("red"))

#mean calculations
bmM <- mean(as.numeric(bronzeM$Age), na.rm = TRUE)
bmF <- mean(as.numeric(bronzeF$Age), na.rm = TRUE)

#percent difference
bpdif <- (bmM / bmF)
#------------------------------------------------------------------------
#Silver analysis 
silverM <- silver[grepl("M", silver$Sex),]
silverF <- silver[grepl("F", silver$Sex),]

#Look into the age of the silver medalists
barplot(table(silverM$Age), main = "Age of Men Silver Medalists", xlab = "Age of Athlete",
        ylab = "Occurance Rate", col = c("blue"))
barplot(table(silverF$Age), main = "Age of Women Silver Medalists", xlab = "Age of Athlete",
        ylab = "Occurance Rate", col = c("red"))

#mean calculations
smM <- mean(as.numeric(silverM$Age), na.rm = TRUE)
smF <- mean(as.numeric(silverF$Age), na.rm = TRUE)

#percent difference
spdif <- (smM / smF)
#-----------------------------------------------------------------------
#gold analysis 
goldM <- gold[grepl("M", gold$Sex),]
goldF <- gold[grepl("F", gold$Sex),]

#Look into the age of the bronze medalists
barplot(table(goldM$Age), main = "Age of Men Gold Medalists", xlab = "Age of Athlete", 
        ylab = "Occurance Rate", col = c("blue"))
barplot(table(goldF$Age), main = "Age of Women Gold Medalists", xlab = "Age of Athlete", 
        ylab = "Occurance Rate", col = c("red"))

#mean calculations
gmM <- mean(as.numeric(goldM$Age), na.rm = TRUE)
gmF <- mean(as.numeric(goldF$Age), na.rm = TRUE)

#percent difference
gpdif <- (gmM / gmF)
#----------------------------------------------------------------------
#Overall age comparisons between medalists 
totalpdif <- (bpdif + spdif + gpdif) / 3 
#----------------------------------------------------------------------
#Compare medal winning athletes with non-winning athletes

#athletes that did NOT win in any event
noMedal <- weightlifting[!grepl("", weightlifting$Medal),]
noMedalM <- noMedal[grepl("M", noMedal$Sex),]
noMedalF <- noMedal[grepl("F", noMedal$Sex),]

#Graph representing age data for athletes that did not take home a medal
barplot(table(noMedalM$Age), main = "Age of Men (No Medal)", xlab = "Age of Athlete", 
        ylab = "Occurance Rate", col = c("blue"))
barplot(table(noMedalF$Age), main = "Age of Women (No Medal)", xlab = "Age of Athlete", 
        ylab = "Occurance Rate", col = c("red"))

nonMpdif <- (gmM / gmF)

meanNoMedalM <- mean(as.numeric(noMedalM$Age), na.rm = TRUE)
meanNoMedalF <- mean(as.numeric(noMedalF$Age), na.rm = TRUE)

#Compare against gold
difGoldVsNoneM <- (meanNoMedalM / gmM) * 100
difGoldVsNoneF <- (meanNoMedalM / gmF) * 100

#Compare against silver
difSilVsNoneM <- (meanNoMedalM / smM) * 100
difSilvVsNoneF <- (meanNoMedalM / smF) * 100

#compare against bronze
difBroVsNoneM <- (meanNoMedalM / bmM) * 100
difBroVsNoneF <- (meanNoMedalM / bmF) * 100

#Create graphs to represent the lifters average age
meansF <- c(meanNoMedalF, bmF, smF, gmF)
meansM <- c(meanNoMedalM, bmM, smM, gmM)

barplot(meansM, names.arg = c("None", "Bronze", "Silver", "Gold"),
        main = "Average Age of Male Athletes", ylab = "Average Age",
        xlab = "Medal Placement", col = c("blue"))
barplot(meansF, names.arg = c("None", "Bronze", "Silver", "Gold"), 
        main = "Average Age of Female Athletes", ylab = "Average Age",
        xlab = "Medal Placement", col = c("red"))
#-------------------------------------------------------------------
#quick math section for report. 
print((gmM + gmF) / 2)  
