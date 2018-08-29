#Nick Jaton 
#August 2018
#Copywrite 2018 
#Analysis of Weightlifting age of medalists in the Olympics
#Data taken from Randi H Griffen via Kaggle

library(ggplot2)

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
meanNames<- c(rep("No Medal", 2), rep("Bronze", 2), rep("Silver", 2), rep("Gold", 2))
meanSex<- rep(c("Female", "Male"), 2)
meanBoth <- c(meanNoMedalF, meanNoMedalM, bmF, bmM, smF, smM, gmF, gmM)
meanFrame <- data.frame(meanSex, meanNames,meanBoth)

# Grouped
agePlot <- ggplot(meanFrame, aes(fill=meanSex, y=meanBoth, x=meanNames)) +
            geom_bar(position="dodge", stat="identity") + ggtitle("Average Age of Medal Winning Athletes") + 
            xlab("Medal Earned") + ylab("Age (Years)") + labs(fill="Gender") 
agePlot + theme(plot.title = element_text(hjust = .5))
         

#-------------------------------------------------------------------
#quick math section for report. 
print((gmM + gmF) / 2)  
#------------------------------------------------------------------
#Seperate lifters into different age groups 

#find max / min ages 
which.min(weightlifting$Age) # At location 66
which.max(weightlifting$Age) # At location 3101

print(weightlifting$Age[66]) # 15 years of age is the lowest age
print(weightlifting$Age[3101]) #  45 years of age is the lowest age 

under21 <- weightlifting[weightlifting$Age < 21,]
under21M <- under21[grepl("M", under21$Sex),]
under21F <- under21[grepl("F", under21$Sex),]

under26 <- weightlifting[weightlifting$Age >= 21,]
under26 <- under26[under26$Age <= 25,]
under26M <- under26[grepl("M", under26$Sex),]
under26F <- under26[grepl("F", under26$Sex),]

under31 <- weightlifting[weightlifting$Age > 25,]
under31 <- under31[under31$Age <= 30,]
under31M <- under31[grepl("M", under31$Sex),]
under31F <- under31[grepl("F", under31$Sex),]

above31 <- weightlifting[weightlifting$Age > 30,]
above31M <- above31[grepl("M", above31$Sex),]
above31F <- above31[grepl("F", above31$Sex),]

ageNA <- weightlifting[is.na(weightlifting$Age),]
ageNAM <- ageNA[grepl("M", ageNA$Sex),]
ageNAF <- ageNA[grepl("F", ageNA$Sex),]

#-----------------------------------------------------------------
#Find out the amount of medal winners under each age range

glu21 <- length(grep("Gold", under21$Medal)) #22 
slu21 <- length(grep("Silver", under21$Medal)) #25 
blu21 <- length(grep("Bronze", under21$Medal)) #18

glu26 <- length(grep("Gold", under26$Medal)) #106
slu26 <- length(grep("Silver", under26$Medal)) #96
blu26 <- length(grep("Bronze", under26$Medal)) #88

glu31 <- length(grep("Gold", under31$Medal)) #66
slu31 <- length(grep("Silver", under31$Medal)) #68
blu31 <- length(grep("Bronze", under31$Medal)) #78

gab31 <- length(grep("Gold", above31$Medal)) #22
sab31 <- length(grep("Silver", above31$Medal)) #21 
bab31 <- length(grep("Bronze", above31$Medal)) #26

gan <- length(grep("Gold", ageNA$Medal)) #1
san <- length(grep("Silver", ageNA$Medal)) #3 
ban <- length(grep("Bronze", ageNA$Medal)) # 6

#Check that all values match up 
length(grep("Gold", weightlifting$Medal)) #217
length(grep("Silver", weightlifting$Medal)) #213
length(grep("Bronze", weightlifting$Medal)) #216

length(grep("", weightlifting$Age))

#Gold
22 + 106 + 66 + 22 + 1 #217
#Silver
25 + 96 + 68 + 21 + 3 #213
#bronze 
18 + 88 + 78 + 26  + 6 #216
#---------------------------------------------------------------
#Graph data based off of Medal 
#create data frame for visual representation. 
nameForCompare <- c(rep(" Age < 21", 3), rep("21 - 26", 3), rep("26 - 31", 3), rep("Above > 31", 3), 
                    rep("NA",3))
allMedalForCompare <- c(blu21, slu21, glu21, blu26, slu26, glu26, blu21 , slu31, glu31, 
                        bab31, sab31, gab31, ban, san, gan)
medalNames <- rep(c("Bronze", "Silver", "Gold"), 5)
medalCompareFrame <- data.frame(nameForCompare, medalNames, allMedalForCompare)
#Generate Bar Graph
compareMedal <- ggplot(medalCompareFrame, aes(x = nameForCompare, fill=medalNames, y=allMedalForCompare)) +
  geom_bar(position="dodge", stat="identity") + ggtitle("Medal Winning Athletes by Age") + 
  xlab("Age Range (Years)") + ylab("Amount of Athletes") + labs(fill="Medal") + scale_fill_brewer(palette = "Paired")
compareMedal + theme(plot.title = element_text(hjust = .5)) 
#--------------------------------------------------------------
#Find out the amount of medal winners under each age range / sex

glu21M <- length(grep("Gold", under21M$Medal)) #18
slu21M <- length(grep("Silver", under21M$Medal)) #18
blu21M <- length(grep("Bronze", under21M$Medal)) #13

glu21F <- length(grep("Gold", under21F$Medal)) #4
slu21F <- length(grep("Silver", under21F$Medal)) #7 
blu21F <- length(grep("Bronze", under21F$Medal)) #5

glu26M <- length(grep("Gold", under26M$Medal)) #82
slu26M <- length(grep("Silver", under26M$Medal)) #74
blu26M <- length(grep("Bronze", under26M$Medal)) #72

glu26F <- length(grep("Gold", under26F$Medal)) #24
slu26F <- length(grep("Silver", under26F$Medal)) #22
blu26F <- length(grep("Bronze", under26F$Medal)) #16

glu31M <- length(grep("Gold", under31M$Medal)) #60
slu31M <- length(grep("Silver", under31M$Medal)) #63
blu31M <- length(grep("Bronze", under31M$Medal)) #67

glu31F <- length(grep("Gold", under31F$Medal)) #6
slu31F <- length(grep("Silver", under31F$Medal)) #5
blu31F <- length(grep("Bronze", under31F$Medal)) #11

gab31M <- length(grep("Gold", above31M$Medal)) #21
sab31M <- length(grep("Silver", above31M$Medal)) #20 
bab31M <- length(grep("Bronze", above31M$Medal)) #23

gab31F <- length(grep("Gold", above31F$Medal)) #1
sab31F <- length(grep("Silver", above31F$Medal)) #1 
bab31F <- length(grep("Bronze", above31F$Medal)) #3

ganM <- length(grep("Gold", ageNAM$Medal)) #1
sanM <- length(grep("Silver", ageNAM$Medal)) #3 
banM <- length(grep("Bronze", ageNAM$Medal)) # 6

ganF <- length(grep("Gold", ageNAF$Medal)) #0
sanF <- length(grep("Silver", ageNAF$Medal)) #0
banF <- length(grep("Bronze", ageNAF$Medal)) #0

MaleLength <- length(grep("M", weightlifting$Sex)) #3474
FemaleLength <- length(grep("F", weightlifting$Sex)) #463

MaleUnder21 <- length(grep("M", under21$Sex)) #302
MaleUnder26 <- length(grep("M", under26$Sex)) #1396
MaleUnder31 <- length(grep("M", under31$Sex)) #1126
MaleAbove31 <- length(grep("M", above31$Sex)) #444

FemaleUnder21 <- length(grep("F", under21$Sex)) #93
FemaleUnder26 <- length(grep("F", under26$Sex)) #222
FemaleUnder31 <- length(grep("F", under31$Sex)) #114
FemaleAbove31 <- length(grep("F", above31$Sex)) #34

#--------------------------------------------------------------

#Note: At this point the athletes that did not have their age displayed will 
#      not be used.

#--------------------------------------------------------------
#percent of athletes compete in each range
(MaleUnder21 / (MaleUnder21 + MaleUnder26 + MaleUnder31 + MaleAbove31)) * 100 #9.241
(MaleUnder26 / (MaleUnder21 + MaleUnder26 + MaleUnder31 + MaleAbove31)) * 100 #42.718
(MaleUnder31 / (MaleUnder21 + MaleUnder26 + MaleUnder31 + MaleAbove31)) * 100 #34.553
(MaleAbove31 / (MaleUnder21 + MaleUnder26 + MaleUnder31 + MaleAbove31)) * 100 #13.586

(FemaleUnder21 / (FemaleUnder21 + FemaleUnder26 + FemaleUnder31 + FemaleAbove31)) * 100 #20.086
(FemaleUnder26 / (FemaleUnder21 + FemaleUnder26 + FemaleUnder31 + FemaleAbove31)) * 100 #47.948
(FemaleUnder31 / (FemaleUnder21 + FemaleUnder26 + FemaleUnder31 + FemaleAbove31)) * 100 #24.622
(FemaleAbove31 / (FemaleUnder21 + FemaleUnder26 + FemaleUnder31 + FemaleAbove31)) * 100 #7.343

#--------------------------------------------------------------
#Find percentages based off of age
#percent of Male lifters per medal 
#Gold 
(glu21M / MaleUnder21) * 100 #5.960 %
(glu21M / MaleLength) * 100 #0.518 %

GPU21M <- (glu21M / (glu21M + glu26M + glu31M + gab31M)) * 100 #9.945 %

(glu26M / MaleUnder26) * 100 #5.874 %
(glu26M / MaleLength) * 100 #2.360 %
GPU26M <- (glu26M / (glu21M + glu26M + glu31M + gab31M)) * 100 #45.304 %

(glu31M / MaleUnder31) * 100 #5.329 %
(glu31M / MaleLength) * 100 #1.727 %
GPU31M <- (glu31M / (glu21M + glu26M + glu31M + gab31M)) * 100 #33.149 %

(gab31M / MaleAbove31) * 100 #4.729 %
(gab31M / MaleLength) * 100 #0.604 %
GPA31M <- (gab31M / (glu21M + glu26M + glu31M + gab31M)) * 100 #11.602 %

#Silver 
(slu21M / MaleUnder21) * 100 #5.960 % 
(slu21M / MaleLength) * 100 #0.518 %
(slu21M / (slu21M + slu26M + slu31M + sab31M)) * 100 #10.286 %

(slu26M / MaleUnder26) * 100 #5.301 %
(slu26M / MaleLength) * 100 #2.130 %
(slu26M / (slu21M + slu26M + slu31M + sab31M)) * 100 #42.286 %

(slu31M / MaleUnder31) * 100 #5.595 %
(slu31M / MaleLength) * 100 #1.813 %
(slu31M / (slu21M + slu26M + slu31M + sab31M)) * 100 #36 %

(sab31M / MaleAbove31) * 100 #4.505 %
(sab31M / MaleLength) * 100 #0.576 %
(sab31M / (slu21M + slu26M + slu31M + sab31M)) * 100 #11.429 %

#Bronze 
(blu21M / MaleUnder21) * 100 #4.305 % 
(blu21M / MaleLength) * 100 #0.374 %
(blu21M / (blu21M + blu26M + blu31M + bab31M)) * 100 #7.429 %

(blu26M / MaleUnder26) * 100 #5.158 %
(blu26M / MaleLength) * 100 #2.073 %
(blu26M / (blu21M + blu26M + blu31M + bab31M)) * 100 #41.143 %

(blu31M / MaleUnder31) * 100 #5.595 %
(blu31M / MaleLength) * 100 #1.923 %
(blu31M / (blu21M + blu26M + blu31M + bab31M)) * 100 #38.286%

(bab31M / MaleAbove31) * 100 #5.180 %
(bab31M / MaleLength) * 100 #0.662 %
(bab31M / (blu21M + blu26M + blu31M + bab31M)) * 100 #13.143 %
#------------------------------------------------------------------
#Find percentages based off of age
#percent of Female lifters per medal 
#Gold 
(glu21F / FemaleUnder21) * 100 #5.960 %
(glu21F / FemaleLength) * 100 #0.518 %
GPU21F <- (glu21F / (glu21F + glu26F + glu31F + gab31F)) * 100 #9.945 %

(glu26F / FemaleUnder26) * 100 #5.874 %
(glu26F / FemaleLength) * 100 #2.360 %
GPU26F <- (glu26F / (glu21F + glu26F + glu31F + gab31F)) * 100 #45.304 %

(glu31F / FemaleUnder31) * 100 #5.329 %
(glu31F / FemaleLength) * 100 #1.727 %
GPU31F <- (glu31F / (glu21F + glu26F + glu31F + gab31F)) * 100 #33.149 %

(gab31F / FemaleAbove31) * 100 #4.729 %
(gab31F / FemaleLength) * 100 #0.604 %
GPA31F <-(gab31F / (glu21F + glu26F + glu31F + gab31F)) * 100 #11.602 %

#Silver 
(slu21F / FemaleUnder21) * 100 #5.960 % 
(slu21F / FemaleLength) * 100 #0.518 %
(slu21F / (slu21F + slu26F + slu31F + sab31F)) * 100 #10.286 %

(slu26F / FemaleUnder26) * 100 #5.301 %
(slu26F / FemaleLength) * 100 #2.130 %
(slu26F / (slu21F + slu26F + slu31F + sab31F)) * 100 #42.286 %

(slu31F / FemaleUnder31) * 100 #5.595 %
(slu31F / FemaleLength) * 100 #1.813 %
(slu31F / (slu21F + slu26F + slu31F + sab31F)) * 100 #36 %

(sab31M / FemaleAbove31) * 100 #4.505 %
(sab31M / FemaleLength) * 100 #0.576 %
(sab31M / (slu21M + slu26M + slu31M + sab31M)) * 100 #11.429 %

#Bronze 
(blu21M / FemaleUnder21) * 100 #4.305 % 
(blu21M / FemaleLength) * 100 #0.374 %
(blu21M / (blu21M + blu26M + blu31M + bab31M)) * 100 #7.429 %

(blu26M / FemaleUnder26) * 100 #5.158 %
(blu26M / FemaleLength) * 100 #2.073 %
(blu26M / (blu21M + blu26M + blu31M + bab31M)) * 100 #41.143 %

(blu31M / FemaleUnder31) * 100 #5.595 %
(blu31M / FemaleLength) * 100 #1.923 %
(blu31M / (blu21M + blu26M + blu31M + bab31M)) * 100 #38.286%

(bab31M / FemaleAbove31) * 100 #5.180 %
(bab31M / FemaleLength) * 100 #0.662 %
(bab31M / (blu21M + blu26M + blu31M + bab31M)) * 100 #13.143 %
#-------------------------------------------------------------------------
#plot percentage data (male)
group <- c("Under 21", "21 - 25", "26 - 30", "Above 31")
percentageByAge <- c(GPU21M, GPU26M, GPU31M, GPA31M)

goldMedalMale <- data.frame(group,percentageByAge)
# Barplot
goldBarRepresentation <- ggplot(goldMedalMale, aes(x="", y=percentageByAge, fill=group)) +
       geom_bar(width = 1, stat = "identity") + ggtitle("Age of Male Gold \n Medal Winning Athletes") + 
       ylab("Representation of Athlete Age (Percentage)") + labs(fill="Age Group (Years)")

#Pie Chart
goldPieRepresentation <- goldBarRepresentation + coord_polar("y", start=0)
goldPieRepresentation + theme(plot.title = element_text(hjust = .5), 
                              axis.title.y=element_blank(),
                              axis.text.y=element_blank(),
                              axis.ticks.y=element_blank())
#------------------------------------------------------------------------
group <- c("Under 21", "21 - 25", "26 - 30", "Above 31")
percentageByAge <- c(GPU21F, GPU26F, GPU31F, GPA31F)

goldMedalMale <- data.frame(group,percentageByAge)
# Barplot
goldBarRepresentation <- ggplot(goldMedalMale, aes(x="", y=percentageByAge, fill=group)) +
  geom_bar(width = 1, stat = "identity") + ggtitle("Age of Female Gold \n Medal Winning Athletes") + 
  ylab("Representation of Athlete Age (Percentage)") + labs(fill="Age Group (Years)")

#Pie Chart
goldPieRepresentation <- goldBarRepresentation + coord_polar("y", start=0)
goldPieRepresentation + theme(plot.title = element_text(hjust = .5), 
                              axis.title.y=element_blank(),
                              axis.text.y=element_blank(),
                              axis.ticks.y=element_blank())
#------------------------------------------------------------------------