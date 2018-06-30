
# Compliance Checks

library(readr)
library(plyr)
Project <- read_csv("C:/Users/aabdul/Dropbox/MIDS/W-241-Alex/Data/Project.csv")
library(data.table)
Project$Duration <- Project$Duration..in.seconds.
Project<-data.table(Project)
#a) i/0Breezing through the questions
count(Project[Duration< 60 & (sid=='p2' | sid=='p3'| sid=='p4'),X1])
# We have 18 people who took less than 60 seconds duration to complete the task. Of these 14 people did not complete the task. Of the remaining 4 who did complete the 
#task, 2 attempts were from the same person(had the same IP address)
slackers<-Project[Duration< 60 & (sid=='p2' | sid=='p3' | sid=='p4'),]

#ii) Instances of same survey takers attempting the survey mutiple times.
# We have 3 instances where the same people attempted the survey couple of times
counts<-data.table(count(Project[(sid=='p2' | sid=='p3'| sid=='p4'),IPAddress]))
counts[freq>1,]
#   IPAdress       freq
#1: 173.17.73.117    2
#2: 174.124.1.197    2
#3:  75.118.6.234    2
#4: 76.119.102.50    2
#5:  98.94.17.127    2

#b)Attrition - We had 30 people who started the survey but did not finish them. We dont have reason to suspect differential attrition. The attrition happened in a random way between the different treatment groups.
count(Project[Finished=='False',X1])
