rm(list=ls()) #remove previously stored variables
library("Hmisc")# import Hmisc library
covid <- read.csv("C:/Users/admin/Downloads/noval_corona virus/COVID19_line_list_data.csv")

# cleaning up death column
covid$death_dummy<-as.integer(covid$death !=0)

#death rate 
sum(covid$death_dummy)/nrow(covid)

#age 
#claim: people are die who are older
dead= subset(covid,death_dummy==1)
alive= subset(covid,death_dummy=0)

#mean by dead/alive*age
mean(dead$age,na.rm=TRUE)
mean(alive$age,na.rm=TRUE)

# is our questioning is statistically proven or not.
t.test(alive$age,dead$age,alternative ="two.sided",conf.level = 0.99)

#normally, if p-value is less than 0.05 we reject null hypothesis,
#here p-value~0 ,so we reject null hypothesis, and conclude that 
#this is statistically significant.

#gender 
#claim: gender has no effect
men= subset(covid,gender=="male")
woman= subset(covid,gender=="female")

#mean by dead/alive*age
mean(men$death_dummy,na.rm=TRUE)
mean(woman$death_dummy,na.rm=TRUE)

# is our questioning is statistically proven or not.
t.test(men$death_dummy,woman$death_dummy,alternative ="two.sided",conf.level = 0.99)
# 99 percent confidence interval: men have from 8% to 8.8% higher chance
#of dying.
# p-value =0.002 < 0.05% ,so this is statistically significant reject 
# null hypothesis.
