# data cleaning with R
install.packages("tidyverse")
library(tidyverse)
view(starwars)# viewing dataset starwars

#checking what type of variable types 
#present in starwars 

glimpse(starwars)#glimpse function
#returns summary of data
# dbl variable type refers to 
# fraction b/w integers.

class(starwars$gender)
unique(starwars$gender)

###############################################################################
#converting character variable of gender
# into categorical variable using
# factor.
#warning levels can't decide for character variable
#so have to convert into factors

starwars$gender_f <-as.factor(starwars$gender)
class(starwars$gender_f)
levels(starwars$gender_f)

#changing levels under a variable 

starwars$gender_f<-factor((starwars$gender),levels = c("masculine","feminine"))
levels(starwars$gender_f)

#selecting variable using tidyverse library
glimpse(starwars)
starwars %>% 
  select(name,height,ends_with("color")) %>%
  names()

#filtering values in variables 
#for particular values

starwars %>% 
  select(name,height,ends_with("color")) %>% 
  filter(hair_color %in% c("blond","brown") & 
  height < 180)

##########################################################################
#missing data handling

view(starwars$height)
mean(starwars$height)# giving NA bcause of missing data in variable
#height variable consist missing values named NA which is not countable
#to mean function
#so lets remove them
mean(starwars$height ,na.rm =TRUE)

#removing missing values from multiple variables

starwars %>% 
  select(name,gender,hair_color,height)

#removing missing values from all variables
starwars %>% 
  select(name,gender,hair_color,height) %>% 
  na.omit()#this will remove all missing values

#filtering missing values
starwars %>% 
  select(name,gender,height,hair_color) %>% 
  filter(!complete.cases(.))#!=not equal to

#removing missing value from single variable
starwars %>% 
  select(name,gender,height,hair_color) %>% 
  drop_na(height) %>% 
  view()#now height variable dosesn't have missing ones.

#changing values of NA to "none"
starwars %>% 
  select(name ,gender,height,hair_color) %>% 
  filter(!complete.cases(.)) %>% 
  mutate(hair_color2=replace_na(hair_color,"none"))
  view()
  
##############################################################################
#removing duplicates
    
  name<-c("andrew","akshay","bella","rosy","andrew")
  age<-c(12,13,14,15,12)
  
friends<-data.frame(name,age)
view(friends)

#identify duplicates in friend data
duplicated(friends) 

#[1] FALSE FALSE FALSE FALSE  TRUE 5 observation indicate logical vector to true
#that mean 5 observation is duplicate of an earlier observation

friends[duplicated(friends),]  #subsetting duplicate observation{base}
friends[!duplicated(friends),]  #subsetting not duplicates observations{base}

friends %>% distinct() %>% 
  view() #throw no duplicates via {tidyverse}system
      
################################################################################

#recoding variable into newone or previous ones

starwars %>% 
  select(name,gender) %>%
  mutate(gender_n=recode(gender,"masculine"=1,"feminine"=2)) %>% 
  filter(complete.cases(.)) %>% 
  view()





