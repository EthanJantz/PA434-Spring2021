### --------
### setup
###

library(randomNames)

### ---------
### Problem 1
###

### Sampling
sample(x = 5) # 3 2 4 5 1
# Pulls an exhaustive set of samples from c(1:5)
# "from a hat until it's empty" sampling

sample(x = 5, size = 2) # 2 4
# Pulls 2 samples from c(1:5)

sample(x = 1:100, size = 4) # 88 97 75 63
# Pulls 4 samples from c(1:100)

sample(x = 1:10, size = 6) # 5 10  6  9  8  7
# Pulls 6 samples from c(1:10)

# sample(x = 1:5, size = 6, replace = F) # Doesn't run, size is too large for x

sample(x = 1:5, size = 6, replace = T) # 1 2 1 1 1 4
# Resolves previous problem by allowing replacements of samples
# pulled from x

### Seeding random ints
set.seed(23)
sample(5) # out: 5 4 3 2 1

set.seed(34)
sample(5) # out: 5 1 4 2 3

set.seed(23)
sample(5) # out: 5 4 3 2 1

# This is useful for reprex on stackExchange 
# or especially github issues where it's required
# to have reprex to get help

### randomNames package test
names = randomNames(4)# [1] "Howe, Tyler"        "Lynn, Otoniel"      "Brinson, Gabriella" "el-Ben, Mahmooda" 
# Very nice little package!

class(names) # Character vector object

### --------------
### Problems 2 - 9
### 

## Problem 2
set.seed(434)
days_unemployed <- sample(x = 0:255, size = 40, replace = T)

## Problem 3
set.seed(434)
applications_submitted <- sample(x = 1:25, size = 40, replace = T)

## Problem 4
set.seed(434)
gender <- sample(x = 0:1, size = 40, replace = T)

## Problem 5
set.seed(434)
# Set name gender using the gender vector
name <- randomNames(40, gender = gender)

## Problem 6
# Bind the columns together into a data frame
data <- as.data.frame(cbind(name, gender, days_unemployed, applications_submitted))

## Problem 7
dim(data) # 40 obs, 4 vars

## Problem 8
class(data$name) # Character
class(data$gender) # Character, this should probably be an int
data$gender <- as.integer(data$gender)
class(data$gender) # Integer, fixed!
class(data$days_unemployed) # same deal as gender

## Note:
# This is because cbind coerces the vectors into a matrix 
# before it's parsed through as.data.frame().
# Since a matrix class object cannot have a variety of 
# variable classes, it coerces each into the class
# of the first variable. Or at least that's my guess.

data$days_unemployed <- as.integer(data$days_unemployed)
data$applications_submitted <- as.integer(data$applications_submitted)
class(data$days_unemployed) # Integer
class(data$applications_submitted) # Integer

## Problem 9
# avg number of days unemployed by applicant
mean(data$days_unemployed) # 122

# sd of days unemployed
sd(data$days_unemployed) # ~71

# min/max count of applications submitted
c(min(data$applications_submitted), max(data$applications_submitted)) # 1, 25

# avg number of applications submitted
mean(data$applications_submitted) # ~13

# count of males/females
table(data$gender) # 18 males, 22 females

# avg apps submitted by female applicants
mean(subset(data, gender == 1)$applications_submitted) # ~13

## Note: 
# This works because subset(data, gender == 1) returns a data.frame object, so I can use the $ notation on it instead of storing it as an object first.

# Max days unemployed by each gender
max(subset(data, gender == 1)$days_unemployed) # 253 for female applicants

max(subset(data, gender == 0)$days_unemployed) # 246 for male applicants

### -----------
### Problem 10
###

# The average person spends 4 months unemployed, and in that time they submit an average of 13 job applications. That equates to less than one application per day.
# Given the data, the problem could be one of (or both) two things. 
# 1.) Applicants are facing some issue keeping them from being able to submit applications. This could be due to motivation, lack of access to technology, etc.
# 2.) There are not enough jobs to apply to in their area.
# For problem 1, we can implement policies guiding people towards assistance in job applications. This could be through library programs such as the Chicago Cybernavigator program or through assistance at community centers specifically focused on jobs. 
# For problem 2, we can focus our efforts on finding remote work opportunities for applicants through partnership or direct service.