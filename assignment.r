# a3-using-data

# Before you get started, set your working directory using the Session menu

###################### Data Frame Manipulation (24 POINTS) ####################

# A vector `students` holding 1,000 values representing students
students <- paste("Student", 1:1000)
print(students)

# A vector that holds 1000 random values in it (these represent grades in a 
# math course). These values are normally distributed with a mean of 88 and 
# a standard deviation of 10
math_grades <- round(rnorm(1000, mean = 88, sd = 10), digits = 0)

# Replaces any values in the `math_grades vector` that are above 100 with
# the number 100
math_grades[math_grades > 100] <- 100

# A vector that holds 1000 random values in it (these represent grades in a 
# spanish course). These values are normally distributed with a mean of 85 and 
# a standard deviation of 12
spanish_grades <- round(rnorm(1000, mean = 85, sd = 12), digits = 0)

# Replaces any values in the `spanish_grades` that are above 100 with
# the number 100
spanish_grades[spanish_grades > 100] <- 100

# Creates a data.frame variable `grades` by combining
# the vectors `students`, `math_grades`, and `spanish_grades`
grades <- data.frame(students, math_grades, spanish_grades, stringsAsFactors = 
                       FALSE)

# A variable that contains the number of rows in dataframe `grades`
num_students <- nrow(grades)

# A variable that contains the number of columns in dataframe `grades` minus 
# one (b/c of their names)
num_courses <- ncol(grades) - 1

# Add a new column `grade_diff` to the dataframe, `grades`, which is equal to 
# `grades$math_grades` minus `grades$spanish_grades`
grades$grade_diff <- grades$math_grades - grades$spanish_grades

# Add another column `better_at_math` as a boolean (TRUE/FALSE) variable that
# indicates that a student got a better grade in math
grades$better_at_math <- math_grades > spanish_grades

# A variable that is the number of students better at math
num_better_at_math <- nrow(grades[grades$better_at_math == TRUE,])

# Writes `grades` dataframe to a new .csv file inside the data/ directory
# with the filename `grades.csv`.
write.csv(grades, file = "data/grades.csv", row.names = FALSE)



########################### Built in R Data (28 points) #######################

# `is.data.frame()` function to test if it is a table.
is.data.frame(Titanic)

# A variable created by converting `Titanic` into a data frame
titanic_df <- data.frame(Titanic, stringsAsFactors = FALSE)

# It's important to understand the _meaning_ of each column before analyzing it
# NOTE: Seek to fully understand the structure of the data before
# proceeding (Hint: How is the variable, Freq, related to the other variables?)
# Using comments below, describe what information is stored in each column
# For categorical variables, list all possible values
# Class: 1st, 2nd, 3rd, Crew
# Sex: Male, Female
# Age: Child, Adult
# Survived: Yes, No
# Freq: 0-670


# A variable that are the *only* the rows of the data frame
# with information about the number children on the Titanic.
children <- titanic_df[titanic_df$Age == 'Child',]

# A variable `num_children` that is the total number of children.
num_children <- sum(children$Freq)

# A variable `most_lost` which has the *row* with the
# largest absolute number of losses (people who did not survive).
non_survivors <- titanic_df[titanic_df$Survived == 'No',]
most_lost <- non_survivors[non_survivors$Freq == max(non_survivors$Freq),]


# A function that takes in two arguments:
# - a ticket class (e.g., "1st", "2nd"), and
# - the dataframe itself (it's good practice to explicitly pass in data frames)
# This function should return the following
# sentence that states the *survival rate* (# survived / # in group)
# of adult men and "women and children" in that ticketing class.
# It should read (for example):
# Of Crew class, 87% of women and children survived and 22% of men survived.

survival_rate <- function(ticket_class, dataframe) {
  
  # Separate dataframe into survivors and nonsurvivors
  non_survivors <- dataframe[dataframe$Survived == 'No',]
  survivors <- dataframe[dataframe$Survived == 'Yes',]
  
  # Creates dataframes based on same class
  class_nonsurvivors <- non_survivors[non_survivors$Class == ticket_class,]
  class_survivors <- survivors[survivors$Class == ticket_class,]
  
  # Number of adult male survivors
  male_adult_survivors <- class_survivors[3, "Freq"]
  # Number of adult male nonsurvivors
  male_adult_nonsurvivors <- class_nonsurvivors[3, "Freq"]
  # Percentage of adult male survival rate
  male_percentage <- paste(round(100 * (male_adult_survivors / 
                                 (male_adult_survivors + 
                                  male_adult_nonsurvivors)), 0), 
                           "%", sep = "")
  
  # Number of other survivors
  other_survivors <- (class_survivors[1, "Freq"] + 
                        class_survivors[2, "Freq"] +
                        class_survivors[4, "Freq"])
  # Number of other nonsurvivors
  other_nonsurvivors <- (class_nonsurvivors[1, "Freq"] + 
                          class_nonsurvivors[2, "Freq"] +
                          class_nonsurvivors[4, "Freq"])
  # Percentage of other survival rate
  other_percentage <- paste(round(100 * (other_survivors / 
                                 (other_survivors + 
                                  other_nonsurvivors)), 0),
                            "%", sep = "")
  
  # A statement describing the survival rate of a class
  answer <- paste("Of", ticket_class, "class,", other_percentage, 
                  "of women and children survived and", male_percentage, 
                  "of men survived.")
  return(answer)
}


# Creates variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class and the `titanic_df` data frame
# to the `survival_rate` function
# (`Crew`, `1st`, `2nd`, and `3rd`)
first_survived <- survival_rate("1st", titanic_df)
second_survived <- survival_rate("2nd", titanic_df)
third_survived <- survival_rate("3rd", titanic_df)
crew_survived <- survival_rate("Crew", titanic_df)


# What notable differences do you observe in the survival rates across classes?
#
# For 1st class, I noticed that the survival rate for women and children is
# the highest, with a 97% survival rate in that class. Also, the lowest 
# survival rate for adult men is 2nd class, with 8%.

# What notable differences do you observe in the survival rates between the
# women and children versus the men in each group?
#
# The survival rates for women and children are higher in all classes. Also,
# while the survival rate generally decreases as the class gets lower for
# women and children, the survival rate for men varies across.



########################### Reading in Data (43 points)########################

# Reads the life_expectancy_years.csv file into a variable called `life_exp`.
life_exp <- read.csv("Data/life_expectancy_years.csv", 
                     stringsAsFactors = FALSE, check.names = FALSE)


# A function that takes in a column name and a data frame and returns the mean
# of that column.
get_col_mean <- function (col_name, dataframe) {
  year <- dataframe[, col_name]
  mean <- round(mean(year, na.rm = TRUE), 1)
  return(mean)
}

# Create a list `col_means` that has the mean value of each column in the
# data frame (except the `Country` column). You should use your function above.


# A variable that holds the difference in average country life expectancy
# between 1800 and 2018
avg_diff <- get_col_mean("2018", life_exp) - get_col_mean("1800", life_exp)

# Creates a column that is the change in life expectancy from 2000 to 2018.
life_exp$change <- life_exp$`2018` - life_exp$`2000`

# A variable that is the *name* of the country with the largest gain in life
# expectancy
most_improved <- max(life_exp$change, na.rm = TRUE)

# A variable that has the *number* of countries whose life expectance has 
# improved less than 1 year between 2000 and 2018
less_one <- life_exp[(life_exp$change < 1.0), ]
# Use complete.cases to ignore the NA rows
num_small_gain <- nrow(less_one[complete.cases(less_one), ])


# A function that takes in a country's name, two (numeric) years, and a
# dataframe as parameters.
# It returns the phrase:
# "Between YEAR1 and YEAR2, the life expectancy in COUNTRY went DIRECTION by
# SOME_YEARS years".

country_change <- function(country, year1, year2, dataframe) {
  # Creates separate dataframe for one country.
  country_df <- dataframe[dataframe$country == country,]
  
  # Calculates the difference between year one and year two
  change <- round(country_df[, year2] - country_df[, year1], 1)
  
  # Determines whether the years increased or decreased.
  if(change > 0) {
    direction = "up"
  } else {
    direction = "down"
  }
  answer <- paste("Between ", year1, " and ", year2, 
                  ", the life expectancy in ", country, " went ", direction, 
                  " by ", change, " years.", sep = "")
  return(answer)
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 1990 in Sweden
sweden_change <- country_change("Sweden", "1960", "1990", life_exp)


# Write a function `compare_change()` that takes in two country names and your
# `life_exp` data frame as parameters, and returns a sentence that describes
# their change in life expectancy from 2000 to 2018 (the `change` column)
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=6.9),
#  whose life expectancy grew by 0.6 years more than Bolivia's (gain=6.3)."
# Make sure to round your numbers to one digit (though only after calculations!)

compare_change <- function(country1, country2, dataframe) {
  # Creates separate dataframe for each country.
  country1_df <- dataframe[dataframe$country == country1,]
  country2_df <- dataframe[dataframe$country == country2,]
  
  # Stores value of change in years
  country1_exp <- round(country1_df[, "change"], 1)
  country2_exp <- round(country2_df[, "change"], 1)
  comparison <- round(abs(country1_exp - country2_exp), 1)
  
  if(country1_exp > country2_exp){
    answer <- paste("The country with the bigger change in life expectancy was ",
          country1, " (gain=", country1_exp, 
          "), whose life expectancy grew by ", comparison, " years more than ",
          country2, "'s (gain=", country2_exp, ").", sep="")
  } else {
    answer <- paste("The country with the bigger change in life expectancy was ",
          country2, " (gain=", country2_exp, 
          "), whose life expectancy grew by ", comparison, " years more than ",
          country1, "'s (gain=", country1_exp, ").", sep="")
  }
  
  return(answer)
}

# A variable that describes who had a larger gain in life expectancy 
# (the U.S. or France)
usa_or_france <- compare_change("United States", "France", life_exp)


# Writes the `life_exp` data.frame to a new .csv file to the
# data/ directory with the filename `life_exp_with_change.csv`.
write.csv(life_exp, "Data/life_exp_with_change.csv", row.names = FALSE)

