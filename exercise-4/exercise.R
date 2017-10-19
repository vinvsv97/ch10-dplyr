# Exercise 4: DPLYR and flights data

# Install the nycflights13 package and read it in.  Require the dplyr package
install.packages("nycflights13")
install.packages("dplyr")
library(nycflights13)
library(dplyr)

# The data.frame flights should now be accessible to you.  View it, 
# and get some basic information about the number of rows/columns
View(flights)

# Add a column that is the amount of time gained in the air (`arr_delay` - `dep_delay`)
flights <- mutate(flights, amt.time.gained = arr_delay - dep_delay)

# Sort your data.frame desceding by the column you just created
flights <- arrange(flights, -amt.time.gained)

# Try doing the last 2 steps in a single operation using the pipe operator
flights <- flights %>% mutate(amt.time.gained = arr_delay - dep_delay) %>% arrange(-amt.time.gained)

# Make a histogram of the amount of gain using the `hist` command
hist(flights$amt.time.gained)

# On average, did flights gain or lose time?
average <- summarise(flights, mean = mean(amt.time.gained, na.rm = TRUE))

# Create a data.frame that is of flights headed to seatac ('SEA'), 
seattle <- data.frame(filter(flights, dest == 'SEA'))
# On average, did flights to seatac gain or loose time?
average.seattle <- summarise(seattle, mean = mean(amt.time.gained, na.rm = TRUE))
### Bonus ###
# Write a function that allows you to specify an origin, a destination, and a column of interest
# that returns a data.frame of flights from the origin to the destination and only the column of interest
## Hint: see chapter 11 section on standard evaluation
selector <- function(or,des,interest){
  return(data.frame(flights %>% filter(origin == or & dest == des) %>% select(interest)))
}

# Retireve the air_time column for flights from JFK to SEA
lol <- selector('JFK','SEA','air_time')
View(lol)
# What was the average air time of those flights (in hours)?  
avg <- mean(lol$air_time, na.rm = TRUE) /60

# What was the min/max average air time for the JFK to SEA flights?
