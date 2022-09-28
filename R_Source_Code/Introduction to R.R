#Set working directory
setwd("D:/GEO-STATISTICS COURSE/2022-23/Practical with R")


numeric_vector <- c(1, 10, 49)
character_vector <- c("a", "b", "c")

# Complete the code for boolean_vector
boolean_vector <- c(TRUE, FALSE, TRUE)

# Poker winnings from Monday to Friday
poker_vector <- c(140, -50, 20, -120, 240)

# Roulette winnings from Monday to Friday
roulette_vector <- c(-24, -50, 100, -350, 10)

A_vector <- c(1, 2, 3)
B_vector <- c(4, 5, 6)

# Take the sum of A_vector and B_vector
total_vector <- A_vector + B_vector

# Print out total_vector
print (total_vector)

help(mean)


no = c(1:25)


sum_no = function(x, y){
  x+y
}

sum_no(2, 5)

# Construct a matrix with 3 rows that contain the numbers 1 up to 9
matrix(1:9, byrow=TRUE, nrow=3)

# Box office Star Wars (in millions!)
new_hope <- c(460.998, 314.4)
empire_strikes <- c(290.475, 247.900)
return_jedi <- c(309.306, 165.8)

# Create box_office
box_office <- c(new_hope, empire_strikes, return_jedi)

# Construct star_wars_matrix
star_wars_matrix <- matrix(box_office, byrow=TRUE, nrow=3)
print(star_wars_matrix)


# Total revenue for US and non-US
total_revenue_vector <- colSums(star_wars_matrix)
# Print out total_revenue_vector
print(total_revenue_vector)


# Definition of vectors
name <- c("Mercury", "Venus", "Earth", 
          "Mars", "Jupiter", "Saturn", 
          "Uranus", "Neptune")
type <- c("Terrestrial planet", 
          "Terrestrial planet", 
          "Terrestrial planet", 
          "Terrestrial planet", "Gas giant", 
          "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 
              11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03, 
              0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

# Create a data frame from the vectors
planets_df <- data.frame(name, type, diameter,rotation, rings)
print(planets_df)
str(planets_df)


str(planets_df)

planets_df
class(planets_df)

# Print out diameter of Mercury (row 1, column 3)
mer_dia = planets_df[1,3]

# Print out data for Mars (entire fourth row)
planets_df[4,]

planets_df[3, ]


# Vector with numerics from 1 up to 10
my_vector <- 1:10 
my_vector
class(my_vector)

# Matrix with numerics from 1 up to 9
my_matrix <- matrix(1:9, ncol = 3, nrow = 5)  # byrow = FALSE (default)
my_matrix
class(my_matrix)

# First 10 elements of the built-in data frame mtcars


data_dia = planets_df[ ,3]
avg_dia = mean(data_dia)
AVG_DIA = mean(planets_df[,3])


rings = planets_df$rings
rings
planets_df[,1][rings]
