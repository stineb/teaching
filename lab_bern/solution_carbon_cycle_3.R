# --------------------------------------------------
# Carbon Cycle Exercises.
# Computer Lab.
# Author: Benjamin Stocker <beni@climate.unibe.ch>
# Responsible: Fortunat Joos <joos@climate.unibe.ch>
# March 2012
# --------------------------------------------------


# Exercise 1
# --------------------------------------------------

# Initialize variable 'sum' with zero.
sum <- 0

# Loop with counter 'i' running from 1 to 100 using 
# intrinsic R function 'seq()'. Enter help(seq) for
# a documentation. 
for (i in seq(100)) {
	sum <- sum + i
}
	
# Print value of 'sum' to screen.
print(paste('Ex. 1.a) The sum is ',sum))


# Exercise 2
# --------------------------------------------------

# Do the same as in 1.a) and add a conditional
# statement. Use the intrinsic modulo function 'x %% y' 
# (returns the remainder of the division x/y) and the 
# logical operators '&&' (logical AND) and 'x == y' 
# (returns 'TRUE' if the values of x and y are equal).
sum <- 0
for (i in seq(100)) {
	if (i %% 3 == 0 && i %% 7 == 0 ) {
		sum <- sum + i
	}	
}	

# Print value of 'sum' to screen.
print(paste('Ex. 1.b) The sum is ',sum))

	
# Exercise 3
# --------------------------------------------------

# Define a vector of length 100, initialized with NA
# (not available = not defined).
vec <- array(NA,c(100))

# Fill up first 25 elements of 'vec' with value 6. 
vec[1:25] <- 6

# Fill up elements 66:100 with value -20.
vec[66:100] <- -20

# Fill up misssing values (NA) by linear interpolation
# using the following algorithm:

# A. 
# Search last element defined before gap and store its position ('last').
# (Of course, we already know the value of 'last' (25). But we're
# here to learn something...)
last <- 1
defined <- TRUE
while (defined){
	if (is.na(vec[last+1])){
		defined <- FALSE
	} else {
		last <- last+1	
	}
}

# One may omit the boolean variable 'defined' and directly write:
last <- 1
while (!is.na(vec[last+1])){
	last <- last + 1
}
	

# B. 
# Search first element defined after gap and store its position ('first').
first <- last + 1
while (is.na(vec[first])){
	first <- first + 1	
}

# C. 
# Loop through missing elements and linearly interpolate to its position.
a <- vec[last]
b <- (vec[first] - vec[last]) / (first - last)
for (i in (last+1):(first-1)){
	vec[i] <- a + b * (i-last)
}

# Plot to check if interpolation is correct.
# Intrinsic function 'plot' requires the following arguments:
# 1st argument: vecor of x-values
# 2nd argument: vecor of y-values (must be of same length as x-value vector)
# more arguments: type help(plot)
plot(seq(length(vec)),vec,type='l')


# Exercise 4
# --------------------------------------------------

# Load data and store as 'table'.
table <- read.table("../co2_monthly_maunaloa.txt",skip=70,header=TRUE,na.strings='-99.99')

# Quick check, if table is read in correctly
head(table)

# Plot the data, with x-axis and y-axis labels. The explicit definition of the axes' ranges
# is required when additional curves are plotted into the same frame (see Ex.5). 
plot(
     table$decyr,
     table$co2_avg,
     type='l',
     xlab="year AD",
     ylab="atmospheric CO2 [ppm]",
     xlim=c(table$decyr[1],table$decyr[length(table[,1])]),
     ylim=c(min(na.exclude(table$co2_avg)),max(na.exclude(table$co2_avg)))
     )


# Exercise 5
# --------------------------------------------------

# Define a function to compute the 12-month running mean.
rmean <- function(vec,boxl){
	rvec <- vec
	for (i in seq(length(vec))){
		startbox <- i-boxl/2
		endbox <- i+boxl/2
		if (startbox < 1) {
			rvec[i] <- NA	
		} else if (endbox > length(vec)) {
			rvec[i] <- NA
		} else {
			rvec[i] <- mean(vec[startbox:endbox])
		}	
	}
	return(rvec)
}

runningmean <- rmean(table$co2_avg,12)

# Plot the running mean into the same frame. The 'xlim' and 'ylim' arguments
# must be the same as in the first plot, otherwise R will not interpret the
# axis range the same way.
par(new=TRUE)
plot(
     table$decyr,
     runningmean,
     type='l',
     col='red',
     xlab="",
     ylab="",
     xlim=c(table$decyr[1],table$decyr[length(table[,1])]),
     ylim=c(min(na.exclude(table$co2_avg)),max(na.exclude(table$co2_avg)))
     )

