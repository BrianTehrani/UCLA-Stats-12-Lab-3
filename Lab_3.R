#### Exercise 1 ####
#setwd("E:/Documents/School/UCLA/Stats 12/Week 5")
soil <- read.table("http://www.stat.ucla.edu/~nchristo/statistics_c173_c273/soil_complete.txt",
                   header=TRUE)
#a
Lead_Soil_Reg <- lm(soil$lead ~ soil$zinc)
summary(Lead_Soil_Reg)

#b
plot(soil$lead ~ soil$zinc, xlab="Zinc Conc.", ylab="Lead Conc.",
     main = "Regression of Zinc Conc. on Lead Conc. in ppm")
abline(Lead_Soil_Reg, col = "red", lwd= 2)

#c
plot(Lead_Soil_Reg$residuals ~ soil$zinc, main = "Residuals Plot",
     ylab = "Residuals", xlab = "Zinc Soil Conc.")
abline(a=0, b=0, col = "red", lwd = 2)

#d
# y =a + bx where a = 17.367688, b = 0.289523

#e
# Using the regression line from d): if Zinc = 1000 ppm, Pb = 306.890688 ppm

#f
# We would heve to use the slope: 100 * 0.289523 = 28.9523
# Therefore lead conc. at A would be 28.9528 times higher than B

#g
# The R-squared value is 0.9114. This value means that 91.14% of the variation 
# in Lead conc. is explained by the Zinc conc.

#h


#### Exercise 2 ####

ice <- read.csv(file.choose()) 
#<- read.csv("sea_ice.csv", header = T)
ice$Date <- as.Date(ice$Date, "%m/%d/%Y")

#a
SeaExt_Time_Reg = lm(ice$Extent ~ ice$Date)
summary(SeaExt_Time_Reg)

#b
plot(ice$Date, ice$Extent, xlab = "Time [yr]", ylab = "Sea Ice Extent", 
     main = "Regression of Sea Ice Extent against Time", type = "l")
abline(SeaExt_Time_Reg, col = "Blue", lwd = 2)
# Is there a trend

#c
plot(SeaExt_Time_Reg$residuals ~ ice$Date, main = "Residuals Plot",
     ylab = "Residuals", xlab = "Time [yr]")
abline(a=0, b=0, col = "Blue", lwd = 2 )
# Ans question

#### Exercise 3 ####

#a
  # Chance that Adam doubles money in the first round: 2/9 => 22.2%
  # Chance that Adam loses all money in the first round: 1/9 => 11.1%

#b
set.seed(123)
die = 1:6

library(mosaic)

  # simulated 2 dice rolls
outcome_die <- do(5000)*sample(die,2,replace = TRUE)
sum_outcome_dice <- rowSums(outcome_die)
  # List first 5 results in lab report

#c
hist(sum_outcome_dice, main = "Histogram of 5000 rolls in Craps",
     xlab = "Sum of die rolls", ylim = c(0, 1000))

#d
occurrence_7  = sum(sum_outcome_dice == 7)
occurrence_11 = sum(sum_outcome_dice == 11)
  # Adam doubled his money
Adam_doubled <- (occurrence_7 + occurrence_11)/length(sum_outcome_dice) * 100

occurrence_2  = sum(sum_outcome_dice == 2)
occurrence_3  = sum(sum_outcome_dice == 3)
occurrence_12 = sum(sum_outcome_dice ==12)
  # Adam lost all his money
Adam_lost <- (occurrence_2 + occurrence_3 + occurrence_12)/length(sum_outcome_dice) * 100

#e
  # Adam winning is independent and not disjoint as there are 12 independent 
  # die roll outcomes for the event with winning being either a 7 or 11.
  # Adam loosing all his money is also independent and not disjoint as there
  # are multiple outcomes in 2,3,or 12

#f 
  # already did in notebook

#### Exercise 4 ####

#a
  # n = 365
  # p = 0.40

#b
  # mean = n*p
  # sd = sqrt(n*p*(1-p))

#c
dbinom(145, size = 356, prob = 0.4)

#d
pbinom(175, size = 356, prob = 0.4) - pbinom(125, size = 356, prob = 0.4) + dbinom(175, size = 356, prob = 0.4)

#e
1 - pnorm(230, mean = 200, sd = 20)
