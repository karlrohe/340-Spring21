# enter the data:
odd = 12
even = 14

estimatedOddBirthdayPercentage = odd/(even+odd)
#simulation:
oddSims = rbinom(10^6,size = 26, prob = .5)
hist(oddSims)
lines(c(odd,odd), c(0,10^9), lwd = 4, col = "red")
# this is the probability of observing 20 (or greater) under the null model.  This is not the probability that our model is wrong.  given that .004 is so small, I would reject the null model.




observedValue = 8
X = rbinom(10^6, 16, prob = 17/25)
Y = rbinom(10^6, 9, prob = 17/25)
simulatedValue = X - Y
hist(simulatedValue)
lines(c(observedValue,observedValue), c(0,10^9), lwd = 4, col = "red")
mean(simulatedValue > observedValue)


observedValue = 8/16 - 0/9
X = rbinom(10^6, 16, prob = 17/25)
Y = rbinom(10^6, 9, prob = 17/25)
simulatedValue = X/16 - Y/9
hist(simulatedValue)
lines(c(observedValue,observedValue), c(0,10^9), lwd = 4, col = "red")
mean(simulatedValue > observedValue)

# chi^2

observedValue = (8 - 16*17/25)^2 / (16*17/25) + 
  (8 - 16*8/25)^2 / (16*8/25) + 
  (0 - 9*17/25)^2 / (9*17/25) + 
  (9 - 9*8/25)^2 / (9*8/25)

X = rbinom(10^7, 16, prob = 17/25)
Y = rbinom(10^7, 9, prob = 17/25)
simulatedValue = (X - 16*17/25)^2 / (16*17/25) + 
  ((16-X) - 16*8/25)^2 / (16*8/25) + 
  (Y - 9*17/25)^2 / (9*17/25) + 
  ((9-Y) - 9*8/25)^2 / (9*8/25)
hist(simulatedValue)
lines(c(observedValue,observedValue), c(0,10^9), lwd = 4, col = "red")
sum(simulatedValue > observedValue)



