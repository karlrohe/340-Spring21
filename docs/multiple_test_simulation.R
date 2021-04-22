#  multiple testing
# Let's do a simulation.
library(magrittr)


n = 1000
p = 20

# Suppose you have these features:
X = matrix(rnorm(n*p), nrow = n)
dim(X)

# Suppose there is a binary outcome, 
#  which only depends on ONE of the feature.
# but this game is a bit realistic because
#   we don't yet get to see which feature actually matters.

the_real_feature = sample(1:p,1)  
# logistic probability with beta = 1
p = exp(.1*X[,the_real_feature])/(1+exp(.1*X[,the_real_feature]))
y = rbinom(n,1,p)

fit = glm(y~X, family = binomial)
# summary(fit)

# here is a histogram of the beta-hats:
summary(fit)$coefficients[,1] %>% hist



# here is the beta-hat of the real feature:
summary(fit)$coefficients[the_real_feature+1,1]
# here is it's p-value:
p_value_for_the_good_one = summary(fit)$coefficients[the_real_feature+1,4]

p_values = summary(fit)$coefficients[,4]

min(p_values)  #here is the smallest p-value
p_value_for_the_good_one  #here is the good one. 

