###################
#### Pre- SESSION 10 - LOGISTIC REGRESSION
###################

install.packages("mlbench")
library(mlbench)

#call datasets inside the package
data("PimaIndiansDiabetes")

## Massaging data
# Subtracting 1 from the "diabetes" that has level 1-2, in order to have 0-1
PimaIndiansDiabetes$binary <- as.numeric(PimaIndiansDiabetes$diabetes)-1

#Splitting TESTING - TRAINING
# getiing a random sample of 80% of the data for the training
train_index <- sample(1:nrow(PimaIndiansDiabetes),
                      size=0.8*nrow(PimaIndiansDiabetes))
train_data <- PimaIndiansDiabetes[train_index, ]

#getting the remaining data (20%) for the testing 
test_data <- PimaIndiansDiabetes[-train_index, ]

# LOGISTIC REGRESSION
# predicts binary based on age, mass, pregnant and triceps, adding "binary" because it's onluy 1-0
# to do the ~, press option + 5
# then save it into an object to analyze it
my_logit <- glm(binary ~ age+mass+pregnant+triceps, data=train_data, family="binomial")

summary(my_logit)

#interpretation of coefficients
#age:
exp(0.028223)-1
#mass
exp(0.102569)-1

# PREDICT NEW DATA 
predict(my_logit, test_data, type="response")
 

# RESCALING VARIABLES
PimaIndiansDiabetes$res_age <- (PimaIndiansDiabetes$age - min(PimaIndiansDiabetes$age))/
                                (max(PimaIndiansDiabetes$age) - min(PimaIndiansDiabetes$age))

PimaIndiansDiabetes$res_mass <- (PimaIndiansDiabetes$mass - min(PimaIndiansDiabetes$mass))/
  (max(PimaIndiansDiabetes$mass) - min(PimaIndiansDiabetes$mass))

PimaIndiansDiabetes$res_pregnant <- (PimaIndiansDiabetes$pregnant - min(PimaIndiansDiabetes$pregnant))/
  (max(PimaIndiansDiabetes$pregnant) - min(PimaIndiansDiabetes$pregnant))

summary(PimaIndiansDiabetes)

#training data
train_index_res <- sample(1:768, size=0.8*768)
train_data_res <- PimaIndiansDiabetes[train_index_res, ]
test_data_res <- PimaIndiansDiabetes[-train_index_res, ]

# re-build the model with rescaled variables
res_logit <- glm(binary ~ res_age+res_mass+res_pregnant, data=train_data_res, family="binomial")

summary(res_logit)




