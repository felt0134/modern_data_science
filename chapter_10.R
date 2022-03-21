


#Chapter 10: Predictive modeling

# The idea that a general specification for a model could be tuned to a specific 
# data set automatically has led to the field of machine learning.

# Machine learning is grouped into two categories:
# 
#   supervised: modeling response variable as a function of inputs, such as with regression
#   
#   unsupervised: findings patterns or groupings in data where there is no clear response variable

class(response ~ one + two)
#class 'formula'

# Many common motivations to develop functions. One of which they note is to
# 'understand how a system works'

# regression models for quantitative response variables return real numbers, 
# models for categorical response variables are called classifiers.

#simple classifications ------




#Census example 


library(tidyverse)
library(mdsr)
url <-
  "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
census <- read_csv(
  url,
  col_names = c(
    "age", "workclass", "fnlwgt", "education", 
    "education_1", "marital_status", "occupation", "relationship", 
    "race", "sex", "capital_gain", "capital_loss", "hours_per_week", 
    "native_country", "income"
  )
) %>%
  mutate(income = factor(income))
glimpse(census)

version
#https://www.tidymodels.org

# remove.packages("rlang")
# install.packages("rlang")

install.packages("tidymodels")
library(tidymodels)

#census data example
set.seed(364)
n <- nrow(census)

#split data: training data as 80% of the data, testing as remainng 20%
census_parts <- census %>%
  initial_split(prop = 0.8)

#?training
train <- census_parts %>%
  training()

#?testing
test <- census_parts %>%
  testing()

#combine
?map_int
list(train, test) %>%
  map_int(nrow)

# % of higher income in training dataset
pi_bar <- train %>%
  count(income) %>%
  mutate(pct = n / sum(n)) %>%
  filter(income == ">50K") %>%
  pull(pct)
pi_bar


# stopped at 10.2.1.1



