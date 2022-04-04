


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


#simple classification models ------



library(tidyverse)
library(mdsr)

#example with cenus data
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

#load tidymodels, a collection of packages much like tidyverse 
library(tidymodels)

# split data into training and testing parts
set.seed(364)
n <- nrow(census)
census_parts <- census %>%
  initial_split(prop = 0.8) #80% training, 20% testing. produces a 'mc_split'

#training split (26048 obs)
train <- census_parts %>%
  training()

#test split (6513 obs)
test <- census_parts %>%
  testing()

#number of rows in each data
list(train, test) %>%
  map_int(nrow) #apply a function in each element of a list or vector. preferable to a loo in R.
?map_int

#% of high earners in training dataset
pi_bar <- train %>%
  count(income) %>% # number cases in each category within this column
  mutate(pct = n / sum(n)) %>% # n is the name of the column output when you use 'count'
  filter(income == ">50K") %>% # just get % of higher income earners
  pull(pct)
pi_bar

#create a dataset of the pct
train %>%
  count(income) %>%
  mutate(pct = n / sum(n))

#specify a 'null' model with no predictors of % of high income earners
mod_null <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(income ~ 1, data = train)

#predict and compute accuracy 
library(yardstick)
pred <- train %>%
  dplyr::select(income, capital_gain) %>% #remember often need to specify its dplyr function
  bind_cols(
    predict(mod_null, new_data = train, type = "class")
  ) %>%
  rename(income_null = .pred_class)

#estimate accuracy of 'null' model
accuracy(pred, income, income_null) #0.76

#confusion matrix
confusion_null <- pred %>%
  conf_mat(truth = income, estimate = income_null)
confusion_null
?conf_mat

#now try logistic regression with one predictor
?logistic_reg

mod_log_1 <- logistic_reg(mode = "classification") %>%
  set_engine("glm") %>%
  fit(income ~ capital_gain, data = train)

train_plus <- train %>%
  mutate(high_earner = as.integer(income == ">50K"))

#plot out this model in ggplot
ggplot(train_plus, aes(x = capital_gain, y = high_earner)) + 
  geom_count(
    position = position_jitter(width = 0, height = 0.05), 
    alpha = 0.5
  ) + 
  geom_smooth(
    method = "glm", method.args = list(family = "binomial"), 
    color = "dodgerblue", lty = 2, se = FALSE
  ) + 
  geom_hline(aes(yintercept = 0.5), linetype = 3) + 
  scale_x_log10(labels = scales::dollar) #put capital gain on log10 and make into $ signs

# see how accurate this model is
pred <- pred %>%
  bind_cols(
    predict(mod_log_1, new_data = train, type = "class")
  ) %>%
  rename(income_log_1 = .pred_class)

#see the accuracy of the model
confusion_log_1 <- pred %>%
  conf_mat(truth = income, estimate = income_log_1)
confusion_log_1

accuracy(pred, income, income_log_1)
#0.80

#evaluating models ------


income_probs <- pred %>%
  dplyr::select(income, income_log_1, capital_gain) %>%
  bind_cols(
    predict(mod_log_1, new_data = train, type = "prob")
  )

head(income_probs)

income_probs %>%
  group_by(rich = `.pred_>50K` > 0.5) %>%
  count() %>%
  mutate(pct = n / nrow(income_probs))


#receiving operating characteristic (ROC) curve displays relationship between
#sensitivity (true positivity rate) and specificity (true negative rate)

roc <- pred %>%
  mutate(estimate = pull(income_probs, `.pred_>50K`)) %>% #pull prediction from other dataframe (with same length)
  roc_curve(truth = income, estimate, event_level = "second") %>%
  autoplot() #uses ggplto code to plot our relationship between sensitivity and positivity rate for predicting high income


#revisit the training and testing dataset split

test %>% skim(capital_gain)
train %>% skim(capital_gain)
#pretty similar

#build dataframe holding the models
mods <- tibble(
  type = c("null", "log_1"),
  mod = list(mod_null, mod_log_1)
)

#go through each of the models using the predict function, both for training and testing
#dataset

mods_2 <- mods %>%
  mutate(
    y_train = list(pull(train, income)),
    y_test = list(pull(test, income)),
    y_hat_train = map(
      mod, 
      ~pull(predict(.x, new_data = train, type = "class"), .pred_class)
    ),
    y_hat_test = map(
      mod, 
      ~pull(predict(.x, new_data = test, type = "class"), .pred_class)
    )
  )
mods
head(mods_2[3])
#look at accuracy 
mods <- mods %>%
  mutate(
    accuracy_train = map2_dbl(y_train, y_hat_train, accuracy_vec),
    accuracy_test = map2_dbl(y_test, y_hat_test, accuracy_vec),
    sens_test = 
      map2_dbl(y_test, y_hat_test, sens_vec, event_level = "second"),
    spec_test = 
      map2_dbl(y_test, y_hat_test, spec_vec, event_level = "second")
  )

?sens_vec


?accuracy

#cross-validation example not from book with regression

library(caret)
data("swiss")
head(swiss, 3)

# Split the data into training and test set
set.seed(123)
samples <- swiss %>% initial_split(prop=0.8)
training_samples <- samples %>% training()
testing_samples <- samples %>% testing()

training_model <- lm(Fertility ~ Agriculture, data = training_samples)
summary(training_model)
  
# Make predictions and compute the R2, RMSE and MAE
predictions <- training_model %>% predict(testing_samples)
#outputs predicted fertility across all rows

data.frame( R2 = R2(predictions, training_samples$Fertility),
            RMSE = RMSE(predictions, training_samples$Fertility),
            MAE = MAE(predictions, training_samples$Fertility))

#http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/

# set.seed(123)
# training.samples <- swiss$Fertility %>%
#   createDataPartition(p = 0.8, list = FALSE)
# train.data  <- swiss[training.samples, ]
# test.data <- swiss[-training.samples, ]
# # Build the model
# model <- lm(Fertility ~ Agriculture, data = train.data)
# # Make predictions and compute the R2, RMSE and MAE
# predictions <- model %>% predict(test.data)
# data.frame( R2 = R2(predictions, test.data$Fertility),
#             RMSE = RMSE(predictions, test.data$Fertility),
#             MAE = MAE(predictions, test.data$Fertility))

?predict
