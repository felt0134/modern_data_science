
#Chapter 11: supervised learning



# Cenus data example -----


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

head(census,1)

library(tidymodels)
set.seed(364)
?set.seed
n <- nrow(census)

#80% training, 20% testing
census_parts <- census %>%
  initial_split(prop = 0.8)

#make the training and testing dataframe
train <- census_parts %>% training()
test <- census_parts %>% testing()

# % of income in training data that is above 50%
pi_bar <- train %>%
  count(income) %>%
  mutate(pct = n / sum(n)) %>%
  filter(income == ">50K") %>%
  pull(pct)


#decision trees ------
library(rpart)
mod_dtree <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(income ~ capital_gain, data = train) # income as related to capital gains

#identifies the optimal split for income as a function of capital gain
split_val <- mod_dtree$fit$splits %>%
  as_tibble() %>%
  pull(index)
#capital gain split is 5119

#find 'optimal' split for the census data
train_plus <- train %>% 
  mutate(hi_cap_gains = capital_gain >= split_val)

#visualize the split or 'single partition' in income according to capital gains (high or low)
ggplot(data = train_plus, aes(x = capital_gain, y = income)) + 
  geom_count(
    aes(color = hi_cap_gains), 
    position = position_jitter(width = 0, height = 0.1), 
    alpha = 0.5
  ) + 
  geom_vline(xintercept = split_val, color = "dodgerblue", lty = 2) + 
  scale_x_log10(labels = scales::dollar)

#the decision tree uses a single variable, capital gains,
#to 'vertically' partition the data into two groups. The algorithm tried
#all possible split values for capital gains and this is the one that
#reduced the gini coefficient the most


# we can bring in additional variables for income as well

# make formula with all variables. 
form <- as.formula(
  "income ~ age + workclass + education + marital_status + 
  occupation + relationship + race + sex + 
  capital_gain + capital_loss + hours_per_week"
)

mod_tree <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>%
  fit(form, data = train)
mod_tree

#many more split with this. Can visualize the tree (code not workin):
library(partykit)
plot(as.party(as.list(mod_tree$fit)))
?as.party

train_plus <- train_plus %>%
  mutate(
    husband_or_wife = relationship %in% c("Husband", "Wife"), 
    college_degree = husband_or_wife & education %in% 
      c("Bachelors", "Doctorate", "Masters", "Prof-school")
  ) %>%
  bind_cols(
    predict(mod_tree, new_data = train, type = "class")
  ) %>%
  rename(income_dtree = .pred_class)


cg_splits <- tribble(
  ~husband_or_wife, ~vals,
  TRUE, 5095.5, 
  FALSE, 7073.5
)

ggplot(data = train_plus, aes(x = capital_gain, y = income)) + 
  geom_count(
    aes(color = income_dtree, shape = college_degree), 
    position = position_jitter(width = 0, height = 0.1), 
    alpha = 0.5
  ) + 
  facet_wrap(~ husband_or_wife) + 
  geom_vline(
    data = cg_splits, aes(xintercept = vals), 
    color = "dodgerblue", lty = 2
  ) + 
  scale_x_log10()

#asses model performance
library(yardstick)
pred <- train %>%
  dplyr::select(income) %>%
  bind_cols(
    predict(mod_tree, new_data = train, type = "class")
  ) %>%
  rename(income_dtree = .pred_class)

#confusion matrix
confusion <- pred %>%
  conf_mat(truth = income, estimate = income_dtree)
confusion

#% of correct predictions
accuracy(pred, income, income_dtree)

#visualize confusion matrix
autoplot(confusion) +
  geom_label(
    aes(
      x = (xmax + xmin) / 2, 
      y = (ymax + ymin) / 2, 
      label = c("TN", "FP", "FN", "TP") #true negative, false positive...
    )
  )

#majority true negative (income < 50K)

#cans et the iprovement in accuracy lower from 1% to 0.2%
mod_tree2 <- decision_tree(mode = "classification") %>%
  set_engine("rpart", control = rpart.control(cp = 0.002)) %>%
  fit(form, data = train)


pred2 <- train %>%
  dplyr::select(income) %>%
  bind_cols(
    predict(mod_tree2, new_data = train, type = "class")
  ) %>%
  rename(income_dtree = .pred_class)

accuracy(pred2, income, income_dtree)
# ~2% increase in accuracy with lower threshold of improvement (resulting in more branches)



#random forests (need to update R) -----

# random forests are collections of decision trees that are aggregated, its like
# a collection of bootstrapped decision trees

#need a newer version of R for randomForest package in R

?rand_forest
?fit

mod_forest <- rand_forest(
  mode = "classification", #type of prediction outcome
  mtry = 3, # number of randomly sampled predictors on each split
  trees = 201 #number of trees in the ensemble (# of random smples?)
) %>%
  set_engine("randomForest") %>%
  fit(form, data = train) #remember 'form' is the previously defined model

# nearest neighbor ------

# focused on predicting outcomes without building models. Nearest neighbor
# assume that data points closer to each other have similar properties or 
# 'closeness', usually quantified as euclidean distance

library(kknn)

# distance metric only works with quantitative variables
train_q <- train %>%
  dplyr::select(income, where(is.numeric), -fnlwgt)

mod_knn <- nearest_neighbor(neighbors = 5, mode = "classification") %>%
  set_engine("kknn", scale = TRUE) %>%
  fit(income ~ ., data = train_q)

pred <- pred %>%
  bind_cols(
    predict(mod_knn, new_data = train, type = "class")
  ) %>%
  rename(income_knn = .pred_class)

pred %>%
  conf_mat(income, income_knn)

pred %>% accuracy(income,income_knn)

#if you want to see the best number of neighbors, you can see how accuracy
#changes with number of K by just building a function of the model and accuracy output and
#looping through different K values


# naive bayes -------

# takes advantage of conditional probabilities to do classifications 


library(discrim)

?naive_Bayes
mod_nb <- naive_Bayes(mode = "classification") %>%
  set_engine("klaR") %>%
  fit(form, data = train)

pred <- pred %>%  
  bind_cols(
    predict(mod_nb, new_data = train, type = "class")
  ) %>%
  rename(income_nb = .pred_class)

accuracy(pred, income, income_nb)


# neural network (stopped here) ------