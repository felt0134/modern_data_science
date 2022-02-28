

#chapter 9: statstical foundations

#Any given data are a sample of a larger population
#this chapter provies tools to connect samples to populations


# Samples and populations nyc flights example ------

library(tidyverse)
library(mdsr)
library(nycflights13)
SF <- flights %>%
  filter(dest == "SFO", !is.na(arr_delay))

set.seed(101)

#randomly subset 25 rows
sf_25 <- SF %>%
  slice_sample(n = 25)

#get summary statstics for a specific column (arr_delay)
sf_25 %>%
  skim(arr_delay)

#compare this to the whole population
SF %>%
  skim(arr_delay)

#look at 98th quantile of sample distibution 
sf_25 %>%
  summarize(q98 = quantile(arr_delay, p = 0.98))
#168

#compare to population
SF %>%
  summarize(q98 = quantile(arr_delay, p = 0.98))
#153 - in line with the sample

#see % of flights where delay is less than
SF %>%
  group_by(arr_delay < 168) %>%
  count() %>%
  mutate(pct = n / nrow(SF))
#98% of flights have delay less than 153 minutes


#Sample statstics examples -----

#get a mean of a sample
n <- 25
SF %>%
  slice_sample(n = n) %>%
  summarize(mean_arr_delay = mean(arr_delay))

#get mean of population
SF %>%
  slice_sample(n = n) %>%
  summarize(mean_arr_delay = mean(arr_delay))

library(purrr)

#do this 500 times: draw a samples from the populationa nd calculate the mean
num_trials <- 500
sf_25_means <- 1:num_trials %>%
  map_dfr(
    ~ SF %>%
      slice_sample(n = n) %>%
      summarize(mean_arr_delay = mean(arr_delay))
  ) %>%
  mutate(n = n)

#look at summary stats
hist(sf_25_means$mean_arr_delay)
sf_25_means %>%
  skim(mean_arr_delay)

summary(sf_25_means)
#not sure how skim is inherently better than summary.

#SE = SD f sampling distribution
#95% confidence interval = mean +/- 2 SE
sf_25_means %>%
  summarize(
    x_bar = mean(mean_arr_delay),
    se = sd(mean_arr_delay)
  ) %>%
  mutate(
    ci_lower = x_bar - 2 * se, # approximately 95% of observations 
    ci_upper = x_bar + 2 * se  # are within two standard errors
  )

#can get 95% CI using a t test
sf_25_means %>%
  pull(mean_arr_delay) %>%
  t.test() #is different...

#see how samplign distribution differs when you draw 100 random rows
n <- 100
sf_100_means <- 1:500 %>%
  map_dfr(
    ~ SF %>%
      slice_sample(n = n) %>%
      summarize(mean_arr_delay = mean(arr_delay))
  ) %>%
  mutate(n = n)


sf_25_means %>%
  bind_rows(sf_100_means) %>% #same as rbind
  ggplot(aes(x = mean_arr_delay)) + 
  geom_histogram(bins = 30) + 
  facet_grid( ~ n) + 
  xlab("Sample mean")

# As a rule, the standard error of a sampling distribution scales as  
# 1/√n
.

#bootstrap ------


# logical leap of treating the sampling distribution as the population itself
# throught the process of resampling: drawing a new sample from an existing sample
# (with replacement)

#resample withr replacement 
SF %>% slice_sample(n = 3, replace = TRUE)

#do a slice of 200 random rows
n <- 200
orig_sample <- SF %>% 
  slice_sample(n = n, replace = FALSE)


#a single resample
orig_sample %>%
  slice_sample(n = n, replace = TRUE) %>%
  summarize(mean_arr_delay = mean(arr_delay))

#now boostramp with resampling of the sliced sample, much like you do for a loop

?map_df

#Apply a function to each element of a list or atomic vector with map_dfr

sf_200_bs <- 1:num_trials %>%
  map_dfr(
    ~orig_sample %>%
      slice_sample(n = n, replace = TRUE) %>%
      summarize(mean_arr_delay = mean(arr_delay))
  ) %>%
  mutate(n = n)

sf_200_bs %>%
  skim(mean_arr_delay)

# nw do the same
sf_200_pop <- 1:num_trials %>%
  map_dfr(
    ~SF %>%
      slice_sample(n = n, replace = TRUE) %>%
      summarize(mean_arr_delay = mean(arr_delay))
  ) %>%
  mutate(n = n)

sf_200_pop %>%
  skim(mean_arr_delay)

# now relate this back to a sample of the airline data
orig_sample %>%
  summarize(q98 = quantile(arr_delay, p = 0.98))
#mean delay is 109

#now do some boostrapping. Do this for 10000 iterations for more data
n <- nrow(orig_sample)
sf_200_bs <- 1:10000 %>%
  map_dfr(
    ~orig_sample %>%
      slice_sample(n = n, replace = TRUE) %>%
      summarize(q98 = quantile(arr_delay, p = 0.98))
  )

sf_200_bs %>%
  skim(q98)

hist(sf_200_bs$q98)
# 95% CI = 107 +/- 20.2*2

# outliers (stopped here) -------



