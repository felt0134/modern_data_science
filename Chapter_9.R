

#chapter 9: statistical foundations

#ultimate objective in data science is to extract meaning from data

#part of a workflow: wrangling -> exploring -> visualizing -> modeling

#Any given data are a sample of a larger population
#this chapter provides tools to connect samples to populations


# Samples and populations nyc flights example ------

library(tidyverse)
library(mdsr)
library(nycflights13)

#filter to just flights to san fran
SF <- flights %>%
  filter(dest == "SFO", !is.na(arr_delay))

set.seed(101)

#randomly subset 25 rows
sf_25 <- SF %>%
  slice_sample(n = 25)

#get summary statistics for a specific column (arr_delay)
sf_25 %>%
  skim(arr_delay)

#compare this to the whole population - quite different
SF %>%
  skim(arr_delay)

#look at 98th quantile of sample distribution 
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


#Sample statistics examples -----

#get a mean of a sample from the population
n <- 25
SF %>%
  slice_sample(n = n) %>%
  summarize(mean_arr_delay = mean(arr_delay))

#get mean of population
SF %>%
  slice_sample(n = n) %>%
  summarize(mean_arr_delay = mean(arr_delay))

library(purrr)

#do this 500 times: draw a samples from the population and calculate the mean
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
  skim(mean_arr_delay) #skim is the same as summary with a little more info

summary(sf_25_means)
skim(sf_25_means)
#not sure how skim is inherently better than summary.

#if the mean is directionally different than the median, that suggests skew, which
#you can see in the samplign distribution: skewed right so and mean is ~2X as high as median

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
# throughout the process of resampling: drawing a new sample from an existing sample
# (with replacement)

#resample with replacement 
SF %>% slice_sample(n = 3, replace = TRUE)  #can get duplicate samples

#without replacement you would literally get all three of the unique samples

SF %>% slice_sample(n = 3, replace = FALSE) #always have unique samples

#do a slice of 200 random unique rows
n <- 200
orig_sample <- SF %>% 
  slice_sample(n = n, replace = FALSE)


#a single resample from that sample
orig_sample %>%
  slice_sample(n = n, replace = TRUE) %>%
  summarize(mean_arr_delay = mean(arr_delay))

#now boostramp with resampling of the sliced sample, much like you do for a loop

?map_df

#Apply a function to each element of a list or atomic vector with map_dfr
#boostrap this to see the sampling variation

sf_200_bs <- 1:num_trials %>%
  map_dfr(
    ~orig_sample %>%
      slice_sample(n = n, replace = TRUE) %>%
      summarize(mean_arr_delay = mean(arr_delay))
  ) %>%
  mutate(n = n)

sf_200_bs %>%
  skim(mean_arr_delay)

# now do the same at the population level and compare
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
#from the sample, this would suggest scheduling for flights to land 117 minutes before meeting

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

#large confidence interval.

hist(sf_200_bs$q98)
# 95% CI = 107 +/- 20.2*2

# outliers (stopped here) -------


#inspect what might be associated with long delays
SF %>%
  filter(arr_delay >= 420) %>% 
  select(month, day, dep_delay, arr_delay, carrier)

#plot out long delays
SF %>% 
  filter(arr_delay < 420) %>%
  ggplot(aes(arr_delay)) + 
  geom_histogram(binwidth = 15) + 
  labs(x = "Arrival delay (in minutes)")

#plot out longer delays by month

SF %>% 
  mutate(long_delay = arr_delay > 60) %>%
  group_by(month, long_delay) %>%
  count() %>%
  pivot_wider(names_from = month, values_from = n) %>% #long to wide format
  data.frame()

#6 and 7, june and july, stick out.


#this doesn't provide a method or framework for identifying outliers. Not sure how helpful this
#section is

#modeling (not much beyond LM) ------

SF %>%
  ggplot(aes(x = hour, y = arr_delay)) +
  geom_boxplot(alpha = 0.1, aes(group = hour)) +
  geom_smooth(method = "lm") + 
  xlab("Scheduled hour of departure") + 
  ylab("Arrival delay (minutes)") + 
  coord_cartesian(ylim = c(-30, 120))


mod1 <- lm(arr_delay ~ hour, data = SF)
broom::tidy(mod1)
summary(mod1) #summary gives a more complete pictue, with R-squared and such.


# confounding factors -----


SAT_2010 <- SAT_2010 %>%
  mutate(Salary = salary/1000)
SAT_plot <- ggplot(data = SAT_2010, aes(x = Salary, y = total)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ylab("Average total score on the SAT") + 
  xlab("Average teacher salary (thousands of USD)")
SAT_plot

# Shows a negative relationship between teacher salary and SAT score, but
# there are confounding variables - interactions - that are important

#control for percent of students taking SAT

# create a sat score column
SAT_2010 <- SAT_2010 %>%
  mutate(SAT_grp = ifelse(sat_pct <= 27, "Low", "High"))
SAT_2010

SAT_mod2 <- lm(total ~ Salary + sat_pct, data = SAT_2010)
summary(SAT_mod2)

#P - values as 'all or nothing' thinking has serious limits
