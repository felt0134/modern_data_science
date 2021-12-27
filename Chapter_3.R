#Chapter 3: A grammar for graphics


#data on countries
data("CIACountries")
d <- CIACountries
head(d)

#aesthetics----

#look a how GDP raltes to % spent on educatation
g <- ggplot(d,aes(x=gdp,y=educ)) +
  geom_point(size=5)

#modify the point or 'glyph' with color
g.2 <- ggplot(d,aes(x=gdp,y=educ)) +
  geom_point(aes(color=net_users),size=5)

#modify the point or 'glyph' with both color and size
g.3 <- ggplot(d,aes(x=gdp,y=educ)) +
  geom_point(aes(color=net_users,size=roadways))

#scales-----

#adjust the x coordinate to go from linear to log scale
g.4 <- ggplot(d,aes(x=gdp,y=educ)) +
  geom_point(aes(color=net_users,size=roadways)) +
  coord_trans(x = 'log10')

#* a log transformation is one way to deal with a right-skewed distribution.

#we can implment this also through scale_y_continuous and add commas
g.5 <- ggplot(d,aes(x=gdp,y=educ)) +
  geom_point(aes(color=net_users,size=roadways)) +
  scale_x_continuous(name='Gross domestic product',trans='log10',
                     labels=scales::comma)
#facets -----

#use facets to split up net (internet) users instead of colors. Less confusing.
g.6 <- ggplot(d,aes(x=gsp,y=educ)) +
  geom_point(aes(size=roadways)) +
  scale_x_continuous(name='Gross domestic product',trans='log10',
                     labels=scales::comma) +
  facet_wrap(~net_users,nrow=1) +
  theme(legend.position='top')

#layers-----

#medicare charges data
data("MedicareCharges")

#filter to New Jersey
charges_nj <- MedicareCharges %>%
  filter(stateProvider=='NJ')

#new jersey healthcare costs per procedure
g.7 <- ggplot(data=charges_nj,
              aes(x=reorder(drg,mean_charge),y=mean_charge)) +
  geom_col(fill='grey') +
  ylab('Statewide average charges') +
  xlab('Procedure') +
  theme(
    axis.text.x = element_text(angle=90,hjust=1,size=rel(0.5)))

#add layer of the rest of the country from the original dataset
g.8 <- ggplot(data=charges_nj,
              aes(x=reorder(drg,mean_charge),y=mean_charge)) +
  geom_col(fill='grey') +
  geom_point(data=MedicareCharges,size=1,alpha=0.3) +
  ylab('Statewide average charges') +
  xlab('Procedure') +
  theme(
    axis.text.x = element_text(angle=90,hjust=1,size=rel(0.5)))

#canonical graphics for specific use cases ------

#univariate
data("SAT_2010")

#histogram: use predefined bins that you can adjust
g.9 <- ggplot(data = SAT_2010,aes(x=math)) +
  geom_histogram(bindwidth=1,fill='white',color='black') +
  labs('Average math SAT score')

#density plot: use kernel smoother to make a continuous distribution
g.10 <- ggplot(data = SAT_2010,aes(x=math)) +
  geom_density(adjust=0.3) +
  labs('Average math SAT score')

#compare across categorical value of state
g.11 <- ggplot(data = SAT_2010,aes(x=reorder(state,math),y=math)) +
  geom_col(fill='grey',color='black') +
  labs(x='State',y='Average math SAT score') +
  theme(
    axis.text.x = element_text(angle=90,hjust=1,size=rel(1))) 

#stacked barchart
g.12 <- ggplot(data = mosaicData::HELPrct,aes(x=homeless)) +
  geom_bar(aes(fill=substance),position='fill') +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() #try with and without reverse the axes
 
#the stacked barchart is good for showing proportions. Drawback is a low data to
#ink ratio: lots of ink to show not much data.

#multivariate -------

#basic scatterplot between and x and a y variable
head(SAT_2010)
g.13 <- ggplot(data = SAT_2010,aes(x=expenditure,y=math)) +
  geom_point(fill='grey',color='black',pch=21) +
  labs(x='Expenditure',y='Average math SAT score') 

# add a regression line
g.14 <- ggplot(data = SAT_2010,aes(x=expenditure,y=math)) +
  geom_point(fill='grey',color='black',pch=21) +
  geom_smooth(method='lm',se=F) +
  labs(x='Expenditure',y='Average math SAT score') 

#create a new variable that looks at these relationships according to % of students taking SAT

SAT_2010 <- SAT_2010 %>%
  mutate(
    
    SAT_rate = cut(
      sat_pct,
      breaks = c(0,30,60,100),
      labels = c("low","medium","high")
      
    )
    
  )


head(SAT_2010)

#now fit a regression line for each color
g.15 <- ggplot(data = SAT_2010,aes(x=expenditure,y=math,color=SAT_rate)) +
  geom_point(fill='grey',color='black',pch=21) +
  geom_smooth(method='lm',se=F) +
  labs(x='Expenditure',y='Average math SAT score') 


library(NHANES)

#relationship between age and height for male versus female
g.16 <- ggplot(
  data = slice_sample(NHANES,n=1000),
  aes(x=Age,y=Height,color= fct_relevel(Gender,"male"))) + #relevel changes the order
  geom_point() +
  geom_smooth() +
  xlab("Age") +
  ylab("Height") +
  labs(color='Sex') 

#time series
library(macleish)

#change in temperature of the course of a year
data("whately_2015")
head(whately_2015)

g.17 <- ggplot(
  data = whately_2015,
  aes(x=when,y=temperature)) + #relevel changes the order
  geom_line(color='darkgrey') +
  geom_smooth() +
  xlab(NULL) +
  ylab("Temperature")


#same plot but categorize by month
whately_2015_dates <- whately_2015 %>%
  mutate(month=as.factor(lubridate::month(when,label=TRUE))) %>% #lubridate makes it easy to get info from dates
  group_by(month) %>%
  skim(temperature) %>% #skim lets you get summary stats for a given variable
  select(-na)

?lubridate
?skim

head(whately_2015_dates)

g.18 <- ggplot(
  data = whately_2015,
  aes(x=lubridate::month(when,label=TRUE),
      y=temperature)) + #relevel changes the order
  geom_boxplot() +
  xlab('Month') +
  ylab("Temperature")


#------historical baby names ----

library(babynames)

BabynamesDist <- make_babynames_dist()
head(BabynamesDist)

andrew <- BabynamesDist %>%
  filter(name == "Andrew" & sex == "M")
name_plot <- ggplot(data = andrew, aes(x = year))

#add bars layer
name_plot <- name_plot +
  geom_col(
    aes(y = count_thousands * alive_prob), 
    fill = "#b2d7e9", 
    color = "white",
    size = 0.1
  )

#add a line layer
name_plot <- name_plot + 
  geom_line(aes(y = count_thousands), size = 2)

#axes labels layer
name_plot <- name_plot +
  ylab("Number of People (thousands)") + 
  xlab(NULL)

#get a weightede stimates of the median year of birth (weighted by the number
#of people estimated to be alive today)

library(Hmisc)
wtd_quantile <- Hmisc::wtd.quantile
median_yob <- andrew %>%
  summarise(
    year = wtd_quantile(year, est_alive_today, probs = 0.5)
  ) %>% 
  pull(year)

#add a bar with that year and make it so it fits under the curve
name_plot <- name_plot +
  geom_col(
    color = "white", fill = "#008fd5", 
    aes(y = ifelse(year == median_yob, est_alive_today / 1000, 0))
  )

#add a curve to point to something
name_plot + geom_curve(
  x = 1970, xend = 1987, y = 20, yend = 10, 
  arrow = arrow(length = unit(0.3, "cm")), curvature = 0.5
)
#most common boy names

#get derived dataframe
com_male <- BabynamesDist %>%
  filter(n > 100, sex == "M") %>% 
  group_by(name) %>%
  mutate(wgt = est_alive_today / sum(est_alive_today)) %>%
  summarise(
    N = n(), 
    est_num_alive = sum(est_alive_today),
    quantiles = list(
      wtd_quantile(
        age_today, est_alive_today, probs = 1:3/4, na.rm = TRUE #1:3/4 = 0.25 0.50 0.75
      )
    )
  ) %>%
  mutate(measures = list(c("q1_age", "median_age", "q3_age"))) %>%
  unnest(cols = c(quantiles, measures)) %>%
  pivot_wider(names_from = measures, values_from = quantiles) %>%
  arrange(desc(est_num_alive)) %>%
  head(25)

#setup plot so ordered by age
w_plot <- ggplot(
  data = com_male, 
  aes(x = reorder(name, -median_age), y = median_age)
) + 
  xlab(NULL) + 
  ylab("Age (in years)") + 
  ggtitle("Median ages for ales with the 25 most common names")

#get ranges of age for each name
w_plot <- w_plot + 
  geom_linerange(
    aes(ymin = q1_age, ymax = q3_age), 
    color = "#f3d478", 
    size = 4.5, 
    alpha = 0.8
  )

#add points of the median age
w_plot <- w_plot +
  geom_point(
    fill = "#ed3324", 
    color = "white", 
    size = 2, 
    shape = 21
  )


#create a tribble like you would a datafreame
context <- tribble(
  ~median_age, ~x, ~label, 
  65, 24, "median",
  29, 16, "25th", 
  48, 16, "75th percentile",
)


age_breaks <- 1:7 * 10 + 5

w_plot + 
  geom_point(
    aes(y = 60, x = 24), 
    fill = "#ed3324", 
    color = "white", 
    size = 2, 
    shape = 21
  ) + 
  geom_text(data = context, aes(x = x, label = label)) + 
  geom_point(aes(y = 24, x = 16), shape = 17) + 
  geom_point(aes(y = 56, x = 16), shape = 17) +
  geom_hline(
    data = tibble(x = age_breaks), 
    aes(yintercept = x), 
    linetype = 3
  ) +
  scale_y_continuous(breaks = age_breaks) + 
  coord_flip()



