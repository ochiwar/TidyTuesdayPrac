library(dslabs)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(tidyr)
library(RColorBrewer)

murders <- as_tibble(murders)
data(heights)
data(NHANES)
data("gapminder")
us_contagious_diseases <- as_tibble(us_contagious_diseases)


head(gapminder)

index <- order(murders$total)
murders$abb[index]

extra_col <-  murders %>% mutate(rate = total/population* 100000, rank = rank(-rate))

new_murder <-  murders %>% mutate(rate = total/population* 100000, rank = rank(-rate)) %>%
    filter(region %in% c("Northeast","West") & rate <1) %>% 
    select(state, rate, rank) 
    


avg <- function(x){
    
    s = sum(x)
    l = length(x)
    
    s/l
}

x <- 1:100
avg(x)

compute_s_n <- function(n){
    x <- 1:n
    sum(x)
}

m <-  25
#create an empty vector 
s_n <- vector(length = m)
for (n in 1:m) {
    s_n[n] <- compute_s_n(n)
}
n <- 1:m
plot(n,s_n)
lines(n, n*(n+1)/2)

#Male Height Index
index <- heights$sex == "Male"
x <- heights$height[index]

sum_k <- sum(69 < x & x <= 72)/length(x)
mean_k <- mean(69 < x & x <= 72)
c(sum = sum_k, mean = mean_k)


average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

j <- scale(x)

mean(abs(j) <2)

#Female Height
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]

c(male = length(male), female = length (female))

#Percentiles
female_percentiles <- quantile(female, seq(.1,.9,.2))
male_percentiles <- quantile(male, seq(.1,.9,.2))
df <- data.frame (female = female_percentiles, male = male_percentiles)
df

r <- murders %>%  
    summarise( rate = sum(total)/ sum(population) * 10^6) %>% .$rate

murders %>% ggplot(aes(population/10^6, total, label = abb)) + 
    geom_abline(intercept = log10(r), lty = 2, color = "darkgrey")+
    geom_point(aes( color = region), size = 3) + 
      geom_text_repel()+
    scale_x_log10()+
    scale_y_log10() + 
    labs(
        title = "US Gun Murders in 2010",
        y = "Total Number of Murders (log scale)",
        x = "Populations in millions (log scale)"
    ) + 
    scale_color_discrete(name = "Region") + 
    theme_economist()


p <- heights %>% 
    filter(sex == "Male") %>% 
    ggplot(aes(x = height))

p + geom_histogram(binwidth = 1, fill = "blue", color = "black" ) + 
    labs(
        title = "Histogram",
        x = "Male heights in inches"
    )

p + geom_density(fill = "skyblue")

j <- heights %>%  
    filter(sex == "Male") %>% 
    ggplot(aes(sample = height))

j + geom_qq()

p1 <- p + geom_histogram(binwidth = 1, fill = "blue", color = "black" )
p2 <-  p + geom_histogram(binwidth = 2, fill = "blue", color = "black" )
p3 <-  p + geom_histogram(binwidth = 3, fill = "blue", color = "black" )
library(gridExtra)
grid.arrange(p1,p2,p3, ncol = 3)

murders %>%  
    ggplot(aes(population, total, label = abb)) + 
    geom_label(color = "blue")


s <- heights %>% 
    filter(sex == "Male") %>% 
    summarise(average = mean(height), standard_deviation = sd(height))

#dotplaceholer - return vectors as against data frames

us_murder_rate <- murders %>% 
    summarise(rate = sum(total)/ sum(population)*10^5) %>% .$rate
us_murder_rate

heights %>% 
    group_by(sex) %>% 
    summarise(average = mean(height),
              median_height = median(height))

heights %>%  top_n(-7, height)

data(NHANES) 
NHANES %>%  
    filter(Gender == "male") %>% 
    group_by(AgeDecade) %>% 
    summarise(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

NHANES %>%  
    group_by(AgeDecade, Gender) %>% 
    summarise(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>% 
    ungroup()

NHANES %>% 
    filter (Gender == "male" & AgeDecade == " 40-49")%>% 
    group_by(Race1)%>% 
    summarize(average = mean(BPSysAve, na.rm = TRUE), 
              standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>% 
    arrange(average)

#Infant Mortality Rates for Sri Lanka and Turkey in 2015

# Gampminder Data ----
gapminder %>% 
    filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%  
    select(country, infant_mortality)


gp <- gapminder %>% 
    filter(year == 1962) %>% 
    ggplot(aes(fertility, life_expectancy, color = continent))


gp  + geom_point()

#Faceting---- 
gp2 <- gapminder %>% 
    filter(year %in% c(1962, 2012)) %>% 
    ggplot(aes(fertility, life_expectancy, color = continent))
gp2  + geom_point() + facet_grid(continent~year)

#Using only one variable in this case year to compare
gp2  + geom_point() + facet_grid(.~year)

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c ("Europe", "Asia")

jp <- gapminder %>% 
    filter(year %in% years, continent %in%  continents) %>% 
    ggplot(aes(fertility, life_expectancy, color = continent)) 

jp + geom_point() + facet_wrap(.~year)


countries <-  c("South Korea", "Germany")
gapminder %>% 
    filter (country %in% countries) %>% 
    ggplot(aes(year, fertility, color = country)) +
    geom_line(size = 1) + 
    theme_minimal()

# Adding labels on line plots----

countries <-  c("South Korea", "Germany")
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60,72))
gapminder %>% 
    filter (country %in% countries) %>% 
    ggplot(aes(year, life_expectancy, color = country)) +
    geom_line(size = 1) + 
    geom_text(data = labels, aes(x,y,label = country), size = 5)+
    theme(legend.position = "none")
    theme_minimal()
    
gapminder <-  gapminder %>% 
    mutate(dollars_per_day = gdp/population/365)
gapminder %>%  glimpse()


past_year <- 1970
present_year <-  2010

# Plotting X Axis in Log Base 2 and Understanding it ----
gapminder %>% 
    filter(year == past_year & !is.na(gdp)) %>% 
    ggplot(aes((dollars_per_day))) + 
    geom_histogram(binwidth = 1, color = "white") + 
    scale_x_continuous(trans = "log2")

# Number of regions in Gapminder Data ----
length(levels(gapminder$region))

# Plot Box Plot and arrange X axis 

yt <-  gapminder %>% 
    filter(year == past_year & !is.na(gdp)) %>% 
    ggplot(aes(region,dollars_per_day))

yt + geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Re-order factor based on a function FUN 

yt_2 <-  gapminder %>% 
    filter(year == past_year & !is.na(gdp)) %>% 
    mutate(region = reorder(region, dollars_per_day, FUN =  median)) %>% 
    ggplot(aes(region,dollars_per_day, fill = continent))
yt_2 + geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab("") + 
    scale_y_continuous(trans = "log2")

# Define a vector for regions in the west 
west <-  c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

# Compare distribution Across time ----

gapminder %>% 
    filter(year == past_year & !is.na(gdp)) %>% 
    mutate(group = ifelse(region %in% west, "West", "Developing" )) %>% 
    ggplot(aes((dollars_per_day))) + 
    geom_histogram(binwidth = 1, color = "white") + 
    scale_x_continuous(trans = "log2") + 
    facet_grid(~group)

gapminder %>% 
    filter(year %in%  c(present_year, past_year) & !is.na(gdp)) %>% 
    mutate(group = ifelse(region %in% west, "West", "Developing" )) %>% 
    ggplot(aes((dollars_per_day))) + 
    geom_histogram(binwidth = 1, color = "white") + 
    scale_x_continuous(trans = "log2") + 
    facet_grid(year ~ group)

# Which countries had data in 1970 and 2010 ----
country_list_1 <- gapminder %>% 
                  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country

country_list_2 <- gapminder %>% 
    filter(year == present_year & !is.na(dollars_per_day)) %>% .$country

#find countries on both lists - there are 108 of them
country_list <-  intersect(country_list_1, country_list_2)

gapminder %>% 
    filter(year %in%  c(present_year, past_year) & country %in% country_list) %>% 
    mutate(group = ifelse(region %in% west, "West", "Developing" )) %>% 
    ggplot(aes((dollars_per_day))) + 
    geom_histogram(binwidth = 1, color = "white") + 
    scale_x_continuous(trans = "log2") + 
    facet_grid(year ~ group)

# Ease comparisons using box plots lying side by side ----

yt_3 <-  gapminder %>% 
    filter(year %in%  c(present_year, past_year) & country %in% country_list) %>% 
    mutate(region = reorder(region, dollars_per_day, FUN =  median)) %>% 
    ggplot(aes(region,dollars_per_day, fill = continent))
yt_3 + geom_boxplot(aes(fill = factor(year))) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab("") + 
    scale_y_continuous(trans = "log2") 

# Insights using Density Plots and an example table----

gapminder %>% 
    filter(year %in%  c(present_year, past_year) & country %in% country_list) %>% 
    mutate(group = ifelse(region %in% west, "West", "Developing" )) %>% 
    group_by(group) %>% 
    summarize(n = n()) %>%  knitr::kable()


dplot <- gapminder %>% 
    filter(year %in%  c(present_year, past_year) & country %in% country_list) %>% 
    mutate(group = ifelse(region %in% west, "West", "Developing" )) %>% 
    ggplot(aes(x = dollars_per_day, y =..count.. , fill = group)) + 
    scale_x_continuous(trans = "log2") 
dplot + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~.)

#case when to define groups 

gapminder <- gapminder %>% 
    mutate(group = case_when(
        .$region %in% west ~ "West",
        .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
        .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
        .$continent =="Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
        TRUE ~ "Others"
    ))
gapminder <- gapminder %>%  
    mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

dplot2 <- gapminder %>% 
    filter(year %in%  c(present_year, past_year) & country %in% country_list) %>% 
    # mutate(group = ifelse(region %in% west, "West", "Developing" )) %>% 
    ggplot(aes(x = dollars_per_day, y =..count.. , fill = group)) + 
    scale_x_continuous(trans = "log2") 
dplot2 + geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~.)

# Using a weight mapping argument
dplot3 <- gapminder %>% 
    filter(year %in%  c(present_year, past_year) & country %in% country_list) %>% 
    group_by(year) %>% 
    mutate(weight = population/sum(population)*2 ) %>%
    ungroup() %>% 
    ggplot(aes(x = dollars_per_day, fill = group, weight = weight)) + 
    scale_x_continuous(trans = "log2") 

dplot3 + geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~.)

# Define more regions using Case When 
#Ecological Fallacy (Survival Rates) ----

gapminder <- gapminder %>% 
    mutate(group = case_when(
        .$region %in% west ~ "The West",
        .$region %in% "Northern Africa" ~ "Northern Africa",
        .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
        .$region == "Southern Asia" ~ "Southern Asia",
        .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
        .$continent =="Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
        .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands",
    ))

surv_income <- gapminder %>% 
    filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>% 
    group_by(group) %>% 
    summarise(income = sum(gdp)/sum(population)/ 365,
              infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))  
    
surv_income %>% arrange(income)

surv_income %>% 
    ggplot(aes(income, infant_survival_rate, label = group, color = group)) + 
    scale_x_continuous(trans = "log2", limit = c(0.25, 150)) + 
    scale_y_continuous(trans = "logit", limit = c(0.875, .998), breaks = c(0.85, .90, .95, .99, .995, .998)) + 
    geom_label(size = 3, show.legend = FALSE)

#Exercises ----
# Using ggplot and the points layer, create a scatter plot of life expectancy versus fertility 
#for the African continent in 2012.
year_now <-  2012

africa_tbl <- gapminder %>% 
    select(country, year, life_expectancy, fertility, continent, region) %>% 
    filter(year %in% year_now & continent == "Africa") %>% 
    ggplot(aes(fertility,life_expectancy, color = region)) 
    
    
    africa_tbl + geom_point(size = 1.5)
    
# Create a table showing the country and region for the African countries (use select) 
# that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.
    
    df <- gapminder %>% 
        select(year, country, life_expectancy, fertility, continent, region) %>% 
        filter(year %in% year_now & continent == "Africa" & fertility <=3 & life_expectancy >= 70 ) %>% 
        select(country, region)

# Did war have an effect on the life expectancy of Vietnam 
years_considered <- c(1960:2010)
countries_considered <- c("Vietnam", "United States")
    tab <-  gapminder %>% 
    filter(year %in% years_considered & country %in% countries_considered )
    
kd <-  tab %>% 
    ggplot(aes(year, life_expectancy, col = country)) + 
    geom_line()
kd    
    
# Use a single line of code to create a time series plot from 1960 to 2010 of life expectancy vs year for Cambodia.
# Cambodia was also involved in this conflict and, after the war, Pol Pot and his communist Khmer Rouge took control 
#and ruled Cambodia from 1975 to 1979. He is considered one of the most brutal dictators in history. 
#Do the data support this claim?

gapminder %>%  filter(country == "Cambodia" & year %in% years_considered ) %>%  
    ggplot (aes(year, life_expectancy)) + 
    geom_line(color = "red", size = 1.5)

#1.4
#Now we are going to calculate and plot dollars per day for African countries in 2010 using GDP data.
#In the first part of this analysis, we will create the dollars per day variable.

gapminder <- gapminder %>%  as_tibble()

daydollars <- gapminder %>%  
    filter(continent == "Africa", year == 2010, !is.na(dollars_per_day))

# Plotting density plots 

daydollars %>%  
    ggplot(aes(dollars_per_day)) + 
    geom_density(fill = "sky blue") + 
    scale_x_continuous(trans = "log2")

#Now we are going to combine the plotting tools we have used in the past two exercises 
#to create density plots for multiple years
years_considered_2 <- c(1970, 2010)
gapminder %>% as_tibble %>%
    filter(year %in% years_considered_2 & continent == "Africa" & !is.na(dollars_per_day)) %>%
    # group_by (year) %>%
    # ungroup()%>%
    ggplot(aes(dollars_per_day, fill = factor(year))) + 
    geom_density (bw = 0.5, position = "stack") + 
    scale_x_continuous(trans = "log2") + 
    facet_grid(.~year)

#Now for multiple regions stacked ----
years_considered_2 <- c(1970, 2010)
    gapminder %>% as_tibble %>%
    filter(year %in% years_considered_2 & continent == "Africa" & !is.na(dollars_per_day)) %>%
    group_by (region) %>%
    ungroup()%>%
    ggplot(aes(dollars_per_day, fill = region)) + 
    geom_density (bw = 0.5, position = "stack", alpha = 0.25) + 
    scale_x_continuous(trans = "log2") + 
    facet_grid(.~year)

#We are going to continue looking at patterns in the gapminder dataset by plotting 
#infant mortality rates versus dollars per day for African countries.    
    
    gapminder %>% as_tibble %>%
        filter(year == 2010 & continent == "Africa" & !is.na(dollars_per_day)) %>%
        ggplot(aes(dollars_per_day, infant_mortality, color = region)) + 
        geom_point() + 
        scale_x_continuous(trans = "log2")
    
  
#Use labels in place of points
    gapminder %>% as_tibble %>%
        filter(year == 2010 & continent == "Africa" & !is.na(dollars_per_day)) %>%
        ggplot(aes(dollars_per_day, infant_mortality, color = region)) + 
        geom_label(aes(label = country)) + 
        scale_x_continuous(trans = "log2")
    
    
#Now we are going to look at changes in the infant mortality and dollars per day 
#patterns African countries between 1970 and 2010.
    
    years_considered_2 <- c(1970, 2010)
    gapminder %>% as_tibble %>%
        filter(year %in% years_considered_2 & continent == "Africa" & !is.na(dollars_per_day)& !is.na(infant_mortality)) %>%
        ggplot(aes(dollars_per_day, infant_mortality, color = region)) + 
        geom_text(aes(label = country)) + 
        scale_x_continuous(trans = "log2") + 
        facet_grid(year~.)
    
# Visualization Examples ----
    
    dat <-  us_contagious_diseases %>% 
        filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% 
        mutate(rate = count / population * 10000 * 52 / weeks_reporting) 
        state <- dat$state 
        rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
        
# Reorder Factor ----
#Redefine the "state" object so that the levels are re-ordered by rate.
        
        state <- reorder(state, rate)
        print(state)
        levels(state)
  
dat %>% 
    mutate(state = reorder(state,rate)) %>% 
    ggplot(aes(state, rate)) +
    geom_bar(stat="identity") +
    coord_flip()     

#Make a box plot of the murder rates by region.
#Order the regions by their median murder rate. 

murders %>%  mutate(rate = total/population*100000) %>% 
    mutate(region = reorder(region, rate, median)) %>% 
    ggplot(aes(region, rate)) + 
    geom_point()+
    geom_boxplot()


#Visualization Exercises Part 2 (Tile Plot)----
# The sample code given creates a tile plot showing the rate of measles cases per population. 
#We are going to modify the tile plot to look at smallpox cases instead.

the_disease = "Measles"
dat2 <- us_contagious_diseases %>% 
    filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>% 
    mutate(rate = count / population * 10000) %>% 
    mutate(state = reorder(state, rate))

dat2 %>% ggplot(aes(year, state, fill = rate)) + 
    geom_tile(color = "grey50") + 
    scale_x_continuous(expand=c(0,0)) + 
    scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
    theme_minimal() + 
    theme(panel.grid = element_blank()) + 
    ggtitle(the_disease) + 
    ylab("") + 
    xlab("")

#Look for Small Pox
#Exclude years in which cases were reported in fewer than 10 weeks from the plot.

the_disease_2 = "Smallpox"
dat3 <- us_contagious_diseases %>% 
    filter(!state%in%c("Hawaii","Alaska") & disease == the_disease_2 & !weeks_reporting < 10 ) %>% 
    mutate(rate = count / population * 10000) %>% 
    mutate(state = reorder(state, rate))

dat3 %>% ggplot(aes(year, state, fill = rate)) + 
    geom_tile(color = "grey50") + 
    scale_x_continuous(expand=c(0,0)) + 
    scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
    theme_minimal() + 
    theme(panel.grid = element_blank()) + 
    ggtitle(the_disease_2) + 
    ylab("") + 
    xlab("")

#Highlighting Average plot with several line plots ---- 

dat4 <- us_contagious_diseases %>%
    filter(!state%in%c("Hawaii","Alaska") & disease == the_disease) %>%
    mutate(rate = count / population * 10000) %>%
    mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
    filter(disease==the_disease) %>% group_by(year) %>%
    summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat4 %>% ggplot() +
    geom_line(aes(year, rate, group = state),  color = "grey50", 
              show.legend = FALSE, alpha = 0.2, size = 1) +
    geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
    scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
    ggtitle("Cases per 10,000 by state") + 
    xlab("") + 
    ylab("") +
    geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
    geom_vline(xintercept=1963, col = "blue")

# Repeat Same for Small pox 

dat5 <- us_contagious_diseases %>%
    filter(!state%in%c("Hawaii","Alaska") & disease == the_disease_2 & !weeks_reporting < 10) %>%
    mutate(rate = count / population * 10000) %>%
    mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
    filter(disease==the_disease_2) %>% group_by(year) %>%
    summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat5 %>% ggplot() +
    geom_line(aes(year, rate, group = state),  color = "grey50", 
              show.legend = FALSE, alpha = 0.2, size = 1) +
    geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
    scale_y_continuous(trans = "sqrt", breaks = c(2,5,20)) + 
    ggtitle("Cases per 10,000 by state") + 
    xlab("") + 
    ylab("") +
    geom_text(data = data.frame(x=1930, y=4.5), mapping = aes(x, y, label="US average"), color="black") + 
    geom_vline(xintercept=1938, col = "blue")


# Plot all rates of diseases for the state of California 

us_contagious_diseases %>% filter(state=="California", !weeks_reporting < 10) %>% 
    group_by(year, disease) %>%
    summarize(rate = sum(count)/sum(population)*10000) %>%
    ungroup() %>% 
    ggplot(aes(year, rate, color = disease)) + 
    geom_line() + 
    scale_y_continuous(trans = "sqrt", breaks = c(5,25,125))

# Now we are going to make a time series plot for the rates of all diseases in the United States. 

us_contagious_diseases %>% 
    filter(!is.na(population)) %>% 
    group_by(year, disease) %>% 
    summarize(rate = sum(count)/ sum(population) * 10000) %>% 
    ungroup() %>% 
    ggplot(aes(year, rate, color = disease)) + 
    geom_line(size = 1) + 
    scale_y_continuous(trans = "sqrt", breaks = c(5,25,60)) + 
    labs(
        title = "Rates of diseases per 10000 in the US "
    )

dat5
