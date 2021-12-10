
# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(dslabs)
library(sysfonts)
library(extrafont)


sysfonts::font_add_google("Source Sans Pro", family = "Source_Sans_Pro")
sysfonts::font_families_google()

data("heights")
head(heights)

data(murders)
head(murders)

str(murders)

murders %>% 
    group_by(region) %>% 
    summarise(
        proportion = n()/nrow(murders)
    ) %>% 
    ggplot(aes(region, proportion, fill = region)) + 
    geom_col()


prop.table(table(heights$sex))


r <- murders %>%
    summarize(rate = sum(total) / sum(population) * 10^6) %>%
    pull(rate)

murders %>% 
    ggplot(aes(population/10^6, total, label = abb)) +
    geom_abline(intercept = log10(r), lty = 2, col = "darkgrey") +
    geom_point(aes(col = region),size = 3) +
    geom_text(nudge_x = 0.075) +
    
    
    scale_x_log10() +
    scale_y_log10() +
    
    labs(
        x = "Populations in Millions (log scale)",
        y = "Total number of murders (log scale)",
        title = "US Gun Murders in US 2010"
    )
library(NHANES)
data(NHANES)

NHANES %>% 
    filter(AgeDecade == " 20-29" & Gender == "female") %>% 
    summarise(average = mean(BPSysAve, na.rm = T), standard_deviation = sd(BPSysAve, na.rm = T)) %>% 
    .$average


NHANES %>% 
    
    filter(Gender == "female") %>% 
    group_by(AgeDecade) %>% 
    summarise(average = mean(BPSysAve, na.rm = T), standard_deviation = sd(BPSysAve, na.rm = T)) 

NHANES %>% 
    
    filter(Gender == "male") %>% 
    group_by(AgeDecade) %>% 
    summarise(average = mean(BPSysAve, na.rm = T), standard_deviation = sd(BPSysAve, na.rm = T)) 

NHANES %>% 
    group_by(AgeDecade, Gender) %>% 
    summarise(average = mean(BPSysAve, na.rm = T), standard_deviation = sd(BPSysAve, na.rm = T))


NHANES %>% 
    
    filter(Gender == "male", AgeDecade == " 40-49") %>% 
    
    group_by(Race1) %>% 
    
    summarise(average = mean(BPSysAve, na.rm = T), standard_deviation = sd(BPSysAve, na.rm = T)) %>% 
    
    arrange(average)
   

data("gapminder")

gapminder %>% head()

gapminder %>% 
    filter(year == 2015, country %in% c("Turkey", "Sri Lanka") ) %>% 
    select(country, infant_mortality)

filter(gapminder, year == 1962) %>%
    ggplot(aes(fertility, life_expectancy, color = continent)) +
    geom_point()


filter(gapminder, year %in% c(1962, 2012)) %>%
    ggplot(aes(fertility, life_expectancy, col = continent)) +
    geom_point() +
    facet_grid(. ~ year)

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
    filter(year %in% years & continent %in% continents) %>%
    ggplot(aes(fertility, life_expectancy, col = continent)) +
    geom_point() +
    facet_wrap(~year)

countries <- c("South Korea", "Germany")
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
    ggplot(aes(year, life_expectancy, col = country)) +
    geom_line() +
    geom_text(data = labels, aes(x, y, label = country), size = 4) +
    theme(legend.position = "none")

gapminder <- gapminder %>%
    mutate(dollars_per_day = gdp/population/365)

past_year <- 1979

gapminder %>%
    filter(year == past_year & !is.na(gdp)) %>%
    ggplot(aes(log2(dollars_per_day))) +
    geom_histogram(binwidth = 1, color = "black")

gapminder %>% 
    filter(year == past_year & !is.na(gdp)) %>% 
    ggplot(aes(dollars_per_day)) +
    geom_histogram(binwidth = 1, color = "black") +
    scale_x_continuous(trans = "log2")

levels(gapminder$region)
gapminder %>% head()

gapminder %>% 
    filter(year == past_year & !is.na(gdp)) %>% 
    mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
    ggplot(aes(y = dollars_per_day, x = region, fill = continent)) + 
    geom_boxplot() +
    labs (
        x = ""
    ) +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1)
    ) + 
    scale_y_continuous(trans = "log2") + 
    geom_point(show.legend = F)
    
 data("us_contagious_diseases")   

 us_contagious_diseases %>%  as_tibble() -> us_contagious_diseases

 the_disease <- "Measles"
 
dat <- us_contagious_diseases %>% 
     filter(!state %in% c("Alaska", "Hawaii")  & disease == the_disease & weeks_reporting >= 10) %>% 
     mutate(rate = count / population *10^4 * 52/weeks_reporting) %>% 
     mutate(state = reorder(state,rate))

dat %>%  
    filter(state == "California", !is.na(rate)) %>% 
    ggplot(aes(year, rate)) +
    geom_line() + 
    ylab("Cases per 10,000") +
    geom_vline(xintercept = 1963, color = "blue")

#Tile Plot

dat %>% 
    ggplot(aes(year, state, fill = rate)) + 
    geom_tile(color = "gray50") + 
    scale_x_continuous(expand = c(0,0)) +  
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = "Reds"), trans = "sqrt") + 
    geom_vline(xintercept = 1963, col = "blue") +
    theme_minimal() + theme(panel.grid = element_blank()) +
    ggtitle(the_disease) +
    ylab("") +
    xlab("")

avg <-  us_contagious_diseases %>%  
    filter(disease == the_disease) %>% 
    group_by(year)  %>% 
    summarise(us_rate = sum(count, na.rm = T) / sum(population, na.rm = T) * 10000)

dat %>% 
    filter(!is.na(rate)) %>% 
    ggplot() + 
    geom_line(aes(year, rate, group = state), color = "grey50", 
              show.legend = FALSE, alpha = 0.2, size = 1) + 
    geom_line(data = avg, aes(year, us_rate), size = 1, col = "black") + 
    scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) +
    ggtitle("Cases per 10,000 by state") +
    xlab("") +
    ylab("") + 
    geom_text (data = data.frame(x=1955, y = 50), aes(x, y, label = "US Average"), col = "black") + 
    geom_vline(xintercept = 1963, color = "blue")


#For Smallpox Calculations

the_disease_2 <- "Smallpox"

avg_2 <- us_contagious_diseases %>% 
    filter(disease == the_disease_2) %>% 
    group_by(year) %>% 
    summarise(us_rate = sum(count, na.rm = T)/ sum(population, na.rm = T) * 10000)

dat_2 <- us_contagious_diseases %>% 
    filter(disease == the_disease_2 & !state %in% c("Hawaii", "Alaska")) %>%
    mutate(us_rate = count / population * 10000 * 52 / weeks_reporting)

dat_2 %>% 
    ggplot() +
    geom_line(aes(year, us_rate, group = state), col = "gray64", size = 1, alpha = 0.2, show.legend = F) + 
    geom_line(data = avg_2, aes(year, us_rate), col = "black", size = 1) + 
    scale_y_continuous(trans = "sqrt", breaks = c(1,5,10,20)) + 
    geom_text(data = data.frame(x = 1938, y = 1.5), aes(x, y, label = "US Average"), col = "black") + 
    labs(
        title = "SmallPox", 
        x = "",
        y = "rate per 10000"
    )

us_contagious_diseases %>% filter(state=="California", weeks_reporting >= 10) %>% 
    group_by(year, disease) %>%
    summarize(rate = sum(count)/sum(population)*10000) %>%
    ggplot(aes(year, rate, color = fct_reorder2(disease, year, rate))) + 
    geom_line(size = 1) + 
    scale_y_continuous(trans = "sqrt", breaks = c(10,50,100))

rate <- us_contagious_diseases %>% 
    filter(!is.na(population)) %>% 
    group_by(year,disease) %>% 
    summarise(us_rate = sum(count, na.rm = T) / sum(population, na.rm = T) * 1e4)

rate %>% 
    ggplot() + 
    geom_line(aes(year, us_rate, color = disease))

options(digits = 3) # report 3 significant digits
library(titanic)

titanic <- titanic_train %>% 
    select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%  
    mutate( Survived = factor(Survived), Pclass = factor(Pclass), Sex = factor(Sex)) %>% 
    as_tibble() 


#Make Density plots of age grouped by sex

titanic %>% 
    
    ggplot(aes(Age, fill= Sex)) +
    geom_density(alpha = 0.5) + 
    facet_wrap(~Sex)


titanic %>% 
    ggplot(aes(Age, fill= Sex, after_stat(count))) +
    geom_density(alpha = 0.5, position = "stack") + 
    facet_wrap(~Sex)

titanic %>% 
    ggplot(aes(Age, fill= Sex, after_stat(count))) +
    geom_density(alpha = 0.2) 

params <- titanic %>%
    filter(!is.na(Age)) %>%
    summarize(mean = mean(Age), sd = sd(Age))

as.list(params) -> params 

titanic %>%  
    ggplot(aes(sample = Age)) + 
    stat_qq() +
    stat_qq_line()

titanic %>% 
    ggplot(aes(Survived, fill = Sex)) + 
    geom_bar(position = position_dodge())
#Most survivors were female

titanic %>% 
    count(Survived) %>% 
    mutate(proportion = n / sum(n)) %>% 
    ggplot(aes(Survived,  proportion)) + 
    geom_bar(stat = "identity")
#Less than half of the passengers survived


titanic %>% 
    ggplot(aes(Age, fill = Survived, after_stat(count))) +
    geom_density(alpha = 0.2) #0-8 age group was more likely to survive than die


titanic %>% 
    ggplot(aes(Age, fill = Survived)) +
    geom_density(alpha = 0.2) + #which age group had the most deaths 18-30
    facet_wrap(~Survived) # which aga gp had the highest proportion of deaths 70-80

titanic %>% 
    filter(!Fare == 0) %>% 
    group_by(Fare, Survived) %>% 
    ggplot(aes(Survived, Fare)) + 
    
    geom_boxplot() + 
    geom_jitter(alpha = 0.2) +
    scale_y_continuous(trans = "sqrt", breaks = c(10, seq(100,500,100)))
#Passengers who paid higher fares generally survived
#The median fare was lower for passengers who did not survive
#Most individuals who paid a fare around $8 did not survive


titanic %>% 
    ggplot(aes(Pclass, fill = Survived)) + 
    geom_bar()
#There were more third class passengers than the first two combined


titanic %>% 
    ggplot(aes(Pclass, fill = Survived)) + 
    geom_bar(position = position_fill()) + 
    labs(
        y = "proportion"
    )

#Survival proportion was highest for first class, followed by 2nd class. Third class had the lowest survival proportion
titanic %>% 
    ggplot(aes(Survived, fill = Pclass)) + 
    geom_bar(position = position_fill())
#Most passengers in first class survived. Most passengers in other classes did not survive
#The majority of those that did not survive where from 3rd class


titanic %>% 
    ggplot(aes(Age, fill = Survived, after_stat(count))) + 
    geom_density(alpha = 0.2) + 
    facet_wrap(Sex ~ Pclass)
#The largest group of passengers was 3rd class males
#Most first class and second class females survived
#almost all second class males did not survive with the exception of children





