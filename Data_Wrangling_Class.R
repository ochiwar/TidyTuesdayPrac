#Direction to working folders ----
path
list.files(path) #Files within the folder

filename <- "murders.csv"
fullpath <- file.path(path,filename)
fullpath
file.copy(fullpath, getwd())
file.exists(filename)

read_lines(fullpath, n_max = 3)

dat <-  read_csv(fullpath)
head(dat)

dat2 <-  read.csv(filename, stringsAsFactors = FALSE)
dat2
class(dat2)
class(dat2$abb)
class(dat)

data("gapminder")
tidy_data <- gapminder %>% 
    filter(country %in% c("South Korea", "Germany")) %>% 
    select(country, year, fertility)
head(tidy_data)
tidy_data %>% 
    ggplot(aes(year,fertility, color = country)) +
    geom_point()

filename2 <- file.path(path,"fertility-two-countries-example.csv"  )
wide_data <- read_csv(filename2)
wide_data

new_tidy_data <- wide_data %>%  
    gather(year, fertility, -country, convert = TRUE)

new_tidy_data %>% 
    ggplot(aes(year,fertility, color = country)) +
    geom_point()


wide_txt <- read_csv("gather_example.txt")
wide_txt %>% 
    gather(disease, count, -("state":"population"),convert = TRUE)


spread_txt <-  read_csv("spread_example.txt")
spread_txt %>% 
    spread(year, time) #This code tells the function to create new columns for each year and spread the time values over those cells.


spread_txt2 <- read_csv("spread_example2.txt")
spread_txt2 %>% 
    #In this command, you properly specify that the column “var” will be used as the new column names, and that the column “people” 
    #should be spread into these two columns.
    spread(var, value = people )

#Separate and Unite ----

path
filename3 <-  file.path(path,"life-expectancy-and-fertility-two-countries-example.csv" )
raw_dat <-  read_csv(filename3)

dat3 <- raw_dat %>% 
    gather(key, value, -country)

dat3 %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>% 
    spread(variable_name, value)


#This column gathers the column names 2015_time, 2015_participants, 2016_time, and 2016_participants into one 
#column called “key”, with the values for each stored in the column “value.” The key column is then separated into two columns, “year” and “variable_name”. 
#The two entries for “variable_name”, time and participants, are then spread into their own columns.

separate_txt <-  read_csv("separate_example2.txt")
separate_txt %>% 
    gather(key = "key",value = "value",-age_group) %>% 
    separate(key, c("year", "variable_name"), sep = "_") %>% 
    spread(variable_name, value)


separate_txt2 <- read_csv("spread_example3.txt")
separate_txt2 %>%  
    separate(key, c("player", "variable_name" ), extra = "merge") %>% 
    spread(variable_name, value) 
   

#Combining Table ----

data("murders")
head(murders)
data("polls_us_election_2016")
head(results_us_election_2016)

tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

tab %>% 
    ggplot(aes(population/10^6, electoral_votes, label = abb)) + 
    geom_point()+ 
    geom_text_repel() + 
    scale_x_continuous(trans = "log2") + 
    scale_y_continuous(trans = "log2") + 
    geom_smooth(method = "lm", se = FALSE)


tab1 <-  slice(murders, 1:6) %>%  select(state, population)
tab1
tab2 <-  slice(results_us_election_2016, c(1:3,22,7:8)) %>%  select(state, electoral_votes)
tab2
k <- tab1$state %in% (tab2$state)
k
tab1$state[k]


left_join(tab1,tab2)

tab1 %>% right_join(tab2)

#Intersection of two tables
inner_join(tab1,tab2)

full_join(tab1,tab2)



#String Processing ----

string_processing <-  read_tsv("string_processing.txt") # Used tab separated values to import file 

string_processing %>% mutate_at(2:3, parse_number) # used the parse_number function to remove commas and dollar signs 

#You can use the str_replace_all command to replace both the “$” and “,” characters, by specifying these in 
#the “pattern” argument of the command. Combining this function with the mutate_at command allows you to reformat 
#both column two and three (Sales and Profit). 
#You then need to use the “as.numeric” command to convert these columns from character strings to numbers.

string_processing %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
    mutate_at(2:3, as.numeric)

#Raw data from reported heights data 
data("reported_heights")
class(reported_heights$height) # reported as character, should be numeric
xx <- as.numeric(reported_heights$height)
sum(is.na(xx))    # sum the number of NAs 


problems <- reported_heights %>%  mutate(new_height = as.numeric(height)) %>% 
    filter(is.na(new_height))  # filter observations with NAs 
    

#Find digits in a string 
#regex = regular expression 

yes <-  c("5", "6", "5'10", "5 feet", "4'11")
no <-  c ("", ".", "Five", "six")
s <-  c(yes, no)
pattern <-  "\\d" #used to find digits in a string
str_detect(s,pattern)
str_view(s, pattern)
str_view_all(s, pattern)

#pattern <- "\\d|ft"
#This regex identifies any numeric characters or the text "ft".

#^ = carrot and $ are called anchors signalling
#beginning and ending of a string 
#eg. ^d\\d$ start of the string followed by 1 digit followed by end of the string
pattern <- "^\\d{1,2}$" #begin string- look for 2-string digit and end
yes <- c("1", "2", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes,no),pattern)

pattern <- "^\\d$" #begin string, look for 1 digit and end
yes <- c("1", "2", "5", "9", "12")
no <- c("123", " 1","a4", "b") #1 not selected 'cos theres space in front
str_view(c(yes,no),pattern) # only 1,2,5,9 selected 


animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

#Your regex command tells R to look for either 4 or 5 lowercase letters in a row anywhere in 
#the string. This is true for the animals “puppy” and “Moose”.

pattern <- "^[4-7]'\\d{1,2}"
#^ carrot = start of string 
#[4-7] = one digit either 4,5,6 or 7
#(') feet symbol
#\\d{1,2} = one or two digits 
#\" = inches symbol where the \ is escape 
#$ end symbol = end of the string 

yes <-  c("5'7\"", "6'2\"", "5'12\"")
no <-  c("6,2\"", "6.2\"", "i am 5'11\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)
#While your first three strings have at least one lowercase letter [a-z], the string MONKEY 
#does not have any lowercase letters and will return a FALSE.

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)
#Your regex pattern tells str_detect to look for an uppercase ([A-Z]) letter at the end of the 
#string ($): this is only true for the string “MONKEY”.

yes <- c("AB", "A11B", "A111B")
no <-  c("A2B", "A221")
pattern <- "A1*B" #* asterisk char means zero or more instances of the previous character
str_detect(yes, pattern)

yes = c("AB", "A1B", "A11B", "A1111B")
data.frame(string = c("AB", "A1B", "A11B", "A1111B"), 
            none_or_more = str_detect(yes,"A1*B"),
           none_or_once = str_detect(yes,"A1?B"),
           once_or_more = str_detect(yes,"A1+B"))


#---
new_pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$" #\\s space
problems %>%  select (height) %>% 
    str_replace("feet|ft|foot", "'") %>%  #replace feet, ft and foot with '
    str_replace("inches|in|''|\"", "") %>%  #remove all inches symbols
    str_detect(new_pattern) %>% sum()

pattern <- "mo*"
animals <- c("moose", "monkey", "meerkat", "mountain lion")
str_detect(animals, pattern)
str_view(animals,pattern)

#This regex pattern ("mo*") looks for an “m” followed by zero or more “o” characters. 
#This is true for all strings in the animal vector.
#This regex pattern ("mo?")looks for an “m” followed by zero or one “o” characters. 
#This is true for all strings in the animal vector. Even though “moose” has 
#two “o”s after the “m”, it still matches the pattern.

schools <-  c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia" ,        
              "U California", "California State University")
schools %>% 
    str_replace("^U\\.?\\s|Univ\\.?\\s", "University ") %>% 
    str_replace("^University of|^University", "University of " )

str_subset(schools,"^Univ\\.+")




