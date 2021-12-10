library(tidyverse)
library(readr)
library(lubridate)
library(reshape2)
library(zoo)

tuesdata <- tidytuesdayR::tt_load("2019-11-26")

studentLoan <- tuesdata$loans

str(studentLoan)

#Change to Quarterly 

studentLoan$year <- paste("20", studentLoan$year, sep = "")
quarterlyLoan <- studentLoan %>% unite(Quarterly, year, quarter, sep = "-")
quarterlyLoan$Quarterly <- as.yearqtr(quarterlyLoan$Quarterly)
quarterlyLoan$Quarterly <- lubridate::yq(quarterlyLoan$Quarterly)


(Grow <- quarterlyLoan %>% 
    group_by(Quarterly) %>% 
    summarise(
        startingLoan = sum(starting, na.rm = TRUE), 
        totalPaid    = sum(total)
    ) %>% 
    mutate(
        remainingDebt  = startingLoan - totalPaid, 
        paidPercentage = totalPaid/startingLoan * 100,
        remainingLoan  = 100 - paidPercentage, 
        paidGrowth     = (totalPaid/lead(totalPaid)-1)*100
    ))

mean(Grow$paidPercentage,na.rm = TRUE)

#preparation

loanGrowth <- quarterlyLoan %>% 
    group_by(Quarterly) %>% 
    summarise(
        startingLoan = sum(starting, na.rm = TRUE), 
        totalPaid    = sum(total)
    ) %>% 
    melt(id = "Quarterly")

#Visualization 

ggplot(loanGrowth, aes(Quarterly, value, fill = variable)) + 
    geom_area(alpha = 0.8) + 
    geom_label(label = "Only 2.8% loan paid on average", hjust = 0.79, vjust = 0.8, 
               x = as.Date("2018-06-01"), 
               y = 12e9, 
               label.size = 0.35,
               color = "black",
               fill = "white",
               family = "Georgia") +
    scale_y_continuous(labels = paste0("$", c(0,30,60,90,120)), name = "(in Billion)") + 
    
    labs(
        title = "A Debt Crisis?",
        subtitle = "A growing student loan in US from 2016 to 2018",
        caption = "https://github.com/ochiwar"
    ) + 
    
    theme(
        text = element_text(size = 12, family = "Georgia"),
        panel.background = element_rect(fill = "#f9eee4"),
        plot.background = element_rect(fill = "#f9eee4"),
        legend.background = element_rect(fill = "#f9eee4"),
        panel.grid.major.y = element_line(color = "grey80"),
        line = element_blank(),
        plot.title = element_text(face = "bold", size = 17, margin = margin(10, 0, 5, 0)),
        plot.subtitle = element_text(face = "italic", size = 13,color ="black", margin = margin(0, 0, 45, 0)),
        plot.caption = element_text(face = "italic", hjust = 0.2, vjust = 0, size = 8), 
        axis.ticks.length = unit(0, "cm"),
        axis.title.x = element_blank(), 
        legend.key = element_rect(color = "#f9eee4", size = 0),
        legend.text = element_text(size = 10),
        legend.justification = "top"
    ) + 
    scale_fill_discrete(name = "", labels = c("Outstanding Loan", "Loan Paid"))

ggplot2::ggsave("Student Loan.png", last_plot())

summary(quarterlyLoan)
studentLoan
