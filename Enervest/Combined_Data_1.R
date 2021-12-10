library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(RcppRoll)

# Download Excel Data ----

daily_prod <-  read_xlsx("C:/Users/ochiw/Dropbox/Enervest Project/AC_DAILY2.xlsx")
master_table <-  read_xlsx("C:/Users/ochiw/Dropbox/Enervest Project/DBS_ARKLATEX.xlsx")




sub_master_table_tbl <- master_table %>% 
    select(PROPNUM, API, LEASE_WELLNAME, MAJOR, LAT_SHL, LONG_SHL, DIRECTION, OPERATOR)

sub_master_table_tbl

# Combine tables using left join ---- 

combined_plot_tbl <- daily_prod %>% 
    left_join(sub_master_table_tbl) %>% 
    glimpse()

combined_plot_tbl %>% 
    group_by(LEASE_WELLNAME) %>% 
    summarise(cum_gas = sum(GAS), cum_oil = sum(OIL)) %>% 
    ungroup() %>% 
    arrange(desc(cum_gas, cum_oil))

combined_plot_tbl %>% 
    
    # Selecting columns and adding a year column
    
    select(LEASE_WELLNAME, D_DATE, GAS, OIL) %>% 
    mutate(year = year(D_DATE)) %>%  
    filter(year == "2018") %>% 
    group_by(year, LEASE_WELLNAME) %>% 
    summarise(cum_gas = sum(GAS), cum_oil = sum(OIL)) %>% 
    ungroup() %>% 
    
    ggplot(aes(cum_gas)) +
    theme_minimal()+
    geom_histogram(color = "white", fill = "red") + 
    scale_x_continuous(labels = scales::number_format(scale = 1e-3, suffix = "MM"), limits = c(0,300000)) +
    scale_y_continuous(limits = c(0,350))+
    theme(
        text = element_text(face = "bold", family = "Corbel")) 

   
combined_plot_tbl %>% 
    
    # Selecting columns and adding a year column
    
    select(LEASE_WELLNAME, D_DATE, GAS, OIL) %>% 
    mutate(year = year(D_DATE)) %>%  
    filter(year == "2018") %>% 
    group_by(year, LEASE_WELLNAME) %>% 
    summarise(cum_gas = sum(GAS), cum_oil = sum(OIL)) %>% 
    ungroup() %>% 
    
    ggplot(aes(cum_oil)) +
    geom_histogram(color = "white", fill = "dark green")+  
    scale_x_continuous(limits = c(0,15000)) +
    scale_y_continuous(limits = c(0,150))

combined_plot_tbl %>% 
    
    select(LEASE_WELLNAME, D_DATE, GAS, OIL) %>% 
    group_by(LEASE_WELLNAME) %>% 
    summarise(roll_sum_oil = max(cumsum(OIL)),
           roll_sum_gas = max(cumsum(GAS))) %>% 
    ungroup() %>% 

    
    ggplot(aes(D_DATE, cum_gas)) +
    
    geom_line()
    

combined_plot_tbl %>% 
    
    select(LEASE_WELLNAME,D_DATE, GAS, OIL, WATER, FTP) %>% 
    mutate(D_DATE = ymd(D_DATE),
           cum_oil = cumsum(OIL)
           ) %>% 
    filter(LEASE_WELLNAME == "AGAN 1") %>% 

    ggplot(aes(D_DATE, cum_oil, color = LEASE_WELLNAME))+
    geom_line(color = "dark green")+ 
    scale_x_date(date_breaks ="1 month", date_labels = "%m-%y")

   fs::dir_create("Enervest/combined_data")
   
   combined_plot_tbl %>% 
       write_excel_csv("Enervest/combined_data.csv")
   
   
    length(which(master_table$OP_REP == "N"))
    
    # Vintage Age of Wells ---- 
    
    master_table %>% 
        select(SPUD_DATE) %>% 
        mutate(year = year(SPUD_DATE)) %>% 
        ggplot(aes(year)) + 
        geom_histogram(color = "white", binwidth = 5) + 
        theme_light() + 
        labs(
            x = "",
            y = "Number of Wells Drilled",
            title = "Vintage age of wells",
            subtitle = "Most wells drilled between 2005 and 2015"
        )+
        theme(
            text = element_text(face = "bold", family = "Corbel"))
    
    
    g <- daily_prod %>% 
        select(D_DATE, GAS, OIL) %>% 
        mutate(D_DATE = ymd(D_DATE)) %>% 
        mutate(month = month.name[month(D_DATE)]) %>% 
        
        group_by(month) %>% 
        summarise(cum_oil = sum(OIL, na.rm = TRUE),
                  cum_gas = sum(GAS, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(month = as.factor(month)) 
    
    # g$month <- factor(g$month, levels = g$month[order(g$cum_oil,decreasing = TRUE)])
    
    g %>% 
        ggplot(aes(month,cum_oil))+
        geom_line()+
        geom_point(size = 3) +
        theme_light()+
        scale_y_continuous(limits = c(0,100000))
        
        
    g %>%
    
        ggplot(aes(month,cum_gas))+
        geom_point(size = 3)+
        theme_light()+
        scale_y_continuous(labels = scales::comma_format())+
        labs(
            y = "cummulative gas",
            title = "Monthly Cummulative Gas Production"
        ) +
        theme(
            text = element_text(face = "bold", family = "Corbel"))
        
    # Number of Vertical and Horizontal Wells ----    
    
    length(which(master_table$DIRECTION!="VERT"))
    
    mean(master_table$TOTAL_DEPTH_TVD, na.rm = TRUE)
    
   # Count the number of counties in Data ----
     master_table %>% 
        count(COUNTY) %>% 
        arrange(desc(n))
    # Count the number of partner operators ----
    master_table %>% 
        count(OPERATOR) %>% 
        arrange(desc(n))
    
    combined_plot_tbl %>% 
        select(D_DATE, LEASE_WELLNAME, GAS, OIL) %>% 
        filter(LEASE_WELLNAME=="ROGERS 61-4") %>% 
        mutate(D_DATE = ymd(D_DATE)) %>% 
        
        ggplot(aes(D_DATE, GAS, color = "dark red")) +
        geom_point(size = 3) + 
        theme_light() +
        labs(
            x = " ",
            title = "Production Profile for ROGERS 61-4",
            subtitle = " Well produces on Gas Lift"
            )+
        theme(
            text = element_text(face = "bold", family = "Corbel", size = 15),
            legend.position = "none"
            )
    
    
    combined_plot_tbl %>%
        select(D_DATE, LEASE_WELLNAME, GAS, OIL) %>% 
        filter(LEASE_WELLNAME =="MORNING STAR 1H-18") %>% 
        mutate(D_DATE = ymd(D_DATE)) %>% 
        
        ggplot(aes(D_DATE, GAS, color = "dark red")) +
        geom_point(size = 3) + 
        scdale_y_continuous(limits = c(0,500))+
        theme_light() +
        labs(
            x = " ",
            title = "Production Profile for MORNING STAR 1H-18",
            subtitle = " Well flows naturally"
        )+
        theme(
            text = element_text(face = "bold", family = "Corbel", size = 15),
            legend.position = "none"
        )
        
        combined_plot_tbl %>% 
            select(GAS, DIRECTION) %>% 
            group_by(DIRECTION) %>% 
            summarise(cum_gas = sum(GAS, na.rm = TRUE))
        
        enervest_daily_prod_tbl <- combined_plot_tbl %>% 
            filter(OPERATOR == "ENERVEST OPERATING LLC") %>% 
            select(D_DATE, GAS, OIL, FTP, CSG, CHK, LEASE_WELLNAME) %>% 
            group_by(D_DATE) %>% 
            summarise(cum_gas = sum(GAS), cum_oil = sum(OIL)) %>% 
            ungroup()
            
        enervest_daily_prod_tbl %>% 
            ggplot() + 
            geom_line(aes(x = D_DATE, y = cum_gas, color = "blue")) + 
            geom_line(aes(x = D_DATE, y = cum_oil, color = "green")) +
            # scale_y_continuous(sec.axis = sec_axis(trans = sum(OIL, na.rm = TRUE), name = "cum_oil")) +
            theme_minimal()
    