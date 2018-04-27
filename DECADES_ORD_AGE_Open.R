# Ordenation Age Distribution - Creation of Tables

###Clear Env. in R ----
    rm(list=ls())

###Load libraries ----
    library(dplyr)
    library(readr)
    library(lubridate)
    library(party)
    library(forecast)
    library(plotly)

#Load Data ----
    ac <- read.csv("Not Open Data of the Church",
               na.strings=c("","NA"))
    
#Filter Data ----

    #ac <- ac[is.na(ac$date_of_death), ]
    ac <- ac[!is.na(ac$ord_date), ]
    ac <- ac[!is.na(ac$date_of_birth),]
    #ac <- ac[ac$bw_status == "Active", ]
    #ac <- ac[ac$cler_in_book == "Yes", ]

#With or Without Deacon ----
    
    ac <- ac[ac$perm_deacon == "N", ]

#Create a Field of Ord Age ----

    date_df <- ac %>% 
               dplyr::select(client_number, 
                             age, 
                             date_of_birth,
                             ord_date, 
                             us_region, 
                             geo_diocese, 
                             gender, 
                             geo_province, 
                             place_of_birth)

    date_df$date_of_birth    <- as.Date(ac$date_of_birth, "%Y-%m-%d")
    date_df$year_birth     <- year(date_df$date_of_birth)

    date_df$ord_date     <-  as.Date(ac$ord_date, "%Y-%m-%d")
    date_df$days_ordenation <- as.numeric(date_df$ord_date - date_df$date_of_birth)
    date_df$age_ordenation <- (date_df$days_ordenation/365.25)
    date_df$year_ordanation <- year(date_df$ord_date)

    hist(date_df$age_ordenation, breaks = 50)

#Divide the Data by Census Category ----

    Clergy25_34   <- date_df[date_df$age_ordenation <= 34, ]
    Clergy25_34$CensusGroup <- "25 to 34"
    Clergy35_54   <- date_df[date_df$age_ordenation <= 54 & date_df$age_ordenation > 34, ]
    Clergy35_54$CensusGroup <- "35 to 54"
    Clergy54_More <- date_df[date_df$age_ordenation >= 55, ]
    Clergy54_More$CensusGroup  <- "55 or Higher"
    Clergy_AgeDistribution <- rbind(Clergy25_34, Clergy35_54, Clergy54_More)
    rm(Clergy25_34, Clergy35_54, Clergy54_More, ac)

    Clergy_AgeDistribution <- Clergy_AgeDistribution[!is.na(Clergy_AgeDistribution$age_ordenation), ]

#Group by Year of Ordanation ----
    Year_Ordanation <- Clergy_AgeDistribution %>%
                       group_by(Clergy_AgeDistribution$year_ordanation) %>%
                       summarise (Total = n())

    p <- plot_ly(Year_Ordanation, 
                 x = Year_Ordanation$`Clergy_AgeDistribution$year_ordanation`, 
                 y = ~Total, 
                 name = 'Number of Ordain by Year', 
                 type = 'scatter', 
                 mode = 'lines')
    p

#Make a New Data Frame 
    Generation <- Clergy_AgeDistribution %>% 
                  mutate(Genarations=cut(Clergy_AgeDistribution$year_birth, 
                                         breaks=c(1900, 
                                                  1909, 
                                                  1919, 
                                                  1929,
                                                  1939,
                                                  1949,
                                                  1959,
                                                  1969,
                                                  1979,
                                                  1989,
                                                  1999,
                                                  2009,
                                                  2019), labels=c("1900's",
                                                                  "1910's",
                                                                  "1920's",
                                                                  "1930's",
                                                                  "1940's",
                                                                  "1950's",
                                                                  "1960's",
                                                                  "1970's",
                                                                  "1980's",
                                                                  "1990's",
                                                                  "2000's",
                                                                  "2010's")))
    
    
    
    category_survey <- Generation %>%
                       group_by(Genarations) %>%
                       summarise (Total = n(),
                                  Ordain_in_theirs_20s = length(age_ordenation[age_ordenation <= 29]),
                                  Ordain_in_theirs_30s = length(age_ordenation[age_ordenation > 29 & age_ordenation <= 39]),
                                  Ordain_in_theirs_40s = length(age_ordenation[age_ordenation > 39 & age_ordenation <= 49]),
                                  Ordain_in_theirs_50s = length(age_ordenation[age_ordenation > 49 & age_ordenation <= 59]),
                                  Ordain_in_theirs_60s = length(age_ordenation[age_ordenation > 59 & age_ordenation <= 69]),
                                  Ordain_in_theirs_70s_or_Higher = length(age_ordenation[age_ordenation > 69])) %>%
                                  mutate(Percentage_of_Current_Clergy = scales::percent((Total)/sum(Total)))
    
    
    #Output of the result --
    write.csv(category_survey, file = paste0("~/projects/asoto_repo/Data/Demographic/All_OrdAge_NoneDeacon.csv"))
    
    #Ordination Age by Generation
    graph_df          <- Generation %>%
                         group_by(Genarations, gender) %>%
                         summarise (Total = n(),
                                    Ordain_in_theirs_20 = length(age_ordenation[age_ordenation <= 29]),
                                    Ordain_in_theirs_30 = length(age_ordenation[age_ordenation > 29 & age_ordenation <= 39]),
                                    Ordain_in_theirs_40 = length(age_ordenation[age_ordenation > 39 & age_ordenation <= 49]),
                                    Ordain_in_theirs_50 = length(age_ordenation[age_ordenation > 49 & age_ordenation <= 59]),
                                    Ordain_in_theirs_60_Higher = length(age_ordenation[age_ordenation > 59])) %>%
                         mutate(Percentage_of_20 = scales::percent((Ordain_in_theirs_20/Total))) %>%
                         mutate(Percentage_of_30 = scales::percent((Ordain_in_theirs_30/Total))) %>%
                         mutate(Percentage_of_40 = scales::percent((Ordain_in_theirs_40/Total))) %>%
                         mutate(Percentage_of_50 = scales::percent((Ordain_in_theirs_50/Total))) %>%
                         mutate(Percentage_of_60 = scales::percent((Ordain_in_theirs_60_Higher/Total)))
    
    write.csv(graph_df, file = paste0("~/File_Location/namefile.csv"))
    
    female_df <- graph_df[graph_df$gender == "F", ]
    write.csv(female_df, file = paste0("~/File_Location/namefile.csv"))
    
    male_df <- graph_df[graph_df$gender == "M", ]
    write.csv(male_df, file = paste0("~/File_Location/namefile.csv"))
    
    