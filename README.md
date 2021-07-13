# IBM_R_WEEK3_NOTE
##IBM Week3##
#############

##Descriptive Statistics
#Describe the basic featrues of dataset. 

head(airline )
library(tidyverse)

summary_airline = airline%>% 
                group_by(Reporting_Airline)%>% 
                summarize(mean = mean(ArrDelayMinutes,na.rm = TRUE),
                          Std_dev = sd(ArrDelayMinutes,na.rm = TRUE))
summary_airline

View(summary_airline)

##count 

airline%>% 
          count( Reporting_Airline)
#Boxplot - showing outlier and range of our dataset
#scatterPlot

##grouping data 

ave_delays = airline%>% 
                    group_by(Reporting_Airline,DayOfWeek)%>%
                    summarize(mean_delay = mean(ArrDelayMinutes,na.rm=TRUE))
desc_ave_delays = ave_delays%>%
            arrange(desc(mean_delay))
desc_ave_delays

##ANOVA

aa_as_subset = airline%>% 
                select(ArrDelay,Reporting_Airline)%>% 
                filter(Reporting_Airline == "AA" | Reporting_Airline == "AS")
aa_as_subset
anova_aa_as = aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
summary(anova_aa_as)


##correlation 

ggplot(airline,
       aes(DepDelayMinutes,
           ArrDelayMinutes))+
      geom_point(na.rm) + 
      geom_smooth(method = "lm") 

lm(DepDelayMinutes~ArrDelayMinutes, data =airline )

airline_rm_na%>% 
          select(DepDelay,ArrDelay)%>%
          cor(method = "pearson")

airline_rm_na = airline%>% 
                  replace_na(list(DepDelay = mean(airline$DepDelay,na.rm =TRUE),
                             ArrDelay = mean(airline$ArrDelay,na.rm = TRUE)))

airline_rm_na%>% 
              cor.test(~DepDelay+ArrDelay,data = .)

#correlation_ matrix 
install.packages("Hmisc")
library(Hmisc)
numerical_airline = airline%>% 
                    select(ArrDelayMinutes,DepDelayMinutes,
                           CarrierDelay,WeatherDelay,SecurityDelay)
airline_cor = rcorr(as.matrix(numerical_airline),
                    type = "pearson")
airline_cor
