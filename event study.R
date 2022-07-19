#Install package
install.packages("tidyverse")
library(tidyverse)

#Load data
repurchase_event <- read.csv("C:/LINH/EVENT STUDY/repurchase_event.csv")
file = repurchase_event

#Check type of dates
typeof(file$date)
typeof(file$event_date)

######################## A misunderstanding method
#Transform character to date
file$date       = as.Date(file$date, format = "%m/%d/%Y")
file$event_date = as.Date(file$event_date, format = "%m/%d/%Y")

#Create windows
file = file %>% 
  group_by(gvkey) %>% 
  mutate(
    gap_date               = as.vector(date-event_date),
    event_window_date      = ifelse(gap_date >= -3 & gap_date <= 3, 1, 0),
    estimation_window_date = ifelse(gap_date >= -120 & gap_date <= -21, 1, 0)
  )

######################## Right way
#Create index (event_date can be weekend)
file <- file %>% 
  group_by(gvkey) %>% 
  mutate(
    index       = 1:length(gvkey),
    event_index = ifelse(date == event_date, index, 0),
    event_index = max(event_index)
  )
#Create windows
file <- file %>% 
  group_by(gvkey) %>% 
  mutate(
    gap_index         = index - event_index, 
    event_window      = ifelse(gap_index >= -3 & gap_index <= 3, 1, 0),
    estimation_window = ifelse(gap_index >= -120 & gap_index <= -21, 1, 0)
  )
#Do you see the difference between gap_date vs. gap_index

#CAR calculation 
?file
est_reg = file[which(file$estimation_window==1), ]
reg = lm(est_reg$ret~est_reg$market_return)
  #STEP1
  #Run regression #https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r
install.packages("plyr")
library(plyr)
?dlply
models = dlply(est_reg, "gvkey", function(df) 
  lm(ret~market_return, data = df))
  #Apply coef to each model and return a data frame
coef_reg = ldply(models, coef)
  #Rename coef into alpha, beta
coef_reg$alpha = coef_reg$`(Intercept)`
coef_reg$beta  = coef_reg$market_return
coef_reg       = subset(coef_reg, select = -c(2,3))
  #Print the summary of each model
#l_ply(models, summary, .print = TRUE)
#?predict
#models
  #Merge coefficients into main data "file"
final = file %>%
  left_join(coef_reg, by = "gvkey") 

  #STEP2
  #Calculate expected return
final = final %>% 
  mutate(
  expected_ret = ifelse(event_window == 1, alpha + beta*market_return, NA)
  )
  
  #STEP3
  #Calculate Abnormal return
final$ab_ret = final$ret - final$expected_ret                    
    
  #STEP4
  #Calculate Cumulative abnormal return
require(data.table)
final = data.table(final)
final = final[, CAR := cumsum(ab_ret), by=list(gvkey, event_window)]

#Significance test (filter by gap = 3)
event_only = final[which(final$event_window==1), ]
file2      = event_only[ , max(CAR), by = gvkey]
t.test(file2$V1)

#Draw graph
file3 = final %>%
  select(gvkey, CAR, event_window, gap_index) %>% 
  filter(event_window==1) 
library(ggplot2)
library(lubridate)
file4 = file3[ , mean(CAR), by = gap_index]
  
  #Use file3 to draw frequency graph
ggplot(data = file3, aes(x=CAR)) + geom_histogram()
  #Use file4 to draw line graph
ggplot(data = file4, aes(x=gap_index, y=V1)) + geom_line()
  #V1 = CAR
#End.