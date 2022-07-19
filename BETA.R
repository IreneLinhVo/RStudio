#PROBLEM 1: Run regression for 1 firm
  #Import dataset
library(tidyverse)
library(plyr)
library(dplyr)
library(haven)
library(data.table)
rm(File_mergerd)
  #ret
msf_20112015 = read_sas("C:/LINH/SES 1/INVESTMENTS/TD/SAS/msf_20112015.sas7bdat", NULL)
  #vwretd
msi_20112015 = read_sas("C:/LINH/SES 1/INVESTMENTS/TD/SAS/msi_20112015.sas7bdat", NULL)
  #Merge
Data1 = merge(msf_20112015, msi_20112015, 
              by.x = c('DATE'),
              by.y = c('DATE'))

  #Run regression for 1 firm
test = Data1[which(Data1$CUSIP=="88160R10"), ]
reg = lm (RET~vwretd, data = test)
summary(reg)

#Extension:  Run regression for all firms

  #Remove missing values
Data1 = Data1 %>% 
  filter(!is.na(RET), !is.infinite(vwretd))

  #Run regression for different firm
models = dlply(Data1, "CUSIP", function(df) 
  lm(RET~vwretd, data = df))
  #Apply coef to each model and return a data frame
coef_reg = ldply(models, coef)
  #Rename coef into alpha, beta
coef_reg = coef_reg %>% 
  mutate(
    Intercept = coef_reg$`(Intercept)`,
    beta      = coef_reg$vwretd
  )
coef_reg       = subset(coef_reg, select = -c(2,3))

#PROBLEM 2: SHARPE RATIO
  #Calculate mean(RET), sd(RET)
library(data.table) 
Data2 = setDT(Data1)[, list(mean_ret = mean(RET,na.rm=TRUE), sd_ret = sd(RET,na.rm=TRUE)), 
                     by = c("CUSIP")]
File_merged = merge(Data1, Data2, 
                    by.x = c('CUSIP'),
                    by.y = c('CUSIP'))
  #Anuualized mean and standard deviation of RET
File_merged = File_merged %>% 
  mutate(
    ann_mean_ret = mean_ret*12,
    ann_sd_ret   = sd_ret*sqrt(12)
  )
File_merged = File_merged %>% 
  mutate(
    Year  = year(DATE),
    Month = month(DATE)
  )
#Risk free rate
  #Download and import FF 3 factors/ 5 factors model
library(readxl)
F_F_Research_Data_Factors = read_excel("C:/LINH/THESIS/F-F_Research_Data_Factors_CSV/F-F_Research_Data_Factors.xlsx")
  #Merge Data1 with FF. Change type of month and year
F_F_Research_Data_Factors$Year  = as.numeric(F_F_Research_Data_Factors$Year)
F_F_Research_Data_Factors$Month = as.numeric(F_F_Research_Data_Factors$Month)
File_merged = merge(File_merged, F_F_Research_Data_Factors, 
                    by.x = c('Year', 'Month'),
                    by.y = c('Year', 'Month'))
  #Sharpe ratio
File_merged = File_merged %>% 
  mutate(SR = (ann_mean_ret - RF)/ann_sd_ret)

#PROBLEM 3: BETA
#Create file to test
library(dplyr)
target <- c("10001", "10002", "10025")
test = filter(File_merged, KYPERMNO %in% target)

#Command
date_rolling   = character()
beta_rolling   = numeric()
PERMNO_rolling = numeric()

for (i in 1:nrow(test)) {
  PERMNO_rolling[i] = test$KYPERMNO[i]
  date_i = test$MCALDT[i]
  date_rolling = append(date_rolling, date_i)
  train = subset(test, date_i > MCALDT & date_i < MCALDT + 60*30) 
  if (nrow(train)>=36)
  {beta_rolling = append(beta_rolling,lm (MRET~Market_RET, data = train)$coef['Market_RET'])}
  else {beta_rolling = append(beta_rolling,0)}
  
}

rolling_data = data.frame(PERMNO_rolling, date_rolling, beta_rolling)

#------------------------------------------------------#

date_rolling = character()
beta_rolling = numeric()

for (i in 1:nrow(File_merged)) {
  date_i = File_merged$MCALDT[i]
  date_rolling = append(date_rolling,date_i)
  train = subset(File_merged, date_i > MCALDT & date_i < MCALDT + 60*30) 
  if (nrow(train)>=36)
  {beta_rolling = append(beta_rolling,lm (MRET~Market_RET, data = train)$coef['Market_RET'])}
  else {beta_rolling = append(beta_rolling,0)}
  
}

rolling_data = data.frame(date_rolling, beta_rolling)
