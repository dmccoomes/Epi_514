# EPI 514 SSB research project
# Group 17: SSBs and Sleep
# Matt Goldberg, David Coomes, John Schoof
# Created 5/1/2020
# Last edited: 5/4/20 JS


## Read in Data and Packages From Matt's Computer## 
##########################################################################  

# load librarys
  #library(uwIntroStats)
  library(epiR)
  library(plyr)
  library(haven)
  library(dplyr)
  library(foreign)


## Read in Data from John's Computer## 
########################################################################## 
# Clear all variables and set working directory
  rm(list=ls())
  setwd("C:/Users/jscho/OneDrive - UW/SPRING 2020-LAPTOP-7K6NFTGB/EPI 514/")
  
# import as foreign
  #brfss2016 <- read_xpt("LLCP2016XPT/LLCP2016.XPT")
  #write.csv(brfss2016, "brfss2016.csv")
  
  SSB_clean <- read.csv("brfss2016.csv")
  
  SSB_clean <- SSB_clean[SSB_clean$X_STATE %in% c(10, 18, 19, 28, 34, 36, 39, 48, 54), ]
  
  glimpse(SSB_clean)
  
  # Keep only variables of interest
  SSB_clean <- SSB_clean[, c("X_STATE", "X_PSU", "X_STSTR", "X_LLCPWT", "SEX", 
                             "X_AGE_G", "SSBSUGR2", "SSBFRUT2", "SLEPTIM1", 
                             "X_INCOMG", "X_EDUCAG", "EXERANY2", "MENTHLTH", 
                             "DIABETE3", "X_BMI5CAT", "X_SMOKER3", "AVEDRNK2", 
                             "ALCDAY5", "DRINKANY5", "X_RFDRHV5", "CVDSTRK3", "CHCCOPD1", "ASTHMA3")]
  summary(SSB_clean)
  glimpse(SSB_clean) # 100,606 observations 
  

## Read in Data From Matt's Computer## 
##########################################################################  


# Clear all variables and set working directory
  rm(list=ls())
  #setwd("/Users/msgoldberg/Desktop/Grad_School/Year_1/Q3/EPI514/SSB_Paper/")

    #David's system
  if (Sys.getenv("USER") == "david") {
    setwd("/Users/david/Desktop/Dave/school/Epi 514/Final Project/Analysis/Data/")
  }
  
  #Matt's system
  if (Sys.getenv("USER") == "msgoldberg") {
    setwd("/Users/msgoldberg/Desktop/Grad_School/Year_1/Q3/EPI514/SSB_Paper/")
  }
  
  
# Read in SAS files from website
  brfss_16 = read.xport("LLCP2016.XPT ")
  brfss_16_v2 = read.xport("LLCP16V2.XPT")

# Subsetting each dataset by the 9 states that completed the SSB module
  # In core set, this includes Delaware, Indiana, Iowa, Mississippi, New Jersy, New York, West Virginia
    SSB_sub <-brfss_16[brfss_16$X_STATE %in% c(10, 18, 19, 28, 34, 36, 54), ]
  # In V2 Set, this includes Ohio and Texas
    SSB_sub_v2 <-brfss_16_v2[brfss_16_v2$X_STATE %in% c(39, 48), ]

# Rename weight variable in both subsets
  SSB_sub$finalwt <-SSB_sub$X_LLCPWT
  SSB_sub_v2$finalwt <-SSB_sub_v2$X_LLCPWT

# Combine the 2 subsetted datasets into our working dataframe
  SSB_clean <-rbind.fill(SSB_sub,SSB_sub_v2)
  # summary(SSB_clean)
  # View(SSB_clean)

  
# Keep only variables of interest
  SSB_clean <- SSB_clean[, c("X_STATE", "X_PSU", "X_STSTR", "X_LLCPWT", "SEX", 
                             "X_AGEG5YR", "X_AGE_G", "SSBSUGR2", "SSBFRUT2", "SLEPTIM1",
                             "X_INCOMG", "X_EDUCAG", "EXERANY2", "MENTHLTH",
                             "DIABETE3", "X_BMI5CAT", "X_SMOKER3", "AVEDRNK2", 
                             "ALCDAY5", "DRINKANY5", "X_RFDRHV5", "CVDSTRK3", "CHCCOPD1", "ASTHMA3")]
  # summary(SSB_clean)
  
# 

  
## Recode and Label Variables ## 
##########################################################################  
  
## Exclusion Criteria

  # STROKE
    SSB_clean$CVDSTRK3[SSB_clean$CVDSTRK3 == 7 | SSB_clean$CVDSTRK3 == 9] <- NA

    # creation of new dichotomous stroke variable
    # 0 = no history of stroke
    # 1 = has suffered a stroke
    SSB_clean$stroke[SSB_clean$CVDSTRK3 == 1] <- 1  
    SSB_clean$stroke[SSB_clean$CVDSTRK3 == 2] <- 0
    
    SSB_clean$stroke <- factor(SSB_clean$stroke,
                           levels = c(1, 0),
                           labels = c("Has suffered a stroke", 
                                      "No history of Stroke"))
    
    #tabulate(SSB_clean$CVDSTRK3)
    table(SSB_clean$stroke, useNA = "always")

    
  # COPD
    SSB_clean$CHCCOPD1[SSB_clean$CHCCOPD1 == 7 | SSB_clean$CHCCOPD1 == 9] <- NA
    
    #creation of new dichotomous COPD variable
    #0 = no history of stroke
    #1 = has suffered a stroke
    SSB_clean$copd[SSB_clean$CHCCOPD1 == 1] <- 1
    SSB_clean$copd[SSB_clean$CHCCOPD1 == 2] <- 0
    
    SSB_clean$copd <- factor(SSB_clean$copd,
                           levels = c(1, 0),
                           labels = c("Has COPD", "No history of COPD"))
    
    #tabulate(SSB_clean$CHCCOPD1)
    table(SSB_clean$copd, useNA = "always")

    
  # ASTHMA
    SSB_clean$ASTHMA3[SSB_clean$ASTHMA3 == 7 | SSB_clean$ASTHMA3 == 9] <- NA
    
    #creation of new dichotomous asthma variable
    #0 = no history of stroke
    #1 = has suffered a stroke
    SSB_clean$asthma[SSB_clean$ASTHMA3 == 1] <- 1
    SSB_clean$asthma[SSB_clean$ASTHMA3 == 2] <- 0
    
    SSB_clean$asthma <- factor(SSB_clean$asthma,
                           levels = c(1, 0),
                           labels = c("Has asthma", "No history of asthma"))
    
    #tabulate(SSB_clean$ASTHMA3)
    table(SSB_clean$asthma, useNA = "always")
    

## Covariates

  # SEX
    SSB_clean$SEX[SSB_clean$SEX == 9] <- NA
    
    SSB_clean$sex_01[SSB_clean$SEX ==2] <- 1
    SSB_clean$sex_01[SSB_clean$SEX ==1] <- 0

    SSB_clean$sex_01 <- factor(SSB_clean$sex_01,
                           levels = c(0, 1),
                           labels = c("Male", "Female"))
    
    #tabulate(SSB_clean$SEX)
    table(SSB_clean$sex_01, useNA = "always")
    
    
  # INCOME
    SSB_clean$X_INCOMG[SSB_clean$X_INCOMG == 9] <- NA
    
    SSB_clean$income_cat <- factor(SSB_clean$X_INCOMG,
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("<15k", "15k-24,999", "25k-34,999", 
                                          "35k-49,999", "50k+"))
    
    #tabulate(SSB_clean$X_INCOMG)
    table(SSB_clean$income_cat, useNA = "always")
    
    
  # AGE - 10 year categories
      SSB_clean$age_cat <- factor(SSB_clean$X_AGE_G,
                           levels = c(1, 2, 3, 4, 5, 6), 
                           labels=c("Age 18-24", "Age 25-34", "Age 35-44", 
                                    "Age 45-54", "Age 55-64", "Age 65+"))
    
    #tabulate(SSB_clean$X_AGEG5YR)
    table(SSB_clean$age_cat, useNA = "always")
    
    
  # EDUCATION
    SSB_clean$X_EDUCAG[SSB_clean$X_EDUCAG == 9] <- NA
    
    SSB_clean$education <- factor(SSB_clean$X_EDUCAG,
                             levels = c(1, 2, 3, 4),
                             labels = c("Did not Graduate High School", 
                                        "Graduated High School", 
                                        "Attended College/Technical School", 
                                        "Graduated College/Technical School"))
                      
    #tabulate(SSB_clean$X_EDUCAG)
    table(SSB_clean$education, useNA = "always")

    
  # EXERCISE/PHYSICAL ACTIVITY
    SSB_clean$EXERANY2[SSB_clean$EXERANY2 == 9 | SSB_clean$EXERANY2 == 7] <- NA
   
    SSB_clean$exercise_01[SSB_clean$EXERANY2 ==1] <- 1
    SSB_clean$exercise_01[SSB_clean$EXERANY2 ==2] <- 0
    
    SSB_clean$exercise_01 <- factor(SSB_clean$exercise_01,
                           levels = c(1, 0),
                           labels = c("Yes", "No"))
    
    #tabulate(SSB_clean$EXERANY2)
    table(SSB_clean$exercise_01, useNA = "always")
    

  # MENTAL HEALTH
    SSB_clean$MENTHLTH[SSB_clean$MENTHLTH == 77 | SSB_clean$MENTHLTH == 99] <- NA
    SSB_clean$MENTHLTH[SSB_clean$MENTHLTH == 88] <- 0
    
    #create new mental health categorical variable
      #0 = 0 days of poor mental health
      #1= 1-10 days of poor mental health
      #2 = 11+ days of poor mental health
    SSB_clean$mental[SSB_clean$MENTHLTH == 0] <- 0
    SSB_clean$mental[SSB_clean$MENTHLTH>0 & SSB_clean$MENTHLTH<=14] <- 1
    SSB_clean$mental[SSB_clean$MENTHLTH >=14] <- 2
    
    SSB_clean$mental <- factor(SSB_clean$mental,
                         levels = c(0, 1, 2),
                         labels = c("No Days of Poor Mental Health", 
                                    "1-13 Days of Poor Mental Health", 
                                    "14+ Days of Poor Mental Health"))
    
    #DMC - recoding for 1-13 and 14+
    SSB_clean$mental <- as.numeric(SSB_clean$mental)
    SSB_clean$mental[SSB_clean$mental==2] <- 0
    SSB_clean$mental[SSB_clean$mental==1] <- 0
    SSB_clean$mental[SSB_clean$mental==3] <- 1
    
    
    SSB_clean$mental <- factor(SSB_clean$mental,
                                levels=c(0,1),
                                labels=c("0-13 Days of Poor Mental Health", "14+ Days of Poor Mental Health"))
    
    
    #tabulate(SSB_clean$mental)
    table(SSB_clean$mental, useNA = "always")
    

  # DIABETES
    SSB_clean$DIABETE3[SSB_clean$DIABETE3 == 7 | SSB_clean$DIABETE3 == 9] <- NA
    
    #creation of new dichotomous diabetes variable
      #0 = non-diabetic OR gestationally diabetic OR pre-diabetic
      #1 = diabetic
    SSB_clean$diabetes_01[SSB_clean$DIABETE3 == 1] <- 1
    SSB_clean$diabetes_01[SSB_clean$DIABETE3 == 2 | SSB_clean$DIABETE3 == 3 | 
                         SSB_clean$DIABETE3 == 4] <- 0
    
    SSB_clean$diabetes_01 <- factor(SSB_clean$diabetes_01,
                           levels = c(1, 0),
                           labels = c("Diabetic", "Non-Diabetic"))
    
    #tabulate(SSB_clean$diabetes)
    table(SSB_clean$diabetes_01, useNA = "always")
    

  # BMI
    SSB_clean$X_BMI5CAT[SSB_clean$X_BMI5CAT == " "] <- NA
    
    #creation of new 3 level categorical BMI variable
      #1 = under/normal weight
      #2= overweight
      #3= obese
    SSB_clean$bmi_cat[SSB_clean$X_BMI5CAT == 1| SSB_clean$X_BMI5CAT == 2] <- 1
    SSB_clean$bmi_cat[SSB_clean$X_BMI5CAT == 3] <- 2
    SSB_clean$bmi_cat[SSB_clean$X_BMI5CAT == 4] <- 3
    
    SSB_clean$bmi_cat <- factor(SSB_clean$bmi_cat,
                       levels = c(1, 2, 3),
                       labels = c("Underweight or Normal Weight", 
                                  "Overweight", "Obese"))
    
    #tabulate(SSB_clean$BMI)
    table(SSB_clean$bmi_cat, useNA = "always")
    

  # SMOKING
    SSB_clean$X_SMOKER3[SSB_clean$X_SMOKER3 == 0] <- NA
    
    #creating new dichotomous smoker/nonsmoker variable
      #0 = Non-smoker (includes never smoked/former smoker)
      #1 = smoker (includes all levels of current use)
    
    SSB_clean$smoking_01[SSB_clean$X_SMOKER3 == 3 | SSB_clean$X_SMOKER3 == 4] <- 0
    SSB_clean$smoking_01[SSB_clean$X_SMOKER3 == 1 | SSB_clean$X_SMOKER3 == 2] <- 1
    
    SSB_clean$smoking_01 <- factor(SSB_clean$smoking_01,
                           levels = c(0, 1),
                           labels = c("Non-Smoker", "Smoker"))
    
    #tabulate(SSB_clean$smoking)
    table(SSB_clean$smoking_01, useNA = "always")
    
    
  # ALCOHOL 
    SSB_clean$AVEDRNK2[SSB_clean$AVEDRNK2 == 77] <- NA
    SSB_clean$AVEDRNK2[SSB_clean$AVEDRNK2 == 99] <- NA
    
    #tabulate(SSB_clean$AVEDRNK2)
    table(SSB_clean$AVEDRNK2, useNA = "always")
    
    #creation of new categorical drinking variable
      #0 = no drinks
      #1 = <7 drinks 
      #2 = 8-20 drinks
      #3 = 21+ drinks
    
    SSB_clean$alcohol_cat[SSB_clean$ALCDAY5==888] <- 0
    SSB_clean$alcohol_cat[SSB_clean$AVEDRNK2 == 0] <- 0
    SSB_clean$alcohol_cat[SSB_clean$AVEDRNK2 >0 & SSB_clean$AVEDRNK2 <=7] <- 1
    SSB_clean$alcohol_cat[SSB_clean$AVEDRNK2 >=8 & SSB_clean$AVEDRNK2 <= 20] <- 2
    SSB_clean$alcohol_cat[SSB_clean$AVEDRNK2 >= 21] <- 3
    
    #tabulate(SSB_clean$alcohol)
    table(SSB_clean$alcohol_cat, useNA = "always")
    
    SSB_clean$alcohol_cat <- factor(SSB_clean$alcohol_cat,
                           levels = c(0, 1, 2, 3),
                           labels = c("0 drinks/wk", "1-7 drinks/wk", 
                                      "8-20 drinks/wk", "21+ drinks/wk"))
                    
    #tabulate(SSB_clean$alcohol)
    table(SSB_clean$alcohol_cat, useNA = "always")
    ###### A LOT OF MISSINGNESS HERE!!!!!!!!
    # 5,366 missing values now
    
    
    #Creating several more alcohol use variables
    # 1. Drinker vs. non-drinker
    SSB_clean$alcohol_yes <- SSB_clean$DRNKANY5
    SSB_clean$alcohol_yes[SSB_clean$alcohol_yes==7] <- NA
    SSB_clean$alcohol_yes[SSB_clean$alcohol_yes==9] <- NA
    SSB_clean$alcohol_yes[SSB_clean$alcohol_yes==2] <- 0

    SSB_clean$alcohol_yes <- factor(SSB_clean$alcohol_yes,
                                    levels = c(0, 1),
                                    labels = c("Non-drinker", "Drinker"))
    
    table(SSB_clean$alcohol_yes, useNA = "always")
    
    
    # 2. Non-drinker, moderate drinker, heavy drinker
    SSB_clean <- SSB_clean %>% mutate(alcohol_hvy = case_when(alcohol_yes==c("Non-drinker") ~ 0,
                                                              alcohol_yes==c("Drinker") & X_RFDRHV5==1 ~ 1,
                                                              X_RFDRHV5==2 ~ 2))
        
    SSB_clean$alcohol_hvy <- factor(SSB_clean$alcohol_hvy,
                                    levels = c(0, 1, 2),
                                    labels = c("Non-drinker", "Moderate drinker", "Heavy drinker"))
    
    table(SSB_clean$alcohol_hvy, useNA = "always")
    #Only 4,463 obs in the heavy drinker category - this may impact our ability to use this covariate
  
      
 ## OUTCOME
    
  # SLEEP
    #tabulate(SSB_clean$SLEPTIM1)
    table(SSB_clean$SLEPTIM1, useNA = "always")
    
    SSB_clean$SLEPTIM1[SSB_clean$SLEPTIM1 == 77 | SSB_clean$SLEPTIM1 == 99] <- NA
    
    #creation of new dichotomous Sleep variable
      #0 = <7 hours of sleep (insufficient sleep)
      #1 = 7+ hours of sleep (sufficient sleep)
    SSB_clean$sleep_01[SSB_clean$SLEPTIM1 <7] <- 1
    SSB_clean$sleep_01[SSB_clean$SLEPTIM1 >= 7] <-0
    
    SSB_clean$sleep_01 <- factor(SSB_clean$sleep_01,
                           levels = c(0, 1),
                           labels = c("7+ Hours of Sleep",
                                      "<7 Hours of Sleep"))
    
    #tabulate(SSB_clean$sleep_01)
    table(SSB_clean$sleep_01, useNA = "always")
    
    # DMC - this wasn't working out well when I ran it, but I don't think we need it becasue it's a duplicate of the one above. 
    # Sleep 1/2 variable
    # SSB_clean$sleep_12 <- 2 - (as.numeric(SSB_clean$sleep_01))
    # 
    # SSB_clean$sleep_12 <- factor(SSB_clean$sleep_12,
    #                        levels = c(2, 1),
    #                        labels = c("<7 Hours of Sleep", 
    #                                   "7+ Hours of Sleep"))
    # 
    # #tabulate(SSB_clean$sleep_12)
    # table(SSB_clean$sleep_12, useNA = "always")
    
    
    
## EXPOSURE
    
  # SSB VARIABLE
    
    SSB_clean$SSBSUGR2[SSB_clean$SSBSUGR2 == 777] <- NA
    SSB_clean$SSBSUGR2[SSB_clean$SSBSUGR2 == 999] <- NA
    SSB_clean$SSBSUGR2[SSB_clean$SSBSUGR2 == 888] <- 0
    
    #tabulate(SSB_clean$SSBSUGR2)
    table(SSB_clean$SSBSUGR2, useNA = "always")
    
    
    SSB_clean$SSBFRUT2[SSB_clean$SSBFRUT2 == 777] <- NA
    SSB_clean$SSBFRUT2[SSB_clean$SSBFRUT2 == 999] <- NA
    SSB_clean$SSBFRUT2[SSB_clean$SSBFRUT2 == 888] <- 0
    
    #tabulate(SSB_clean$SSBFRUT2)
    table(SSB_clean$SSBFRUT2, useNA = "always")
    
    #Creating SODA component of SSB variable
    SSB_clean$soda <- SSB_clean$SSBSUGR2
    SSB_clean <- SSB_clean %>%
      mutate(soda = case_when(soda>100 & soda<200 ~ soda-100,
                              soda>200 & soda<300 ~ (soda-200)/7,
                              soda>300 & soda<400 ~ (soda-300)/30,
                              soda==0 ~ 0)
      )
    
    SSB_clean %>% head()
    summary(SSB_clean$soda)
    
    summary(SSB_clean$soda[SSB_clean$soda>10])
    str(SSB_clean$soda[SSB_clean$soda>10 & !is.na(SSB_clean$soda)])
    #There are 99 observations above 10 sodas per day
    str(SSB_clean$soda[SSB_clean$soda>20 & !is.na(SSB_clean$soda)])
    #There are 36 observations above 20 sodas per day
    hist(SSB_clean$soda[SSB_clean$soda>10])
    
    #Creating JUICE component of SSB variable
    SSB_clean$juice <- SSB_clean$SSBFRUT2
    SSB_clean <- SSB_clean %>%
      mutate(juice = case_when(juice>100 & juice<200 ~ juice-100,
                               juice>200 & juice<300 ~ (juice-200)/7,
                               juice>300 & juice<400 ~ (juice-300)/30,
                               juice==0 ~ 0)
      )
    
    SSB_clean %>% head()
    summary(SSB_clean$juice)
    
    summary(SSB_clean$juice[SSB_clean$juice>10])
    str(SSB_clean$juice[SSB_clean$juice>10 & !is.na(SSB_clean$juice)])
    #There are 59 observations above 10 SSBs per day
    str(SSB_clean$juice[SSB_clean$juice>20 & !is.na(SSB_clean$juice)])
    #There are 24 observations above 20 SSBs per day
    hist(SSB_clean$juice[SSB_clean$juice>10])
    
    
    #Combining SODA and JUICE Variables into SSB variable
    SSB_clean$ssb_daily <- SSB_clean$soda + SSB_clean$juice    
    SSB_clean %>% head()
    summary(SSB_clean$ssb_daily)
    
    str(SSB_clean$ssb_daily[SSB_clean$ssb_daily>10 & !is.na(SSB_clean$ssb_daily)])
    #There are 240 observations above 10 SSBs per day
    str(SSB_clean$ssb_daily[SSB_clean$ssb_daily>20 & !is.na(SSB_clean$ssb_daily)])
    #There are 68 observations above 20 SSBs per day
    hist(SSB_clean$ssb_daily[SSB_clean$ssb_daily>10])
    
    
    # Create new combined categorical SSB variable
      # 0 = <1 per day
      # 1 = 1-1.99 per day 
      # 2 = 2 or more per day
      SSB_clean$ssb_cat1 <- SSB_clean$ssb_daily
      SSB_clean <- SSB_clean %>% mutate(ssb_cat1 = case_when(ssb_daily<1 ~ 0,
                                                           ssb_daily>=1 & ssb_daily<2 ~ 1,
                                                           ssb_daily>=2 ~ 2))
    
      SSB_clean$ssb_cat1 <- factor(SSB_clean$ssb_cat1,
                           levels = c(0, 1, 2),
                           labels = c("<1 SSB per Day", "[1-2) SSB per Day", 
                                      "2+ SSB per Day"))
    
    #tabulate(SSB_clean$SSBcat)
    table(SSB_clean$ssb_cat1, useNA = "always")
    
    
    # Creating an alternate combined SSB variable
      # 0 = 0 per day
      # 1 = more than 0 but less than 1 per day 
      # 2 = 1 or more per day
      SSB_clean$ssb_cat2 <- SSB_clean$ssb_daily
      SSB_clean <- SSB_clean %>% mutate(ssb_cat2 = case_when(ssb_daily==0 ~ 0,
                                                             ssb_daily>0 & ssb_daily<1 ~ 1,
                                                             ssb_daily>=1 ~ 2))
      
      SSB_clean$ssb_cat2 <- factor(SSB_clean$ssb_cat2,
                                   levels = c(0, 1, 2),
                                   labels = c("0 SSB per Day", "(0-1) SSB per Day", 
                                              "1+ SSB per Day"))
      
      #tabulate(SSB_clean$SSBcat)
      table(SSB_clean$ssb_cat2, useNA = "always")
    
    

## REMOVE OBSERVATIONS BASED ON EXCLUSION CRITERIA ## 
##########################################################################  
     
  # NO COPD PATIENTS
  # NO STROKE PATIENTS
  # NO ASTHMA PATIENTS
    #SSB_clean <-SSB_clean[SSB_clean$copd %in% c(0), ]
    #SSB_clean <-SSB_clean[SSB_clean$asthma %in% c(0), ]
    #SSB_clean <-SSB_clean[SSB_clean$stroke %in% c(0), ]
    
    #View(SSB_clean)
    
    #Stroke, Asthma, and COPD != 1
    
    
    SSB_clean <- SSB_clean %>% filter(as.numeric(copd) != 1)
    
    SSB_clean <- SSB_clean %>% filter(as.numeric(asthma) != 1)
    
    SSB_clean <- SSB_clean %>% filter(as.numeric(stroke) != 1)
                                    
    
    #View(SSB_clean)
    
    
    #removing observations that are missing for exposure or outcome
    
    SSB_clean <- SSB_clean %>% filter(!is.na(ssb_cat1))
    SSB_clean <- SSB_clean %>% filter(!is.na(sleep_01))
    
    
    
    glimpse(SSB_clean)
  # x observations
    
    
## RENAME OTHER VARIABLES ## 
##########################################################################      
  
  # State
    SSB_clean$state <- SSB_clean$X_STATE
    
  # Survey weights
    #SSB_clean$final_weight <- SSB_clean$X_LLCPWT
    SSB_clean$final_weight <- SSB_clean$finalwt
    
    
  # PSU
    SSB_clean$psu <- SSB_clean$X_PSU
    
  # STSTR
    SSB_clean$ststr <- SSB_clean$X_STSTR
    
    
## SAVE FINAL DATASET ## 
##########################################################################  
     
  # Keep only variables of interest
    SSB_master <- SSB_clean[, c("SEQNO", "state", "psu", "ststr", "final_weight", 
                               "sex_01", "age_cat", "SSBSUGR2", "SSBFRUT2",
                               "stroke", "copd", "asthma", "income_cat", 
                               "education", "exercise_01", "mental",
                               "diabetes_01", "bmi_cat", "smoking_01",
                               "alcohol_cat", "alcohol_yes", "alcohol_hvy", "sleep_01",
                               "soda", "juice", "ssb_daily", "ssb_cat1", "ssb_cat2")]
    
    
    glimpse(SSB_master)
    #final number of observations = 57,591
    
  # Saving the dataset as a CSV for analysis steps
    write.csv(SSB_master, "SSB_master.csv")
  # Saving as R file to keep factors
    save(SSB_master, file="SSB_master.RData")


