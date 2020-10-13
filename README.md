# STA304-PS2
STA304 PS2
Trying something new 

```{r, echo = FALSE, fig.align='center'}
library(tidyverse)
library(survey)

# Data Cleaning
gss <- read.csv("gss.csv")
gss <- gss %>% select(age, sex, province, income_respondent, education)

# Removing NA entries
sum(is.na(education)) # 341; other 4 variables don't have NA entries
gss <- gss %>% filter(!is.na(education))

# Number of females and males
sum(gss$sex=="Female") # 11203
sum(gss$sex=="Male") # 9399

# Make data into tibble
gss <- tibble(gss)

# Male == 0, Female == 1
gss <- gss %>% mutate(sex = ifelse(sex == "Female", 1, 0))

# Income Range into integers
# Less than $25,000 == 0
# $25,000 to $49,999 == 1
# $50,000 to $74,999 == 2
# $75,000 to $99,999 == 3
# $100,000 to 124,999 == 4
# $125,000 and more == 5
gss <- gss %>% mutate(income_respondent = ifelse(income_respondent == "Less than $25,000", 0, 
ifelse(income_respondent == "$25,000 to $49,999", 1, 
ifelse(income_respondent == "$50,000 to $74,999", 2, 
ifelse(income_respondent == "$75,000 to $99,999", 3, 
ifelse(income_respondent == "$100,000 to $ 124,999", 4, 5))))))


# Regression Estimation 
n <- as.numeric(length(gss$sex))
N <- 30302287
fpc.gss <- rep(N, n) # finite population correction
gss.design <- svydesign(id = ~1, data = gss, fpc = fpc.gss)
gss.reg <- svyglm(income_respondent ~ age + as.factor(sex), gss.design) # logit function cannot be applicable because outcome (y) is not binary
summary(gss.reg)
```

```{r, include=FALSE, echo=FALSE, message=FALSE}

#here's my try to clean the data (Pam)

#Load worksheet
gssdata <- tibble(read_csv("gss.csv"))

#Filter for data of interest
#Looking back at the data, I used region rather than province because it's only 5 var instead of 10
#We can change it if anything
gssdata <- 
  gssdata %>%
  select(age,sex,region,income_respondent,education,average_hours_worked)

#Not considering people who aren't full time
gssdata <-
  gssdata %>%
  filter(!average_hours_worked=="0.1 to 29.9 hours")

gssdata<-
  gssdata %>%
  filter(!average_hours_worked=="Don't know")

gssdata <- na.omit(gssdata)

#Changing income and sex to dummy var 
gssdata <-
  gssdata %>%
  mutate(income_respondent = 
           case_when(income_respondent=="Less than $25,000"~1,
                     income_respondent=="$25,000 to $49,999"~2,
                     income_respondent=="$50,000 to $74,999"~3,
                     income_respondent=="$75,000 to $99,999"~4,
                     income_respondent=="$100,000 to $ 124,999"~5,
                     income_respondent=="$125,000 and more"~6))

gssdata<-
  gssdata %>%
  mutate(sex = 
           case_when(sex=="Male"~1,
                     sex=="Female"~0))

n=length(gssdata$age)
N=30302287

#If we wanted to compare the how the gender wage gap differs between regions
gss_QC <- 
  gssdata %>%
  filter(region == "Quebec")

gss_BC <-
  gssdata %>%
  filter(region == "British Columbia")

gss_PR <-
  gssdata %>%
  filter(region == "Prairie region")

gss_ON <-
  gssdata %>%
  filter(region == "Ontario")

gss_AT <-
  gssdata %>%
  filter(region == "Atlantic region")

```



