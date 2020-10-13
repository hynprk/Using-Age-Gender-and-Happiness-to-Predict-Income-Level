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
