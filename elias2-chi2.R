####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################

### Settings + Packages

library(dplyr)

library(psych)

### Load data 
GSS <- read.csv("GSS2022.csv")

####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################

## Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary() 
# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc
# Step 3: Confirm: table() / summary()

############                     DEPENDENT VARIABLE                     ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$nextgen)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, strongly_agree = ifelse(nextgen == 1, 1, 0)) 
GSS <- mutate(GSS, agrees = ifelse(nextgen == 1 | nextgen == 2, 1, 0)) 
GSS <- mutate(GSS, disagrees = ifelse(nextgen == 3 | nextgen == 4, 1, 0)) 
# STEP 3: Confirm creation (if necessary)
table(GSS$nextgen, GSS$strongly_agree)
table(GSS$nextgen, GSS$agrees)
table(GSS$nextgen, GSS$disagrees)

############                  INDEPENDENT VARIABLE 1 (RACE)             ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$race)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, white = ifelse(race == 1, 1, 0)) 
GSS <- mutate(GSS, black = ifelse(race == 2, 1, 0)) 
GSS <- mutate(GSS, other = ifelse(race == 3, 1, 0)) 
# STEP 3: Confirm creation (if necessary)
table(GSS$race, GSS$white)
table(GSS$race, GSS$black)
table(GSS$race, GSS$other)

############                  INDEPENDENT VARIABLE 2 (AGE)              ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$age)
# STEP 2: no recode because quantitative

############                  INDEPENDENT VARIABLE 3 (INCOME)           ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$rincome)
# STEP 2: no recode because quantitative

############                  INDEPENDENT VARIABLE 4 (WEALTH)           ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$wealth)
# STEP 2: no recode because quantitative

############                  INDEPENDENT VARIABLE 5 (LVL OF ED)        ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$educ)
# STEP 2: no recode because quantitative

############                  INDEPENDENT VARIABLE 6 (SEX)              ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$sex)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, male = ifelse(sex == 1, 1, 0)) 
GSS <- mutate(GSS, female = ifelse(sex == 2, 1, 0)) 
# STEP 3: Confirm creation (if necessary)
table(GSS$race, GSS$male)
table(GSS$race, GSS$female)

############                  INDEPENDENT VARIABLE 7 (RELIGION)         ############
############              SUBSTANTIVE VARIABLE NAME HERE                ############

# STEP 1: Examine variable and coding schema 
table(GSS$relig)
# STEP 2: Recode if necessary or justify if not neccesary
GSS <- mutate(GSS, Christian = ifelse(relig == 1 | relig == 2 | relig == 10 | relig == 11, 1, 0))
GSS <- mutate(GSS, Other_relig = ifelse(relig == 3 | relig == 5 | relig == 6 | relig == 7 | relig == 8 | relig == 9 | relig == 12 | relig == 13, 1, 0))
GSS <- mutate(GSS, None = ifelse(relig == 4, 1, 0))
# STEP 3: Confirm creation (if necessary)
table(GSS$relig, GSS$Christian)
table(GSS$relig, GSS$Other_relig)
table(GSS$relig, GSS$None)

####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep
my_varlist <- c("nextgen", "agrees", "disagrees", "race", "white", 
                "black", "other", "age", "rincome", "wealth", "educ", 
                "sex", "male", "female", "relig", "Christian", "Other_relig", "None")
### STEP 2: create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))
### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)
cor(my_dataset)

####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################

# TABLE 1: DESCRIPTIVE STATISTICS HERE

####################################################################################
############        PHASE 4: Contingency Table + Correlation Matrix     ############
####################################################################################

# TABLE 2: CONTINGENCY TABLE HERE

####################################################################################
############        PHASE 5: Logistic Regression Analysis               ############
####################################################################################
# model 1: Ascribed #
model1 <- glm(agrees ~ black + other +
                age +
                male 
              , data = my_dataset, family = binomial)
summary(model1)
# model 2: Achieved #
model2 <- glm(agrees ~ educ +
                rincome +
                wealth
               , data = my_dataset, family = binomial)
summary(model2)
# model 3: Personal #
model3 <- glm(agrees ~ Christian + 
                Other_relig
              , data = my_dataset, family = binomial)
summary(model3)
# model 4: All #
model4 <- glm(agrees ~ black + other +
                 age +
                 male +
                 rincome+
                 educ+
                 wealth+
                 Christian + Other_relig
               , data = my_dataset, family = binomial)
summary(model4)

exp(0.150458)
