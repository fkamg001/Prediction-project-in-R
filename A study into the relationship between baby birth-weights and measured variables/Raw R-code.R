
# load required libraries
library( 'dplyr' )
library( 'knitr' )
library( 'ggplot2' )
library( 'car' )
library( 'Epi ')
library( 'GGally' ) 
library( 'effects' )
library( 'boot' )

# set.seed ensures when we take a random 80% sample for training and 20% for testing 
# of model that we get the same results
set.seed(123)

# options for setting directories
# adjust working directory to suit user.
setwd("D:/sbr_Development/MSc/Semester 1/MT5762/Assignments/#2")	


# load data into R
team.data <- read.table('babies23.data', header=TRUE)
#write.csv( my.data, 'quickLookie.csv' )
#View( team.data )

# add naming conventions to columns to replace numeric counterparts
# here we'll also take the opportunity to generalise all unknowns to NA...
# we can later show those as 'Unknown' if we wish - at this stage however just
# keeps things a little cleaner
team.clean.data <-  team.data %>%
  mutate( 
    sex = case_when( sex == 1 ~ 'Male',
                     sex == 2 ~ 'Female',
                     TRUE ~ NA_character_ ),
    
    race = case_when( race %in% 0:5 ~ 'White',
                      race == 6 ~ 'Mex', 
                      race == 7 ~ 'Black',
                      race == 8 ~ 'Asian',
                      race == 9 ~ 'Mixed',
                      TRUE ~ NA_character_ ),
    
    ed = case_when( ed == 0 ~ 'Less than 8th Grade',
                    ed == 1 ~ '8th - 12th Grade - did not graduate',
                    ed == 2 ~ 'HS graduate - no other schooling',
                    ed == 3 ~ 'HS + trade',
                    ed == 4 ~ 'HS + some College',
                    ed == 5 ~ 'College graduate',
                    ed %in% 6:7  ~ 'Trade school HS unclear',
                    TRUE ~ NA_character_ ),
    
    drace = case_when(  drace %in% 0:5 ~ 'White',
                        drace == 6 ~ 'Mex', 
                        drace == 7 ~ 'Black',
                        drace == 8 ~ 'Asian',
                        drace == 9 ~ 'Mixed',
                        TRUE ~ NA_character_ ),
    
    ded = case_when(  ded == 0 ~ 'Less than 8th Grade',
                      ded == 1 ~ '8th - 12th Grade - did not graduate',
                      ded == 2 ~ 'HS graduate - no other schooling',
                      ded == 3 ~ 'HS + trade',
                      ded == 4 ~ 'HS + some College',
                      ded == 5 ~ 'College graduate',
                      ded %in% 6:7  ~ 'Trade school HS unclear',
                      TRUE ~ NA_character_ ),
    
    marital = case_when(  marital == 1 ~ 'Married',
                          marital == 2 ~ 'Legally Separated', 
                          marital == 3 ~ 'Divorced',
                          marital == 4 ~ 'Widowed',
                          marital == 5 ~ 'Never Married',
                          TRUE ~ NA_character_ ),
    
    inc = case_when(  inc == 1 ~ 'Under 2500',
                      inc == 2 ~ '2500 - 4999', 
                      inc == 3 ~ '5000 - 7499',
                      inc == 4 ~ '7500 - 9999',
                      inc == 5 ~ '10000 - 12499', 
                      inc == 6 ~ '12500 - 14999',
                      inc == 7 ~ '15000 - 17499',
                      inc == 8 ~ '17500 - 19999', 
                      inc == 9 ~ '20000 and over',
                      # 98 is unknown, 99 not asked ~ same thing?
                      TRUE ~ NA_character_ ),
    
    smoke = case_when(  smoke == 0 ~ 'Never',
                        smoke == 1 ~ 'Smokes now', 
                        smoke == 2 ~ 'Until current pregnancy',
                        smoke == 3 ~ 'Once did, not now',
                        TRUE ~ NA_character_ ),
    
    time = case_when( time == 0 ~ 'Never smoked',
                      time == 1 ~ 'Still smokes',
                      time == 2 ~ 'During current pregnancy', 
                      time == 3 ~ 'Within 1 year',
                      time == 4 ~ '1 to 2 years ago',
                      time == 5 ~ '2 to 3 years ago', 
                      time == 6 ~ '3 to 4 years ago',
                      time == 7 ~ '5 to 9 years ago',
                      time == 8 ~ '10+ years ago', 
                      time == 9 ~ 'Quit and dont know',
                      # 98 is unknown, 99 not asked ~ same thing?
                      TRUE ~ NA_character_ ),
    
    number = case_when( number == 0 ~ 'Never',
                        number == 1 ~ '1-4',
                        number == 2 ~ '5-9', 
                        number == 3 ~ '10-14',
                        number == 4 ~ '15-19',
                        number == 5 ~ '20-29', 
                        number == 6 ~ '30-39',
                        number == 7 ~ '40-60',
                        number == 8 ~ '60+', 
                        number == 9 ~ 'Smoke but dont know',
                        # 98 is unknown, 99 not asked ~ same thing?
                        TRUE ~ NA_character_ ),
    
    newY_Groupedwt = case_when( wt <= 108.5 ~ 1,
                                wt <= 120  ~ 2,
                                wt <= 131 ~ 3,
                                # 98 is unknown, 99 not asked ~ same thing?
                                TRUE ~ 4 ),
    
    # lowe birth weight references;
    # https://www.verywellfamily.com/baby-birth-weight-statistics-2633630
    # https://www.babycentre.co.uk/a1033196/low-birth-weight-in-babies
    lowBirthWeight = case_when( wt <= 88 ~ 'Yes',
                                TRUE ~ 'No' ),
    
    mumEverSmoked = case_when( time == 'Never smoked' ~ 'No',
                               TRUE ~ 'Yes' )
    
  ) %>%
  # add m prefix to all columns associated with 'Mother' to clarify 
  rename( 'mparity' = parity, 'mage' = age, 'mwt' = wt.1, 
          'mht' = ht, 'mrace' = race, 'med' = ed, 
          'msmoke' = smoke, 'mtime' = time, 'mnumber' = number ) %>%
  
  # remove msmoke due to collinearity with mtimes & mnumber
  # in addition we can remove other unrequired columns 
  select( -sex, -id, -pluralty, -outcome )


# generalise all 999 unknown's to NA
unknown999 <- c( 'gestation', 'wt', 'mage', 'mwt', 'dage', 'dwt' )
team.clean.data[ unknown999 ] [ team.clean.data[ unknown999 ] == 999 ] <- NA

# generalise all 99 unknown's to NA
unknown99 <- c( 'mparity', 'mht', 'dht' )
team.clean.data[ unknown99 ] [ team.clean.data[ unknown99 ] == 99 ] <- NA

# view cleaned resulting data set
#View( team.clean.data )
#str( team.clean.data )

# *
# * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * Data Exploration Section

# *
# * Initial table summary of Low Birth Weight & baby weight differentials
# *
table.LowBirthWeights <-  team.clean.data %>%
  count( lowBirthWeight ) %>%
  ungroup() %>% 
  mutate( prop = round( prop.table( n ), digits = 2 ) )  %>% 
  bind_rows( group_by(.) %>%
               summarise( n = sum( n ), prop = sum( prop ) ) %>%
               mutate( lowBirthWeight ='Total' ) ) %>%  
  rename( 'Low Birth Weight' = lowBirthWeight,
          'Number of Observations' = n,
          'Proportion of Data Set (%)' = prop )

# check out table
kable( table.LowBirthWeights )

# lets have a look at the differentials of baby weights, coloured by low birth weight column
hist.babyWeights <- ggplot( data = team.clean.data,
                            aes( x = wt, fill = lowBirthWeight ) ) +
  geom_bar() +
  ggtitle('Distribution of Baby Weights') +
  xlab('Range of Baby Weights') +
  ylab('Count of differences observed') +
  theme_light()

# check out chart
hist.babyWeights

# *
# * Collinearity between msmoke & mtime 
# *
# show strange relationship between mSmoke & mTime
# make copy data set to help plot results
chart.msmoketime <- team.clean.data

# something to possibly consider
chart.msmoketime <- chart.msmoketime[ !is.na(chart.msmoketime$msmoke), ]
chart.msmoketime <- chart.msmoketime[ !is.na(chart.msmoketime$mtime), ]

# strange relationship occurs when mTime equals 'During current pregnancy'
highlight.gene <- 'During current pregnancy'
# add highlight flag to new data set
chart.msmoketime$highlight <- ifelse( chart.msmoketime$mtime == highlight.gene, 'Ambiguity Identified', 'Clear Meaning')

textdf <- chart.msmoketime[chart.msmoketime$mtime == highlight.gene, ]
mycolours <- c( 'Ambiguity Identified' = 'red', 'Clear Meaning' = 'grey50')

# make final scatter plot
scPlot.mSmokemTime <- ggplot( data = chart.msmoketime, 
                              aes( x = msmoke, y = mtime ) ) +
  geom_point( size = 3, 
              aes( colour = highlight ) ) +
  scale_color_manual( 'Colour Key:', values = mycolours ) +
  ggtitle( 'Relationship between mSmoke and mTime variables in data set' ) +
  xlab( 'Mothers Smoking Status' ) +
  ylab( 'If the Mother quit smokng, how long ago' ) +
  theme( legend.position = 'none' ) +
  theme_light()
# check out chart
scPlot.mSmokemTime


# *
# * BASE DATA SET FOR MODELLING 
# *
base.model.dataset <- team.clean.data %>% 
  select( -newY_Groupedwt, -lowBirthWeight, -mumEverSmoked ) %>% 
  na.omit() 


# 1st Order Interactions - priori logical
# Common thoughts are if Mother has ever smoked, and gave birth early we would assume the babies weight would be lower 
# than average
# first clean data set to ensure mEverSmoke is included in data
interactions.model.dataset <- team.clean.data %>% 
                              select( -newY_Groupedwt, -lowBirthWeight ) %>% 
                              na.omit() 

# generate 1st Order Interactions
intaraction.priori.logical <- lm( wt ~ mumEverSmoked:gestation, 
                                  data = interactions.model.dataset )

# *
#  backwards p-value method  -------------------------------------------------

p.model.data <- base.model.dataset
p.model <- lm(wt ~ ., data = p.model.data)
Anova(p.model)

#Remove the coliniar covariantes
new.p.Model.1 <- update(p.model, . ~ . -mtime)
Anova(new.p.Model.1)

new.p.Model.2 <- update(new.p.Model.1, . ~ . -msmoke)
Anova(new.p.Model.2)
# Now continue

new.p.Model.3 <- update(new.p.Model.2, . ~ . -dage)
Anova(new.p.Model.3)

new.p.Model.4 <- update(new.p.Model.3, . ~ . -mage)
Anova(new.p.Model.4)

new.p.Model.5 <- update(new.p.Model.4, . ~ . -marital)
Anova(new.p.Model.5)

new.p.Model.6 <- update(new.p.Model.5, . ~ . -inc)
Anova(new.p.Model.6)

new.p.Model.7 <- update(new.p.Model.6, . ~ . -dht)
Anova(new.p.Model.5)

new.p.Model.8 <- update(new.p.Model.7, . ~ . -med)
Anova(new.p.Model.8)

new.p.Model.9 <- update(new.p.Model.8, . ~ . -mwt)
Anova(new.p.Model.9)

new.p.Model.10 <- update(new.p.Model.9, . ~ . -mrace)
Anova(new.p.Model.8)

# Date is not considered to be of interest as a covariate and is therefore removed
new.p.Model.11 <- update(new.p.Model.10, . ~ . -date)
Anova(new.p.Model.11)

new.p.Model.12 <- update(new.p.Model.11, . ~ . -ded)
Anova(new.p.Model.12)

new.p.Model.13 <- update(new.p.Model.12, . ~ . -dwt)
Anova(new.p.Model.13)

# We now have a model - following the p-value method - which contains the 
# gestation, mparity, mht, drace, and mnumber covariates
p.value.model <- lm(wt ~ gestation + mparity + mht + drace  +  mnumber, data = p.model.data)
# summary(p.value.model)
#plot(p.value.model)
#AIC(p.value.model)

# adjusted R-squared ---------------------------------------------------------------

# model with all varibales - adjusted R squared = 0.29
R.model.test <- base.model.dataset  
R.full.model <- lm( wt ~ ., data = R.model.test )
summary(R.full.model)

# model with removal of dage - adjusted R squared = 0.2913
R1.model.test <- base.model.dataset  %>% select (-dage)
R1.full.model <- lm( wt ~ ., data = R1.model.test )
summary(R1.full.model)

# model with additional removal of mage  - adjusted R squared = 0.2925
R2.model.test <- base.model.dataset %>% select (-mage, -dage)
R2.full.model <- lm( wt ~ ., data = R2.model.test )
summary(R2.full.model)

# model with additional removal of inc - adjusted R squared = 0.2981
R3.model.test <- base.model.dataset  %>% select (-inc, -mage, -dage) 
R3.full.model <- lm( wt ~ ., data = R3.model.test )
summary(R3.full.model)

# model with additional removal of dht - adjusted R squared = 0.2989
R4.model.test <- base.model.dataset %>% select (-dht, -inc,-mage, -dage)
R4.full.model <- lm( wt ~ ., data = R4.model.test )
summary(R4.full.model)

# model with additional removal of mrace - adjusted R squared = 0.2996
R5.model.test <- base.model.dataset  %>% select (-mrace, -dht, -inc, -mage, -dage)
R5.full.model <- lm( wt ~ ., data = R5.model.test )
summary(R5.full.model)

# model with additional removal of marital - adjusted R squared = 0.3028
R6.model.test <- base.model.dataset  %>% select (-marital, -mrace, -dht, -inc, -mage, -dage)
R6.full.model <- lm( wt ~ ., data = R6.model.test )
summary(R6.full.model)

# model with additional removal of mwt - adjusted R squared = 0.3035
R7.model.test <- base.model.dataset  %>% select (-mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R7.full.model <- lm( wt ~ ., data = R7.model.test )
summary(R7.full.model)

# model with additional removal of med - adjusted R squared = 0.3053
R8.model.test <- base.model.dataset %>% select (-med, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R8.full.model <- lm( wt ~ ., data = R8.model.test )
summary(R8.full.model)

# model with additional removal of date - adjusted R squared = 0.3022
R9.model.test <- base.model.dataset  %>% select (-date, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R9.full.model <- lm( wt ~ ., data = R9.model.test )
summary(R9.full.model)
#adjusted R squared not higher - leave date in model

# model with additional removal of mtime - adjusted R squared = 0.3027
R10.model.test <- base.model.dataset  %>% select (-mtime, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R10.full.model <- lm( wt ~ ., data = R10.model.test )
summary(R10.full.model)
#adjusted R squared not higher - leave mtime in model

# model with additional removal of drace - adjusted R squared = 0.2816
R11.model.test <- base.model.dataset  %>% select (-drace, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R11.full.model <- lm( wt ~ ., data = R11.model.test )
summary(R11.full.model)
# adjusted R squared not higher - leave drace in model

# model with additional removal of mnumber - adjusted R squared = 0.2764
R12.model.test <- base.model.dataset  %>% select (-mht, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R12.full.model <- lm( wt ~ ., data = R12.model.test )
summary(R12.full.model)
# adjusted R squared not higher - leave mnumber in model

# model with additional removal of msmoke - adjusted R squared = 0.3035
R13.model.test <- base.model.dataset  %>% select (-msmoke, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R13.full.model <- lm( wt ~ ., data = R13.model.test )
summary(R13.full.model)
# adjusted R squared not highest - leave msmoke in model

# model with additional removal of dwt - adjusted R squared = 0.3000
R14.model.test <- base.model.dataset  %>% select (-dwt, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R14.full.model <- lm( wt ~ ., data = R14.model.test )
summary(R14.full.model)
# adjusted R squared not highest - leave dwt in model

# model with additional removal of ded - adjusted R squared = 0.3004
R15.model.test <- base.model.dataset %>% select (-ded, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R15.full.model <- lm( wt ~ ., data = R15.model.test )
summary(R15.full.model)
# adjusted R squared not highest - leave ded in model

# model with additional removal of mht - adjusted R squared = 0.2764
R16.model.test <- base.model.dataset %>% select (-mht, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R16.full.model <- lm( wt ~ ., data = R16.model.test )
summary(R16.full.model)
# adjusted R squared not highest - leave mht in model

# model with additional removal of mparity - adjusted R squared = 0.2952
R17.model.test <- base.model.dataset %>% select (-mparity, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R17.full.model <- lm( wt ~ ., data = R17.model.test )
summary(R17.full.model)
# adjusted R squared not highest - leave mparity in model

# model with additional removal of gestation - adjusted R squared = 0.1633
R18.model.test <- base.model.dataset %>% select (-gestation, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R18.full.model <- lm( wt ~ ., data = R18.model.test )
summary(R18.full.model)
# adjusted R squared not highest - leave gestation in model

# Therefore using adjusted R squared the best linear model is
R.final.model.test <- base.model.dataset  %>% select (-med, -mwt, -marital, -mrace, -dht, -inc, -mage, -dage)
R.final.full.model <- lm( wt ~ ., data = R.final.model.test )
summary(R.final.full.model)
# Removing the following variables med, mwt, marital, mrace, dht, inc, mage, dage, sex, id, outcome, pluralty

# Final model includes; gestation, mparity, mht, drace, ded, dwt,mnumber - adjusted R squared is 0.3053
# variable date has been removed as it is of no interest
# mtime and msmoke have been removed as collinear with mnumber


# AIC ---------------------------------------------------------------------

# * INITIAL MODEL SELECTION ~ AIC Stepwise 
# *
# with our base model dataset
model3.lm.AICStep <- lm( wt ~ ., data = base.model.dataset )
model3.AICStep <- step( model3.lm.AICStep )
# check model for collinearity - proves atleast one exists
vif( model3.AICStep )
# confirm which variables are collinear - mtime and mnumber
alias( model3.AICStep )

# however, from our data exploration we know something is up with msmoke & mtime
# so lets apply the same approach as above, but force the step() function to disregard 
# mtime - easiest way to do that is to remove mtime from the data set provided
# try another model removing mtime from dataset
base.model.dataset.without.mtime <- base.model.dataset %>% 
  select( -mtime )
model3.lm.AICStep.without.mtime <- lm( wt ~ ., data = base.model.dataset.without.mtime )
# calculate new best model, and what do we have here... AIC is now 3294.08 - lower than our previous best!...
model3.AICStep.without.mtime <- step( model3.lm.AICStep.without.mtime )

# so lets confirm that we achieve the same AIC result by selecting the specific predictive variables
# test model without time on data set including mtime
model3.lm.comparisonStuff <- lm( wt ~ mnumber + dwt + mparity + msmoke + drace + mht + gestation, data = base.model.dataset ) 
# we do, so our actual best model is this one - not the above
model3.comparisonStuff <- step( model3.lm.comparisonStuff )

# now check for collinearity - proves atleast one exists
vif( model3.lm.comparisonStuff )
# confirm which variables are collinear - mnumber and msmoke
alias( model3.lm.comparisonStuff )


# therefor our final best model for AIC stepwise approach is below
model3.lm.final <- lm( wt ~ mnumber + dwt + mparity + drace + mht + gestation, data = base.model.dataset ) 
model3.final <- step( model3.lm.final )

# returnining an AIC value of 5003.591
AIC( model3.final )

# *
# * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * Selecting Optimum Model
# * AIC, BIC, K-Folds Cross Validation

# *
# * Custom function to appraise models we have built for this assignment
# * simply pass in linear model, a name to reference that model, the train
# * percentage of dataset to use and number of k-fold validaton to process
# *
# * Function returns single row of results for AIC, BIX & k-fold validation outputs
# *
function.ModelAppraisal <- function( lm.model, model.name, 
                                     train.percentage, k ){
  
  # load required libraries
  library( 'dplyr' )
  library( 'car' )
  library( 'lattice' )
  library( 'caret' )
  
  # set seed to ensure results can be replicated
  set.seed(123)
  
  # extract model data set to use
  kcv.dataset <- model.frame( lm.model )
  # now we are going to re-construct the model formula used
  dataColumns <- as.vector( names( kcv.dataset ) )
  ref.xCovariates <- length( dataColumns )
  func.xCovariates <- dataColumns[ 2:ref.xCovariates ]
  func.Y <- dataColumns[ 1 ]
  # generate linear function to use
  func.LinearModel <- as.formula( paste( func.Y, paste( func.xCovariates,
                                                        collapse = '+' ), 
                                         sep = '~' ) )
  
  # lets create splits/partitions of train and test
  kcv.split <- createDataPartition( y = kcv.dataset[, func.Y], 
                                    p = train.percentage, list = FALSE )
  # train
  kcv.train <- kcv.dataset[ kcv.split, ]
  # test
  kcv.test <- kcv.dataset[ -kcv.split, ]
  
  kcv.type <- paste( 'k = ', k, sep = '' )
  
  # run cross validation on entire dataset to gauge results
  kcv.TrainModel <- train(  func.LinearModel,
                            kcv.train,
                            method = 'lm',
                            trControl = trainControl( method = 'cv', 
                                                      number = 5,
                                                      verboseIter = T ) )
  
  # now preduct values with trained model
  kcv.TrainPredictions <- predict( kcv.TrainModel, kcv.test )
  
  # create a data frame of predications vs actual test results
  kcv.Predictions <- data.frame( obs = kcv.test[, func.Y], 
                                 pred = kcv.TrainPredictions )
  
  # show how effective model is
  aic.Results <- round( AIC( lm.model ), digits = 2 )
  bic.Results <- round( BIC( lm.model ), digits = 2 )
  # to use test/predicton results adjust comments accordingly
  #kcv.Results <- round( defaultSummary( kcv.Predictions ), digits = 2 )
  # to use the train results adjust comments accordingly
  kc.TrainResults <- kcv.TrainModel$results[2:4]
  kcv.Results <- round( as.vector( kc.TrainResults ), digits = 2 )
  
  # generate full appraisal data set
  model.appraisal <-  as.data.frame( cbind( Model = model.name, AIC = aic.Results, BIC = bic.Results, 
                                            Type = kcv.type, 
                                            RMSE = kcv.Results[1], Rsquared = kcv.Results[2], MAE = kcv.Results[3] ) ) %>%
    rename( 'Cross Validation (CV) Type' = Type,
            'CV - RMSE' = RMSE, 
            'CV - Rsquared' = Rsquared,
            'CV - MAE' = MAE )
  # return model appraisal to user
  return( model.appraisal )
}

# set global variables to support model appraisal
k <- 5
train.percentage <- 0.8

# utilise modelAppraisal function to generate AIC, BIC & k-fold Cross Validation results
model1.Appraisal <- function.ModelAppraisal( intaraction.priori.logical, 'Model 1 - Priori Logical', train.percentage, k )
model2.Appraisal <- function.ModelAppraisal( p.value.model, 'Model 2 - pvalues', train.percentage, k )
model3.Appraisal <- function.ModelAppraisal( R.final.full.model, 'Model 3 - Adjusted Rsquared', train.percentage, k ) 
model4.Appraisal <- function.ModelAppraisal( model3.final, 'Model 4 - AIC', train.percentage, k )

# ****
# **** to add any more comparisons, simply copy and paste the above line and then add it to the rbind() step below
# ****

# using appraisals above, create one full table
fullmodel.AppraisalTable <- rbind( model1.Appraisal, model2.Appraisal, model3.Appraisal, model4.Appraisal )

# View results in Table
kable( fullmodel.AppraisalTable )

# **** can you guys please double check these results, even when I manually run BIC( p.value.model ) I get a lower BIC result 
# **** compared to running BIC( model3.final ) 


# *
# * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * Model Assumption Checks


# check model for collinearity - proves atleast one exists
vif( model3.AICStep )
# confirm which variables are collinear - mtime and mnumber
alias( model3.AICStep )

# however, from our data exploration we know something is up with msmoke & mtime
# so lets apply the same approach as above, but force the step() function to disregard 
# mtime - easiest way to do that is to remove mtime from the data set provided
# try another model removing mtime from dataset
base.model.dataset.without.mtime <- base.model.dataset %>% 
  select( -mtime )
model3.lm.AICStep.without.mtime <- lm( wt ~ ., data = base.model.dataset.without.mtime )
# calculate new best model, and what do we have here... AIC is now 3294.08 - lower than our previous best!...
model3.AICStep.without.mtime <- step( model3.lm.AICStep.without.mtime )

# so lets confirm that we achieve the same AIC result by selecting the specific predictive variables
# test model without time on data set including mtime
model3.lm.comparisonStuff <- lm( wt ~ mnumber + dwt + mparity + msmoke + drace + mht + gestation, data = base.model.dataset ) 
# we do, so our actual best model is this one - not the above
model3.comparisonStuff <- step( model3.lm.comparisonStuff )

# now check for collinearity - proves atleast one exists
vif( model3.lm.comparisonStuff )
# confirm which variables are collinear - mnumber and msmoke
alias( model3.lm.comparisonStuff )

# There are aliased coefs ranning into perfect collinearity 
# Indentifying which linearly dependent variables are culprits
ld.variables <- attributes(alias(model3.lm.comparisonStuff)$Complete)$dimnames[[1]]

# therefor our final best model for AIC stepwise approach is below
model3.lm.final <- lm( wt ~ mnumber + dwt + mparity + drace + mht + gestation, data = base.model.dataset ) 
model3.final <- step( model3.lm.final )

# Returning an AIC value of 5003.591
AIC(model3.lm.final)
# Evaluate Collinearity using variance inflation factors 
vif(model3.lm.final)
alias(model3.lm.final)

# Model diagnostics: collinearity
# Examining this using Variance Inﬂation Factors (VIFs)
# Measurinng the instability of parameter estimates due to dependencies between covariates 
# They are non-negative and a rule of thumb is that VIFs >10 is problematic 
vif(model3.lm.final) > 10

# Model diagnostics - error distribution
# Once all the signal is eliminated, only iid Normal distributed remain. Shape • 
# Checking shape of errors by the distribution of residuals (yi −ˆ yi) • 
# Using QQ-Norm plots and tests of Normality (Shapiro-Wilks, H0 the data (errors) are Normally distributed)
# Model diagnostics - error shape
qqnorm(resid(model3.lm.final)) 
qqline(resid(model3.lm.final))
shapiro.test(resid(model3.lm.final))

# Trying to track down the extreme residuals 
hist(resid(model3.lm.final))
High.Resid <- which(abs(resid(model3.lm.final))>5) 
base.model.dataset[High.Resid,]

# Evaluate Nonlinearity
# component + residual plot 
crPlots(model3.lm.final)
# Showing Ceres plots 
ceresPlots(model3.lm.final)

# Assessing linearity 
termplot(lm(wt ~ gestation + mparity + mht + dwt + mnumber + drace, 
            data = base.model.dataset, se=T))



termplot(lm(wt ~ gestation + mparity + mht + dwt + mnumber + drace, 
            data = base.model.dataset, se=T, partial.resid = T))

# Spread: 
# Checking variance of residuals (variance of the error distribution) is constant WRT ˆ y and the x 
# Testing with Breusch-Pagan (H0 the errors are homoscedastic/have constant variance)
ncvTest(model3.lm.final)
# Plots residuals against ˆ y and the x
Model.Resid <- resid(model3.lm.final)
plot(base.model.dataset$wt, Model.Resid, ylab = 'residuals', xlab = 'Mother Weight')
plot(base.model.dataset$mage, Model.Resid, ylab = 'residuals', xlab = 'Mother Age')
plot(fitted(model3.lm.final), Model.Resid, ylab = 'residuals', xlab = 'Fitted values')

# plotting studentized residuals vs. fitted values 
spreadLevelPlot(model3.lm.final)

# One can be explicit about which covariate to test against: 
ncvTest(model3.lm.final, wt ~ .)

# Independence: 
# Checking serial correlation of residuals, when ordered in some logical way e.g. order of collection, ˆ y, or an x 
# Plots residuals against ˆ y and the x 
# Testing with Durbin-Watson (H0 the errors are uncorrelated)
durbinWatsonTest(model3.lm.final)
Graph_1 <- plot(model3.lm.final, which = 1:2)

cut.fit <- cut(fitted(model3.lm.final), breaks = quantile(fitted(model3.lm.final),
                                                          probs = c(seq(0,1,length=20))))

table(cut.fit)

# plotting paris covariates
numericonly <- base.model.dataset %>% select_if(is.numeric)
ggpairs(numericonly)
# Running summary
summary(model3.lm.final)
Anova(model3.lm.final) 
confint(model3.lm.final)
plot(confint(model3.lm.final))
# demonstrating the confidence intervals graphically 
# demonstrating the confidence intervals graphically 
effect.model <- base.model.dataset %>% 
  mutate(gestation= factor(gestation), mparity = factor(mparity), 
         drace= factor(drace), mnumber= factor(mnumber), mht= factor(mht),
         dwt = factor(dwt)) 

altered.model <- update(model3.lm.final, wt ~. , data = effect.model)
plot(effect(term = "drace", mod = altered.model))
plot(effect(term = "mnumber", mod = altered.model))
plot(effect(term = "mparity", mod = altered.model))
plot(effect(term = "dwt", mod = altered.model))
plot(effect(term = "gestation", mod = altered.model))
plot(effect(term = "mht", mod = altered.model))

# First-order interaction between wt and gestation
First.order.interaction1 <- lm( wt ~ gestation, data = base.model.dataset )                                            
summary(First.order.interaction1)
Anova(First.order.interaction1)
plot(First.order.interaction1)
ggplot(data = base.model.dataset) + geom_point(aes(x = gestation, y = wt), pch = 21, size = 4, 
                                               fill = 'darkorange')
# First-order interaction between wt and mht
First.order.interaction2 <- lm( wt ~ mht, data = base.model.dataset )                                            
summary(First.order.interaction2)
Anova(First.order.interaction2)
plot(First.order.interaction2)
ggplot(data = base.model.dataset) + geom_point(aes(x = mht, y = wt), pch = 21, size = 4, 
                                               fill = 'darkorange')

# First-order interaction between wt and drace
First.order.interaction3 <- lm( wt ~ drace, data = base.model.dataset )                                            
summary(First.order.interaction3)
Anova(First.order.interaction3)
plot(First.order.interaction3)
ggplot(data = base.model.dataset) + geom_point(aes(x = drace, y = wt), pch = 21, size = 4, 
                                               fill = 'darkorange')
Plot.drace <- ggplot(base.model.dataset) + geom_boxplot(aes(drace, wt), fill = 'purple', alpha = 0.8)
Plot.drace

# First-order interaction between wt and msomke
First.order.interaction4 <- lm( wt ~ msmoke, data = base.model.dataset )                                            
summary(First.order.interaction4)
Anova(First.order.interaction4)
plot(First.order.interaction4)
ggplot(data = base.model.dataset) + geom_point(aes(x = msmoke, y = wt), pch = 21, size = 4, 
                                               fill = 'darkorange')
Plot.msmoke <- ggplot(base.model.dataset) + geom_boxplot(aes(msmoke, wt), fill = 'purple', alpha = 0.8)

# First-order interaction between wt and mnumber
First.order.interaction5 <- lm( wt ~ msmoke, data = base.model.dataset )                                            
summary(First.order.interaction5)
Anova(First.order.interaction5)
plot(First.order.interaction5)
ggplot(data = base.model.dataset) + geom_point(aes(x = mnumber, y = wt), pch = 21, size = 4, 
                                               fill = 'darkorange')
Plot.mnumber <- ggplot(base.model.dataset) + geom_boxplot(aes(mnumber, wt), fill = 'purple', alpha = 0.8)
Plot.mnumber

# Percentile confidence intervals from a nonparametric bootstrap

# Setting the seed
set.seed(123)

# Fitting the original model
model <- lm( wt ~ mnumber + dwt + mparity + drace + mht + gestation, 
             
             data = base.model.dataset )
# Viewing dataset
#View( base.model.dataset )

# Assigning predictions and residuals
pred <- predict(model)
resid <- resid(model)

# * 
# * Extension of bootstrap function to return confidence intervals for 'best' model 
# * which contains 16 coefficients and intercept
# *
bootstrap.fn <- function(lm.object, B = 10, alpha = 0.05 ){

# Purpose: Performing confidence interval calculation for
#           parameters of our best linear regression model using
#           non-parametric bootstrap based either on percentile
#       
# Input:  lm.object - Object created by regression function lm()
#         residualBS
#         B represents number of bootstrap replicates
#         alpha - Type I error rate
# Output: A 2*17 matrix of lower/upper bounds (columns) for intercept and slope (rows)
#         vector of diagnostics for inadmissible residual bootstraps responses and qq plots 

set.seed( 123 )
param.name <- c("Intercept", "Slope")
n.obs <- nrow( lm.object$model )

# Setting matrix that contains the bootstrap coefficients
boot.coef <- matrix(NA, nrow = B + 1, ncol = 17)
number.coefs <- matrix(17, nrow = B, ncol = 1)
ci.bounds <- matrix(NA, nrow = 17, ncol = 2)

# Conducting the bootstrap
for (i in 1:B){
    
resample.index <- sample( 1:n.obs, n.obs, replace = TRUE )
    
resample <- lm.object$model[ resample.index, ]

# Refitting the model   
new.model <- lm( as.formula( lm.object$terms ), data = resample )

# Extracting the parameter estimates    
num.coefs <- length( coef( new.model ) )
    
if( num.coefs == 17 ){ 
      
# Extracting the parameter estimates
boot.coef[i,] <- coef(new.model) 
      
} else { number.coefs[i,] <- num.coefs   }
}
# Adding the original parameter estimates 
boot.coef[ B+1,] <- coef(lm.object)
  
new.boot.coef <- boot.coef[ !rowSums( !is.finite( boot.coef ) ), ]
  
actual.bootscompleted <- nrow( new.boot.coef )

# calculating confidence intervals 
for(i in 1:17) {
ci.bounds[i, ] <- round(quantile(new.boot.coef[,i], 
                                 probs =c( alpha / 2, ( 1 - alpha / 2 ) ) ), digits = 6 )
    
}
  
return(list( ci.bounds = ci.bounds, 
                
                boot = boot.coef,
                
                refinedboot = new.boot.coef,
                
                numberofBootscompleted = actual.bootscompleted,
                
                coefs = number.coefs ) )
  
}

# Showing the confidence intervals of bootstrap
regular.boot <- bootstrap.fn(model, B = 999 )
regular.boot$ci.bounds

# Regular bootstrap CI for intercept
print(paste("Regular bootstrap CI for intercept", regular.boot$ci.bounds[1, 1],
            "to", regular.boot$ci.bounds[1, 2]))
# Regular bootstrap CI for slope
print(paste("Regular bootstrap CI for slope", regular.boot$ci.bounds[2, 1],
            "to", regular.boot$ci.bounds[2, 2]))


