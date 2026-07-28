
library(dplyr)
library(tidyverse)
library(forecast)
library(car)
library(knitr)

options(scipen=999)

# Load Training Data
fullTrainData <- read.csv("richardsontraining.csv")

# select needed variables

lmData <- subset(fullTrainData, select = c(IMPR_VAL, LAND_VAL, TOT_VAL, REVAL_YR, PREV_REVAL_YR, PREV_MKT_VAL, YR_BUILT, ACT_AGE, BLDG_CLASS_CD, 
                                       CDU_RATING_DESC, TOT_MAIN_SF, TOT_LIVING_AREA_SF, NUM_STORIES_DESC, FOUNDATION_TYP_DESC, 
                                       FENCE_TYP_DESC, ROOF_TYP_DESC, NUM_FIREPLACES, NUM_KITCHENS, NUM_FULL_BATHS, NUM_HALF_BATHS,
                                       NUM_WET_BARS, NUM_BEDROOMS, SPRINKLER_SYS_IND, DECK_IND, SPA_IND, POOL_IND, SAUNA_IND, 
                                       DEPRECIATION_PCT, PROPERTY_ZIPCODE))

# Filter out Townhomes, Land only and condominiums (keep only numeric values - homes)
lmData <- lmData %>%
  filter(!is.na(as.numeric(BLDG_CLASS_CD)))

# Shorten PROPERTY_ZIPCODE to only 5 digits
lmData$PROPERTY_ZIPCODE <- substring(as.character(lmData$PROPERTY_ZIPCODE), 1, 5)

# Remove non-Richardson zipcode
lmData <- subset(lmData, PROPERTY_ZIPCODE != 75044)

# clean data of missing values

lmData <- lmData %>%
  drop_na(TOT_VAL, YR_BUILT, CDU_RATING_DESC, TOT_MAIN_SF, TOT_LIVING_AREA_SF, NUM_STORIES_DESC, FOUNDATION_TYP_DESC,
            FENCE_TYP_DESC, ROOF_TYP_DESC, NUM_FIREPLACES, NUM_KITCHENS, NUM_FULL_BATHS, NUM_HALF_BATHS, NUM_WET_BARS,
            NUM_BEDROOMS, SPRINKLER_SYS_IND, DECK_IND, SPA_IND, POOL_IND, SAUNA_IND, PROPERTY_ZIPCODE)

# Load validation data and create validation data set in same way

fullValidData <- read.csv("richardsonvalidation.csv")

validData <- subset(fullValidData, select = c(IMPR_VAL, LAND_VAL, TOT_VAL, REVAL_YR, PREV_REVAL_YR, PREV_MKT_VAL, YR_BUILT, ACT_AGE, BLDG_CLASS_CD,
                                       CDU_RATING_DESC, TOT_MAIN_SF, TOT_LIVING_AREA_SF, NUM_STORIES_DESC, FOUNDATION_TYP_DESC, 
                                       FENCE_TYP_DESC, ROOF_TYP_DESC, NUM_FIREPLACES, NUM_KITCHENS, NUM_FULL_BATHS, NUM_HALF_BATHS,
                                       NUM_WET_BARS, NUM_BEDROOMS, SPRINKLER_SYS_IND, DECK_IND, SPA_IND, POOL_IND, SAUNA_IND, 
                                       DEPRECIATION_PCT, PROPERTY_ZIPCODE))

# Filter out Townhomes, Land only and condominiums (keep only numeric values - homes)
validData <- validData %>%
  filter(!is.na(as.numeric(BLDG_CLASS_CD)))

# Shorten PROPERTY_ZIPCODE to only 5 digits
validData$PROPERTY_ZIPCODE <- substring(as.character(validData$PROPERTY_ZIPCODE), 1, 5)

# Remove non-Richardson zipcode
validData <- subset(validData, PROPERTY_ZIPCODE != 75044)

# Loading data
rownames(lmData) <- 1:9496
rownames(validData) <- 9497:20662

# Checking column names
setdiff(colnames(lmData),colnames(validData))
setdiff(colnames(validData),colnames(lmData))

# Combining data for investigation
data <- rbind(lmData, validData)

# Remove samples where CDU_RATING_DESC is "MANUALLY ENTER DEPRECIATION"
data <- subset(data, CDU_RATING_DESC != "MANUALLY ENTER DEPRECIATION")

# Remove samples where NUM_STORIES_DESC is "UNASSIGNED"
data <- subset(data, NUM_STORIES_DESC != "UNASSIGNED")

# Add decade grouping to dataset for graphing and data exploration
data <- data %>%
  mutate(Decade = paste0(floor(YR_BUILT/10)*10, "s"))

# Add Polynomial value - TOT_LIVING_AREA_SF^2
data <- data %>%
  mutate(TOT_LIVING_AREA_SF_SQRD = TOT_LIVING_AREA_SF^2)

# Add difference between new value and previous value

data <- data %>%
  mutate(VAL_DIFF = TOT_VAL - PREV_MKT_VAL)

summary(data$VAL_DIFF)

# Add percentage of change from TOT_VAL

data <- data %>%
  mutate(PCT_CHG = VAL_DIFF/TOT_VAL)

summary(data$PCT_CHG)

# Factoring  and converting columns to numeric
classes <- sapply(data, class)
factored <- data[0:nrow(data), FALSE]

for (i in 1:length(classes)){
  if (classes[i] == 'character' | classes[i] == 'logical'){
    factored = cbind(factored, factor(data[,names(classes[i])]))
  }
}

names(factored) <- names(classes[classes == 'character' | classes == 'logical'])
level <- sapply(factored, levels) # Saving the levels

# Replacing columns in data with factored columns
for (i in names(factored)){
  data[,i] <- factored[,i]
}



# # factor the needed values
# 
 data <- data %>%
   mutate(YR_BUILT = as.factor(YR_BUILT),
          PROPERTY_ZIPCODE = as.factor(PROPERTY_ZIPCODE),
          CDU_RATING_DESC = factor(CDU_RATING_DESC, 
                                      levels = c("UNDESIRABLE",
                                                 "VERY POOR",
                                                 "POOR",
                                                 "FAIR",
                                                 "AVERAGE",
                                                 "GOOD",
                                                 "VERY GOOD",
                                                 "EXCELLENT")),
          NUM_STORIES_DESC = factor(NUM_STORIES_DESC, 
                                       levels = c("ONE STORY",
                                                  "ONE AND ONE HALF STORIES",
                                                  "TWO STORIES",
                                                  "TWO AND ONE HALF STORIES",
                                                  "THREE STORIES")),
          SPRINKLER_SYS_IND = as.factor(SPRINKLER_SYS_IND),
          DECK_IND = as.factor(DECK_IND),
          SPA_IND = as.factor(SPA_IND),
          POOL_IND = as.factor(POOL_IND),
          SAUNA_IND = as.factor(SAUNA_IND),
          Decade = as.factor(Decade))
 
 reviewSummary <- sapply(data, summary)
 reviewTable <- sapply(data, table)
 
# # factor the needed values
# 
# validData <- validData %>%
#   mutate(YR_BUILT = as.factor(YR_BUILT),
#          ZipCode = as.factor(ZipCode),
#          CDU_RATING_DESC = as.factor(CDU_RATING_DESC),
#          NUM_STORIES_DESC = as.factor(NUM_STORIES_DESC),
#          SPRINKLER_SYS_IND = as.factor(SPRINKLER_SYS_IND),
#          DECK_IND = as.factor(DECK_IND),
#          SPA_IND = as.factor(SPA_IND),
#          POOL_IND = as.factor(POOL_IND),
#          SAUNA_IND = as.factor(SAUNA_IND))
# 
# #add missing factors in validation data from training data
# validData$YR_BUILT <- factor(validData$YR_BUILT, levels = levels(lmData$YR_BUILT))
# validData$FOUNDATION_TYP_DESC <- factor(validData$FOUNDATION_TYP_DESC, levels = levels(lmData$FOUNDATION_TYP_DESC))
# validData$ROOF_TYP_DESC <- factor(validData$ROOF_TYP_DESC, levels = levels(lmData$ROOF_TYP_DESC))
# validData$NUM_STORIES_DESC <- factor(validData$NUM_STORIES_DESC, levels = levels(lmData$NUM_STORIES_DESC))

# Split data back into Train and Test
train <- data[1:9496,]
test <- data[9497:20662,]

# Remove house that was built in 1866 from testing data
test <- subset(test, YR_BUILT != 1866)

# Remove house that has a FOUNDATION_TYPE_DESC as "FOUNDATION SUPPORT"
test <- subset(test, FOUNDATION_TYP_DESC != "FOUNDATION SUPPORT")

# Remove houses that have Roof "CUTUP, DORMER"
test <- subset(test, ROOF_TYP_DESC != "CUTUP, DORMER")

# Remove houses that have Roof "SALTBOX"
test <- subset(test, ROOF_TYP_DESC != "SALTBOX")

# Remove Houses that have Roof "UNASSIGNED"
test <- subset(test, ROOF_TYP_DESC != "UNASSIGNED")

# REMOVE Houes with Barbwire Fencing from training dataset
train <- subset(train, FENCE_TYP_DESC != "BARBWIRE")


reviewSummaryTrain <- sapply(train, summary)
reviewTableTrain <- sapply(train, table)

reviewSummaryTest <- sapply(test, summary)
reviewTableTest <- sapply(test, table)

#First model with all variables

lmFull <- lm(TOT_VAL ~ ACT_AGE + CDU_RATING_DESC + TOT_MAIN_SF + TOT_LIVING_AREA_SF + NUM_STORIES_DESC + FOUNDATION_TYP_DESC +
               FENCE_TYP_DESC + ROOF_TYP_DESC + NUM_FIREPLACES + NUM_KITCHENS + NUM_FULL_BATHS + NUM_HALF_BATHS + NUM_WET_BARS +
               NUM_BEDROOMS + SPRINKLER_SYS_IND + DECK_IND + SPA_IND + POOL_IND + SAUNA_IND + PROPERTY_ZIPCODE, data = train)
summary(lmFull)

lmNull <- lm(TOT_VAL ~ 1, data = train)


# use step() to run forward regression.
forward1 <- step(lmNull, scope=list(lower=lmNull, upper=lmFull), direction = "forward")
summary(forward1)

model1 <- lm(formula = TOT_VAL ~ TOT_MAIN_SF + PROPERTY_ZIPCODE + CDU_RATING_DESC + 
               NUM_STORIES_DESC + FOUNDATION_TYP_DESC + ACT_AGE + ROOF_TYP_DESC + 
               NUM_KITCHENS + POOL_IND + TOT_LIVING_AREA_SF + NUM_FULL_BATHS + 
               NUM_HALF_BATHS + NUM_BEDROOMS + NUM_FIREPLACES + FENCE_TYP_DESC + 
               DECK_IND, data = train)

#check vif on model
vif(model1)

## VIF shows collinearity problem with TOT_MAIN_SF and TOT_LIVING_AREA_SF.  Will remove TOT_MAIN_SF for rest of exploration

# Exploration of data through plots

# plot TOT_VAL vs TOT_LIVING_AREA_SF
cols <- c("blue", "green", "red")

# Plot based off Zipcode
ggplot(train, aes(x = TOT_LIVING_AREA_SF, y = TOT_VAL, color = PROPERTY_ZIPCODE)) + 
  geom_point() +
  scale_color_manual(values = cols) +
  labs(
    title = "Housing Value vs Living Area by ZipCode",
    x = "Total Living Area (sq ft)",
    y = "Total Value",
    color = "ZipCode"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot based off YR_BUILT
ggplot(train, aes(x = TOT_LIVING_AREA_SF, y = TOT_VAL, color = Decade)) + 
  geom_point() +
  labs(
    title = "Housing Value vs Living Area by Decade",
    x = "Total Living Area (sq ft)",
    y = "Total Value",
    color = "Decade"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot based off CDU_RATING_DESC
ggplot(train, aes(x = TOT_LIVING_AREA_SF, y = TOT_VAL, color = CDU_RATING_DESC)) + 
  geom_point() +
  labs(
    title = "Housing Value vs Living Area by CDU Rating",
    x = "Total Living Area (sq ft)",
    y = "Total Value",
    color = "CDU Rating"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram of Total Value

ggplot(train, aes(x = TOT_VAL)) + 
  geom_histogram(color = "blue", fill = "white", size = 1.25, bins = 40) +
  labs(
    title = "Histogram of Housing Values",
    x = "Total Value",
    y = "Number of Houses",
  ) +
  xlim(0, 2000000) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram of Living Area Square Footage

ggplot(train, aes(x = TOT_LIVING_AREA_SF)) + 
  geom_histogram(color = "darkgreen", fill = "white", size = 1.25, bins = 40) +
  labs(
    title = "Histogram of Living Area",
    x = "Total Living Square Footage",
    y = "Number of Houses",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram of Value Difference (Total valuation - Previous Valuation)

ggplot(train, aes(x = VAL_DIFF)) + 
  geom_histogram(color = "darkred", fill = "white", size = 1.25, bins = 40) +
  labs(
    title = "Histogram of Value Difference",
    x = "Valuation Difference",
    y = "Number of Houses",
  ) +
  scale_x_continuous(limits = c(-20000, 500000)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram of Percentage Change

ggplot(train, aes(x = PCT_CHG)) + 
  geom_histogram(color = "maroon", fill = "white", size = 1.25, bins = 40) +
  labs(
    title = "Histogram of Value Percentage Change",
    x = "Value Percentage Change",
    y = "Number of Houses",
  ) +
  #scale_x_continuous(limits = c(-20000, 500000)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Percentage Change vs Total Living Square Footage by CDU Rating
ggplot(train, aes(x = TOT_LIVING_AREA_SF, y = PCT_CHG, color = CDU_RATING_DESC)) + 
  geom_point() +
  labs(
    title = "Percentage Change vs Living Area by CDU Rating",
    x = "Total Living Area (sq ft)",
    y = "Value Percentage Change",
    color = "CDU Rating"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Percentage Change vs Total Living Square Footage by PROPERTY_ZIPCODE
ggplot(train, aes(x = TOT_LIVING_AREA_SF, y = PCT_CHG, color = PROPERTY_ZIPCODE)) + 
  geom_point() +
  scale_color_manual(values = cols) +
  labs(
    title = "Percentage Change vs Living Area by ZipCode",
    x = "Total Living Area (sq ft)",
    y = "Value Percentage Change",
    color = "ZipCode"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Percentage Change vs Total Living Square Footage by Decade
ggplot(train, aes(x = TOT_LIVING_AREA_SF, y = PCT_CHG, color = Decade)) + 
  geom_point() +
  labs(
    title = "Percentage Change vs Living Area by Decade",
    x = "Total Living Area (sq ft)",
    y = "Value Percentage Change",
    color = "Decade"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## barchart exploration on TOT_VAL

# CDU RATING AVG VALUE
data.for.plot <- aggregate(train$TOT_VAL, by = list(train$CDU_RATING_DESC), FUN = mean)
names(data.for.plot) <- c("CDU_RATING", "MeanTotalValue")
 
ggplot(data.for.plot) + 
  geom_bar(aes(x = CDU_RATING, y = MeanTotalValue), stat = "identity", color = "darkblue", size = 1.5) +
  labs(
    title = "CDU Rating Avg Value",
    x = "CDU Rating",
    y = "Average House Value",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



# TOT_MAIN_SF and TOT_LIVING_AREA_SF could have collinearity problems - will remove TOT_MAIN_SF for rest of model building
# Will redo step() forward regression without TOT_MAIN_SF

#Second model with all variables -TOT_MAIN_SF

# lmFull <- lm(TOT_VAL ~ ACT_AGE + CDU_RATING_DESC + TOT_LIVING_AREA_SF + NUM_STORIES_DESC + FOUNDATION_TYP_DESC +
#                FENCE_TYP_DESC + ROOF_TYP_DESC + NUM_FIREPLACES + NUM_KITCHENS + NUM_FULL_BATHS + NUM_HALF_BATHS + NUM_WET_BARS +
#                NUM_BEDROOMS + SPRINKLER_SYS_IND + DECK_IND + SPA_IND + POOL_IND + SAUNA_IND + PROPERTY_ZIPCODE, data = train)
# YR_BUILT is having problem with validation, changing to use Decade instead
# Also will remove house built in 1866 from testing data
lmFull <- lm(TOT_VAL ~ ACT_AGE + CDU_RATING_DESC + TOT_LIVING_AREA_SF + NUM_STORIES_DESC + FOUNDATION_TYP_DESC +
            FENCE_TYP_DESC + ROOF_TYP_DESC + NUM_FIREPLACES + NUM_KITCHENS + NUM_FULL_BATHS + NUM_HALF_BATHS + NUM_WET_BARS +
             NUM_BEDROOMS + SPRINKLER_SYS_IND + DECK_IND + SPA_IND + POOL_IND + SAUNA_IND + PROPERTY_ZIPCODE, data = train)


summary(lmFull)

lmNull <- lm(TOT_VAL ~ 1, data = train)


# use step() to run forward regression.
forward2 <- step(lmNull, scope=list(lower=lmNull, upper=lmFull), direction = "forward")
summary(forward2)

model2 <- lm(formula = TOT_VAL ~ TOT_LIVING_AREA_SF + PROPERTY_ZIPCODE + 
               CDU_RATING_DESC + NUM_STORIES_DESC + FOUNDATION_TYP_DESC + 
               ACT_AGE + ROOF_TYP_DESC + POOL_IND + NUM_KITCHENS + NUM_FULL_BATHS + 
               NUM_HALF_BATHS + NUM_BEDROOMS + NUM_FIREPLACES + FENCE_TYP_DESC + 
               DECK_IND, data = train)

#check vif on model
vif(model2)


#check accuracy on validation data
predForward <- predict(model2, test)
accuracy(predForward, test$TOT_VAL)


# use step() to run stepwise regression backwards
back1 <- step(lmFull, direction = "backward")
summary(back1)  

model3 <- lm(formula = TOT_VAL ~ ACT_AGE + CDU_RATING_DESC + TOT_LIVING_AREA_SF + 
               NUM_STORIES_DESC + FOUNDATION_TYP_DESC + FENCE_TYP_DESC + 
               ROOF_TYP_DESC + NUM_FIREPLACES + NUM_KITCHENS + NUM_FULL_BATHS + 
               NUM_HALF_BATHS + NUM_BEDROOMS + DECK_IND + POOL_IND + PROPERTY_ZIPCODE, 
             data = train)

#check VIF on model
vif(model3)

#check accuracy on validation data
predBack1 <- predict(model3, test)
accuracy(predBack1, test$TOT_VAL)


# use step() to run stepwise regression.
step1 <- step(lmFull, direction = "both")
summary(step1)  

model4 <- lm(formula = TOT_VAL ~ ACT_AGE + CDU_RATING_DESC + TOT_LIVING_AREA_SF + 
               NUM_STORIES_DESC + FOUNDATION_TYP_DESC + FENCE_TYP_DESC + 
               ROOF_TYP_DESC + NUM_FIREPLACES + NUM_KITCHENS + NUM_FULL_BATHS + 
               NUM_HALF_BATHS + NUM_BEDROOMS + DECK_IND + POOL_IND + PROPERTY_ZIPCODE, 
             data = train)

#check VIF
vif(model4)


#check accuracy on validation data
predStep1 <- predict(step1, test)
accuracy(predStep1, test$TOT_VAL)



# Explore polynomial models - TOT_LIVING_AREA_SF 

model5 <- lm(formula = TOT_VAL ~ ACT_AGE + CDU_RATING_DESC + TOT_LIVING_AREA_SF + 
               I(TOT_LIVING_AREA_SF^2) + NUM_STORIES_DESC + FOUNDATION_TYP_DESC + FENCE_TYP_DESC + 
               ROOF_TYP_DESC + NUM_FIREPLACES + NUM_KITCHENS + NUM_FULL_BATHS + 
               NUM_HALF_BATHS + NUM_BEDROOMS + DECK_IND + POOL_IND + PROPERTY_ZIPCODE, 
             data = train)
summary(model5)
model5

#check accuracy on validation data
predPoly <- predict(model5, test)
accuracy(predPoly, test$TOT_VAL)

# Explore models predicting PCT_CHG on housing

lmFull <- lm(VAL_DIFF ~ ACT_AGE + CDU_RATING_DESC + TOT_LIVING_AREA_SF + NUM_STORIES_DESC + FOUNDATION_TYP_DESC +
               FENCE_TYP_DESC + ROOF_TYP_DESC + NUM_FIREPLACES + NUM_KITCHENS + NUM_FULL_BATHS + NUM_HALF_BATHS + NUM_WET_BARS +
               NUM_BEDROOMS + SPRINKLER_SYS_IND + DECK_IND + SPA_IND + POOL_IND + SAUNA_IND + PROPERTY_ZIPCODE, data = train)


summary(lmFull)

lmNull <- lm(VAL_DIFF ~ 1, data = train)


# use step() to run forward regression.
forward6 <- step(lmNull, scope=list(lower=lmNull, upper=lmFull), direction = "forward")
summary(forward6)

model2 <- lm(formula = VAL_DIFF ~ TOT_LIVING_AREA_SF + PROPERTY_ZIPCODE + 
               CDU_RATING_DESC + NUM_KITCHENS + NUM_STORIES_DESC + FOUNDATION_TYP_DESC + 
               ACT_AGE + FENCE_TYP_DESC + ROOF_TYP_DESC + NUM_FIREPLACES + 
               SPRINKLER_SYS_IND + NUM_FULL_BATHS + SPA_IND + NUM_BEDROOMS + 
               DECK_IND, data = train)

#check vif on model
vif(model2)


#check accuracy on validation data
predForward <- predict(model2, test)
accuracy(predForward, test$TOT_VAL)









# # Regression Tree
# library(rpart)
# library(rpart.plot)
# 
# # Build the regression tree model
# regTree <- rpart(PCT_CHG ~ Decade + CDU_RATING_DESC + TOT_LIVING_AREA_SF +
#                    TOT_LIVING_AREA_SF_SQRD + NUM_STORIES_DESC + FOUNDATION_TYP_DESC + ROOF_TYP_DESC + 
#                    NUM_FIREPLACES + NUM_KITCHENS + NUM_FULL_BATHS + NUM_HALF_BATHS + 
#                    NUM_WET_BARS + NUM_BEDROOMS + DECK_IND + POOL_IND + PROPERTY_ZIPCODE, data = train, method = "class")
# 
# # Plot the full tree
# prp(regTree, type = 1, extra = 1, split.font = 1, varlen = -10)
# 
# # Print the complexity parameter table to identify the optimal CP for pruning
# printcp(regTree)
# 
# # Plot the complexity parameter to visualize pruning
# plotcp(regTree)
# 
# # Prune the tree based on the optimal CP value (adjust based on printcp output)
# optimal_cp <- regTree$cptable[which.min(regTree$cptable[, "xerror"]), "CP"]
# 
# pruned_tree <- prune(regTree, cp = optimal_cp)
# 
# # Plot the pruned tree
# rpart.plot(pruned_tree, type = 1, extra = 101, under = TRUE)
