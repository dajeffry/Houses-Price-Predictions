install.packages("ggplot2")
library(ggplot2)

## 2B EDA
## House Price Prediction

# 1. Data Extraction

## read data
house_df <- read.csv("Data/data.csv")

## read data
str(house_df)

## statistical summary
summary(house_df)

# 2. EDA
## 2.1 Univariate Data Analysis (one variable)
## Analyze distribution of target variable (price)
ggplot(data = house_df, aes(y = price)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 2000000))

## 2.2 Bivariate Data Analysis
## analyze price by number of bedrooms
## x = bedroom, y = price

house_df$bedrooms2 <- factor(house_df$bedrooms)

ggplot(house_df, aes(x=bedrooms2, y=price)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 2000000))

ggplot(data = house_df, aes(x = bedrooms2)) +
  geom_bar() +
  stat_count(geom = "text", color = "black", size = 3.5,
             aes(label = ..count..), position=position_stack(vjust = 0.5))

## 2.3 Multivariate Data Analysis

## compute correlation coefficient (R)

house_df_num <- house_df[ , 2:12]
r <- cor(house_df_num)

library(corrgram)
corrgram(house_df_num, order = TRUE,
         upper.panel = panel.pie)      # order = mengurutkan data danmelihat yg redundant
corrgram(house_df_num, order = TRUE,
         upper.panel = panel.cor)
corrgram(house_df_num,   # hubungan terdekat dengan price
         upper.panel = panel.pie)  

### Notes
### 1. there are outliers in price
### 2. Incorrect price values (price = 0)
### 3. In general, the higher number of bedrooms, 
###   the higher the price. however, for houses with 0 bedrooms,
###   the price are significantly higher.
### 4. Based on pearson's correlation coefficient (R),
###   the variable with highest correlation with target (price) are
###   sqft_living, sqft_above, and bathrooms.
### 5. Location is an important feature to predict price.
###    so, its necessary to include in the modelling


# 3. Data Preparation

## 3.1 Data Cleaning (Day 7)
summary(house_df$price)

### 3.1.1 remove data with price == 0

### get index that price == 0
idx_price_0 <- which(house_df_num$price == 0) 

### remove obs with price == 0
house_df_num <- house_df_num[ -idx_price_0, ]

### 3.1.2 Remove outlier
### get outlier index

out_price <- boxplot.stats(house_df_num$price)$out
out_price

idx_out <- which(house_df_num$price %in% c(out_price))
idx_out

### remove obs with outliers
house_df_num <- house_df_num[ -idx_out, ]


## 3.2 Feature Extraction

## 3.3 split data into train and test
## Train:Test = 70:30

# for reproducible result
set.seed(2021)

m <- nrow(house_df_num)

m_train <- m * 0.7
m_train
train_idx <- sample(m, m_train)

train_df <- house_df_num[train_idx,]
test_df <- house_df_num[-train_idx,]

# 4. Modeling

## 4.1 Simple Linear Regression
model.slr <- lm(formula = price ~ sqft_living,
                data = train_df)
summary(model.slr)

## 4.2 Polynomial Regression

model.poly <- lm(formula = price ~ sqft_living + I(sqft_living^2),
                 data = train_df)
summary(model.poly)

## 4.3 Multivariate :inear Regression
model.mlr1 <- lm(formula = price ~ sqft_living + bathrooms +view,
                data = train_df)
model.mlr1

model.mlr2 <- lm(formula = price ~ . ,
                 data = train_df)
model.mlr2

## 4.4 MLR with Interaction
model.int <- lm(formula = price ~ sqft_living + bathrooms +
                  sqft_living:bathrooms,
                data = train_df)
model.int


## 4.5 MyModel

mymodel <- lm(formula = price ~ sqft_above + I(sqft_basement^1),
              data = train_df)
mymodel

# 5. Evaluation

## Actual value from test data
actual <- test_df$price

## predicted calues using SLR model
pred.slr <- predict(model.slr, test_df)
pred.poly <- predict(model.poly, test_df)
pred.mlr1 <- predict(model.mlr1, test_df)
pred.mlr2 <- predict(model.mlr2, test_df)
pred.int <- predict(model.int, test_df)
pred.mymodel <- predict(mymodel, test_df)

## create dataframe for actual and predicted values
prediction_df <- data.frame(actual, pred.slr, pred.poly,
                            pred.mlr1, pred.mlr2, pred.int)

## visualize actual vs predicted
p1 <- ggplot(data = prediction_df, aes(x = actual, y = pred.slr)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(limits = c(0, 1500000),
                     breaks = c(500000,1000000,1500000),
                     labels = c("$500k", "$1M", "$1,5M")) +
  scale_y_continuous(limits = c(0, 1500000),
                     breaks = c(500000,1000000,1500000),
                     labels = c("$500k", "$1M", "$1,5M")) +
  labs(title = "Simple Regression")

p2 <- ggplot(data = prediction_df, aes(x = actual, y = pred.poly)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(limits = c(0, 1500000),
                     breaks = c(500000,1000000,1500000),
                     labels = c("$500k", "$1M", "$1,5M")) +
  scale_y_continuous(limits = c(0, 1500000),
                     breaks = c(500000,1000000,1500000),
                     labels = c("$500k", "$1M", "$1,5M")) +
  labs(title = "Polynomial Regression")

p3 <- ggplot(data = prediction_df, aes(x = actual, y = pred.mlr2)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(limits = c(0, 1500000),
                     breaks = c(500000,1000000,1500000),
                     labels = c("$500k", "$1M", "$1,5M")) +
  scale_y_continuous(limits = c(0, 1500000),
                     breaks = c(500000,1000000,1500000),
                     labels = c("$500k", "$1M", "$1,5M")) +
  labs(title = "Multivariate Regression")

p4 <- ggplot(data = prediction_df, aes(x = actual, y = pred.int)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(limits = c(0, 1500000),
                     breaks = c(500000,1000000,1500000),
                     labels = c("$500k", "$1M", "$1,5M")) +
  scale_y_continuous(limits = c(0, 1500000),
                     breaks = c(500000,1000000,1500000),
                     labels = c("$500k", "$1M", "$1,5M")) +
  labs(title = "Interaction")

library(gridExtra)
grid.arrange(p1, p2, p3, p4)

## Compute Performance evaluation

performance <- function(actual, predicted){
  ## root mean square error (rmse)
   e <- predicted - actual  # error
  se <- e^2 # squared error
  sse <- sum(se) # sum of squared error
  mse <- mean(se) # mean squared eror
  rmse <- sqrt(mse) # root mean squared error
  
  return(rmse)
}

performance(actual, pred.slr)
performance(actual, pred.poly)
performance(actual, pred.mlr2)
performance(actual, pred.int)

performance <- function(actual, predicted, model){
  ## root mean square error (rmse)
  e <- predicted - actual  # error
  se <- e^2 # squared error
  sse <- sum(se) # sum of squared error
  mse <- mean(se) # mean squared eror
  rmse <- sqrt(mse) # root mean squared error
  
  ## Pearson's Correlation Coefficient (R)
  r <- cor(predicted, actual)
  
  result <- paste("=== Model:", model,
                  "\nRoot Mean Squared Error (RMSE): ",round(rmse,2),
                  "\nCorrelation Coefficient (R): ", round(r,5))
  
                  
  cat(result)
}

performance(actual, pred.slr, "Single Linear regression")
performance(actual, pred.poly, "Polynomial Regression")
performance(actual, pred.mlr2, "Multivariate Linear Regression")
performance(actual, pred.int, "Multivariate with Interaction")
performance(actual, pred.mymodel, "My Model")
