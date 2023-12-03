
library(readr)
df <- read_csv("London.csv")
View(df)
head(df)
summary(df)
head(df, 10)
tail(df, 5)
nrow(df)
ncol(df)
str(df)
library(tidyverse)
library(gapminder)
library(ggthemes)
library(ggplot2)
library(gridExtra)
ggplot(data = df, aes(x = Price, y = `No. of Bedrooms`)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(labels = scales::comma)
ggplot(data = df, aes(x = log(Price), y = `No. of Bedrooms`)) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.5) +
  scale_x_continuous(labels = scales::comma, name = "Log(Price)") +
  labs(y = "Number of Bedrooms") +
  plot1 <- ggplot(df, aes(x = Price)) +
  geom_histogram(binwidth = 50000, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Price") +
  scale_x_continuous(labels = scales::comma)
plot2 <- ggplot(df, aes(x = Area.in.sq.ft)) +
  geom_histogram(binwidth = 100, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Area.in.sq.ft") +
  scale_x_continuous(labels = scales::comma)
plot3 <- ggplot(df, aes(x = No..of.Bedrooms)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black", alpha = 0.7) +
  labs(title = "Histogram of No..of.Bedrooms") +
  scale_x_continuous(labels = scales::comma)
plot4 <- ggplot(df, aes(x = log(Price))) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log(Price)") +
  scale_x_continuous(labels = scales::comma)
plot5 <- ggplot(df, aes(x = log(Area.in.sq.ft))) +
  geom_histogram(binwidth = 0.1, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log(Area.in.sq.ft)") +
  scale_x_continuous(labels = scales::comma)
plot6 <- ggplot(df, aes(x = No..of.Bedrooms)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black", alpha = 0.7) +
  labs(title = "Histogram of No..of.Bedrooms") +
  scale_x_continuous(labels = scales::comma)
grid.arrange(plot1, plot2, plot3, ncol = 3)
grid.arrange(plot4, plot5, plot6, ncol = 3)
missing_data <- sum(is.na(df))
cat("Total missing data entries in the 'df' dataset: ", missing_data, "\n")
missing_by_column <- colSums(is.na(df))
cat("Missing data by column:\n")
print(missing_by_column)
missing_locations <- sum(df$Location == "")
cat("Missing data entries in the 'Location' column: ", missing_locations, "\n")
df <- df[!is.na(df$Location), ]
head(df)
missing_data_updated <- sum(is.na(df))
cat("Total missing data entries in the updated 'df' dataset: ", missing_data_updated, "\n")
summary(df)
df <- subset(df, select = -c(Postal.Code, Location, Property.Name))
df <- subset(df, select = -c(X)) s
str(df)
house_type_counts <- table(df$House.Type)
house_type_counts <- house_type_counts[house_type_counts >= 10]
city_county_counts <- table(df$City.County)
city_county_counts <- city_county_counts[city_county_counts >= 10]
cat("House Type Counts (>= 10):\n")
print(house_type_counts)
cat("\nCity/County Counts (>= 10):\n")
print(city_county_counts)
df <- df[df$House.Type %in% names(house_type_counts[house_type_counts >= 10]), ]
df <- df[df$City.County %in% names(city_county_counts[city_county_counts >= 10]), ]
cat("Updated House Type Counts (>= 10):\n")
print(table(df$House.Type))
cat("\nUpdated City/County Counts (>= 10):\n")
print(table(df$City.County))
view(df)
df$`House_Type_Encoded` <- factor(df$`House.Type`, levels = c("House", "Flat / Apartment", "Penthouse", "New development","Studio"), labels = c(1, 2, 3, 4, 5))
df$`County_Encoded` <- factor(df$`City.County`, levels = c("London", "Essex", "Middlesex", "Surrey","twickenham"), labels = c(1, 2, 3, 4, 5))
df$County_Encoded <- as.integer(df$County_Encoded)
df$House_Type_Encoded <- as.integer(df$House_Type_Encoded)
library(fastDummies)
df <- dummy_cols(df, select_columns = "House_Type_Encoded", remove_first_dummy = TRUE)
df <- dummy_cols(df, select_columns = "County_Encoded", remove_first_dummy = TRUE)
view(df)
str(df)
df$`House Type` <- NULL
df$`House.Type` <- NULL
df$`City.County` <- NULL
df$No..of.Receptions <- NULL
df$No..of.Bathrooms <- NULL
df$County_Encoded_NA <- NULL
df <- na.omit(df)
df <- df[df$Price <= 25000000, ]
df <- df[df$No..of.Bedrooms < 7, ]
summary(df$No..of.Bedrooms)
library(corrplot)
my_cormatrix = round(cor(df),2)
my_cormatrix
summary(df)
str(df)
head(df)
missing_bedrooms <- sum(is.na(df$No..of.Bedrooms))
cat("Number of missing values in No..of.Bedrooms:", missing_bedrooms, "\n")
unique_bedrooms <- unique(df$No..of.Bedrooms)
cat("Unique values in No..of.Bedrooms:", unique_bedrooms, "\n")
df <- df[df$No..of.Bedrooms != 0, ]
model <- lm(Price ~ Area.in.sq.ft + No..of.Bedrooms + House_Type_Encoded + County_Encoded +
              House_Type_Encoded_2 + House_Type_Encoded_3 + House_Type_Encoded_4 +
              County_Encoded_2 + County_Encoded_3, data = df)
summary(model)
df$log_Price <- log(df$Price)
df$log_Area <- log(df$Area.in.sq.ft)

df <- df[, !names(df) %in% c("Price", "Area.in.sq.ft")]
model <- lm(log_Price ~ log_Area + No..of.Bedrooms + House_Type_Encoded + County_Encoded +
              House_Type_Encoded_2 + House_Type_Encoded_3 + House_Type_Encoded_4 +
              County_Encoded_2 + County_Encoded_3, data = df)
summary(model)
df$No..of.Bedrooms <- NULL
model <- lm(log_Price ~ log_Area + House_Type_Encoded + County_Encoded +
              House_Type_Encoded_2 + House_Type_Encoded_3 + House_Type_Encoded_4 +
              County_Encoded_2 + County_Encoded_3, data = df)
summary(model)
predicted_log_prices <- predict(model, df)
residuals <- df$log_Price - predicted_log_prices
library(ggplot2)
ggplot(data.frame(Predicted = predicted_log_prices, Residuals = residuals), aes(x = Predicted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Predicted Values",
       x = "Predicted Log Price",
       y = "Residuals")

ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals",
       x = "Residuals")
residuals <- df$log_Price - predicted_log_prices
rmse <- sqrt(mean(residuals^2))
cat("Root Mean Square Error (RMSE):", rmse, "\n")
summary(model)
predicted_log_prices <- predict(model, df)
comparison_df <- data.frame(
  Actual = df$log_Price,
  Predicted = predicted_log_prices
)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

ggplot(comparison_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Log Prices",
       x = "Actual Log Price",
       y = "Predicted Log Price")



