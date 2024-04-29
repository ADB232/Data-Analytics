##### 4.0 Exploratory Analysis of Sales Data #####

# 1. Load and explore the data

# Install and import necessary libraries.

library(tidyverse)
library(ggplot2)
library(moments)
library(dplyr)
library(patchwork)
library(plotly) 

if (!require("viridis")) {
  # Install the package if it's not already installed
  install.packages("viridis")
}
library("viridis")

if (!require("car")) {
  # Install the package if it's not already installed
  install.packages("car")
}  
library(car)

if (!require("lmtest")) {
  # Install the package if it's not already installed
  install.packages("lmtest")
}
library(lmtest)

if (!require("reshape2")) {
  # Install the package if it's not already installed
  install.packages("reshape2")
}
    
library(reshape2)

#setwd(dir='*****')

# Import the data set. Please choose the turtle sales csv file

sales <- read.csv(file.choose(), header = TRUE)

# Print the data frame.

print(sales)

# Check for Duplicates and null values

sales %>%
  filter(duplicated(.))

colSums(is.na(sales)) # There are 2 missing values in the year column

# Create a Rest of the World Column

sales1 <- sales %>%
  mutate(ROW_Sales = round(Global_Sales - NA_Sales - EU_Sales, 2)) %>%
  select(Product, Platform, Year, Genre, Publisher, NA_Sales, EU_Sales, ROW_Sales, Global_Sales)

head(sales1)

### We will use sales1 later in assessing the highest grossing game, platform, publisher and Genre

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 

sales2 <- select(sales1, -Year, -Genre, -Publisher)

# View the data frame.

View(sales2)
dim(sales2)

# View the descriptive statistics.

summary(sales2)

################################################################################

# 2. Review plots to determine insights into the data set.


## 2a) Scatterplots
# Create scatterplots. ggplot was used as it provides better customisation of visuals.

# Scatterplot for NA_Sales vs. Global_Sales

ggplot(data = sales2, aes(x = NA_Sales, y = Global_Sales, color = Platform)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +  # Add a best fit line for overall trend
  theme_light() +
  labs(title = "NA Sales vs Global Sales by Platform", x = "NA Sales", y = "Global Sales") +
  theme(legend.position = "right")


# Scatterplot for EU_Sales vs. Global_Sales

ggplot(data = sales2, aes(x = EU_Sales, y = Global_Sales, color = Platform)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +
  theme_light() +
  labs(title = "EU Sales vs Global Sales by Platform", x = "EU Sales", y = "Global Sales") +
  theme(legend.position = "right")

# Scatterplot for ROW_Sales vs. Global_Sales

ggplot(data = sales2, aes(x = ROW_Sales, y = Global_Sales, color = Platform)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +  # Add a best fit line for overall trend
  theme_light() +
  labs(title = "ROW Sales vs Global Sales by Platform", x = "Rest of World Sales", y = "Global Sales") +
  theme(legend.position = "right")


# Scatterplot for EU_Sales vs. NA_Sales

ggplot(data = sales2, aes(x = EU_Sales, y = NA_Sales, color = Platform)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +
  theme_light() +
  labs(title = "EU Sales vs NA Sales by Platform", x = "EU Sales", y = "NA Sales") +
  theme(legend.position = "right")

# Scatterplot for ROW_Sales vs. NA_Sales

ggplot(data = sales2, aes(x = ROW_Sales, y = NA_Sales, color = Platform)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +
  theme_light() +
  labs(title = "ROW Sales vs NA Sales by Platform", x = "ROW Sales", y = "NA Sales") +
  theme(legend.position = "right")

# Scatterplot for ROW_Sales vs. EU_Sales

ggplot(data = sales2, aes(x = ROW_Sales, y = EU_Sales, color = Platform)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 0.5) +
  theme_light() +
  labs(title = "ROW Sales vs EU Sales by Platform", x = "ROW Sales", y = "NA Sales") +
  theme(legend.position = "right")

## 2b) Histograms
# Create histograms.

# Faceted Histogram for all sales

# Reshape the data to long format

sales_long <- pivot_longer(sales2, 
                           cols = c(NA_Sales, EU_Sales, ROW_Sales, Global_Sales), 
                           names_to = "Region", values_to = "Sales")
sales_long

# Reorder so that Global_Sales is first

sales_long$Region <- factor(sales_long$Region, 
                            levels = c("Global_Sales", "NA_Sales", "EU_Sales", "ROW_Sales"))

# Create a combined histogram with facets function

ggplot(data = sales_long, aes(x = Sales)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  facet_wrap(~ Region, scales = "free_y") +
  theme_minimal() +
  labs(title = "Histograms of Sales by Region", x = "Sales", y = "Frequency")


## 2c) Boxplots

# Create a boxplot of all Platforms in terms of Global Sales

ggplot(data = sales2, aes(x = Global_Sales, y = Platform, fill = Platform)) +
  geom_boxplot() +
  theme_light() +
  labs(title = "Global Sales by Platform", x = "Global Sales", y = "Platform") +
  theme(legend.position = "none") +
  scale_fill_viridis(discrete = TRUE, option = "C") 


# Create a boxplot of the top 10 platforms. Group by platform and summarize global sales

platform_sales <- sales2 %>%
  group_by(Platform) %>%
  summarise(NA_Sales_Sum = sum(NA_Sales),
            EU_Sales_Sum = sum(EU_Sales),
            ROW_Sales_Sum = sum(ROW_Sales),
            Global_Sales_Sum = sum(Global_Sales)) %>%
  arrange(desc(Global_Sales_Sum)) %>%
  slice_head(n = 10) 


# View the results
print(platform_sales)

# Filter the sales2 df to contain only the top 10 platforms.

filtered_data <- sales2 %>%
  filter(Platform %in% platform_sales$Platform)

# Create boxplots
ggplot(filtered_data, aes(x = Global_Sales, y = Platform, fill = Platform)) +
  geom_boxplot() +
  theme_light() +
  labs(title = "Global Sales by Top Platforms", x = "Global Sales", y = "Platform") +
  scale_fill_viridis(discrete = TRUE, option = "C")

# There is a significant outlier in the Wii platform at 67.8 in global sales. Let's identify the outlier...

sales_ordered <- sales2 %>%
  arrange(desc(Global_Sales))


# View the top entries, which are your outliers on the high side
head(sales_ordered)

# The product is 107 and attracted more than double the next highest grossing product in terms of global sales.


#####      Create a bar chart showing the top 10 platforms in each region    ######


# Remove Golbal sales. Filter for only top 10 platforms and groupby platform and region

sales_long_filtered <- sales_long %>%
  filter(Region != "Global_Sales")

filtered_data2 <- sales_long_filtered %>%
  filter(Platform %in% platform_sales$Platform) %>%
  group_by(Platform, Region) %>%  
  summarise(Sales = sum(Sales), .groups = 'drop') 


ggplot(filtered_data2_summarized, aes(x = reorder(Platform, Sales), y = Sales, fill = Region)) +  
  geom_col(position = 'dodge') +
  theme_light() +
  labs(title = "Regional Sales by Top Platforms", x = "Platform", y = "Genre") +
  scale_fill_viridis(discrete = TRUE, option = "C")

#####    Which is the highest grossing Genre?   #####

head(sales1)

sales_genres <- sales1 %>%
  group_by(Genre) %>% 
  summarise(Sales = sum(Global_Sales), .groups = 'drop') 

sales_genres

ggplot(sales_genres, aes(x = reorder(Genre, -Sales), y = Sales, fill= Genre)) +  
  geom_bar(stat = "identity") +
  theme_light() +
  labs(title = "Global Sales by Genre", x = "Genre", y = "Global Sales") +
  scale_fill_brewer(palette = "Set3")
  

#####    Which is the highest grossing Publisher?   #####

sales_publisher <- sales1 %>%
  group_by(Publisher) %>%
  summarise(Global_Sales_Sum = sum(Global_Sales)) %>%
  arrange(desc(Global_Sales_Sum)) %>%
  slice_head(n = 10) 

sales_publisher

ggplot(sales_publisher, aes(x = reorder(Publisher, -Global_Sales_Sum), y = Global_Sales_Sum, fill = Publisher)) +  
  # Specify the geom_bar function.
  geom_bar(stat = "identity") +
  labs(title = "Sales by Publisher", x = "Publisher", y = "Sales") +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
### Heatmaps ###

# Create heatmap df filtering for null values in the year column and grouping by Year and Platform.

platform_heatmap <- sales %>%
  filter(!is.na(Year))%>%
  group_by(Year, Platform) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  ungroup()

# Scale the axis

year_breaks <- seq(from = 1982, to = 2016, by = 5)

# Create the heatmap plot

heatmap_plot <- ggplot(platform_heatmap, aes(x = Year, y = Platform, fill = Total_Sales)) +
  geom_tile() + 
  scale_fill_gradient(low = "blue", high = "red") + 
  scale_x_continuous(breaks = year_breaks) + 
  theme_minimal() + 
  labs(title = "Heatmap of Global Sales by Platform and Year",
       x = "Year",
       y = "Platform",
       fill = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(heatmap_plot)


# Create heatmap df filtering for null values in the year column and grouping by Year and Publisher

publisher_heatmap <- sales %>%
  filter(!is.na(Year) & !is.na(Publisher)) %>%
  group_by(Year, Publisher) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  ungroup()

# Scale the axis
year_breaks <- seq(from = 1982, to = 2016, by = 5)

# Create the heatmap plot

publisher_heatmap_plot <- ggplot(publisher_heatmap, aes(x = Year, y = Publisher, fill = Total_Sales)) +
  geom_tile() + 
  scale_fill_gradient(low = "orange", high = "red") + 
  scale_x_continuous(breaks = year_breaks) + 
  theme_minimal() + 
  labs(title = "Heatmap of Global Sales by Publisher and Year",
       x = "Year",
       y = "Publisher",
       fill = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7)) 

# Print the plot
print(publisher_heatmap_plot)


# Bar plot Regional Sales over time

sales_year <- sales1 %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(NA_Sales_Sum = sum(NA_Sales),
            EU_Sales_Sum = sum(EU_Sales),
            ROW_Sales_Sum = sum(ROW_Sales),
            Global_Sales_Sum = sum(Global_Sales)) %>%
  ungroup()
sales_year

# sales_year to long format

# Reshape the data from wide to long format
sales_long <- pivot_longer(sales_year, cols = c(NA_Sales_Sum, EU_Sales_Sum, ROW_Sales_Sum),
                           names_to = "Sales_Region", values_to = "Sales")

ggplot(sales_long, aes(x = Year, y = Sales, fill = Sales_Region)) + 
  geom_col() +
  theme_minimal() + 
  labs(title = "Yearly Sales by Region",
       x = "Year",
       y = "Sales",
       fill = "Region") +
  scale_fill_viridis(discrete = TRUE, 
                     labels = c("NA_Sales_Sum" = "NA Sales", "EU_Sales_Sum" = "EU Sales", "ROW_Sales_Sum" = "ROW Sales")) +
  scale_x_discrete(limits = sales_long$Year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Pie chart of total sales by region

#Aggregate sales by region if not already aggregated
sales_summary <- sales_long %>%
  group_by(Sales_Region) %>%
  summarise(Sales = sum(Sales)) %>%
  ungroup()

sales_summary <- sales_summary %>%
  mutate(Percentage = Sales / sum(Sales) * 100)


# Create the pie chart
ggplot(sales_summary, aes(x = "", y = Sales, fill = Sales_Region)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() + 
  scale_fill_viridis(discrete = TRUE) + 
  labs(title = "Sales Distribution by Region") + 
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white")


##############################################################################

# 3. Observations and insights

# Outlined in the accompanying report




###############################################################################
###############################################################################

##### 5.0 Data Reliability and Suitability for Regression Modelling

# 1. Load and explore the data

# View data frame created in Week 4.

View(sales2)
head(sales2)

# Check output: Determine the min, max, and mean values.

sales_cols <- select(sales2, -Product, -Platform)

sapply(sales_cols, min)
sapply(sales_cols, max)
sapply(sales_cols, mean)


# View the descriptive statistics.

summary(sales_cols)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

sales_product <- sales2  %>%  
  group_by(Product)  %>%
  summarise(NA_Sales_Sum = sum(NA_Sales),
            EU_Sales_Sum = sum(EU_Sales),
            ROW_Sales_Sum = sum(ROW_Sales),
            Global_Sales_Sum = sum(Global_Sales))

# View the data frame.

head(sales_product)
view(sales_product)

# Cross check for records = 175

sales3_unique <- sales3 %>%
  distinct(Product) %>%
  nrow()
  
sales3_unique

# Explore the data frame.

dim(sales_product)

summary(sales_product)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
# There are 175 unique games. To illustrate them distinctly, 
# I created a scatter plot with plotly so that when you hover over the points the product and sales are displayed.

plot_ly(data = sales_product, x = ~Product, y = ~Global_Sales_Sum, type = 'scatter', mode = 'markers',
        text = ~Product, hoverinfo = 'text+y', marker = list(size = 10)) %>%
  layout(title = 'Global Sales per Product',
         xaxis = list(title = 'Product'),
         yaxis = list(title = 'Global Sales'))



###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

qqnorm(sales_product$NA_Sales_Sum,
       xlab='z value',
       ylab='NA Sales',
       col='blue')
qqline(sales_product$NA_Sales_Sum,
       col='red')

qqnorm(sales_product$EU_Sales_Sum,
       xlab='z value',
       ylab='EU Sales',
       col='blue')
qqline(sales_product$EU_Sales_Sum,
       col='red')

qqnorm(sales_product$ROW_Sales_Sum,
       xlab='z value',
       ylab='ROW Sales',
       col='blue')
qqline(sales_product$ROW_Sales_Sum,
       col='red')

qqnorm(sales_product$Global_Sales_Sum,
       xlab='z value',
       ylab='Global Sales',
       col='blue')
qqline(sales_product$Global_Sales_Sum,
       col='red')



## 3b) Perform Shapiro-Wilk test
# Install and import Moments.

# install.packages('moments')

# Perform Shapiro-Wilk test on all sales data.

SW_results <- sapply(sales_product[, c("NA_Sales_Sum", "EU_Sales_Sum", "ROW_Sales_Sum", "Global_Sales_Sum")], shapiro.test)

SW_results


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

# Specify the skewness and kurtosis functions.

skewness(sales_product[, c("NA_Sales_Sum", "EU_Sales_Sum", "ROW_Sales_Sum", "Global_Sales_Sum")]) 
kurtosis(sales_product[, c("NA_Sales_Sum", "EU_Sales_Sum", "ROW_Sales_Sum", "Global_Sales_Sum")])



## 3d) Determine correlation
# Determine correlation.

# NA & EU
NA_EU_cor <- cor(sales_product$NA_Sales_Sum, sales_product$EU_Sales_Sum)
NA_EU_cor

# NA & ROW
NA_ROW_cor <- cor(sales_product$NA_Sales_Sum, sales_product$ROW_Sales_Sum)
NA_ROW_cor

# NA & Global
NA_G_cor <- cor(sales_product$NA_Sales_Sum, sales_product$Global_Sales_Sum)
NA_G_cor

# EU & ROW
EU_ROW_cor <- cor(sales_product$EU_Sales_Sum, sales_product$ROW_Sales_Sum)
EU_ROW_cor

# EU & Global
EU_G_cor <- cor(sales_product$EU_Sales_Sum, sales_product$Global_Sales_Sum)
EU_G_cor

# ROW & Global
ROW_G_cor <- cor(sales_product$ROW_Sales_Sum, sales_product$Global_Sales_Sum)
ROW_G_cor

# Determine the correlation for the whole data frame.

round (cor(sales_product[, c("NA_Sales_Sum", "EU_Sales_Sum", "ROW_Sales_Sum", "Global_Sales_Sum")]),
       digits=2)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


##### Scatterplots #####

# Scatterplot for NA_Sales vs. Global_Sales

ggplot(data = sales_product, aes(x = NA_Sales_Sum, y = Global_Sales_Sum, color = Product)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 0.5) +  # Add a best fit line for overall trend
  theme_light() +
  labs(title = "NA Sales vs Global Sales by Product", x = "NA Sales", y = "Global Sales") +
  theme(legend.position = "right") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Pearson's r: %.2f", NA_G_cor), hjust = 1.5, vjust = 7, size = 3.5)

# Scatterplot for EU_Sales vs. Global_Sales

ggplot(data = sales_product, aes(x = EU_Sales_Sum, y = Global_Sales_Sum, color = Product)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 0.5) +  # Add a best fit line for overall trend
  theme_light() +
  labs(title = "EU Sales vs Global Sales by Product", x = "EU Sales", y = "Global Sales") +
  theme(legend.position = "right") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Pearson's r: %.2f", EU_G_cor), hjust = 1.5, vjust = 7, size = 3.5)

# Scatterplot for ROW_Sales vs. Global_Sales

ggplot(data = sales_product, aes(x = ROW_Sales_Sum, y = Global_Sales_Sum, color = Product)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 0.5) +  # Add a best fit line for overall trend
  theme_light() +
  labs(title = "ROW Sales vs Global Sales by Product", x = "ROW Sales", y = "Global Sales") +
  theme(legend.position = "right") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Pearson's r: %.2f", ROW_G_cor), hjust = 1.5, vjust = 7, size = 3.5)

# Scatterplot for NA_Sales vs. EU_Sales

ggplot(data = sales_product, aes(x = NA_Sales_Sum, y = EU_Sales_Sum, color = Product)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 0.5) +  # Add a best fit line for overall trend
  theme_light() +
  labs(title = "NA Sales vs EU Sales by Product", x = "NA Sales", y = "EU Sales") +
  theme(legend.position = "right") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Pearson's r: %.2f", NA_EU_cor), hjust = 1.5, vjust = 7, size = 3.5)

# Scatterplot for NA_Sales vs. ROW_Sales

ggplot(data = sales_product, aes(x = NA_Sales_Sum, y = ROW_Sales_Sum, color = Product)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 0.5) +  # Add a best fit line for overall trend
  theme_light() +
  labs(title = "NA Sales vs ROW Sales by Product", x = "NA Sales", y = "ROW Sales") +
  theme(legend.position = "right") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Pearson's r: %.2f", NA_ROW_cor), hjust = 1.5, vjust = 7, size = 3.5)

# Scatterplot for EU_Sales vs. ROW_Sales

ggplot(data = sales_product, aes(x = EU_Sales_Sum, y = ROW_Sales_Sum, color = Product)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed", size = 0.5) +  # Add a best fit line for overall trend
  theme_light() +
  labs(title = "EU Sales vs ROW Sales by Product", x = "EU Sales", y = "ROW Sales") +
  theme(legend.position = "right") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Pearson's r: %.2f", EU_ROW_cor), hjust = 1.5, vjust = 7, size = 3.5)


##### Kernel Density Plots #####

# Specify the ggplot function.
ggplot(sales_product, aes(x = Global_Sales_Sum)) +
  # Add fill colour to the function.
  geom_density(fill = 'blue', bw = 1) +
  theme_light() +
  # Specify the title.
  labs(title = "KDE Global Sales", x = "Density", y = "Global Sales")
  

# Specify the ggplot function.
ggplot(sales_product, aes(x = NA_Sales_Sum)) +
  # Add fill colour to the function.
  geom_density(fill = 'red', bw = 1) +
  theme_light() +
  # Specify the title.
  labs(title = "KDE NA Sales", x = "Density", y = "NA Sales")

# Specify the ggplot function.
ggplot(sales_product, aes(x = EU_Sales_Sum)) +
  # Add fill colour to the function.
  geom_density(fill = 'green', bw = 1) +
  theme_light() +
  # Specify the title.
  labs(title = "KDE EU Sales", x = "Density", y = "EU Sales")

# Specify the ggplot function.
ggplot(sales_product, aes(x = ROW_Sales_Sum)) +
  # Add fill colour to the function.
  geom_density(fill = 'yellow', bw = 1) +
  theme_light() +
  # Specify the title.
  labs(title = "KDE ROW Sales", x = "Density", y = "ROW Sales")


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Linear Regression

# 1. Load and explore the data
# View data frame created in Week 5.

head(sales_product)
View(sales_product)

# Determine a summary of the data frame.

summary(sales_product)

# Select only numeric columns from the original data frame.

sales_cols2 <- select(sales_product, -Product)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

sales_cor <- round(cor(sales_cols2), 2)

sales_cor

# Create a linear regression model. NA_Sales vs Global_Sales

# Fit a linear model with Global_Sales_Sum as the dependent variable
model1 <- lm(sales_product$Global_Sales_Sum ~ sales_product$NA_Sales_Sum)

model1

## 2b) Create a plot (simple linear regression)
# Basic visualisation.

# Create a scatter plot with NA_Sales_Sum on the x-axis and Global_Sales_Sum on the y-axis

plot(sales_product$NA_Sales_Sum, sales_product$Global_Sales_Sum,
     xlab = "NA Sales", ylab = "Global Sales", main = "Scatter Plot with Line of Best Fit")

# Add the line of best fit to the scatter plot
abline(model1, col = "red")


# View the summary stats.

summary(model1)


# Create a visualisation to determine normality of data set.

qqnorm(residuals(model1))
qqline(residuals(model1), col='red')

# Check for Heteroscedasticity
plot(model1$residuals)


# Create a linear regression model. NA_Sales vs EU_Sales

# Fit a linear model with NA_Sales_Sum as the dependent variable

model2 <- lm(sales_product$NA_Sales_Sum ~ sales_product$EU_Sales_Sum)

model2


# Create a scatter plot with NA_Sales_Sum on the x-axis and Eu_Sales_Sum on the y-axis

plot(sales_product$NA_Sales_Sum, sales_product$EU_Sales_Sum,
     xlab = "NA Sales", ylab = "Global Sales", main = "Scatter Plot with Line of Best Fit")

# Add the line of best fit to the scatter plot
abline(model2, col = "red")


# View the summary stats.

summary(model2)


# Create a visualisation to determine normality of data set.

qqnorm(residuals(model2))
qqline(residuals(model2), col='red')

# Check for Heteroscedasticity
plot(model2$residuals)




###############################################################################

# 3. Create a multiple linear regression model

sales_cols2

# Multiple linear regression model.

# Create a new object and 
# specify the lm function and the variables.

MLR1 = lm(Global_Sales_Sum ~ NA_Sales_Sum+EU_Sales_Sum, data=sales_cols2)

summary(MLR1)


# Split the data into training/test

set.seed(123) 
indices <- sample(1:nrow(sales_cols2), size = 0.8 * nrow(sales_cols2))

train_set <- sales_cols2[indices, ]
test_set <- sales_cols2[-indices, ]

MLR1 <- lm(Global_Sales_Sum ~ NA_Sales_Sum + EU_Sales_Sum, data = train_set)

summary(MLR1)

test_set$predicted_Global_Sales_Sum <- predict(MLR1, newdata = test_set)

# Calcualte mean squared error

rmse <- sqrt(mean((test_set$Global_Sales_Sum - test_set$predicted_Global_Sales_Sum)^2))

print(rmse)

# Calculate residuals
residuals <- test_set$Global_Sales_Sum - test_set$predicted_Global_Sales_Sum

# Calculate SSR
SSR <- sum(residuals^2)

# Calculate SST
SST <- sum((test_set$Global_Sales_Sum - mean(test_set$Global_Sales_Sum))^2)

# Calculate R-squared
R_squared <- 1 - (SSR/SST)

# Print R-squared
print(R_squared)


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

new_sales_data <- data.frame(
  NA_Sales_Sum = c(34.02, 3.93, 2.73, 2.26, 22.08), 
  EU_Sales_Sum = c(23.80, 1.56, 0.65, 0.97, 0.52)  
)

new_sales_data

# Make predictions

predictions <- predict(MLR1, newdata = new_sales_data)

# Viewing the predictions

new_sales_data$Predicted_Global_Sales_Sum <- round(predictions, 2)

new_sales_data

######## Check for Normal Distribution of residuals ##########

shapiro_test_result <- shapiro.test(residuals(MLR1))

# Print the results
print(shapiro_test_result)

qqnorm(residuals(MLR1))
qqline(residuals(MLR1), col = "red")

######## Check for Heteroscedasticity ########

# Extract residuals
residuals <- residuals(MLR1)

# Extract fitted values
fitted_values <- fitted(MLR1)

# Plot residuals vs. fitted values

plot(fitted_values, residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")

plot(MLR1)

###### Check for Muli-colinearity #######

vif_results <- vif(MLR1)
print(vif_results)

###### Check for Autocorrelation ######

dw_test_result <- dwtest(MLR1)

dw_test_result

#### Apply log function and re-run model ######

sales_cols3 <- mutate(sales_cols2, 
                      log_Global_Sales_Sum=log(Global_Sales_Sum))

sales_cols3

MLR2 <- lm(log_Global_Sales_Sum ~ NA_Sales_Sum + EU_Sales_Sum, data = sales_cols3)

summary(MLR2)

shapiro.test(residuals(MLR2))

qqnorm(residuals(MLR2))
qqline(residuals(MLR2), col = "red")



###############################################################################
###############################################################################




