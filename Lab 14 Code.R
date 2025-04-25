

########################### AGR 333 Lab 14 Code #############################

#### Step 1 #####

install.packages('gridExtra')

library(ggplot2)
library(gridExtra)

#### Step 2: Set working directory (edit this path for your computer) ####

setwd("~/Purdue/Spring 2025/AGR 333/Lab 14/Ag_Econ_Lab14")
getwd()  # Check your working directory

# Import WASDE data

WASDE <- read.csv("WASDE.csv") 

# Explore the dataset
head(WASDE)  
str(WASDE)

### Step 3: Visualize Corn Price, Demand, and Supply Trends Over Time ###
library(ggplot2)
library(gridExtra)


# Plot corn prices over time
g_price <- ggplot(data = WASDE, aes(x = year, y = corn_price)) + 
  geom_line(color = "blue") +
  ggtitle("Corn Prices Over Time") +
  labs(y = "Corn Price (USD)", x = "Year")

print(g_price)

# Plot corn demand (use) over time
g_demand <- ggplot(data = WASDE, aes(x = year, y = total_use)) + 
  geom_line(color = "purple") +
  ggtitle("Corn Demand Over Time") +
  labs(y = "Corn Demand (Millions of Bushels)", x = "Year")

print(g_demand)


# Plot corn supply over time
g_supply <- ggplot(data = WASDE, aes(x = year, y = total_supply)) + 
  geom_line(color = "pink") +
  ggtitle("Corn Supply Over Time") +
  labs(y = "Corn Supply (Millions of Bushels)", x = "Year")

print(g_supply)


# Arrange the plots in a grid
t_plots <- grid.arrange(g_price, g_demand, g_supply, nrow = 3)


ggsave("t_plots.png", plot = t_plots)

# Save your plot with ggsave("filename.png")


#### Step 4: Relating Price to Corn Stock-to-Use Ratio ####

WASDE$SUR <- WASDE$end_stocks / WASDE$total_use

library(ggplot2)

s_plots <- ggplot(data = WASDE, aes(x = SUR, y = corn_price)) + 
  geom_point(shape = 1, color = "blue") + 
  geom_smooth(method = "lm", color = "purple") +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio") +
  labs(y = "Corn Price (USD)", x = "Stock-to-Use Ratio (SUR)")

ggsave("s_plot.png", plot = s_plots)

##### Step 5: Linear Regression and Diagnostics #######

# Estimate the linear regression model
reg1 <- lm(corn_price ~ SUR, data = WASDE)
summary(reg1)  

coefficients <- summary(reg1)$coefficients
results <- data.frame(
  Term = rownames(coefficients),
  Estimate = coefficients[, 1],
  Std_Error = coefficients[, 2],
  t_Value = coefficients[, 3],
  P_Value = coefficients[, 4]
)


r_squared <- summary(reg1)$r.squared
n_obs <- nrow(WASDE)
additional_metrics <- data.frame(
  Term = c("R-squared", "Number of Observations"),
  Estimate = c(r_squared, n_obs),
  Std_Error = NA,
  t_Value = NA,
  P_Value = NA
)

# Add R-squared and number of observations
additional_metrics <- data.frame(
  Term = c("R-squared", "Number of Observations"),
  Estimate = c(r_squared, n_obs),
  Std_Error = NA,  # Not applicable
  t_Value = NA,    # Not applicable
  P_Value = NA     # Not applicable
)

# Combine both
results <- rbind(results, additional_metrics)
                
write.csv(results, "Regression_Results.csv", row.names = FALSE)

library(gtsummary)
tbl_regression(reg1, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

# Calculating Price Elasticity

beta_hat <- summary(reg1)$coefficients["SUR", "Estimate"]

mean_sur <- mean(WASDE$SUR)
mean_price <- mean(WASDE$corn_price)

elasticity


# Residual Analysis

summary(resid(reg1))

# Histogram of residuals
hist(resid(reg1), 
     main = "Histogram of Linear Regression Errors",
     xlab = "Linear Model Residuals")

# Residuals vs. SUR Plot: Scatterplot of errors vs SUR

ggplot(data = WASDE, aes(x = SUR, y = residuals(reg1))) +
  geom_point(shape = 1, color = "blue") +
  ggtitle("Residuals vs. Stock-to-Use Ratio (SUR)") +
  labs(y = "Residuals", x = "Stock-to-Use Ratio (SUR)") +
  theme_minimal()

#### Step 6: Non-linear Regression and Diagnostics ####

# Create the inverse of stock-to-use ratio
WASDE$SUR_Inv <- 1 / WASDE$SUR

# Regress corn prices on inverse SUR
reg1 <- lm(corn_price ~ SUR_Inv, data = WASDE)

# Examine regression results
summary(reg1)


install.packages("gtsummary")
library(gtsummary)

# Create a professional regression table

library(gtsummary)

# Generate the regression table with gtsummary
tbl_regression(reg1, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs)) %>%
  bold_p(t = 0.05)  

# Print the table
reg_table

# Elasticity Calculation (Non-linear Model)

# Residual analysis
summary(resid(reg1))
hist(resid(reg1), main="Histogram of Non-linear Regression Errors", xlab="Non-linear Model Residuals")

# Residuals vs SUR plot
ggplot(data=WASDE, aes(x=SUR, y=resid(reg1))) +
  geom_point(shape=1) +
  ggtitle("Non-linear Regression Errors vs. Stock-to-Use Ratio") +
  labs(y="Errors", x="Stock-to-Use Ratio")

###### Step 7: Estimating separate demand curves for different time periods ###

# Create a character variable denoting the two time periods, create a dummy variable for the post-2006 period, graph a scatterplot of price on SUR with unique colors and regression lines for each period
WASDE$period <- ifelse(WASDE$year >= 2006, "2006-2019", "1973-2005")
WASDE$P2006 <- as.numeric(WASDE$year >= 2006)

ggplot(data=WASDE, aes(x=SUR, y=corn_price, color=period)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Corn Prices vs. Stock-to-Use Ratio (1973–2019)") +
  labs(y="Corn Price ($)", x="Stock-to-Use Ratio")

# Run a linear regression with time period specific
reg3 <- lm(corn_price ~ SUR + P2006 + SUR:P2006, data=WASDE)

summary(reg3)


#### Step 8: Check for auto-correlation/serial correlation #######

# Collect the residuals from the last regression, create a time series of the errors with a one-year lag of the error, then regress the error terms on the lagged error terms
error <- ts(resid(reg3), start=1973, end=2019, frequency=1)   # the ts() function tells R to set the errors as a time-series 
lag_error <- lag(error, -1)                                   # the lag() function creates a one-period lag of the error term
error <- cbind(error, lag_error)                              # cbind() binds the specified vectors together as columns to create a new data frame

reg4 <- lm(error ~ lag_error, data=error)

library(gtsummary)
tbl_regression(reg4, intercept = TRUE) %>%
  add_glance_source_note(include = c(r.squared, nobs))

summary(reg4)








