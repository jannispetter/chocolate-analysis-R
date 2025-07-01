library(tidyverse)
library(janitor)
library(readr)
library(dplyr)
install.packages("janitor")

Chocolate ->d1

#Question 1

#removing N/A
d1_clean <- d1 %>% na.omit()

# View the cleaned data
glimpse(d1_clean)


#renaming columns
d2 <- d1_clean %>%
  rename(
    sales_verhouten = sales,
    price_verhouten = price1,
    price_droste = price2,
    price_baronie = price3,
    price_delicata = price4,
    
    feature_verhouten = feature1,
    feature_droste = feature2,
    feature_baronie = feature3,
    feature_delicata = feature4,
    
    display_verhouten = display1,
    display_droste = display2,
    display_baronie = display3,
    display_delicata = display4,
    
    comb_verhouten = fand1,
    comb_baronie = fand3,
    comb_delicata = fand4 )

#summary statistics

install.packages("skimr")

# Load the skimr package
library(skimr)

skim(d2)


#scatter plot

library(ggplot2)

# Create scatter plot with linear trend line
ggplot(d2, aes(x = price_verhouten, y = sales_verhouten)) +
  geom_point(color = "darkblue", alpha = 0.7) +  # scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # regression line
  labs(
    title = "Scatter Plot: Price vs. Sales (Verhouten 100g)",
    x = "Price (€)",
    y = "Sales (hundreds of kilos)"
  ) +
  theme_minimal()


# Correlation coefficient (Pearson)
cor.test(d2$price_verhouten, d2$sales_verhouten, method = "pearson")

#Question2

#adding trend
d2 <- d2 %>%
  arrange(week) %>%
  mutate(
    time = 1:n()
  )

#log-log model with trend
model_with_trend <- lm(log(sales_verhouten) ~ log(price_verhouten) + time, data = d2)
summary(model_with_trend)

# Estimate log-log model without trend
model_price_elasticity <- lm(log(sales_verhouten) ~ log(price_verhouten), data = d2)

summary(model_price_elasticity)


#plot trend
# Predict fitted values from your model
d2$fitted_sales <- exp(fitted(model_with_trend))  # convert log-scale back to level

# Plot actual vs. fitted sales over time
ggplot(d2, aes(x = time)) +
  geom_line(aes(y = sales_verhouten), color = "blue", size = 1, alpha = 0.7) +
  geom_line(aes(y = fitted_sales), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Actual vs. Fitted Sales (Verhouten)",
    x = "Time (Weeks)",
    y = "Sales (Hundreds of Kilos)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Question 3

# Create seasonal dummy variables
# weeks are based on context

# Step 1: 
d2 <- d2 %>%
  mutate(
    saint_nicolas = ifelse(week %in% c(49), 1, 0),
    christmas = ifelse(week %in% c(51, 52), 1, 0),
    easter = ifelse(week %in% c(14), 1, 0)
  )

#step 2: transform temp to Fahrenheit bc it has - values which cant be logged
d2 <- d2 %>%
  mutate(temp_fahrenheit = (temp * 9/5) + 32,
         log_tempf = log(temp_fahrenheit))


#regression without comb
model_controls <- lm(log(sales_verhouten) ~ 
                       log(price_verhouten) +
                       log_tempf + 
                       saint_nicolas + christmas + easter +
                       feature_verhouten + display_verhouten,
                     data = d2)

summary(model_controls)

#regression with comb

model_controls2 <- lm(log(sales_verhouten) ~ 
                       log(price_verhouten) +
                       log_tempf + 
                       saint_nicolas + christmas + easter +
                       feature_verhouten + display_verhouten + comb_verhouten,
                     data = d2)

summary(model_controls2)

#APA
install.packages("performance")
library(apaTables)
library(sjPlot)
library(broom)
library(ggplot2)

apa.reg.table(model_controls2, filename = "regression_tableq3.doc")


#Question 4
#adding other variables without comb
model_competition <- lm(log(sales_verhouten) ~ 
                          log(price_verhouten) + 
                          log_tempf + 
                          saint_nicolas + christmas + easter +
                          feature_verhouten + display_verhouten +
                          log(price_droste) + log(price_baronie) + log(price_delicata) +
                          feature_droste + feature_baronie + feature_delicata +
                          display_droste + display_baronie + display_delicata,
                        data = d2)

summary(model_competition)

#adding comb on top
model_competition <- lm(log(sales_verhouten) ~ 
                          log(price_verhouten) + 
                          log_tempf + 
                          saint_nicolas + christmas + easter +
                          feature_verhouten + display_verhouten +
                          log(price_droste) + log(price_baronie) + log(price_delicata) +
                          feature_droste + feature_baronie + feature_delicata +
                          display_droste + display_baronie + display_delicata + comb_baronie+ comb_delicata +comb_verhouten,
                        data = d2)

summary(model_competition)

#without comp prices logged
model_competition <- lm(log(sales_verhouten) ~ 
                          log(price_verhouten) + 
                          log_tempf + 
                          saint_nicolas + christmas + easter +
                          feature_verhouten + display_verhouten +
                          price_droste + price_baronie + price_delicata +
                          feature_droste + feature_baronie + feature_delicata +
                          display_droste + display_baronie + display_delicata + comb_baronie+ comb_delicata +comb_verhouten,
                        data = d2)

summary(model_competition)

#Question 5
#a)
d2 <- d2 %>%
  arrange(week) %>%
  mutate(
    lag_price = lag(log(price_verhouten), 1),
    lead_price = lead(log(price_verhouten), 1)
  )

modelquestion5 <- lm(log(sales_verhouten) ~ 
                            log(price_verhouten) + 
                            lag_price + 
                            lead_price,
                          data = d2)

summary(modelquestion5)
