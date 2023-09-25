library(tidyverse)
library(lubridate)

# Create a sequence of dates
dates <- seq(from = as.Date("1979-01-01"), to = as.Date("2023-12-31"), by = "day")
df <- data.frame(date = dates)

# Extract day, month, and year from date
df$day <- day(df$date)
df$month <- factor(month(df$date, label = TRUE), levels = month.abb)
df$year <- year(df$date)

# Step 1: Assign default values
set.seed(123)
df$ice <- runif(nrow(df), min = -2.5, max = 2.5)

# Step 2: Adjust values based on criteria
# Step 2.1: Adjust values for 2023
# Create a sequence for temperature drop from January to August in 2023
transition_2023 <- approx(x = c(as.Date("2023-01-01"), as.Date("2023-08-31")),
                          y = c(-2, -6),
                          xout = df$date[df$year == 2023 & df$month %in% c("January", "February", "March", "April", "May", "June", "July", "August")])$y

df$ice[df$year == 2023 & df$month %in% c("January", "February", "March", "April", "May", "June", "July", "August")] <- transition_2023

# Print out first few rows of the dataframe
head(df)

props = df |> 
  group_by(year, month) |> 
  summarize(meanIce = mean(ice))


ggplot(data = props, aes(x = month, y = meanIce)) + 
  stat_summary(geom = "line", aes(group = year))
