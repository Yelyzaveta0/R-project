getwd()
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)
data <- read.csv("C:/Users/Лиза/Desktop/Rstudio projects/International_Education_Costs.csv")
data
summary(data)
mean(data$Rent_USD)

#1 group by country and find the avg tuition cost
data %>%
  group_by(Country) %>%
  summarise(AvgTution=mean(Tuition_USD))

#2 top 10 most expensive cities for education

data <- data %>% clean_names()
data

#boxplot(tuition_usd ~ country, data=data, main="Tuition by Country", xlab="Tuition(USD)", ylab="Country",col="orange", horizontal=TRUE, notch=FALSE)


top_countries <- data %>%
  group_by(Country) %>%
  summarise(avg_cost=mean(Tuition_USD, na.rm=TRUE)) %>%
  arrange(desc(avg_cost)) %>%
  slice_head(n = 10) %>%
  pull(Country)


#boxplot(tuition_usd ~ country, data = filtered_data, main = "Tuition by Top 10 Countries", xlab = "Tuition (USD)", col = "orange", horizontal = TRUE, notch = FALSE)
  
table(filtered_data$Country) 



filtered_data <- data %>%
  filter(Country %in% top_countries) %>%
  mutate(Country = factor(Country, levels=top_countries))

#Plot og filtered top 10 countries with tuition + avg tuition in each country(red)
ggplot(filtered_data, aes(x=Tuition_USD, y=Country)) + geom_boxplot(fill="orange", color="black", notch=FALSE) + stat_summary(
  fun=mean, geom="point", shape=20, size=3, color="red"
) + labs(
  title = "Tuition by Top 10 countries", 
  x = "Tuition in USD",
  y = "Country"
) + theme_minimal()

#Rent expenses by top 10 countries with tuition
avg_rent <- data %>%
  group_by(Country) %>%
  summarise(avg_rent_cost = mean(Rent_USD, na.rm=TRUE)) %>%
  arrange(desc(avg_rent_cost)) %>%
  slice_head(n = 10) 

ggplot(avg_rent, aes(x = reorder(Country, avg_rent_cost), y = avg_rent_cost)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(
    title = "Average Rent Expenses (Top 10 Countries)",
    x = "Country",
    y = "Rent in USD"
  ) + theme_minimal()


#Average living index in top 10 countries
avg_living <- data %>%
  group_by(Country) %>%
  summarise(avg_living_cost = mean(Living_Cost_Index, na.rm=TRUE)) %>%
  arrange(desc(avg_living_cost)) %>%
  slice_head(n = 10)

ggplot(avg_living, aes(x = reorder(Country, avg_living_cost), y = avg_living_cost)) + geom_col(fill = "purple") +
  coord_flip() + labs(
  title = "Average Living Cost Index (Top 10 Countries)", 
  x = "Country", 
  y = "Index"
  ) + theme_minimal()
