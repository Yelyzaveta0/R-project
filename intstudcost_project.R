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
  group_by(country) %>%
  summarise(avg_cost=mean(tuition_usd, na.rm=TRUE)) %>%
  arrange(desc(avg_cost)) %>%
  slice_head(n = 10) %>%
  pull(country)


#boxplot(tuition_usd ~ country, data = filtered_data, main = "Tuition by Top 10 Countries", xlab = "Tuition (USD)", col = "orange", horizontal = TRUE, notch = FALSE)
  
table(filtered_data$country) 



filtered_data <- data %>%
  filter(country %in% top_countries) %>%
  mutate(country = factor(country, levels=top_countries))

#Plot og filtered top 10 countries with tuition + avg tuition in each country(red)
ggplot(filtered_data, aes(x=tuition_usd, y=country)) + geom_boxplot(fill="orange", color="black", notch=FALSE) + stat_summary(
  fun=mean, geom="point", shape=20, size=3, color="red"
) + labs(
  title = "Tuition by Top 10 countries", 
  x = "Tuition in USD",
  y = "Country"
) + theme_minimal()
