install.packages("ggplot2")
install.packages("vioplot")

# Import necessary libraries
#library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)

null_hypothesis <- "The mean number of goals scored in women's international soccer matches is the same as men's."
Alternate_Hypothesis <- "The mean number of goals scored in women's international soccer matches is greater than men's."

# Start your code here!
# Use as many cells as you like
women_data <- read_csv("women_results.csv", show_col_types = FALSE)
men_data <- read_csv("men_results.csv",show_col_types=FALSE)

women_data $ date <- as.Date(women_data $ date)
men_data $ date <- as.Date(men_data $ date)
spec(women_data)
spec (men_data)

unique (women_data$tournament)
unique (men_data $ tournament)

table (women_data $ tournament=='FIFA World Cup')
table (men_data $ tournament=='FIFA World Cup')

women_data_wc  <- women_data %>%
  filter(date >= as.Date("2002-01-01")) %>%
  filter(tournament == 'FIFA World Cup') %>%
  mutate(total_goals = home_score + away_score) 
  # summarise(count = n())

men_data_wc  <- men_data %>%
  filter(date >= as.Date("2002-01-01")) %>%
  filter(tournament == 'FIFA World Cup') %>%
  mutate(total_goals = home_score + away_score)


mean_goals_women = mean(women_data_wc $ total_goals)
mean_goals_men = mean(men_data_wc $ total_goals)
cat("Mean goals per match for women:", mean_goals_women,"\n")
cat("Mean goals per match for men:", mean_goals_men,"\n")


library(vioplot)
vioplot(women_data_wc $ total_goals, men_data_wc $ total_goals,
        names = c("Women", "Men"),
        ylab = "Total Goals per Match",
        main = "Goals per Match: Women vs Men (FIFA WC since 2002)",
        col = c("orange", "skyblue"))

t_test_result <- t.test (women_data_wc $ totla_goals,
 men_data_wc $ total_goals,
	   alternative = "greater" , var.equal = FALSE)

print(t_test_result)

if (t_test_result$p.value < 0.1) {
  cat("The null hypothesis:", null_hypothesis, "is rejeced under a level of significance (p =", round(t_test_result$p.value, 4), ")\n")
} else {
  cat("The null hypothesis:", Alternate_Hypothesis, "is accepted under a level of significance (p =", round(t_test_result$p.value, 4), ")\n")
}
