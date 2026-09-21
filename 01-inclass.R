#--------------------------------------------------
# Week 1: In-class assignment
#--------------------------------------------------

# There is no one correct way to write the code to answer the questions
# But your code needs to 
# a. answer the question
# b. be fully reproducible

# For this assignment, we will use 
# the `yrbss` data 
# in the `openintro` package 

install.packages("openintro")
library(openintro)

# Other useful packages
install.packages("tidyverse")
library(tidyverse)

# Read the documentation for `yrbss` to learn about all the variables.
?yrbss

# The code below uses the `flextable` package to create a table of summary characteristics of
# Grade and Gender
# Modify the code below such that the grade shows in increasing order
# and all category labels start with a capital letter

install.packages("flextable")
library(flextable)

yrbss$Grade <- yrbss$grade
yrbss$Gender <- yrbss$gender

z <- summarizor(
  yrbss[c("Grade", "Gender")],
  overall_label = NULL
)
ft_1 <- as_flextable(z) 
ft_1


# To understand the pattern of physical activity by grade and gender,
# 1) aggregate  `physically_active_7d` by calculating its mean within each grade and gender
# 2) create a plot showing the average number of physically active days
#      x-axis: grade
#      y-axis: Mean of `physcially_active_7d`
#      Distinguish gender using different colors, symbols, or lines
# *** I would use the following functions: aggregate(), ggplot(), geom_line() but there is 
# no one correct way to do this
# Ensure that the figure is clearly labeled and includes an appropriate legend

yrbss$grade <- factor(yrbss$grade, levels = c("9", "10", "11", "12", "other"))

aggregate(physically_active_7d ~ grade + gender,
  yrbss, mean, na.rm = TRUE) |>
  ggplot(aes(x = grade, y = physically_active_7d,
             color = gender, group = gender)) +
  geom_line() +
  labs(x = "Grade", y = "Mean # of Physically Active Days",
    color = "Gender", title = "Mean Number of Physically Active Days by Grade and Gender")

# Create a plot that shows the relationship betwen physical activity and bmi
# among female students in grade 12 
# Ensure that the figure is clearly labeled and includes an appropriate legend

yrbss$bmi <- yrbss$weight / (yrbss$height^2)

grade12_f <- yrbss |>
  filter(grade == "12", gender == "female")

ggplot(grade12_f, aes(x = bmi, y = physically_active_7d)) +
  geom_point(color = "skyblue") +
  labs(x = "BMI", y = "Physically Active Days in the Past 7 Days",
    title = "Physical Activity and BMI Among Female Grade 12 Students")

# Push your completed code to your GitHub repository