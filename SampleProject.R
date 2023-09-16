
# This project uses imaginary data from a stratified random experiment conducted
# in Kenya to understand if tracking in classrooms have an impact on learning 
# outcomes.

# The project is composed of 3 parts: Data preparation, data analysis, and visualizations.

# Installing packages necessary for this exercise

install.packages("haven")
install.packages("readxl")
install.packages("stringr")
install.packages("stargazer")
install.packages("clubSandwich")
install.packages("writexl")
library(writexl)
library(clubSandwich)
library(stargazer)
library(haven)
library(readxl)
library(stringr)
library(dplyr)
library(sandwich)

# Reading the Datasets into R
# The student_test_data is named student_data and teacher_data is named teacher_data

student_data <- read_excel("Downloads/for_candidates/student_test_data.xlsx")
teacher_data <- read_dta("Downloads/for_candidates/teacher_data.dta")

#===============================================================================
#============================DATA PREPARATION===================================
#===============================================================================

# Recoding the arithmetic scores in the student_data as numeric rather than string 

# There are 4 unique variables in the original aX_correct columns ".", "NONE, "1", "2", "3". 
# Before recoding the arithmetic scores as numeric, "NONE" string is replaced with "0".
# If this step is not performed "NONE" will be recoded as NA when passed into the as.numeric function
# Note: "." will be recoded as NA because it is assumed a missing value.

aX_correct <- c("a1_correct", "a2_correct", "a3_correct", "a4_correct",
                "a5_correct", "a6_correct", "a7_correct", "a8_correct")

for (aX in aX_correct) {
  student_data[[aX]] <- str_replace(student_data[[aX]], "NONE", "0")
}


# Recoding the arithmetic scores as numeric

for (aX in aX_correct) {
  student_data[[aX]] <- as.numeric(student_data[[aX]])
}


#=================================

# Dropping rows where input for all w_correct, w_incorrect, w_missing, 
# s_correct, s_incorrect, s_missing are missing: 
# These rows are coded as -99 in the raw data.
# Note: If the values are completely missing in either section, the below code will 
# drop the observations from the student_data data frame

student_data <-student_data[((student_data$w_correct != -99 | student_data$w_incorrect != -99 | 
                                student_data$w_missing != -99 ) & ( student_data$s_correct != -99 |
                                                                      student_data$s_incorrect != -99 | student_data$s_missing != -99)), ]

#================================

# Generating variable total_score which is a summation of all the correct 
# reading, spelling, and arithmetic scores. 
# This variable will be stored in the student_data data frame
# Given that spelling and arithmetic scores contain missing values, this variable
# will also contain missing values.

student_data$total_score <- (student_data$w_correct + student_data$s_correct + student_data$a1_correct + 
                               student_data$a2_correct + student_data$a3_correct + student_data$a4_correct + 
                               student_data$a5_correct + student_data$a6_correct + student_data$a7_correct +
                               student_data$a8_correct + student_data$spelling_correct)
#=================================

# d) Generating a variable for letter grade called letter_grade
# The variable will be stored in student_data data frame 
# Note: Given that this variable is generated from total_score, it will contain
# missing values.

student_data <- student_data %>%
  mutate(letter_grade = case_when(
    total_score >= 80 & total_score <= 98 ~ "A",
    total_score >= 60 & total_score <= 79 ~ "B",
    total_score >= 40 & total_score <= 59 ~ "C",
    total_score >= 20 & total_score <= 39 ~ "D",
    total_score >= 0 & total_score <= 19 ~ "F"
  ))

#==================================

# e) Standardizing the total score, relative to the control group.

# Additional Step
# The total_score variable has 54 missing values. Given that it might bias the 
# analysis if these scores were replaced with estimates and given the large sample
# size, the observations associated with the missing total_score variable are 
# dropped. The letter_grade variable also has an additional missing value that is 
# due to incorrect coding (the total score was negative). This observation will
# also be dropped.
# Note: Given that this is a stratified random experiment, it is advised to also
# check the weighted propensity scores after omitting observations. I did not have
# time to do this. 

student_data <- student_data[complete.cases(student_data$total_score), ]
student_data <- student_data[complete.cases(student_data$letter_grade), ]

# Calculating the mean of the control group

mean_control <- mean(student_data[(student_data$tracking_assigned == 0), ]$total_score)

# Calculating the standard deviation of the control group

sd_control <- sd(student_data[(student_data$tracking_assigned == 0), ]$total_score)

# Generating z-scores for each observation, relative to the control group
# and storing them as a variable named z_score in student_data

student_data$z_score <- ((student_data$total_score - mean_control) / sd_control)

#================================

# Computing the average years of experience by school and creating new 
# data set named avg_experience where the average years of teacher experience 
# by school is stored.

# if the yrstaught variable is coded as -99, it is recoded to NA in order to
# specify that it is a missing variable.

teacher_data$yrstaught[teacher_data$yrstaught == -99] <- NA

# Averaging "yrstaught" variable relative to "schoolid" and consolidating it in a 
# data set named avg_experience

avg_experience <- teacher_data %>%
  group_by(schoolid) %>%
  summarize(yrstaught = if (all(is.na(yrstaught))) NA else mean(yrstaught))

avg_experience <- rename(avg_experience, avg.yrstaught = yrstaught)

#================================

# h) Merging student_data and avg_experience together relative to school id.
# the new data set is named merged_data

merged_data <- left_join(student_data, avg_experience, by= "schoolid")

#=================================
# Creating a total_math_score variable in merged_data 

merged_data$total_math_score <- (merged_data$a1_correct + merged_data$a2_correct +
                                   merged_data$a3_correct + merged_data$a4_correct +
                                   merged_data$a5_correct + merged_data$a6_correct +
                                   merged_data$a7_correct + merged_data$a7_correct +
                                   merged_data$a8_correct)

# Creating dummy variables for zone and storing them in merged_data as "d" for 
# regression readjustment in data analysis section. 

merged_data$d <- model.matrix(~ zone - 1, data = merged_data)


#===============================================================================
#============================DATA ANALYSİS======================================
#===============================================================================

# a) Regressing the standardized total score on the "assigned to tracking" variable.
# Since the independent variable is binary and the dependent variable is continuous,
# a linear regression with intercept will be employed.

linear_treatment_model <- lm(z_score ~ tracking_assigned, merged_data)
print(summary(linear_treatment_model))

# According to the linear regression model, the effect of being assigned to tracking (treatment) 
# is an estimated increase in the total score by 0.1578 standard deviations (or, 2.57 raw score). 
# Hence, the point estimate shows a positive relationship between treatment and test scores. 
# The point estimate / coefficient estimate is also statistically significant as the p-value 
# for the test statistic is 1.88e-09 – approximately 0. In other words, we can reject 
# the null hypothesis with close to absolute certainty. The null hypothesis in this situation 
# would have stated that the test scores for both control and treatment groups are equal 
#and any variation in observations is random. Obviously, this regression does not take
# into consideration any confounding variables and also does not question whether test scores 
# are actually a good indicator for performance in school.

#============================

# c) Regression Readjustment: Regressing the standardized total score on 
# "assigned to tracking", "girl", "d" (administrative zone dummy variable), and
# the interaction between "girl" and "assigned to tracking". 


readjusted_model <- lm(z_score ~ tracking_assigned +  
                         d + (girl * tracking_assigned) ,
                       data = merged_data, )
summary(model)

#The assignment to tracking variable was randomly assigned to schools which were 
# stratified based on administrative zone, which would in theory mitigate biases due to the location of the students. 
# However, different schools might have different sizes. For example, let’s say that the treatment was randomly
# assigned to a comparatively large school in one of the wealthier districts. Since we run the regression on
# individual student performances and more students from the wealthier district are assigned to treatment (because of the school size),
# we might see positive bias. For this reason, I have created dummy variables for administrative zone and 
# will include them as a fixed effect in the new regression.   

# Gender might have also posed an issue, because of the existence of same-gender schools in Kenya.
# Also, more domain knowledge on how gender effects performance in tests is needed to examine if it can be a 
# confounding factor and also if different genders react the same to tracking in school classrooms. 
# I will include gender in my new regression as a fixed effect and interaction term to observe 
# how gender affects the outcome and how assignment to treatment varies based on gender. 

# Average years of experience can also create biases since a school assigned to treatment 
# might be performing better because of teacher experience. However, there are a lot of missing data 
# on average teaching experience in schools. If I include the variable in the regression, 
# the regression will exclude 2088 observations which might create more biases than it hopes to mitigate. 
# In order to solve the missing value problem, we can replace the missing values with estimates.
# However, due to the time constraints of the exercise and my lack of domain knowledge on teacher experiences
# in various administrative zones in Kenya, I have opted to not calculate the missing values and 
# not include average years of experience in my readjusted regression.

# In the readjusted regression, the effect of being assigned to treatment is an estimated 
# increase in the total score by 0.14984 standard deviations, slightly lower than the first regression; 
# however, it still signals a positive relationship between treatment and outcome. 
# With a p-value of 3.88e-05 on the t statistics, this point estimate is statistically significant. 
# The regression illustrates that certain zones have statistically significant effects on the test scores, 
# which demonstrates that they are confounding factors. 
# Overall, the readjusted regression confirms our prior results. 

#=============================

# Regression Readjustment: Employing cluster robust standard error for previous
# regression models

clustered_simple <- coef_test(linear_treatment_model, vcov = "CR1", 
                              cluster = merged_data$schoolid, test = "naive-t")[1:2,]
clustered_readjusted <- coef_test(readjusted_model, vcov = "CR1", 
                                  cluster = merged_data$schoolid, test = "naive-t")[1:2,]
clustered_simple
summary(linear_treatment_model)

# In order to mitigate the bias of group effects on standard error,
# I use cluster robust standard error in my regression.
# When we use clustered standard error on the first simple regression, 
# the SE is 0.08 significantly higher than its previous model and the estimate
# is no longer statistically significant for a p-value cutoff of 0.05.
# When the bluster robust standard error is employed for the readjusted regression 
# that includes gender and zone as independent variables, the standard error associated 
# with the tracking_assigned coefficient estimate slightly increases. 
# This indicates that students from the same school are acting similarly. 
# Furthermore, while the tracking_assigned estimate was  significant at a level of 0.0001 
# without the cluster robust standard error, with the robust standard error it is only significant at a level of 0.05. 
# These findings indicate that there is a positive relationship between assignment to tracking and test scores. 
# However, given that there are multiple subgroups in this analysis,
# 0.05 might not be an adequate cutoff to reject the null hypothesis. 
# Further research and domain knowledge is needed to determine if 0.05 is an adequate cutoff.



#===============================================================================
#=============================VISUALIZATIONS====================================
#===============================================================================

#Regression Table

Regression_Table <- stargazer(linear_treatment_model, readjusted_model, title = "Linear Regression: The Effect of Tracking in Classrooms on Test Scores",
                              out = "regression_table.txt", type = "text")



# Bar chart for the average non-standardized total math score 
# for each of the two districts, differentiated by assignment to treatment

unique(merged_data$district)
# Calculating the average math score in the BUNGOMA district for those assigned 
# to treatment
bungoma_treatment_mean <- mean(merged_data[(merged_data$tracking_assigned == 1) & 
                                             (merged_data$district == "BUNGOMA"),]$total_math_score)

# Calculating the average math score in the BUNGOMA district for those NOT assigned 
# to treatment

bungoma_control_mean <- mean(merged_data[(merged_data$tracking_assigned == 0) & 
                                           (merged_data$district == "BUNGOMA"),]$total_math_score)

# Calculating the average math score in the BUTERE M district for those assigned 
# to treatment

butere_treatment_mean <- mean(merged_data[(merged_data$tracking_assigned == 1) & 
                                            (merged_data$district == "BUTERE/M"),]$total_math_score)

# Calculating the average math score in the BUTERE M district for those NOT assigned 
# to treatment

butere_control_mean <- mean(merged_data[(merged_data$tracking_assigned == 0) & 
                                          (merged_data$district == "BUTERE/M"),]$total_math_score)

averages <- c(bungoma_control_mean, bungoma_treatment_mean, butere_control_mean, butere_treatment_mean)
names<- c("NOT ASSIGNED - BUNGOMA", "ASSIGNED - BUNGOMA", "NOT ASSIGNED - BUTERE M", "ASSIGNED - BUTERE M")

par(mar = c(5, 6, 7, 2))

barplot(averages, names.arg = names, col = c("orange", "orange","lightblue", "lightblue"),
        main = "",
        xlab= "", ylab = "")

#Adding titles etc.

title(xlab = expression(bold(underline("District Name and Assignment to Tracking"))), 
      col.lab = "darkgray", cex.lab = 1.4)
title(ylab = expression(bold(underline("Average Math Score out of 24"))), col.lab = "darkgray", cex.lab = 1.4)
title(main = expression(bold("Non-Standardized Average Math Scores Relative to District and Assigment to Tracking")), 
      cex.main = 1.5)
axis(side = 2, at = seq(0, 9 , by = 1 ))      
abline(h = 0, col = "black")

text(x = barplot(averages, plot = FALSE), y = averages, labels = round(averages, 2), pos = 1, cex = 1, font= 2) 


write_xlsx(merged_data, path = "/Downloads/merged_data.xlsx")
write.csv(merged_data, file = "my_data.csv", row.names = FALSE)
