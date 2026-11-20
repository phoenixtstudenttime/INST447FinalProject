library(tidyverse)
library(ggpubr)
library(ggplot2)
library(rstatix)

#this is for me to set up my directory on my computer, you can ignore it!!!!
#setwd("~/INST447/TeamProject")
#dir("~/INST447/TeamProject")
#getwd()

#import the data
#download.file("education_career_success.csv")
file.exists("education_career_success.csv")
file.access("education_career_success.csv")
read.csv("education_career_success.csv")
#save in a dataframe
successdata <- read_csv("education_career_success.csv")

#Calculate the mean, median and mode for a column

#Academic Performance
#HS GPA:
mean(successdata$High_School_GPA, na.rm = TRUE)
median(successdata$High_School_GPA, na.rm = TRUE)
#Mode for HS GPA
hsvector <- successdata$High_School_GPA
unique_hs <- unique(hsvector)
hspositions <- match(hsvector, unique_hs)
hs_freq <- tabulate(hspositions) #calculate frequencies
hs_maxfreq <- which.max(hs_freq)
unique_hs[hs_maxfreq]
hs_calculate_mode <- function(hsvector){
  unique_hs <- unique(hsvector)
  unique_hs[which.max(tabulate(match(hsvector, unique_hs)))]
} #create a function for mode
hs_calculate_mode(hsvector)

#SAT Score:
mean(successdata$SAT_Score, na.rm = TRUE)
median(successdata$SAT_Score, na.rm = TRUE)
#Mode for SAT score
sat_vector <- successdata$SAT_Score
unique_sat <- unique(sat_vector)
sat_positions <- match(sat_vector, unique_sat)
sat_freq <- tabulate(sat_positions) #calculate frequencies
sat_maxfreq <- which.max(sat_freq)
unique_sat[sat_maxfreq]
sat_calculate_mode <- function(sat_vector){
  unique_sat <- unique(sat_vector)
  unique_sat[which.max(tabulate(match(sat_vector, unique_sat)))]
} #create a function for mode
sat_calculate_mode(sat_vector)

#University GPA:
mean(successdata$University_GPA, na.rm = TRUE)
median(successdata$University_GPA, na.rm = TRUE)
uni_vector <- successdata$University_GPA
unique_uni <- unique(uni_vector)
uni_positions <- match(uni_vector, unique_uni)
uni_freq <- tabulate(uni_positions) #calculate frequencies
uni_maxfreq <- which.max(uni_freq)
unique_uni[uni_maxfreq]
uni_calculate_mode <- function(uni_vector){
  unique_uni <- unique(uni_vector)
  unique_uni[which.max(tabulate(match(uni_vector, unique_uni)))]
} #create a function for mode
uni_calculate_mode(uni_vector)

#Experience
#Internships Completed
mean(successdata$Internships_Completed, na.rm = TRUE)
median(successdata$Internships_Completed, na.rm = TRUE)
intern_vector <- successdata$Internships_Completed
unique_intern <- unique(intern_vector)
intern_positions <- match(intern_vector, unique_intern)
intern_freq <- tabulate(intern_positions) #calculate frequencies
intern_maxfreq <- which.max(intern_freq)
unique_intern[intern_maxfreq]
intern_calculate_mode <- function(intern_vector){
  unique_intern <- unique(intern_vector)
  unique_intern[which.max(tabulate(match(intern_vector, unique_intern)))]
} #create a function for mode
intern_calculate_mode(intern_vector)

#Projects Completed
mean(successdata$Projects_Completed, na.rm = TRUE)
median(successdata$Projects_Completed, na.rm = TRUE)
project_vector <- successdata$Projects_Completed
unique_project <- unique(project_vector)
project_positions <- match(project_vector, unique_project)
project_freq <- tabulate(project_positions) #calculate frequencies
project_maxfreq <- which.max(project_freq)
unique_project[project_maxfreq]
project_calculate_mode <- function(project_vector){
  unique_project <- unique(project_vector)
  unique_project[which.max(tabulate(match(project_vector, unique_project)))]
} #create a function for mode
project_calculate_mode(project_vector)

#Certifications
mean(successdata$Certifications, na.rm = TRUE)
median(successdata$Certifications, na.rm = TRUE)
certificate_vector <- successdata$Certifications
unique_certificate <- unique(certificate_vector)
certificate_positions <- match(certificate_vector, unique_certificate)
certificate_freq <- tabulate(certificate_positions) #calculate frequencies
certificate_maxfreq <- which.max(certificate_freq)
unique_certificate[certificate_maxfreq]
certificate_calculate_mode <- function(certificate_vector){
  unique_certificate <- unique(certificate_vector)
  unique_certificate[which.max(tabulate(match(certificate_vector, unique_certificate)))]
} #create a function for mode
certificate_calculate_mode(certificate_vector)

#Job Success
#Job Offers
mean(successdata$Job_Offers, na.rm = TRUE)
median(successdata$Job_Offers, na.rm = TRUE)
job_vector <- successdata$Job_Offers
unique_job <- unique(job_vector)
job_positions <- match(job_vector, unique_job)
job_freq <- tabulate(job_positions) #calculate frequencies
job_maxfreq <- which.max(job_freq)
unique_job[job_maxfreq]
job_success_calculate_mode <- function(job_vector){
  unique_job <- unique(job_vector)
  unique_job[which.max(tabulate(match(job_vector, unique_job)))]
} #create a function for mode
job_success_calculate_mode(job_vector)

#Starting Salary
mean(successdata$Starting_Salary, na.rm = TRUE)
median(successdata$Starting_Salary, na.rm = TRUE)
salary_vector <- successdata$Starting_Salary
unique_salary <- unique(salary_vector)
salary_positions <- match(salary_vector, unique_salary)
salary_freq <- tabulate(salary_positions) #calculate frequencies
salary_maxfreq <- which.max(salary_freq)
unique_salary[salary_maxfreq]
salary_calculate_mode <- function(salary_vector){
  unique_salary <- unique(salary_vector)
  unique_salary[which.max(tabulate(match(salary_vector, unique_salary)))]
} #create a function for mode
salary_calculate_mode(salary_vector)

#Current Job Level (Not numeric)
mean(successdata$Current_Job_Level, na.rm = TRUE)
median(successdata$Current_Job_Level, na.rm = TRUE)
current_job_vector <- successdata$Starting_Salary
unique_current_job <- unique(current_job_vector)
current_job_positions <- match(current_job_vector, unique_current_job)
current_job_freq <- tabulate(current_job_positions) #calculate frequencies
current_job_maxfreq <- which.max(current_job_freq)
unique_current_job[current_job_maxfreq]
current_job_calculate_mode <- function(current_job_vector){
  unique_current_job <- unique(current_job_vector)
  unique_current_job[which.max(tabulate(match(current_job_vector, unique_current_job)))]
} #create a function for mode
current_job_calculate_mode(current_job_vector)


#calculate standard deviation, variance, min, max, range and quantile
#HS GPA:
sd(successdata$High_School_GPA, na.rm = TRUE)
var(successdata$High_School_GPA, na.rm = TRUE)
min(successdata$High_School_GPA, na.rm = TRUE)
max(successdata$High_School_GPA, na.rm = TRUE)
range(successdata$High_School_GPA, na.rm = TRUE)
quantile(successdata$High_School_GPA, na.rm = TRUE)
#SAT Score
sd(successdata$SAT_Score, na.rm = TRUE)
var(successdata$SAT_Score, na.rm = TRUE)
min(successdata$SAT_Score, na.rm = TRUE)
max(successdata$SAT_Score, na.rm = TRUE)
range(successdata$SAT_Score, na.rm = TRUE)
quantile(successdata$SAT_Score, na.rm = TRUE)
#University GPA
sd(successdata$University_GPA, na.rm = TRUE)
var(successdata$University_GPA, na.rm = TRUE)
min(successdata$University_GPA, na.rm = TRUE)
max(successdata$University_GPA, na.rm = TRUE)
range(successdata$University_GPA, na.rm = TRUE)
quantile(successdata$University_GPA, na.rm = TRUE)

#Internships Completed
sd(successdata$Internships_Completed, na.rm = TRUE)
var(successdata$Internships_Completed, na.rm = TRUE)
min(successdata$Internships_Completed, na.rm = TRUE)
max(successdata$Internships_Completed, na.rm = TRUE)
range(successdata$Internships_Completed, na.rm = TRUE)
quantile(successdata$Internships_Completed, na.rm = TRUE)
#Projects Completed
sd(successdata$Projects_Completed, na.rm = TRUE)
var(successdata$Projects_Completed, na.rm = TRUE)
min(successdata$Projects_Completed, na.rm = TRUE)
max(successdata$Projects_Completed, na.rm = TRUE)
range(successdata$Projects_Completed, na.rm = TRUE)
quantile(successdata$Projects_Completed, na.rm = TRUE)
#Certifications
sd(successdata$Certifications, na.rm = TRUE)
var(successdata$Certifications, na.rm = TRUE)
min(successdata$Certifications, na.rm = TRUE)
max(successdata$Certifications, na.rm = TRUE)
range(successdata$Certifications, na.rm = TRUE)
quantile(successdata$Certifications, na.rm = TRUE)

#Job Offers
sd(successdata$Job_Offers, na.rm = TRUE)
var(successdata$Job_Offers, na.rm = TRUE)
min(successdata$Job_Offers, na.rm = TRUE)
max(successdata$Job_Offers, na.rm = TRUE)
range(successdata$Job_Offers, na.rm = TRUE)
quantile(successdata$Job_Offers, na.rm = TRUE)
#Starting Salary
sd(successdata$Starting_Salary, na.rm = TRUE)
var(successdata$Starting_Salary, na.rm = TRUE)
min(successdata$Starting_Salary, na.rm = TRUE)
max(successdata$Starting_Salary, na.rm = TRUE)
range(successdata$Starting_Salary, na.rm = TRUE)
quantile(successdata$Starting_Salary, na.rm = TRUE)
#Current Job Level
sd(successdata$Current_Job_Level, na.rm = TRUE)
var(successdata$Current_Job_Level, na.rm = TRUE)
min(successdata$Current_Job_Level, na.rm = TRUE)
max(successdata$Current_Job_Level, na.rm = TRUE)
range(successdata$Current_Job_Level, na.rm = TRUE)
quantile(successdata$Current_Job_Level, na.rm = TRUE)


#use sapply() to calculate the mean and standard deviation for all
academic_scores <- successdata[c("High_School_GPA", "SAT_Score", "University_GPA")]
sapply(academic_scores, mean, na.rm = TRUE)
sapply(academic_scores, sd, na.rm = TRUE)

experience_scores <- successdata[c("Internships_Completed", "Projects_Completed", "Certifications")]
sapply(experience_scores, mean, na.rm = TRUE)
sapply(experience_scores, sd, na.rm = TRUE)

jobsuccess_scores <- successdata[c("Job_Offers", "Starting_Salary", "Current_Job_Level")]
sapply(jobsuccess_scores, mean, na.rm = TRUE)
sapply(jobsuccess_scores, sd, na.rm = TRUE)


#use summary() function to get summary statistics for the entire dataset
summary(successdata)


#calculate the mean and standard deviation for different regions
#I'll use field of study column
#HS GPA:
successdata %>%
  group_by(Field_of_Study) %>%
  get_summary_stats(High_School_GPA, type = "mean_sd")

#SAT Score
successdata %>%
  group_by(Field_of_Study) %>%
  get_summary_stats(SAT_Score, type = "mean_sd")

#University GPA
successdata %>%
  group_by(Field_of_Study) %>%
  get_summary_stats(University_GPA, type = "mean_sd")

#Internships Completed
successdata %>%
  group_by(Field_of_Study) %>%
  get_summary_stats(Internships_Completed, type = "mean_sd")

#Projects Completed
successdata %>%
  group_by(Field_of_Study) %>%
  get_summary_stats(Projects_Completed, type = "mean_sd")

#Certifications
successdata %>%
  group_by(Field_of_Study) %>%
  get_summary_stats(Certifications, type = "mean_sd")

#Job Offers
successdata %>%
  group_by(Field_of_Study) %>%
  get_summary_stats(Job_Offers, type = "mean_sd")

#Starting Salary
successdata %>%
  group_by(Field_of_Study) %>%
  get_summary_stats(Starting_Salary, type = "mean_sd")

#Current Job Level
successdata %>%
  group_by(Field_of_Study) %>%
  get_summary_stats(Current_Job_Level, type = "mean_sd")

```





