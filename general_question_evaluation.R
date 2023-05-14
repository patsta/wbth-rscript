library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)
options(OutDec=",")
colors <- c("#5944C6", "#11AAFF", "#A2CDF4","#086ADB")

data_survey <- read_excel(<FILE_NAME>)

attach(data_survey)
####################################################################################
# Get overview of participants - Age Group
####################################################################################
age_freq <- table(`Which of the following age group do you belong to?`)
age_freq_with_sum <- addmargins(table(`Which of the following age group do you belong to?`))
age_rel_freq <- prop.table(age_freq)
age_data <- cbind(round(age_rel_freq, 4) )
age_data <- round(age_rel_freq, 4) * 100
row.names(age_data) <- c("18–24","25–34","35–44","45 und älter")
pie(age_data, labels = age_data, col=colors, main="Aufteilung nach Alterskategorie")
legend(legend = names(age_data),cex = 0.9, fill = colors, x = 0.9, y = 1)


####################################################################################
# Get overview of participants - Years working <Company>
####################################################################################
years_freq <- table(`How long have you been working at <Company>?`) 
years_freq_with_sum <- addmargins(table(`How long have you been working at <Company>?`))  
years_rel_freq <- prop.table(years_freq)

# Translate row names to german
row.names(years_rel_freq) <- c("zwischen 10 und 20 Jahren","zwischen 5 und 9 Jahren","weniger als 5 Jahre","mehr als 20 Jahre")

years_rel_freq
years_rel_freq
years_data <- cbind(round(years_rel_freq, 4) )
years_data <- round(years_rel_freq, 4) * 100 

pie(years_data, labels = years_data, col=colors, main="Berufsjahre bei <Company>")
legend(legend = names(years_data),cex = 0.9, fill = colors, x = 0.82, y = 1)

####################################################################################
# Get overview of participants - Gender
####################################################################################
gender_freq <- table(`What gender do you identify as?
`)

gender_freq_with_sum <- addmargins(table(`What gender do you identify as?
`))
gender_rel_freq <- prop.table(gender_freq)
gender_data <- cbind(round(gender_rel_freq, 4))
gender_data <- round(gender_rel_freq, 4) * 100
row.names(gender_data) <- c("Frau","Mann","Keine Angabe")
pie(gender_data, labels = gender_data, col=colors, main="Geschlecht")
legend(legend = names(gender_data),cex = 0.9, fill = colors, x = 0.9, y = 1)

####################################################################################
# Get overview of participants - Job Description
####################################################################################
job_freq <- table(`Which area best fits your job?`)

job_freq_with_sum <- addmargins(table(`Which area best fits your job?`))
job_rel_freq <- prop.table(job_freq)
job_data <- cbind(round(job_rel_freq, 4))
job_data <- round(job_rel_freq, 4) * 100
row.names(job_data) <- c("Agile Rollen","Engineer")
pie(job_data, labels = job_data, col=colors, main="Arbeitstätigkeit")
legend(legend = names(job_data),cex = 0.9, fill = colors, x = 0.9, y = 1)
