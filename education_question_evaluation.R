library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)

colors <- c("#5944C6", "#11AAFF", "#A2CDF4","#086ADB")

data_survey <- read_excel(<FILE>)
data_survey$`Which area best fits your job?` == 'Engineer'
attach(data_survey)

####################################################################################
# Education Days
####################################################################################
education_days_freq <- table(`How many education days do you spend per year?`)


education_days_freq_with_sum <- addmargins(table(`How many education days do you spend per year?`))
education_days_rel_freq <- prop.table(education_days_freq)

education_days_data <- cbind(round(education_days_freq, 4) )
education_days_data <- round(education_days_rel_freq, 4) * 100
pie(education_days_data, labels = education_days_data, col = c("#E61E64", "#5944C6", "#A2CDF4", "#DDE3E7", "#0EABA9", "#11AAFF"), main="Anzahl eingesetzter Weiterbildungstage")
legend(legend = names(education_days_data),cex = 0.9, fill = c("#E61E64", "#5944C6", "#A2CDF4", "#DDE3E7", "#0EABA9", "#11AAFF"), x = 1, y = 1)

####################################################################################
# Education Days Support
####################################################################################
support_education_freq <- table(`Do you want to be supported by your manager in the selection of suitable further education opportunities?`)

support_education_freq_with_sum <- addmargins(table(`Do you want to be supported by your manager in the selection of suitable further education opportunities?`))
support_educations_rel_freq <- prop.table(support_education_freq)

support_education_data <- cbind(round(support_education_freq, 4) )
support_education_data <- round(support_educations_rel_freq, 4) * 100
row.names(support_education_data) <- c("Nein", "Ja")
pie(support_education_data, labels = support_education_data, col = c("#5944C6", "#11AAFF"), main="Unterstützung durch den Vorgesetzen bei der Weiterbildung")
legend(legend = names(support_education_data),cex = 0.9, fill = c("#5944C6", "#11AAFF"), x = 1, y = 1)

####################################################################################
# Education - Oppertunities Engineer
####################################################################################
filtered_oppertunities_eng <- `I am satisfied with my opportunities to develop in my job`[`Which area best fits your job?` == 'Engineer']

engineer_oppertunities_coded <- ifelse(filtered_oppertunities_eng == 'Strongly Agree', 5,
                                         ifelse(filtered_oppertunities_eng == 'Agree',4,
                                                ifelse(filtered_oppertunities_eng ==  'Neither agree or disagree', 3,
                                                       ifelse(filtered_oppertunities_eng == 'Disagree', 2,1))))

mean_oppertunities_engineer <- round(mean(engineer_oppertunities_coded),2)

####################################################################################
# Education - Oppertunities Agile Role
####################################################################################
filtered_oppertunities_agile <- `I am satisfied with my opportunities to develop in my job`[`Which area best fits your job?` == 'Agile roles']

agile_oppertunities_coded <- ifelse(filtered_oppertunities_agile == 'Strongly Agree', 5,
                                       ifelse(filtered_oppertunities_agile == 'Agree',4,
                                              ifelse(filtered_oppertunities_agile ==  'Neither agree or disagree', 3,
                                                     ifelse(filtered_oppertunities_agile == 'Disagree', 2,1))))

mean_oppertunities_agile <- round(mean(agile_oppertunities_coded),2)

####################################################################################
# Education - Professional Development Engineer
####################################################################################
filtered_development_eng <- `My organisation strives for my professional development`[`Which area best fits your job?` == 'Engineer']

engineer_development_coded <- ifelse(filtered_development_eng == 'Strongly Agree', 5,
                                       ifelse(filtered_development_eng == 'Agree',4,
                                              ifelse(filtered_development_eng ==  'Neither agree or disagree', 3,
                                                     ifelse(filtered_development_eng == 'Disagree', 2,1))))

mean_development_engineer <- round(mean(engineer_development_coded),2)

####################################################################################
# Education - Professional Development Agile Role
####################################################################################
filtered_development_agile <- `My organisation strives for my professional development`[`Which area best fits your job?` == 'Agile roles']

agile_development_coded <- ifelse(filtered_development_agile == 'Strongly Agree', 5,
                                    ifelse(filtered_development_agile == 'Agree',4,
                                           ifelse(filtered_development_agile ==  'Neither agree or disagree', 3,
                                                  ifelse(filtered_development_agile == 'Disagree', 2,1))))

mean_development_agile <- round(mean(agile_development_coded),2)

####################################################################################
# Education - Training Satisfaction Engineer
####################################################################################
filtered_training_eng <- `I am satisfied with the training provided by my organisation for my work`[`Which area best fits your job?` == 'Engineer']

engineer_training_coded <- ifelse(filtered_training_eng == 'Strongly Agree', 5,
                               ifelse(filtered_training_eng == 'Agree',4,
                                      ifelse(filtered_training_eng ==  'Neither agree or disagree', 3,
                                             ifelse(filtered_training_eng == 'Disagree', 2,1))))

mean_training_engineer <- round(mean(engineer_training_coded),2)

####################################################################################
# Education - Training Satisfaction Agile Role
####################################################################################
filtered_training_agile <- `I am satisfied with the training provided by my organisation for my work`[`Which area best fits your job?` == 'Agile roles']

agile_training_coded <- ifelse(filtered_training_agile == 'Strongly Agree', 5,
                                  ifelse(filtered_training_agile == 'Agree',4,
                                         ifelse(filtered_training_agile ==  'Neither agree or disagree', 3,
                                                ifelse(filtered_training_agile == 'Disagree', 2,1))))

mean_training_agile <- round(mean(agile_training_coded),2)

####################################################################################
# Education - Invest Satisfaction Engineer
####################################################################################
filtered_invest_eng <- `I am satisfied with what my organisation invests in education and training`[`Which area best fits your job?` == 'Engineer']

engineer_invest_coded <- ifelse(filtered_invest_eng == 'Strongly Agree', 5,
                                  ifelse(filtered_invest_eng == 'Agree',4,
                                         ifelse(filtered_invest_eng ==  'Neither agree or disagree', 3,
                                                ifelse(filtered_invest_eng == 'Disagree', 2,1))))

mean_invest_engineer <- round(mean(engineer_invest_coded),2)

####################################################################################
# Education - Invest Satisfaction Agile Role
####################################################################################
filtered_invest_agile <- `I am satisfied with what my organisation invests in education and training`[`Which area best fits your job?` == 'Agile roles']

agile_invest_coded <- ifelse(filtered_invest_agile == 'Strongly Agree', 5,
                               ifelse(filtered_invest_agile == 'Agree',4,
                                      ifelse(filtered_invest_agile ==  'Neither agree or disagree', 3,
                                             ifelse(filtered_invest_agile == 'Disagree', 2,1))))

mean_invest_agile <- round(mean(agile_invest_coded),2)


# Barplot
categories <- c("Oppertunities", "Development", "Training", "Invest")
engineer_data_education <- c(mean_oppertunities_engineer, mean_development_engineer, mean_training_engineer, mean_invest_engineer)
agile_data_education <- c(mean_oppertunities_agile, mean_development_agile, mean_training_agile, mean_invest_agile)

data_sorted_by_category <- c(mean_oppertunities_engineer, mean_oppertunities_agile, mean_development_engineer, mean_development_agile, mean_training_engineer, mean_training_agile, mean_invest_engineer, mean_invest_agile)

# Combine the values into a matrix
data_matrix_education <- rbind(engineer_data_education, agile_data_education)


# Create the barplot
par(mar=c(10, 15, 15, 8))
bar <- barplot(data_matrix_education, beside = TRUE, names.arg = categories, col = c("#5944C6", "#11AAFF"),
               main = "Fragen zur Ausbildung bei der <Company>", xlab = "Frage Kategorie", ylab = "Mittelwert", ylim=c(0,5), cex.names = 0.8)

legend(legend = c("Engineer", "Agile Rollen"),cex = 1.2, fill = colors, x = 10, y = 5.5 )
text(x = bar, y = data_sorted_by_category + 0.2, label = data_matrix_education,col = "black")


####################################################################################
# Education - Invest Satisfaction Engineer
####################################################################################
filtered_invest_eng <- `I am satisfied with what my organisation invests in education and training`[`Which area best fits your job?` == 'Engineer']

engineer_invest_coded <- ifelse(filtered_invest_eng == 'Strongly Agree', 5,
                                ifelse(filtered_invest_eng == 'Agree',4,
                                       ifelse(filtered_invest_eng ==  'Neither agree or disagree', 3,
                                              ifelse(filtered_invest_eng == 'Disagree', 2,1))))

mean_invest_engineer <- round(mean(engineer_invest_coded),2)

####################################################################################
# Education - Invest Satisfaction Engineer
####################################################################################
filtered_invest_agile <- `I am satisfied with what my organisation invests in education and training`[`Which area best fits your job?` == 'Agile roles']

agile_invest_coded <- ifelse(filtered_invest_agile == 'Strongly Agree', 5,
                             ifelse(filtered_invest_agile == 'Agree',4,
                                    ifelse(filtered_invest_agile ==  'Neither agree or disagree', 3,
                                           ifelse(filtered_invest_agile == 'Disagree', 2,1))))

mean_invest_agile <- round(mean(agile_invest_coded),2)


# Barplot
categories <- c("Möglichkeiten", "Entwicklung", "Training", "Investition")
engineer_data_education <- c(mean_oppertunities_engineer, mean_development_engineer, mean_training_engineer, mean_invest_engineer)
agile_data_education <- c(mean_oppertunities_agile, mean_development_agile, mean_training_agile, mean_invest_agile)

data_sorted_by_category <- c(mean_oppertunities_engineer, mean_oppertunities_agile, mean_development_engineer, mean_development_agile, mean_training_engineer, mean_training_agile, mean_invest_engineer, mean_invest_agile)

# Combine the values into a matrix
data_matrix_education <- rbind(engineer_data_education, agile_data_education)

# Create the barplot
bar <- barplot(data_matrix_education, beside = TRUE, names.arg = categories, col = c("#5944C6", "#11AAFF"),
               main = "Fragen zur Ausbildung bei der <Company>", xlab = "Frage Kategorie", ylab = "Mittelwert", ylim=c(0,5), cex.names = 0.8)

legend(legend = c("Engineer", "Agile Rollen"),cex = 1.2, fill = colors, x = 12, y = 5)
text(x = bar, y = data_sorted_by_category + 0.2, label = data_matrix_education,col = "black")

####################################################################################
# Education Formats Ranking - Engineer Brown Bag
####################################################################################
filtered_brownbag_eng <- `Brown Bag Meeting / Tech Talks`[`Which area best fits your job?` == 'Engineer']
mean_brownbag_engineer <- round(mean(filtered_brownbag_eng),2)

####################################################################################
# Education Formats Ranking - Engineer Classroom (on-site)
####################################################################################
filtered_classroom_eng <- `Classroom (on-site)`[`Which area best fits your job?` == 'Engineer']

mean_classroom_engineer <- round(mean(filtered_classroom_eng),2)

####################################################################################
# Education Formats Ranking - Engineer Community of practice
####################################################################################
filtered_community_eng <- `Community of practice`[`Which area best fits your job?` == 'Engineer']

mean_community_engineer <- round(mean(filtered_community_eng),2)

####################################################################################
# Education Formats Ranking - Engineer Hackathon
####################################################################################
filtered_hackathon_eng <- `Hackathon`[`Which area best fits your job?` == 'Engineer']

mean_hackathon_engineer <- round(mean(filtered_hackathon_eng),2)

####################################################################################
# Education Formats Ranking - Engineer Proof of concept (e.g. during IP-Sprint )
####################################################################################
filtered_poc_eng <- `Proof of concept (e.g. during IP-Sprint )`[`Which area best fits your job?` == 'Engineer']

mean_poc_engineer <- round(mean(filtered_poc_eng),2)

####################################################################################
# Education Formats Ranking - Engineer Virtual Classroom
####################################################################################
filtered_virtual_eng <- `Virtual Classroom`[`Which area best fits your job?` == 'Engineer']

mean_virtual_engineer <- round(mean(filtered_virtual_eng),2)

####################################################################################
# Education Formats Ranking - Engineer Web Based Training
####################################################################################
filtered_web_eng <- `Web Based Training`[`Which area best fits your job?` == 'Engineer']

mean_web_engineer <- round(mean(filtered_web_eng),2)

####################################################################################
# Education Formats Ranking - Agile Brown Bag
####################################################################################
filtered_brownbag_agile <- `Brown Bag Meeting / Tech Talks`[`Which area best fits your job?` == 'Agile roles']

mean_brownbag_agile <- round(mean(filtered_brownbag_agile),2)

####################################################################################
# Education Formats Ranking - Agile Classroom (on-site)
####################################################################################
filtered_classroom_agile <- `Classroom (on-site)`[`Which area best fits your job?` == 'Agile roles']

mean_classroom_agile <- round(mean(filtered_classroom_agile),2)

####################################################################################
# Education Formats Ranking - Agile Community of practice
####################################################################################
filtered_community_agile <- `Community of practice`[`Which area best fits your job?` == 'Agile roles']

mean_community_agile <- round(mean(filtered_community_agile),2)

####################################################################################
# Education Formats Ranking - Agile Hackathon
####################################################################################
filtered_hackathon_agile <- `Hackathon`[`Which area best fits your job?` == 'Agile roles']

mean_hackathon_agile <- round(mean(filtered_hackathon_agile),2)

####################################################################################
# Education Formats Ranking - Agile Proof of concept (e.g. during IP-Sprint )
####################################################################################
filtered_poc_agile <- `Proof of concept (e.g. during IP-Sprint )`[`Which area best fits your job?` == 'Agile roles']

mean_poc_agile <- round(mean(filtered_poc_agile),2)

####################################################################################
# Education Formats Ranking - Agile Virtual Classroom
####################################################################################
filtered_virtual_agile <- `Virtual Classroom`[`Which area best fits your job?` == 'Agile roles']

mean_virtual_agile <- round(mean(filtered_virtual_agile),2)

####################################################################################
# Education Formats Ranking - Agile Web Based Training
####################################################################################
filtered_web_agile <- `Web Based Training`[`Which area best fits your job?` == 'Agile roles']

mean_web_agile <- round(mean(filtered_web_agile),2)

# Barplot
education_formats <- c("Brown Bag", "Classroom", "CoP", "Hackathon", "PoC", "Virtual", "Web Based")
engineer_data_education_formats <- c(mean_brownbag_engineer, mean_classroom_engineer, mean_community_engineer, mean_hackathon_engineer, mean_poc_engineer, mean_virtual_engineer, mean_web_engineer)
agile_data_education_formats <- c(mean_brownbag_agile, mean_classroom_agile, mean_community_agile, mean_hackathon_agile, mean_poc_agile, mean_virtual_agile, mean_web_agile)

data_sorted_by_format <- c(mean_brownbag_engineer, mean_brownbag_agile, mean_classroom_engineer, mean_classroom_agile, mean_community_engineer, mean_community_agile, mean_hackathon_engineer, mean_hackathon_agile, mean_poc_engineer, mean_poc_agile, mean_virtual_engineer, mean_virtual_agile, mean_web_engineer, mean_web_agile)

# Combine the values into a matrix
data_matrix_education_format <- rbind(engineer_data_education_formats, agile_data_education_formats)

# Create the barplot
bar <- barplot(data_matrix_education_format, beside = TRUE, names.arg = education_formats, col = c("#5944C6", "#11AAFF"),
               main = "Ausbildungsvarianten nach Rollen", xlab = "Typ", ylab = "Mittelwert", ylim=c(0,7), cex.names = 0.8)

legend(legend = c("Engineer", "Agile Rollen"),cex = 1.2, fill = colors, x = 16, y = 7)
text(x = bar, y = data_sorted_by_format + 0.2, label = data_matrix_education_format,col = "black")

####################################################################################
# Education Formats Ranking - Overall Brown Bag
####################################################################################
filtered_brownbag_overall <- `Brown Bag Meeting / Tech Talks`
mean_brownbag_overall <- round(mean(filtered_brownbag_overall),2)

####################################################################################
# Education Formats Ranking - Overall Classroom (on-site)
####################################################################################
filtered_classroom_overall <- `Classroom (on-site)`

mean_classroom_overall <- round(mean(filtered_classroom_overall),2)

####################################################################################
# Education Formats Ranking - Overall Community of practice
####################################################################################
filtered_community_overall <- `Community of practice`

mean_community_overall <- round(mean(filtered_community_overall),2)

####################################################################################
# Education Formats Ranking - Overall Hackathon
####################################################################################
filtered_hackathon_overall <- `Hackathon`

mean_hackathon_overall <- round(mean(filtered_hackathon_overall),2)

####################################################################################
# Education Formats Ranking - Overall Proof of concept (e.g. during IP-Sprint )
####################################################################################
filtered_poc_overall <- `Proof of concept (e.g. during IP-Sprint )`

mean_poc_overall <- round(mean(filtered_poc_overall),2)

####################################################################################
# Education Formats Ranking - Overall Virtual Classroom
####################################################################################
filtered_virtual_overall <- `Virtual Classroom`

mean_virtual_overall <- round(mean(filtered_virtual_overall),2)

####################################################################################
# Education Formats Ranking - Overall Web Based Training
####################################################################################
filtered_web_overall <- `Web Based Training`

mean_web_overall <- round(mean(filtered_web_overall),2)

education_formats <- c("Brown Bag", 
                       "Classroom", 
                       "CoP", 
                       "Hackathon", 
                       "PoC", 
                       "Virtual",
                       "Web Based")

overall_data_education_formats <- c(mean_brownbag_overall, 
                                    mean_classroom_overall, 
                                    mean_community_overall, 
                                    mean_hackathon_overall, 
                                    mean_poc_overall, 
                                    mean_virtual_overall, 
                                    mean_web_overall)


formats_data <- data.frame(education_formats, overall_data_education_formats)
formats_data_sorted  <- formats_data[order(formats_data$overall_data_education_formats, decreasing = TRUE),]


# Barplot


# Create the barplot
bar <- barplot(formats_data_sorted$overall_data_education_formats, beside = TRUE, names.arg = formats_data_sorted$education_formats, col = c("#5944C6", "#11AAFF"),
               main = "Ausbildungsvarianten Overall", xlab = "Typ", ylab = "Mittelwert", ylim=c(0,7), cex.names = 0.8)

text(x = bar, y = formats_data_sorted$overall_data_education_formats + 0.2, label = formats_data_sorted$overall_data_education_formats,col = "black")


