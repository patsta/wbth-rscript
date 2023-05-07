library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)

colors <- c("#5944C6", "#11AAFF", "#A2CDF4","#086ADB")

data_survey <- read_excel(<FILE>)
data_survey$`Which area best fits your job?` == 'Engineer'
attach(data_survey)


####################################################################################
# Future Skills - Learning-Literacy
####################################################################################
learning_literacy_agile <- table(`Learning-Literacy`[`Which area best fits your job?` == 'Agile roles'])
# Mean
round(mean(`Learning-Literacy`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Learning-Literacy`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Learning-Literacy`[`Which area best fits your job?` == 'Agile roles'])

learning_literacy_engineer <- table(`Learning-Literacy`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Learning-Literacy`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Learning-Literacy`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Learning-Literacy`[`Which area best fits your job?` == 'Engineer'])

####################################################################################
# Future Skills - Self-efficacy
####################################################################################
self_efficacy_agile <- table(`Self-efficacy`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Self-efficacy`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Self-efficacy`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Self-efficacy`[`Which area best fits your job?` == 'Agile roles'])

self_efficacy_engineer <- table(`Self-efficacy`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Self-efficacy`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Self-efficacy`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Self-efficacy`[`Which area best fits your job?` == 'Engineer'])
####################################################################################
# Future Skills - Self-determination
####################################################################################
self_determination_agile <- table(`Self-determination`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Self-determination`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Self-determination`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Self-determination`[`Which area best fits your job?` == 'Agile roles'])

self_determination_engineer <- table(`Self-determination`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Self-determination`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Self-determination`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Self-determination`[`Which area best fits your job?` == 'Engineer'])
####################################################################################
# Future Skills - Self-competence
####################################################################################
self_competence_agile <- table(`Self-competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Self-competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Self-competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Self-competence`[`Which area best fits your job?` == 'Agile roles'])

self_competence_engineer <- table(`Self-competence`[`Which area best fits your job?` == 'Engineer'])
self_efficacy_engineer_rel <- round(prop.table(self_efficacy_engineer),2)

# Mean
round(mean(`Self-competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Self-competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Self-competence`[`Which area best fits your job?` == 'Engineer'])
####################################################################################
# Future Skills - Reflective competence
####################################################################################
reflective_agile <- table(`Reflective competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Reflective competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Reflective competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Reflective competence`[`Which area best fits your job?` == 'Agile roles'])

reflective_engineer <- table(`Reflective competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Reflective competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Reflective competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Reflective competence`[`Which area best fits your job?` == 'Engineer'])

####################################################################################
# Future Skills - Decision competence
####################################################################################
decision_agile <- table(`Decision competence`[`Which area best fits your job?` == 'Agile roles'])
# Mean
round(mean(`Decision competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Decision competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Decision competence`[`Which area best fits your job?` == 'Agile roles'])

decision_engineer <- table(`Decision competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Decision competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Decision competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Decision competence`[`Which area best fits your job?` == 'Engineer'])

####################################################################################
# Future Skills - Initiative and performance competence
####################################################################################
initiative_agile <- table(`Initiative and performance competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Initiative and performance competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Initiative and performance competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Initiative and performance competence`[`Which area best fits your job?` == 'Agile roles'])

initiative_engineer <- table(`Initiative and performance competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Initiative and performance competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Initiative and performance competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Initiative and performance competence`[`Which area best fits your job?` == 'Engineer'])
####################################################################################
# Future Skills - Ambiguity competence
####################################################################################
ambiguity_agile <- table(`Ambiguity competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Ambiguity competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Ambiguity competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Ambiguity competence`[`Which area best fits your job?` == 'Agile roles'])

ambiguity_engineer <- table(`Ambiguity competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Ambiguity competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Ambiguity competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Ambiguity competence`[`Which area best fits your job?` == 'Engineer'])

####################################################################################
# Future Skills - Ethical competence
####################################################################################
ethical_agile <- table(`Ethical competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Ethical competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Ethical competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Ethical competence`[`Which area best fits your job?` == 'Agile roles'])

ethical_engineer <- table(`Ethical competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Ethical competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Ethical competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Ethical competence`[`Which area best fits your job?` == 'Engineer'])

####################################################################################
# Future Skills - Design-thinking competence
####################################################################################
design_thinking_agile <- table(`Design-thinking competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Design-thinking competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Design-thinking competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Design-thinking competence`[`Which area best fits your job?` == 'Agile roles'])

design_thinking_engineer <- table(`Design-thinking competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Design-thinking competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Design-thinking competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Design-thinking competence`[`Which area best fits your job?` == 'Engineer'])

####################################################################################
# Future Skills - Innovation competence
####################################################################################
innovation_agile <- table(`Innovation competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Innovation competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Innovation competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Innovation competence`[`Which area best fits your job?` == 'Agile roles'])

innovation_engineer <- table(`Innovation competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Innovation competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Innovation competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Innovation competence`[`Which area best fits your job?` == 'Engineer'])

####################################################################################
# Future Skills - Systems competence
####################################################################################
system_agile <- table(`Systems competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Systems competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Systems competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Systems competence`[`Which area best fits your job?` == 'Agile roles'])

system_engineer <- table(`Systems competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Systems competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Systems competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Systems competence`[`Which area best fits your job?` == 'Engineer'])

####################################################################################
# Future Skills - Digital literacy
####################################################################################
digital_agile <- table(`Digital literacy`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Digital literacy`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Digital literacy`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Digital literacy`[`Which area best fits your job?` == 'Agile roles'])

digital_engineer <- table(`Digital literacy`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Digital literacy`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Digital literacy`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Digital literacy`[`Which area best fits your job?` == 'Engineer'])
####################################################################################
# Future Skills - Sensemaking
####################################################################################
sensemaking_agile <- table(`Sensemaking`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Sensemaking`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Sensemaking`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Sensemaking`[`Which area best fits your job?` == 'Agile roles'])

sensemaking_engineer <- table(`Sensemaking`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Sensemaking`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Sensemaking`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Sensemaking`[`Which area best fits your job?` == 'Engineer'])

####################################################################################
# Future Skills - Future and design competence
####################################################################################
future_agile <- table(`Future and design competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Future and design competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Future and design competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Future and design competence`[`Which area best fits your job?` == 'Agile roles'])

future_engineer <- table(`Future and design competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Future and design competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Future and design competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Future and design competence`[`Which area best fits your job?` == 'Engineer'])
####################################################################################
# Future Skills - Cooperation competence
####################################################################################
cooperation_agile<- table(`Cooperation competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Cooperation competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Cooperation competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Cooperation competence`[`Which area best fits your job?` == 'Agile roles'])

cooperation_engineer <- table(`Cooperation competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Cooperation competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Cooperation competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Cooperation competence`[`Which area best fits your job?` == 'Engineer'])
####################################################################################
# Future Skills - Communication competence
####################################################################################
communication_agile<- table(`Communication competence`[`Which area best fits your job?` == 'Agile roles'])

# Mean
round(mean(`Communication competence`[`Which area best fits your job?` == 'Agile roles']),2)
# SD
round(sd(`Communication competence`[`Which area best fits your job?` == 'Agile roles']),2)
# Median
median(`Communication competence`[`Which area best fits your job?` == 'Agile roles'])

communication_engineer <- table(`Communication competence`[`Which area best fits your job?` == 'Engineer'])

# Mean
round(mean(`Communication competence`[`Which area best fits your job?` == 'Engineer']),2)
# SD
round(sd(`Communication competence`[`Which area best fits your job?` == 'Engineer']),2)
# Median
median(`Communication competence`[`Which area best fits your job?` == 'Engineer'])
