library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)

colors <- c("#5944C6", "#11AAFF", "#A2CDF4","#086ADB")

data_survey <- read_excel("<FILE>")
data_survey$`Which area best fits your job?` == 'Engineer'
attach(data_survey)

####################################################################################
# Future Skills - Cloud engineer - Agile Role
####################################################################################
devops_agile <- round(mean(`Development and operations (DevOps)`[`Which area best fits your job?` == 'Agile roles']),2)
automation_agile <- round(mean(`Automation`[`Which area best fits your job?` == 'Agile roles']),2)
cloud_service_platform_agile <- round(mean(`Cloud service platform expertise`[`Which area best fits your job?` == 'Agile roles']),2)
cloud_security_agile <- round(mean(`Cloud security`[`Which area best fits your job?` == 'Agile roles']),2)
iaas_agile <- round(mean(`Infrastructure as code`[`Which area best fits your job?` == 'Agile roles']),2)

containers_agile <- round(mean(`Containers`[`Which area best fits your job?` == 'Agile roles']),2)
hybrid_cloud_agile <- round(mean(`Hybrid cloud`[`Which area best fits your job?` == 'Agile roles']),2)
performance_agile <- round(mean(`Performance testing, metrics and analytics`[`Which area best fits your job?` == 'Agile roles']),2)
network_agile <- round(mean(`Network management`[`Which area best fits your job?` == 'Agile roles']),2)
database_agile <- round(mean(`Database management`[`Which area best fits your job?` == 'Agile roles']),2)
storage_agile <- round(mean(`Storage management`[`Which area best fits your job?` == 'Agile roles']),2)
machine_agile <- round(mean(`Machine learning and Artificial intelligence`[`Which area best fits your job?` == 'Agile roles']),2)

names <- c("Development und Operations (DevOps)", 
           "Automation", 
           "Fachwissen über Cloud-Service-Plattformen", 
           "Cloud Sicherheit", 
           "Infrastruktur als Code",
           "Containers",
           "Hybrid cloud",
           "Leistungstests, Metriken und Analysen",
           "Netzwerk-Management",
           "Datenbank-Management",
           "Storage-Management",
           "Machine learning und Artificial intelligence"
           ) 
data <- c(devops_agile, 
          automation_agile, 
          cloud_service_platform_agile, 
          cloud_security_agile,
          iaas_agile,
          containers_agile,
          hybrid_cloud_agile,
          performance_agile,
          network_agile,
          database_agile,
          storage_agile,
          machine_agile
          )

agile_skills_data <- data.frame(names, data)
agile_data_sorted <- agile_skills_data[order(agile_skills_data$data, decreasing = TRUE),]
par(mar=c(10, 15, 5, 8))
bar <- barplot(agile_data_sorted$data, names.arg = agile_data_sorted$names, col= c("#11AAFF", "#A2CDF4"), las=1, xlim=c(0,12), cex.names = 0.8, xlab = "Mittelwert", horiz = TRUE, main="Mittelwerte der Themenbereiche (Agile Rollen)")
axis(1, at=seq(0.0,12.0, by = 1))
text(x = agile_data_sorted$data * 0.8, y = bar, label = agile_data_sorted$data,col = "black")

####################################################################################
# Future Skills - Cloud engineer - Engineer
####################################################################################
devops_engineer <- round(mean(`Development and operations (DevOps)`[`Which area best fits your job?` == 'Engineer']),2)
automation_engineer <- round(mean(`Automation`[`Which area best fits your job?` == 'Engineer']),2)
cloud_service_platform_engineer <- round(mean(`Cloud service platform expertise`[`Which area best fits your job?` == 'Engineer']),2)
cloud_security_engineer <- round(mean(`Cloud security`[`Which area best fits your job?` == 'Engineer']),2)
iaas_engineer <- round(mean(`Infrastructure as code`[`Which area best fits your job?` == 'Engineer']),2)
containers_engineer <- round(mean(`Containers`[`Which area best fits your job?` == 'Engineer']),2)
hybrid_cloud_engineer <- round(mean(`Hybrid cloud`[`Which area best fits your job?` == 'Engineer']),2)
performance_engineer <- round(mean(`Performance testing, metrics and analytics`[`Which area best fits your job?` == 'Engineer']),2)
network_engineer <- round(mean(`Network management`[`Which area best fits your job?` == 'Engineer']),2)
database_engineer <- round(mean(`Database management`[`Which area best fits your job?` == 'Engineer']),2)
storage_engineer <- round(mean(`Storage management`[`Which area best fits your job?` == 'Engineer']),2)
machine_engineer <- round(mean(`Machine learning and Artificial intelligence`[`Which area best fits your job?` == 'Engineer']),2)

names <- c("Development und Operations (DevOps)", 
           "Automation", 
           "Fachwissen über Cloud-Service-Plattformen", 
           "Cloud Sicherheit", 
           "Infrastruktur als Code",
           "Containers",
           "Hybrid cloud",
           "Leistungstests, Metriken und Analysen",
           "Netzwerk-Management",
           "Datenbank-Management",
           "Storage-Management",
           "Machine learning und Artificial intelligence"
) 
data <- c(devops_engineer, 
          automation_engineer, 
          cloud_service_platform_engineer, 
          cloud_security_engineer,
          iaas_engineer,
          containers_engineer,
          hybrid_cloud_engineer,
          performance_engineer,
          network_engineer,
          database_engineer,
          storage_engineer,
          machine_engineer
)

engineer_skills_data <- data.frame(names, data)
engineer_data_sorted <- engineer_skills_data[order(engineer_skills_data$data, decreasing = TRUE),]
par(mar=c(10, 15, 5, 8))
bar <- barplot(engineer_data_sorted$data, names.arg = engineer_data_sorted$names, col= c("#11AAFF", "#A2CDF4") , las=1, xlim=c(0,12), cex.names = 0.8, xlab = "Mittelwert", horiz = TRUE, main="Mittelwerte der Themenbereiche (Engineer)")
axis(1, at=seq(0.0,12.0, by = 1))
text(x = engineer_data_sorted$data * 0.8, y = bar, label = engineer_data_sorted$data,col = "black")

####################################################################################
# Future Skills - Cloud engineer - Overall
####################################################################################
devops_overall <- round(mean(`Development and operations (DevOps)`),2)
automation_overall <- round(mean(`Automation`),2)
cloud_service_platform_overall <- round(mean(`Cloud service platform expertise`),2)
cloud_security_overall <- round(mean(`Cloud security`),2)
iaas_overall <- round(mean(`Infrastructure as code`),2)
containers_overall <- round(mean(`Containers`),2)
hybrid_cloud_overall <- round(mean(`Hybrid cloud`),2)
performance_overall <- round(mean(`Performance testing, metrics and analytics`),2)
network_overall <- round(mean(`Network management`),2)
database_overall <- round(mean(`Database management`),2)
storage_overall <- round(mean(`Storage management`),2)
machine_overall <- round(mean(`Machine learning and Artificial intelligence`),2)

names <- c("Development und Operations (DevOps)", 
           "Automation", 
           "Fachwissen über Cloud-Service-Plattformen", 
           "Cloud Sicherheit", 
           "Infrastruktur als Code",
           "Containers",
           "Hybrid cloud",
           "Leistungstests, Metriken und Analysen",
           "Netzwerk-Management",
           "Datenbank-Management",
           "Storage-Management",
           "Machine learning und Artificial intelligence"
) 
data <- c(devops_overall, 
          automation_overall, 
          cloud_service_platform_overall, 
          cloud_security_overall,
          iaas_overall,
          containers_overall,
          hybrid_cloud_overall,
          performance_overall,
          network_overall,
          database_overall,
          storage_overall,
          machine_overall
)

overall_skills_data <- data.frame(names, data)
overall_data_sorted <- overall_skills_data[order(overall_skills_data$data, decreasing = TRUE),]
par(mar=c(10, 15, 5, 8))
bar <- barplot(overall_data_sorted$data, names.arg = overall_data_sorted$names, col= c("#11AAFF", "#A2CDF4"), las=1, xlim=c(0,12), cex.names = 0.8, xlab = "Mittelwert", horiz = TRUE, main="Mittelwerte Overall")
axis(1, at=seq(0.0,12.0, by = 1))
text(x = overall_data_sorted$data * 0.8, y = bar, label = overall_data_sorted$data,col = "black")