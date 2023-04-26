library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)

colors <- c("#5944C6", "#11AAFF", "#A2CDF4","#086ADB")
# Works only on local machine
data_survey <- read_excel(<FILE>)
data_survey$`Which area best fits your job?` == 'Engineer'
attach(data_survey)

####################################################################################
# Future Skills - Technical product - Engineer Importance AWS
####################################################################################
filtered_aws_eng_importance <- `AWS (Amazon Web Services)...66`[`Which area best fits your job?` == 'Engineer']

mean_importance_engineer_aws <- round(mean(filtered_aws_eng_importance),2)

####################################################################################
# Future Skills - Technical product - Engineer Importance Azure
####################################################################################
filtered_azure_eng_importance <- `Azure...67`[`Which area best fits your job?` == 'Engineer']

mean_importance_engineer_azure <- round(mean(filtered_azure_eng_importance),2)

####################################################################################
# Future Skills - Technical product - Engineer Importance Cloudfoundry
####################################################################################
filtered_cf_eng_importance <- `Cloudfoundry...68`[`Which area best fits your job?` == 'Engineer']

mean_importance_engineer_cf <- round(mean(filtered_cf_eng_importance),2)

####################################################################################
# Future Skills - Technical product - Engineer Importance Kubernetes
####################################################################################
filtered_kubernetes_eng_importance <- `Kubernetes...69`[`Which area best fits your job?` == 'Engineer']

mean_importance_engineer_kubernetes <- round(mean(filtered_kubernetes_eng_importance),2)

####################################################################################
# Future Skills - Technical product - Engineer Importance Linux 
####################################################################################
filtered_linux_eng_importance <- `Linux...70`[`Which area best fits your job?` == 'Engineer']

mean_importance_engineer_linux <- round(mean(filtered_linux_eng_importance),2)

####################################################################################
# Future Skills - Technical product - Engineer Importance VMware
####################################################################################
filtered_vmware_eng_importance <- `VMware`[`Which area best fits your job?` == 'Engineer']

mean_importance_engineer_vmware <- round(mean(filtered_vmware_eng_importance),2)

####################################################################################
# Future Skills - Technical product - Engineer Importance Windows
####################################################################################
filtered_windows_eng_importance <- `Windows (e.g. Server)...72`[`Which area best fits your job?` == 'Engineer']

mean_importance_engineer_windows <- round(mean(filtered_windows_eng_importance),2)

####################################################################################
# Future Skills - Technical product - Agile Importance AWS
####################################################################################
filtered_aws_agile_importance <- `AWS (Amazon Web Services)...66`[`Which area best fits your job?` == 'Agile roles']

mean_importance_agile_aws <- round(mean(filtered_aws_agile_importance),2)

####################################################################################
# Future Skills - Technical product - Agile Importance Azure
####################################################################################
filtered_azure_agile_importance <- `Azure...67`[`Which area best fits your job?` == 'Agile roles']

mean_importance_agile_azure <- round(mean(filtered_azure_agile_importance),2)
####################################################################################
# Future Skills - Technical product - Agile Importance Cloudfoundry
####################################################################################
filtered_cf_agile_importance <- `Cloudfoundry...68`[`Which area best fits your job?` == 'Agile roles']

mean_importance_agile_cf <- round(mean(filtered_cf_agile_importance),2)
####################################################################################
# Future Skills - Technical product - Agile Importance Kubernetes
####################################################################################
filtered_kubernetes_agile_importance <- `Kubernetes...69`[`Which area best fits your job?` == 'Agile roles']

mean_importance_agile_kubernetes <- round(mean(filtered_kubernetes_agile_importance),2)

####################################################################################
# Future Skills - Technical product - Agile Importance Linux 
####################################################################################
filtered_linux_agile_importance <- `Linux...70`[`Which area best fits your job?` == 'Agile roles']


mean_importance_agile_linux <- round(mean(filtered_linux_agile_importance),2)
####################################################################################
# Future Skills - Technical product - Agile Importance VMware
####################################################################################
filtered_vmware_agile_importance <- `VMware`[`Which area best fits your job?` == 'Agile roles']

mean_importance_agile_vmware <- round(mean(filtered_vmware_agile_importance),2)

####################################################################################
# Future Skills - Technical product - Agile Importance Windows
####################################################################################
filtered_windows_agile_importance <- `Windows (e.g. Server)...72`[`Which area best fits your job?` == 'Agile roles']

mean_importance_agile_windows <- round(mean(filtered_windows_agile_importance),2)


# Sample data
product_names <- c("AWS", "Azure", "Cloudfoundry", "Kubernetes", "Linux", "VMware", "Windows")
engineer_data <- c(mean_importance_engineer_aws, mean_importance_engineer_azure, mean_importance_engineer_cf, mean_importance_engineer_kubernetes, mean_importance_engineer_linux,mean_importance_engineer_vmware, mean_importance_engineer_windows)
agile_data <- c(mean_importance_agile_aws, mean_importance_agile_azure, mean_importance_agile_cf, mean_importance_agile_kubernetes, mean_importance_agile_linux,mean_importance_agile_vmware, mean_importance_agile_windows)

data_sorted_by_product <- c(mean_importance_engineer_aws, mean_importance_agile_aws, mean_importance_engineer_azure, mean_importance_agile_azure, mean_importance_engineer_cf, mean_importance_agile_cf, mean_importance_engineer_kubernetes, mean_importance_agile_kubernetes, mean_importance_engineer_linux, mean_importance_agile_linux, mean_importance_engineer_vmware, mean_importance_agile_vmware, mean_importance_engineer_windows, mean_importance_agile_windows)

# Combine the values into a matrix
data_matrix <- rbind(engineer_data, agile_data)

# Create the barplot
bar <- barplot(data_matrix, beside = TRUE, names.arg = product_names, col = c("#5944C6", "#11AAFF"),
               main = "Wichtigkeit der Produkte", xlab = "Produkte", ylab = "Mittelwert", ylim=c(0,7), cex.names = 0.8)

legend(legend = c("Engineer", "Agile Rollen"),cex = 1.2, fill = colors, x = 18, y = 7)
text(x = bar, y = data_sorted_by_product + 0.3, label = data_matrix,col = "black")
