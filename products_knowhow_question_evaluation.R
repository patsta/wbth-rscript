library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)

options(OutDec=",")

colors <- c("#5944C6", "#11AAFF", "#A2CDF4","#086ADB")

data_survey <- read_excel(<FILE>)
data_survey$`Which area best fits your job?` == 'Engineer'
attach(data_survey)

####################################################################################
# Future Skills - Technical product - Engineer Know-How AWS
####################################################################################
filtered_aws_eng_knowhow <- `AWS (Amazon Web Services)...58`[`Which area best fits your job?` == 'Engineer']

engineer_aws_knowhow_coded <- ifelse(filtered_aws_eng_knowhow == 'Extremely well', 5,
                              ifelse(filtered_aws_eng_knowhow == 'Quite well',4,
                              ifelse(filtered_aws_eng_knowhow ==  'Fairly well', 3,
                              ifelse(filtered_aws_eng_knowhow == 'Mildly well', 2,1))))

mean_engineer_aws <- round(mean(engineer_aws_knowhow_coded),2)

####################################################################################
# Future Skills - Technical product - Engineer Know-How Azure
####################################################################################
filtered_azure_eng_knowhow <- `Azure...59`[`Which area best fits your job?` == 'Engineer']

engineer_azure_knowhow_coded <- ifelse(filtered_azure_eng_knowhow == 'Extremely well', 5,
                                     ifelse(filtered_azure_eng_knowhow == 'Quite well',4,
                                            ifelse(filtered_azure_eng_knowhow ==  'Fairly well', 3,
                                                   ifelse(filtered_azure_eng_knowhow == 'Mildly well', 2,1))))

mean_engineer_azure <- round(mean(engineer_azure_knowhow_coded),2)


####################################################################################
# Future Skills - Technical product - Engineer Know-How Cloudfoundry
####################################################################################
filtered_cf_eng_knowhow <- `Cloudfoundry...60`[`Which area best fits your job?` == 'Engineer']

engineer_cf_knowhow_coded <- ifelse(filtered_cf_eng_knowhow == 'Extremely well', 5,
                                       ifelse(filtered_cf_eng_knowhow == 'Quite well',4,
                                              ifelse(filtered_cf_eng_knowhow ==  'Fairly well', 3,
                                                     ifelse(filtered_cf_eng_knowhow == 'Mildly well', 2,1))))

mean_engineer_cf <- round(mean(engineer_cf_knowhow_coded),2)


####################################################################################
# Future Skills - Technical product - Engineer Know-How Kubernetes
####################################################################################
filtered_kubernetes_eng_knowhow <- `Kubernetes...61`[`Which area best fits your job?` == 'Engineer']

engineer_kubernetes_knowhow_coded <- ifelse(filtered_kubernetes_eng_knowhow == 'Extremely well', 5,
                                    ifelse(filtered_kubernetes_eng_knowhow == 'Quite well',4,
                                           ifelse(filtered_kubernetes_eng_knowhow ==  'Fairly well', 3,
                                                  ifelse(filtered_kubernetes_eng_knowhow == 'Mildly well', 2,1))))

mean_engineer_kubernetes <- round(mean(engineer_kubernetes_knowhow_coded),2)


####################################################################################
# Future Skills - Technical product - Engineer Know-How Linux 
####################################################################################
filtered_linux_eng_knowhow <- `Linux...62`[`Which area best fits your job?` == 'Engineer']

engineer_linux_knowhow_coded <- ifelse(filtered_linux_eng_knowhow == 'Extremely well', 5,
                                            ifelse(filtered_linux_eng_knowhow == 'Quite well',4,
                                                   ifelse(filtered_linux_eng_knowhow ==  'Fairly well', 3,
                                                          ifelse(filtered_linux_eng_knowhow == 'Mildly well', 2,1))))

mean_engineer_linux <- round(mean(engineer_linux_knowhow_coded),2)

####################################################################################
# Future Skills - Technical product - Engineer Know-How VMware
####################################################################################
filtered_vmware_eng_knowhow <- `VMware (e.g. NSX, vCenter)`[`Which area best fits your job?` == 'Engineer']

engineer_vmware_knowhow_coded <- ifelse(filtered_vmware_eng_knowhow == 'Extremely well', 5,
                                       ifelse(filtered_vmware_eng_knowhow == 'Quite well',4,
                                              ifelse(filtered_vmware_eng_knowhow ==  'Fairly well', 3,
                                                     ifelse(filtered_vmware_eng_knowhow == 'Mildly well', 2,1))))

mean_engineer_vmware <- round(mean(engineer_vmware_knowhow_coded),2)

####################################################################################
# Future Skills - Technical product - Engineer Know-How Windows
####################################################################################
filtered_windows_eng_knowhow <- `Windows (e.g. Server)...64`[`Which area best fits your job?` == 'Engineer']

engineer_windows_knowhow_coded <- ifelse(filtered_windows_eng_knowhow == 'Extremely well', 5,
                                        ifelse(filtered_windows_eng_knowhow == 'Quite well',4,
                                               ifelse(filtered_windows_eng_knowhow ==  'Fairly well', 3,
                                                      ifelse(filtered_windows_eng_knowhow == 'Mildly well', 2,1))))

mean_engineer_windows <- round(mean(engineer_windows_knowhow_coded),2)

####################################################################################
# Future Skills - Technical product - Agile Know-How AWS
####################################################################################
filtered_aws_agile_knowhow <- `AWS (Amazon Web Services)...58`[`Which area best fits your job?` == 'Agile roles']

agile_aws_knowhow_coded <- ifelse(filtered_aws_agile_knowhow == 'Extremely well', 5,
                                     ifelse(filtered_aws_agile_knowhow == 'Quite well',4,
                                            ifelse(filtered_aws_agile_knowhow ==  'Fairly well', 3,
                                                   ifelse(filtered_aws_agile_knowhow == 'Mildly well', 2,1))))

mean_agile_aws <- round(mean(agile_aws_knowhow_coded),2)

####################################################################################
# Future Skills - Technical product - Agile Know-How Azure
####################################################################################
filtered_azure_agile_knowhow <- `Azure...59`[`Which area best fits your job?` == 'Agile roles']

agile_azure_knowhow_coded <- ifelse(filtered_azure_agile_knowhow == 'Extremely well', 5,
                                       ifelse(filtered_azure_agile_knowhow == 'Quite well',4,
                                              ifelse(filtered_azure_agile_knowhow ==  'Fairly well', 3,
                                                     ifelse(filtered_azure_agile_knowhow == 'Mildly well', 2,1))))

mean_agile_azure <- round(mean(agile_azure_knowhow_coded),2)

####################################################################################
# Future Skills - Technical product - Agile Know-How Cloudfoundry
####################################################################################
filtered_cf_agile_knowhow <- `Cloudfoundry...60`[`Which area best fits your job?` == 'Agile roles']

agile_cf_knowhow_coded <- ifelse(filtered_cf_agile_knowhow == 'Extremely well', 5,
                                    ifelse(filtered_cf_agile_knowhow == 'Quite well',4,
                                           ifelse(filtered_cf_agile_knowhow ==  'Fairly well', 3,
                                                  ifelse(filtered_cf_agile_knowhow == 'Mildly well', 2,1))))

mean_agile_cf <- round(mean(agile_cf_knowhow_coded),2)


####################################################################################
# Future Skills - Technical product - Agile Know-How Kubernetes
####################################################################################
filtered_kubernetes_agile_knowhow <- `Kubernetes...61`[`Which area best fits your job?` == 'Agile roles']

agile_kubernetes_knowhow_coded <- ifelse(filtered_kubernetes_agile_knowhow == 'Extremely well', 5,
                                            ifelse(filtered_kubernetes_agile_knowhow == 'Quite well',4,
                                                   ifelse(filtered_kubernetes_agile_knowhow ==  'Fairly well', 3,
                                                          ifelse(filtered_kubernetes_agile_knowhow == 'Mildly well', 2,1))))

mean_agile_kubernetes <- round(mean(agile_kubernetes_knowhow_coded),2)

####################################################################################
# Future Skills - Technical product - Agile Know-How Linux 
####################################################################################
filtered_linux_agile_knowhow <- `Linux...62`[`Which area best fits your job?` == 'Agile roles']

agile_linux_knowhow_coded <- ifelse(filtered_linux_agile_knowhow == 'Extremely well', 5,
                                       ifelse(filtered_linux_agile_knowhow == 'Quite well',4,
                                              ifelse(filtered_linux_agile_knowhow ==  'Fairly well', 3,
                                                     ifelse(filtered_linux_agile_knowhow == 'Mildly well', 2,1))))

mean_agile_linux <- round(mean(agile_linux_knowhow_coded),2)
####################################################################################
# Future Skills - Technical product - Agile Know-How VMware
####################################################################################
filtered_vmware_agile_knowhow <- `VMware (e.g. NSX, vCenter)`[`Which area best fits your job?` == 'Agile roles']

agile_vmware_knowhow_coded <- ifelse(filtered_vmware_agile_knowhow == 'Extremely well', 5,
                                        ifelse(filtered_vmware_agile_knowhow == 'Quite well',4,
                                               ifelse(filtered_vmware_agile_knowhow ==  'Fairly well', 3,
                                                      ifelse(filtered_vmware_agile_knowhow == 'Mildly well', 2,1))))

mean_agile_vmware <- round(mean(agile_vmware_knowhow_coded),2)

####################################################################################
# Future Skills - Technical product - Agile Know-How Windows
####################################################################################
filtered_windows_agile_knowhow <- `Windows (e.g. Server)...64`[`Which area best fits your job?` == 'Agile roles']

agile_windows_knowhow_coded <- ifelse(filtered_windows_agile_knowhow == 'Extremely well', 5,
                                         ifelse(filtered_windows_agile_knowhow == 'Quite well',4,
                                                ifelse(filtered_windows_agile_knowhow ==  'Fairly well', 3,
                                                       ifelse(filtered_windows_agile_knowhow == 'Mildly well', 2,1))))

mean_agile_windows <- round(mean(agile_windows_knowhow_coded),2)



# Sample data
product_names <- c("AWS", "Azure", "CloudFoundry", "Kubernetes", "Linux", "VMware", "Windows")
engineer_data <- c(mean_engineer_aws, mean_engineer_azure, mean_engineer_cf, mean_engineer_kubernetes, mean_engineer_linux,mean_engineer_vmware, mean_engineer_windows)
agile_data <- c(mean_agile_aws, mean_agile_azure, mean_agile_cf, mean_agile_kubernetes, mean_agile_linux,mean_agile_vmware, mean_agile_windows)

data_sorted_by_product <- c(
                            mean_engineer_aws, 
                            mean_agile_aws, 
                            mean_engineer_azure, 
                            mean_agile_azure, 
                            mean_engineer_cf,
                            mean_agile_cf, 
                            mean_engineer_kubernetes, 
                            mean_agile_kubernetes, 
                            mean_engineer_linux, 
                            mean_agile_linux, 
                            mean_engineer_vmware, 
                            mean_agile_vmware, 
                            mean_engineer_windows, 
                            mean_agile_windows
                     )

# Combine the values into a matrix
data_matrix <- rbind(engineer_data, agile_data)

# Create the barplot
bar <- barplot(data_matrix, beside = TRUE, names.arg = product_names, col = c("#5944C6", "#11AAFF"),
        main = "Übersicht Kenntnisse der Produkte", xlab = "Produkte", ylab = "Mittelwert", ylim=c(0,5), cex.names = 0.8)

legend(legend = c("Engineer", "Agile Rollen"),cex = 1.2, fill = colors, x = 15, y = 5,)
text(x = bar, y = data_sorted_by_product + 0.2, label = data_matrix,col = "black")

