library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)

colors <- c("#5944C6", "#11AAFF", "#A2CDF4","#086ADB")

data_survey <- read_excel(<FILE>)
data_survey$`Which area best fits your job?` == 'Engineer'
attach(data_survey)

####################################################################################
# Future Skills - Programming languages - Bash
####################################################################################
bash_language_table <- table(`Bash / Shell`)

# Replace values
bash_coded <- ifelse(`Bash / Shell` == 'Very Important', 3,
                     ifelse(`Bash / Shell` == 'Moderately Important',2, 1))
# Mean 
mean_bash <- round(mean(bash_coded),2)

####################################################################################
# Future Skills - Programming languages - C#
####################################################################################
csharp_language_table <- (`C#`)
  
# Replace values
csharp_coded <- ifelse(`C#` == 'Very Important', 3,
                     ifelse(`C#` == 'Moderately Important',2, 1))
# Mean 
mean_csharp <- round(mean(csharp_coded),2)
####################################################################################
# Future Skills - Programming languages - Go
####################################################################################
go_language_table <- (`Go`)

# Replace values
go_coded <- ifelse(`Go` == 'Very Important', 3,
                       ifelse(`Go` == 'Moderately Important',2, 1))
# Mean 
mean_go <- round(mean(go_coded),2)
####################################################################################
# Future Skills - Programming languages - HTML / CSS
####################################################################################
html_language_table <- (`HTML / CSS`)


# Replace values
html_coded <- ifelse(`HTML / CSS` == 'Very Important', 3,
                       ifelse(`HTML / CSS` == 'Moderately Important',2, 1))
# Mean 
mean_html <- round(mean(html_coded),2)
####################################################################################
# Future Skills - Programming languages - Java
####################################################################################
java_language_table <- (`Java`)


# Replace values
java_coded <- ifelse(`Java` == 'Very Important', 3,
                     ifelse(`Java` == 'Moderately Important',2, 1))
# Mean 
mean_java <- round(mean(java_coded),2)
####################################################################################
# Future Skills - Programming languages - JavaScript
####################################################################################
javascript_language_table <- (`JavaScript`)


# Replace values
javascript_coded <- ifelse(`JavaScript` == 'Very Important', 3,
                     ifelse(`JavaScript` == 'Moderately Important',2, 1))
# Mean 
mean_javascript <- round(mean(javascript_coded),2)

####################################################################################
# Future Skills - Programming languages - Kotlin
####################################################################################
kotlin_language_table <- (`Kotlin`)


# Replace values
kotlin_coded <- ifelse(`Kotlin` == 'Very Important', 3,
                           ifelse(`Kotlin` == 'Moderately Important',2, 1))
# Mean 
mean_kotlin <- round(mean(kotlin_coded),2)

####################################################################################
# Future Skills - Programming languages - PowerShell
####################################################################################
powershell_language_table <- (`PowerShell`)


# Replace values
powershell_coded <- ifelse(`PowerShell` == 'Very Important', 3,
                       ifelse(`PowerShell` == 'Moderately Important',2, 1))
# Mean 
mean_powershell <- round(mean(powershell_coded),2)
####################################################################################
# Future Skills - Programming languages - Python
####################################################################################
python_language_table <- (`Python`)

# Replace values
python_coded <- ifelse(`Python` == 'Very Important', 3,
                           ifelse(`Python` == 'Moderately Important',2, 1))
# Mean 
mean_python <- round(mean(python_coded),2)
####################################################################################
# Future Skills - Programming languages - Ruby
####################################################################################
ruby_language_table <- (`Ruby`)

# Replace values
ruby_coded <- ifelse(`Ruby` == 'Very Important', 3,
                       ifelse(`Ruby` == 'Moderately Important',2, 1))
# Mean 
mean_ruby <- round(mean(ruby_coded),2)
####################################################################################
# Future Skills - Programming languages - Rust
####################################################################################
rust_language_table <- (`Rust`)

# Replace values
rust_coded <- ifelse(`Rust` == 'Very Important', 3,
                     ifelse(`Rust` == 'Moderately Important',2, 1))
# Mean 
mean_rust <- round(mean(rust_coded),)

####################################################################################
# Future Skills - Programming languages - SQL
####################################################################################
sql_language_table <- (`SQL`)

# Replace values
sql_coded <- ifelse(`SQL` == 'Very Important', 3,
                     ifelse(`SQL` == 'Moderately Important',2, 1))
# Mean 
mean_sql <- round(mean(sql_coded),2)

####################################################################################
# Future Skills - Programming languages - TypeScript
####################################################################################
typescript_language_table <- (`TypeScript`)

# Replace values
typescript_coded <- ifelse(`TypeScript` == 'Very Important', 3,
                     ifelse(`TypeScript` == 'Moderately Important',2, 1))
# Mean 
mean_typescript <- round(mean(typescript_coded),2)

####################################################################################
# Future Skills - Programming languages - Diagram
####################################################################################
  names <- c("Bash", 
           "C#", 
           "Go", 
           "HTML", 
           "Java",
           "JavaScript",
           "Kotlin",
           "PowerShell",
           "Python",
           "Ruby",
           "Rust",
           "SQL",
           "TypeScript"

) 
data <- c(mean_bash, 
          mean_csharp, 
          mean_go,
          mean_html,
          mean_java,
          mean_javascript,
          mean_kotlin,
          mean_powershell,
          mean_python,
          mean_ruby,
          mean_rust,
          mean_sql,
          mean_typescript
)

programming_skills_data <- data.frame(names, data)
programming_data_sorted <- programming_skills_data[order(programming_skills_data$data, decreasing = TRUE),]
par(mar=c(10, 15, 5, 8))
bar <- barplot(programming_data_sorted$data, names.arg = programming_data_sorted$names, col= c("#11AAFF", "#A2CDF4"), las=1, xlim=c(0,3), cex.names = 0.8, xlab = "Mittelwert", horiz = TRUE, main="Mittelwerte Programmiersprachen")
text(x = programming_data_sorted$data * 0.8, y = bar, label = programming_data_sorted$data,col = "black")