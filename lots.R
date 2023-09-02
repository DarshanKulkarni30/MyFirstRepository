library(dplyr)
library(ggplot2)
library(DataExplorer)
library(summarytools)
library(reshape2)  
library(ggpubr)


##### Import & Clean
setwd("C:\\Darshan\\Code\\R")

#Data Import
data <- read.csv("Employee_Attrition.csv")

##ggplot
attrition_plot <- ggplot(data, aes(x = factor(Attrition))) + geom_bar() +
  labs(tutle="Attrition",
       x="Attrition",
       y="Count")
print(attrition_plot)


# Grouped Scatter plot with marginal density plots
ggscatterhist(
  data, x = "MonthlyIncome", y = "TotalWorkingYears",
  color = "Department", size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.params = list(fill = "Department", color = "black", size = 0.2)
)


ggscatterhist(
  data, x = "MonthlyIncome", y = "Age",
  color = "Department", size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.plot = "boxplot",
  ggtheme = theme_bw()
)


#Basic density plot:
# Basic density plot
ggplot(data, aes(MonthlyIncome)) +
  geom_density()

# Add mean line
ggplot(data, aes(MonthlyIncome)) +
  geom_density(fill = "lightgray") +
  geom_vline(aes(xintercept = mean(MonthlyIncome)), linetype = 2)



# Change line color by groups
ggplot(data, aes(MonthlyIncome, color = Department)) +
  geom_density() +
  scale_color_viridis_d()


data1 <- data                   # Replicate example data


for (j in colnames(data1)){
  if (class(data1[[j]])=="integer") 
  {print("Numeric")
    print(j)
    grp <- c(j)
    title1 <- paste("% Attrition by ", j)
    AP <- data1 %>%  group_by(across(all_of(grp))) %>% summarize(ATTR = mean(Attrition) * 100)
    print(AP)

    # Stage 2 Plotting
    attrition_dept_plot <- ggplot(AP, aes(.data[[grp]],ATTR)) +
      geom_bar(stat = "identity") +
      labs(title = title1,
           x = grp,
           y = "Attrition Rate (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Display the plot
    print(attrition_dept_plot)
    
    }
  else {print("Contexttual") 
    print(j)
    grp <- c(j)
    title1 <- paste("% Attrition by ", j)
    AP <- data1 %>%  group_by(across(all_of(grp))) %>% summarize(ATTR = mean(Attrition) * 100)
    print(AP)
    # Stage 2 Plotting
    attrition_dept_plot <- ggplot(AP, aes(.data[[grp]],ATTR)) +
      geom_bar(stat = "identity") +
      labs(title = title1,
           x = grp,
           y = "Attrition Rate (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Display the plot
    print(attrition_dept_plot)
    
    
    }
  Sys.sleep(5)
}


