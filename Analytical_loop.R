library(dplyr)
library(ggplot2)
library(DataExplorer)
library(summarytools)
library(reshape2)  
library(ggpubr)
library(moments)


##### Import & Clean
setwd("C:\\Darshan\\Code\\R")

#Data Import
data <- read.csv("Employee_Attrition.csv")

#Creating temp data frame to work with
data1 <- data   
# Replicate example data
#Name of the column you want to analyze data against
Analysis="Attrition"
#Time in seconds you want to pause for each analysis
PAUSE <- 5


missing_values <- is.na(data1$Age)
print(missing_values)
print(class(data1$Age))


for (j in colnames(data1))
  {
  if (class(data1[[j]])!= Analysis) 
  {
    print(class(data1[[j]]))
    print(j)
    grp <- c(j)
    
    #### Show Summary
    print("Shwoing Summary")
    col_summary <- summary(data1[[grp]])
    print (col_summary)
    
    
    
    
    
    ####If data is numeric check for distribution / Skewness and kurtosis
    if (class(data1[[j]]) == "integer") {
      
      
      print("Process missing numeric values")
      missing_values <- sum(is.na(data1[[j]]))
      #print(missing_values)
      
      if (missing_values==0)
      {
        print(paste(missing_values," missing values in ", grp ," So nothin to do!!" ))
      }
      else
      {
        print(paste(missing_values," missing values in ", grp ," So processing ..")) 
        
        mean_value <- mean(data1[[j]], na.rm = TRUE)
        data1[[j]][is.na(data1[[j]])] <- mean_value
        
        
      }
      
      
      #Fix outliers for numerical data
      # Calculate Q1 and Q3 for the Age column
      Q1 <- quantile(data1[[j]], 0.25)
      Q3 <- quantile(data1[[j]], 0.75)
      
      # Calculate the IQR (Interquartile Range)
      IQR <- Q3 - Q1
      
      # Calculate the lower and upper bounds for outliers
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      print(paste("IQR is :",IQR))
      print(paste("lower_bound is :",lower_bound))
      print(paste("upper_bound is :",upper_bound))
      
      # Identify and treat outliers
      data1[[j]][data1[[j]] < lower_bound] <- lower_bound
      data1[[j]][data1[[j]] > upper_bound] <- upper_bound
      
      print(paste("Fixed outliers for: ",grp))
      
      
    print (paste("Since data is ",class(data1[[j]]) , " Lets check for distribution"))
    column_data <- data1[[grp]]
    skew <- skewness(column_data)
    print(paste("skewness  of", grp, "column:", skew))
    
    
    column_data <- data1[[grp]]
    kurt <- kurtosis(column_data)
    
    print(paste("Kurtosis of", grp, "column:", kurt))
    
    ####Shapiro-Wilk test to confirm normality. If P-value is > 0.05 then it can be considered normally dist.
    if (kurt != "NaN"){
    column_data <- data1[[grp]]
    shapiro_result <- shapiro.test(column_data)
    print(paste("P value of Shapiro-Wilk test for", grp, "column:", shapiro_result$p.value))
   
    
    if (shapiro_result$p.value > 0.05) {
      print(paste("The", grp, "column data is approximately normally distributed"))
    } else {
      print(paste("The", grp, "column data is not normally distributed"))
    }
    } else {print("Not possible to check shapiro.test, so skipping")}
    
    #column_range <- range(data1[[grp]])
    #print(column_range)
    
    #CRANGE<-column_range[2]-column_range[1]
    #print(paste("Range is ",CRANGE))
    #BINWIDTH<-CRANGE/10
    
    
    iqr <- IQR(data1[[grp]])
    
    # Calculate the Freedman-Diaconis bin width
    # Formula: 2 * IQR / (n^(1/3))
    BINWIDTH <- 2 * iqr / (length(data1[[grp]])^(1/3))
    if (BINWIDTH<=1){BINWIDTH<-1}
    print(paste("BINWIDTH is ",BINWIDTH))
    
    }
    
    Sys.sleep(PAUSE)
    
    
    title1 <- paste("% Attrition by ", j)
    AP <- data1 %>%  group_by(across(all_of(grp))) %>% summarize(ATTR = mean(.data[[Analysis]]) * 100)
    print(AP)
    
    # Stage 2 Plotting
    attrition_dept_plot <- ggplot(AP, aes(.data[[grp]],ATTR)) +
      geom_bar(stat = "identity", width = 0.5, fill="tomato2") +
      labs(title = title1,
           x = grp,
           y = "Attrition Rate (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Display the plot
    print(attrition_dept_plot)
    print("Attrition Plot Printed")
    Sys.sleep(PAUSE)
    
    ### Histogram - 
    
    # Display the histogram
    if (class(data1[[j]]) == "integer") {
      print("Printing Histogram")
    P_histogram <- ggplot(data1, aes(x = .data[[grp]])) +
      geom_histogram(binwidth = BINWIDTH, fill = "tomato", color = "black")
    
    # Display the histogram
    print(P_histogram)
    print("Printed Histogram")
    
    
    
    
  
  
    }
    
    else
    {
      print("Fix outliers for non numeric data")
      print("Dont know how to do it baby")
    }
    
  }
 
  Sys.sleep(PAUSE)
  }
  



##### Check Missing Values
# Check for missing values in each column - numeric
#missing_values <- colSums(is.na(data1))

# Display the count of missing values for each column
#print(missing_values)

