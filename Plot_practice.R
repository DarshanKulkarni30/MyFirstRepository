
# Grouped Scatter plot with marginal density plots
ggscatterhist(
  data, x = "MonthlyIncome", y = "Age",
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



grp <- "Gender"

AP <- data1 %>%  group_by(across(all_of(grp))) %>% summarize(ATTR = mean(Attrition) * 100)
print(AP)

# Stage 2 Plotting
attrition_dept_plot <- ggplot(AP, aes(.data[[grp]],ATTR)) +
  geom_bar(stat = "identity", width = 0.5, fill="tomato2") +
  labs(title = title1,
       x = grp,
       y = "Attrition Rate (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  print(attrition_dept_plot)


geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Bar Chart", 
       subtitle="% Attrition by Age", 
       caption="Attrition Analysis by Age") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))