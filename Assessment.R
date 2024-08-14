# Load necessary libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(rworldmap)
library(maps)
library(ggmap)
library(gridExtra) # For grid.arrange

# Import the data set
data <- read.csv("dataset.csv")

# --- Data Pre-processing ---

# Rename columns for readability
data <- data %>%
  rename(
    JobTitle = Job.Title,
    EmploymentType = Employment.Type,
    ExperienceLevel = Experience.Level,
    ExpertiseLevel = Expertise.Level,
    SalaryCurrency = Salary.Currency,
    CompanyLocation = Company.Location,
    SalaryInUSD = Salary.in.USD,
    EmployeeResidence = Employee.Residence,
    CompanySize = Company.Size,
    Year = Year
  )

# --- Exploratory Data Analysis (EDA) --- 

# Summary statistics
summary(data)

# Data types and missing values
str(data)

# --- Non-Trivial Visualizations ---

# Question 1: Do average salaries for Data Scientists and Data Engineers increase significantly between 'Expert' and 'Junior' expertise levels in the United States?
# (Separate Line Charts)
filtered_data <- data %>%
  filter(JobTitle %in% c("Data Scientist", "Data Engineer")) %>% 
  filter(CompanyLocation == "United States") 

# Data Scientist Plot
p1a <- ggplot(filtered_data %>% filter(JobTitle == "Data Scientist"), aes(x = ExpertiseLevel, y = SalaryInUSD, group = JobTitle)) +
  stat_summary(fun = "mean", geom = "line", size = 1) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1) +
  labs(x = "Expertise Level", y = "Average Salary (USD)") +
  ggtitle("Average Salary for Data Scientists by Expertise Level (US)") +
  scale_color_manual(values = c("Data Scientist" = "#00AFBB")) + # Only one color needed here
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "gray", size = 0.2, linetype = "dashed"), 
        panel.grid.minor = element_line(color = "gray", size = 0.1, linetype = "dashed")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) # Display in K terms
# Display the plot
print(p1a) 

# Data Engineer Plot
p1b <- ggplot(filtered_data %>% filter(JobTitle == "Data Engineer"), aes(x = ExpertiseLevel, y = SalaryInUSD, group = JobTitle)) +
  stat_summary(fun = "mean", geom = "line", size = 1) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1) +
  labs(x = "Expertise Level", y = "Average Salary (USD)") +
  ggtitle("Average Salary for Data Engineers by Expertise Level (US)") +
  scale_color_manual(values = c("Data Engineer" = "#E7B800")) + # Only one color needed here
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "gray", size = 0.2, linetype = "dashed"), 
        panel.grid.minor = element_line(color = "gray", size = 0.1, linetype = "dashed")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) 

# Display the plot
print(p1b) 

# (Combined Chart)
filtered_data <- data %>%
  filter(JobTitle %in% c("Data Scientist", "Data Engineer")) %>% 
  filter(CompanyLocation == "United States") 

p1c <- ggplot(filtered_data, aes(x = ExpertiseLevel, y = SalaryInUSD, color = JobTitle, group = JobTitle)) +
  stat_summary(fun = "mean", geom = "line", size = 1) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1) +
  labs(x = "Expertise Level", y = "Average Salary (USD)") +
  ggtitle("Average Salary for Data Scientists and Data Engineers by Expertise Level (US)") +
  scale_color_manual(values = c("Data Scientist" = "#00AFBB", "Data Engineer" = "#E7B800")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "gray", size = 0.2, linetype = "dashed"), 
        panel.grid.minor = element_line(color = "gray", size = 0.1, linetype = "dashed")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) # Display in K terms

# Display the plot
print(p1c)

# Create the plot using ggplot and ggplotly
p2 <- ggplot(data, aes(x = ExperienceLevel, y = SalaryInUSD, color = ExperienceLevel)) +
  geom_point(alpha = 0.7) +
  labs(x = "Experience Level", y = "Salary (USD)", title = "Salary vs. Experience by Employment Type") +
  facet_wrap(~ EmploymentType, scales = "free") +
  theme_minimal() +
  scale_y_continuous(labels = comma_format(scale = 1e-3, suffix = "K")) 

p2_interactive <- ggplotly(p2)

# Display the plot
p2_interactive

# Question 3: "How does the concentration of large data science companies differ across continents, and are there any interesting regional patterns?"
# (Interactive Map)


# 1. Prepare data for mapping
data_for_map <- data %>%
  filter(CompanySize == "Large") %>% 
  group_by(CompanyLocation) %>%
  summarize(count = n())

# 2. Join with country map data
sPDF <- joinCountryData2Map(data_for_map, joinCode = "NAME", nameJoinColumn = "CompanyLocation")

# 3. Extract data for plotting
map_data <- data.frame(
  country = sPDF$NAME,
  count = sPDF$count,
  continent = sPDF$continent,
  long = coordinates(sPDF)[, 1],
  lat = coordinates(sPDF)[, 2]
)

# 4. Filter out countries with a count of zero or NA
map_data <- map_data %>%
  filter(count > 0)

# 5. Create the interactive map using plotly
plot_ly(data = map_data,
        type = 'choropleth',
        locations = ~country,
        locationmode = 'country names',
        z = ~count,
        text = ~paste(country, '<br>Count:', count),
        colorscale = 'Viridis',
        colorbar = list(title = 'Number of Large Companies'),
        hoverinfo = "text"
) %>%
  layout(
    title = 'Distribution of Large Data Science Companies by Continent',
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = 'natural earth')
    )
  ) %>%
  layout(facet = list(type = "wrap", facet_row = ~continent, nrow = 2))


#Question 4-Is there a trend for salaries to increase faster for "Senior" data job role compared to "Mid" or "Entry" level professionals over the years?

# Calculate average salary for each year and experience level
salary_trend_by_experience <- data %>%
  group_by(Year, ExperienceLevel) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE))

# Create an interactive line plot showing salary trends for each experience level
p <- ggplotly(
  ggplot(salary_trend_by_experience, aes(x = Year, y = AverageSalary, color = ExperienceLevel, group = ExperienceLevel)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(x = "Year", y = "Average Salary (USD)", title = "Salary Trends by Experience Level") +
    theme_minimal() 
)

print(p)



# Question 5: Impact of Residence on Salary, Considering Job Title and Experience
# (Scatter plot with faceting)

# Group by residence, job title, and experience level
residence_salary_data <- data %>%
  group_by(EmployeeResidence, JobTitle, ExperienceLevel) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE))

# Create the visualization (scatter plot with faceting)
p5 <- ggplot(residence_salary_data, aes(x = ExperienceLevel, y = AverageSalary, color = ExperienceLevel)) +
  geom_point() +
  labs(x = "Experience Level", y = "Average Salary (USD)", title = "Average Salary by Residence, Job Title, and Experience Level") +
  facet_grid(EmployeeResidence ~ JobTitle) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)

# Question 6: Comparison of Salaries in Large Companies Considering Job Title and Experience
# (Grouped Bar Chart)

# Group by company size, job title, and experience level
company_size_salary <- data %>%
  group_by(CompanySize, JobTitle, ExperienceLevel) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE)) %>%
  ungroup()

# Filter for large companies only
large_company_salary <- company_size_salary %>%
  filter(CompanySize == "Large")

# Create the visualization (grouped bar chart)
p6 <- ggplot(large_company_salary, aes(x = ExperienceLevel, y = AverageSalary, fill = ExperienceLevel)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ JobTitle, scales = "free") +
  labs(
    x = "Experience Level",
    y = "Average Salary in USD",
    title = "Average Salary by Job Title and Experience Level in Large Companies"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(p6)

# Question 7: How Has Company Size Distribution Changed Over Time?
# (Pie Chart and Bar Chart)

# Calculate proportions of company sizes for each year
data_proportions <- data %>%
  group_by(Year, CompanySize) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

# Bar chart of company size distribution over time
p7_bar <- ggplot(data_proportions, aes(x = Year, y = Count, fill = CompanySize)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Year",
    y = "Count",
    title = "Company Size Distribution Over Time"
  ) +
  theme_minimal()

print(p7_bar)

# Pie chart for a specific year (e.g., 2022)
specific_year <- 2022
data_specific_year <- data_proportions %>%
  filter(Year == specific_year)

p7_pie <- ggplot(data_specific_year, aes(x = "", y = Proportion, fill = CompanySize)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(
    x = NULL,
    y = NULL,
    title = paste("Company Size Distribution in", specific_year)
  ) +
  theme_void() +
  theme(legend.position = "right")

print(p7_pie)

# Question 8: How do salary expectations vary across different job roles in the data science field, and are there notable trends in this variation over time?
# (Interactive Plot)

# Calculate the average salary for each job role and year
salary_by_role_year <- data %>%
  group_by(Year, JobTitle) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE)) 

# Get the top 10 highest-paying job roles
top_roles <- salary_by_role_year %>%
  group_by(JobTitle) %>%
  summarize(MeanSalary = mean(AverageSalary)) %>%
  arrange(desc(MeanSalary)) %>%
  slice(1:10) %>%
  pull(JobTitle)

# Filter the salary data for the top 10 roles
salary_by_role_year_top <- salary_by_role_year %>%
  filter(JobTitle %in% top_roles)

# Create an interactive plot showing salary trends for the top 10 roles over time
p8 <- ggplotly(
  ggplot(salary_by_role_year_top, aes(x = Year, y = AverageSalary, color = JobTitle, group = JobTitle)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(x = "Year", y = "Average Salary (USD)", title = "Salary Trends for Top 10 Data Science Job Roles") +
    theme_minimal()
)

print(p8)



# Question 9: What is the relationship between company size and salary for different job roles based on the region the employee lives?

# Feature Engineering: Create Continent Column
data <- data %>%
  mutate(Continent = countrycode(EmployeeResidence, "country.name", "continent"))

# Filter data for Data Scientist, Data Analyst, and Data Engineer roles
filtered_data <- data %>%
  filter(JobTitle %in% c("Data Scientist", "Data Analyst", "Data Engineer"))

# Create an interactive scatter plot with company size and salary, faceted by employee continent
p9 <- ggplotly(
  ggplot(filtered_data, aes(x = CompanySize, y = SalaryInUSD, color = Continent)) +
    geom_point(alpha = 0.7) +
    labs(x = "Company Size", y = "Salary (USD)", title = "Company Size vs. Salary for Data Science Roles (By Employee Continent)") +
    facet_wrap(~ JobTitle, ncol = 3) +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))
)

print(p9)

# Question 10: How does the geographic distribution of data science professionals vary by job role, and are there any notable trends in this distribution over time?
# (Interactive Map with Faceting)

# Calculate the number of professionals in each location for each job role and year
location_by_role_year <- data %>%
  group_by(Year, JobTitle, CompanyLocation) %>%
  summarize(Count = n()) 

# Create an interactive map showing the geographic distribution of data science professionals, faceted by job role and year
p10 <- ggplotly(
  ggplot(location_by_role_year, aes(map_id = CompanyLocation, fill = Count)) +
    geom_map(map = map_data("world"), color = "white", size = 0.25) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
    labs(title = "Geographic Distribution of Data Science Professionals") +
    facet_grid(JobTitle ~ Year) +
    theme_minimal()
)

print(p10)
# Load necessary libraries
library(tidyverse)

# Assuming `data` is your dataset and it has columns `Year`, `JobTitle`, and `SalaryInUSD`

# Calculate the average salary for each job role and year
salary_by_role_year <- data %>%
  group_by(Year, JobTitle) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE))

# Get the top 10 highest-paying job roles
top_roles <- salary_by_role_year %>%
  group_by(JobTitle) %>%
  summarize(MeanSalary = mean(AverageSalary)) %>%
  arrange(desc(MeanSalary)) %>%
  slice(1:10) %>%
  pull(JobTitle)

# Filter the salary data for the top 10 roles
salary_by_role_year_top <- salary_by_role_year %>%
  filter(JobTitle %in% top_roles)

# Create a line plot with a nicer theme
p8 <- ggplot(salary_by_role_year_top, aes(x = Year, y = AverageSalary, color = JobTitle, group = JobTitle)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Average Salary (USD)", title = "Salary Trends for Top 10 Data Science Job Roles") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_blank(), # Remove legend title for simplicity
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) +
  # Add labels to the end of each line, colored to match the lines
  geom_text(
    data = salary_by_role_year_top %>% 
      group_by(JobTitle) %>%
      slice_tail(n = 1), 
    aes(label = JobTitle, color = JobTitle), 
    hjust = -0.1, vjust = 0.5, size = 4, show.legend = FALSE
  )

# Display the plot
print(p8)



# 1. Prepare data for mapping
data_for_map <- data %>%
  filter(CompanySize == "Large") %>% 
  group_by(CompanyLocation) %>%
  summarize(count = n())

# 2. Join with country map data
sPDF <- joinCountryData2Map(data_for_map, joinCode = "NAME", nameJoinColumn = "CompanyLocation")

# 3. Create the static map
ggplot() +
  geom_polygon(data = sPDF, aes(x = long, y = lat, group = group, fill = count), color = "gray", size = 0.2) +
  labs(title = "Distribution of Large Data Science Companies by Continent", fill = "Number of Companies") +
  theme_bw() +
  scale_fill_viridis_c(option = "magma", direction = -1) +  # Use a color scale for better visualization
  coord_map("ortho", orientation = c(90, 0, 0)) +  # Set map projection
  theme(
    panel.grid = element_blank(), 
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16) 
  )



# Load necessary libraries
library(tidyverse)
library(plotly)

# Assuming `data` is your dataset and it has columns `Year`, `JobTitle`, and `SalaryInUSD`

# Calculate the average salary for each job role and year
salary_by_role_year <- data %>%
  group_by(Year, JobTitle) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE))

# Get the top 10 highest-paying job roles
top_roles <- salary_by_role_year %>%
  group_by(JobTitle) %>%
  summarize(MeanSalary = mean(AverageSalary)) %>%
  arrange(desc(MeanSalary)) %>%
  slice(1:10) %>%
  pull(JobTitle)

# Filter the salary data for the top 10 roles
salary_by_role_year_top <- salary_by_role_year %>%
  filter(JobTitle %in% top_roles)

# Create a line plot
p8 <- ggplot(salary_by_role_year_top, aes(x = Year, y = AverageSalary, color = JobTitle, group = JobTitle)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Average Salary (USD)", title = "Salary Trends for Top 10 Data Science Job Roles") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "right" # Include the legend
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

# Convert ggplot to plotly
p8_plotly <- ggplotly(p8)

# Display the plotly plot
p8_plotly




library(ggplot2)
library(dplyr)
library(forcats) 

# ... (Your data loading and preprocessing code)

# --- Data Preprocessing ---

# Calculate the count of each experience level for each job title
job_experience_counts <- data %>%
  group_by(JobTitle, ExperienceLevel) %>%
  summarise(count = n())

# Get the top 20 job titles based on total count
top_20_job_titles <- job_experience_counts %>%
  group_by(JobTitle) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%  # Arrange in descending order of total_count
  head(20) %>%
  pull(JobTitle)

# Filter the data to include only the top 20 job titles
filtered_data <- job_experience_counts %>%
  filter(JobTitle %in% top_20_job_titles)

# --- Visualization ---

ggplot(filtered_data, aes(x = JobTitle, y = count, fill = ExperienceLevel)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Experience Levels Across Top 20 Job Titles",
       x = "Job Title",
       y = "Count",
       fill = "Experience Level") +
  theme(axis.text.x = element_text(hjust = 1)) + 
  coord_flip() 


# Calculate average salaries by JobTitle and Year
data_summary <- data %>%
  group_by(Year, JobTitle) %>%
  summarise(AverageSalary = mean(SalaryInUSD, na.rm = TRUE)) %>%
  ungroup()

# Plotting with ggplot2
p <- ggplot(data_summary, aes(x = JobTitle, y = Year, color = AverageSalary)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "blue", high = "red", guide = "legend", name = "Average Salary (USD)") +
  labs(title = "Average Salary Over Time by Job Role",
       x = "Job Role",
       y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p)













# Load necessary libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(rworldmap)
library(maps)
library(ggmap)
library(gridExtra) # For grid.arrange

# Import the data set
data <- read.csv("dataset.csv")

# --- Data Preprocessing ---

# Rename columns for readability
data <- data %>%
  rename(
    JobTitle = Job.Title,
    EmploymentType = Employment.Type,
    ExperienceLevel = Experience.Level,
    ExpertiseLevel = Expertise.Level,
    SalaryCurrency = Salary.Currency,
    CompanyLocation = Company.Location,
    SalaryInUSD = Salary.in.USD,
    EmployeeResidence = Employee.Residence,
    CompanySize = Company.Size,
    Year = Year
  )

# --- Exploratory Data Analysis (EDA) --- 

# Summary statistics
summary(data)

# Data types and missing values
str(data)

# --- Non-Trivial Visualizations ---

# Question 1: Do average salaries for Data Scientists and Data Engineers increase significantly between 'Expert' and 'Junior' expertise levels in the United States?
# (Separate Line Charts)
filtered_data <- data %>%
  filter(JobTitle %in% c("Data Scientist", "Data Engineer")) %>% 
  filter(CompanyLocation == "United States") 
# Define a custom palette
custom_palette <- c("#00AFBB", "#E7B800", "#FC4E07", "#6ECA92", "#845EC2")

# Data Scientist Plot
p1a <- ggplot(filtered_data %>% filter(JobTitle == "Data Scientist"), aes(x = ExpertiseLevel, y = SalaryInUSD, group = JobTitle)) +
  stat_summary(fun = "mean", geom = "line", size = 1) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1) +
  labs(x = "Expertise Level", y = "Average Salary (USD)") +
  ggtitle("Average Salary for Data Scientists by Expertise Level (US)") +
  scale_color_manual(values = custom_palette[1]) +  # Use the first color from custom palette
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "gray", size = 0.2, linetype = "dashed"), 
        panel.grid.minor = element_line(color = "gray", size = 0.1, linetype = "dashed")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))  # Display in K terms

# Display the plot
print(p1a) 

# Data Engineer Plot
p1b <- ggplot(filtered_data %>% filter(JobTitle == "Data Engineer"), aes(x = ExpertiseLevel, y = SalaryInUSD, group = JobTitle)) +
  stat_summary(fun = "mean", geom = "line", size = 1) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1) +
  labs(x = "Expertise Level", y = "Average Salary (USD)") +
  ggtitle("Average Salary for Data Engineers by Expertise Level (US)") +
  scale_color_manual(values = custom_palette[2]) +  # Use the second color from custom palette
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "gray", size = 0.2, linetype = "dashed"), 
        panel.grid.minor = element_line(color = "gray", size = 0.1, linetype = "dashed")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))  # Display in K terms

# Display the plot
print(p1b) 

# (Combined Chart)
p1c <- ggplot(filtered_data, aes(x = ExpertiseLevel, y = SalaryInUSD, color = JobTitle, group = JobTitle)) +
  stat_summary(fun = "mean", geom = "line", size = 1) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1) +
  labs(x = "Expertise Level", y = "Average Salary (USD)") +
  ggtitle("Average Salary for Data Scientists and Data Engineers by Expertise Level (US)") +
  scale_color_manual(values = custom_palette[c(1, 2)]) +  # Use the first two colors from custom palette
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank(), 
        panel.grid.major = element_line(color = "gray", size = 0.2, linetype = "dashed"), 
        panel.grid.minor = element_line(color = "gray", size = 0.1, linetype = "dashed")
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))  # Display in K terms

# Display the plot
print(p1c)



# Create the plot using ggplot and ggplotly
p2 <- ggplot(data, aes(x = ExperienceLevel, y = SalaryInUSD, color = ExperienceLevel)) +
  geom_point(alpha = 0.7) +
  labs(x = "Experience Level", y = "Salary (USD)", title = "Salary vs. Experience by Employment Type") +
  facet_wrap(~ EmploymentType, scales = "free") +
  theme_minimal() +
  scale_y_continuous(labels = comma_format(scale = 1e-3, suffix = "K")) 

p2_interactive <- ggplotly(p2)

# Display the plot
p2_interactive

# Question 3: "How does the concentration of large data science companies differ across continents, and are there any interesting regional patterns?"
# (Interactive Map)


# 1. Prepare data for mapping
data_for_map <- data %>%
  filter(CompanySize == "Large") %>% 
  group_by(CompanyLocation) %>%
  summarize(count = n())

# 2. Join with country map data
sPDF <- joinCountryData2Map(data_for_map, joinCode = "NAME", nameJoinColumn = "CompanyLocation")

# 3. Extract data for plotting
map_data <- data.frame(
  country = sPDF$NAME,
  count = sPDF$count,
  continent = sPDF$continent,
  long = coordinates(sPDF)[, 1],
  lat = coordinates(sPDF)[, 2]
)

# 4. Filter out countries with a count of zero or NA
map_data <- map_data %>%
  filter(count > 0)

# 5. Create the interactive map using plotly
plot_ly(data = map_data,
        type = 'choropleth',
        locations = ~country,
        locationmode = 'country names',
        z = ~count,
        text = ~paste(country, '<br>Count:', count),
        colorscale = 'Viridis',
        colorbar = list(title = 'Number of Large Companies'),
        hoverinfo = "text"
) %>%
  layout(
    title = 'Distribution of Large Data Science Companies by Continent',
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = 'natural earth')
    )
  ) %>%
  layout(facet = list(type = "wrap", facet_row = ~continent, nrow = 2))


#Question 4-Is there a trend for salaries to increase faster for "Senior" data job role compared to "Mid" or "Entry" level professionals over the years?

# Calculate average salary for each year and experience level
salary_trend_by_experience <- data %>%
  group_by(Year, ExperienceLevel) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE))

# Create an interactive line plot showing salary trends for each experience level
p <- ggplotly(
  ggplot(salary_trend_by_experience, aes(x = Year, y = AverageSalary, color = ExperienceLevel, group = ExperienceLevel)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(x = "Year", y = "Average Salary (USD)", title = "Salary Trends by Experience Level") +
    theme_minimal() 
)

print(p)



# Question 5: Impact of Residence on Salary, Considering Job Title and Experience
# (Scatter plot with faceting)

# Group by residence, job title, and experience level
residence_salary_data <- data %>%
  group_by(EmployeeResidence, JobTitle, ExperienceLevel) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE))

# Create the visualization (scatter plot with faceting)
p5 <- ggplot(residence_salary_data, aes(x = ExperienceLevel, y = AverageSalary, color = ExperienceLevel)) +
  geom_point() +
  labs(x = "Experience Level", y = "Average Salary (USD)", title = "Average Salary by Residence, Job Title, and Experience Level") +
  facet_grid(EmployeeResidence ~ JobTitle) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)

# Question 6: Comparison of Salaries in Large Companies Considering Job Title and Experience
# (Grouped Bar Chart)

# Group by company size, job title, and experience level
company_size_salary <- data %>%
  group_by(CompanySize, JobTitle, ExperienceLevel) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE)) %>%
  ungroup()

# Filter for large companies only
large_company_salary <- company_size_salary %>%
  filter(CompanySize == "Large")

# Create the visualization (grouped bar chart)
p6 <- ggplot(large_company_salary, aes(x = ExperienceLevel, y = AverageSalary, fill = ExperienceLevel)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ JobTitle, scales = "free") +
  labs(
    x = "Experience Level",
    y = "Average Salary in USD",
    title = "Average Salary by Job Title and Experience Level in Large Companies"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(p6)

# Question 7: How Has Company Size Distribution Changed Over Time?
# (Pie Chart and Bar Chart)

# Calculate proportions of company sizes for each year
data_proportions <- data %>%
  group_by(Year, CompanySize) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

# Bar chart of company size distribution over time
p7_bar <- ggplot(data_proportions, aes(x = Year, y = Count, fill = CompanySize)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Year",
    y = "Count",
    title = "Company Size Distribution Over Time"
  ) +
  theme_minimal()

print(p7_bar)

# Pie chart for a specific year (e.g., 2022)
specific_year <- 2022
data_specific_year <- data_proportions %>%
  filter(Year == specific_year)

p7_pie <- ggplot(data_specific_year, aes(x = "", y = Proportion, fill = CompanySize)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(
    x = NULL,
    y = NULL,
    title = paste("Company Size Distribution in", specific_year)
  ) +
  theme_void() +
  theme(legend.position = "right")

print(p7_pie)

# Question 8: How do salary expectations vary across different job roles in the data science field, and are there notable trends in this variation over time?
# (Interactive Plot)

# Calculate the average salary for each job role and year
salary_by_role_year <- data %>%
  group_by(Year, JobTitle) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE)) 

# Get the top 10 highest-paying job roles
top_roles <- salary_by_role_year %>%
  group_by(JobTitle) %>%
  summarize(MeanSalary = mean(AverageSalary)) %>%
  arrange(desc(MeanSalary)) %>%
  slice(1:10) %>%
  pull(JobTitle)

# Filter the salary data for the top 10 roles
salary_by_role_year_top <- salary_by_role_year %>%
  filter(JobTitle %in% top_roles)

# Create an interactive plot showing salary trends for the top 10 roles over time
p8 <- ggplotly(
  ggplot(salary_by_role_year_top, aes(x = Year, y = AverageSalary, color = JobTitle, group = JobTitle)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(x = "Year", y = "Average Salary (USD)", title = "Salary Trends for Top 10 Data Science Job Roles") +
    theme_minimal()
)

print(p8)



# Question 9: What is the relationship between company size and salary for different job roles based on the region the employee lives?

# Feature Engineering: Create Continent Column
data <- data %>%
  mutate(Continent = countrycode(EmployeeResidence, "country.name", "continent"))

# Filter data for Data Scientist, Data Analyst, and Data Engineer roles
filtered_data <- data %>%
  filter(JobTitle %in% c("Data Scientist", "Data Analyst", "Data Engineer"))

# Create an interactive scatter plot with company size and salary, faceted by employee continent
p9 <- ggplotly(
  ggplot(filtered_data, aes(x = CompanySize, y = SalaryInUSD, color = Continent)) +
    geom_point(alpha = 0.7) +
    labs(x = "Company Size", y = "Salary (USD)", title = "Company Size vs. Salary for Data Science Roles (By Employee Continent)") +
    facet_wrap(~ JobTitle, ncol = 3) +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))
)

print(p9)

# Question 10: How does the geographic distribution of data science professionals vary by job role, and are there any notable trends in this distribution over time?
# (Interactive Map with Faceting)

# Calculate the number of professionals in each location for each job role and year
location_by_role_year <- data %>%
  group_by(Year, JobTitle, CompanyLocation) %>%
  summarize(Count = n()) 

# Create an interactive map showing the geographic distribution of data science professionals, faceted by job role and year
p10 <- ggplotly(
  ggplot(location_by_role_year, aes(map_id = CompanyLocation, fill = Count)) +
    geom_map(map = map_data("world"), color = "white", size = 0.25) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
    labs(title = "Geographic Distribution of Data Science Professionals") +
    facet_grid(JobTitle ~ Year) +
    theme_minimal()
)

print(p10)
# Load necessary libraries
library(tidyverse)

# Assuming `data` is your dataset and it has columns `Year`, `JobTitle`, and `SalaryInUSD`

# Calculate the average salary for each job role and year
salary_by_role_year <- data %>%
  group_by(Year, JobTitle) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE))

# Get the top 10 highest-paying job roles
top_roles <- salary_by_role_year %>%
  group_by(JobTitle) %>%
  summarize(MeanSalary = mean(AverageSalary)) %>%
  arrange(desc(MeanSalary)) %>%
  slice(1:10) %>%
  pull(JobTitle)

# Filter the salary data for the top 10 roles
salary_by_role_year_top <- salary_by_role_year %>%
  filter(JobTitle %in% top_roles)

# Create a line plot with a nicer theme
p8 <- ggplot(salary_by_role_year_top, aes(x = Year, y = AverageSalary, color = JobTitle, group = JobTitle)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Average Salary (USD)", title = "Salary Trends for Top 10 Data Science Job Roles") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_blank(), # Remove legend title for simplicity
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) +
  # Add labels to the end of each line, colored to match the lines
  geom_text(
    data = salary_by_role_year_top %>% 
      group_by(JobTitle) %>%
      slice_tail(n = 1), 
    aes(label = JobTitle, color = JobTitle), 
    hjust = -0.1, vjust = 0.5, size = 4, show.legend = FALSE
  )

# Display the plot
print(p8)



# 1. Prepare data for mapping
data_for_map <- data %>%
  filter(CompanySize == "Large") %>% 
  group_by(CompanyLocation) %>%
  summarize(count = n())

# 2. Join with country map data
sPDF <- joinCountryData2Map(data_for_map, joinCode = "NAME", nameJoinColumn = "CompanyLocation")

# 3. Create the static map
ggplot() +
  geom_polygon(data = sPDF, aes(x = long, y = lat, group = group, fill = count), color = "gray", size = 0.2) +
  labs(title = "Distribution of Large Data Science Companies by Continent", fill = "Number of Companies") +
  theme_bw() +
  scale_fill_viridis_c(option = "magma", direction = -1) +  # Use a color scale for better visualization
  coord_map("ortho", orientation = c(90, 0, 0)) +  # Set map projection
  theme(
    panel.grid = element_blank(), 
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16) 
  )



# Load necessary libraries
library(tidyverse)
library(plotly)

# Assuming `data` is your dataset and it has columns `Year`, `JobTitle`, and `SalaryInUSD`

# Calculate the average salary for each job role and year
salary_by_role_year <- data %>%
  group_by(Year, JobTitle) %>%
  summarize(AverageSalary = mean(SalaryInUSD, na.rm = TRUE))

# Get the top 10 highest-paying job roles
top_roles <- salary_by_role_year %>%
  group_by(JobTitle) %>%
  summarize(MeanSalary = mean(AverageSalary)) %>%
  arrange(desc(MeanSalary)) %>%
  slice(1:10) %>%
  pull(JobTitle)

# Filter the salary data for the top 10 roles
salary_by_role_year_top <- salary_by_role_year %>%
  filter(JobTitle %in% top_roles)

# Create a line plot
p8 <- ggplot(salary_by_role_year_top, aes(x = Year, y = AverageSalary, color = JobTitle, group = JobTitle)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Average Salary (USD)", title = "Salary Trends for Top 10 Data Science Job Roles") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "right" # Include the legend
  ) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

# Convert ggplot to plotly
p8_plotly <- ggplotly(p8)

# Display the plotly plot
p8_plotly



#10.	Do certain regions with generally lower salaries offer higher salaries to "Expert" data analysit, implying that expertise can command a premium even in less lucrative markets?



# --- Analysis ---

# Filter for Expert Data Analysts
expert_analysts <- data %>%
  filter(JobTitle == "Data Analyst" & ExpertiseLevel == "Expert")

# Calculate average salary for Expert Data Analysts in each region
avg_salary_by_region <- expert_analysts %>%
  group_by(CompanyLocation) %>%
  summarize(avg_salary_usd = mean(SalaryInUSD))

# Sort by average salary in descending order
avg_salary_by_region <- avg_salary_by_region %>%
  arrange(desc(avg_salary_usd))

# --- Visualization --- 

# Get the world map data
world_map <- map_data("world")

# Create a map with country borders
map_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  coord_fixed(ratio = 1) +
  labs(title = "Average Salaries for Expert Data Analysts Worldwide", x = "Longitude", y = "Latitude")

# Join salary data to the world map data
world_map_salaries <- merge(world_map, avg_salary_by_region, by.x = "region", by.y = "CompanyLocation", all.x = TRUE)


# Create a map with salary data (using a different color palette)
salary_map <- map_plot +
  geom_polygon(data = world_map_salaries, aes(x = long, y = lat, group = group, fill = avg_salary_usd), color = "black") +
  scale_fill_gradientn(
    colors = c("#ffffb3", "#fec44f", "#fe9929", "#ec7014", "#cc4c02", "#8c2d04"), 
    name = "Average Salary (USD)", 
    limits = c(min(world_map_salaries$avg_salary_usd, na.rm = TRUE), max(world_map_salaries$avg_salary_usd, na.rm = TRUE))
  )

# Display the map
ggplotly(salary_map) 

# --- Interpretation ---

# Look for regions with lower average salaries overall but higher salaries for Expert Data Analysts.
# This could suggest that expertise is highly valued in those markets.
# Calculate average salary for ALL Data Analysts in each region
avg_salary_all_analysts <- data %>%
  filter(JobTitle == "Data Analyst") %>%
  group_by(CompanyLocation) %>%
  summarize(avg_salary_all_usd = mean(SalaryInUSD))
# Merge with the average salary of Expert Data Analysts
salary_comparison <- merge(avg_salary_all_analysts, avg_salary_by_region, 
                           by = "CompanyLocation", all.x = TRUE)
# Calculate the premium for Expert Data Analysts
salary_comparison <- salary_comparison %>%
  mutate(salary_premium = avg_salary_usd - avg_salary_all_usd)
# Find regions with a positive salary premium (Expert > Average)
regions_with_premium <- salary_comparison %>%
  filter(salary_premium > 0) %>%
  arrange(desc(salary_premium))

print(regions_with_premium)


# Scatter plot of salary comparison
ggplot(salary_comparison, aes(x = avg_salary_all_usd, y = avg_salary_usd)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = "Average Salary for All Data Analysts (USD)", 
       y = "Average Salary for Expert Data Analysts (USD)",
       title = "Salary Comparison: Expert vs. All Data Analysts") +
  theme_bw()
