
# Overview:
This analysis focuses on exploring smart mobility patterns based on demographic and smartphone usage data. The dataset used contains information about individuals' demographics, smartphone app usage frequency, and smart mobility adoption. The analysis is conducted using R programming language, incorporating libraries such as tidyverse, ggplot2, skimr, and DataExplorer for efficient data manipulation, visualization, and exploratory data analysis (EDA).

## Data Preparation:
Load Required Libraries:

library(readr)
library(tidyverse)
library(ggplot2)
library(skimr)
library(DataExplorer)
Import the Data:
MobilityBW_Data <- read_csv("MobilityBW_Data.csv")
Mobility <- as.data.frame(MobilityBW_Data)
Select Variables for Analysis:

Data <- Mobility %>% select(AGE, GENDER, STATE, Education, sp_telefon, sp_text, sp_web, sp_email, sp_foto, sp_snsa, sp_snsp, sp_kauf, sp_bank, sp_apps, sp_gps, sp_bluet, sp_spiel, sp_musikvideo, smartmobility)
Data Cleaning:

# Remove missing values
Data <- na.omit(Data)

# Recode 'GENDER' variable
Data$GENDER <- as.factor(recode(Data$GENDER, 
                      "m\xe4nnlich" = "Male",
                      "weiblich" = "Female",
                      "divers" = "Diverse"))

# Recode 'Education' variable
Data$Education <- as.numeric(recode(Data$Education ,
                         "2 - von der Schule abgegangen ohne Schulabschluss" = "2",
                         "3 - Hauptschulabschluss (Volksschulabschluss) oder gleichwertiger Abschluss" = "3",
                         "4 - Polytechnische Oberschule der DDR mit Abschluss der 8. oder 9. Klasse" = "4",
                         "5 - Realschulabschluss (Mittlere Reife) oder gleichwertiger Abschluss" = "5", 
                         "6 - Polytechnische Oberschule der DDR mit Abschluss der 10. Klasse" = "6",
                         "7 - Fachhochschulreife" = "7",
                         "8 - Abitur/Allgemeine oder fachgebundene Hochschulreife (Gymnasium bzw. EOS, auch EOS mit Lehre)" = "8",
                         "9 - anderen Schulabschluss, und zwar:"  = "9"))

# Create 'Mean_time_used_on_smartphone' variable
Data[, 5:18] <- lapply(
  Data[, 5:18], 
  function(x) {
    recode(x, "1 - mehrmals t\xe4glich" = "1", "2 - t\xe4glich"  = "2", "3 - mehrmals in der Woche"   = "3",
           "4 - mehrmals im Monat" = "4", "5 - einmal im Monat oder seltener" = "5", "6 - nie" = "6")
  }
)

# Convert the relevant columns to numeric 
Data[, 5:18] <- lapply(Data[,5:18], as.numeric)

# Create a new column 'Mean_time_used_on_smartphone'
Data$Mean_time_used_on_smartphone <- rowMeans(Data[, 5:18], na.rm = TRUE)

# Remove unnecessary columns
Data <- Data[, -(5:18)]

# Recode 'smartmobility' variable to 0 and 1
Data <- Data %>%
  mutate(smartmobility = ifelse(smartmobility == "ja", 1, 0))

Explanatory Data Analysis (EDA):
Overview of the Dataset:
str(Data)
summary(Data)
skim(Data)

Univariate Analysis:
# Distribution of Education Levels
ggplot(Data, aes(x = Education)) +
  geom_bar() +
  ggtitle("Distribution of Education Levels")

# Distribution of Smartphone Usage
ggplot(Data, aes(x = Mean_time_used_on_smartphone)) +
  geom_bar() +
  ggtitle("Distribution of Smartphone Usage")

# Bar Plot of Smart Mobility Use
ggplot(Data, aes(x = factor(smartmobility), fill = factor(smartmobility))) +
  geom_bar(alpha = 0.7, position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Bar Plot of Smart Mobility Use",
       x = "Smart Mobility Use", y = "Count") +
  theme_minimal()

Bivariate Analysis:
# Smart Mobility Use Across Education Levels
ggplot(Data, aes(x = Education, y = smartmobility)) +
  geom_bar(stat = "identity") +
  ggtitle("Smart Mobility Use Across Education Levels") +
  xlab("Education Levels") +
  ylab("Smart Mobility Use") 

Correlation Analysis:
# Correlation Matrix
corr_matrix <- cor(Data[, c("Education", "Mean_time_used_on_smartphone", "smartmobility")])
print("Correlation Matrix:")
print(corr_matrix)
plot_correlation(corr_matrix)

# Scatterplot Matrix
pairs(Data[, c("Education", "Mean_time_used_on_smartphone", "smartmobility")])

Summary Statistics:
summary_stats <- Data %>%
  group_by(GENDER) %>%
  summarise(mean_smart_mobility_use = mean(smartmobility),
            mean_usage_on_phone = mean(Mean_time_used_on_smartphone),
            mean_education_level = mean(Education))
print("Summary Statistics:")
print(summary_stats)

Regression Analysis:

# Logistic Regression Model
model <- glm(smartmobility ~ Education + Mean_time_used_on_smartphone, data = Data, family = "binomial")

# Check the summary of the model
summary(model)

Correlation Analysis (Pearson):
cor.test(Data$smartmobility, Data$Mean_time_used_on_smartphone)
cor.test(Data$smartmobility, Data$Education)


Feel free to explore, replicate, and contribute to the analysis. For any inquiries, contact [njorogeofrey73@gmail.com].
