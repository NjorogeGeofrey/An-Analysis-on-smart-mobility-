#Load required libraries
library(readr)
library(tidyverse)
library(ggplot2)
library(skimr)
library(DataExplorer)
#Import the data
MobilityBW_Data <- read_csv("MobilityBW_Data.csv")
#Check the class of the data
class(Mobility)
#Convert it to dataframe
Mobility <- as.data.frame(Mobility)

#For this analysis we shall select the foolowing variables.
Data <- Mobility %>% select(AGE, GENDER, STATE, Education, sp_telefon, sp_text, sp_web, sp_email, sp_foto, sp_snsa, sp_snsp, sp_kauf, sp_bank, sp_apps, sp_gps, sp_bluet, sp_spiel, sp_musikvideo, smartmobility)
head(Data)
str(Data)
#Data cleaning
#First all missing values
Data <- na.omit(Data)
#Naming gender in to male and females and diverse and making it a factor data type with 3 levels
Data$GENDER <- as.factor(Data$GENDER)
levels(Data$GENDER)
Data$GENDER <- recode(Data$GENDER, 
                      "m\xe4nnlich" = "Male",
                      "weiblich" = "Female",
                      "divers" = "Diverse")
View(Data)

#Now we do the same for education
Data$Education <- recode(Data$Education ,
                         "2 - von der Schule abgegangen ohne Schulabschluss" = "2",
                         "3 - Hauptschulabschluss (Volksschulabschluss) oder gleichwertiger Abschluss" = "3",
                         "4 - Polytechnische Oberschule der DDR mit Abschluss der 8. oder 9. Klasse" = "4",
                         "5 - Realschulabschluss (Mittlere Reife) oder gleichwertiger Abschluss" = "5", 
                         "6 - Polytechnische Oberschule der DDR mit Abschluss der 10. Klasse" = "6",
                         "7 - Fachhochschulreife" = "7",
                         "8 - Abitur/Allgemeine oder fachgebundene Hochschulreife (Gymnasium bzw. EOS, auch EOS mit Lehre)" = "8",
                         "9 - anderen Schulabschluss, und zwar:"  = "9")

Data$Education <- as.numeric(Data$Education)

#Now we create the variable (frequency_of_time_used_on_smartphone) this will contain the row avarage fo the time spent on the smart phone on different apps

# Recode the observations in the six columns
Data[, 5:18] <- lapply(
  Data[, 5:18], 
  function(x) {
    recode(x, "1 - mehrmals t\xe4glich" = "1", "2 - t\xe4glich"  = "2", "3 - mehrmals in der Woche"   = "3",
           "4 - mehrmals im Monat" = "4", "5 - einmal im Monat oder seltener" = "5", "6 - nie" = "6")
  }
)


# Convert the relevant columns to numeric 
Data[, 5:18] <- 
  lapply(Data[,5:18], as.numeric)

# Create a new column 'Mean_of_time_used_on_smartphone'
Data$Mean_time_used_on_smartphone <- rowMeans(Data[, 5:18], na.rm = TRUE)

# Display the updated dataset
head(Data)
#Now we can remove the variables which have obeservatyions on how much time a person spends on each app in a smartphone since we have the mean amount spent on a smart phone
Data <- Data[, -(5:18)]
#Next we convert the smart mobility question into 0 and 1 factor since we shall use logistic regression.
# Recode smart mobility variable to 0 and 1
Data <- Data %>%
  mutate(smartmobility = ifelse(smartmobility == "ja", 1, 0))

# Display the updated dataset
head(Data)
#Now the data is ready for analysis.


#Explanatory Data analysis.
# Overview of the dataset
str(Data)
summary(Data)
skim(Data)

# Visualizations
# Univariate Analysis
ggplot(Data, aes(x = Education)) +
  geom_bar() +
  ggtitle("Distribution of Education Levels")

ggplot(Data, aes(x = Mean_time_used_on_smartphone)) +
  geom_bar() +
  ggtitle("Distribution of Smartphone Usage")

# Create a bar plot
ggplot(Data, aes(x = factor(smartmobility), fill = factor(smartmobility))) +
  geom_bar(alpha = 0.7, position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Bar Plot of Smart Mobility Use",
       x = "Smart Mobility Use", y = "Count") +
  theme_minimal()


# Bivariate Analysis
ggplot(Data, aes(x = Education, y = smartmobility)) +
  geom_bar(stat = "identity") +
  ggtitle("Smart Mobility Use Across Education Levels") +
  xlab("Education Levels") +
  ylab("Smart Mobility Use") 

# Correlation Matrix

corr_matrix <- cor(Data[, c("Education", "Mean_time_used_on_smartphone", "smartmobility")])
print("Correlation Matrix:")
print(corr_matrix)
plot_correlation(corr_matrix)
# Scatterplot Matrix
pairs(Data[, c("Education", "Mean_time_used_on_smartphone", "smartmobility")])


# Summary Statistics
summary_stats <- Data %>%
  group_by(GENDER) %>%
  summarise(mean_smart_mobility_use = mean(smartmobility),
            mean_usage_on_phone = mean(Mean_time_used_on_smartphone),
            mean_education_level = mean(Education))
print("Summary Statistics:")
print(summary_stats)


#Regression Analysis
model <- glm(smartmobility ~ Education + Mean_time_used_on_smartphone, data = Data, family = "binomial")

# Check the summary of the model
summary(model)

#Correlation Analysis(Pearson)

cor.test(Data$smartmobility, Data$Mean_time_used_on_smartphone)
cor.test(Data$smartmobility, Data$Education)





