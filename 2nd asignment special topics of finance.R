library(dplyr)
library(readxl) 
library(ggplot2) 
library(viridis)
library(fastDummies)
file_path <- "C:/Users/papav/OneDrive/Υπολογιστής/assingment2 special topics of finance/Monthly_Credit_Ratings_With_Outlook_CleanDate.xlsx"
data <- read_excel(file_path)
data <- data %>% select(Date,Country,Yield_10y, Credit.Ratings, Outlook, `CDS 5yrs Spread`)
filtered_data <- dummy_cols(data, select_columns = "Outlook", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# Φίλτρο για την Ελλάδα
filtered_data <- subset(filtered_data, Country == "Greece" )
# Βήμα 1: Μετατρέπουμε τη Date σε μορφή ημερομηνίας (αν δεν είναι ήδη)
filtered_data$Date <- as.Date(filtered_data$Date, format = "%Y-%m-%d")

ggplot(filtered_data, aes(x = Date, y = Yield_10y)) +
  geom_point(color = "black") +         # Σημεία
  geom_line(color = "orange") +           # Γραμμή σύνδεσης
  labs(title = "10-Year Bond Yield Over Time (Greece)",
       x = "Date", y = "Yield 10y") +
  theme_minimal()

summary(filtered_data %>%
          select(`CDS 5yrs Spread`,Yield_10y ))

ggplot(filtered_data, aes(x = factor(Credit.Ratings))) +
  geom_bar(fill = "orange") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Frequency of Credit Ratings",
       x = "Credit Rating",
       y = "Count") +
  theme_minimal()


colSums(filtered_data %>%
          select(`Outlook_negative watch`, 
                 Outlook_positive, 
                 `Outlook_under review`))

model <- lm(Yield_10y ~ Credit.Ratings + `CDS 5yrs Spread` + 
              `Outlook_negative watch` + Outlook_positive, 
            data = filtered_data)

summary(model)
filtered_data$predicted <- predict(model)


ggplot(filtered_data, aes(x = Date)) +
  geom_point(aes(y = Yield_10y, color = "Actual"), size = 1) +
  geom_line(aes(y = predicted, color = "Predicted"), size = 1) +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "orange")) +
  labs(title = "Actual vs Predicted 10-Year Yield Over Time (Greece)",
       x = "Date", y = "Yield (10y)", color = "Series") +
  theme_minimal()
