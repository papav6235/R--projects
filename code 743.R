# analysis for banking profit

library(tidyverse)
library(psych)      
library(ggplot2)    
library(readxl)    
library(plm)        
library(car)        
library(lmtest)     
library(sandwich)   
library(corrplot)   
library(stargazer)  

data <- read_excel("x.xlsx")

# optimize data
str(data)
head(data)
attach(data)
# chech for n/a or missing values
missing_values <- colSums(is.na(data))
print(missing_values[missing_values > 0])



# finanacial ratios
data <- data %>%
  mutate(
    CAP = eqtot / asset,  # Capital ratio
    LOAN = lnlsnet / asset,  # Loan ratio # Deposits to assets ratio
    LLP = lnatres/ lnlsnet  # Loan loss provisions to loans ratio
  )

# rename variables with capitals
names(data) <- toupper(names(data))



# var from analysis with these variables
model_vars <- c("ROA", "NIM", "SIZE", "CAP", "LOAN", "DEP", "LLP")

# description statistics
desc_stats <- describe(data[model_vars])
print(desc_stats)

# table from stats
desc_table <- data.frame(
  Variable = rownames(desc_stats),
  Mean = desc_stats$mean,
  Median = desc_stats$median,
  SD = desc_stats$sd,
  Min = desc_stats$min,
  Max = desc_stats$max,
  N = desc_stats$n
)

print(desc_table)

# corellation matrix
cor_matrix <- cor(data[model_vars], use = "pairwise.complete.obs")
print(cor_matrix)

# corr plot
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7)


## 3. mead values roa, nim
yearly_profitability <- data %>%
  group_by(YEAR) %>%
  summarise(
    Avg_ROA = mean(ROA, na.rm = TRUE),
    Avg_NIM = mean(NIM, na.rm = TRUE)
  )

# log profitability
yearly_profitability_long <- yearly_profitability %>%
  pivot_longer(
    cols = starts_with("Avg_"),
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  mutate(Indicator = gsub("Avg_", "", Indicator))

# plot of profitabilities indexes
ggplot(yearly_profitability_long, aes(x = YEAR, y = Value, color = Indicator, group = Indicator)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Indicator, scales = "free_y") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Διαχρονική Τάση Δεικτών Κερδοφορίας",
       x = "Έτος", y = "Τιμή Δείκτη") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# means other variables
yearly_determinants <- data %>%
  group_by(YEAR) %>%
  summarise(
    Avg_SIZE = mean(SIZE, na.rm = TRUE),
    Avg_CAP = mean(CAP, na.rm = TRUE),
    Avg_LOAN = mean(LOAN, na.rm = TRUE),
    Avg_DEP = mean(DEP, na.rm = TRUE),
    Avg_LLP = mean(LLP, na.rm = TRUE)
  )

# "long" 
yearly_determinants_long <- yearly_determinants %>%
  pivot_longer(
    cols = starts_with("Avg_"),
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  mutate(Indicator = gsub("Avg_", "", Indicator))

# plot 
ggplot(yearly_determinants_long, aes(x = YEAR, y = Value, color = Indicator, group = Indicator)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Indicator, scales = "free_y") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Διαχρονική Τάση Επεξηγηματικών Μεταβλητών",
       x = "Έτος", y = "Τιμή") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 1.  VIF (Variance Inflation Factor)
library(car)

vif_roa <- vif(lm(ROA ~ SIZE + CAP + LOAN + DEP +LLP , data = data))
print("VIF model for ROA:")
print(vif_roa)

vif_nim <- vif(lm(NIM ~ SIZE + CAP + LOAN + DEP +LLP , data = data))
print("VIF model for NIM:")
print(vif_nim)

# 2. corr matrix
correlation_matrix <- cor(data[, c("SIZE","LLP", "CAP", "LOAN", "DEP", "ROA", "NIM")], 
                          use = "complete.obs")
print("Πίνακας συσχετίσεων:")
print(round(correlation_matrix, 3))

# 3. corr plot
library(corrplot)
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45) 


################################3
# 4.  Pooled OLS roa
roa_pooled <- lm(ROA ~ SIZE + CAP + LOAN + DEP+ LLP, data = data)
summary(roa_pooled)

# Pooled OLS NIM 
nim_pooled <- lm(NIM ~ SIZE + CAP + LOAN + DEP+ LLP, data = data)
summary(nim_pooled)

# checking vif
vif_roa_new <- vif(roa_pooled)
print("VIF model for ROA :")
print(vif_roa_new)

vif_nim_new <- vif(nim_pooled)
print("VIF model for NIM :")
print(vif_nim_new)

# heteroscedasticity
bptest(roa_pooled)
bptest(nim_pooled)

#  White methid
coeftest(roa_pooled, vcov = vcovHC(roa_pooled, type = "HC1"))
coeftest(nim_pooled, vcov = vcovHC(nim_pooled, type = "HC1"))

## 5. analysis Panel Data

data$NAME <- toupper(data$NAME)

# keep NAME + YEAR
data <- data %>% distinct(NAME, YEAR, .keep_all = TRUE)

# make panel dataset
pdata <- pdata.frame(data, index = c("NAME", "YEAR"))

# 5.1 Panel Models for ROA 


cor(pdata[, c("SIZE", "CAP", "LOAN","LLP", "DEP")]) 

# Random Effects Model για ROA
roa_re <- plm(ROA ~ SIZE + CAP + LOAN+ LLP , 
              data = pdata, model = "random")
summary(roa_re)
roa_fe <- plm(ROA ~ SIZE + CAP + LOAN + LLP , 
              data = pdata, model = "within")
summary(roa_fe)
# Έλεγχος Hausman for fixed and random effects
hausman_test_roa <- phtest(roa_fe, roa_re)
print(hausman_test_roa)

# 5.2 Panel Models for NIM (without LLP)

# Fixed Effects Model for NIM
nim_fe <- plm(NIM ~ SIZE + CAP + LOAN + LLP  , 
              data = pdata, model = "within")
summary(nim_fe)

# Random Effects Model for NIM
nim_re <- plm(NIM ~ SIZE + CAP + LOAN + LLP ,  
              data = pdata, model = "random")
summary(nim_re)

#  Hausman between fixed and random effects
hausman_test_nim <- phtest(nim_fe, nim_re)
print(hausman_test_nim)

# heteroscedasticity
bptest(roa_fe)
bptest(nim_fe)
###################################################
#make roe index

length(ibefxtr)
length(eqtot)
nrow(data)

data$ROE <- data$IBEFXTR / data$EQTOT 
# add to panel data  ROE
pdata <- pdata.frame(data, index = c("NAME", "YEAR"))

print(data$ROE)
summary(data$ROE) 
# Random Effects Model for ROA 
cor(pdata[, c("ROE", "SIZE", "CAP", "LOAN", "LLP", "DEP")], use = "complete.obs")
vif(lm(ROE ~ SIZE + CAP + LOAN + LLP + DEP, data = data))
roe_re <- plm(ROE ~ SIZE + CAP + LOAN + LLP , data = pdata, model = "random")
roe_fe <- plm(ROE ~ SIZE + CAP + LOAN + LLP , data = pdata, model = "within")
summary(roe_fe) 
summary(roe_re) 
hausman_test_roe <- phtest(roe_fe, roe_re)
print(hausman_test_roe) 

yearly_roe <- data %>%
  group_by(YEAR) %>%
  summarise(Avg_ROE = mean(ROE, na.rm = TRUE))

ggplot(yearly_roe, aes(x = YEAR, y = Avg_ROE)) +
  geom_line(linewidth = 1, color = "darkred") +
  geom_point(size = 2, color = "darkred") +
  theme_minimal() +
  labs(title = "yearly ROE",
       x = "Έτος", y = "Μέσο ROE")
ggplot(data, aes(x = as.factor(YEAR), y = ROE)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Boxplot  ROE every year",
       x = "Έτος", y = "ROE")
##########################NEW############################################3
library(dplyr)

#  Equity/Assets
df <- data %>%
  mutate(eq_to_asset = EQTOT / ASSET)

#  std(ROA) for every bank
roa_sd <- df %>%
  group_by(NAME) %>%
  summarise(roa_sd = sd(ROA, na.rm = TRUE)) %>%
  ungroup()

# put alls in first dataset
df <- df %>%
  left_join(roa_sd, by = "NAME")
df <- df %>%
  mutate(
    ROA = as.numeric(ROA),
    eq_to_asset = as.numeric(eq_to_asset),
    roa_sd = as.numeric(roa_sd.x),
    z_score = (ROA + eq_to_asset) / roa_sd
  )

# calculate Z-score
df <- df %>%
  mutate(z_score = (ROA + eq_to_asset) / roa_sd)

# results
head(df %>% select(NAME, YEAR, ROA, eq_to_asset, roa_sd, z_score))
library(plm)

df <- df %>%
  mutate(
    log_asset = log(ASSET),
    loan_to_deposit = LNLSNET / DEP,
    npl_ratio = LNRECONS / LNLSNET
  )


pdata <- pdata.frame(df, index = c("NAME", "YEAR"))

model <- plm(
  z_score ~ log_asset + eq_to_asset + loan_to_deposit + npl_ratio,
  data = pdata,
  model = "within"
)

summary(model)
model_random <- plm(
  z_score ~ log_asset + eq_to_asset + loan_to_deposit + npl_ratio,
  data = pdata,
  model = "random"
)

summary(model_random)

# Hausman test
phtest(model, model_random)
