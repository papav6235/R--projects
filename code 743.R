# Ανάλυση Προσδιοριστικών Παραγόντων Κερδοφορίας Τραπεζών

## Φόρτωση απαραίτητων βιβλιοθηκών
# Φόρτωση απαραίτητων βιβλιοθηκών
library(tidyverse)
library(psych)      # Για συναρτήσεις παραγοντικής ανάλυσης
library(ggplot2)    # Για οπτικοποίηση
library(readxl)     # Για ανάγνωση αρχείων Excel
library(plm)        # Για ανάλυση panel data
library(car)        # Για διαγνωστικά τεστ
library(lmtest)     # Για διαγνωστικά τεστ
library(sandwich)   # Για robust standard errors
library(corrplot)   # Για γραφήματα συσχετίσεων
library(stargazer)  # Για πίνακες αποτελεσμάτων παλινδρόμησης

## 1. Προετοιμασία και εξερεύνηση δεδομένων
# Εισαγωγή δεδομένων
data <- read_excel("datavol_encoded-final.xlsx")

# Εξέταση της δομής των δεδομένων
str(data)
head(data)
attach(data)
# Έλεγχος για τυχόν ελλιπείς τιμές
missing_values <- colSums(is.na(data))
print(missing_values[missing_values > 0])



# Υπολογισμός Χρηματοοικονομικών Δεικτών
data <- data %>%
  mutate(
    # Προσθήκη των μεταβλητών που αναφέρονται στην εξίσωση του paper
     # Μέγεθος τράπεζας (λογάριθμος περιουσιακών στοιχείων)
    CAP = eqtot / asset,  # Capital ratio
    LOAN = lnlsnet / asset,  # Loan ratio # Deposits to assets ratio
    LLP = lnatres/ lnlsnet  # Loan loss provisions to loans ratio
  )

# Μετονομασία όλων των μεταβλητών σε ΚΕΦΑΛΑΙΑ
names(data) <- toupper(names(data))


## 2. Περιγραφική Στατιστική Ανάλυση
# Επιλογή των μεταβλητών ενδιαφέροντος για το μοντέλο
model_vars <- c("ROA", "NIM", "SIZE", "CAP", "LOAN", "DEP", "LLP")

# Αναλυτικά περιγραφικά στατιστικά
desc_stats <- describe(data[model_vars])
print(desc_stats)

# Δημιουργία πίνακα περιγραφικών στατιστικών
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

# Συσχετίσεις μεταξύ των μεταβλητών
cor_matrix <- cor(data[model_vars], use = "pairwise.complete.obs")
print(cor_matrix)

# Οπτικοποίηση του πίνακα συσχετίσεων
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7)


## 3. Διαχρονική Ανάλυση Δεικτών Κερδοφορίας
# Μέσοι όροι δεικτών κερδοφορίας ανά έτος
yearly_profitability <- data %>%
  group_by(YEAR) %>%
  summarise(
    Avg_ROA = mean(ROA, na.rm = TRUE),
    Avg_NIM = mean(NIM, na.rm = TRUE)
  )

# Μετατροπή σε μορφή "long" για ευκολότερη οπτικοποίηση
yearly_profitability_long <- yearly_profitability %>%
  pivot_longer(
    cols = starts_with("Avg_"),
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  mutate(Indicator = gsub("Avg_", "", Indicator))

# Γράφημα δεικτών κερδοφορίας
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

# Μέσοι όροι επεξηγηματικών μεταβλητών ανά έτος
yearly_determinants <- data %>%
  group_by(YEAR) %>%
  summarise(
    Avg_SIZE = mean(SIZE, na.rm = TRUE),
    Avg_CAP = mean(CAP, na.rm = TRUE),
    Avg_LOAN = mean(LOAN, na.rm = TRUE),
    Avg_DEP = mean(DEP, na.rm = TRUE),
    Avg_LLP = mean(LLP, na.rm = TRUE)
  )

# Μετατροπή σε μορφή "long" για ευκολότερη οπτικοποίηση
yearly_determinants_long <- yearly_determinants %>%
  pivot_longer(
    cols = starts_with("Avg_"),
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  mutate(Indicator = gsub("Avg_", "", Indicator))

# Γράφημα επεξηγηματικών μεταβλητών
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

## 4. Προκαταρκτική Ανάλυση με Pooled OLS
# 1. Έλεγχος πολυσυγγραμμικότητας με VIF (Variance Inflation Factor)
library(car)

# Έλεγχος VIF για το μοντέλο ROA
vif_roa <- vif(lm(ROA ~ SIZE + CAP + LOAN + DEP +LLP , data = data))
print("VIF για μοντέλο ROA:")
print(vif_roa)

# Έλεγχος VIF για το μοντέλο NIM
vif_nim <- vif(lm(NIM ~ SIZE + CAP + LOAN + DEP +LLP , data = data))
print("VIF για μοντέλο NIM:")
print(vif_nim)

# 2. Πίνακας συσχετίσεων για να δούμε ποιες μεταβλητές συσχετίζονται έντονα
correlation_matrix <- cor(data[, c("SIZE","LLP", "CAP", "LOAN", "DEP", "ROA", "NIM")], 
                          use = "complete.obs")
print("Πίνακας συσχετίσεων:")
print(round(correlation_matrix, 3))

# 3. Οπτικοποίηση του πίνακα συσχετίσεων
library(corrplot)
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45) 


################################3
# 4. Προκαταρκτική Ανάλυση με Pooled OLS (Αντιμετωπίζοντας την Πολυσυγγραμμικότητα)

# Η μεταβλητή LLP έχει το υψηλότερο VIF (4.70) και υψηλή συσχέτιση με DEP και NIM
# Την αφαιρούμε πρώτα από τα μοντέλα μας
# Pooled OLS για ROA 
roa_pooled <- lm(ROA ~ SIZE + CAP + LOAN + DEP+ LLP, data = data)
summary(roa_pooled)

# Pooled OLS για NIM 
nim_pooled <- lm(NIM ~ SIZE + CAP + LOAN + DEP+ LLP, data = data)
summary(nim_pooled)

# Έλεγχος νέων VIF για να δούμε αν επιλύθηκε το πρόβλημα
vif_roa_new <- vif(roa_pooled)
print("VIF για το μοντέλο ROA :")
print(vif_roa_new)

vif_nim_new <- vif(nim_pooled)
print("VIF για το μοντέλο NIM :")
print(vif_nim_new)

# Έλεγχος ετεροσκεδαστικότητας
bptest(roa_pooled)
bptest(nim_pooled)

# Διορθωμένα τυπικά σφάλματα με τη μέθοδο του White
coeftest(roa_pooled, vcov = vcovHC(roa_pooled, type = "HC1"))
coeftest(nim_pooled, vcov = vcovHC(nim_pooled, type = "HC1"))

## 5. Ανάλυση Panel Data

# Βεβαιωθείτε ότι το NAME είναι σε κεφαλαία
data$NAME <- toupper(data$NAME)

# Κρατήστε μόνο μοναδικούς συνδυασμούς NAME + YEAR
data <- data %>% distinct(NAME, YEAR, .keep_all = TRUE)

# Δημιουργία του panel dataset (το NAME είναι η μονάδα, το YEAR ο χρόνος)
pdata <- pdata.frame(data, index = c("NAME", "YEAR"))

# 5.1 Panel Models για ROA 


cor(pdata[, c("SIZE", "CAP", "LOAN","LLP", "DEP")]) 

# Random Effects Model για ROA
roa_re <- plm(ROA ~ SIZE + CAP + LOAN+ LLP , 
              data = pdata, model = "random")
summary(roa_re)
roa_fe <- plm(ROA ~ SIZE + CAP + LOAN + LLP , 
              data = pdata, model = "within")
summary(roa_fe)
# Έλεγχος Hausman για επιλογή μεταξύ fixed και random effects
hausman_test_roa <- phtest(roa_fe, roa_re)
print(hausman_test_roa)

# 5.2 Panel Models για NIM (χωρίς LLP)

# Fixed Effects Model για NIM
nim_fe <- plm(NIM ~ SIZE + CAP + LOAN + LLP  , 
              data = pdata, model = "within")
summary(nim_fe)

# Random Effects Model για NIM
nim_re <- plm(NIM ~ SIZE + CAP + LOAN + LLP ,  
              data = pdata, model = "random")
summary(nim_re)

# Έλεγχος Hausman για επιλογή μεταξύ fixed και random effects
hausman_test_nim <- phtest(nim_fe, nim_re)
print(hausman_test_nim)

# Έλεγχος για ετεροσκεδαστικότητα σε panel models
bptest(roa_fe)
bptest(nim_fe)
###################################################
# Υπολογισμός ROE (Return on Equity) χρησιμοποιώντας ibefxtr ως proxy για Net Income
#Δείκτης ROE (Return on Equity):Ο δείκτης απόδοσης ιδίων κεφαλαίων υπολογίστηκε ως το πηλίκο των λειτουργικών κερδών προ φόρων και έκτακτων στοιχείων προς το συνολικό ύψος ιδίων κεφαλαίων. Συγκεκριμένα, λόγω έλλειψης διαθέσιμης μεταβλητής που να αντιπροσωπεύει καθαρά κέρδη (Net Income), χρησιμοποιήθηκε η μεταβλητή IBEFXTR (income before extraordinary items and taxes) ως αποδεκτό proxy για τα καθαρά κέρδη. Ο παρονομαστής του δείκτη ήταν η μεταβλητή EQTOT (total equity capital), όπως αυτή περιγράφεται στα δεδομένα της FDIC.#



length(ibefxtr)
length(eqtot)
nrow(data)

data$ROE <- data$IBEFXTR / data$EQTOT 
# Ενημέρωση του pdata με τη νέα μεταβλητή ROE
pdata <- pdata.frame(data, index = c("NAME", "YEAR"))

print(data$ROE)
summary(data$ROE) 
# Random Effects Model για ROA 
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
  labs(title = "Διαχρονική Εξέλιξη του ROE",
       x = "Έτος", y = "Μέσο ROE")
ggplot(data, aes(x = as.factor(YEAR), y = ROE)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Boxplot του ROE ανά Έτος",
       x = "Έτος", y = "ROE")
##########################NEW############################################3
library(dplyr)

# Υπολογισμός Equity/Assets
df <- data %>%
  mutate(eq_to_asset = EQTOT / ASSET)

# Υπολογισμός std(ROA) ανά τράπεζα (χρησιμοποιώντας NAME)
roa_sd <- df %>%
  group_by(NAME) %>%
  summarise(roa_sd = sd(ROA, na.rm = TRUE)) %>%
  ungroup()

# Συγχώνευση στο αρχικό dataset
df <- df %>%
  left_join(roa_sd, by = "NAME")
df <- df %>%
  mutate(
    ROA = as.numeric(ROA),
    eq_to_asset = as.numeric(eq_to_asset),
    roa_sd = as.numeric(roa_sd.x),
    z_score = (ROA + eq_to_asset) / roa_sd
  )

# Τελικός υπολογισμός Z-score
df <- df %>%
  mutate(z_score = (ROA + eq_to_asset) / roa_sd)

# Προβολή αποτελεσμάτων
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
