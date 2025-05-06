#Charger les données avec chemin relatif
df_macro <- readXL("Data_MacroFactor.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Data", stringsAsFactors=TRUE)
df_period <- readXL("Data_Period.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Sheet1", stringsAsFactors=TRUE)

#Joindre les 2 datasets avec la date
df <- merge(df_period, df_macro, by = "Date", all = TRUE)


#Gérer les cellules vide (N/A)
#supprimer les lignes pour Date, Cycle_day ou Period
df <- df %>% filter(!is.na(Cycle_day) & !is.na(Period) & !is.na(Date))

#utiliser la dernière valeur connue
library(zoo)
library(dplyr)
df <- df %>%
  mutate(
    Metabolism = na.locf(Metabolism, na.rm = FALSE),
    Weight = na.locf(Weight, na.rm = FALSE),
    Calories = na.locf(Calories, na.rm = FALSE),
    Protein = na.locf(Protein, na.rm = FALSE),
    Fat = na.locf(Fat, na.rm = FALSE),
    Carbs = na.locf(Carbs, na.rm = FALSE),
    Target_Calories = na.locf(Target_Calories, na.rm = FALSE),
    Target_Protein = na.locf(Target_Protein, na.rm = FALSE),
    Target_Fat = na.locf(Target_Fat, na.rm = FALSE),
    Target_Carbs = na.locf(Target_Carbs, na.rm = FALSE),
    Sodium = na.locf(Sodium, na.rm = FALSE),
    Goal = na.locf(Goal, na.rm = FALSE)
  )
#si c'est la première valeur qu'il manque, utiliser la première valeur connue
df <- df %>%
  mutate(
    Metabolism = na.locf(Metabolism, fromLast = TRUE, na.rm = FALSE),
    Weight = na.locf(Weight, fromLast = TRUE, na.rm = FALSE),
    Calories = na.locf(Calories, fromLast = TRUE, na.rm = FALSE),
    Protein = na.locf(Protein, fromLast = TRUE, na.rm = FALSE),
    Fat = na.locf(Fat, fromLast = TRUE, na.rm = FALSE),
    Carbs = na.locf(Carbs, fromLast = TRUE, na.rm = FALSE),
    Target_Calories = na.locf(Target_Calories, fromLast = TRUE, na.rm = FALSE),
    Target_Protein = na.locf(Target_Protein, fromLast = TRUE, na.rm = FALSE),
    Target_Fat = na.locf(Target_Fat, fromLast = TRUE, na.rm = FALSE),
    Target_Carbs = na.locf(Target_Carbs, fromLast = TRUE, na.rm = FALSE),
    Sodium = na.locf(Sodium, fromLast = TRUE, na.rm = FALSE),
    Goal = na.locf(Goal, fromLast = TRUE, na.rm = FALSE)
  )


#Assigner la phase du cycle
df <- df %>%
  mutate(Cycle_Phase = case_when(
    Period == "Y" ~ "Menstrual",      
    Cycle_day >= 12 & Cycle_day <= 16 ~ "Ovulation", 
    Cycle_day >= 17 & Cycle_day <= 23 ~ "Luteal",
    Cycle_day >= 24 & Cycle_day <= 28 ~ "Pre-Menstrual",
    Cycle_day >= 29 ~ "Long Cycle",       
    TRUE ~ "Follicular"               
  ))


#Créer les nouvelles colonnes
#Création Variation quotidienne du poids (Scale Weight)
    # Convert factors to numeric
    df$Weight <- as.numeric(as.character(df$Weight))
df$Weight_Diff <- df$Weight - lag(df$Weight, 1)

#Création Variation calorique quotidienne (surplus/déficit)
    # Convert factors to numeric
    df$Calories <- as.numeric(as.character(df$Calories))
    df$Metabolism <- as.numeric(as.character(df$Metabolism))
df$Caloric_Balance <- df$Calories - df$Metabolism  # Positive = surplus, Negative = deficit

#Création Tendance du poids (7 days smoothing average)
library(zoo)
df$Weight_Trend <- rollmean(df$Weight, 7, fill = NA, align = "right")

# Calculer la valeur théorique de la variance du poids en livres
df$Theo_Weight_Diff <- df$Caloric_Balance / 3500

# Calculer la différence entre la variation actuelle et théorique
df$WeightDiff_Analysis <- df$Weight_Diff - df$Theo_Weight_Diff


#Graphique 1
#Variation de poids vs les phase du cycle vs tendance
library(ggplot2)
ggplot(df, aes(x = Date, y = Weight)) +
  geom_point(aes(color = Cycle_Phase), alpha = 0.6)+
  geom_line(aes(y = Weight_Trend), color = "blue", size = 1)+
  labs(title = "Variation de poids dans le temps")


#Graphique 2
#Création d'une colonne pour indiquer si la variation de poids est positive ou négative
df$WeightDiff_Sign <- ifelse(df$WeightDiff_Analysis > 0, "Positive", "Negative")

#Compter le nombre de positifs pour chaque jour du cycle
graph2_data <- df %>%
  group_by(Cycle_day, WeightDiff_Sign) %>%
  summarise(count = n())

#Créer le graphique en barre
ggplot(graph2_data, aes(x = Cycle_day, y = count, fill = WeightDiff_Sign)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() +
  labs(title = "Nombre de jours dont la variation de poids est plus importante que ce qui serait attendu (Positif) vs\nle nombre de jours dont la variation de poids est normale (Négatif)", 
       x = "Jour du cycle", y = "Compte de jours") +
  scale_fill_manual(values = c("Positive" = "green", "Negative" = "red")) 


#Compter les jours avec une variation positive
top_10_positive_days <- df %>%
  group_by(Cycle_day, Cycle_Phase) %>%
  summarise(Positive_Count = sum(WeightDiff_Analysis > 0, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Positive_Count)) %>%
  head(10)  # Select top 10
print(top_10_positive_days)


#ANOVA
anova <- aov(WeightDiff_Analysis ~ Cycle_day, data = df)
summary(anova)


#T-Tests
#Calculer le pourcentage de jours positifs pour chaque jour du cycle
    positive_percentage <- df %>%
      group_by(Cycle_day, Cycle_Phase) %>%
      summarise(
        Total_Days = n(),
        Positive_Days = sum(WeightDiff_Analysis > 0, na.rm = TRUE),
        Positive_Percentage = (Positive_Days / Total_Days) * 100,
        .groups = 'drop'
      )
    print(positive_percentage, n = Inf)

#ANOVA_2
anova_phase <- aov(Positive_Percentage ~ Cycle_Phase, data = positive_percentage)
    summary(anova_phase)  

#Définir les comparaisons
library(broom)
library(tidyr)
library(purrr)  
phase_comparisons <- list(
  c("Luteal", "Ovulation"),
  c("Luteal", "Menstrual"),
  c("Luteal", "Pre-Menstrual"),
  c("Follicular", "Ovulation"),
  c("Follicular", "Menstrual"),
  c("Follicular", "Pre-Menstrual"),
  c("Menstrual", "Ovulation"),
  c("Menstrual", "Pre-Menstrual")
)

#Faire tous les T-Tests
t_test_results <- phase_comparisons %>%
  map_df(~ {
    test <- t.test(Positive_Percentage ~ Cycle_Phase, 
                   data = positive_percentage %>% filter(Cycle_Phase %in% .x))
    
    tibble(
      Phase_1 = .x[1],
      Phase_2 = .x[2],
      t_value = test$statistic,
      p_value = test$p.value,
      mean_1 = test$estimate[1],
      mean_2 = test$estimate[2]
    )
  })
print(t_test_results)


#Création de nouvelles colonnes
#Création du ratio Sodium-to-Calorie
df$Sodium <- as.numeric(as.character(df$Sodium))
df$Sodium_Calorie_Ratio <- df$Sodium / df$Calories

# Créer une colonne de différence de poids pour le jour suivant
df <- df %>%
  arrange(Date) %>%
  mutate(WeightDiff_Analysis_next_day = lead(WeightDiff_Analysis, order_by = Date))


#Graphique 3
#Visualisation de la corrélation Sodium-to-Calorie Ratio et WeightDiff_Analysis du jour suivant
ggplot(df, aes(x = Sodium_Calorie_Ratio, y = WeightDiff_Analysis_next_day)) +
  geom_point(aes(color = Cycle_Phase), alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(title = "Relation entre Sodium-to-Calorie Ratio et la variation de poids",
       x = "Sodium-to-Calorie Ratio", y = "Variation de poids (WeightDiff_Analysis_next_day)") +
  theme_minimal()

# Calculer la corrélation entre Sodium-to-Calorie Ratio et WeightDiff_Analysis du jour suivant
cor_result <- cor.test(df$Sodium_Calorie_Ratio, df$WeightDiff_Analysis_next_day, method = "pearson", use = "complete.obs")
cor_result
