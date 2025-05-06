---
title: "Analyse Personnalisée des Fluctuations de Poids"
output: flexdashboard::flex_dashboard
runtime: shiny
---

```{r setup, include=FALSE}
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)
library(plotly)
library(zoo)

# Variable de contrôle pour le message d'erreur de durée minimale
too_few_days <- reactiveVal(FALSE)
```

# Interface principale
```{r}
fluidRow(
  column(6, downloadButton("downloadTemplate", "Télécharger le Template CSV")),
  column(6, fileInput("file", "Importer votre fichier CSV", accept = ".csv")),
  column(12,
         tags$p(strong(style = "font-size: 24px !important;", "Commencez par télécharger le modèle de données, remplissez-le avec vos informations, puis importez-le ci-dessus.")),
         p("⚠️ Pour que l’analyse soit significative, il est recommandé de fournir au moins 28 jours consécutifs de données, représentant un cycle complet.⚠️"),
         p("Si trop de valeurs sont manquantes, certaines analyses pourraient être inexactes ou ne pas refléter fidèlement vos variations réelles."),
         p("Si les données pour la Date, Cycle_Day ou Period sont manquantes, la ligne sera supprimée."),
         p("Les autres valeurs manquantes ont été remplacées par la dernière valeur connue ou, si aucune valeur précédente n'était disponible, par la première valeur connue.")
         )
)
```

# Rapport intéractif
```{r}
tabsetPanel(
  tabPanel("1. Importation des Données",
      conditionalPanel(
             condition = "output.tooFewDaysMessage",
             p(style = "color:red; font-weight:bold;",
               "⚠️ Vous devez importer au moins 28 jours de données pour que l’analyse soit pertinente.")
           ),
           p("Les valeurs manquantes ont été remplacées par la dernière valeur connue ou, si aucune valeur précédente n'était disponible, par la première valeur connue."),
           div(style = "height:600px; overflow-y:auto;",
               DTOutput("table_preview")
           ),
           p(HTML("Phases du Cycle:<br>
             Menstruation: lorsque Period = Y<br>
             Folliculaire: après les menstruations et jusqu'au jour 11<br>
             Ovulation: jours 12 à 16<br>
             Lutéale: jours 17 à 23<br>
             Pré-menstruelle: jours 24 à 28<br>
             Cycle long: jour 29 jusqu'au début des menstruations"))
  ),
  tabPanel("2. Évolution du Poids",
           dateRangeInput("date_filter", 
               label = "Filtrer par date", 
               start = NULL, 
               end = NULL,
               format = "yyyy-mm-dd"),
           plotlyOutput("weight_plot"),
           HTML("<p><b>Légende des lignes :</b><br>
              <span style='color:black;'>■</span> Poids réel (ligne noire)<br>
              <span style='color:blue;'>■</span> Tendance du poids (ligne bleue)</p>")
  ),
tabPanel("3. Rapport sur le Cycle menstruel",
  h4("ANALYSE DU CYCLE MENSTRUEL"),
  verbatimTextOutput("model_summary"),
  
  div(style = "text-align: left;",
      h4("⚖️  Gain de poids moyen (lbs), non relié à l'apport calorique durant différentes phases du cycle"),
      p("Le gain de poids inexpliqué fait référence à une prise de poids qui ne peut pas être attribuée à un excès d'apport calorique, ce qui suggère qu'il pourrait être dû à d'autres facteurs. Parmi les causes possibles, on retrouve la rétention d'eau liée à des fluctuations hormonales, des variations de la masse musculaire, ou encore des changements dans l'équilibre électrolytique du corps, comme la rétention de sodium."),
      div(style = "width: fit-content;", DTOutput("phase_effect_table"))),

  div(style = "text-align: left;",
      h4("Jours du cycle les plus associés à une prise de poids inexpliquée"),
      div(style = "width: fit-content;", DTOutput("top_positive_days"))),

  div(style = "text-align: left;",
      h4("Prédictions des 10 prochaines Ovulations et Menstruations"),
      div(style = "width: fit-content;", DTOutput("prediction_table")))
),
 tabPanel("4. Rapport Nutritionnel",
      
      # Ajouter un espace pour filtrer les dates (optionnel)
      dateRangeInput("date_filter_report",                
              label = "Filtrer par date", 
               start = NULL, 
               end = NULL,
               format = "yyyy-mm-dd"),
      
      #Filtre par objectif
      selectInput("goal_filter", 
            "Filtrer par Objectif :", 
            choices = c("Tous", "Weight Loss", "Weight Gain", "Maintenance"),
            selected = "Tous"),

      # Case à cocher pour inclure ou non les jours du tableau 'top_positive_days'
      checkboxInput("include_positive_days", "Cocher pour exclure les journées les plus associées à une prise de poids inexpliquée pour l'estimation du métabolisme.
Ces journées sont souvent liées à la rétention d’eau ou à d'autres facteurs temporaires, et non à une véritable prise de masse. Les exclure permet        d’obtenir une estimation plus précise de votre métabolisme réel.", value = TRUE),

        verbatimTextOutput("report_summary"),
 
        fluidRow(
    column(6, plotlyOutput("protein_fat_carbs_pie")), 
    column(6, plotlyOutput("protein_fat_carbs_recommended_pie")) 
  )

    ),
  tabPanel("5. Prédictions",
    
     numericInput("current_weight", "Poids actuel (lbs)", value = 150, min = 50, max = 400, step = 0.1),
     numericInput("projected_calories", "Apport calorique quotidien (Kcal)", value = 2000, min = 800, max = 6000, step = 100),
     numericInput("projected_days", "Durée de la simulation (jours)", value = 7, min = 1, max = 90),
     actionButton("calculate_projection", "Calculer la projection"),
     verbatimTextOutput("weight_projection"),
     plotlyOutput("projection_plot", height = "400px", width = "100%")

     )
)
```

```{r}
#1. Génération du Template et Importation des Données
# Générer un template CSV téléchargeable
output$downloadTemplate <- downloadHandler(
  filename = "template_donnees.csv",
  content = function(file) {
    write.csv(data.frame(
      Date = "mm/dd/yyyy",
      Cycle_Day = "integer",
      Period = "Y/N",
      Metabolism = "numeric, nombre total de calories nécessaires chaque jour pour couvrir le métabolisme de base et l’énergie dépensée par l’activité physique",
      Weight = "numeric, lbs",
      Calories = "numeric",
      Protein = "numeric, g",
      Fat = "numeric, g",
      Carbs = "numeric, g",
      Target_Calorie = "numeric",
      Target_Protein = "numeric, g",
      Target_Fat = "numeric, g",
      Target_Carbs = "numeric, g",
      Goal = "Weight Loss / Weight Gain / Maintenance"
    ), file, row.names = FALSE)
  }
)

# Lecture du fichier importé
data <- reactive({
  req(input$file)
  df <- read.csv(input$file$datapath) %>% mutate(Date = as.Date(Date, format="%m/%d/%Y"))
  return(df)
})

# Gestion des valeurs manquantes
cleaned_data <- reactive({
  req(data())
  df <- data()
  
  df <- df %>%
    mutate(across(c(Metabolism, Weight, Calories, Protein, Fat, Carbs, 
                    Target_Calorie, Target_Protein, Target_Fat, Target_Carbs, Goal),
                  ~ na.locf(., na.rm = FALSE))) %>%
    mutate(across(c(Metabolism, Weight, Calories, Protein, Fat, Carbs, 
                    Target_Calorie, Target_Protein, Target_Fat, Target_Carbs, Goal),
                  ~ na.locf(., fromLast = TRUE, na.rm = FALSE))) %>%
    filter(!is.na(Cycle_Day) & !is.na(Period) & !is.na(Date)) %>%
    
  #Création Cycle_phase
    mutate(
    Cycle_phase = case_when(
      Period == "Y" ~ "Menstrual",
      Cycle_Day >= 12 & Cycle_Day <= 16 ~ "Ovulation",
      Cycle_Day >= 17 & Cycle_Day <= 23 ~ "Luteal",
      Cycle_Day >= 24 & Cycle_Day <= 28 ~ "Pre-Menstrual",
      Cycle_Day >= 29 ~ "Long Cycle",
      TRUE ~ "Follicular"
    )
  )
  
 # Vérifier s'il y a au moins 28 jours distincts
  nb_days <- n_distinct(df$Date)
  if (nb_days < 28) {
    too_few_days(TRUE)
  } else {
    too_few_days(FALSE)
  }
  
  return(df)
})

# Affichage du tableau interactif
output$table_preview <- renderDT({
  req(cleaned_data())
  df <- cleaned_data()
  if (nrow(df) == 0) {
    return(datatable(data.frame(Message = "Aucune donnée chargée"), options = list(pageLength = 10)))
  }
  isolate({
    datatable(df, options = list(pageLength = 15, scrollY = "600px", scrollX = TRUE))
  })
})

# Mettre à jour la plage de dates une fois les données chargées
observe({
  req(cleaned_data())
  df <- cleaned_data()

  updateDateRangeInput(session, "date_filter",
                       start = min(df$Date, na.rm = TRUE),
                       end = max(df$Date, na.rm = TRUE))
  
  updateDateRangeInput(session, "date_filter_report",
                       start = min(df$Date, na.rm = TRUE),
                       end = max(df$Date, na.rm = TRUE))
})


# Message si moins de 28 jours
output$tooFewDaysMessage <- reactive({
  too_few_days()
})
outputOptions(output, "tooFewDaysMessage", suspendWhenHidden = FALSE)

```

```{r}
#2. Évolution du Poids
output$weight_plot <- renderPlotly({
  req(cleaned_data())
  df <- cleaned_data()

  # Apply date filter if selected
  if (!is.null(input$date_filter)) {
    df <- df %>% 
      filter(Date >= input$date_filter[1] & Date <= input$date_filter[2])
  }
  
  weight_min <- min(df$Weight, na.rm = TRUE)
  weight_max <- max(df$Weight, na.rm = TRUE)
  
# Couleur en arrière plan pour les phases du cycle
  phase_rects <- df %>%
    arrange(Date) %>%
    group_by(grp = cumsum(lag(Cycle_phase, default = first(Cycle_phase)) != Cycle_phase)) %>%
    summarise(
      start = min(Date),
      end = max(Date),
      Cycle_phase = first(Cycle_phase),
      .groups = "drop"
    )

  phase_colors <- c(
    "Menstrual" = "#fcbba1",
    "Follicular" = "#c6dbef",
    "Ovulation" = "#a1d99b",
    "Luteal" = "#fdd0a2",
    "Pre-Menstrual" = "#d9d9d9",
    "Long Cycle" = "#e7e1ef"
  )

  gg <- ggplot(df, aes(x = Date)) +
    # Ajouter les couleurs dans le graphique
    geom_rect(data = phase_rects,
              aes(xmin = start, xmax = end, ymin = weight_min, ymax = weight_max, fill = Cycle_phase),
              alpha = 0.3, inherit.aes = FALSE) +
    # Ligne pour le poids réel
    geom_line(aes(y = Weight), color = "black", size = 0.5) +
    # Ligne de tendance de poids
    geom_line(aes(y = predict(loess(Weight ~ as.numeric(Date)))), color = "blue", size = 1) +
    scale_fill_manual(values = phase_colors, name = "Phase du cycle") +
    labs(title = "Évolution du Poids", x = "Date", y = "Poids (lbs)") +
    theme_minimal()

  ggplotly(gg)
})

```

```{r}
#3. Rapport sur le Cycle menstruel
data_model <- reactive({
  req(cleaned_data())
  df <- cleaned_data() %>%
    arrange(Date)
  
    # Ajouter les colonnes nécessaires pour l’analyse de poids
  df <- df %>%
    mutate(
      Weight_Diff = Weight - lag(Weight, default = first(Weight)),
      Caloric_Balance = Calories - Metabolism,
      Theo_Weight_Diff = Caloric_Balance / 3500,
      WeightDiff_Analysis = Weight_Diff - Theo_Weight_Diff
    )
    
  #Calculer la durée moyenne des cycles menstruels
    df <- df %>%
      mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
      arrange(Date)
    
        # Identifier les débuts de cycle (passage de N à Y)
        df <- df %>%
          mutate(StartCycle = Period == "Y" & lag(Period, default = "N") == "N")
        
        # Extraire les dates de début de cycle
        start_dates <- df %>%
          filter(StartCycle) %>%
          pull(Date)
        
        # Calcul des durées entre débuts de cycle
        cycle_durations <- diff(start_dates)
        
        # Exclure les intervalles > 60 jours
        filtered_durations <- cycle_durations[cycle_durations <= 60]
        
        # Calcul de la durée moyenne
        avg_cycle_duration <- mean(as.numeric(filtered_durations), na.rm = TRUE)
  
  # Prédire les prochaines menstruations
  last_cycle_start <- max(start_dates)
  next_menstruations <- seq.Date(from = last_cycle_start + avg_cycle_duration,
                                 by = avg_cycle_duration,
                                 length.out = 12)
  
  # Ovulation estimée 14 jours avant la prochaine menstruation (ou 14 jours après le début d’un cycle)
  next_ovulations <- next_menstruations - 14
  
  # Calcul de la durée moyenne des menstruations
df <- df %>%
  mutate(Menstrual_Group = cumsum(Period == "Y" & lag(Period, default = "N") == "N"))

menstruation_durations <- df %>%
  filter(Period == "Y") %>%
  group_by(Menstrual_Group) %>%
  summarise(duration = n(), .groups = "drop") %>%
  pull(duration)

avg_menstruation_duration <- mean(menstruation_durations, na.rm = TRUE)

  # Créer un tableau
  prediction_table <- data.frame(
    `Ovulation prévue` = format(next_ovulations, "%Y-%m-%d"),
    `Menstruation prévue` = format(next_menstruations, "%Y-%m-%d")
  )
  
   # Excès de poids par phase
  phases_to_analyze <- c("Menstrual", "Ovulation", "Pre-Menstrual")
  excess_weight_by_phase <- df %>%
    filter(Cycle_phase %in% phases_to_analyze, WeightDiff_Analysis > 0) %>%
    group_by(Cycle_phase) %>%
    summarise(
      Moyenne_Excès_Poids_lbs = round(mean(WeightDiff_Analysis, na.rm = TRUE), 2),
      .groups = "drop"
    )

  # Jours du cycle les plus souvent associés à un excès de poids
top_10_positive_days <- df %>%
  group_by(Cycle_Day, Cycle_phase) %>%
  summarise(Positive_Count = sum(WeightDiff_Analysis > 0, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Positive_Count)) %>%
  head(10) %>%
  select(Cycle_Day, Cycle_phase)
  
# Retourner tout ce qu’on veut utiliser dans l’app
  list(
    df = df,  # données enrichies
    avg_cycle_duration = avg_cycle_duration,
    avg_menstruation_duration = avg_menstruation_duration,
    prediction_table = prediction_table,
    excess_weight_by_phase = excess_weight_by_phase,
    top_10_positive_days = top_10_positive_days
  )
})

output$model_summary <- renderPrint({
  req(data_model())
  pred <- data_model()

  cat("📅 Durée moyenne du cycle          :", round(pred$avg_cycle_duration, 1), "jours\n")
  cat("🩸 Durée moyenne des menstruations :", round(pred$avg_menstruation_duration, 1), "jours\n")
})

# Tableau: Gain de poids par phase
output$phase_effect_table <- renderDT({
  req(data_model())
  datatable(
    data_model()$excess_weight_by_phase,
    rownames = FALSE,
    options = list(dom = 't', autoWidth = TRUE),
    class = 'compact stripe hover cell-border'
  )
})

# Tableau: Top 10 jours du cycle
output$top_positive_days <- renderDT({
  req(data_model())
  datatable(
    data_model()$top_10_positive_days,
    rownames = FALSE,
    options = list(dom = 't', autoWidth = TRUE),
    class = 'compact stripe hover cell-border'
  )
})

# Tableau: Prédictions
output$prediction_table <- renderDT({
  req(data_model())
  datatable(
    data_model()$prediction_table,
    rownames = FALSE,
    options = list(dom = 't', autoWidth = TRUE),
    class = 'compact stripe hover cell-border'
  )
})

```

```{r}
#4. Rapport Nutritionnel
report <- reactive({
  req(cleaned_data()) 
  
  df <- cleaned_data()
  
  # Apply date filter if specified
  if (!is.null(input$date_filter_report)) {
    df <- df %>%
      filter(Date >= input$date_filter_report[1] & Date <= input$date_filter_report[2])
  }

  # Apply goal filter if specified
  if (!is.null(input$goal_filter) && input$goal_filter != "Tous") {
  df <- df %>% filter(Goal == input$goal_filter)
  }

  # Optionally include positive days from top_positive_days table ONLY to metabolism
  df_metabolism <- df
  if (input$include_positive_days) {
  df_metabolism <- df_metabolism %>%
    filter(Cycle_Day %in% data_model()$top_10_positive_days$Cycle_Day)
}


  # Estimation of total weight change
  df <- df[order(df$Date), ]
  start_date <- min(df$Date)
  end_date <- max(df$Date)
  weight_start <- df$Weight[df$Date == start_date]
  weight_end <- df$Weight[df$Date == end_date]
  weight_change <- weight_end - weight_start

  # Basal Metabolism Rate (BMR) and caloric analysis
  df_metabolism <- df_metabolism %>%
  arrange(Date) %>%
  mutate(
    Weight_Lag = lag(Weight),
    Caloric_Change = (Weight - Weight_Lag) * 3500,
    Estimated_Metabolism = Calories - Caloric_Change
  ) %>%
  filter(!is.na(Estimated_Metabolism))
  average_metabolism <- mean(df_metabolism$Estimated_Metabolism, na.rm = TRUE)


  # Macronutrient consumption averages
  avg_protein_consumed <- mean(df$Protein, na.rm = TRUE)
  avg_fat_consumed <- mean(df$Fat, na.rm = TRUE)
  avg_carb_consumed <- mean(df$Carbs, na.rm = TRUE)

  # Macronutrient target averages
  avg_protein_target <- mean(df$Target_Protein, na.rm = TRUE)
  avg_fat_target <- mean(df$Target_Fat, na.rm = TRUE)
  avg_carb_target <- mean(df$Target_Carbs, na.rm = TRUE)
  
  #Retourner tout ce qu'on veut utiliser dans l'app
  list(
    df = df, #données enrichies
    weight_change = weight_change,
    average_metabolism = average_metabolism
  )
})

output$report_summary <- renderPrint({
  req(report())
  pred <- report()

  if (pred$weight_change > 0) {
    cat("Gain de poids total de", round(pred$weight_change, 2), "lbs\n")
  } else if (pred$weight_change < 0) {
    cat("Perte de poids totale de", abs(round(pred$weight_change, 2)), "lbs\n")
  } else {
    cat("Aucun changement de poids\n")
  }
  
  cat("Métabolisme moyen estimé sur la période:", round(pred$average_metabolism, 0), "Kcal\n")
})


# Pie chart for consumed macronutrients
output$protein_fat_carbs_pie <- renderPlotly({
  req(cleaned_data())
  df <- cleaned_data()

  # Apply date filter
  if (!is.null(input$date_filter_report)) {
    df <- df %>%
      filter(Date >= input$date_filter_report[1] & Date <= input$date_filter_report[2])
  }

  # Apply goal filter (if not "Tous")
  if (input$goal_filter != "Tous") {
    df <- df %>% filter(Goal == input$goal_filter)
  }

  # Macronutrient consumption
  avg_protein_consumed <- mean(df$Protein, na.rm = TRUE)
  avg_carb_consumed <- mean(df$Carbs, na.rm = TRUE)
  avg_fat_consumed <- mean(df$Fat, na.rm = TRUE)

  plot_ly(
    labels = factor(c("Protéines", "Glucides", "Graisses"),
                    levels = c("Protéines", "Glucides", "Graisses")),
    values = c(avg_protein_consumed, avg_carb_consumed, avg_fat_consumed),
    type = 'pie',
    sort = FALSE,
    marker = list(
      colors = c('#FF6347', '#FFD700', '#90EE90'),
      line = list(color = '#FFFFFF', width = 1)
    ),
    textinfo = 'label+percent',
    hoverinfo = 'label+value+percent'
  ) %>% layout(
    title = "Répartition des Macronutriments Consommés",
    showlegend = TRUE
  )
})

# Pie chart for recommended macronutrients
output$protein_fat_carbs_recommended_pie <- renderPlotly({
  req(cleaned_data())
  df <- cleaned_data()

  # Apply date filter
  if (!is.null(input$date_filter_report)) {
    df <- df %>%
      filter(Date >= input$date_filter_report[1] & Date <= input$date_filter_report[2])
  }

  # Apply goal filter (if not "Tous")
  if (input$goal_filter != "Tous") {
    df <- df %>% filter(Goal == input$goal_filter)
  }

  # Macronutrient targets
  avg_protein_target <- mean(df$Target_Protein, na.rm = TRUE)
  avg_carb_target <- mean(df$Target_Carbs, na.rm = TRUE)
  avg_fat_target <- mean(df$Target_Fat, na.rm = TRUE)

  plot_ly(
    labels = factor(c("Protéines", "Glucides", "Graisses"),
                    levels = c("Protéines", "Glucides", "Graisses")),
    values = c(avg_protein_target, avg_carb_target, avg_fat_target),
    type = 'pie',
    sort = FALSE,
    marker = list(
      colors = c('#FF6347', '#FFD700', '#90EE90'),
      line = list(color = '#FFFFFF', width = 1)
    ),
    textinfo = 'label+percent',
    hoverinfo = 'label+value+percent'
  ) %>% layout(
    title = "Répartition des Macronutriments Recommandés",
    showlegend = TRUE
  )
})

```

```{r}
#5. Prédictions

observeEvent(input$calculate_projection, {
  req(input$current_weight, input$projected_calories, input$projected_days)
  
  # Entrées utilisateur
  current_weight <- input$current_weight
  daily_calories <- input$projected_calories
  duration_days <- input$projected_days
  
  # Récupération du métabolisme estimé (moyenne)
  metabolism <- report()$average_metabolism
  
  # Calcul du bilan calorique total et de la variation de poids
  total_caloric_balance <- (daily_calories - metabolism) * duration_days
  expected_weight_change <- total_caloric_balance / 3500  # 3500 Kcal = 1 lb
  projected_weight <- current_weight + expected_weight_change
  
  # Affichage dans le texte
  output$weight_projection <- renderPrint({
    cat("📊 Prévision du poids :\n")
    cat("- Poids actuel :", round(current_weight, 2), "lbs\n")
    cat("- Apport calorique prévu :", daily_calories, "Kcal/jour pendant", duration_days, "jours\n")
    cat("- Métabolisme estimé :", round(metabolism, 0), "Kcal/jour\n")
    cat("- Bilan calorique total :", round(total_caloric_balance, 0), "Kcal\n")
    cat("- Variation de poids estimée :", round(expected_weight_change, 2), "lbs\n")
    cat("📈 Poids projeté :", round(projected_weight, 2), "lbs\n")
  })
  
  # Affichage du graphique de l'évolution du poids projeté
output$projection_plot <- renderPlotly({
  days <- 1:duration_days
  projected_weights <- current_weight + (days * (expected_weight_change / duration_days))
  
  plot_ly(x = days, y = projected_weights, type = 'scatter', mode = 'lines', 
          line = list(color = 'blue', width = 2)) %>%
    layout(title = "Évolution du poids projeté sur la période",
           xaxis = list(title = "Jours"),
           yaxis = list(title = "Poids projeté (lbs)"))
  })

})


```