library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

# Charger les données
data <- read_excel("C:/Users/Asus/OneDrive/Desktop/tp stat/dashboard natalité/don_mls.xlsx")
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = span(icon("city"), " Taux d'urbanisation")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse Statistique", tabName = "statistiques", icon = icon("chart-bar")),
      menuItem("Ajustement Linéaire", tabName = "ajustement", icon = icon("chart-line")),
      menuItem("Prédiction", tabName = "prediction", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap"),
      tags$style(HTML("
        /* Global Styles */
        body { font-family: 'Montserrat', sans-serif; background-color: #f4f6f9; }
        .skin-purple .main-header .logo { background-color: #6a1b9a; font-weight: 700; font-size: 24px; }
        .skin-purple .main-header .navbar { background-color: #8e24aa; }
        .skin-purple .main-sidebar { background-color: #2c3e50; }
        .content-wrapper, .right-side { background-color: #f4f6f9; }

        /* Box/Card Styling */
        .box {
          border-radius: 12px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          border-top: none;
          background: #ffffff;
          transition: transform 0.2s ease, box-shadow 0.2s ease;
        }
        .box:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 16px rgba(0,0,0,0.15);
        }
        .box-header.with-border {
          border-bottom: 1px solid #eee;
          background: #fafafa;
          border-radius: 12px 12px 0 0;
          padding: 15px;
        }
        .box-title { font-weight: 600; font-size: 16px; color: #333; }

        /* Value Boxes  & Buttons */
        .btn { border-radius: 50px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px; }
        .btn-danger { background-color: #e53935; border-color: #e53935; }
        .btn-danger:hover { background-color: #c62828; }

        /* Inputs */
        .form-control { border-radius: 8px; border: 1px solid #ddd; box-shadow: none; }
        .form-control:focus { border-color: #8e24aa; box-shadow: 0 0 0 2px rgba(142, 36, 170, 0.2); }
      "))
    ),
    tabItems(
      #  Page Analyse Statistique
      tabItem(
        tabName = "statistiques",
        fluidRow(
          box(
            title = "Indicateurs Clés", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("vbox_urban", width = 6),
              valueBoxOutput("vbox_birth", width = 6)
            )
          )
        ),
        fluidRow(
          box(
            title = "Tableau des données", width = 12, status = "primary",
            solidHeader = TRUE, collapsible = TRUE, icon = icon("table"),
            DTOutput("data_table")
          )
        ),
        fluidRow(
          box(
            title = "Distributions des variables", width = 12, status = "warning",
            solidHeader = TRUE, collapsible = TRUE, icon = icon("chart-pie"),
            plotOutput("boxplot_des_variables", height = "300px")
          )
        ),
        fluidRow(
          box(
            title = "Nuage de points (Exploratoire)", width = 12, status = "info",
            solidHeader = TRUE, collapsible = TRUE, icon = icon("search"),
            plotOutput("plot1", height = "300px")
          )
        )
      ),

      #  Page Ajustement linéaire
      tabItem(
        tabName = "ajustement",
        fluidRow(
          box(
            title = "Qualité de l'ajustement", width = 12, status = "danger", solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("vbox_corr", width = 12)
            )
          )
        ),
        fluidRow(
          box(
            title = "Modèle de Régression", width = 8, status = "info",
            solidHeader = TRUE, collapsible = TRUE, icon = icon("chart-line"),
            plotOutput("plot2", height = "400px")
          ),
          box(
            title = "Résumé Statistique", width = 4, status = "success",
            solidHeader = TRUE, collapsible = TRUE, icon = icon("list-alt"),
            verbatimTextOutput("reg_summary")
          )
        )
      ),
      tabItem(
        tabName = "prediction",
        fluidRow(
          box(
            title = "Paramètres de la prédiction", width = 4, solidHeader = TRUE, status = "primary",
            icon = icon("sliders-h"),
            numericInput("new_x", "Saisir le taux d'urbanisation (%):", value = 50, min = 0, max = 100, step = 0.5),
            hr(),
            actionButton("predict_btn", "Lancer la prédiction", icon = icon("rocket"), class = "btn-primary btn-block")
          ),
          box(
            title = "Résultat de la prédiction", width = 8, solidHeader = TRUE, status = "success",
            icon = icon("bullseye"),
            uiOutput("pred_result_ui"),
            plotOutput("pred_plot", height = "350px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Custom Theme for consistency
  my_theme <- theme_minimal(base_size = 14) +
    theme(
      text = element_text(family = "Montserrat"),
      plot.title = element_text(face = "bold", hjust = 0.5, color = "#2c3e50"),
      axis.title = element_text(face = "bold", color = "#2c3e50"),
      axis.text = element_text(color = "#555555"),
      panel.grid.major = element_line(color = "#e0e0e0"),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )

  # Value Boxes
  output$vbox_urban <- renderValueBox({
    mean_val <- mean(data$`Taux d'urbanisation`, na.rm = TRUE)
    valueBox(
      paste0(round(mean_val, 2), "%"), "Moyenne Urbanisation",
      icon = icon("city"),
      color = "purple"
    )
  })

  output$vbox_birth <- renderValueBox({
    mean_val <- mean(data$`Taux de natalité`, na.rm = TRUE)
    valueBox(
      paste0(round(mean_val, 2), "‰"), "Moyenne Natalité",
      icon = icon("baby"),
      color = "fuchsia"
    )
  })

  output$vbox_corr <- renderValueBox({
    cor_val <- cor(data$`Taux d'urbanisation`, data$`Taux de natalité`, use = "complete.obs")
    valueBox(
      round(cor_val, 2), "Corrélation",
      icon = icon("link"),
      color = "maroon"
    )
  })

  # Tableau des données
  output$data_table <- renderDT({
    df_subset <- data[, c("Taux d'urbanisation", "Taux de natalité")]
    datatable(df_subset,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        searching = FALSE,
        lengthChange = FALSE,
        info = FALSE,
        paging = TRUE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#8e24aa', 'color': '#fff'});",
          "}"
        )
      )
    )
  })

  # Figure 1 : Nuage de points simple (Converted to ggplot for better style)
  output$plot1 <- renderPlot({
    req(data)
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    # Assuming we want to plot the first two numeric variables found, or just urbanization vs birth rate again but raw
    # If the logic was "plot all numeric variables", base R `plot(dataframe)` does a matrix scatterplot.
    # GGally::ggpairs would be better but let's stick to a simple clean scatter of the 2 main vars since that's the focus.
    # Reverting to the logic: "Nuage de points des variables numériques" -> likely a pairs plot.
    # Since we want to modernize, let's keep it simple: Correlation between the two main variables.

    ggplot(data, aes(x = `Taux d'urbanisation`, y = `Taux de natalité`)) +
      geom_point(color = "#8e24aa", alpha = 0.7, size = 3) +
      labs(
        title = "Distribution conjointe",
        x = "Taux d'urbanisation", y = "Taux de natalité"
      ) +
      my_theme
  })

  # Figure 2 : ggplot avec modèle linéaire
  output$plot2 <- renderPlot({
    if (!all(c("Taux d'urbanisation", "Taux de natalité") %in% colnames(data))) {
      return(NULL)
    }

    modele <- lm(`Taux de natalité` ~ `Taux d'urbanisation`, data = data)

    ggplot(data, aes(x = `Taux d'urbanisation`)) +
      geom_point(aes(y = `Taux de natalité`), color = "#8e24aa", alpha = 0.6, size = 3) +
      geom_smooth(aes(y = `Taux de natalité`), method = "lm", color = "#e53935", se = TRUE, fill = "#e53935", alpha = 0.1) +
      labs(
        x = "Taux d'urbanisation", y = "Taux de natalité",
        title = "Ajustement linéaire"
      ) +
      my_theme
  })

  output$reg_summary <- renderPrint({
    modele <- lm(`Taux de natalité` ~ `Taux d'urbanisation`, data = data)
    summary(modele)
  })

  #  Boxplots supplémentaires
  output$boxplot_des_variables <- renderPlot({
    req(data)

    data_long <- data %>%
      select(`Taux de natalité`, `Taux d'urbanisation`) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valeur")

    ggplot(data_long, aes(x = Variable, y = Valeur, fill = Variable)) +
      geom_boxplot(alpha = 0.8, outlier.colour = "red", outlier.shape = 1) +
      scale_fill_manual(values = c("Taux de natalité" = "#ab47bc", "Taux d'urbanisation" = "#7e57c2")) +
      labs(title = "Distributions (Boxplots)", y = "Valeur", x = "") +
      my_theme +
      theme(legend.position = "none")
  })

  #  Prédiction Visualisation & ValueBox

  # Reactive calculation for prediction
  prediction_vals <- eventReactive(input$predict_btn, {
    req(input$new_x)

    # Input Validation
    if (input$new_x < 0 || input$new_x > 100) {
      return(list(error = "Erreur : Le taux d'urbanisation doit être compris entre 0 et 100."))
    }

    modele <- lm(`Taux de natalité` ~ `Taux d'urbanisation`, data = data)
    new_data <- data.frame(`Taux d'urbanisation` = input$new_x, check.names = FALSE)

    # Point prediction
    pred <- predict(modele, newdata = new_data)
    # Start confint
    pred_int <- predict(modele, newdata = new_data, interval = "confidence", level = 0.95)

    list(fit = pred, lwr = pred_int[1, "lwr"], upr = pred_int[1, "upr"], x = input$new_x, error = NULL)
  })

  output$pred_result_ui <- renderUI({
    res <- prediction_vals()

    if (!is.null(res$error)) {
      return(div(
        style = "text-align: center; padding: 20px; color: #d32f2f; font-weight: bold; font-size: 18px;",
        icon("exclamation-triangle"), br(),
        res$error
      ))
    }

    div(
      style = "text-align: center; padding: 20px;",
      div(
        style = "font-size: 56px; font-weight: 700; color: #8e24aa; line-height: 1;",
        paste0(round(res$fit, 2), "‰")
      ),
      div(
        style = "font-size: 18px; color: #7f8c8d; margin-top: 5px; font-weight: 500;",
        "Taux de natalité prédit"
      ),
      div(
        style = "margin-top: 15px; background: #f3e5f5; display: inline-block; padding: 8px 15px; border-radius: 20px; color: #6a1b9a; font-weight: 600;",
        paste0("Intervalle 95% : [", round(res$lwr, 2), " - ", round(res$upr, 2), "]")
      )
    )
  })

  output$pred_plot <- renderPlot({
    req(prediction_vals())
    res <- prediction_vals()

    if (!is.null(res$error)) {
      return(NULL)
    }

    modele <- lm(`Taux de natalité` ~ `Taux d'urbanisation`, data = data)

    ggplot(data, aes(x = `Taux d'urbanisation`)) +
      geom_point(aes(y = `Taux de natalité`), color = "#bdbdbd", alpha = 0.4, size = 2) + # Gray out existing data
      geom_smooth(aes(y = `Taux de natalité`), method = "lm", color = "#7e57c2", se = TRUE, alpha = 0.1) +

      # The predicted point
      geom_point(aes(x = res$x, y = res$fit), color = "#d32f2f", size = 5, shape = 18) +
      geom_errorbar(aes(x = res$x, ymin = res$lwr, ymax = res$upr, y = res$fit), color = "#d32f2f", width = 2, size = 1.2) +
      geom_text(aes(x = res$x, y = res$upr, label = "Prédiction"), vjust = -1, color = "#d32f2f", fontface = "bold") +
      labs(
        x = "Taux d'urbanisation", y = "Taux de natalité",
        title = paste("Visualisation de la prédiction pour x =", res$x, "%")
      ) +
      my_theme
  })
}


shinyApp(ui, server)
