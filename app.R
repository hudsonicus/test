library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# UI ----
ui <- page_sidebar(
  title = tags$strong("Données de manchots"),
  sidebar = sidebar(
    "Choisir l'espèce et l'année",
  selectInput(
    "espece", # Nom
    tags$strong("Espèce"),
    #labels = c(),
    choices = unique(penguins$species), # Enregistrer les valeurs avec 1,2,3 (exemple)
    selected = 1
  ),
  checkboxGroupInput(
    "annee", # Nom
    "Sélectionner",
    choices = list("2007" = 1, "2008" = 2, "2009" = 3),
    selected = 1
  )
  ),
  card(
    card_header(tags$strong("Tableau de bord")),
    "Présentation des données de manchots en Antarctique",
      layout_columns(
      card(#"L'espèce choisie est: ", 
           #textOutput("espece_select"),
           plotOutput("plot1")
           ),
      card(#"L'année choisie est: ", 
        #textOutput("annee_select"),
        plotOutput("plot2")),
      col_widths = c(6, 6)
    ),
    card("Tableau",
         dataTableOutput("table"))
      
    )
    
  
)

# Serveur logic ----
server <- function(input, output) {
  
  bs_themer()
  
  output$espece_select = renderText({
    input$espece
  })
  
  output$annee_select = renderText({
    input$annee
  })
  
  output$plot1 = renderPlot(
    penguins %>%
      filter(!is.na(flipper_len),!is.na(body_mass)) %>%
      ggplot(aes(x = body_mass, y = flipper_len)) +
      geom_point(color = "#74ADD1", alpha = 0.7) + 
      geom_smooth(method = "lm", se = FALSE, color = "#2B83BA", linewidth = 1) +
      labs(title = "Longueur de l'aile selon la masse corporelle",
           x = "Masse corporelle (g)",
           y = "Longueur de l'aile (mm)" ) +
      theme_minimal()
  )
  
  output$plot2 = renderPlot(
    penguins %>%
      mutate(sex = recode(sex, "female" = "femelle", "male"   = "mâle")) %>%
      filter(!is.na(body_mass), !is.na(sex)) %>%
      ggplot(aes(x = sex, y = body_mass, color = sex)) +
      stat_summary(fun = mean, geom = "point", size = 3) +
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
      facet_wrap(~ year) +
      scale_color_manual(values = c("femelle" = "#74ADD1", "mâle"   = "#2B83BA")) +
      labs(title = "Masse corporelle selon le sexe",
           x = NULL,
           y = "Masse corporelle (g)",
           color = "Sexe") +
      theme_minimal() +
      theme(axis.text.x = element_blank())
  )
  
  output$table = renderDataTable(
    penguins
  )
  
}

# Appeler shinyapp ----
shinyApp(ui = ui, server = server)
