##libraries
library(shinythemes)
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(leaflet.extras)
library(ggplot2)
library(tidyr)
library(haven)
library(raster)
library(rsconnect)
library(arrow)


#global data
MH_states <- read_parquet("MH_states.parquet")
states <- readRDS("states_simplified.rds")

#only needed columns
MH_clean <- read_parquet("MH_clean.parquet", 
                         col_select = c(
                           # Add your demographic columns here - adjust names as needed
                           "Sex", "Age", "Race", "Education", "Ethnicity", 
                           "Marriage Status", "Employment Status", "Not in Labor Force", 
                           "Residential Status", "Veteran Status", "Number of Diagnosed Disorders",
                           # Diagnosis flags
                           "TRAUSTREFLG", "ANXIETYFLG", "ADHDFLG", "CONDUCTFLG", 
                           "DELIRDEMFLG", "BIPOLARFLG", "DEPRESSFLG", "ODDFLG", 
                           "PDDFLG", "PERSONFLG", "SCHIZOFLG", "ALCSUBFLG", "OTHERDISFLG"
                         ))

MH_clean <- MH_clean %>%
  dplyr::mutate(dplyr::across(where(is.character), as.factor))

#only essential columns
states <- states %>%
  dplyr::select(STATEFP, NAME, geometry)

#join states data
MH_states_map <- dplyr::left_join(states, MH_states, by = "STATEFP")

rm(states, MH_states)
gc() 

#color palette
pal <- leaflet::colorBin(
  palette = c('#96AEE0', "#6F88BF", "#345294", "#10337D", "#061A45"),
  domain = MH_states_map$MH_RATE,
  bins = c(0,20,40,60,80,100),
  na.color = "#BDBDBD"
)

#diagnosis names
diagnosis_names <- c(
  TRAUSTREFLG="Trauma & Stress", ANXIETYFLG="Anxiety", ADHDFLG="ADHD",
  CONDUCTFLG="Conduct Disorder", DELIRDEMFLG="Delirium/Dementia",
  BIPOLARFLG="Bipolar", DEPRESSFLG="Depression", ODDFLG="ODD",
  PDDFLG="PDD", PERSONFLG="Personality Disorder", SCHIZOFLG="Schizophrenia",
  ALCSUBFLG="Alcohol/Substance", OTHERDISFLG="Other"
)


#server
server <- function(input, output, session){
  
  # Leaflet map 
  output$map <- leaflet::renderLeaflet({
    
    leaflet::leaflet(MH_states_map) %>%
      leaflet::addProviderTiles("Stadia.StamenToner") %>%
      leaflet::setView(lng = -95, lat = 38, zoom = 3) %>%
      leaflet::addPolygons(
        fillColor = ~pal(MH_RATE),
        weight = 1.2,
        color = "#4A4A4A",
        fillOpacity = 0.85,
        highlight = leaflet::highlightOptions(
          weight = 3,
          color = "#000000",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~lapply(
          paste0(
            "<b>State:</b> ", NAME, "<br/>",
            "<b>Overall Diagnosis Rate:</b> ", MH_RATE, "%<br/><br/>",
            "<b>Trauma & Stress:</b> ", TRAUSTREFLG, "%<br/>",
            "<b>Anxiety:</b> ", ANXIETYFLG, "%<br/>",
            "<b>ADHD:</b> ", ADHDFLG, "%<br/>",
            "<b>Conduct:</b> ", CONDUCTFLG, "%<br/>",
            "<b>Delirium/Dementia:</b> ", DELIRDEMFLG, "%<br/>",
            "<b>Bipolar:</b> ", BIPOLARFLG, "%<br/>",
            "<b>Depression:</b> ", DEPRESSFLG, "%<br/>",
            "<b>ODD:</b> ", ODDFLG, "%<br/>",
            "<b>PDD:</b> ", PDDFLG, "%<br/>",
            "<b>Personality:</b> ", PERSONFLG, "%<br/>",
            "<b>Schizophrenia:</b> ", SCHIZOFLG, "%<br/>",
            "<b>Alcohol/Substance:</b> ", ALCSUBFLG, "%<br/>",
            "<b>Other:</b> ", OTHERDISFLG, "%<br/>"
          ),
          htmltools::HTML
        )
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        pal = pal,
        values = MH_states_map$MH_RATE,
        title = "Any Mental Health Diagnosis (%)",
        na.label = "No Data",
        opacity = 1
      )
  })
  
  
  #barplot (with sampling)
  make_demo_barplot <- function(data, flag_col, demo_var, plot_title) {
    
    filtered <- data %>% 
      dplyr::filter(.data[[flag_col]] == 1)
    
    if(nrow(filtered) > 50000) {
      filtered <- filtered %>% dplyr::slice_sample(n = 50000)
    }
    
    plot_data <- filtered %>%
      dplyr::group_by(.data[[demo_var]]) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(proportion = count / sum(count))
    
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data[[demo_var]],
        y = proportion,
        fill = .data[[demo_var]]
      )
    ) +
      ggplot2::geom_bar(stat = "identity", color = "white") +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::percent(proportion, accuracy = 1)),
        vjust = -0.5,
        color = "white",
        size = 4
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = ggplot2::expansion(mult = c(0, 0.1))
      ) +
      ggplot2::labs(
        title = plot_title,
        x = demo_var,
        y = "Proportion (%)"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        plot.background   = ggplot2::element_rect(fill = "black", color = NA),
        panel.background  = ggplot2::element_rect(fill = "black", color = NA),
        panel.grid.major  = ggplot2::element_line(color = "gray40"),
        panel.grid.minor  = ggplot2::element_line(color = "gray40"),
        axis.text         = ggplot2::element_text(color = "white"),
        axis.title        = ggplot2::element_text(color = "white", face = "bold"),
        plot.title        = ggplot2::element_text(color = "white", face = "bold", hjust = 0.5),
        legend.position   = "none",
        axis.text.x       = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }
  
  
  #barplot output
  output$demo_plot <- renderPlot({
    make_demo_barplot(
      data = MH_clean,
      flag_col = input$demo_diagnosis,
      demo_var = input$demo_variable,
      plot_title = paste(
        "Distribution of", input$demo_variable,
        "for", diagnosis_names[input$demo_diagnosis], 
        "Diagnoses in the United States 2023"
      )
    )
  })
  
  
  #data table w/ limit
  filtered_data <- reactive({
    data <- if (input$diagnosis_filter == "NONE") {
      MH_clean
    } else {
      MH_clean %>% dplyr::filter(.data[[input$diagnosis_filter]] == 1)
    }
    if(nrow(data) > 3000) {
      data <- data %>% dplyr::slice_head(n = 3000)
    }
    
    data
  })
  
  output$filtered_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      options = list(
        pageLength = 15, 
        scrollX = TRUE,
        dom = 'ltipr' 
      )
    )
  })
  
}


#ui
ui <- shinyUI(
  fluidPage(
    theme = shinytheme("superhero"),
    
    # Changing font
    tags$head(
      tags$style(HTML("
    * {
      font-family: 'Baskerville', serif !important;
    }
  "))
    ),
    
    # Bold titles
    tags$head(
      tags$style(HTML("
      /* Bold navListPanel titles */
      .navlist-panel .nav>li>a {
        font-weight: 700 !important;
      }
    "))
    ),
    
    # Fixing data table
    tags$style(HTML("
  table.dataTable {
    color: white !important;
    background-color: #222 !important;
  }
  table.dataTable th {
    color: white !important;
    background-color: #333 !important;
  }
  table.dataTable td {
    color: white !important;
  }
  .dataTables_wrapper .dataTables_filter input,
  .dataTables_wrapper .dataTables_length select {
    background-color: #333 !important;
    color: white !important;
    border: 1px solid #555 !important;
  }
  .dataTables_wrapper .dataTables_filter label,
  .dataTables_wrapper .dataTables_length label,
  .dataTables_wrapper .dataTables_info,
  .dataTables_wrapper .dataTables_paginate {
    color: white !important;
  }
  .dataTables_wrapper .dataTables_paginate a {
    color: white !important;
    background-color: #333 !important;
    border: 1px solid #555 !important;
  }
  .dataTables_wrapper .dataTables_paginate a.current {
    background-color: #555 !important;
  }
")),
    
    navlistPanel(
      "Navigation Bar",
      widths = c(3,9),
      # Welcome tab
      tabPanel(
        "Welcome",
        br(),
        div(
          style = "text-align: center;",
          img(src = "banner-8817510_1280.jpg", height = "240px"),
          h1("Welcome to Mental Illness Data Center"),
          h4("by Alli Hoffman"),
          h2("Instructions:"),
          h4("This 2023 data is collected by the Substance Abuse and Mental Health Servies Administration of the United States (SAMHSA). 
          This SAMHSA data includes demographic and mental health characteristics for clients who have used mental health services in facilities that report to individual state administrative data systems, including geospatial and demographic data.  
             Use the tab panels on the left to explore these aspects of the data, as well as key limitations and findings I found notable. Please observe the limitations below before proceeding."),
          h2("Data Limitations:"),
          div(
            style = "text-align: left; margin-left: 20px;", 
            tags$ul(
              tags$li("The MH-CLD dataset does not represent the total national demand for mental health treatment."),
              tags$li("It does not describe the mental health status of the entire U.S. population."),
              tags$li("Data come from facilities under state mental health administration authority, which may vary in licensure, certification, and funding."),
              tags$li("Some states regulate private facilities and practitioners differently, and services may be provided in correctional facilities in some states but not others."),
              tags$li("Some states may report more or fewer diagnoses depending on insurance coverage requirements or state-specific policies, so comparisons between states should be made with caution."),
              tags$li("Up to three mental health diagnoses per individual are reported; not all diagnoses for every individual may be captured."),
              tags$li("Some individuals may have no valid mental health diagnosis reported."),
              tags$li("Missing data is not necessarily random, so prevalence rates may be biased."),
              tags$li(strong("Note: Data has been sampled for performance on shinyapps.io free tier."))
            )
          )
        )
      ),
      
      #map tab
      tabPanel(
        "Mental Illness Map",
        br(),
        div(
          style = "text-align: center;",
          h2("Mental Illness Map: Tracking the Most Common Diagnoses"),
          h4("These proportions represent the frequency at which these illnesses are present in each state (based on those who use mental health services).")
        ),
        br(),
        leafletOutput("map", width = "100%", height = "600px")
      ),
      tabPanel(
        "Demographics",
        br(),
        div(
          style = "text-align: center;",
          h2("Explore Demographics by Diagnosis"),
          h4("Choose a diagnosis and a demographic variable to visualize proportional distributions.")
        ),
        
        br(),
        selectInput(
          inputId = "demo_diagnosis",
          label = "Select Diagnosis:",
          choices = c(
            "Trauma & Stress" = "TRAUSTREFLG",
            "Anxiety" = "ANXIETYFLG",
            "ADHD" = "ADHDFLG",
            "Conduct Disorder" = "CONDUCTFLG",
            "Delirium/Dementia" = "DELIRDEMFLG",
            "Bipolar Disorder" = "BIPOLARFLG",
            "Depression" = "DEPRESSFLG",
            "Oppositional Defiant Disorder" = "ODDFLG",
            "Pervasive Developmental Disorder" = "PDDFLG",
            "Personality Disorder" = "PERSONFLG",
            "Schizophrenia" = "SCHIZOFLG",
            "Alcohol/Substance" = "ALCSUBFLG",
            "Other" = "OTHERDISFLG"
          ),
          selected = "TRAUSTREFLG"
        ),
        
        selectInput(
          inputId = "demo_variable",
          label = "Select Demographic Variable:",
          choices = c(
            "Sex", "Age", "Race", "Education", "Ethnicity", "Marriage Status", 
            "Employment Status", "Not in Labor Force", "Residential Status", 
            "Veteran Status", "Number of Diagnosed Disorders"
          ),
          selected = "Sex"
        ),
        
        plotOutput("demo_plot")
      ),
      tabPanel(
        "Data Table",
        fluidPage(
          br(),
          div(
            style = "text-align: center;",
            h2("Take a Look at the Data!"),
            h4("Use the drop down below to filter the data as you see fit"),
            h5("(Limited to 3,000 rows for performance)")
          ),
          br(),
          selectInput(
            "diagnosis_filter",
            "Filter by Diagnosis:",
            choices = c(
              "None" = "NONE",
              "Trauma & Stress" = "TRAUSTREFLG",
              "Anxiety" = "ANXIETYFLG",
              "ADHD" = "ADHDFLG",
              "Conduct Disorder" = "CONDUCTFLG",
              "Delirium/Dementia" = "DELIRDEMFLG",
              "Bipolar Disorder" = "BIPOLARFLG",
              "Depression" = "DEPRESSFLG",
              "Oppositional Defiant Disorder" = "ODDFLG",
              "Pervasive Developmental Disorder" = "PDDFLG",
              "Personality Disorder" = "PERSONFLG",
              "Schizophrenia" = "SCHIZOFLG",
              "Alcohol/Substance" = "ALCSUBFLG",
              "Other" = "OTHERDISFLG"
            ),
            selected = "NONE"
          ),
          
          # Data table underneath
          DT::dataTableOutput("filtered_table")
        )
      ),
      tabPanel(
        "Key Findings",
        br(),
        h1("Here are some key findings about this dataset:"),
        br(),
        div(
          style = "text-align: left; margin-left: 20px;",
          h2("Age Patterns"),
          tags$ul(
            tags$li("The distribution of age is greatly determined by the disorder:"),
            tags$li("Conduct disorders, ADHD, ODD, trauma, and PDD are largely present in children aged 0-11, due to early-onset symptoms that are disruptive to schooling."),
            tags$li("Dementia and delirium are largely present in the 65+ population."),
            tags$li("A very small percentage of children have diagnoses like personality disorders, bipolar disorder, and schizophrenia, as these typically manifest later in life due to genetics and traumatic experiences.")
          )
        ),
        div(
          style = "text-align: left; margin-left: 20px;",
          h2("Racial Bias"),
          tags$ul(
            tags$li("There is a noticeable racial bias in diagnosing highly stigmatized disorders:"),
            tags$li("Black/African American individuals have higher proportions of conduct disorders, oppositional defiant disorders, and schizophrenia diagnoses, likely due to systemic bias in the diagnostic process."),
            tags$li("American Indian/Alaskan Native populations show a higher proportion of substance use disorders, influenced by socioeconomic factors, systemic inequities, and historical trauma.")
          )
        ),
        div(
          style = "text-align: left; margin-left: 20px;",
          h2("Sex Bias"),
          tags$ul(
            tags$li("Females are disproportionately diagnosed with anxiety, bipolar disorder, personality disorders, depression, and stress-related disorders. These are often classified as 'internal' or 'emotional,' reflecting societal stereotypes."),
            tags$li("Males are disproportionately diagnosed with ADHD, conduct disorder, oppositional defiant disorder, PDD, schizophrenia, and substance use disorders, often framed as 'external' or 'behavioral,' reflecting societal stereotypes of men as action-oriented and stoic.")
          )
        ),
        div(
          style = "text-align: left; margin-left: 20px;",
          h2("Other Significant Findings"),
          tags$ul(
            tags$li("Dementia/Delirium patients are more likely to live in 'other' housing, such as memory care facilities or specialized homes for the elderly."),
            tags$li("Individuals with substance use diagnoses have among the highest rates of homelessness (~8%), likely due to insufficient care and government support."),
            tags$li("Those diagnosed with anxiety, ODD, PDD, personality disorders, and substance abuse disorders frequently have co-occurring disorders, indicating overlapping symptoms."),
            tags$li("Dementia/Delirium patients are most often widowed or divorced, likely due to age. Bipolar and personality disorder patients also show high rates of divorce or widowhood, suggesting relationship difficulties linked to these disorders.")
          )
        )
      )
    )
  )
)


shinyApp(ui = ui, server = server)