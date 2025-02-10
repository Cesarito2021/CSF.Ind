# ******************************************************************************
# This is CSF-Ind web-based app 
# ******************************************************************************
#' @name UI for CSF-Ind
#' @title UI for Climate Smart Forestry Indicator Generator
#'
#' @description This function defines the user interface (UI) for the Climate Smart Forestry (CSF) Indicator Generator.
#' It provides an interactive dashboard for processing adaptation and mitigation pillars.
#'
#' @param data A `data.frame` containing forest inventory data.
#' @param input A list containing column names and parameters required for processing.
#'
#' @return A processed dataset with calculated forest diversity metrics.
#'
#' @import dplyr 
#' @import shiny shinydashboard bslib DT ggplot2 GGally openxlsx ggpubr tidyverse rmarkdown gridExtra 
#' @import shinyscreenshot rnaturalearth sf tinytex kableExtra reshape2 vegan DescTools ForestStandMetrics
#-----------------------------------------------------------------------------
#                   Library
#-----------------------------------------------------------------------------
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(bslib))
suppressMessages(library(DT))
suppressMessages(library(ggplot2))
suppressMessages(library(GGally))
suppressMessages(library(openxlsx))
suppressMessages(library(ggpubr))
suppressMessages(library(tidyverse))
suppressMessages(library(rmarkdown))
suppressMessages(library(gridExtra))
suppressMessages(library(shinyscreenshot))
suppressMessages(library(rnaturalearth))
suppressMessages(library(sf))
suppressMessages(library(tinytex))
suppressMessages(library(kableExtra))
suppressMessages(library(reshape2))
suppressMessages(library(vegan))
suppressMessages(library(DescTools))
options(shiny.maxRequestSize=100*(1024*1024))
#-----------------------------------------------------------------------------
# Countries
#-----------------------------------------------------------------------------
european_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", 
                        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                        "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania", 
                        "Luxembourg", "Malta", "Netherlands", "Norway", "Poland", 
                        "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", 
                        "Switzerland", "United Kingdom")  # Add/adjust as needed
#-----------------------------------------------------------------------------
# Ui
#-----------------------------------------------------------------------------
ui <- dashboardPage(
  # Shiny App UI data
  dashboardHeader(title = div(
    style = "font-size: 24px; font-family: 'Brandon Grotesque'; font-weight: bold;",
    HTML(paste0(
      '<span style="color: #6FBC85;" >CSF-Ind</span>'#, 
    ))
  )),
  dashboardSidebar(
    div(style = "display: flex; align-items: center; justify-content: flex-start; padding: 10px;",
        a(href = "#Home",
          img(src = "https://www3.unimol.it/assets/images/unimol/images/header/unimol_on.svg", height = "80px", style = "margin-right: 10px; margin-left: 10px;")
        ),
        a(href = "#Home",
          img(src = "https://forwards-project.eu/wp-content/themes/forwards_theme/images/FWD-LOGO-RGB_MAIN_COLOR.svg", height = "80px", style = "margin-left: 5px;")
        )
    ),
    sidebarMenu(
      tags$div(
        style = "padding: 12px; font-size: 16px;font-family: 'Brandon Grotesque'; font-weight: bold;color:#003D39;",
        "CSF concept"
      ),
      menuItem(paste0("Mitigation"), tabName =paste0("Mitigation"), icon = icon("ok", lib = "glyphicon")),
      menuItem(paste0("Adaptation"), tabName =paste0("Adaptation"), icon = icon("ok", lib = "glyphicon")),
      menuItem(paste0("Social_Dimension"), tabName =paste0("Social_Dimension"), icon = icon("ok", lib = "glyphicon")),
      menuItem(paste0("CSF_Composite_Assessment"), tabName =paste0("CSF_Composite_Performance_Assessment"), icon = icon("menu-hamburger", lib = "glyphicon"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/css_app.css")
    ),
    # *********************************
    #  First Page
    # *********************************
    tabItems(
      tabItem(tabName = "Mitigation",
              fluidPage(
                titlePanel(
                  div(
                    style = "font-size: 30px; font-family: 'Brandon Grotesque'; font-weight: bold;",
                    HTML(paste0(
                      '<span style="color:#f9f6f6;">CSF - Mitigation Pillar </span>'
                    ))
                  )
                ),
                sidebarLayout(
                  sidebarPanel(
                    div(
                      style = "margin-bottom: -10px;", 
                      fileInput("file","Upload Excel file (.xlsx)",buttonLabel = "Choose File")
                    ),
                    selectInput("country", "Select EU country or Europe for analysis", choices = european_countries, selected = "Italy"),
                    conditionalPanel(
                      condition = "input.country == 'Italy'",
                      selectInput("model_1", label = "Select CSF indicator to analyse", 
                                  choices = c("1.1_GrowingStock","1.2_CarbonStock", "1.3_LyingDeadwood","1.4_StandingDeadwood","1.5_AllDeadwood"), 
                                  selected = "1.2_CarbonStock"),
                      # *********************************************************
                      # ************************  1  ****************************
                      # *********************************************************
                      conditionalPanel(
                        condition = "input.model_1 == '1.1_GrowingStock'",
                        hr(),
                        radioButtons(
                          inputId = "I1_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I1_Provide_ForManInt == 'Yes'",
                          selectInput("I1_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I1_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I1_Provide_plot == 'Yes'",
                          selectInput("I1_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        sliderInput("I1_plot_size_col", "Configurate Plot Area (m2):", min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out"),
                        selectInput("I1_height_col", "Select Tree Height (m) Column", ""),
                        selectInput("I1_dbh_col", "Select Tree Diameter at 1.3m Column", ""),
                        selectInput("I1_specie_col", "Select Tree Species Column", ""),
                        hr()
                      ),
                      # *********************************************************
                      # ************************  2  ****************************
                      # *********************************************************
                      conditionalPanel(
                        condition = "input.model_1 == '1.2_CarbonStock'",
                        hr(),
                        radioButtons(
                          inputId = "I2_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I2_Provide_ForManInt == 'Yes'",
                          selectInput("I2_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I2_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I2_Provide_plot == 'Yes'",
                          selectInput("I2_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        selectInput("I2_dom_col", "Select Dominant Tree Species Column",choices = NULL),
                        selectInput("I2_vol_col", "Select Stand Volume (m3/ha) Column", ""),
                        hr()
                      ),
                      # *********************************************************
                      # ************************   3 ****************************
                      # *********************************************************
                      conditionalPanel(
                        condition = "input.model_1 == '1.3_LyingDeadwood'",
                        hr(),
                        radioButtons(
                          inputId = "I3_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I3_Provide_ForManInt == 'Yes'",
                          selectInput("I3_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I3_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I3_Provide_plot == 'Yes'",
                          selectInput("I3_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I3_LDT_CWD_option",
                          label = "Will you calculate LDT or CWD ?",
                          choices = list("ldt" = "ldt", "cwd" = "cwd"),
                          selected = "ldt"
                        ),
                        conditionalPanel(
                          condition = "input.I3_LDT_CWD_option == 'ldt'",
                          selectInput("I3_TH_tot_col", "Select Tree Height (m) Column", ""),
                          selectInput("I3_DBH_col", "Select Tree Diameter at 1.3m (cm) Column", "")
                        ),
                        conditionalPanel(
                          condition = "input.I3_LDT_CWD_option == 'cwd'",
                          selectInput("I3_L_tot_col", "Select Height or Length (m) Column", ""),
                          selectInput("I3_Dhalf_col", "Select Diameter at half-length (cm) Column", "")
                        ),
                        sliderInput("I3_plot_size_col", "Configurate Plot Area (m2):", min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out")
                      ),
                      # *********************************************************
                      # ************************   4 ****************************
                      # *********************************************************
                      conditionalPanel(
                        condition = "input.model_1 == '1.4_StandingDeadwood'",
                        hr(),
                        radioButtons(
                          inputId = "I4_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I4_Provide_ForManInt == 'Yes'",
                          selectInput("I4_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I4_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I4_Provide_plot == 'Yes'",
                          selectInput("I4_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I4_SDT_SNAG_option",
                          label = "Will you calculate LDT or CWD ?",
                          choices = list("sdt" = "sdt", "snag" = "snag"),
                          selected = "sdt"
                        ),
                        conditionalPanel(
                          condition = "input.I4_SDT_SNAG_option == 'sdt'",
                          selectInput("I4_TH_tot_col", "Select Tree Height (m) Column", ""),
                          selectInput("I4_DBH_col", "Select Tree Diameter at 1.3m (cm) Column", "")
                        ),
                        conditionalPanel(
                          condition = "input.I4_SDT_SNAG_option == 'snag'",
                          selectInput("I4_L_tot_col", "Select Height or Length  (m) Column", ""),
                          selectInput("I4_Dhalf_col", "Select Diameter at half-length (cm) Column", "")
                        ),
                        sliderInput("I4_plot_size_col", "Configurate Plot Area (m2):", min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out")
                      ),
                      # *********************************************************
                      # ************************   4 ****************************
                      # *********************************************************
                      conditionalPanel(
                        condition = "input.model_1 == '1.5_AllDeadwood'",
                        hr(),
                        radioButtons(
                          inputId = "I5_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I5_Provide_ForManInt == 'Yes'",
                          selectInput("I5_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I5_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I5_Provide_plot == 'Yes'",
                          selectInput("I5_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        sliderInput("I5_plot_size_col", "Configurate Plot Area (m2):", min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out"),
                        selectInput("I5_L_col", "Select Height or Length (m) Column", ""),
                        selectInput("I5_Dmax_col", "Select Maximum Diameter (cm) Column", ""),
                        selectInput("I5_Dmin_col", "Select Minimum Diameter  (cm) Column", ""),
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "input.country == 'Italy'",
                      downloadButton("downloadData_Miti_ForMan", "Download Table ForManInt (.xlsx)", icon("download"), 
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      downloadButton("downloadData_Miti_Plot", "Download Table Plot (.xlsx)", icon("download"), 
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      downloadButton("downloadReportPDF_1", "Download Report (.pdf)", icon("download"),
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      actionButton("reset", "Reset", icon("rotate-left"),
                                   style = "color: #fff; background-color: #da275d; border-color: white;width:100%; font-size:40")
                      
                    ),
                    style = "width: 300px;"
                  ),
                  mainPanel(
                    fluidRow(
                      box(title = "Background", color= "olive", solidHeader = TRUE,
                          collapsible = TRUE,background = "olive",imageOutput("CSF1", height = 200)),
                      box(title = "Country Selected", color= "olive", solidHeader = TRUE,
                          collapsible = TRUE,background = "olive",plotOutput("mapEU1", height = 200))
                    ) ,
                    DTOutput("data_summary_1"),
                    plotOutput("bar_boxplot_1"),
                    conditionalPanel(
                      condition = "output.piechartAvailable_1 == true",
                      plotOutput("pie_charts_1",height = "500px")
                    ),
                    conditionalPanel(
                      condition = "output.heatmapAvailable_1 == true",
                      plotOutput("heatmap_1",height = "500px")
                    )
                  )
                )
              )
      ),
      # *********************************
      #  Second Page
      # *********************************
      tabItem(tabName = "Adaptation",
              fluidPage(
                titlePanel(
                  div(
                    style = "font-size: 30px; font-family: 'Brandon Grotesque'; font-weight: bold;",
                    HTML(paste0(
                      '<span style="color:#f9f6f6;">CSF - Adaptation Pillar </span>'
                    ))
                  )
                ),
                sidebarLayout(
                  sidebarPanel(
                    div(
                      style = "margin-bottom: -10px;", 
                      fileInput("file", "Upload Excel file (.xlsx)",buttonLabel = "Choose File")
                    ),
                    selectInput("country", "Select country for report location", choices = european_countries, selected = "Italy"),
                    conditionalPanel(
                      condition = "input.country == 'Italy'",
                      selectInput("model_2", label = "Select CSF indicator to analyse", 
                                  choices = c("2.1_ForestDiversity","2.2_AdvancedForestDiversity"), 
                                  selected = "2.1_ForestDiversity"),
                      conditionalPanel(
                        condition = "input.model_2 == '2.1_ForestDiversity'",
                        hr(),
                        radioButtons(
                          inputId = "I51_Provide_ForManInt",
                          label = "Will you provide ForManInt information?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I51_Provide_ForManInt == 'Yes'",
                          selectInput("I51_ForManInt_col", "Select ForManInt Column", "")
                        ),
                        hr(),
                        radioButtons(
                          inputId = "I51_Provide_plot",
                          label = "Will you conduct a multi-plot analysis?",
                          choices = list("Yes" = "Yes", "No" = "No"),
                          selected = "No"
                        ),
                        conditionalPanel(
                          condition = "input.I51_Provide_plot == 'Yes'",
                          selectInput("I51_plot_col", "Select Plot ID Column", "")
                        ),
                        hr(),
                        sliderInput("I51_plot_size_col", "Configurate Plot Area (m2):", min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out"),
                        selectInput("I51_height_col", "Select Tree Height (m) Column", ""),
                        selectInput("I51_dbh_col", "Select Tree Diameter at 1.3m(cm) Column", ""),
                        selectInput("I51_specie_col", "Select Tree Species Column", ""),
                        hr()
                      ),
                      conditionalPanel(
                        condition = "input.model_2 == '2.2_AdvancedForestDiversity'",
                        hr(),
                        selectInput("I6_ForManInt_col", "Select ForManInt Column", ""),
                        hr(),
                        sliderInput("I6_plot_size_col", "Configurate Plot Area (m2):",min = 200, max = 1000, value = 530, step = 10),
                        verbatimTextOutput("Slider1Out"),
                        selectInput("I6_plot_col", "Select Plot ID Column", choices = NULL),
                        selectInput("I6_dbh_col", "Select Tree Diameter (cm) Column", ""),
                        selectInput("I6_height_col", "Select Tree Height (m) Column", ""),
                        selectInput("I6_ba_col", "Select Basal area (m2) Column", ""),
                        selectInput("I6_specie_col", "Select Tree Species Column", ""),
                        hr()
                      )
                    ),
                    conditionalPanel(
                      condition = "input.country == 'Italy'",
                      downloadButton("downloadData_Adap_ForMan", "Download Table (.xlsx)", icon("download"), 
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      downloadButton("downloadData_Adap_Plot", "Download Table (.xlsx)", icon("download"), 
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      downloadButton("downloadReportPDF_2", "Download Report (.pdf)", icon("download"),
                                     style = "color: #fff; background-color: #00A19A; border-color: white;width:100%; font-size:40"),
                      actionButton("reset", "Reset", icon("rotate-left"),
                                   style = "color: #fff; background-color: #da275d; border-color: white;width:100%; font-size:40")
                    ),
                    style = "width: 300px;"
                  ),
                  mainPanel(#style = "padding: 0px 0px; margin-top:0em margin-left:0em",
                    fluidRow(
                      box(title = "Background", color= "olive", solidHeader = TRUE,
                          collapsible = TRUE,background = "olive",imageOutput("CSF2", height = 200)),
                      box(title = "Country Selected", color= "olive", solidHeader = TRUE,
                          collapsible = TRUE,background = "olive",plotOutput("mapEU2", height = 200))
                    ) ,
                    #
                    #
                    DTOutput("data_summary_2"),
                    plotOutput("bar_boxplot_2"),
                    conditionalPanel(
                      condition = "output.heatmapAvailable_2 == true",
                      plotOutput("heatmap_2",height = "500px")
                    )
                  )
                )
              )
      ),
      # *********************************
      #  Third Page
      # *********************************
      tabItem(tabName = "Social_Dimension",
              fluidPage(
                titlePanel(
                  div(
                    style = "font-size: 30px; font-family: 'Brandon Grotesque'; font-weight: bold;",
                    HTML(paste0(
                      '<span style="color:#f9f6f6;">CSF - Social Dimension Pillar </span>'
                    ))
                  )
                ))
      ),
      # *********************************
      #  Fourth
      # *********************************
      tabItem(tabName = "CSF_Composite_Performance_Assessment",
              fluidPage(
                titlePanel(
                  div(
                    style = "font-size: 30px; font-family: 'Brandon Grotesque'; font-weight: bold;",
                    HTML(paste0(
                      '<span style="color:#f9f6f6;">CSF Composite Performance Assessment</span>'
                    ))
                  )
                ))
      )
      # *********************************
      #  To be continue
      # *********************************
    )
  )
) 

#' @name Server for CSF-Ind
#' @title Server for Climate Smart Forestry Indicator Generator
#'
#' @description This function defines the server  for the Climate Smart Forestry (CSF) Indicator Generator.
#' It provides an interactive dashboard for processing adaptation and mitigation pillars.
#'
#' @param data A `data.frame` containing forest inventory data.
#' @param input A list containing column names and parameters required for processing.
#'
#' @return A processed dataset with calculated forest diversity metrics.
#'
#' @import dplyr 
#' @import shiny shinydashboard bslib DT ggplot2 GGally openxlsx ggpubr tidyverse rmarkdown gridExtra 
#' @import shinyscreenshot rnaturalearth sf tinytex kableExtra reshape2 vegan DescTools ForestStandMetrics

#-----------------------------------------------------------------------------
# Server
#-----------------------------------------------------------------------------

server <- function(input, output, session) {
  # **************************************
  #        CSF logo
  # ************************************** 
  output$CSF1 <- renderImage({
    # Return a list containing the filename
    list(src = 'C:/Users/calvi/OneDrive/Desktop/App_Forwards/R/Logo/CSF-ciclo1.png',
         width = "100%",
         height = "100%",
         alt = "Chart of good stuff")
  }, deleteFile = FALSE)
  output$CSF2 <- renderImage({
    # Return a list containing the filename
    list(src = 'C:/Users/calvi/OneDrive/Desktop/App_Forwards/R/Logo/CSF-ciclo2.png',
         width = "100%",
         height = "100%",
         alt = "Chart of good stuff")
  }, deleteFile = FALSE)
  # **************************************
  #        EU map
  # ************************************** 
  output$mapEU1 <- renderPlot({
    selected_country <- input$country
    world <- ne_countries(scale = 50, returnclass = "sf")
    map1 <- plot_map(world, selected_country)
    vals$map1 <- map1
    print(map1)
  })
  output$mapEU2 <- renderPlot({
    selected_country <- input$country
    world <- ne_countries(scale = 50, returnclass = "sf")
    map2 <- plot_map(world, selected_country)
    vals$map2 <- map2
    print(map2)
  })
  # **************************************
  #        Main Reactive Functions
  # **************************************
  
  data_processing_1 <- reactive({
    data <- load_data(input$file$datapath)
    
    switch(input$model_1,
           "1.1_GrowingStock" = process_growing_stock(data, input),
           "1.2_CarbonStock" = process_carbon_stock(data, input),
           "1.3_LyingDeadwood" = process_lying_deadwood(data, input),
           "1.4_StandingDeadwood" = process_standing_deadwood(data, input),
           "1.5_AllDeadwood" = process_all_deadwood(data, input)
    )
  })
  data_processing_2 <- reactive({
    data <- load_data(input$file$datapath)
    
    if (input$model_2 == "2.1_ForestDiversity") {
      process_forest_diversity(data, input)
    }
  })
  # **************************************
  #        Show 10 rows in Shiny
  # ************************************** 
  vals <- reactiveValues()
  output$data_summary_1 <- renderDT({
    data <- data_processing_1()
    if (is.null(data)) {
      return(NULL)
    }
    # 
    vals$data_summary_1 <- datatable(data[[2]],rownames = FALSE,
                                     options = list(
                                       columnDefs = list(list(className = 'dt-center', targets = '_all'))
                                     ))
    vals$data_summary_1
  })
  output$data_summary_2 <- renderDT({
    data <- data_processing_2()
    if (is.null(data)) {
      return(NULL)
    }
    # 
    vals$data_summary_2 <- datatable(data[[2]],rownames = FALSE,
                                     options = list(
                                       columnDefs = list(list(className = 'dt-center', targets = '_all'))
                                     ))
    vals$data_summary_2
  })
  # **************************************
  #        Boxplot graph
  # **************************************
  # Render plot for data_processing_1
  output$bar_boxplot_1 <- renderPlot({
    data <- data_processing_1()
    if (is.null(data)) {
      return(NULL)
    }
    plot_data <- data[[2]]  # Extract the relevant part of the data
    plot <- generate_plot(plot_data, "Boxplot of Numeric Variables (Model 1)")
    print(plot)
    vals$bar_boxplot_1 <- plot
  }, bg = "#003D39")
  # Render plot for data_processing_2
  output$bar_boxplot_2 <- renderPlot({
    data <- data_processing_2()
    if (is.null(data)) {
      return(NULL)
    }
    plot_data <- data[[2]]  # Extract the relevant part of the data
    plot <- generate_plot(plot_data, "Boxplot of Numeric Variables (Model 2)")
    print(plot)
    vals$bar_boxplot_2 <- plot
  }, bg = "#003D39")
  # **************************************
  #        Heatmap graph
  # **************************************
  # Setting data
  is_heatmap_available <- function(data) {
    if (is.null(data)) {
      return(FALSE)
    }
    
    numeric_data <- data %>%
      select_if(is.numeric) %>%
      select(!matches("Ads|Id|plot")) %>%
      drop_na()
    
    return(nrow(numeric_data) > 1)  # Heatmap is available if there are multiple numeric rows
  }
  # Generate heatmap
  output$heatmapAvailable_1 <- reactive({
    data <- data_processing_1()
    data <- data[[2]]
    is_heatmap_available(data)
  })
  output$heatmapAvailable_2 <- reactive({
    data <- data_processing_2()
    data <- data[[2]]
    is_heatmap_available(data)
  })
  # Render heatmap
  output$heatmap_1 <- renderPlot({
    data <- data_processing_1()
    data <- data[[1]]
    heatmap <- generate_heatmap(data, "Heatmap of Pearson correlation coefficient matrix (Model 1)")
    vals$heatmap_1 <- heatmap  # Store the heatmap in vals
    suppressMessages(print(heatmap))
  }, bg = "white")
  output$heatmap_2 <- renderPlot({
    data <- data_processing_2()
    data <- data[[2]]
    heatmap <- generate_heatmap(data, "Heatmap of Pearson correlation coefficient matrix (Model 2)")
    vals$heatmap_2 <- heatmap  # Store the heatmap in vals
    suppressMessages(print(heatmap))
  }, bg = "white")
  # check heatmaps on backend
  outputOptions(output, "heatmapAvailable_1", suspendWhenHidden = FALSE)
  outputOptions(output, "heatmapAvailable_2", suspendWhenHidden = FALSE)
  ## **************************************
  ##        Pie-Chart 
  ## **************************************
  # Setting data
  is_pie_chart_available <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
      return(FALSE)
    }
    
    categorical_data <- data %>%
      mutate_if(is.character, as.factor) %>%
      select_if(is.factor) %>%
      select(!matches("Ads|Id|plot"))
    
    return(ncol(categorical_data) > 0)  # Pie chart is available if there are categorical columns
  }
  # generate pie-chart
  output$piechartAvailable_1 <- reactive({
    data <- data_processing_1()
    data <- data[[2]]
    is_pie_chart_available(data)
  })
  # Render pie-chart
  output$pie_charts_1 <- renderPlot({
    data <- data_processing_1()
    data <- data[[2]]
    pie_charts <- generate_pie_charts(data)
    vals$pie_charts_1 <- pie_charts  # Store the pie charts in vals
    print(pie_charts)
  }, bg = "white")
  # check piechart on backend
  outputOptions(output, "piechartAvailable_1", suspendWhenHidden = FALSE)
  ## **************************************
  ##        Downloaded file function
  ## **************************************
  output$downloadData_Miti_ForMan <- downloadHandler( 
    filename = function() {"OutputData_CSF_Mitigation_ForManInt.xlsx"},
    content = function(file) {
      data <- data_processing_1()
      data <- data[[2]]
      if (!is.null(data)) {
        write.xlsx(data, file)
      }
    })
  output$downloadData_Miti_Plot <- downloadHandler( 
    filename = function() {"OutputData_CSF_Mitigation_Plot.xlsx"},
    content = function(file) {
      data <- data_processing_1()
      data <- data[[1]]
      if (!is.null(data)) {
        write.xlsx(data, file)
      }
    })
  #
  output$downloadData_Adap_ForMan <- downloadHandler(
    filename = function() {"OutputData_Adaptation_ForManInt.xlsx"},
    content = function(file) {
      data <- data_processing_2()
      data <- data[[2]]
      if (!is.null(data)) {
        write.xlsx(data, file)
      }
    })
  #
  output$downloadData_Adap_Plot <- downloadHandler(
    filename = function() {"OutputData_Adaptation_Plot.xlsx"},
    content = function(file) {
      data <- data_processing_2()
      data <- data[[1]]
      if (!is.null(data)) {
        write.xlsx(data, file)
      }
    })
  # **************************************
  #        Download Report data (.pdf)
  # **************************************
  vals <- reactiveValues(
    data_summary_1 = NULL,
    heatmap_1 = NULL,
    pie_charts_1 = NULL,
    bar_boxplot_1 = NULL,
    map1 = NULL,
    
    data_summary_2 = NULL,
    heatmap_2 = NULL,
    bar_boxplot_2 = NULL,
    map2 = NULL
  )
  #
  output$downloadReportPDF_1 <- downloadHandler(
    filename = function() { "Report_CSF_Mitigation.pdf" },
    content = function(file) {
      generate_report(
        file = file,
        type = "mitigation",
        data_processing_function = data_processing_1,
        vals = vals
      )
    }
  )
  # **************************************
  output$downloadReportPDF_2 <- downloadHandler(
    filename = function() { "Report_CSF_Adaptation.pdf" },
    content = function(file) {
      generate_report(
        file = file,
        type = "adaptation",
        data_processing_function = data_processing_2,
        vals = vals
      )
    }
  )
  # **************************************
  #        Selection Input data option
  # **************************************
  observe({
    req(input$file)
    data <- openxlsx::read.xlsx(input$file$datapath)
    # **************************************************************************
    # Mitigation pillar
    # **************************************************************************
    updateSelectInput(session, "I1_plot_col", choices = names(data))
    updateSelectInput(session, "I1_height_col", choices = names(data))
    updateSelectInput(session, "I1_dbh_col", choices = names(data))
    updateSelectInput(session, "I1_specie_col", choices = names(data))
    updateSelectInput(session, "I1_ForManInt_col", choices = names(data))
    # 
    updateSelectInput(session, "I2_plot_col", choices = names(data))
    updateSelectInput(session, "I2_dom_col", choices = names(data))
    updateSelectInput(session, "I2_vol_col", choices = names(data))
    updateSelectInput(session, "I2_ForManInt_col", choices = names(data))
    #
    updateSelectInput(session, "I3_plot_size_col", choices = names(data))
    updateSelectInput(session, "I3_plot_col", choices = names(data))
    updateSelectInput(session, "I3_TH_tot_col", choices = names(data))
    updateSelectInput(session, "I3_DBH_col", choices = names(data))
    updateSelectInput(session, "I3_L_tot_col", choices = names(data))
    updateSelectInput(session, "I3_Dhalf_col", choices = names(data))
    updateSelectInput(session, "I3_ForManInt_col", choices = names(data))
    # 
    updateSelectInput(session, "I4_plot_size_col", choices = names(data))
    updateSelectInput(session, "I4_plot_col", choices = names(data))
    updateSelectInput(session, "I4_TH_tot_col", choices = names(data))
    updateSelectInput(session, "I4_DBH_col", choices = names(data))
    updateSelectInput(session, "I4_L_tot_col", choices = names(data))
    updateSelectInput(session, "I4_Dhalf_col", choices = names(data))
    updateSelectInput(session, "I4_ForManInt_col", choices = names(data))
    # 
    updateSelectInput(session, "I5_ForManInt_col", choices = names(data))
    updateSelectInput(session, "I5_plot_col", choices = names(data))
    updateSelectInput(session, "I5_L_col", choices = names(data))
    updateSelectInput(session, "I5_Dmax_col", choices = names(data))
    updateSelectInput(session, "I5_Dmin_col", choices = names(data))
    # **************************************************************************
    # Adaptation pillar
    # **************************************************************************
    updateSelectInput(session, "I51_plot_col", choices = names(data))
    updateSelectInput(session, "I51_height_col", choices = names(data))
    updateSelectInput(session, "I51_dbh_col", choices = names(data))
    updateSelectInput(session, "I51_specie_col", choices = names(data))
    updateSelectInput(session, "I51_ForManInt_col", choices = names(data))
    
    
  })
  # **************************************
  #        Reset
  # ************************************** 
  observeEvent(input$reset, {
    session$reload()
  })
}

#-----------------------------------------------------------------------------
# End
#-----------------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1300))
