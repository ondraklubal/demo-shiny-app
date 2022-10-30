library(ggplot2)
library(dplyr)
library(data.table)
library(reactable)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

#browser()

data <- read.csv("./www/who_suicide_statistics.csv", header = TRUE, sep = ",")


# UI -----------------------

ui <- dashboardPage(
    
    # Header of the app
    dashboardHeader(title = "Demo appka"),
    
    # Sidebar of the app
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Suicide mortality", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Další dashboard", tabName = "next_one", icon = icon("widget"))
      )
      
    ),
    
    # Body of the app
    dashboardBody(
      
      # Add your own shiny style
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
      
      tabItems(
        tabItem(tabName = "dashboard",
                
                fluidRow(
                  column(6,
                         p(strong("Suicide mortality in the US"), style = "font-size: 20px; color: black"),
                         tags$div(class = "header", checked = NA,
                                  tags$a(href = "https://www.kaggle.com/datasets/szamil/who-suicide-statistics?resource=download", "Link to the original dataset here."))),
                  column(2, offset = 4,
                         downloadButton("downloadData", label = "Download data"))
                ),
                
                tags$br(),
                
                fluidRow(
                  box(sliderInput("slider_years", label = "Range of years", 
                                  step = 1, sep = "", ticks = FALSE,
                                  min  = min(data$year), max = max(data$year), 
                                  value = c(min(data$year)+5, max(data$year)-5)), width = 4),
                  
                  box(pickerInput("states", label = "States", choices = unique(data[[1]]), selected = data[1,1], multiple = TRUE, 
                                  options = list(
                                    `actions-box` = TRUE, 
                                    size = 10,
                                    `selected-text-format` = "count > 3")
                                  ), width = 4),
                  
                  
                  box(checkboxGroupInput("sex", label = "Sex", choices = unique(data[[3]]), selected = NULL), width = 4)
                  
                ),
                
                fluidRow(
                  box(plotOutput("distPlot", height = 350)),
                  box(reactableOutput("table"))
                )
        ),
        tabItem(tabName = "next_one",
                h3("Nadpis další")
                )
      )
    )
)

# SERVER -----------------------

server <- function(input, output) {
  
  
    df_plot <- reactive({
      df_plot <- data[data$year %in% input$slider_years[1]:input$slider_years[2] & data$country == input$states, ]
      
      if(is.null(input$sex) == FALSE)
        df_plot <- df_plot[df_plot$sex == input$sex, ]
      
      #df_plot <- na.omit(df_plot)
      #browser() 
      #setDT(df_plot)
      
      df_plot <- as.data.frame(unclass(df_plot), stringsAsFactors = TRUE)
      
      #browser()
      
      df_plot <- df_plot %>% 
        group_by(year,sex) %>% 
        summarise(suicides_no = sum(suicides_no, na.rm = T))
    })
  
    output$distPlot <- renderPlot({
      
      ggplot(df_plot(), aes(x = year, y = suicides_no, group = sex, color = sex))+ 
        geom_line() +
        geom_point()+
        labs(title="Suicides by year and sex", x ="Year", y = "No of suicides")+
        theme_minimal()
        
    })
    
    output$table <- renderReactable({
      
      reactable(df_plot(), 
                sortable = TRUE, filterable = TRUE, height = 350, defaultPageSize = 5,
                defaultColDef = colDef(align = "center")
              )
      
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data.csv", sep = "")
      },
      content = function(file) {
        write.csv(df_plot(), file, row.names = FALSE)
      }
    )
}

shinyApp(ui, server)
