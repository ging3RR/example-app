options(shiny.maxRequestSize = 30*1024^2) #increse the file size to limit 30MB

library(shiny)
library(shinyWidgets)
library(here)
library(tidyverse)
library(ggthemes)
library(shinythemes)

#load in data
#europe <-  readRDS(here::here("data/europe.rds"))

# europe <- europe %>% 
#     mutate(AvgTemperatureC = round(((AvgTemperatureF-32)*(5/9)),2))





#outputs from nicolas








# Define UI for application that draws a histogram
ui <- fluidPage(
    
    #theme of the page
    theme = shinytheme("journal"),
    
    # Application title
    titlePanel("Shiny Project"),
    
    h3("This is a R Shiny Project"), 
    
    # Sidebar ----
    sidebarLayout(
        sidebarPanel(
            
            "This is the Sidebar",
            
            sliderInput(inputId = "year", 
                        label = "Year", 
                        min = 2000,
                        max = 2019,
                        value = 2000,
                        step = 1,
                        sep = ""),
            
            selectInput(inputId = "country",
                        label = "Country",
                        choices = ""),#c(unique(europe$Country))),
            
            selectInput(inputId = "city",
                        label = "City",
                        choices = ""), #c(unique(europe$City))),
            
            textInput(inputId = "text_input", label = "Input text here"),
            
            radioButtons(inputId = "temp_scale",
                         label = "Choose F or C",
                         choices = list("Fahrenheit" = "Fahrenheit", "Celsius" = "Celsius"),
                         selected = "Celsius"),
            
            actionButton(inputId = "button", label = "Calculate!"),
            
            fileInput(inputId = "file", label = "Upload a file (RDS)",
                      multiple = FALSE, accept = c(".rds")),
            
            downloadButton(outputId = "download", label = "Download your file")
            
            
            
            
        ),
        
        # main panel ----
        mainPanel(
            "This is the Main Panel",
            
            textOutput(outputId = "all_output"),
            
            tabsetPanel(type = "tabs",
                        tabPanel(title = "Info",
                                 h3("App description"),
                                 p("This is a demo shiny app. We created this in the R Shiny Workshop.", br(),
                                   "it uses a data set with:", br(),
                                   strong("temperature"), "(in Fahrenheit or Celsius)", br(),
                                   "choose between", strong("cities, countries and year")), 
                                 hr(),
                                 
                                 verbatimTextOutput("summary")),
                        tabPanel(title ="Data",
                                 dataTableOutput("table")),
                        tabPanel(title ="Plots", 
                                 fluidRow(
                                     column(width = 6, plotOutput("barplot")),
                                     column(width = 6, plotOutput("barplot2"))
                                 )
                                 
                        )
                        
            )
        )
    )
)



# Server----
server <- function(input, output, session) {
    
    europe <- eventReactive(input$file, {
        readRDS(input$file$datapath) %>% 
            mutate(AvgTemperatureC = round(((AvgTemperatureF-32)*(5/9)),2))
    })
    
    europe_avgtempyearch <- reactive({
        europe() %>% 
            filter(Country == input$country) %>% 
            group_by(Country, City, Year) %>% 
            summarize(Temp_Grad_mean = mean(AvgTemperatureC), 
                      Tep_fahrenheit_mean = mean(AvgTemperatureF))
        
    })
    
    
    country_df <- eventReactive(input$button,{
        europe() %>%
            filter(Year == input$year) %>% 
            filter(Country == input$country)
    })
    
    
    city_df <- reactive({
        country_df() %>% 
            filter(City == input$city) %>% 
            filter(Year == input$year)
    })
    
    year_df <- reactive({
        country_df() %>% 
            filter(City == input$city) %>% # Subset the rows for specific City
            filter(Year == input$year) %>%  # Subset the rows for specific Year
            group_by(Country, City, Year, Month) %>% 
            summarise(MinTempF = min(AvgTemperatureF),
                      MeanTempF = round(mean(AvgTemperatureF), 1),
                      MaxTempF = max(AvgTemperatureF),
                      MinTempC = min(AvgTemperatureC),
                      MeanTempC = round(mean(AvgTemperatureC), 1),
                      MaxTempC = max(AvgTemperatureC)) %>% 
            ungroup()
    })
    
    output$all_output <- renderText({
        paste("Your inputs are:",input$year, input$country, input$city, input$text_input, input$temp_scale)
    })
    
    output$barplot <- renderPlot({
        ggplot(data = europe_avgtempyearch(), mapping = aes(x = Year, y = Temp_Grad_mean, fill = City)) +
            geom_bar(stat = "identity", position = "dodge") + 
            scale_fill_hue(c=45, l=80) +
            scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#128721")) +
            ylab("Average Temp") + theme_tufte() + ggtitle("Average Temperatur per Year and City")
    })
    output$summary <- renderPrint({
        summary(europe())
    })
    
    output$table <- renderDataTable({
        year_df()
    })
    output$barplot2 <- renderPlot({
        ggplot(data = year_df()) +
            geom_line(mapping = aes(x = Month, y = MinTempC), size = 1, colour = "red", linetype = "dotted") +
            geom_line(mapping = aes(x = Month, y = MeanTempC), size = 1, colour = "black") +
            geom_line(mapping = aes(x = Month, y = MaxTempC), size = 1, colour = "red", linetype = "dotted") +
            scale_x_discrete(name = "", limits = month.abb) +
            ylab("Average daily temperatures (in Celsius)")
    })
    
    observe({
        choices_country <- unique(europe()$Country)
        updateSelectInput(session, inputId = "country", choices = choices_country)
    })
    
    observe({
        choices_country <- unique(europe()$City[europe()$Country == input$country])
        updateSelectInput(session, inputId = "city", choices = choices_country)
    })
    
    output$download <- downloadHandler(
        filename = "city_data.csv",
        content = function(file){
            write.csv2(city_df(), file = file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
