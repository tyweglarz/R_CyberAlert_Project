library(dplyr)
library(stringr)
library(tidyverse)
library(shiny)
library(shinyjs)

attackInfo <- read.csv("~/Desktop/R_Project/20200204_1025_repot_job_256.csv",
                       stringsAsFactors = FALSE)
newAttackInfo <- attackInfo %>% 
    dplyr::mutate(dayOfWeek.Received = str_extract(Day.Received, "^.{0,3}"))
print(str(newAttackInfo))

countryInput <- select(newAttackInfo, Source.Country)
dayInput <- select(newAttackInfo, dayOfWeek.Received)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage (

    # Application title
    titlePanel("Data analysis of threat logs from AgConnections"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("countryInput", "Country",
                        choices = c("United States", "China", "Russian Federation", "Italy", "Egypt"), selected = "United States"),
            selectInput("dayInput", "Day of Week",
                        choices = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
            ),
        #select input day of week 
        # # of threats on left matching input day of week and source country
        # only one country and 5 bars of threat type and count on the bottom?
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("coolplot"),
            br(), br(),
           tableOutput("results")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$coolplot <- renderPlot({
        filtered <- 
            newAttackInfo %>% 
            filter(Source.Country == input$countryInput,
                   dayOfWeek.Received == input$dayInput )
    ggplot(filtered, aes(Threat.Category, fill=I("blue"), col=I("black"))) +
        geom_bar(width = 0.3) +
        theme_classic() +
        ggtitle("Total Cyber Threats by Type") +
        xlab("Type of Threat") +
        geom_text(stat='count',aes(label=..count..),vjust=-1) +
        ylab("Total")
})
}
shinyApp(ui = ui, server = server)
