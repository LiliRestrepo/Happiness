library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

happy2 <- read.csv("happy.csv", stringsAsFactors = TRUE) 


# Dashboard

# Sidebar 
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Happiness Level", tabName = "HappinessLevel", icon = icon("smile-wink")),
        menuItem("RandomForest", icon = icon("pagelines"), tabName = "RF"),
        menuItem("Neural Network", icon = icon("project-diagram"), tabName = "NN"),
        menuItem("Deep Neural Network", icon = icon("network-wired"), tabName = "DNN"),
        menuItem("SVM", icon = icon("vuejs"), tabName = "SVM"),
        menuItem("AutoML", icon = icon("truck"), tabName = "AML"),
        menuItem("Data Source", icon = icon("link"), 
                 href = "https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021?select=world-happiness-report-2021.csv")
    )
)

# Body
body <- dashboardBody(
    #Main Page
    tabItems(
        
        tabItem(tabName = "HappinessLevel",
                #h2("Happiness Level"),
                fluidRow(
                    
                    box(
                        title = "Happiness Level per Country", status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("HappyLevelG"),
                    ),
                    
                    box(
                        title = "Life Expentancy per Country", status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("LifeEx1"),
                    ),
                    
                    box(
                        title = "Inputs", status = "warning", solidHeader = TRUE,
                        
                        
                        selectInput(inputId = "select",
                                    label = h4("Region"),
                                    choices = c(levels(happy2$Regional.indicator))
                        ),
                        
                        sliderInput(inputId = "Level",
                                    label = h4("Size:"),
                                    min = 1,
                                    max = 8,
                                    value = c(6,8))
                    ),
                    
                    box(
                        title = "Happiness Level vs GDP", subtitle = "xxx", status = "info", solidHeader = TRUE,
                        collapsible = TRUE, collapsed = TRUE,
                        tags$img(src = "GDPvsHL.gif", height = "400px", width = "600px")
                    ),
                    
                    box(
                        title = "Happiness Level vs Life Expectancy", status = "info", solidHeader = TRUE,
                        collapsible = TRUE, collapsed = TRUE,
                        tags$img(src = "HPvsLE.gif", height = "400px", width = "600px")
                    ),
                    
                    box(
                        title = "Happiness Level vs Generosity perception", status = "info", solidHeader = TRUE,
                        collapsible = TRUE, collapsed = TRUE,
                        tags$img(src = "HvsGen.gif", height = "400px", width = "600px")
                    ),
                    
                    
                )
        ),
        
        
        
        #Random Forest
        tabItem(tabName = "RF",
                #h2("RandomForest"),
                fluidRow(
                    # Graphic
                    box(
                        title = "Random forest", status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        tags$img(src = "RF.png", height = "400px", width = "600px")
                    ),
                    # A static infoBox
                    infoBox("Random Forest Accuracy", "0.1053", icon = icon("thumbs-up"), color="yellow"),
                    
                )
        ),
        
        # NN
        tabItem(tabName = "NN",
                # h2("Neural Network")
                fluidRow(
                    # Graphic
                    box(
                        title = "Neural Network", status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        tags$img(src = "NN.png", height = "400px", width = "600px")
                    ),
                    # A static infoBox
                    infoBox("Neural Network Accuracy", "0.8158", icon = icon("thumbs-up"), color="orange"),
                    
                )
        ),
        
        # DNN
        tabItem(tabName = "DNN",
                #h2("Deep Neural Network")
                fluidRow(
                    # Graphic
                    box(
                        title = "Deep Neural Network", status = "info", solidHeader = TRUE,
                        collapsible = TRUE,
                        tags$img(src = "DNN.png", height = "400px", width = "600px")
                    ),
                    # A static infoBox
                    infoBox("Deep Neural Network Accuracy", "0.6579", icon = icon("thumbs-up"), color="maroon"),
                    
                )
        ),
        
        # SVM
        tabItem(tabName = "SVM",
                h2("SVM"),
                fluidRow(
                    
                    # A static valueBox
                    valueBox("0.2368", "SVM Accuracy", icon = icon("angle-double-right"), color = "yellow"),
                    
                    # A static valueBox
                    valueBox(0.006014, "Training error", icon = icon("angle-double-right"), color = "green"),
                    
                    # A static valueBox
                    valueBox(-0.814, "Objective Function Value", icon = icon("angle-double-right"), color = "orange"),
                    
                )
        ),
        
        # AML
        tabItem(tabName = "AML",
                h2("AutoML"),
                dataTableOutput("AMLtable")
        )
    )
)


#  DASHBOARD
ui <- fluidPage(
    
    # Dashboard Page
    dashboardPage(
        dashboardHeader(title = "Happiness Level Worldwide 2021"),
        sidebar,
        body
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    
    # Plot 2
    
    output$HappyLevelG <- renderPlotly({
        
        
        
        HappyLevelG <- ggplot(data = happy2 
                              %>% filter(Regional.indicator == input$select) 
                              %>% filter(Ladder.score <= input$Level) ,
                              aes(x = Country.name, y = Ladder.score,fill=Regional.indicator)) + 
            geom_bar(stat = "identity", na.rm=TRUE)+
            scale_fill_manual(values = c("orange1"))+
            theme(axis.text.x = element_text(face = "bold", size = 5, angle = 90)) +
            theme(legend.position="none") +
            labs(x = "Country",
                 y = "Happiness Level")+
            theme(axis.title.x = element_text(size = 10),
                  axis.title.y = element_text(size = 10))
        
        ggplotly(HappyLevelG)
        
    })
    
    output$LifeEx1 <- renderPlotly({
        
        
        LifeEx1 <- ggplot(data = happy2 %>% filter(Regional.indicator == input$select),
                          mapping = aes(x = Country.name, y = Healthy.life.expectancy)) +
            geom_point(mapping = aes(color = Regional.indicator, legend = FALSE)) + 
            geom_smooth(method = "loess") +
            scale_fill_manual(values = c("orange1"))+
            theme(axis.text.x = element_text(face = "bold", size = 5, angle = 90))+
            theme(legend.position="none") +
            labs(x = "Country",
                 y = "Life Expectancy")+
            theme(axis.title.x = element_text(size = 10),
                  axis.title.y = element_text(size = 10))
        
        ggplotly(LifeEx1)
        
    })
    
    # Table
    output$AMLtable <- renderDataTable(AMLtable)
    
    
} 

# Run the application 
shinyApp(ui = ui, server = server)
